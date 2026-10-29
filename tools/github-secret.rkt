#lang racket/base
;; github-secret.rkt -- GitHub Actions repository secrets and release
;; plumbing checks through the REST API, in Racket only (no gh CLI).
;;
;;   racket tools/github-secret.rkt --token-file F set-secret OWNER/REPO NAME [--value-file V | --value-is-token]
;;   racket tools/github-secret.rkt --token-file F check OWNER/REPO ...
;;   racket tools/github-secret.rkt --token-file F dispatch-ping OWNER/REPO
;;
;; A secret is encrypted for the repository's public key with libsodium's
;; sealed box (crypto_box_seal): X25519 with an ephemeral key, the key
;; derived by HSalsa20, the nonce BLAKE2b-24(ephemeral-pk || pk), and
;; XSalsa20-Poly1305 for the box. X25519, BLAKE2b and randomness come from
;; racket/crypto; Salsa20 and Poly1305 are written out below.
;; The token is never printed; only HTTP status codes and names are.

(require racket/cmdline racket/port racket/string racket/list racket/vector
         net/http-client net/base64 json
         racket/crypto)

(provide sealed-box secretbox-xsalsa20poly1305 hsalsa20 salsa20-block poly1305 blake2b)

;; ---------------------------------------------------------------- bytes
(define (u32-le bstr i) (integer-bytes->integer bstr #f #f i (+ i 4)))
(define (u32->le! bstr i v) (integer->integer-bytes v 4 #f #f bstr i))
(define (add32 a b) (bitwise-and (+ a b) #xFFFFFFFF))
(define (rotl32 v n) (bitwise-and (bitwise-ior (arithmetic-shift v n) (arithmetic-shift v (- n 32))) #xFFFFFFFF))

;; ---------------------------------------------------------------- Salsa20
(define sigma (vector #x61707865 #x3320646e #x79622d32 #x6b206574))

(define (salsa20-rounds! x)
  ;; 20 rounds (10 double rounds) in place on a 16-word vector
  (define (qr a b c d)
    (vector-set! x b (bitwise-xor (vector-ref x b) (rotl32 (add32 (vector-ref x a) (vector-ref x d)) 7)))
    (vector-set! x c (bitwise-xor (vector-ref x c) (rotl32 (add32 (vector-ref x b) (vector-ref x a)) 9)))
    (vector-set! x d (bitwise-xor (vector-ref x d) (rotl32 (add32 (vector-ref x c) (vector-ref x b)) 13)))
    (vector-set! x a (bitwise-xor (vector-ref x a) (rotl32 (add32 (vector-ref x d) (vector-ref x c)) 18))))
  (for ([i (in-range 10)])
    (qr 0 4 8 12) (qr 5 9 13 1) (qr 10 14 2 6) (qr 15 3 7 11)
    (qr 0 1 2 3) (qr 5 6 7 4) (qr 10 11 8 9) (qr 15 12 13 14)))

(define (salsa20-state key in16)
  ;; key 32 bytes; in16 = 16 bytes placed at words 6..9 (nonce/counter)
  (define x (make-vector 16 0))
  (vector-set! x 0 (vector-ref sigma 0))
  (vector-set! x 5 (vector-ref sigma 1))
  (vector-set! x 10 (vector-ref sigma 2))
  (vector-set! x 15 (vector-ref sigma 3))
  (for ([i (in-range 4)])
    (vector-set! x (+ 1 i) (u32-le key (* 4 i)))
    (vector-set! x (+ 11 i) (u32-le key (+ 16 (* 4 i)))))
  (for ([i (in-range 4)])
    (vector-set! x (+ 6 i) (u32-le in16 (* 4 i))))
  x)

(define (salsa20-block key in16)
  ;; 64-byte keystream block: rounds then add the input
  (define x0 (salsa20-state key in16))
  (define x (vector-copy x0))
  (salsa20-rounds! x)
  (define out (make-bytes 64))
  (for ([i (in-range 16)])
    (u32->le! out (* 4 i) (add32 (vector-ref x i) (vector-ref x0 i))))
  out)

(define (hsalsa20 key in16)
  ;; 32-byte derived key: rounds without the final addition, words 0 5 10 15 6 7 8 9
  (define x (salsa20-state key in16))
  (salsa20-rounds! x)
  (define out (make-bytes 32))
  (for ([i (in-list '(0 5 10 15 6 7 8 9))] [j (in-naturals)])
    (u32->le! out (* 4 j) (vector-ref x i)))
  out)

(define (salsa20-stream key nonce8 len)
  ;; keystream of `len` bytes for an 8-byte nonce, counter from 0
  (define out (make-bytes len 0))
  (let loop ([pos 0] [counter 0])
    (when (< pos len)
      (define in16 (bytes-append nonce8 (integer->integer-bytes counter 8 #f #f)))
      (define block (salsa20-block key in16))
      (define n (min 64 (- len pos)))
      (bytes-copy! out pos block 0 n)
      (loop (+ pos n) (add1 counter))))
  out)

;; ---------------------------------------------------------------- Poly1305
;; (integer-bytes->integer only handles 1/2/4/8 bytes, so little-endian
;; conversions of 16-byte values are written out)
(define (bytes->nat-le bstr start end)
  (for/fold ([n 0]) ([i (in-range (sub1 end) (sub1 start) -1)])
    (+ (arithmetic-shift n 8) (bytes-ref bstr i))))

(define (nat->bytes-le n len)
  (define out (make-bytes len 0))
  (for ([i (in-range len)])
    (bytes-set! out i (bitwise-and (arithmetic-shift n (* -8 i)) 255)))
  out)

(define poly-p (- (expt 2 130) 5))

(define (poly1305 key32 msg)
  (define r (bitwise-and (bytes->nat-le key32 0 16) #x0ffffffc0ffffffc0ffffffc0fffffff))
  (define s (bytes->nat-le key32 16 32))
  (define len (bytes-length msg))
  (define acc
    (let loop ([i 0] [acc 0])
      (cond
        [(>= i len) acc]
        [else
         (define n (min 16 (- len i)))
         (define block (+ (bytes->nat-le msg i (+ i n)) (arithmetic-shift 1 (* 8 n))))
         (loop (+ i n) (modulo (* (+ acc block) r) poly-p))])))
  (define tag (bitwise-and (+ acc s) (sub1 (expt 2 128))))
  (nat->bytes-le tag 16))


;; ---------------------------------------------------------------- BLAKE2b
;; The sealed-box nonce is BLAKE2b with a 24-byte output. racket/crypto's
;; blake2b in the Racket that runs this tool only produces 64 bytes, and a
;; shorter BLAKE2b is a different function (the output length is in the
;; parameter block), so it is written out here (RFC 7693, unkeyed).
(define mask64 (sub1 (expt 2 64)))
(define (add64 . xs) (bitwise-and (apply + xs) mask64))
(define (rotr64 v n) (bitwise-and (bitwise-ior (arithmetic-shift v (- n)) (arithmetic-shift v (- 64 n))) mask64))
(define blake2b-iv
  (vector #x6a09e667f3bcc908 #xbb67ae8584caa73b #x3c6ef372fe94f82b #xa54ff53a5f1d36f1
          #x510e527fade682d1 #x9b05688c2b3e6c1f #x1f83d9abfb41bd6b #x5be0cd19137e2179))
(define blake2b-sigma
  (vector (vector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          (vector 14 10 4 8 9 15 13 6 1 12 0 2 11 7 5 3)
          (vector 11 8 12 0 5 2 15 13 10 14 3 6 7 1 9 4)
          (vector 7 9 3 1 13 12 11 14 2 6 5 10 4 0 15 8)
          (vector 9 0 5 7 2 4 10 15 14 1 11 12 6 8 3 13)
          (vector 2 12 6 10 0 11 8 3 4 13 7 5 15 14 1 9)
          (vector 12 5 1 15 14 13 4 10 0 7 6 3 9 2 8 11)
          (vector 13 11 7 14 12 1 3 9 5 0 15 4 8 6 2 10)
          (vector 6 15 14 9 11 3 0 8 12 2 13 7 1 4 10 5)
          (vector 10 2 8 4 7 6 1 5 15 11 9 14 3 12 13 0)))

(define (blake2b-compress! h block t last?)
  (define m (for/vector ([i (in-range 16)]) (integer-bytes->integer block #f #f (* 8 i) (+ 8 (* 8 i)))))
  (define v (make-vector 16 0))
  (for ([i (in-range 8)])
    (vector-set! v i (vector-ref h i))
    (vector-set! v (+ 8 i) (vector-ref blake2b-iv i)))
  (vector-set! v 12 (bitwise-xor (vector-ref v 12) (bitwise-and t mask64)))
  (vector-set! v 13 (bitwise-xor (vector-ref v 13) (bitwise-and (arithmetic-shift t -64) mask64)))
  (when last? (vector-set! v 14 (bitwise-xor (vector-ref v 14) mask64)))
  (define (g a b c d x y)
    (vector-set! v a (add64 (vector-ref v a) (vector-ref v b) x))
    (vector-set! v d (rotr64 (bitwise-xor (vector-ref v d) (vector-ref v a)) 32))
    (vector-set! v c (add64 (vector-ref v c) (vector-ref v d)))
    (vector-set! v b (rotr64 (bitwise-xor (vector-ref v b) (vector-ref v c)) 24))
    (vector-set! v a (add64 (vector-ref v a) (vector-ref v b) y))
    (vector-set! v d (rotr64 (bitwise-xor (vector-ref v d) (vector-ref v a)) 16))
    (vector-set! v c (add64 (vector-ref v c) (vector-ref v d)))
    (vector-set! v b (rotr64 (bitwise-xor (vector-ref v b) (vector-ref v c)) 63)))
  (for ([r (in-range 12)])
    (define sg (vector-ref blake2b-sigma (modulo r 10)))
    (define (mi i) (vector-ref m (vector-ref sg i)))
    (g 0 4 8 12 (mi 0) (mi 1)) (g 1 5 9 13 (mi 2) (mi 3))
    (g 2 6 10 14 (mi 4) (mi 5)) (g 3 7 11 15 (mi 6) (mi 7))
    (g 0 5 10 15 (mi 8) (mi 9)) (g 1 6 11 12 (mi 10) (mi 11))
    (g 2 7 8 13 (mi 12) (mi 13)) (g 3 4 9 14 (mi 14) (mi 15)))
  (for ([i (in-range 8)])
    (vector-set! h i (bitwise-xor (vector-ref h i) (vector-ref v i) (vector-ref v (+ 8 i))))))

(define (blake2b msg outlen)
  (define h (vector-copy blake2b-iv))
  (vector-set! h 0 (bitwise-xor (vector-ref h 0) #x01010000 outlen))
  (define len (bytes-length msg))
  (define nblocks (max 1 (quotient (+ len 127) 128)))
  (for ([i (in-range nblocks)])
    (define start (* i 128))
    (define end (min len (+ start 128)))
    (define block (make-bytes 128 0))
    (bytes-copy! block 0 msg start end)
    (blake2b-compress! h block end (= i (sub1 nblocks))))
  (define out (make-bytes 64))
  (for ([i (in-range 8)]) (integer->integer-bytes (vector-ref h i) 8 #f #f out (* 8 i)))
  (subbytes out 0 outlen))

;; ---------------------------------------------------------------- box
(define (secretbox-xsalsa20poly1305 key nonce24 msg)
  ;; -> tag(16) || ciphertext, libsodium's crypto_secretbox_easy layout
  (define subkey (hsalsa20 key (subbytes nonce24 0 16)))
  (define stream (salsa20-stream subkey (subbytes nonce24 16 24) (+ 32 (bytes-length msg))))
  (define poly-key (subbytes stream 0 32))
  (define ct (make-bytes (bytes-length msg)))
  (for ([i (in-range (bytes-length msg))])
    (bytes-set! ct i (bitwise-xor (bytes-ref msg i) (bytes-ref stream (+ 32 i)))))
  (bytes-append (poly1305 poly-key ct) ct))

(define (sealed-box recipient-pk msg)
  ;; libsodium crypto_box_seal: epk(32) || secretbox(beforenm(esk, pk), blake2b-24(epk || pk), msg)
  (define esk (x25519-generate-private-key))
  (define epk (x25519-public-key esk))
  (define shared (or (x25519 esk recipient-pk) (error 'sealed-box "weak recipient key")))
  (define key (hsalsa20 shared (make-bytes 16 0)))
  (define nonce (blake2b (bytes-append epk recipient-pk) 24))
  (bytes-append epk (secretbox-xsalsa20poly1305 key nonce msg)))

;; ---------------------------------------------------------------- GitHub API
(define (read-token path)
  (define raw (string-trim (call-with-input-file path port->string)))
  (define datum (with-handlers ([exn:fail? (lambda (e) #f)]) (read (open-input-string raw))))
  (define token (if (string? datum) (string-trim datum) raw))
  (when (string=? token "") (error 'github-secret "token file is empty: ~a" path))
  token)

(define (api token method path #:data [data #f] #:ok [ok '(200)])
  (define-values (status headers in)
    (http-sendrecv "api.github.com" path
                   #:ssl? #t
                   #:method method
                   #:headers (append (list (string-append "Authorization: Bearer " token)
                                           "Accept: application/vnd.github+json"
                                           "X-GitHub-Api-Version: 2022-11-28"
                                           "User-Agent: package-racket-github-secret")
                                     (if data (list "Content-Type: application/json") '()))
                   #:data data))
  (define body (port->string in))
  (close-input-port in)
  (define code (string->number (cadr (string-split (bytes->string/utf-8 status) " "))))
  (unless (member code ok)
    (error 'github-secret "~a ~a -> HTTP ~a~a" method path code
           (if (string=? body "") "" (string-append "\n" (substring body 0 (min 300 (string-length body)))))))
  (values code (if (string=? (string-trim body) "") #f (string->jsexpr body))))

(define (set-secret! token repo name value)
  (define-values (c1 pk) (api token "GET" (format "/repos/~a/actions/secrets/public-key" repo)))
  (define recipient (base64-decode (string->bytes/utf-8 (hash-ref pk 'key))))
  (unless (= 32 (bytes-length recipient)) (error 'github-secret "unexpected public key length"))
  (define sealed (sealed-box recipient (string->bytes/utf-8 value)))
  (define payload (jsexpr->bytes (hash 'encrypted_value (bytes->string/utf-8 (base64-encode sealed #""))
                                       'key_id (hash-ref pk 'key_id))))
  (define-values (c2 _) (api token "PUT" (format "/repos/~a/actions/secrets/~a" repo name) #:data payload #:ok '(201 204)))
  (printf "~a: secret ~a ~a (HTTP ~a)\n" repo name (if (= c2 201) "created" "updated") c2))

(define (check! token repos)
  (for ([repo (in-list repos)])
    (define-values (c info) (api token "GET" (format "/repos/~a" repo)))
    (define perms (hash-ref info 'permissions (hash)))
    (printf "~a: push=~a admin=~a\n" repo (hash-ref perms 'push #f) (hash-ref perms 'admin #f))))

(define (dispatch-ping! token repo)
  (define-values (c _) (api token "POST" (format "/repos/~a/dispatches" repo)
                            #:data (jsexpr->bytes (hash 'event_type "ping")) #:ok '(204)))
  (printf "~a: repository_dispatch accepted (HTTP ~a); no workflow listens for `ping`\n" repo c))

(module+ main
  (define token-file #f)
  (define value-file #f)
  (define value-is-token? #f)
  (define args
    (command-line
     #:program "github-secret.rkt"
     #:once-each
     [("--token-file") path "File holding the GitHub token (plain text or one Racket string datum)" (set! token-file path)]
     [("--value-file") path "File holding the secret value (set-secret)" (set! value-file path)]
     [("--value-is-token") "Use the token itself as the secret value (set-secret)" (set! value-is-token? #t)]
     #:args (command . rest) (cons command rest)))
  (unless token-file (error 'github-secret "--token-file is required"))
  (define token (read-token token-file))
  (match-command (car args) (cdr args) token value-file value-is-token?))

(define (match-command command rest token value-file value-is-token?)
  (case (string->symbol command)
    [(set-secret)
     (unless (= 2 (length rest)) (error 'github-secret "set-secret OWNER/REPO NAME"))
     (define value (cond [value-is-token? token]
                         [value-file (string-trim (call-with-input-file value-file port->string))]
                         [else (error 'github-secret "set-secret needs --value-file or --value-is-token")]))
     (set-secret! token (first rest) (second rest) value)]
    [(check) (check! token rest)]
    [(dispatch-ping)
     (unless (= 1 (length rest)) (error 'github-secret "dispatch-ping OWNER/REPO"))
     (dispatch-ping! token (first rest))]
    [else (error 'github-secret "unknown command: ~a" command)]))

(module+ test
  (require rackunit)
  ;; Poly1305, RFC 7539 section 2.5.2 (key and tag given big-endian in the RFC text)
  (define (hex->bytes h)
    (apply bytes (for/list ([i (in-range 0 (string-length h) 2)]) (string->number (substring h i (+ i 2)) 16))))
  (check-equal? (poly1305 (hex->bytes "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b")
                          #"Cryptographic Forum Research Group")
                (hex->bytes "a8061dc1305136c6c22b8baf0c0127a9"))
  ;; BLAKE2b-512 of "abc", RFC 7693 appendix A
  (check-equal? (blake2b #"abc" 64)
                (hex->bytes (string-append "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d1"
                                           "7d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923")))
  ;; sealed boxes are verified end to end against pynacl by tools/github-secret-oracle.py
  (check-equal? (bytes-length (sealed-box (x25519-public-key (x25519-generate-private-key)) #"hello")) (+ 32 16 5)))
