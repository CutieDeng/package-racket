#lang racket/base

(require racket/cmdline
         racket/file
         racket/list
         racket/match
         racket/port
         racket/string)

(define message-file-arg #f)

(define allowed-types
  '(FEAT FIX REFACTOR TEST DOCS BUILD))

(define (eprintln msg)
  (displayln msg (current-error-port)))

(define (trim-git-comment-lines content)
  (define lines (string-split content "\n" #:trim? #f))
  (string-trim
   (string-join
    (for/list ([line (in-list lines)]
               #:unless (string-prefix? (string-trim line) "#"))
      line)
    "\n")))

(define (read-single-datum source)
  (call-with-input-string
   source
   (lambda (in)
     (define datum (read in))
     (define next (read in))
     (unless (eof-object? next)
       (raise-user-error 'commit-message "message must contain exactly one Racket datum")
     ) ; end unless exactly one datum
     datum)))

(define (feature-info-list? value)
  (and (list? value)
       (not (and (pair? value)
                 (eq? (car value) 'feature)))))

(define (valid-commit-datum? value)
  (match value
    [(list kind title feature-info info)
     (and (memq kind allowed-types)
          (string? title)
          (positive? (string-length title))
          (<= (string-length title) 50)
          (feature-info-list? feature-info)
          (string? info))]
    [_ #f]))

(define (print-examples!)
  (begin
    (eprintln "Expected commit message shape:")
    (eprintln "")
    (eprintln "(FEAT \"title\"")
    (eprintln "")
    (eprintln "()")
    (eprintln "\"detail info\"")
    (eprintln ")")
    (eprintln "")
    (eprintln "TYPE must be one of FEAT, FIX, REFACTOR, TEST, DOCS, BUILD.")
    (eprintln "The third field must be a feature information list; use () when empty.")
    (eprintln "Do not write a literal (feature ...) form in the third field.")
  ) ; end begin print-examples!
) ; end define print-examples!

(command-line
 #:program "check-commit-message.rkt"
 #:once-each
 [("--message-file") path "Git commit message file"
                     (set! message-file-arg path)]
 #:args ()
 (void))

(unless message-file-arg
  (raise-user-error 'check-commit-message.rkt "missing --message-file")
) ; end unless message file

(define message (trim-git-comment-lines (file->string message-file-arg)))

(when (string=? message "")
  (eprintln "Commit message is empty after removing git comments.")
  (print-examples!)
  (exit 1)
) ; end when empty message

(define datum
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (eprintln (string-append "Commit message is not a single readable Racket datum: "
                                              (exn-message exn)))
                     (print-examples!)
                     (exit 1))])
    (read-single-datum message)))

(unless (valid-commit-datum? datum)
  (eprintln "Commit message does not match the required Racket datum shape.")
  (print-examples!)
  (exit 1)
) ; end unless valid datum
