#lang reader tstring/lang/reader racket/base

(require file/tar
         file/md5
         json
         net/uri-codec
         net/url
         racket/cmdline
         racket/file
         racket/list
         racket/match
         racket/path
         racket/place
         racket/port
         racket/runtime-path
         racket/string
         racket/system)

;; Style note:
;; Complex blocks use an explicit begin body plus a single-line closing
;; parenthesis with an end comment. That keeps Racket structure readable in a
;; C-style block-scanning workflow.

(define managed-marker-suffix ".package-racket-managed")
(define-runtime-path script-dir ".")

(define package-cache-modes
  '("postinstall" "cached"))

(define (cached-package-name package-name)
  f"{package-name}-cached")

(define (package-name-for-cache-mode package-name cache-mode)
  (cond
    [(string=? cache-mode "postinstall") package-name]
    [(string=? cache-mode "cached") (cached-package-name package-name)]
    [else
     (raise-user-error 'package-name-for-cache-mode
                       f"cache mode must be postinstall or cached: {cache-mode}")]
  ) ; end cond cache mode package name
) ; end define package-name-for-cache-mode

(struct cfg
  (targets
   racket-root
   make-dir
   package-config
   source-version
   formula-version
   package-name
   release
   prefix
   artifact-dir
   work-dir
   stage-dir
   install-root
   jobs
   skip-build?
   keep-work?
   dry-run?
   make-bin
   tar-bin
   dpkg-deb-bin
   ar-bin
   xz-bin
   deb-backend
   rpmbuild-bin
   rpm-bin
   createrepo-bin
   deb-arch
   rpm-system
   rpm-release
   rpm-arch
   maintainer
   summary
   license
   url
   bottle-root-url
   homebrew-tap
   formula
   update-formula?
   formula-build-mode
   brew-ci-config
   source-release-config
   source-release-repo
   source-release-tag
   source-release-asset
   source-release-token-file
   apt-release-config
   apt-release-repo
   apt-release-tag
   apt-release-asset
   apt-release-token-file
   deb-repo-config
   deb-ci-config
   rpm-repo-config
   rpm-ci-config
   windows-ci-config
   deb-repo-root
   deb-system
   deb-release
   rpm-repo-root
   windows-repo-root
   rpm-repo-id
   rpm-repo-name
   rpm-repo-baseurl
   rpm-repo-enabled?
   rpm-repo-gpgcheck?
   replace-release-asset
   ruby-bin
   with-docs?
   brew-packages
   make-args)
  #:transparent)

(module+ test
  (require rackunit))

(define (println/flush msg)
  (displayln msg)
  (flush-output))

(define (write-text-file! path content)
  (begin
    (call-with-output-file path
      #:exists 'truncate/replace
      (lambda (out)
        (display content out)
      ) ; end lambda out
    ) ; end call-with-output-file
  ) ; end begin write-text-file!
) ; end define write-text-file!

(define (write-executable-text-file! path content)
  (begin
    (write-text-file! path content)
    (file-or-directory-permissions path #o755)
  ) ; end begin write-executable-text-file!
) ; end define write-executable-text-file!

(define (complete-path* p)
  (simplify-path (path->complete-path p)))

(define (clean-path-string p)
  (path->string (complete-path* p)))

(define (empty-directory? path)
  (null? (directory-list path)))

(define (path-basename path)
  (define-values (base name must-be-dir?) (split-path (complete-path* path)))
  (cond
    [(path? name) (path->string name)]
    [(eq? name 'same) "same"]
    [(eq? name 'up) "up"]
    [else "path"]))

(define (homebrew-tap-name tap-path)
  (begin
    (define tap-dir (complete-path* tap-path))
    (define owner-dir (or (path-only tap-dir)
                          (raise-user-error 'homebrew-tap-name f"tap path has no parent: {(clean-path-string tap-dir)}")))
    (define owner (path-basename owner-dir))
    (define repo-dir (path-basename tap-dir))
    (define repo
      (if (string-prefix? repo-dir "homebrew-")
          (substring repo-dir (string-length "homebrew-"))
          repo-dir))
    f"{owner}/{repo}"
  ) ; end begin homebrew-tap-name
) ; end define homebrew-tap-name

(define (managed-marker-path dir)
  (define d (complete-path* dir))
  (define parent (or (path-only d) (current-directory)))
  (build-path parent f".{(path-basename d)}-{managed-marker-suffix}"))

(define (write-marker! marker dir)
  (begin
    (make-directory* (or (path-only marker) (current-directory)))
    (write-text-file!
     marker
     f"managed by package-racket.rkt for {(clean-path-string dir)}
")
  ) ; end begin write-marker!
) ; end define write-marker!

(define (clear-managed-dir! who dir)
  (begin
    (define d (complete-path* dir))
    (define marker (managed-marker-path d))
    (make-directory* (or (path-only d) (current-directory)))
    (cond
      [(directory-exists? d)
       (cond
         [(file-exists? marker)
          (delete-directory/files d)]
         [(empty-directory? d)
          (delete-directory d)]
         [else
          (raise-user-error who
                            f"directory exists and is not managed by this tool: {(clean-path-string d)}")]
       ) ; end cond existing directory
      ]
      [(file-exists? d)
       (raise-user-error who
                         f"path exists but is not a directory: {(clean-path-string d)}")]
      [else
       (void)]
    ) ; end cond path state
    (write-marker! marker d)
  ) ; end begin clear-managed-dir!
) ; end define clear-managed-dir!

(define (reset-managed-dir! who dir)
  (clear-managed-dir! who dir)
  (make-directory* dir))

(define (delete-managed-dir-if-present! dir)
  (begin
    (define d (complete-path* dir))
    (define marker (managed-marker-path d))
    (when (and (directory-exists? d) (file-exists? marker))
      (delete-directory/files d)
      (delete-file marker)
    ) ; end when managed directory exists
  ) ; end begin delete-managed-dir-if-present!
) ; end define delete-managed-dir-if-present!

(define (assert-directory who p)
  (unless (directory-exists? p)
    (raise-user-error who f"directory does not exist: {(clean-path-string p)}")))

(define (assert-file who p)
  (unless (file-exists? p)
    (raise-user-error who f"file does not exist: {(clean-path-string p)}")))

(define (assert-nonempty-file who p)
  (assert-file who p)
  (when (zero? (file-size p))
    (raise-user-error who f"file is empty: {(clean-path-string p)}")))

(define (assert-nonempty-directory who p)
  (assert-directory who p)
  (when (empty-directory? p)
    (raise-user-error who f"directory is empty: {(clean-path-string p)}")))

(define (assert-writable-directory who p)
  (assert-directory who p)
  (define probe (make-temporary-file ".package-racket-write-test~a" #f p))
  (delete-file probe))

(define (resolve-executable who program)
  (or (find-executable-path program)
      (raise-user-error who f"executable not found in PATH: {program}")))

(define (assert-executable who program)
  (resolve-executable who program)
  (void))

(define (shell-quote s)
  (define str (if (path? s) (path->string s) s))
  (cond
    [(regexp-match? #rx"^[A-Za-z0-9_./:=+@%,-]+$" str) str]
    [else f"'{(regexp-replace* #rx"'" str "'\\''")}'"]))

(define (shell-single-quoted s)
  (begin
    (define str (if (path? s) (path->string s) s))
    (define single-quote (integer->char 39))
    (define double-quote (integer->char 34))
    (define quote-mark (string single-quote))
    (define quote-escape
      (list->string (list single-quote double-quote single-quote double-quote single-quote)))
    (string-append quote-mark
                   (string-join (string-split str quote-mark #:trim? #f) quote-escape)
                   quote-mark)
  ) ; end begin shell-single-quoted
) ; end define shell-single-quoted

(define (command-line-string program args)
  (string-join (map shell-quote (cons program args)) " "))

(define (run! who program args #:dry-run? dry-run?)
  (begin
    (println/flush f"$ {(command-line-string program args)}")
    (unless dry-run?
      (define executable (resolve-executable who program))
      (define ok? (apply system* executable args))
      (unless ok?
        (raise-user-error who f"command failed: {(command-line-string program args)}")
      ) ; end unless command ok
    ) ; end unless dry-run
  ) ; end begin run!
) ; end define run!

(define (capture! who program args)
  (begin
    (define executable (resolve-executable who program))
    (define-values (proc out in err)
      (apply subprocess #f #f #f executable args)
    ) ; end define-values proc/out/in/err
    (close-output-port in)
    (define stdout (port->string out))
    (define stderr (port->string err))
    (subprocess-wait proc)
    (define status (subprocess-status proc))
    (close-input-port out)
    (close-input-port err)
    (unless (zero? status)
      (raise-user-error who
                        f"command failed with exit {status}: {(command-line-string program args)}
{stderr}")
    ) ; end unless command success
    stdout
  ) ; end begin capture!
) ; end define capture!

(define (bytes->lower-hex bs)
  (define digits "0123456789abcdef")
  (list->string
   (for*/list ([b (in-bytes bs)]
               [n (in-list (list (arithmetic-shift b -4)
                                 (bitwise-and b #xF)))])
     (string-ref digits n))))

(define (sha256-file path)
  (call-with-input-file path
    (lambda (in)
      (bytes->lower-hex (sha256-bytes in)))))

(define (read-racket-version racket-root)
  (define version-file (build-path racket-root "racket" "src" "version" "racket_version.h"))
  (assert-file 'read-racket-version version-file)
  (define content (file->string version-file))
  (define (macro-int name)
    (define rx (pregexp f"#define[ \t]+{(regexp-quote name)}[ \t]+([0-9]+)"))
    (match (regexp-match rx content)
      [(list _ n) (string->number n)]
      [_ (raise-user-error 'read-racket-version
                           f"could not find {name} in {(clean-path-string version-file)}")]))
  (define x (macro-int "MZSCHEME_VERSION_X"))
  (define y (macro-int "MZSCHEME_VERSION_Y"))
  (define z (macro-int "MZSCHEME_VERSION_Z"))
  (define w (macro-int "MZSCHEME_VERSION_W"))
  (cond
    [(not (zero? w)) f"{x}.{y}.{z}.{w}"]
    [(not (zero? z)) f"{x}.{y}.{z}"]
    [else f"{x}.{y}"]))

(define (assert-version-string who label version)
  (begin
    (unless (and (string? version)
                 (regexp-match? #px"^[0-9]+([.][0-9]+)*$" version))
      (raise-user-error who
                        f"{label} must be a dotted numeric version such as 9.2.2 or 9.2.2.1: {version}")
    ) ; end unless dotted numeric version
    version
  ) ; end begin assert-version-string
) ; end define assert-version-string

(define (catalog-lookup-version version)
  (match (regexp-match #px"^([0-9]+[.][0-9]+)" version)
    [(list _ v) v]
    [_ version]))

(define (read-package-formula-version package-config)
  (begin
    (define raw (read-rktd-hash 'read-package-config package-config))
    (assert-version-string 'read-package-config
                           'formula-version
                           (config-required-string 'read-package-config raw 'formula-version))
  ) ; end begin read-package-formula-version
) ; end define read-package-formula-version

(define (read-package-source-version package-config)
  (begin
    (define raw (read-rktd-hash 'read-package-config package-config))
    (assert-version-string 'read-package-config
                           'source-version
                           (config-required-string 'read-package-config raw 'source-version))
  ) ; end begin read-package-source-version
) ; end define read-package-source-version

(define (assert-racket-root racket-root)
  (assert-directory 'main (build-path racket-root "racket" "src"))
  (assert-directory 'main (build-path racket-root "racket" "collects"))
  (assert-file 'main (build-path racket-root "racket" "src" "version" "racket_version.h")))

(define (assert-make-dir make-dir)
  (assert-directory 'main make-dir)
  (assert-file 'main (build-path make-dir "Makefile")))

(define (normalize-targets raw-targets)
  (begin
    (define pieces
      (for*/list ([raw (in-list raw-targets)]
                  [piece (in-list (string-split raw ","))]
                  #:when (not (string=? "" (string-trim piece))))
        (string-downcase (string-trim piece))
      ) ; end for*/list pieces
    ) ; end define pieces
    (when (null? pieces)
      (raise-user-error 'main "missing --target; use brew, brew-ci, source-release, apt, apt-release, deb-spec, deb-ci, rpm, rpm-spec, rpm-ci, rpm-repo, windows-portable-ci, or all")
    ) ; end when missing target
    (define expanded
      (append-map
       (lambda (target)
         (match target
           ["all" '("brew" "apt" "rpm")]
           [(or "brew" "apt" "apt-release" "deb-spec" "deb-ci" "rpm" "rpm-spec" "rpm-ci" "rpm-repo" "brew-ci" "source-release" "windows-portable-ci") (list target)]
           [_ (raise-user-error 'main f"unknown --target: {target}")]
         ) ; end match target
       ) ; end lambda target
       pieces
      ) ; end append-map
    ) ; end define expanded
    (filter (lambda (target) (member target expanded string=?))
            '("brew-ci" "brew" "source-release" "apt" "apt-release" "deb-spec" "deb-ci" "rpm-spec" "rpm-ci" "rpm" "rpm-repo" "windows-portable-ci"))
  ) ; end begin normalize-targets
) ; end define normalize-targets

(define (racket-root-free-target? target)
  (or (string=? target "source-release")
      (string=? target "apt-release")
      (string=? target "deb-spec")
      (string=? target "deb-ci")
      (string=? target "rpm")
      (string=? target "rpm-spec")
      (string=? target "rpm-ci")
      (string=? target "rpm-repo")
      (string=? target "windows-portable-ci")))

(define (needs-racket-root? targets)
  (not (andmap racket-root-free-target? targets)))

(define (needs-homebrew-tap? targets)
  (or (member "brew" targets string=?)
      (member "brew-ci" targets string=?)))

(define (needs-bottle-root-url? targets)
  (or (member "brew" targets string=?)
      (member "brew-ci" targets string=?)))

(define (target-selected? c target)
  (and (member target (cfg-targets c) string=?) #t))

(define (needs-rpm-repo-config? targets)
  (or (member "rpm-spec" targets string=?)
      (member "rpm-ci" targets string=?)
      (member "rpm-repo" targets string=?)))

(define (needs-deb-repo-config? targets)
  (or (member "deb-spec" targets string=?)
      (member "deb-ci" targets string=?)))

(define (needs-deb-ci-config? targets)
  (member "deb-ci" targets string=?))

(define (needs-rpm-ci-config? targets)
  (member "rpm-ci" targets string=?))

(define (needs-windows-ci-config? targets)
  (member "windows-portable-ci" targets string=?))

(define (needs-rpm-target? targets)
  (or (member "rpm-spec" targets string=?)
      (member "rpm" targets string=?)
      (member "rpm-repo" targets string=?)))

(define (prefix-relative-elements prefix)
  (define trimmed (regexp-replace #rx"^/+" prefix ""))
  (when (string=? trimmed "")
    (raise-user-error 'main "prefix must not be /"))
  (string-split trimmed "/" #:trim? #t))

(define (prefix-install-path install-root prefix)
  (apply build-path install-root (prefix-relative-elements prefix)))

(define (assert-prefix prefix)
  (unless (absolute-path? (string->path prefix))
    (raise-user-error 'main f"--prefix must be absolute: {prefix}"))
  (void))

(define rpm-shared-directories
  '("/bin"
    "/boot"
    "/dev"
    "/etc"
    "/lib"
    "/lib64"
    "/opt"
    "/run"
    "/sbin"
    "/usr"
    "/usr/bin"
    "/usr/etc"
    "/usr/games"
    "/usr/include"
    "/usr/lib"
    "/usr/lib64"
    "/usr/libexec"
    "/usr/local"
    "/usr/sbin"
    "/usr/share"
    "/usr/share/applications"
    "/usr/share/doc"
    "/usr/share/icons"
    "/usr/share/icons/hicolor"
    "/usr/share/man"
    "/usr/share/man/man1"
    "/usr/share/man/man2"
    "/usr/share/man/man3"
    "/usr/share/man/man4"
    "/usr/share/man/man5"
    "/usr/share/man/man6"
    "/usr/share/man/man7"
    "/usr/share/man/man8"
    "/var"))

(define (rpm-shared-directory? installed-path)
  (member installed-path rpm-shared-directories string=?))

(define (installed-path-under-prefix? installed-path prefix)
  (or (string=? installed-path prefix)
      (string-prefix? installed-path f"{prefix}/")))

(define (installed-path-string install-root path)
  (begin
    (define rel (find-relative-path (complete-path* install-root)
                                    (complete-path* path)))
    (define rel-str (path->string rel))
    (when (or (absolute-path? rel)
              (string=? rel-str "")
              (string-prefix? rel-str ".."))
      (raise-user-error 'rpm-file-list
                        f"path is outside install root: {(clean-path-string path)}")
    ) ; end when unsafe relative path
    (string-append "/" rel-str)
  ) ; end begin installed-path-string
) ; end define installed-path-string

(define (rpm-file-list-quote installed-path)
  (begin
    (define escaped-percent (regexp-replace* #rx"%" installed-path "%%"))
    (cond
      [(regexp-match? #rx"^[A-Za-z0-9_./+@%=-]+$" escaped-percent)
       escaped-percent]
      [else
       (define escaped-quotes (regexp-replace* #rx"\"" escaped-percent "\\\\\""))
       (string-append "\"" escaped-quotes "\"")]
    ) ; end cond quote needed
  ) ; end begin rpm-file-list-quote
) ; end define rpm-file-list-quote

(define (rpm-file-list c)
  (begin
    (define root (cfg-install-root c))
    (define prefix (cfg-prefix c))
    (assert-nonempty-directory 'rpm-file-list root)
    (define entries '())
    (define (record! path type)
      (begin
        (define installed (installed-path-string root path))
        (unless (installed-path-under-prefix? installed prefix)
          (raise-user-error 'rpm-file-list
                            f"staged path is outside --prefix {prefix}: {installed}")
        ) ; end unless path under prefix
        (match type
          ['directory
           (unless (rpm-shared-directory? installed)
             (set! entries (cons f"%dir {(rpm-file-list-quote installed)}" entries))
           ) ; end unless shared directory
          ]
          [(or 'file 'link)
           (set! entries (cons (rpm-file-list-quote installed) entries))
          ]
          [other
           (raise-user-error 'rpm-file-list
                             f"unsupported staged file type {other}: {(clean-path-string path)}")]
        ) ; end match type
      ) ; end begin record
    ) ; end define record!
    (define (walk path)
      (begin
        (define type (file-or-directory-type path))
        (record! path type)
        (when (eq? type 'directory)
          (for ([child (in-list (sort (directory-list path #:build? #t)
                                      string<?
                                      #:key path->string))])
            (walk child)
          ) ; end for child
        ) ; end when directory
      ) ; end begin walk
    ) ; end define walk
    (for ([child (in-list (sort (directory-list root #:build? #t)
                                string<?
                                #:key path->string))])
      (walk child)
    ) ; end for root child
    (define sorted-entries (sort entries string<?))
    (when (null? sorted-entries)
      (raise-user-error 'rpm-file-list
                        f"RPM file list is empty under {(clean-path-string root)}")
    ) ; end when empty file list
    sorted-entries
  ) ; end begin rpm-file-list
) ; end define rpm-file-list

(define (write-rpm-file-list! c manifest-path entries)
  (begin
    (write-text-file! manifest-path
                      (string-append (string-join entries "\n") "\n"))
    (assert-nonempty-file 'write-rpm-file-list! manifest-path)
  ) ; end begin write-rpm-file-list!
) ; end define write-rpm-file-list!

(define (assert-bottle-root-url root-url)
  (begin
    (unless (and (string? root-url) (not (string=? root-url "")))
      (raise-user-error 'main "--bottle-root-url must be a non-empty string")
    ) ; end unless non-empty bottle root url
    (unless (string-prefix? root-url "https://")
      (raise-user-error 'main f"--bottle-root-url must start with https://: {root-url}")
    ) ; end unless https bottle root url
    (when (or (string-contains? root-url " ")
              (string-contains? root-url "\"")
              (string-contains? root-url "'"))
      (raise-user-error 'main f"--bottle-root-url contains unsafe characters: {root-url}")
    ) ; end when unsafe bottle root url
    (when (string-suffix? root-url "/")
      (raise-user-error 'main f"--bottle-root-url must not end with /: {root-url}")
    ) ; end when trailing slash bottle root url
  ) ; end begin assert-bottle-root-url
) ; end define assert-bottle-root-url

(define formula-build-modes
  '("incremental" "full"))

(define (assert-formula-build-mode mode)
  (unless (member mode formula-build-modes string=?)
    (raise-user-error 'main
                      f"--formula-build-mode must be incremental or full: {mode}")
  ) ; end unless valid formula build mode
) ; end define assert-formula-build-mode

(define (normalize-rpm-arch arch)
  (begin
    (define normalized (string-downcase (string-trim arch)))
    (match normalized
      [(or "x86_64" "x64" "amd64") "x86_64"]
      [(or "aarch64" "arm64") "aarch64"]
      [_ (raise-user-error 'main
                           f"--rpm-arch must be x86_64, amd64, x64, aarch64, or arm64: {arch}")]
    ) ; end match normalized arch
  ) ; end begin normalize-rpm-arch
) ; end define normalize-rpm-arch

(define (normalize-deb-arch arch)
  (begin
    (define normalized (string-downcase (string-trim arch)))
    (match normalized
      [(or "amd64" "x86_64" "x64") "amd64"]
      [(or "arm64" "aarch64") "arm64"]
      [_ (raise-user-error 'main
                           f"--deb-arch must be amd64, x86_64, x64, arm64, or aarch64: {arch}")]
    ) ; end match normalized deb arch
  ) ; end begin normalize-deb-arch
) ; end define normalize-deb-arch

(define deb-supported-systems
  '("debian12" "ubuntu2404"))

(define (assert-deb-system system)
  (begin
    (unless (and (string? system)
                 (member system deb-supported-systems string=?))
      (raise-user-error 'main
                        f"--deb-system must be one of {(string-join deb-supported-systems ", ")}: {system}")
    ) ; end unless supported deb system
    system
  ) ; end begin assert-deb-system
) ; end define assert-deb-system

(define (assert-deb-release release)
  (begin
    (unless (and (string? release)
                 (regexp-match? #px"^[0-9][A-Za-z0-9+~_-]*$" release))
      (raise-user-error 'main
                        f"--deb-release must start with a digit and contain only letters, digits, _, +, ~, or -: {release}")
    ) ; end unless valid deb release
    release
  ) ; end begin assert-deb-release
) ; end define assert-deb-release

(define rpm-supported-systems
  '("el9" "fc40" "fc43" "fc44" "openeuler2203" "openeuler2403"))

(define (assert-rpm-system system)
  (begin
    (unless (and (string? system)
                 (member system rpm-supported-systems string=?))
      (raise-user-error 'main
                        f"--rpm-system must be one of {(string-join rpm-supported-systems ", ")}: {system}")
    ) ; end unless supported rpm system
    system
  ) ; end begin assert-rpm-system
) ; end define assert-rpm-system

(define (assert-rpm-release release)
  (begin
    (unless (and (string? release)
                 (regexp-match? #px"^[0-9][0-9A-Za-z_+~-]*$" release))
      (raise-user-error 'main
                        f"--rpm-release must start with a digit and use only letters, digits, _, +, ~, or -: {release}")
    ) ; end unless syntactically safe rpm release
    release
  ) ; end begin assert-rpm-release
) ; end define assert-rpm-release

(define (rpm-full-release release system)
  f"{release}.{system}")

(define (path-contained-in? child parent)
  (define parent-str (path->string (path->directory-path (complete-path* parent))))
  (define child-str (path->string (complete-path* child)))
  (string-prefix? child-str parent-str))

(define (regexp-match-count rx content)
  (length (regexp-match* rx content)))

(define (regexp-first-start rx content)
  (match (regexp-match-positions rx content)
    [(list (cons start _)) start]
    [_ #f]))

(define (validate-formula-version-before-sha! who content formula-path)
  (begin
    (define version-start (regexp-first-start #px"(?m:^  version \"[^\"]+\")" content))
    (define sha-start (regexp-first-start #px"(?m:^  sha256 \"[0-9a-f]{64}\")" content))
    (when (and version-start sha-start (not (< version-start sha-start)))
      (raise-user-error who
                        f"formula version line must appear before source sha256 line: {(clean-path-string formula-path)}")
    ) ; end when version appears after sha
  ) ; end begin validate-formula-version-before-sha!
) ; end define validate-formula-version-before-sha!

(define (build-install-root! c)
  (begin
    (define root (cfg-install-root c))
    (cond
      [(cfg-skip-build? c)
       (unless (cfg-dry-run? c)
         (assert-directory 'apt/rpm root)
         (assert-nonempty-directory 'apt/rpm (prefix-install-path root (cfg-prefix c)))
       ) ; end unless dry-run skip-build
      ]
      [else
       (unless (cfg-dry-run? c)
         (assert-make-dir (cfg-make-dir c))
         (reset-managed-dir! 'build-install-root! root)
       ) ; end unless dry-run prepare build root
       (define args
         (append
          (list "-C" (clean-path-string (cfg-make-dir c))
                "unix-style"
                f"PREFIX={(cfg-prefix c)}"
                f"DESTDIR={(clean-path-string root)}"
                f"JOBS={(cfg-jobs c)}")
          (cfg-make-args c)
         ) ; end append make args
       ) ; end define args
       (run! 'build-install-root! (cfg-make-bin c) args #:dry-run? (cfg-dry-run? c))
       (unless (cfg-dry-run? c)
         (assert-nonempty-directory 'build-install-root! (prefix-install-path root (cfg-prefix c)))
       ) ; end unless dry-run validate install root
      ]
    ) ; end cond build-install-root!
  ) ; end begin build-install-root!
) ; end define build-install-root!

(define (write-deb-control! c deb-root)
  (begin
    (define debian-dir (build-path deb-root "DEBIAN"))
    (make-directory* debian-dir)
    (define control-path (build-path debian-dir "control"))
    (write-text-file!
     control-path
     f"Package: {(cfg-package-name c)}
Version: {(cfg-formula-version c)}-{(cfg-release c)}
Section: devel
Priority: optional
Architecture: {(cfg-deb-arch c)}
Maintainer: {(cfg-maintainer c)}
Homepage: {(cfg-url c)}
Description: {(cfg-summary c)}
 Racket packaged from a local checkout.
"
    )
    (validate-deb-control! c control-path)
  ) ; end begin write-deb-control!
) ; end define write-deb-control!

(define (validate-deb-control! c control-path)
  (begin
    (assert-nonempty-file 'validate-deb-control! control-path)
    (define content (file->string control-path))
    (for ([needle (in-list (list f"Package: {(cfg-package-name c)}"
                                 f"Version: {(cfg-formula-version c)}-{(cfg-release c)}"
                                 f"Architecture: {(cfg-deb-arch c)}"
                                 f"Description: {(cfg-summary c)}"))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-deb-control!
                          f"generated control file is missing: {needle}")
      ) ; end unless needle present
    ) ; end for needle
  ) ; end begin validate-deb-control!
) ; end define validate-deb-control!

(define (deb-relative-path root path)
  (begin
    (define rel (find-relative-path (complete-path* root)
                                    (complete-path* path)))
    (define rel-str (path->string rel))
    (when (or (absolute-path? rel)
              (string=? rel-str "")
              (string-prefix? rel-str ".."))
      (raise-user-error 'write-deb-md5sums!
                        f"path is outside deb root: {(clean-path-string path)}")
    ) ; end when unsafe relative path
    rel-str
  ) ; end begin deb-relative-path
) ; end define deb-relative-path

(define (md5-file-hex path)
  (call-with-input-file path
    (lambda (in)
      (bytes->string/utf-8 (md5 in))
    ) ; end lambda in
  ) ; end call-with-input-file
) ; end define md5-file-hex

(define (deb-md5sum-lines deb-root)
  (begin
    (define lines '())
    (define (walk path)
      (begin
        (define type (file-or-directory-type path))
        (match type
          ['directory
           (for ([child (in-list (sort (directory-list path #:build? #t)
                                       string<?
                                       #:key path->string))])
             (walk child)
           ) ; end for child
          ]
          ['file
           (define rel (deb-relative-path deb-root path))
           (set! lines (cons f"{(md5-file-hex path)}  {rel}" lines))
          ]
          ['link
           (void)]
          [other
           (raise-user-error 'write-deb-md5sums!
                             f"unsupported staged file type {other}: {(clean-path-string path)}")]
        ) ; end match type
      ) ; end begin walk
    ) ; end define walk
    (for ([child (in-list (sort (directory-list deb-root #:build? #t)
                                string<?
                                #:key path->string))]
          #:unless (equal? (file-name-from-path child) (string->path "DEBIAN")))
      (walk child)
    ) ; end for root child
    (sort lines string<?)
  ) ; end begin deb-md5sum-lines
) ; end define deb-md5sum-lines

(define (write-deb-md5sums! deb-root)
  (begin
    (define md5-path (build-path deb-root "DEBIAN" "md5sums"))
    (define lines (deb-md5sum-lines deb-root))
    (write-text-file! md5-path
                      (string-append (string-join lines "\n") "\n"))
    (assert-nonempty-file 'write-deb-md5sums! md5-path)
  ) ; end begin write-deb-md5sums!
) ; end define write-deb-md5sums!

(define (validate-deb! c deb-path)
  (begin
    (assert-nonempty-file 'validate-deb! deb-path)
    (define members
      (string-split (capture! 'validate-deb! (cfg-ar-bin c) (list "t" (clean-path-string deb-path)))
                    "\n"
                    #:trim? #t)
    ) ; end define members
    (define expected '("debian-binary" "control.tar.xz" "data.tar.xz"))
    (define members-text (string-join members ", "))
    (unless (equal? members expected)
      (raise-user-error 'validate-deb!
                        f"unexpected .deb archive members: {members-text}")
    ) ; end unless members expected
    (println/flush f"Validated .deb: {(clean-path-string deb-path)}")
  ) ; end begin validate-deb!
) ; end define validate-deb!

(define (apt-deb-name c)
  f"{(cfg-package-name c)}_{(cfg-formula-version c)}-{(cfg-release c)}_{(cfg-deb-arch c)}.deb")

(define (apt-deb-path c)
  (build-path (cfg-artifact-dir c) (apt-deb-name c)))

(define (build-apt! c)
  (begin
    (define install-root (cfg-install-root c))
    (define deb-root (build-path (cfg-work-dir c) "apt-root"))
    (define deb-path (apt-deb-path c))
    (println/flush f"APT package: {(clean-path-string deb-path)}")
    (unless (cfg-dry-run? c)
      (make-directory* (cfg-artifact-dir c))
      (assert-directory 'build-apt! (prefix-install-path install-root (cfg-prefix c)))
      (clear-managed-dir! 'build-apt! deb-root)
      (copy-directory/files install-root deb-root)
      (write-deb-control! c deb-root)
      (write-deb-md5sums! deb-root)
    ) ; end unless dry-run prepare deb root
    (if (use-dpkg-deb? c)
        (run! 'build-apt!
              (cfg-dpkg-deb-bin c)
              (list "--root-owner-group" "--build" (clean-path-string deb-root) (clean-path-string deb-path))
              #:dry-run? (cfg-dry-run? c))
        (build-deb-with-ar! c deb-root deb-path)
    ) ; end if deb backend
    (unless (cfg-dry-run? c)
      (validate-deb! c deb-path)
    ) ; end unless dry-run validate deb
  ) ; end begin build-apt!
) ; end define build-apt!

(define (use-dpkg-deb? c)
  (match (cfg-deb-backend c)
    ["dpkg-deb" #t]
    ["ar" #f]
    ["auto" (and (find-executable-path (cfg-dpkg-deb-bin c)) #t)]
    [other (raise-user-error 'build-apt! f"unknown --deb-backend: {other}")]))

(define (build-deb-with-ar! c deb-root deb-path)
  (begin
    (define deb-work (build-path (cfg-work-dir c) "deb-parts"))
    (define debian-binary (build-path deb-work "debian-binary"))
    (define control-tar (build-path deb-work "control.tar"))
    (define data-tar (build-path deb-work "data.tar"))
    (define control-tar-xz (build-path deb-work "control.tar.xz"))
    (define data-tar-xz (build-path deb-work "data.tar.xz"))
    (unless (cfg-dry-run? c)
      (assert-executable 'build-deb-with-ar! (cfg-ar-bin c))
      (assert-executable 'build-deb-with-ar! (cfg-tar-bin c))
      (assert-executable 'build-deb-with-ar! (cfg-xz-bin c))
      (reset-managed-dir! 'build-deb-with-ar! deb-work)
      (write-text-file! debian-binary "2.0\n")
      (when (file-exists? deb-path)
        (delete-file deb-path)
      ) ; end when old deb exists
    ) ; end unless dry-run prepare deb fallback
    (run! 'build-deb-with-ar!
          (cfg-tar-bin c)
          (list "--format=ustar" "--uid" "0" "--gid" "0" "--uname" "root" "--gname" "root"
                "-C" (clean-path-string (build-path deb-root "DEBIAN"))
                "-cf" (clean-path-string control-tar)
                "control")
          #:dry-run? (cfg-dry-run? c))
    (run! 'build-deb-with-ar!
          (cfg-xz-bin c)
          (list "-z" "-f" "-9" (clean-path-string control-tar))
          #:dry-run? (cfg-dry-run? c))
    (run! 'build-deb-with-ar!
          (cfg-tar-bin c)
          (list "--format=ustar" "--uid" "0" "--gid" "0" "--uname" "root" "--gname" "root"
                "--exclude" "./DEBIAN"
                "-C" (clean-path-string deb-root)
                "-cf" (clean-path-string data-tar)
                ".")
          #:dry-run? (cfg-dry-run? c))
    (run! 'build-deb-with-ar!
          (cfg-xz-bin c)
          (list "-z" "-f" "-9" (clean-path-string data-tar))
          #:dry-run? (cfg-dry-run? c))
    (run! 'build-deb-with-ar!
          (cfg-ar-bin c)
          (list "-qSc" (clean-path-string deb-path)
                (clean-path-string debian-binary)
                (clean-path-string control-tar-xz)
                (clean-path-string data-tar-xz))
          #:dry-run? (cfg-dry-run? c))
  ) ; end begin build-deb-with-ar!
) ; end define build-deb-with-ar!

(define generated-deb-repo-notice-marker
  "GENERATED DEB PACKAGING METADATA - DO NOT EDIT IN deb-racket.")

(define (deb-repo-readme-path c)
  (build-path (cfg-deb-repo-root c) "README.md"))

(define (deb-repo-gitignore-path c)
  (build-path (cfg-deb-repo-root c) ".gitignore"))

(define (deb-sources-dir c)
  (build-path (cfg-deb-repo-root c) "SOURCES"))

(define (deb-scripts-dir c)
  (build-path (cfg-deb-repo-root c) "scripts"))

(define (deb-definition-source-keep-path c)
  (build-path (deb-sources-dir c) ".gitkeep"))

(define (deb-script-path c name)
  (build-path (deb-scripts-dir c) name))

(define (deb-source-archive-name c)
  (brew-source-tgz-name c))

(define (deb-full-release release system)
  f"{release}.{system}")

(define (deb-package-version c release system)
  f"{(cfg-source-version c)}-{(deb-full-release release system)}")

(define (deb-generated-package-name c [release (cfg-deb-release c)]
                                    [system (cfg-deb-system c)]
                                    [arch (cfg-deb-arch c)]
                                    [cache-mode "postinstall"])
  f"{(package-name-for-cache-mode (cfg-package-name c) cache-mode)}_{(deb-package-version c release system)}_{arch}.deb")

(define (deb-script-header name)
  f"#!/usr/bin/env bash
set -euo pipefail

# {generated-deb-repo-notice-marker}
# Generated entrypoint: {name}

")

(define (deb-repo-gitignore-content)
  ".DS_Store
*.tmp
*.swp
.*.swp
.commit
.build/
artifacts/
*.deb
")

(define (deb-readme-content c)
  f"# deb-racket

{generated-deb-repo-notice-marker}

This repository is the Debian package build-script repository generated by
`package-racket`. It is not an apt repository. Treat `SOURCES/`, `scripts/`,
`.github/workflows/`, `.gitignore`, and `README.md` as generated outputs.
Change `package-racket` and regenerate instead of hand-editing production
packaging files here.

The generated build script supports two cache modes: `postinstall` builds the
default `{(cfg-package-name c)}` package and generates the system compiled cache
during package installation; `cached` builds `{(cached-package-name (cfg-package-name c))}`
and embeds the system compiled cache in the `.deb` payload.

## Layout

- `SOURCES/.gitkeep`: source placeholder; build scripts copy or download the
  stable source archive into their explicit work directory.
- `scripts/deb-common.sh`: shared safety checks and staging helpers.
- `scripts/build-deb.sh`: builds a binary `.deb` from the stable source archive.
- `scripts/verify-deb.sh`: validates `.deb` filename and metadata.
- `.github/workflows/build-deb.yml`: builds configured Debian targets with
  GitHub Actions and uploads release assets after every target succeeds.

## Regenerate

Run from `package-racket` to overwrite generated scripts:

```sh
racket package-racket.rkt \\
  --target deb-spec \\
  --deb-repo-config {(clean-path-string (cfg-deb-repo-config c))}
```

Run from `package-racket` to overwrite generated CI:

```sh
racket package-racket.rkt \\
  --target deb-ci \\
  --deb-repo-config {(clean-path-string (cfg-deb-repo-config c))} \\
  --deb-ci-config {(clean-path-string (cfg-deb-ci-config c))}
```

## Build

Build a `.deb` from the generated GitHub Release source URL:

```sh
scripts/build-deb.sh \\
  --artifact-dir /path/to/artifacts \\
  --work-dir /path/to/work \\
  --deb-system {(cfg-deb-system c)} \\
  --deb-release {(cfg-deb-release c)} \\
  --deb-arch {(cfg-deb-arch c)} \\
  --cache-mode postinstall \\
  --prefix /usr
```

Use a local source archive for offline or pinned local builds:

```sh
scripts/build-deb.sh \\
  --source-archive /path/to/{(deb-source-archive-name c)} \\
  --artifact-dir /path/to/artifacts \\
  --work-dir /path/to/work \\
  --deb-system {(cfg-deb-system c)} \\
  --deb-release {(cfg-deb-release c)} \\
  --deb-arch {(cfg-deb-arch c)} \\
  --cache-mode cached \\
  --prefix /usr
```

Supported Debian-family systems are `debian12` and `ubuntu2404`. The package
revision is generated as `deb-release.deb-system`, so `{(cfg-deb-release c)}`
and `{(cfg-deb-system c)}` produce version `{(deb-package-version c (cfg-deb-release c) (cfg-deb-system c))}`.

Validate an existing `.deb`:

```sh
scripts/verify-deb.sh \\
  --deb /path/to/artifacts/{(deb-generated-package-name c)} \\
  --deb-system {(cfg-deb-system c)} \\
  --deb-release {(cfg-deb-release c)} \\
  --deb-arch {(cfg-deb-arch c)} \\
  --cache-mode postinstall
```
")

(define (deb-common-script-content c [source-sha256 (rpm-source-sha256/local c)])
  f"{(deb-script-header "deb-common.sh")}BASE_PACKAGE_NAME={(shell-single-quoted (cfg-package-name c))}
CACHED_PACKAGE_NAME={(shell-single-quoted (cached-package-name (cfg-package-name c)))}
PACKAGE_NAME=\"$BASE_PACKAGE_NAME\"
PACKAGE_VERSION={(shell-single-quoted (cfg-source-version c))}
PACKAGE_SOURCE_VERSION={(shell-single-quoted (cfg-source-version c))}
DEFAULT_DEB_SYSTEM={(shell-single-quoted (cfg-deb-system c))}
DEFAULT_DEB_RELEASE={(shell-single-quoted (cfg-deb-release c))}
DEFAULT_DEB_ARCH={(shell-single-quoted (cfg-deb-arch c))}
DEFAULT_PREFIX={(shell-single-quoted (cfg-prefix c))}
DEFAULT_CACHE_MODE=postinstall
SOURCE_ARCHIVE_NAME={(shell-single-quoted (deb-source-archive-name c))}
DEFAULT_SOURCE_URL={(shell-single-quoted (formula-source-url c))}
SOURCE_SHA256={(shell-single-quoted source-sha256)}
PACKAGE_SUMMARY={(shell-single-quoted (cfg-summary c))}
PACKAGE_MAINTAINER={(shell-single-quoted (cfg-maintainer c))}
PACKAGE_HOMEPAGE={(shell-single-quoted (cfg-url c))}

die() {{
  printf 'ERROR: %s\\n' \"$*\" >&2
  exit 1
}}

usage_error() {{
  die \"$1. Run with --help for usage.\"
}}

repo_root_from_script() {{
  local script_dir
  script_dir=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)
  CDPATH= cd -- \"$script_dir/..\" && pwd
}}

require_repo_root() {{
  local root=\"$1\"
  [ -d \"$root\" ] || die \"repository root does not exist: $root\"
  [ -f \"$root/scripts/deb-common.sh\" ] || die \"missing common script: $root/scripts/deb-common.sh\"
  [ -f \"$root/SOURCES/.gitkeep\" ] || die \"missing source placeholder: $root/SOURCES/.gitkeep\"
}}

require_file() {{
  [ -f \"$1\" ] || die \"file does not exist: $1\"
}}

require_nonempty_file() {{
  require_file \"$1\"
  [ -s \"$1\" ] || die \"file is empty: $1\"
}}

require_dir() {{
  [ -d \"$1\" ] || die \"directory does not exist: $1\"
}}

require_absolute_path() {{
  case \"$1\" in
    /*) ;;
    *) die \"$2 must be an absolute path: $1\" ;;
  esac
}}

require_exe() {{
  command -v \"$1\" >/dev/null 2>&1 || die \"executable not found in PATH: $1\"
}}

maybe_require_exe() {{
  local dry_run=\"$1\"
  local exe=\"$2\"
  if [ \"$dry_run\" = 1 ]; then
    printf 'Would require executable: %s\\n' \"$exe\"
  else
    require_exe \"$exe\"
  fi
}}

run_cmd() {{
  local dry_run=\"$1\"
  shift
  printf '$'
  printf ' %q' \"$@\"
  printf '\\n'
  if [ \"$dry_run\" = 0 ]; then
    \"$@\"
  fi
}}

normalize_arch() {{
  case \"$1\" in
    amd64|x86_64|x64) printf 'amd64\\n' ;;
    arm64|aarch64) printf 'arm64\\n' ;;
    *) die \"deb arch must be amd64, x86_64, x64, arm64, or aarch64: $1\" ;;
  esac
}}

validate_deb_system() {{
  case \"$1\" in
    debian12|ubuntu2404) ;;
    *) die \"deb system must be debian12 or ubuntu2404: $1\" ;;
  esac
}}

validate_deb_release() {{
  local release=\"$1\"
  [ -n \"$release\" ] || die \"deb release is required\"
  case \"$release\" in
    *.*) die \"deb release must not contain . because system is appended separately: $release\" ;;
    [0-9]*) ;;
    *) die \"deb release must start with a digit: $release\" ;;
  esac
  case \"$release\" in
    *[!A-Za-z0-9_+~-]*) die \"deb release contains unsupported characters: $release\" ;;
  esac
}}

validate_cache_mode() {{
  case \"$1\" in
    postinstall|cached) ;;
    *) die \"cache mode must be postinstall or cached: $1\" ;;
  esac
}}

package_name_for_cache_mode() {{
  local mode=\"$1\"
  validate_cache_mode \"$mode\"
  case \"$mode\" in
    postinstall) printf '%s\\n' \"$BASE_PACKAGE_NAME\" ;;
    cached) printf '%s\\n' \"$CACHED_PACKAGE_NAME\" ;;
  esac
}}

conflicting_package_name_for_cache_mode() {{
  local mode=\"$1\"
  validate_cache_mode \"$mode\"
  case \"$mode\" in
    postinstall) printf '%s\\n' \"$CACHED_PACKAGE_NAME\" ;;
    cached) printf '%s\\n' \"$BASE_PACKAGE_NAME\" ;;
  esac
}}

deb_full_release() {{
  local release=\"$1\"
  local system=\"$2\"
  printf '%s.%s\\n' \"$release\" \"$system\"
}}

deb_package_version() {{
  local release=\"$1\"
  local system=\"$2\"
  printf '%s-%s\\n' \"$PACKAGE_VERSION\" \"$(deb_full_release \"$release\" \"$system\")\"
}}

deb_name_for_arch() {{
  local arch=\"$1\"
  local release=\"$2\"
  local system=\"$3\"
  local mode=\"${{4:-$DEFAULT_CACHE_MODE}}\"
  local package_name
  package_name=$(package_name_for_cache_mode \"$mode\")
  printf '%s_%s_%s.deb\\n' \"$package_name\" \"$(deb_package_version \"$release\" \"$system\")\" \"$arch\"
}}

find_staged_config_dir() {{
  local stage_root=\"$1\"
  local prefix=\"$2\"
  local candidate
  for candidate in \"$stage_root/etc/racket\" \"$stage_root$prefix/etc/racket\"; do
    if [ -f \"$candidate/config.rktd\" ]; then
      printf '%s\\n' \"$candidate\"
      return
    fi
  done
  die \"could not find staged Racket config.rktd under $stage_root\"
}}

find_staged_racket() {{
  local stage_root=\"$1\"
  local prefix=\"$2\"
  local candidate
  for candidate in \"$stage_root$prefix/bin/racket\" \"$stage_root/bin/racket\"; do
    if [ -x \"$candidate\" ]; then
      printf '%s\\n' \"$candidate\"
      return
    fi
  done
  die \"could not find staged racket executable under $stage_root\"
}}

find_staged_collects_dir() {{
  local stage_root=\"$1\"
  local prefix=\"$2\"
  local candidate
  for candidate in \"$stage_root$prefix/share/racket/collects\" \"$stage_root/usr/share/racket/collects\"; do
    if [ -d \"$candidate\" ]; then
      printf '%s\\n' \"$candidate\"
      return
    fi
  done
  die \"could not find staged Racket collects under $stage_root\"
}}

require_staged_system_cache() {{
  local stage_root=\"$1\"
  local prefix=\"$2\"
  local cache_root=\"$stage_root/var/cache/racket/compiled\"
  local runtime_collects_dir=\"$prefix/share/racket/collects\"
  local runtime_pkgs_dir=\"$prefix/share/racket/pkgs\"
  local runtime_collects_cache=\"$cache_root/${{runtime_collects_dir#/}}\"
  local runtime_pkgs_cache=\"$cache_root/${{runtime_pkgs_dir#/}}\"
  if ! find \"$runtime_collects_cache\" -path '*/compiled/*.zo' -type f -print -quit 2>/dev/null | grep -q .; then
    die \"runtime-keyed staged system compiled cache is empty: $runtime_collects_cache\"
  fi
  if ! find \"$runtime_pkgs_cache\" -path '*/compiled/*.zo' -type f -print -quit 2>/dev/null | grep -q .; then
    die \"runtime-keyed staged package compiled cache is empty: $runtime_pkgs_cache\"
  fi
  require_staged_cache_deps_runtime_keyed \"$cache_root\" \"$stage_root\"
}}

require_staged_cache_deps_runtime_keyed() {{
  local cache_root=\"$1\"
  local stage_root=\"$2\"
  if grep -RFl --include '*.dep' \"$stage_root\" \"$cache_root\" 2>/dev/null | grep -q .; then
    die \"staged compiled cache dependency metadata contains buildroot paths: $cache_root\"
  fi
}}

require_staged_rhombus_cache_root() {{
  local demod_cache_root=\"$1\"
  local cache_kind=\"$2\"
  local prefix=\"$3\"
  local runtime_collects_dir=\"$prefix/share/racket/collects\"
  local runtime_pkgs_dir=\"$prefix/share/racket/pkgs\"
  local runtime_collects_cache=\"$demod_cache_root/${{runtime_collects_dir#/}}\"
  local runtime_pkgs_cache=\"$demod_cache_root/${{runtime_pkgs_dir#/}}\"
  if ! find \"$runtime_collects_cache\" -path '*/compiled/*.zo' -type f -print -quit 2>/dev/null | grep -q .; then
    die \"runtime-keyed staged Rhombus demod $cache_kind collects cache is empty: $runtime_collects_cache\"
  fi
  if ! find \"$runtime_pkgs_cache\" -path '*/compiled/*.zo' -type f -print -quit 2>/dev/null | grep -q .; then
    die \"runtime-keyed staged Rhombus demod $cache_kind package cache is empty: $runtime_pkgs_cache\"
  fi
}}

require_staged_rhombus_cache() {{
  local stage_root=\"$1\"
  local prefix=\"$2\"
  local rhombus_ephemeral_cache=\"$stage_root$prefix/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod\"
  local stage_key=\"${{stage_root#/}}\"
  require_staged_rhombus_cache_root \"$rhombus_ephemeral_cache/linklet\" linklet \"$prefix\"
  require_staged_rhombus_cache_root \"$rhombus_ephemeral_cache/native\" native \"$prefix\"
  if [ -n \"$stage_key\" ] && (cd \"$rhombus_ephemeral_cache\" && find . -path \"*/$stage_key/*\" -print -quit 2>/dev/null | grep -q .); then
    die \"staged Rhombus demod cache contains buildroot-keyed paths: $stage_key\"
  fi
  require_staged_cache_deps_runtime_keyed \"$rhombus_ephemeral_cache\" \"$stage_root\"
}}

escape_config_sed_pattern() {{
  printf '%s\\n' \"$1\" | sed 's/[][\\\\.^$*|]/\\\\&/g'
}}

escape_config_sed_replacement() {{
  printf '%s\\n' \"$1\" | sed 's/[\\\\&|]/\\\\&/g'
}}

replace_config_value() {{
  local config_file=\"$1\"
  local key=\"$2\"
  local from=\"$3\"
  local to=\"$4\"
  local required=\"${{5:-optional}}\"
  local needle replacement escaped_needle escaped_replacement tmp_file
  needle=\"($key . \\\"$from\\\")\"
  replacement=\"($key . \\\"$to\\\")\"
  if ! grep -F \"$needle\" \"$config_file\" >/dev/null; then
    if [ \"$required\" = required ]; then
      die \"config does not contain expected $key value $from: $config_file\"
    fi
    return 0
  fi
  escaped_needle=$(escape_config_sed_pattern \"$needle\")
  escaped_replacement=$(escape_config_sed_replacement \"$replacement\")
  tmp_file=\"$config_file.package-racket-rewrite.$$\"
  sed \"s|$escaped_needle|$escaped_replacement|g\" \"$config_file\" > \"$tmp_file\" || {{ rm -f \"$tmp_file\"; return 1; }}
  mv \"$tmp_file\" \"$config_file\"
}}

write_staged_config() {{
  local config_file=\"$1\"
  local stage_root=\"$2\"
  local prefix=\"$3\"
  local runtime_cache_root=\"$4\"
  local staged_cache_root=\"$5\"
  replace_config_value \"$config_file\" compiled-file-system-cache-root \"$runtime_cache_root\" \"$staged_cache_root\" required
  replace_config_value \"$config_file\" share-dir \"$prefix/share/racket\" \"$stage_root$prefix/share/racket\"
  replace_config_value \"$config_file\" pkgs-dir \"$prefix/share/racket/pkgs\" \"$stage_root$prefix/share/racket/pkgs\"
  replace_config_value \"$config_file\" doc-dir \"$prefix/share/doc/racket\" \"$stage_root$prefix/share/doc/racket\"
  replace_config_value \"$config_file\" lib-dir \"$prefix/lib/racket\" \"$stage_root$prefix/lib/racket\"
  replace_config_value \"$config_file\" include-dir \"$prefix/include/racket\" \"$stage_root$prefix/include/racket\"
  replace_config_value \"$config_file\" bin-dir \"$prefix/bin\" \"$stage_root$prefix/bin\"
  replace_config_value \"$config_file\" apps-dir \"$prefix/share/applications\" \"$stage_root$prefix/share/applications\"
  replace_config_value \"$config_file\" man-dir \"$prefix/share/man\" \"$stage_root$prefix/share/man\"
}}

move_staged_cache_tree() {{
  local cache_root=\"$1\"
  local from_source=\"$2\"
  local to_source=\"$3\"
  local from=\"$cache_root/${{from_source#/}}\"
  local to=\"$cache_root/${{to_source#/}}\"
  [ -e \"$from\" ] || return 0
  [ \"$from\" = \"$to\" ] && return 0
  mkdir -p \"$(dirname \"$to\")\"
  if [ -e \"$to\" ]; then
    cp -a \"$from\"/. \"$to\"/
    rm -rf \"$from\"
  else
    mv \"$from\" \"$to\"
  fi
}}

rewrite_text_file() {{
  local path=\"$1\"
  local from=\"$2\"
  local to=\"$3\"
  local escaped_from escaped_to tmp_file
  escaped_from=$(escape_config_sed_pattern \"$from\")
  escaped_to=$(escape_config_sed_replacement \"$to\")
  tmp_file=\"$path.package-racket-rewrite.$$\"
  sed \"s|$escaped_from|$escaped_to|g\" \"$path\" > \"$tmp_file\" || {{ rm -f \"$tmp_file\"; return 1; }}
  mv \"$tmp_file\" \"$path\"
}}

touch_cache_zos_after_deps() {{
  local cache_root=\"$1\"
  local dep_path dep_seconds max_dep_seconds touch_seconds
  max_dep_seconds=0
  [ -d \"$cache_root\" ] || return 0
  while IFS= read -r -d '' dep_path; do
    dep_seconds=$(stat -c %Y \"$dep_path\")
    if [ \"$dep_seconds\" -gt \"$max_dep_seconds\" ]; then
      max_dep_seconds=\"$dep_seconds\"
    fi
  done < <(find \"$cache_root\" -type f -name '*.dep' -print0)
  [ \"$max_dep_seconds\" -gt 0 ] || return 0
  touch_seconds=$((max_dep_seconds + 1))
  find \"$cache_root\" -type f -name '*.zo' -exec touch -d \"@$touch_seconds\" {{}} +
}}

rewrite_staged_cache_dep_paths() {{
  local cache_root=\"$1\"
  local stage_root=\"$2\"
  local prefix=\"$3\"
  local dep_path
  [ -d \"$cache_root\" ] || return 0
  while IFS= read -r -d '' dep_path; do
    rewrite_text_file \"$dep_path\" \"$stage_root$prefix\" \"$prefix\"
    rewrite_text_file \"$dep_path\" \"$stage_root/etc\" \"/etc\"
  done < <(find \"$cache_root\" -type f -name '*.dep' -print0)
  touch_cache_zos_after_deps \"$cache_root\"
}}

normalize_staged_system_cache() {{
  local stage_root=\"$1\"
  local prefix=\"$2\"
  local cache_root=\"$stage_root/var/cache/racket/compiled\"
  move_staged_cache_tree \"$cache_root\" \"$stage_root$prefix/share/racket/collects\" \"$prefix/share/racket/collects\"
  move_staged_cache_tree \"$cache_root\" \"$stage_root$prefix/share/racket/pkgs\" \"$prefix/share/racket/pkgs\"
  rewrite_staged_cache_dep_paths \"$cache_root\" \"$stage_root\" \"$prefix\"
  rm -f \"$stage_root/var/cache/racket/racket-compiled-cache.log\"
  find \"$cache_root\" -type d -empty -delete 2>/dev/null || true
}}

normalize_staged_rhombus_cache() {{
  local stage_root=\"$1\"
  local prefix=\"$2\"
  local demod_root=\"$stage_root$prefix/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod\"
  local demod_cache_root
  [ -d \"$demod_root\" ] || return 0
  for demod_cache_root in \"$demod_root\"/*; do
    [ -d \"$demod_cache_root\" ] || continue
    move_staged_cache_tree \"$demod_cache_root\" \"$stage_root$prefix/share/racket/collects\" \"$prefix/share/racket/collects\"
    move_staged_cache_tree \"$demod_cache_root\" \"$stage_root$prefix/share/racket/pkgs\" \"$prefix/share/racket/pkgs\"
    find \"$demod_cache_root\" -type d -empty -delete 2>/dev/null || true
  done
  rewrite_staged_cache_dep_paths \"$demod_root\" \"$stage_root\" \"$prefix\"
  find \"$demod_root\" -type d -empty -delete 2>/dev/null || true
}}

warm_staged_rhombus_cache() {{
  local stage_root=\"$1\"
  local prefix=\"$2\"
  local config_dir=\"$3\"
  local racket_bin=\"$4\"
  local runtime_config_dir=\"/etc/racket\"
  local runtime_cache_parent=\"/var/cache/racket\"
  local runtime_cache_root=\"$runtime_cache_parent/compiled\"
  local staged_cache_parent=\"$stage_root$runtime_cache_parent\"
  local runtime_share_dir=\"$prefix/share/racket\"
  local runtime_collects_dir=\"$runtime_share_dir/collects\"
  local runtime_lib_dir=\"$prefix/lib/racket\"
  local runtime_bin_dir=\"$prefix/bin\"
  local runtime_racket_bin=\"$runtime_bin_dir/racket\"
  local runtime_rhombus_bin=\"$runtime_bin_dir/rhombus\"
  local staged_rhombus_bin=\"$stage_root$runtime_bin_dir/rhombus\"
  local runtime_links=
  local empty_home=
  cleanup_runtime_links() {{
    if [ -n \"${{runtime_links:-}}\" ]; then
      printf '%s\\n' \"$runtime_links\" | while IFS= read -r runtime_link; do
        [ -n \"$runtime_link\" ] || continue
        [ -L \"$runtime_link\" ] && rm -f \"$runtime_link\"
      done
    fi
  }}
  cleanup_warmup() {{
    cleanup_runtime_links
    [ -n \"${{empty_home:-}}\" ] && rm -rf \"$empty_home\"
  }}
  add_runtime_link() {{
    local runtime_link_target=\"$1\"
    local runtime_link_path=\"$2\"
    if [ -e \"$runtime_link_path\" ] || [ -L \"$runtime_link_path\" ]; then
      die \"runtime staging link path already exists: $runtime_link_path\"
    fi
    mkdir -p \"$(dirname \"$runtime_link_path\")\"
    ln -s \"$runtime_link_target\" \"$runtime_link_path\"
    runtime_links=\"$runtime_link_path
$runtime_links\"
  }}
  mkdir -p \"$staged_cache_parent\"
  [ -x \"$staged_rhombus_bin\" ] || die \"missing staged Rhombus launcher: $staged_rhombus_bin\"
  empty_home=$(mktemp -d)
  trap cleanup_warmup EXIT
  add_runtime_link \"$stage_root$runtime_share_dir\" \"$runtime_share_dir\"
  add_runtime_link \"$stage_root$runtime_lib_dir\" \"$runtime_lib_dir\"
  add_runtime_link \"$config_dir\" \"$runtime_config_dir\"
  add_runtime_link \"$staged_cache_parent\" \"$runtime_cache_parent\"
  add_runtime_link \"$racket_bin\" \"$runtime_racket_bin\"
  add_runtime_link \"$staged_rhombus_bin\" \"$runtime_rhombus_bin\"
  if ! HOME=\"$empty_home\" PLTCOMPILEDROOTS=\"$runtime_cache_root\" \"$runtime_rhombus_bin\" --version >/dev/null; then
    cleanup_warmup
    trap - EXIT
    return 1
  fi
  if ! HOME=\"$empty_home\" PLTCOMPILEDROOTS=\"$runtime_cache_root\" \"$runtime_rhombus_bin\" -e 'println(\"package-racket-rhombus-cache\")' >/dev/null; then
    cleanup_warmup
    trap - EXIT
    return 1
  fi
  cleanup_warmup
  trap - EXIT
}}

build_staged_system_cache() {{
  local stage_root=\"$1\"
  local prefix=\"$2\"
  local runtime_cache_root=\"/var/cache/racket/compiled\"
  local staged_cache_root=\"$stage_root$runtime_cache_root\"
  local config_dir config_file collects_dir racket_bin backup
  config_dir=$(find_staged_config_dir \"$stage_root\" \"$prefix\")
  config_file=\"$config_dir/config.rktd\"
  collects_dir=$(find_staged_collects_dir \"$stage_root\" \"$prefix\")
  racket_bin=$(find_staged_racket \"$stage_root\" \"$prefix\")
  backup=\"$config_file.package-racket-cache-backup\"
  cp \"$config_file\" \"$backup\"
  write_staged_config \"$config_file\" \"$stage_root\" \"$prefix\" \"$runtime_cache_root\" \"$staged_cache_root\"
  mkdir -p \"$staged_cache_root\"
  if ! \"$racket_bin\" -X \"$collects_dir\" -G \"$config_dir\" -N raco -l- raco setup -j 1 --system --no-user --reset-cache -D --no-pkg-deps --no-launcher; then
    cp \"$backup\" \"$config_file\"
    rm -f \"$backup\"
    return 1
  fi
  cp \"$backup\" \"$config_file\"
  rm -f \"$backup\"
  if ! warm_staged_rhombus_cache \"$stage_root\" \"$prefix\" \"$config_dir\" \"$racket_bin\"; then
    return 1
  fi
  normalize_staged_system_cache \"$stage_root\" \"$prefix\"
  normalize_staged_rhombus_cache \"$stage_root\" \"$prefix\"
  require_staged_system_cache \"$stage_root\" \"$prefix\"
  require_staged_rhombus_cache \"$stage_root\" \"$prefix\"
}}

reset_output_dir() {{
  local dry_run=\"$1\"
  local path=\"$2\"
  require_absolute_path \"$path\" \"output directory\"
  if [ \"$path\" = / ]; then
    die \"refusing to reset / as output directory\"
  fi
  if [ \"$dry_run\" = 1 ]; then
    printf 'Would reset output directory: %s\\n' \"$path\"
  else
    rm -rf \"$path\"
    mkdir -p \"$path\"
  fi
}}

validate_source_archive() {{
  local dry_run=\"$1\"
  local archive=\"$2\"
  local expected_root=\"racket-$PACKAGE_SOURCE_VERSION\"
  if [ \"$dry_run\" = 1 ]; then
    printf 'Would validate source archive: %s\\n' \"$archive\"
    return
  fi
  require_nonempty_file \"$archive\"
  tar -tzf \"$archive\" \"$expected_root/src/configure\" >/dev/null \\
    || die \"source archive missing $expected_root/src/configure: $archive\"
  tar -tzf \"$archive\" \"$expected_root/collects/racket/main.rkt\" >/dev/null \\
    || die \"source archive missing $expected_root/collects/racket/main.rkt: $archive\"
}}

verify_source_sha256() {{
  local dry_run=\"$1\"
  local archive=\"$2\"
  if [ -z \"$SOURCE_SHA256\" ]; then
    printf 'No generated source sha256 is pinned; skipping source sha256 check.\\n'
    return
  fi
  if [ \"$dry_run\" = 1 ]; then
    printf 'Would verify source sha256: %s\\n' \"$SOURCE_SHA256\"
    return
  fi
  local actual
  if command -v sha256sum >/dev/null 2>&1; then
    actual=$(sha256sum \"$archive\" | cut -d ' ' -f 1)
  elif command -v shasum >/dev/null 2>&1; then
    actual=$(shasum -a 256 \"$archive\" | cut -d ' ' -f 1)
  else
    die \"executable not found in PATH: sha256sum or shasum\"
  fi
  [ \"$actual\" = \"$SOURCE_SHA256\" ] \\
    || die \"source sha256 mismatch: expected $SOURCE_SHA256 but got $actual\"
}}

prepare_source_archive() {{
  local dry_run=\"$1\"
  local source_archive=\"$2\"
  local source_url=\"$3\"
  local dest=\"$4\"
  require_absolute_path \"$dest\" \"source archive destination\"
  if [ \"$dry_run\" = 0 ]; then
    mkdir -p \"$(dirname \"$dest\")\"
  fi
  if [ -n \"$source_archive\" ]; then
    require_nonempty_file \"$source_archive\"
    run_cmd \"$dry_run\" cp \"$source_archive\" \"$dest\"
  else
    [ -n \"$source_url\" ] || die \"source URL is empty\"
    maybe_require_exe \"$dry_run\" curl
    run_cmd \"$dry_run\" curl -fL --retry 3 --output \"$dest\" \"$source_url\"
  fi
  validate_source_archive \"$dry_run\" \"$dest\"
  verify_source_sha256 \"$dry_run\" \"$dest\"
}}
")

(define (deb-build-script-content c)
  f"{(deb-script-header "build-deb.sh")}SCRIPT_DIR=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)
source \"$SCRIPT_DIR/deb-common.sh\"

usage() {{
  cat <<'USAGE'
Usage: scripts/build-deb.sh --artifact-dir PATH --work-dir PATH --deb-system SYSTEM --deb-release RELEASE --deb-arch ARCH [options]

Build a binary .deb from a stable source archive. All mutable paths are named.

Options:
  --source-archive PATH  Local source archive to copy into the build work dir.
  --source-url URL       Source archive URL. Defaults to the generated release URL.
  --artifact-dir PATH    Directory that receives the final .deb.
  --work-dir PATH        Build work directory.
  --deb-system SYSTEM    debian12 or ubuntu2404.
  --deb-release RELEASE  Package revision base, for example 1. The system suffix is appended separately.
  --cache-mode MODE      postinstall or cached. Defaults to postinstall.
  --prefix PATH          Install prefix inside the package. Defaults to generated /usr.
  --deb-arch ARCH        amd64, x86_64, x64, arm64, or aarch64.
  --jobs N               Parallel jobs passed to make.
  --dry-run              Print checks and commands without writing outputs.
USAGE
}}

DRY_RUN=0
SOURCE_ARCHIVE=
SOURCE_URL=\"$DEFAULT_SOURCE_URL\"
SOURCE_URL_EXPLICIT=0
ARTIFACT_DIR=
WORK_DIR=
DEB_SYSTEM=
DEB_RELEASE=
DEB_ARCH=
JOBS=1
PREFIX=\"$DEFAULT_PREFIX\"
CACHE_MODE=\"$DEFAULT_CACHE_MODE\"

while [ $# -gt 0 ]; do
  case \"$1\" in
    --source-archive) [ $# -ge 2 ] || usage_error \"missing value for --source-archive\"; SOURCE_ARCHIVE=\"$2\"; shift 2 ;;
    --source-url) [ $# -ge 2 ] || usage_error \"missing value for --source-url\"; SOURCE_URL=\"$2\"; SOURCE_URL_EXPLICIT=1; shift 2 ;;
    --artifact-dir) [ $# -ge 2 ] || usage_error \"missing value for --artifact-dir\"; ARTIFACT_DIR=\"$2\"; shift 2 ;;
    --work-dir) [ $# -ge 2 ] || usage_error \"missing value for --work-dir\"; WORK_DIR=\"$2\"; shift 2 ;;
    --deb-system) [ $# -ge 2 ] || usage_error \"missing value for --deb-system\"; DEB_SYSTEM=\"$2\"; shift 2 ;;
    --deb-release) [ $# -ge 2 ] || usage_error \"missing value for --deb-release\"; DEB_RELEASE=\"$2\"; shift 2 ;;
    --cache-mode) [ $# -ge 2 ] || usage_error \"missing value for --cache-mode\"; CACHE_MODE=\"$2\"; shift 2 ;;
    --prefix) [ $# -ge 2 ] || usage_error \"missing value for --prefix\"; PREFIX=\"$2\"; shift 2 ;;
    --deb-arch) [ $# -ge 2 ] || usage_error \"missing value for --deb-arch\"; DEB_ARCH=\"$2\"; shift 2 ;;
    --jobs) [ $# -ge 2 ] || usage_error \"missing value for --jobs\"; JOBS=\"$2\"; shift 2 ;;
    --dry-run) DRY_RUN=1; shift ;;
    --help) usage; exit 0 ;;
    *) usage_error \"unknown option: $1\" ;;
  esac
done

REPO_ROOT=$(repo_root_from_script)
require_repo_root \"$REPO_ROOT\"
[ -n \"$ARTIFACT_DIR\" ] || usage_error \"--artifact-dir is required\"
[ -n \"$WORK_DIR\" ] || usage_error \"--work-dir is required\"
[ -n \"$DEB_SYSTEM\" ] || usage_error \"--deb-system is required\"
[ -n \"$DEB_RELEASE\" ] || usage_error \"--deb-release is required\"
[ -n \"$DEB_ARCH\" ] || usage_error \"--deb-arch is required\"
validate_deb_system \"$DEB_SYSTEM\"
validate_deb_release \"$DEB_RELEASE\"
validate_cache_mode \"$CACHE_MODE\"
NORMALIZED_ARCH=$(normalize_arch \"$DEB_ARCH\")
PACKAGE_NAME=$(package_name_for_cache_mode \"$CACHE_MODE\")
CONFLICTING_PACKAGE_NAME=$(conflicting_package_name_for_cache_mode \"$CACHE_MODE\")
if [ -n \"$SOURCE_ARCHIVE\" ] && [ \"$SOURCE_URL_EXPLICIT\" = 1 ]; then
  usage_error \"use either --source-archive or --source-url, not both\"
fi

maybe_require_exe \"$DRY_RUN\" tar
maybe_require_exe \"$DRY_RUN\" make
maybe_require_exe \"$DRY_RUN\" dpkg-deb

SOURCE_WORK=\"$WORK_DIR/source\"
SOURCE_PATH=\"$SOURCE_WORK/$SOURCE_ARCHIVE_NAME\"
EXTRACT_ROOT=\"$WORK_DIR/source-tree\"
STAGE_ROOT=\"$WORK_DIR/deb-root\"
DEBIAN_DIR=\"$STAGE_ROOT/DEBIAN\"
DEB_NAME=$(deb_name_for_arch \"$NORMALIZED_ARCH\" \"$DEB_RELEASE\" \"$DEB_SYSTEM\" \"$CACHE_MODE\")
DEB_VERSION=$(deb_package_version \"$DEB_RELEASE\" \"$DEB_SYSTEM\")

printf 'Repository root: %s\\n' \"$REPO_ROOT\"
printf 'DEB system: %s\\n' \"$DEB_SYSTEM\"
printf 'DEB release: %s\\n' \"$DEB_RELEASE\"
printf 'DEB version: %s\\n' \"$DEB_VERSION\"
printf 'DEB cache mode: %s\\n' \"$CACHE_MODE\"
printf 'DEB package name: %s\\n' \"$PACKAGE_NAME\"
printf 'Source archive: %s\\n' \"${{SOURCE_ARCHIVE:-$SOURCE_URL}}\"
printf 'DEB output: %s\\n' \"$ARTIFACT_DIR/$DEB_NAME\"

if [ \"$DRY_RUN\" = 0 ]; then
  reset_output_dir 0 \"$SOURCE_WORK\"
  reset_output_dir 0 \"$EXTRACT_ROOT\"
  reset_output_dir 0 \"$STAGE_ROOT\"
fi
prepare_source_archive \"$DRY_RUN\" \"$SOURCE_ARCHIVE\" \"$SOURCE_URL\" \"$SOURCE_PATH\"

if [ \"$DRY_RUN\" = 1 ]; then
  printf 'Would extract source archive into: %s\\n' \"$EXTRACT_ROOT\"
  printf 'Would build install root: %s\\n' \"$STAGE_ROOT\"
  printf 'Would write Debian control metadata: %s\\n' \"$DEBIAN_DIR/control\"
  printf 'Would configure cache mode: %s\\n' \"$CACHE_MODE\"
  printf 'Would build DEB artifact: %s\\n' \"$ARTIFACT_DIR/$DEB_NAME\"
  exit 0
fi

tar -xzf \"$SOURCE_PATH\" -C \"$EXTRACT_ROOT\"
mapfile -t source_dirs < <(find \"$EXTRACT_ROOT\" -mindepth 1 -maxdepth 1 -type d | sort)
if [ \"${{#source_dirs[@]}}\" -ne 1 ]; then
  printf 'Expected exactly one extracted source directory, got %s\\n' \"${{#source_dirs[@]}}\" >&2
  printf '  %s\\n' \"${{source_dirs[@]}}\" >&2
  exit 1
fi
SOURCE_DIR=\"${{source_dirs[0]}}\"

sed -i 's|))$|) (default-scope . \"installation\") (compiled-file-cache-roots . (user system)) (compiled-file-system-cache-root . \"/var/cache/racket/compiled\"))|' \"$SOURCE_DIR/etc/config.rktd\"
sed -i 's/\"1[.]1\"/\"3\"/g' \"$SOURCE_DIR/collects/openssl/libssl.rkt\" \"$SOURCE_DIR/collects/openssl/libcrypto.rkt\"
cd \"$SOURCE_DIR/src\"
./configure \\
  --disable-debug \\
  --disable-dependency-tracking \\
  --enable-origtree=no \\
  --enable-sharezo \\
  --prefix=\"$PREFIX\" \\
  --sysconfdir=/etc \\
  --enable-useprefix
make -j\"$JOBS\"
make install DESTDIR=\"$STAGE_ROOT\"
cd \"$REPO_ROOT\"
find \"$STAGE_ROOT\" -type d -name compiled ! -path '*/info-domain/compiled' -prune -exec rm -rf {{}} +
if [ \"$CACHE_MODE\" = cached ]; then
  build_staged_system_cache \"$STAGE_ROOT\" \"$PREFIX\"
fi

if ! find \"$STAGE_ROOT\" -mindepth 1 -maxdepth 1 -print -quit | grep -q .; then
  die \"staged package root is empty: $STAGE_ROOT\"
fi
mkdir -p \"$DEBIAN_DIR\"
cat > \"$DEBIAN_DIR/control\" <<CONTROL
Package: $PACKAGE_NAME
Version: $DEB_VERSION
Section: devel
Priority: optional
Architecture: $NORMALIZED_ARCH
Maintainer: $PACKAGE_MAINTAINER
Homepage: $PACKAGE_HOMEPAGE
Conflicts: $CONFLICTING_PACKAGE_NAME
Replaces: $CONFLICTING_PACKAGE_NAME
CONTROL
if [ \"$CACHE_MODE\" = cached ]; then
  cat >> \"$DEBIAN_DIR/control\" <<CONTROL
Provides: $BASE_PACKAGE_NAME
CONTROL
fi
cat >> \"$DEBIAN_DIR/control\" <<CONTROL
Depends: libc6, libedit2, libffi8, libssl3, libsqlite3-0, zlib1g
Description: $PACKAGE_SUMMARY
 Racket packaged from a stable source release archive.
CONTROL
if [ \"$CACHE_MODE\" = postinstall ]; then
cat > \"$DEBIAN_DIR/postinst\" <<'POSTINST'
#!/bin/sh
set -e
if [ \"$1\" = \"configure\" ]; then
  raco setup --system --no-user --reset-cache -D --no-pkg-deps --no-launcher
  compiled_cache_root=\"/var/cache/racket/compiled\"
  mkdir -p \"$compiled_cache_root\"
  empty_home=$(mktemp -d)
  if ! HOME=\"$empty_home\" PLTCOMPILEDROOTS=\"$compiled_cache_root\" rhombus --version >/dev/null; then
    rm -rf \"$empty_home\"
    exit 1
  fi
  if ! HOME=\"$empty_home\" PLTCOMPILEDROOTS=\"$compiled_cache_root\" rhombus -e 'println(\"package-racket-rhombus-cache\")' >/dev/null; then
    rm -rf \"$empty_home\"
    exit 1
  fi
  rm -rf \"$empty_home\"
fi
exit 0
POSTINST
else
cat > \"$DEBIAN_DIR/postinst\" <<'POSTINST'
#!/bin/sh
set -e
exit 0
POSTINST
fi
chmod 755 \"$DEBIAN_DIR/postinst\"

if [ \"$CACHE_MODE\" = postinstall ]; then
cat > \"$DEBIAN_DIR/prerm\" <<'PRERM'
#!/bin/sh
set -e
package_present() {{
  dpkg-query -W -f='${{db:Status-Abbrev}}' \"$1\" 2>/dev/null | grep -q '^i'
}}
if [ \"$1\" = \"remove\" ] || [ \"$1\" = \"deconfigure\" ]; then
  if ! package_present \"{(cached-package-name (cfg-package-name c))}\"; then
    if command -v raco >/dev/null 2>&1; then
      raco setup --system --delete-cache || true
    fi
    rm -rf /usr/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod
    rmdir /usr/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral /usr/share/racket/pkgs/rhombus-lib/rhombus/private/compiled 2>/dev/null || true
  fi
fi
exit 0
PRERM
else
cat > \"$DEBIAN_DIR/prerm\" <<'PRERM'
#!/bin/sh
set -e
exit 0
PRERM
fi
chmod 755 \"$DEBIAN_DIR/prerm\"
cat > \"$DEBIAN_DIR/postrm\" <<'POSTRM'
#!/bin/sh
set -e
OTHER_RACKET_PACKAGE='@OTHER_RACKET_PACKAGE@'
package_present() {{
  dpkg-query -W -f='${{db:Status-Abbrev}}' \"$1\" 2>/dev/null | grep -q '^i'
}}
other_racket_package_present() {{
  package_present \"$OTHER_RACKET_PACKAGE\"
}}
if [ \"$1\" = \"remove\" ] || [ \"$1\" = \"purge\" ]; then
  if ! other_racket_package_present; then
    rm -rf /var/cache/racket/compiled
    rm -rf /usr/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod
    rmdir /usr/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral /usr/share/racket/pkgs/rhombus-lib/rhombus/private/compiled 2>/dev/null || true
  fi
fi
exit 0
POSTRM
sed -i \"s|@OTHER_RACKET_PACKAGE@|$CONFLICTING_PACKAGE_NAME|g\" \"$DEBIAN_DIR/postrm\"
chmod 755 \"$DEBIAN_DIR/postrm\"

(cd \"$STAGE_ROOT\" && find . -type f ! -path './DEBIAN/*' -print0 | sort -z | xargs -0 md5sum > DEBIAN/md5sums)
require_nonempty_file \"$DEBIAN_DIR/control\"
require_nonempty_file \"$DEBIAN_DIR/postinst\"
require_nonempty_file \"$DEBIAN_DIR/prerm\"
require_nonempty_file \"$DEBIAN_DIR/postrm\"
require_nonempty_file \"$DEBIAN_DIR/md5sums\"
mkdir -p \"$ARTIFACT_DIR\"
dpkg-deb --root-owner-group --build \"$STAGE_ROOT\" \"$ARTIFACT_DIR/$DEB_NAME\"
\"$REPO_ROOT/scripts/verify-deb.sh\" --deb \"$ARTIFACT_DIR/$DEB_NAME\" --deb-system \"$DEB_SYSTEM\" --deb-release \"$DEB_RELEASE\" --deb-arch \"$NORMALIZED_ARCH\" --cache-mode \"$CACHE_MODE\"
printf 'DEB package: %s\\n' \"$ARTIFACT_DIR/$DEB_NAME\"
")

(define (deb-verify-script-content c)
  f"{(deb-script-header "verify-deb.sh")}SCRIPT_DIR=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)
source \"$SCRIPT_DIR/deb-common.sh\"

usage() {{
  cat <<'USAGE'
Usage: scripts/verify-deb.sh --deb PATH --deb-system SYSTEM --deb-release RELEASE --deb-arch ARCH [--cache-mode MODE] [--dry-run]

Validate .deb filename and metadata.
USAGE
}}

DRY_RUN=0
DEB_PATH=
DEB_SYSTEM=
DEB_RELEASE=
DEB_ARCH=
CACHE_MODE=\"$DEFAULT_CACHE_MODE\"

while [ $# -gt 0 ]; do
  case \"$1\" in
    --deb) [ $# -ge 2 ] || usage_error \"missing value for --deb\"; DEB_PATH=\"$2\"; shift 2 ;;
    --deb-system) [ $# -ge 2 ] || usage_error \"missing value for --deb-system\"; DEB_SYSTEM=\"$2\"; shift 2 ;;
    --deb-release) [ $# -ge 2 ] || usage_error \"missing value for --deb-release\"; DEB_RELEASE=\"$2\"; shift 2 ;;
    --deb-arch) [ $# -ge 2 ] || usage_error \"missing value for --deb-arch\"; DEB_ARCH=\"$2\"; shift 2 ;;
    --cache-mode) [ $# -ge 2 ] || usage_error \"missing value for --cache-mode\"; CACHE_MODE=\"$2\"; shift 2 ;;
    --dry-run) DRY_RUN=1; shift ;;
    --help) usage; exit 0 ;;
    *) usage_error \"unknown option: $1\" ;;
  esac
done

REPO_ROOT=$(repo_root_from_script)
require_repo_root \"$REPO_ROOT\"
[ -n \"$DEB_PATH\" ] || usage_error \"--deb is required\"
[ -n \"$DEB_SYSTEM\" ] || usage_error \"--deb-system is required\"
[ -n \"$DEB_RELEASE\" ] || usage_error \"--deb-release is required\"
[ -n \"$DEB_ARCH\" ] || usage_error \"--deb-arch is required\"
validate_deb_system \"$DEB_SYSTEM\"
validate_deb_release \"$DEB_RELEASE\"
validate_cache_mode \"$CACHE_MODE\"
NORMALIZED_ARCH=$(normalize_arch \"$DEB_ARCH\")
PACKAGE_NAME=$(package_name_for_cache_mode \"$CACHE_MODE\")
DEB_VERSION=$(deb_package_version \"$DEB_RELEASE\" \"$DEB_SYSTEM\")
EXPECTED_DEB=$(deb_name_for_arch \"$NORMALIZED_ARCH\" \"$DEB_RELEASE\" \"$DEB_SYSTEM\" \"$CACHE_MODE\")

if [ \"$DRY_RUN\" = 1 ]; then
  printf 'Would verify DEB: %s\\n' \"$DEB_PATH\"
  printf 'Would expect DEB basename: %s\\n' \"$EXPECTED_DEB\"
  printf 'Would expect DEB version: %s\\n' \"$DEB_VERSION\"
  printf 'Would expect DEB cache mode: %s\\n' \"$CACHE_MODE\"
  printf 'Would expect DEB package name: %s\\n' \"$PACKAGE_NAME\"
  exit 0
fi

require_exe dpkg-deb
require_exe tar
require_nonempty_file \"$DEB_PATH\"
[ \"$(basename \"$DEB_PATH\")\" = \"$EXPECTED_DEB\" ] || die \"DEB basename does not match expected $EXPECTED_DEB: $DEB_PATH\"

package=$(dpkg-deb --field \"$DEB_PATH\" Package)
version=$(dpkg-deb --field \"$DEB_PATH\" Version)
arch=$(dpkg-deb --field \"$DEB_PATH\" Architecture)
[ \"$package\" = \"$PACKAGE_NAME\" ] || die \"DEB Package field mismatch: expected $PACKAGE_NAME got $package\"
[ \"$version\" = \"$DEB_VERSION\" ] || die \"DEB Version field mismatch: expected $DEB_VERSION got $version\"
[ \"$arch\" = \"$NORMALIZED_ARCH\" ] || die \"DEB Architecture field mismatch: expected $NORMALIZED_ARCH got $arch\"
contents=$(dpkg-deb --contents \"$DEB_PATH\")
control_files=$(dpkg-deb --ctrl-tarfile \"$DEB_PATH\" | tar -tf -)
if printf '%s\\n' \"$contents\" | grep -E '(^|[[:space:]])\\./var/cache/racket/racket-compiled-cache[.]log$' >/dev/null; then
  die \"DEB payload unexpectedly includes racket compiled cache debug log\"
fi
for script in ./postinst ./prerm ./postrm; do
  printf '%s\\n' \"$control_files\" | grep -Fx \"$script\" >/dev/null \\
    || die \"DEB control archive missing $script\"
done
postinst_content=$(dpkg-deb --ctrl-tarfile \"$DEB_PATH\" | tar -xOf - ./postinst)
if [ \"$CACHE_MODE\" = postinstall ]; then
  printf '%s\\n' \"$postinst_content\" | grep -F 'raco setup --system --no-user --reset-cache -D --no-pkg-deps --no-launcher' >/dev/null \\
    || die \"DEB postinst does not build the system compiled cache\"
  printf '%s\\n' \"$postinst_content\" | grep -F 'package-racket-rhombus-cache' >/dev/null \\
    || die \"DEB postinst does not warm the Rhombus demod cache\"
  printf '%s\\n' \"$postinst_content\" | grep -F 'PLTCOMPILEDROOTS=\"$compiled_cache_root\" rhombus --version' >/dev/null \\
    || die \"DEB postinst does not warm the Rhombus version cache into the system cache\"
  if printf '%s\\n' \"$contents\" | grep -E '(^|[[:space:]])\\./var/cache/racket/compiled/.+[.]zo$' >/dev/null; then
    die \"postinstall DEB payload unexpectedly includes system compiled cache .zo files\"
  fi
else
  if printf '%s\\n' \"$postinst_content\" | grep -F 'raco setup --system --no-user --reset-cache -D --no-pkg-deps' >/dev/null; then
    die \"cached DEB postinst unexpectedly builds the system compiled cache\"
  fi
  printf '%s\\n' \"$contents\" | grep -E '(^|[[:space:]])\\./var/cache/racket/compiled/.+[.]zo$' >/dev/null \\
    || die \"cached DEB payload does not include system compiled cache .zo files\"
  runtime_collects_cache=\"./var/cache/racket/compiled/${{DEFAULT_PREFIX#/}}/share/racket/collects\"
  printf '%s\\n' \"$contents\" | grep -F \"$runtime_collects_cache/\" | grep -E '[.]zo$' >/dev/null \\
    || die \"cached DEB payload does not include runtime-keyed collects cache .zo files\"
  runtime_pkgs_cache=\"./var/cache/racket/compiled/${{DEFAULT_PREFIX#/}}/share/racket/pkgs\"
  printf '%s\\n' \"$contents\" | grep -F \"$runtime_pkgs_cache/\" | grep -E '[.]zo$' >/dev/null \\
    || die \"cached DEB payload does not include runtime-keyed package cache .zo files\"
  rhombus_ephemeral_cache=\"./${{DEFAULT_PREFIX#/}}/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod\"
  printf '%s\\n' \"$contents\" | grep -F \"$rhombus_ephemeral_cache/\" | grep -E '[.]zo$' >/dev/null \\
    || die \"cached DEB payload does not include Rhombus demod cache .zo files\"
  runtime_rhombus_collects_cache=\"$rhombus_ephemeral_cache/linklet/${{DEFAULT_PREFIX#/}}/share/racket/collects\"
  printf '%s\\n' \"$contents\" | grep -F \"$runtime_rhombus_collects_cache/\" | grep -E '[.]zo$' >/dev/null \\
    || die \"cached DEB payload does not include runtime-keyed Rhombus demod collects cache .zo files\"
  runtime_rhombus_pkgs_cache=\"$rhombus_ephemeral_cache/linklet/${{DEFAULT_PREFIX#/}}/share/racket/pkgs\"
  printf '%s\\n' \"$contents\" | grep -F \"$runtime_rhombus_pkgs_cache/\" | grep -E '[.]zo$' >/dev/null \\
    || die \"cached DEB payload does not include runtime-keyed Rhombus demod package cache .zo files\"
  runtime_rhombus_native_collects_cache=\"$rhombus_ephemeral_cache/native/${{DEFAULT_PREFIX#/}}/share/racket/collects\"
  printf '%s\\n' \"$contents\" | grep -F \"$runtime_rhombus_native_collects_cache/\" | grep -E '[.]zo$' >/dev/null \\
    || die \"cached DEB payload does not include runtime-keyed Rhombus demod native collects cache .zo files\"
  runtime_rhombus_native_pkgs_cache=\"$rhombus_ephemeral_cache/native/${{DEFAULT_PREFIX#/}}/share/racket/pkgs\"
  printf '%s\\n' \"$contents\" | grep -F \"$runtime_rhombus_native_pkgs_cache/\" | grep -E '[.]zo$' >/dev/null \\
    || die \"cached DEB payload does not include runtime-keyed Rhombus demod native package cache .zo files\"
  if printf '%s\\n' \"$contents\" | grep -F \"$rhombus_ephemeral_cache/\" | grep -F '/deb-root/' >/dev/null; then
    die \"cached DEB payload includes buildroot-keyed Rhombus demod cache paths\"
  fi
fi
prerm_content=$(dpkg-deb --ctrl-tarfile \"$DEB_PATH\" | tar -xOf - ./prerm)
if [ \"$CACHE_MODE\" = postinstall ]; then
  printf '%s\\n' \"$prerm_content\" | grep -F 'raco setup --system --delete-cache' >/dev/null \\
    || die \"DEB prerm does not delete the system compiled cache\"
  printf '%s\\n' \"$prerm_content\" | grep -F 'package_present' >/dev/null \\
    || die \"DEB prerm does not guard cache deletion for package replacement\"
else
  if printf '%s\\n' \"$prerm_content\" | grep -F 'raco setup --system --delete-cache' >/dev/null; then
    die \"cached DEB prerm unexpectedly deletes the system compiled cache through raco\"
  fi
fi
postrm_content=$(dpkg-deb --ctrl-tarfile \"$DEB_PATH\" | tar -xOf - ./postrm)
if [ \"$CACHE_MODE\" = cached ]; then
  other_package=\"$BASE_PACKAGE_NAME\"
else
  other_package=\"$CACHED_PACKAGE_NAME\"
fi
printf '%s\\n' \"$postrm_content\" | grep -F 'rm -rf /var/cache/racket/compiled' >/dev/null \\
  || die \"DEB postrm does not purge the system compiled cache directory\"
printf '%s\\n' \"$postrm_content\" | grep -F 'rhombus-lib/rhombus/private/compiled/ephemeral/demod' >/dev/null \\
  || die \"DEB postrm does not purge the Rhombus demod cache directory\"
printf '%s\\n' \"$postrm_content\" | grep -F 'rmdir /usr/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral' >/dev/null \\
  || die \"DEB postrm does not remove empty Rhombus ephemeral cache parents\"
printf '%s\\n' \"$postrm_content\" | grep -F 'other_racket_package_present' >/dev/null \\
  || die \"DEB postrm does not guard shared cache deletion for package replacement\"
printf '%s\\n' \"$postrm_content\" | grep -F \"OTHER_RACKET_PACKAGE='$other_package'\" >/dev/null \\
  || die \"DEB postrm does not guard shared cache deletion with the other package\"
if printf '%s\\n' \"$postrm_content\" | grep -F '@OTHER_RACKET_PACKAGE@' >/dev/null; then
  die \"DEB postrm contains unreplaced other package placeholder\"
fi
printf 'Validated DEB: %s\\n' \"$DEB_PATH\"
")

(define (assert-deb-repo-root! c #:write? [write? #t])
  (begin
    (define root (cfg-deb-repo-root c))
    (assert-directory 'deb-repo root)
    (assert-directory 'deb-repo (build-path root ".git"))
    (when write?
      (assert-writable-directory 'deb-repo root)
    ) ; end when write check requested
  ) ; end begin assert-deb-repo-root!
) ; end define assert-deb-repo-root!

(define (validate-generated-deb-script! c name required-needles)
  (begin
    (define path (deb-script-path c name))
    (assert-nonempty-file 'validate-deb-spec-scaffold! path)
    (define content (file->string path))
    (define bits (file-or-directory-permissions path 'bits))
    (unless (not (zero? (bitwise-and bits #o111)))
      (raise-user-error 'validate-deb-spec-scaffold!
                        f"generated script is not executable: {(clean-path-string path)}")
    ) ; end unless executable bit present
    (for ([needle (in-list (cons "set -euo pipefail" required-needles))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-deb-spec-scaffold!
                          f"generated script {name} is missing: {needle}")
      ) ; end unless script content contains needle
    ) ; end for script needle
  ) ; end begin validate-generated-deb-script!
) ; end define validate-generated-deb-script!

(define (deb-spec-output-paths c)
  (list (deb-repo-gitignore-path c)
        (deb-definition-source-keep-path c)
        (deb-script-path c "deb-common.sh")
        (deb-script-path c "build-deb.sh")
        (deb-script-path c "verify-deb.sh")
        (deb-repo-readme-path c)))

(define (validate-deb-spec-scaffold! c)
  (begin
    (assert-nonempty-file 'validate-deb-spec-scaffold! (deb-repo-readme-path c))
    (assert-nonempty-file 'validate-deb-spec-scaffold! (deb-repo-gitignore-path c))
    (assert-file 'validate-deb-spec-scaffold! (deb-definition-source-keep-path c))
    (define readme-content (file->string (deb-repo-readme-path c)))
    (for ([needle (in-list (list "Debian package build-script repository"
                                 "not an apt repository"
                                 "scripts/build-deb.sh"
                                 "scripts/verify-deb.sh"))])
      (unless (string-contains? readme-content needle)
        (raise-user-error 'validate-deb-spec-scaffold!
                          f"generated README is missing: {needle}")
      ) ; end unless readme content contains needle
    ) ; end for readme needle
    (validate-generated-deb-script! c
	                                    "deb-common.sh"
	                                    '("prepare_source_archive"
	                                      "validate_source_archive"
	                                      "require_repo_root"
	                                      "validate_cache_mode"
		                                      "build_staged_system_cache"
		                                      "warm_staged_rhombus_cache"
		                                      "find_staged_collects_dir"
		                                      "write_staged_config"
		                                      "normalize_staged_system_cache"
		                                      "normalize_staged_rhombus_cache"
		                                      "rewrite_staged_cache_dep_paths"
		                                      "touch_cache_zos_after_deps"
		                                      "grep -RFl --include '*.dep'"
		                                      "find \"$cache_root\" -type f -name '*.zo' -exec touch -d \"@$touch_seconds\""
		                                      "require_staged_rhombus_cache_root"
		                                      "runtime-keyed staged system compiled cache"
		                                      "runtime-keyed staged package compiled cache"
		                                      "staged compiled cache dependency metadata contains buildroot paths"
		                                      "runtime-keyed staged Rhombus demod $cache_kind collects cache"
		                                      "runtime-keyed staged Rhombus demod $cache_kind package cache"
		                                      "staged Rhombus demod cache contains buildroot-keyed paths"
		                                      "pkgs-dir"
		                                      "racket-compiled-cache.log"
		                                      "-X \"$collects_dir\" -G \"$config_dir\""
		                                      "raco setup -j 1 --system --no-user --reset-cache -D --no-pkg-deps --no-launcher"
		                                      "PLTCOMPILEDROOTS=\"$runtime_cache_root\""
		                                      "\"$runtime_rhombus_bin\" -e"
		                                      "replace_config_value"
		                                      "racket9-cached"
		                                      "deb_name_for_arch"))
    (validate-generated-deb-script! c
                                    "build-deb.sh"
                                    '("Usage: scripts/build-deb.sh"
                                     "dpkg-deb --root-owner-group --build"
                                     "Depends: libc6, libedit2"
	                                      "compiled-file-cache-roots"
	                                      "--enable-sharezo"
	                                      "find \"$STAGE_ROOT\" -type d -name compiled ! -path '*/info-domain/compiled'"
		                                      "--cache-mode"
		                                      "build_staged_system_cache"
		                                      "PLTCOMPILEDROOTS=\"$compiled_cache_root\" rhombus -e"
		                                      "package-racket-rhombus-cache"
		                                      "$DEBIAN_DIR/postinst"
	                                      "raco setup --system --no-user --reset-cache -D --no-pkg-deps --no-launcher"
	                                      "$DEBIAN_DIR/prerm"
	                                      "raco setup --system --delete-cache"
	                                      "package_present"
	                                      "other_racket_package_present"
	                                      "OTHER_RACKET_PACKAGE="
	                                      "$DEBIAN_DIR/postrm"
	                                      "rm -rf /var/cache/racket/compiled"
	                                      "rhombus-lib/rhombus/private/compiled/ephemeral/demod"
	                                      "rmdir /usr/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral"
	                                     "--dry-run"))
    (validate-generated-deb-script! c
                                    "verify-deb.sh"
                                    '("dpkg-deb --field"
                                      "dpkg-deb --contents"
                                      "dpkg-deb --ctrl-tarfile"
	                                      "DEB control archive missing"
	                                      "DEB postinst does not build the system compiled cache"
	                                      "DEB postinst does not warm the Rhombus demod cache"
	                                      "DEB postinst does not warm the Rhombus version cache into the system cache"
	                                      "DEB prerm does not guard cache deletion for package replacement"
		                                      "cached DEB payload does not include system compiled cache"
			                                      "cached DEB payload does not include runtime-keyed collects cache"
			                                      "cached DEB payload does not include runtime-keyed package cache"
			                                      "cached DEB payload does not include Rhombus demod cache"
			                                      "cached DEB payload does not include runtime-keyed Rhombus demod collects cache"
			                                      "cached DEB payload does not include runtime-keyed Rhombus demod package cache"
			                                      "cached DEB payload does not include runtime-keyed Rhombus demod native collects cache"
			                                      "cached DEB payload does not include runtime-keyed Rhombus demod native package cache"
			                                      "cached DEB payload includes buildroot-keyed Rhombus demod cache paths"
			                                      "DEB postrm does not purge the Rhombus demod cache directory"
		                                      "DEB postrm does not remove empty Rhombus ephemeral cache parents"
		                                      "DEB postrm does not guard shared cache deletion for package replacement"
		                                      "DEB postrm does not guard shared cache deletion with the other package"
		                                      "DEB postrm contains unreplaced other package placeholder"
		                                      "racket compiled cache debug log"
	                                      "--cache-mode"
	                                      "--dry-run"))
  ) ; end begin validate-deb-spec-scaffold!
) ; end define validate-deb-spec-scaffold!

(define (write-deb-spec-scaffold! c)
  (begin
    (assert-deb-repo-root! c #:write? #t)
    (make-directory* (deb-sources-dir c))
    (make-directory* (deb-scripts-dir c))
    (define source-url (formula-source-url c))
    (define source-sha256
      (resolve-source-archive-sha256! 'write-deb-spec-scaffold!
                                      c
                                      source-url
                                      "DEB source archive"))
    (write-text-file! (deb-repo-gitignore-path c) (deb-repo-gitignore-content))
    (write-text-file! (deb-definition-source-keep-path c) "")
    (write-executable-text-file! (deb-script-path c "deb-common.sh")
                                 (deb-common-script-content c source-sha256))
    (write-executable-text-file! (deb-script-path c "build-deb.sh")
                                 (deb-build-script-content c))
    (write-executable-text-file! (deb-script-path c "verify-deb.sh")
                                 (deb-verify-script-content c))
    (write-text-file! (deb-repo-readme-path c) (deb-readme-content c))
    (validate-deb-spec-scaffold! c)
    (println/flush f"Generated DEB scaffold: {(clean-path-string (cfg-deb-repo-root c))}")
  ) ; end begin write-deb-spec-scaffold!
) ; end define write-deb-spec-scaffold!

(define (print-deb-spec-scaffold-dry-run! c)
  (begin
    (assert-deb-repo-root! c #:write? #f)
    (println/flush f"Would generate DEB scaffold in: {(clean-path-string (cfg-deb-repo-root c))}")
    (for ([path (in-list (deb-spec-output-paths c))])
      (println/flush f"Would generate DEB file: {(clean-path-string path)}")
    ) ; end for deb spec output path
  ) ; end begin print-deb-spec-scaffold-dry-run!
) ; end define print-deb-spec-scaffold-dry-run!

(define (build-deb-spec! c)
  (begin
    (println/flush f"DEB config: {(clean-path-string (cfg-deb-repo-config c))}")
    (println/flush f"DEB root: {(clean-path-string (cfg-deb-repo-root c))}")
    (println/flush f"DEB package: {(cfg-package-name c)}")
    (if (cfg-dry-run? c)
        (print-deb-spec-scaffold-dry-run! c)
        (write-deb-spec-scaffold! c))
  ) ; end begin build-deb-spec!
) ; end define build-deb-spec!

(define (rpm-source-archive-name c)
  (brew-source-tgz-name c))

(define (rpm-source-cache-path c source-url)
  (let-values ([(owner repo tag asset-name)
                (github-release-download-url-values 'rpm-source-cache-path source-url)])
    (build-path (cfg-work-dir c) "rpm-source" asset-name)
  ) ; end let-values rpm source cache path
)

(define (rpm-version c)
  (cfg-source-version c))

(define (rpm-release c)
  (rpm-full-release (cfg-rpm-release c) (cfg-rpm-system c)))

(define (rpm-spec-default-macro name value)
  (string-append "%{!?" name ":%global " name " " value "}"))

(define (rpm-shared-directory-shell-pattern)
  (string-join rpm-shared-directories "|"))

(define (rpm-shared-directory-egrep-pattern)
  (string-join rpm-shared-directories "|"))

(define (rpm-source-sha256/local c)
  (let ([source-path (brew-output-tgz c)])
    (if (file-exists? source-path)
        (sha256-file source-path)
        "")
  ) ; end let source path
) ; end define rpm-source-sha256/local

(define (rpm-spec-content c [source-url (formula-source-url c)]
                          [source-sha256 (rpm-source-sha256/local c)])
  f"%{{!?package_name:%global package_name {(cfg-package-name c)}}}
%{{!?cache_mode:%global cache_mode postinstall}}
%global base_package_name {(cfg-package-name c)}
%global cached_package_name {(cached-package-name (cfg-package-name c))}
Name: %{{package_name}}
Version: {(rpm-version c)}
{(rpm-spec-default-macro "package_system" (cfg-rpm-system c))}
{(rpm-spec-default-macro "package_release" (cfg-rpm-release c))}
Release: %{{package_release}}.%{{package_system}}
Summary: {(cfg-summary c)}
License: {(cfg-license c)}
URL: {(cfg-url c)}
Source0: {source-url}
AutoReqProv: no
Requires: libedit
%if \"%{{cache_mode}}\" == \"cached\"
Provides: %{{base_package_name}} = %{{version}}-%{{release}}
Conflicts: %{{base_package_name}}
%else
Conflicts: %{{cached_package_name}}
%endif
# Racket CS stores its boot image in the .rackboot ELF section. RPM debuginfo
# extraction removes that section on openEuler, so the package must keep debug
# data in the main executables.
%global debug_package %{{nil}}
%global __brp_compress %{{nil}}
%global package_prefix {(cfg-prefix c)}
%global source_sha256 {source-sha256}

%description
Racket packaged from a stable source release archive.

%prep
if [ -n \"%{{source_sha256}}\" ]; then
  if command -v sha256sum >/dev/null 2>&1; then
    actual=$(sha256sum %{{SOURCE0}} | cut -d ' ' -f 1)
  elif command -v shasum >/dev/null 2>&1; then
    actual=$(shasum -a 256 %{{SOURCE0}} | cut -d ' ' -f 1)
  else
    echo \"sha256 checker not found: sha256sum or shasum\" >&2
    exit 1
  fi
  if [ \"$actual\" != \"%{{source_sha256}}\" ]; then
    echo \"Source0 sha256 mismatch: expected %{{source_sha256}} but got $actual\" >&2
    exit 1
  fi
fi
%setup -q -n racket-{(cfg-source-version c)}

%build
sed -i 's|))$|) (default-scope . \"installation\") (compiled-file-cache-roots . (user system)) (compiled-file-system-cache-root . \"/var/cache/racket/compiled\"))|' etc/config.rktd
sed -i 's/\"1[.]1\"/\"3\"/g' collects/openssl/libssl.rkt collects/openssl/libcrypto.rkt
cd src
./configure \\
  --disable-debug \\
  --disable-dependency-tracking \\
  --enable-origtree=no \\
  --enable-sharezo \\
  --prefix=%{{package_prefix}} \\
  --sysconfdir=%{{_sysconfdir}} \\
  --enable-useprefix
make %{{?_smp_mflags}}

%install
rm -rf %{{buildroot}}
cd src
make install DESTDIR=%{{buildroot}}
cd ..
find \"%{{buildroot}}\" -type d -name compiled ! -path '*/info-domain/compiled' -prune -exec rm -rf {{}} +
%if \"%{{cache_mode}}\" == \"cached\"
config_dir=\"%{{buildroot}}%{{_sysconfdir}}/racket\"
config_file=\"$config_dir/config.rktd\"
runtime_config_dir=\"%{{_sysconfdir}}/racket\"
runtime_cache_parent=\"/var/cache/racket\"
runtime_cache_root=\"/var/cache/racket/compiled\"
staged_cache_parent=\"%{{buildroot}}$runtime_cache_parent\"
staged_cache_root=\"%{{buildroot}}$runtime_cache_root\"
racket_bin=\"%{{buildroot}}%{{package_prefix}}/bin/racket\"
runtime_share_dir=\"%{{package_prefix}}/share/racket\"
runtime_collects_dir=\"$runtime_share_dir/collects\"
runtime_lib_dir=\"%{{package_prefix}}/lib/racket\"
runtime_links=
[ -f \"$config_file\" ] || {{ echo \"missing staged config: $config_file\" >&2; exit 1; }}
[ -x \"$racket_bin\" ] || {{ echo \"missing staged racket: $racket_bin\" >&2; exit 1; }}
[ -d \"%{{buildroot}}$runtime_collects_dir\" ] || {{ echo \"missing staged collects: %{{buildroot}}$runtime_collects_dir\" >&2; exit 1; }}
[ -d \"%{{buildroot}}$runtime_lib_dir\" ] || {{ echo \"missing staged Racket lib directory: %{{buildroot}}$runtime_lib_dir\" >&2; exit 1; }}
cleanup_runtime_links() {{
  if [ -n \"${{runtime_links:-}}\" ]; then
    printf '%s\\n' \"$runtime_links\" | while IFS= read -r runtime_link; do
      [ -n \"$runtime_link\" ] || continue
      [ -L \"$runtime_link\" ] && rm -f \"$runtime_link\"
    done
  fi
}}
add_runtime_link() {{
  runtime_link_target=\"$1\"
  runtime_link_path=\"$2\"
  if [ -e \"$runtime_link_path\" ] || [ -L \"$runtime_link_path\" ]; then
    echo \"runtime staging link path already exists: $runtime_link_path\" >&2
    exit 1
  fi
  mkdir -p \"$(dirname \"$runtime_link_path\")\"
  ln -s \"$runtime_link_target\" \"$runtime_link_path\"
  runtime_links=\"$runtime_link_path
$runtime_links\"
}}
mkdir -p \"$staged_cache_parent\"
trap cleanup_runtime_links EXIT
add_runtime_link \"%{{buildroot}}$runtime_share_dir\" \"$runtime_share_dir\"
add_runtime_link \"%{{buildroot}}$runtime_lib_dir\" \"$runtime_lib_dir\"
add_runtime_link \"$config_dir\" \"$runtime_config_dir\"
add_runtime_link \"$staged_cache_parent\" \"$runtime_cache_parent\"
if ! \"$racket_bin\" -X \"$runtime_collects_dir\" -G \"$runtime_config_dir\" -N raco -l- raco setup --system --no-user --reset-cache -D --no-pkg-deps --no-launcher; then
  exit 1
fi
if ! \"$racket_bin\" -U -R \"$runtime_cache_root\" -X \"$runtime_collects_dir\" -G \"$runtime_config_dir\" -N rhombus -l- rhombus/run.rhm --version >/dev/null; then
  exit 1
fi
if ! \"$racket_bin\" -U -R \"$runtime_cache_root\" -X \"$runtime_collects_dir\" -G \"$runtime_config_dir\" -N rhombus -l- rhombus/run.rhm -e 'println(\"package-racket-rhombus-cache\")' >/dev/null; then
  exit 1
fi
rhombus_ephemeral_cache=\"%{{buildroot}}$runtime_share_dir/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod\"
find \"$rhombus_ephemeral_cache\" -path '*/compiled/*.zo' -type f -print -quit | grep -q . || {{ echo \"staged Rhombus demod cache is empty: $rhombus_ephemeral_cache\" >&2; exit 1; }}
cleanup_runtime_links
trap - EXIT
move_cache_tree() {{
  from_source=\"$1\"
  to_source=\"$2\"
  from=\"$staged_cache_root/${{from_source#/}}\"
  to=\"$staged_cache_root/${{to_source#/}}\"
  [ -e \"$from\" ] || return 0
  [ \"$from\" = \"$to\" ] && return 0
  mkdir -p \"$(dirname \"$to\")\"
  if [ -e \"$to\" ]; then
    cp -a \"$from\"/. \"$to\"/
    rm -rf \"$from\"
  else
    mv \"$from\" \"$to\"
  fi
}}
runtime_collects_dir=\"%{{package_prefix}}/share/racket/collects\"
runtime_pkgs_dir=\"%{{package_prefix}}/share/racket/pkgs\"
move_cache_tree \"%{{buildroot}}$runtime_collects_dir\" \"$runtime_collects_dir\"
move_cache_tree \"%{{buildroot}}$runtime_pkgs_dir\" \"$runtime_pkgs_dir\"
rm -f \"%{{buildroot}}/var/cache/racket/racket-compiled-cache.log\"
find \"$staged_cache_root\" -type d -empty -delete 2>/dev/null || :
runtime_collects_cache=\"$staged_cache_root/${{runtime_collects_dir#/}}\"
runtime_pkgs_cache=\"$staged_cache_root/${{runtime_pkgs_dir#/}}\"
find \"$runtime_collects_cache\" -path '*/compiled/*.zo' -type f -print -quit | grep -q . || {{ echo \"runtime-keyed staged system compiled cache is empty: $runtime_collects_cache\" >&2; exit 1; }}
find \"$runtime_pkgs_cache\" -path '*/compiled/*.zo' -type f -print -quit | grep -q . || {{ echo \"runtime-keyed staged package compiled cache is empty: $runtime_pkgs_cache\" >&2; exit 1; }}
%endif

manifest=\"%{{name}}.files\"
paths=\"%{{name}}.paths\"
: > \"$manifest\"
find \"%{{buildroot}}\" -mindepth 1 | sort > \"$paths\"
while IFS= read -r path; do
  rel=${{path#\"%{{buildroot}}\"}}
  [ -n \"$rel\" ] || continue
  case \"$rel\" in
    {(rpm-shared-directory-shell-pattern)}) continue ;;
  esac
  if [ -d \"$path\" ] && [ ! -L \"$path\" ]; then
    printf '%s %s\\n' '%%dir' \"$rel\" >> \"$manifest\"
  elif [ -f \"$path\" ] || [ -L \"$path\" ]; then
    printf '%s\\n' \"$rel\" >> \"$manifest\"
  else
    printf 'unsupported staged file type: %s\\n' \"$path\" >&2
    exit 1
  fi
done < \"$paths\"
grep -Eq '^(%dir )?({(rpm-shared-directory-egrep-pattern)})$' \"$manifest\" && exit 1

%if \"%{{cache_mode}}\" == \"postinstall\"
%posttrans
setup_jobs=
if [ -r /etc/os-release ]; then
  . /etc/os-release
  if [ \"${{ID:-}}\" = \"fedora\" ] && [ \"${{VERSION_ID:-}}\" = \"44\" ]; then
    setup_jobs=\"-j 1\"
  fi
fi
if [ -n \"$setup_jobs\" ]; then
  raco setup $setup_jobs --system --no-user --reset-cache -D --no-pkg-deps
else
  raco setup --system --no-user --reset-cache -D --no-pkg-deps
fi
compiled_cache_root=\"/var/cache/racket/compiled\"
mkdir -p \"$compiled_cache_root\"
empty_home=$(mktemp -d)
if ! HOME=\"$empty_home\" racket -U -R \"$compiled_cache_root\" -N rhombus -l- rhombus/run.rhm --version >/dev/null; then
  rm -rf \"$empty_home\"
  exit 1
fi
if ! HOME=\"$empty_home\" racket -U -R \"$compiled_cache_root\" -N rhombus -l- rhombus/run.rhm -e 'println(\"package-racket-rhombus-cache\")' >/dev/null; then
  rm -rf \"$empty_home\"
  exit 1
fi
rm -rf \"$empty_home\"
%endif

%if \"%{{cache_mode}}\" == \"postinstall\"
%preun
if [ \"$1\" = \"0\" ] && ! rpm -q --quiet %{{cached_package_name}} >/dev/null 2>&1 && command -v raco >/dev/null 2>&1; then
  raco setup --system --delete-cache || :
fi
%endif

%postun
%if \"%{{cache_mode}}\" == \"cached\"
other_package=\"%{{base_package_name}}\"
%else
other_package=\"%{{cached_package_name}}\"
%endif
if [ \"$1\" = \"0\" ] && ! rpm -q --quiet \"$other_package\" >/dev/null 2>&1; then
  rm -rf /var/cache/racket/compiled
  rm -rf %{{package_prefix}}/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod
  rmdir %{{package_prefix}}/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral %{{package_prefix}}/share/racket/pkgs/rhombus-lib/rhombus/private/compiled 2>/dev/null || :
fi

%files -f %{{name}}.files
%defattr(-,root,root,-)
")

(define (write-rpm-spec! c spec-path [source-url (formula-source-url c)]
                         [source-sha256 (resolve-rpm-source-sha256! c source-url)])
  (begin
    (write-text-file! spec-path (rpm-spec-content c source-url source-sha256))
    (validate-rpm-spec! c spec-path source-url)
  ) ; end begin write-rpm-spec!
) ; end define write-rpm-spec!

(define (validate-rpm-spec! c spec-path source-url)
  (begin
    (assert-nonempty-file 'validate-rpm-spec! spec-path)
    (define content (file->string spec-path))
    (for ([needle (in-list (list f"%{{!?package_name:%global package_name {(cfg-package-name c)}}}"
                                 "%{!?cache_mode:%global cache_mode postinstall}"
                                 f"%global base_package_name {(cfg-package-name c)}"
                                 f"%global cached_package_name {(cached-package-name (cfg-package-name c))}"
                                 "Name: %{package_name}"
                                 f"Version: {(rpm-version c)}"
                                 (rpm-spec-default-macro "package_system" (cfg-rpm-system c))
                                 (rpm-spec-default-macro "package_release" (cfg-rpm-release c))
                                 "Release: %{package_release}.%{package_system}"
                                 f"Source0: {source-url}"
                                 "Requires: libedit"
                                 "%if \"%{cache_mode}\" == \"cached\""
                                 "Provides: %{base_package_name} = %{version}-%{release}"
                                 "Conflicts: %{base_package_name}"
                                 "Conflicts: %{cached_package_name}"
                                 "%global __brp_compress %{nil}"
                                 "%global debug_package %{nil}"
                                 ".rackboot ELF section"
                                 "%global package_prefix"
                                 "%global source_sha256"
                                 "Source0 sha256 mismatch"
                                 "%setup -q -n racket-"
                                 "compiled-file-cache-roots"
                                 "--enable-sharezo"
                                 "./configure"
                                 "make install DESTDIR=%{buildroot}"
                                 "find \"%{buildroot}\" -type d -name compiled ! -path '*/info-domain/compiled'"
                                 "missing staged collects"
                                 "runtime_config_dir=\"%{_sysconfdir}/racket\""
                                 "add_runtime_link"
	                                 "runtime staging link path already exists"
	                                 "-X \"$runtime_collects_dir\" -G \"$runtime_config_dir\""
	                                 "-U -R \"$runtime_cache_root\""
	                                 "--no-launcher"
	                                 "-N rhombus -l- rhombus/run.rhm --version"
	                                 "racket -U -R \"$compiled_cache_root\""
	                                 "package-racket-rhombus-cache"
                                 "staged Rhombus demod cache"
                                 "runtime_pkgs_dir"
                                 "move_cache_tree"
                                 "runtime-keyed staged system compiled cache"
                                 "runtime-keyed staged package compiled cache"
                                 "racket-compiled-cache.log"
                                 "%posttrans"
                                 "/etc/os-release"
                                 "setup_jobs=\"-j 1\""
                                 "raco setup --system --no-user --reset-cache -D --no-pkg-deps"
                                 "%preun"
                                 "raco setup --system --delete-cache"
                                 "%postun"
                                 "other_package="
	                                 "rpm -q --quiet \"$other_package\" >/dev/null 2>&1"
	                                 "rm -rf /var/cache/racket/compiled"
	                                 "%{package_prefix}/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod"
	                                 "rmdir %{package_prefix}/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral"
	                                 "printf '%s %s\\n' '%%dir' \"$rel\" >> \"$manifest\""
                                 "%files -f %{name}.files"))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-rpm-spec!
                          f"generated RPM spec is missing: {needle}")
      ) ; end unless needle present
    ) ; end for needle
    (when (string-contains? content "printf '%%dir %s\\n'")
      (raise-user-error 'validate-rpm-spec!
                        "generated RPM spec must not put %dir in the printf format string")
    ) ; end when unsafe printf format
    (when (regexp-match? #px"(?m:^/usr$|^%dir /usr$)" content)
      (raise-user-error 'validate-rpm-spec!
                        "generated RPM spec must not claim the shared /usr directory")
    ) ; end when owns /usr
  ) ; end begin validate-rpm-spec!
) ; end define validate-rpm-spec!

(define (validate-rpm! c rpm-path)
  (begin
    (assert-nonempty-file 'validate-rpm! rpm-path)
    (define metadata
      (capture! 'validate-rpm! (cfg-rpm-bin c) (list "-qip" (clean-path-string rpm-path)))
    ) ; end define metadata
    (for ([needle (in-list (list f"Name        : {(cfg-package-name c)}"
                                 f"Version     : {(rpm-version c)}"
                                 f"Release     : {(rpm-release c)}"
                                 f"Architecture: {(cfg-rpm-arch c)}"))])
      (unless (string-contains? metadata needle)
        (raise-user-error 'validate-rpm!
                          f"RPM metadata is missing: {needle}")
      ) ; end unless metadata contains needle
    ) ; end for needle
    (define scripts
      (capture! 'validate-rpm! (cfg-rpm-bin c) (list "-qp" "--scripts" (clean-path-string rpm-path)))
    ) ; end define scripts
    (for ([needle (in-list (list "raco setup --system --no-user --reset-cache -D --no-pkg-deps"
                                 "raco setup --system --delete-cache"
                                 "rm -rf /var/cache/racket/compiled"))])
      (unless (string-contains? scripts needle)
        (raise-user-error 'validate-rpm!
                          f"RPM scriptlets are missing: {needle}")
      ) ; end unless scripts contains needle
    ) ; end for scriptlet needle
    (println/flush f"Validated .rpm: {(clean-path-string rpm-path)}")
  ) ; end begin validate-rpm!
) ; end define validate-rpm!

(define (rpm-package-name c)
  f"{(cfg-package-name c)}-{(rpm-version c)}-{(rpm-release c)}.{(cfg-rpm-arch c)}.rpm")

(define (rpm-package-path c)
  (build-path (cfg-artifact-dir c) (rpm-package-name c)))

(define (rpm-build-root c)
  (build-path (cfg-work-dir c) "rpm"))

(define (rpm-build-source-path c rpm-root)
  (build-path rpm-root "SOURCES" (rpm-source-archive-name c)))

(define (prepare-rpm-build-root! c rpm-root)
  (begin
    (reset-managed-dir! 'build-rpm! rpm-root)
    (for ([dir (in-list '("BUILD" "BUILDROOT" "RPMS" "SOURCES" "SPECS" "SRPMS"))])
      (make-directory* (build-path rpm-root dir))
    ) ; end for rpmbuild dir
  ) ; end begin prepare rpm build root
)

(define (rpm-existing-source-archive c source-url)
  (let ([local-source (brew-output-tgz c)])
    (cond
      [(file-exists? local-source) local-source]
      [else
       (let ([cached-source (rpm-source-cache-path c source-url)])
         (and (file-exists? cached-source) cached-source)
       ) ; end let cached source
      ]
    ) ; end cond existing source archive
  ) ; end let local source archive
)

(define (validate-rpm-source-archive! c archive)
  (begin
    (assert-nonempty-file 'validate-rpm-source-archive! archive)
    (run! 'validate-rpm-source-archive!
          (cfg-tar-bin c)
          (list "-tzf"
                (clean-path-string archive)
                f"racket-{(rpm-version c)}/src/configure")
          #:dry-run? #f)
    (run! 'validate-rpm-source-archive!
          (cfg-tar-bin c)
          (list "-tzf"
                (clean-path-string archive)
                f"racket-{(rpm-version c)}/collects/racket/main.rkt")
          #:dry-run? #f)
  ) ; end begin validate rpm source archive
)

(define (prepare-rpm-source-archive! c dest source-url expected-sha256)
  (begin
    (define existing-source (rpm-existing-source-archive c source-url))
    (make-directory* (path-only dest))
    (cond
      [existing-source
       (copy-file existing-source dest #t)
       (println/flush f"RPM Source0 from local file: {(clean-path-string existing-source)}")]
      [else
       (println/flush f"Downloading RPM Source0: {source-url}")
       (download-https-url! 'build-rpm! source-url dest)]
    ) ; end cond local or downloaded source
    (assert-nonempty-file 'build-rpm! dest)
    (define actual-sha256 (sha256-file dest))
    (unless (string=? actual-sha256 expected-sha256)
      (raise-user-error 'build-rpm!
                        f"RPM Source0 sha256 mismatch: expected {expected-sha256} but got {actual-sha256}")
    ) ; end unless source sha256 matches
    (validate-rpm-source-archive! c dest)
    (println/flush f"Prepared RPM Source0: {(clean-path-string dest)}")
  ) ; end begin prepare rpm source archive
)

(define (rpm-path-list-summary paths)
  (if (null? paths)
      "<none>"
      (string-join (map clean-path-string paths) ", ")))

(define (copy-built-rpm! c rpm-root)
  (begin
    (define rpms-dir (build-path rpm-root "RPMS"))
    (define expected-name (rpm-package-name c))
    (define rpms
      (sort (find-files (lambda (p)
                          (and (file-exists? p)
                               (regexp-match? #rx"[.]rpm$" (path->string p)))
                        ) ; end lambda p
                        rpms-dir)
            path<?)
    ) ; end define rpms
    (define matches
      (filter (lambda (rpm)
                (equal? (file-name-from-path rpm) (string->path expected-name)))
              rpms))
    (unless (= (length matches) 1)
      (raise-user-error 'build-rpm!
                        f"expected exactly one rpmbuild output named {expected-name}; observed: {(rpm-path-list-summary rpms)}")
    ) ; end unless exactly one expected rpm
    (define dest (rpm-package-path c))
    (copy-file (car matches) dest #t)
    (validate-rpm! c dest)
    (println/flush f"RPM package: {(clean-path-string dest)}")
  ) ; end begin copy-built-rpm!
) ; end define copy-built-rpm!

(define (build-rpm! c)
  (begin
    (define rpm-root (rpm-build-root c))
    (define sources-dir (build-path rpm-root "SOURCES"))
    (define specs-dir (build-path rpm-root "SPECS"))
    (define spec-path (build-path specs-dir f"{(cfg-package-name c)}.spec"))
    (define source-url (formula-source-url c))
    (define source-path (rpm-build-source-path c rpm-root))
    (define rpm-path (rpm-package-path c))
    (println/flush f"RPM spec: {(clean-path-string spec-path)}")
    (println/flush f"RPM source archive: {source-url}")
    (println/flush f"RPM package: {(clean-path-string rpm-path)}")
    (unless (cfg-dry-run? c)
      (make-directory* (cfg-artifact-dir c))
      (assert-executable 'build-rpm! (cfg-rpmbuild-bin c))
      (assert-executable 'build-rpm! (cfg-rpm-bin c))
      (assert-executable 'build-rpm! (cfg-tar-bin c))
      (prepare-rpm-build-root! c rpm-root)
    ) ; end unless dry-run prepare rpm
    (define source-sha256
      (if (cfg-dry-run? c)
          "<dry-run: source sha256 not resolved>"
          (resolve-rpm-source-sha256! c source-url)))
    (if (cfg-dry-run? c)
        (begin
          (println/flush f"Would reset RPM build root: {(clean-path-string rpm-root)}")
          (println/flush f"Would write RPM spec: {(clean-path-string spec-path)}")
          (println/flush f"Would prepare RPM Source0: {(clean-path-string source-path)}")
        ) ; end begin dry-run rpm source
        (begin
          (write-rpm-spec! c spec-path source-url source-sha256)
          (prepare-rpm-source-archive! c source-path source-url source-sha256)
        ) ; end begin materialize rpm source
    ) ; end if dry-run write source
    (run! 'build-rpm!
          (cfg-rpmbuild-bin c)
          (list "-bb"
                "--target" (cfg-rpm-arch c)
                "--define" f"_topdir {(clean-path-string rpm-root)}"
                "--define" "_build_id_links none"
                "--define" "_sysconfdir /etc"
                "--define" f"package_prefix {(cfg-prefix c)}"
                "--define" f"package_name {(cfg-package-name c)}"
                "--define" "cache_mode postinstall"
                "--define" f"package_system {(cfg-rpm-system c)}"
                "--define" f"package_release {(cfg-rpm-release c)}"
                "--define" f"_smp_mflags -j{(cfg-jobs c)}"
                (clean-path-string spec-path))
          #:dry-run? (cfg-dry-run? c))
    (if (cfg-dry-run? c)
        (println/flush f"Would copy RPM artifact to: {(clean-path-string rpm-path)}")
        (copy-built-rpm! c rpm-root))
  ) ; end begin build-rpm!
) ; end define build-rpm!

(define rpm-repo-supported-arches
  '("x86_64" "aarch64"))

(define generated-rpm-repo-notice-marker
  "GENERATED RPM PACKAGING METADATA - DO NOT EDIT IN rpm-racket.")

(define generated-rpm-repository-notice-marker
  "GENERATED RPM REPOSITORY METADATA - DO NOT EDIT IN rpm-racket.")

(define (rpm-spec-dir c)
  (build-path (cfg-rpm-repo-root c) "SPECS"))

(define (rpm-sources-dir c)
  (build-path (cfg-rpm-repo-root c) "SOURCES"))

(define (rpm-scripts-dir c)
  (build-path (cfg-rpm-repo-root c) "scripts"))

(define (rpm-definition-spec-path c)
  (build-path (rpm-spec-dir c) f"{(cfg-package-name c)}.spec"))

(define (rpm-definition-source-keep-path c)
  (build-path (rpm-sources-dir c) ".gitkeep"))

(define (rpm-script-path c name)
  (build-path (rpm-scripts-dir c) name))

(define (rpm-repo-arch-root c [arch (cfg-rpm-arch c)])
  (build-path (cfg-rpm-repo-root c) "repo" arch))

(define (rpm-repo-packages-dir c [arch (cfg-rpm-arch c)])
  (build-path (rpm-repo-arch-root c arch) "Packages"))

(define (rpm-repository-file-path c)
  (build-path (cfg-rpm-repo-root c) f"{(cfg-package-name c)}.repo"))

(define (rpm-repo-readme-path c)
  (build-path (cfg-rpm-repo-root c) "README.md"))

(define (rpm-repo-gitignore-path c)
  (build-path (cfg-rpm-repo-root c) ".gitignore"))

(define (rpm-spec-readme-content c)
  f"# rpm-racket

{generated-rpm-repo-notice-marker}

This repository is the RPM SPEC and build-script repository generated by
`package-racket`. It is not an RPM artifact repository. Treat `SPECS/`,
`SOURCES/`, `scripts/`, `.github/workflows/`, and `README.md` as outputs from
`package-racket`; do not hand-edit them for production changes. Change the
package-racket configuration and regenerate instead.

The generated build script supports two cache modes: `postinstall` builds the
default `{(cfg-package-name c)}` package and generates the system compiled cache
during package installation; `cached` builds `{(cached-package-name (cfg-package-name c))}`
and embeds the system compiled cache in the RPM payload.

## Layout

- `SPECS/{(cfg-package-name c)}.spec`: RPM build definition.
- `SOURCES/.gitkeep`: source placeholder; build scripts copy or download the
  stable source archive into their explicit work directory.
- `scripts/rpm-common.sh`: shared safety checks and staging helpers.
- `scripts/build-rpm.sh`: builds a binary RPM from the generated spec.
- `scripts/build-srpm.sh`: builds a source RPM from the same stable source
  archive.
- `scripts/verify-rpm.sh`: validates RPM name, metadata, arch, and payload
  ownership boundaries.
- `.github/workflows/build-rpm.yml`: builds configured RPM targets with GitHub
  Actions and uploads release assets after every target succeeds.

## Regenerate

Run from `package-racket` to overwrite the SPEC and scripts:

```sh
racket package-racket.rkt \\
  --target rpm-spec \\
  --prefix /usr \\
  --rpm-system {(cfg-rpm-system c)} \\
  --rpm-release {(cfg-rpm-release c)} \\
  --rpm-arch arm64 \\
  --rpm-repo-config {(clean-path-string (cfg-rpm-repo-config c))}
```

Run from `package-racket` to overwrite the generated RPM CI workflow:

```sh
racket package-racket.rkt \\
  --target rpm-ci \\
  --prefix /usr \\
  --rpm-repo-config {(clean-path-string (cfg-rpm-repo-config c))} \\
  --rpm-ci-config {(clean-path-string (cfg-rpm-ci-config c))}
```

## Build

Build a binary RPM on a target Linux host from the generated GitHub Release
source URL:

```sh
scripts/build-rpm.sh \\
  --artifact-dir /path/to/artifacts \\
  --work-dir /path/to/work \\
  --rpm-system {(cfg-rpm-system c)} \\
  --rpm-release {(cfg-rpm-release c)} \\
  --rpm-arch arm64 \\
  --cache-mode postinstall \\
  --prefix /usr
```

Use a local source archive for offline or pinned local builds:

```sh
scripts/build-rpm.sh \\
  --source-archive /path/to/{(brew-source-tgz-name c)} \\
  --artifact-dir /path/to/artifacts \\
  --work-dir /path/to/work \\
  --rpm-system {(cfg-rpm-system c)} \\
  --rpm-release {(cfg-rpm-release c)} \\
  --rpm-arch arm64 \\
  --cache-mode cached \\
  --prefix /usr
```

Supported RPM systems are `el9`, `fc40`, `fc43`, `fc44`, `openeuler2203`, and
`openeuler2403`. The generic `openeuler` value is intentionally rejected for
production artifacts. Common explicit target examples:

```sh
--rpm-system el9 --rpm-release {(cfg-rpm-release c)} --rpm-arch x86_64
--rpm-system fc40 --rpm-release {(cfg-rpm-release c)} --rpm-arch x86_64
--rpm-system fc43 --rpm-release {(cfg-rpm-release c)} --rpm-arch x86_64
--rpm-system fc44 --rpm-release {(cfg-rpm-release c)} --rpm-arch x86_64
--rpm-system openeuler2203 --rpm-release {(cfg-rpm-release c)} --rpm-arch x86_64
--rpm-system openeuler2203 --rpm-release {(cfg-rpm-release c)} --rpm-arch arm64
--rpm-system openeuler2403 --rpm-release {(cfg-rpm-release c)} --rpm-arch x86_64
--rpm-system openeuler2403 --rpm-release {(cfg-rpm-release c)} --rpm-arch arm64
```

Build the matching SRPM from the generated GitHub Release source URL:

```sh
scripts/build-srpm.sh \\
  --artifact-dir /path/to/artifacts \\
  --work-dir /path/to/work \\
  --rpm-system {(cfg-rpm-system c)} \\
  --rpm-release {(cfg-rpm-release c)} \\
  --rpm-arch arm64 \\
  --prefix /usr
```

Use a local source archive for the matching SRPM:

```sh
scripts/build-srpm.sh \\
  --source-archive /path/to/{(brew-source-tgz-name c)} \\
  --artifact-dir /path/to/artifacts \\
  --work-dir /path/to/work \\
  --rpm-system {(cfg-rpm-system c)} \\
  --rpm-release {(cfg-rpm-release c)} \\
  --rpm-arch arm64 \\
  --prefix /usr
```

Validate an existing RPM:

```sh
scripts/verify-rpm.sh \\
  --rpm /path/to/artifacts/{(cfg-package-name c)}-{(rpm-version c)}-{(rpm-release c)}.aarch64.rpm \\
  --rpm-system {(cfg-rpm-system c)} \\
  --rpm-release {(cfg-rpm-release c)} \\
  --rpm-arch arm64 \\
  --cache-mode postinstall
```
")

(define rpm-spec-gitignore-content
  ".DS_Store
*.tmp
*.swp
.*.swp
.commit
.build/
artifacts/
*.rpm
")

(define (rpm-script-header name)
  f"#!/usr/bin/env bash
set -euo pipefail

# {generated-rpm-repo-notice-marker}
# Generated entrypoint: {name}

")

(define (rpm-common-script-content c [source-sha256 (rpm-source-sha256/local c)])
  f"{(rpm-script-header "rpm-common.sh")}BASE_PACKAGE_NAME={(shell-single-quoted (cfg-package-name c))}
CACHED_PACKAGE_NAME={(shell-single-quoted (cached-package-name (cfg-package-name c)))}
PACKAGE_NAME=\"$BASE_PACKAGE_NAME\"
PACKAGE_VERSION={(shell-single-quoted (rpm-version c))}
PACKAGE_SOURCE_VERSION={(shell-single-quoted (cfg-source-version c))}
DEFAULT_RPM_SYSTEM={(shell-single-quoted (cfg-rpm-system c))}
DEFAULT_RPM_RELEASE={(shell-single-quoted (cfg-rpm-release c))}
DEFAULT_PREFIX={(shell-single-quoted (cfg-prefix c))}
DEFAULT_CACHE_MODE=postinstall
SOURCE_ARCHIVE_NAME={(shell-single-quoted (rpm-source-archive-name c))}
DEFAULT_SOURCE_URL={(shell-single-quoted (formula-source-url c))}
SOURCE_SHA256={(shell-single-quoted source-sha256)}
SPEC_NAME={(shell-single-quoted (string-append (cfg-package-name c) ".spec"))}

die() {{
  printf 'ERROR: %s\\n' \"$*\" >&2
  exit 1
}}

usage_error() {{
  die \"$1. Run with --help for usage.\"
}}

repo_root_from_script() {{
  local script_dir
  script_dir=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)
  CDPATH= cd -- \"$script_dir/..\" && pwd
}}

require_repo_root() {{
  local root=\"$1\"
  [ -d \"$root\" ] || die \"repository root does not exist: $root\"
  [ -f \"$root/SPECS/$SPEC_NAME\" ] || die \"missing spec file: $root/SPECS/$SPEC_NAME\"
  [ -f \"$root/scripts/rpm-common.sh\" ] || die \"missing common script: $root/scripts/rpm-common.sh\"
}}

require_file() {{
  [ -f \"$1\" ] || die \"file does not exist: $1\"
}}

require_nonempty_file() {{
  require_file \"$1\"
  [ -s \"$1\" ] || die \"file is empty: $1\"
}}

require_dir() {{
  [ -d \"$1\" ] || die \"directory does not exist: $1\"
}}

require_nonempty_dir() {{
  require_dir \"$1\"
  if ! find \"$1\" -mindepth 1 -maxdepth 1 -print -quit | grep -q .; then
    die \"directory is empty: $1\"
  fi
}}

require_absolute_path() {{
  case \"$1\" in
    /*) ;;
    *) die \"$2 must be an absolute path: $1\" ;;
  esac
}}

require_exe() {{
  command -v \"$1\" >/dev/null 2>&1 || die \"executable not found in PATH: $1\"
}}

maybe_require_exe() {{
  local dry_run=\"$1\"
  local exe=\"$2\"
  if [ \"$dry_run\" = 1 ]; then
    printf 'Would require executable: %s\\n' \"$exe\"
  else
    require_exe \"$exe\"
  fi
}}

run_cmd() {{
  local dry_run=\"$1\"
  shift
  printf '$'
  printf ' %q' \"$@\"
  printf '\\n'
  if [ \"$dry_run\" = 0 ]; then
    \"$@\"
  fi
}}

normalize_arch() {{
  case \"$1\" in
    x86_64|x64|amd64) printf 'x86_64\\n' ;;
    aarch64|arm64) printf 'aarch64\\n' ;;
    *) die \"rpm arch must be x86_64, amd64, x64, aarch64, or arm64: $1\" ;;
  esac
}}

validate_rpm_system() {{
  case \"$1\" in
    el9|fc40|fc43|fc44|openeuler2203|openeuler2403) ;;
    *) die \"rpm system must be el9, fc40, fc43, fc44, openeuler2203, or openeuler2403: $1\" ;;
  esac
}}

validate_rpm_release() {{
  local release=\"$1\"
  [ -n \"$release\" ] || die \"rpm release is required\"
  case \"$release\" in
    *.*) die \"rpm release must not contain . because system is appended separately: $release\" ;;
    [0-9]*) ;;
    *) die \"rpm release must start with a digit: $release\" ;;
  esac
  case \"$release\" in
    *[!A-Za-z0-9_+~-]*) die \"rpm release contains unsupported characters: $release\" ;;
  esac
}}

validate_cache_mode() {{
  case \"$1\" in
    postinstall|cached) ;;
    *) die \"cache mode must be postinstall or cached: $1\" ;;
  esac
}}

package_name_for_cache_mode() {{
  local mode=\"$1\"
  validate_cache_mode \"$mode\"
  case \"$mode\" in
    postinstall) printf '%s\\n' \"$BASE_PACKAGE_NAME\" ;;
    cached) printf '%s\\n' \"$CACHED_PACKAGE_NAME\" ;;
  esac
}}

rpm_full_release() {{
  local release=\"$1\"
  local system=\"$2\"
  printf '%s.%s\\n' \"$release\" \"$system\"
}}

rpm_name_for_arch() {{
  local arch=\"$1\"
  local release=\"$2\"
  local system=\"$3\"
  local mode=\"${{4:-$DEFAULT_CACHE_MODE}}\"
  local package_name
  package_name=$(package_name_for_cache_mode \"$mode\")
  printf '%s-%s-%s.%s.rpm\\n' \"$package_name\" \"$PACKAGE_VERSION\" \"$(rpm_full_release \"$release\" \"$system\")\" \"$arch\"
}}

srpm_name() {{
  local release=\"$1\"
  local system=\"$2\"
  printf '%s-%s-%s.src.rpm\\n' \"$PACKAGE_NAME\" \"$PACKAGE_VERSION\" \"$(rpm_full_release \"$release\" \"$system\")\"
}}

reset_output_dir() {{
  local dry_run=\"$1\"
  local path=\"$2\"
  require_absolute_path \"$path\" \"output directory\"
  if [ \"$path\" = / ]; then
    die \"refusing to reset / as output directory\"
  fi
  if [ \"$dry_run\" = 1 ]; then
    printf 'Would reset output directory: %s\\n' \"$path\"
  else
    rm -rf \"$path\"
    mkdir -p \"$path\"
  fi
}}

prepare_rpmbuild_tree() {{
  local dry_run=\"$1\"
  local rpm_root=\"$2\"
  reset_output_dir \"$dry_run\" \"$rpm_root\"
  if [ \"$dry_run\" = 0 ]; then
    mkdir -p \"$rpm_root/BUILD\" \"$rpm_root/BUILDROOT\" \"$rpm_root/RPMS\" \\
             \"$rpm_root/SOURCES\" \"$rpm_root/SPECS\" \"$rpm_root/SRPMS\"
  fi
}}

validate_source_archive() {{
  local dry_run=\"$1\"
  local archive=\"$2\"
  local expected_root=\"racket-$PACKAGE_SOURCE_VERSION\"
  if [ \"$dry_run\" = 1 ]; then
    printf 'Would validate source archive: %s\\n' \"$archive\"
    return
  fi
  require_nonempty_file \"$archive\"
  tar -tzf \"$archive\" \"$expected_root/src/configure\" >/dev/null \\
    || die \"source archive missing $expected_root/src/configure: $archive\"
  tar -tzf \"$archive\" \"$expected_root/collects/racket/main.rkt\" >/dev/null \\
    || die \"source archive missing $expected_root/collects/racket/main.rkt: $archive\"
}}

verify_source_sha256() {{
  local dry_run=\"$1\"
  local archive=\"$2\"
  if [ -z \"$SOURCE_SHA256\" ]; then
    printf 'No generated source sha256 is pinned; skipping source sha256 check.\\n'
    return
  fi
  if [ \"$dry_run\" = 1 ]; then
    printf 'Would verify source sha256: %s\\n' \"$SOURCE_SHA256\"
    return
  fi
  local actual
  if command -v sha256sum >/dev/null 2>&1; then
    actual=$(sha256sum \"$archive\" | cut -d ' ' -f 1)
  elif command -v shasum >/dev/null 2>&1; then
    actual=$(shasum -a 256 \"$archive\" | cut -d ' ' -f 1)
  else
    die \"executable not found in PATH: sha256sum or shasum\"
  fi
  [ \"$actual\" = \"$SOURCE_SHA256\" ] \\
    || die \"source sha256 mismatch: expected $SOURCE_SHA256 but got $actual\"
}}

prepare_source_archive() {{
  local dry_run=\"$1\"
  local source_archive=\"$2\"
  local source_url=\"$3\"
  local dest=\"$4\"
  require_absolute_path \"$dest\" \"source archive destination\"
  if [ \"$dry_run\" = 0 ]; then
    mkdir -p \"$(dirname \"$dest\")\"
  fi
  if [ -n \"$source_archive\" ]; then
    require_nonempty_file \"$source_archive\"
    run_cmd \"$dry_run\" cp \"$source_archive\" \"$dest\"
  else
    [ -n \"$source_url\" ] || die \"source URL is empty\"
    maybe_require_exe \"$dry_run\" curl
    run_cmd \"$dry_run\" curl -fL --retry 3 --output \"$dest\" \"$source_url\"
  fi
  validate_source_archive \"$dry_run\" \"$dest\"
  verify_source_sha256 \"$dry_run\" \"$dest\"
}}
")

(define (rpm-build-script-content c)
  f"{(rpm-script-header "build-rpm.sh")}SCRIPT_DIR=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)
source \"$SCRIPT_DIR/rpm-common.sh\"

usage() {{
  cat <<'USAGE'
Usage: scripts/build-rpm.sh --artifact-dir PATH --work-dir PATH --rpm-system SYSTEM --rpm-release RELEASE --rpm-arch ARCH [options]

Build a binary RPM from SPECS/racket9.spec and a stable source archive. All
mutable paths are named.

Options:
  --source-archive PATH  Local {(rpm-source-archive-name c)} to copy into rpmbuild.
  --source-url URL       Source archive URL. Defaults to the generated release URL.
  --artifact-dir PATH    Directory that receives the final .rpm.
  --work-dir PATH        Build work directory for rpmbuild.
  --rpm-system SYSTEM    el9, fc40, fc43, fc44, openeuler2203, or openeuler2403.
  --rpm-release RELEASE  Package release base, for example 1. The system suffix is appended separately.
  --cache-mode MODE      postinstall or cached. Defaults to postinstall.
  --prefix PATH          Install prefix inside the package. Defaults to generated /usr.
  --rpm-arch ARCH        x86_64, amd64, x64, aarch64, or arm64.
  --jobs N               Parallel jobs passed to rpmbuild through _smp_mflags.
  --rpmbuild-arg ARG     Extra rpmbuild argument. May be repeated.
  --dry-run              Print checks and commands without writing outputs.
USAGE
}}

DRY_RUN=0
SOURCE_ARCHIVE=
SOURCE_URL=\"$DEFAULT_SOURCE_URL\"
SOURCE_URL_EXPLICIT=0
ARTIFACT_DIR=
WORK_DIR=
RPM_SYSTEM=
RPM_RELEASE=
RPM_ARCH=
JOBS=1
PREFIX=\"$DEFAULT_PREFIX\"
CACHE_MODE=\"$DEFAULT_CACHE_MODE\"
RPMBUILD_ARGS=()

while [ $# -gt 0 ]; do
  case \"$1\" in
    --source-archive) [ $# -ge 2 ] || usage_error \"missing value for --source-archive\"; SOURCE_ARCHIVE=\"$2\"; shift 2 ;;
    --source-url) [ $# -ge 2 ] || usage_error \"missing value for --source-url\"; SOURCE_URL=\"$2\"; SOURCE_URL_EXPLICIT=1; shift 2 ;;
    --artifact-dir) [ $# -ge 2 ] || usage_error \"missing value for --artifact-dir\"; ARTIFACT_DIR=\"$2\"; shift 2 ;;
    --work-dir) [ $# -ge 2 ] || usage_error \"missing value for --work-dir\"; WORK_DIR=\"$2\"; shift 2 ;;
    --rpm-system) [ $# -ge 2 ] || usage_error \"missing value for --rpm-system\"; RPM_SYSTEM=\"$2\"; shift 2 ;;
    --rpm-release) [ $# -ge 2 ] || usage_error \"missing value for --rpm-release\"; RPM_RELEASE=\"$2\"; shift 2 ;;
    --cache-mode) [ $# -ge 2 ] || usage_error \"missing value for --cache-mode\"; CACHE_MODE=\"$2\"; shift 2 ;;
    --prefix) [ $# -ge 2 ] || usage_error \"missing value for --prefix\"; PREFIX=\"$2\"; shift 2 ;;
    --rpm-arch) [ $# -ge 2 ] || usage_error \"missing value for --rpm-arch\"; RPM_ARCH=\"$2\"; shift 2 ;;
    --jobs) [ $# -ge 2 ] || usage_error \"missing value for --jobs\"; JOBS=\"$2\"; shift 2 ;;
    --rpmbuild-arg) [ $# -ge 2 ] || usage_error \"missing value for --rpmbuild-arg\"; RPMBUILD_ARGS+=(\"$2\"); shift 2 ;;
    --dry-run) DRY_RUN=1; shift ;;
    --help) usage; exit 0 ;;
    *) usage_error \"unknown option: $1\" ;;
  esac
done

REPO_ROOT=$(repo_root_from_script)
require_repo_root \"$REPO_ROOT\"
[ -n \"$ARTIFACT_DIR\" ] || usage_error \"--artifact-dir is required\"
[ -n \"$WORK_DIR\" ] || usage_error \"--work-dir is required\"
[ -n \"$RPM_SYSTEM\" ] || usage_error \"--rpm-system is required\"
[ -n \"$RPM_RELEASE\" ] || usage_error \"--rpm-release is required\"
[ -n \"$RPM_ARCH\" ] || usage_error \"--rpm-arch is required\"
validate_rpm_system \"$RPM_SYSTEM\"
validate_rpm_release \"$RPM_RELEASE\"
validate_cache_mode \"$CACHE_MODE\"
NORMALIZED_ARCH=$(normalize_arch \"$RPM_ARCH\")
RPM_PACKAGE_NAME=$(package_name_for_cache_mode \"$CACHE_MODE\")
if [ -n \"$SOURCE_ARCHIVE\" ] && [ \"$SOURCE_URL_EXPLICIT\" = 1 ]; then
  usage_error \"use either --source-archive or --source-url, not both\"
fi

maybe_require_exe \"$DRY_RUN\" tar
maybe_require_exe \"$DRY_RUN\" rpm
maybe_require_exe \"$DRY_RUN\" rpmbuild

RPMBUILD_ROOT=\"$WORK_DIR/rpmbuild\"
SPEC_PATH=\"$RPMBUILD_ROOT/SPECS/$SPEC_NAME\"
SOURCE_PATH=\"$RPMBUILD_ROOT/SOURCES/$SOURCE_ARCHIVE_NAME\"
RPM_FULL_RELEASE=$(rpm_full_release \"$RPM_RELEASE\" \"$RPM_SYSTEM\")
RPM_NAME=$(rpm_name_for_arch \"$NORMALIZED_ARCH\" \"$RPM_RELEASE\" \"$RPM_SYSTEM\" \"$CACHE_MODE\")
RPM_OUTPUT=\"$RPMBUILD_ROOT/RPMS/$NORMALIZED_ARCH/$RPM_NAME\"

printf 'Repository root: %s\\n' \"$REPO_ROOT\"
printf 'RPM system: %s\\n' \"$RPM_SYSTEM\"
printf 'RPM release: %s\\n' \"$RPM_RELEASE\"
printf 'RPM full release: %s\\n' \"$RPM_FULL_RELEASE\"
printf 'RPM cache mode: %s\\n' \"$CACHE_MODE\"
printf 'RPM package name: %s\\n' \"$RPM_PACKAGE_NAME\"
printf 'Source archive: %s\\n' \"${{SOURCE_ARCHIVE:-$SOURCE_URL}}\"
printf 'RPM output: %s\\n' \"$ARTIFACT_DIR/$RPM_NAME\"

prepare_rpmbuild_tree \"$DRY_RUN\" \"$RPMBUILD_ROOT\"
if [ \"$DRY_RUN\" = 0 ]; then
  cp \"$REPO_ROOT/SPECS/$SPEC_NAME\" \"$SPEC_PATH\"
fi
prepare_source_archive \"$DRY_RUN\" \"$SOURCE_ARCHIVE\" \"$SOURCE_URL\" \"$SOURCE_PATH\"
if [ \"${{#RPMBUILD_ARGS[@]}}\" -gt 0 ]; then
  run_cmd \"$DRY_RUN\" rpmbuild -bb --target \"$NORMALIZED_ARCH\" \\
    --define \"_topdir $RPMBUILD_ROOT\" \\
    --define \"_build_id_links none\" \\
    --define \"_sysconfdir /etc\" \\
    --define \"package_prefix $PREFIX\" \\
    --define \"package_name $RPM_PACKAGE_NAME\" \\
    --define \"cache_mode $CACHE_MODE\" \\
    --define \"package_system $RPM_SYSTEM\" \\
    --define \"package_release $RPM_RELEASE\" \\
    --define \"_smp_mflags -j$JOBS\" \\
    \"${{RPMBUILD_ARGS[@]}}\" \\
    \"$SPEC_PATH\"
else
  run_cmd \"$DRY_RUN\" rpmbuild -bb --target \"$NORMALIZED_ARCH\" \\
    --define \"_topdir $RPMBUILD_ROOT\" \\
    --define \"_build_id_links none\" \\
    --define \"_sysconfdir /etc\" \\
    --define \"package_prefix $PREFIX\" \\
    --define \"package_name $RPM_PACKAGE_NAME\" \\
    --define \"cache_mode $CACHE_MODE\" \\
    --define \"package_system $RPM_SYSTEM\" \\
    --define \"package_release $RPM_RELEASE\" \\
    --define \"_smp_mflags -j$JOBS\" \\
    \"$SPEC_PATH\"
fi

if [ \"$DRY_RUN\" = 1 ]; then
  printf 'Would copy RPM artifact: %s -> %s\\n' \"$RPM_OUTPUT\" \"$ARTIFACT_DIR/$RPM_NAME\"
else
  require_nonempty_file \"$RPM_OUTPUT\"
  mkdir -p \"$ARTIFACT_DIR\"
  cp \"$RPM_OUTPUT\" \"$ARTIFACT_DIR/$RPM_NAME\"
  \"$REPO_ROOT/scripts/verify-rpm.sh\" --rpm \"$ARTIFACT_DIR/$RPM_NAME\" --rpm-system \"$RPM_SYSTEM\" --rpm-release \"$RPM_RELEASE\" --rpm-arch \"$NORMALIZED_ARCH\" --cache-mode \"$CACHE_MODE\"
  printf 'RPM package: %s\\n' \"$ARTIFACT_DIR/$RPM_NAME\"
fi
")

(define (rpm-build-srpm-script-content c)
  f"{(rpm-script-header "build-srpm.sh")}SCRIPT_DIR=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)
source \"$SCRIPT_DIR/rpm-common.sh\"

usage() {{
  cat <<'USAGE'
Usage: scripts/build-srpm.sh --artifact-dir PATH --work-dir PATH --rpm-system SYSTEM --rpm-release RELEASE --rpm-arch ARCH [options]

Build a source RPM from SPECS/racket9.spec and a stable source archive.

Options:
  --source-archive PATH  Local {(rpm-source-archive-name c)} to copy into rpmbuild.
  --source-url URL       Source archive URL. Defaults to the generated release URL.
  --artifact-dir PATH    Directory that receives the final .src.rpm.
  --work-dir PATH        Build work directory for rpmbuild.
  --rpm-system SYSTEM    el9, fc40, fc43, fc44, openeuler2203, or openeuler2403.
  --rpm-release RELEASE  Package release base, for example 1. The system suffix is appended separately.
  --prefix PATH          Install prefix inside the package. Defaults to generated /usr.
  --rpm-arch ARCH        x86_64, amd64, x64, aarch64, or arm64.
  --jobs N               Parallel jobs recorded in generated build macros.
  --rpmbuild-arg ARG     Extra rpmbuild argument. May be repeated.
  --dry-run              Print checks and commands without writing outputs.
USAGE
}}

DRY_RUN=0
SOURCE_ARCHIVE=
SOURCE_URL=\"$DEFAULT_SOURCE_URL\"
SOURCE_URL_EXPLICIT=0
ARTIFACT_DIR=
WORK_DIR=
RPM_SYSTEM=
RPM_RELEASE=
RPM_ARCH=
JOBS=1
PREFIX=\"$DEFAULT_PREFIX\"
RPMBUILD_ARGS=()

while [ $# -gt 0 ]; do
  case \"$1\" in
    --source-archive) [ $# -ge 2 ] || usage_error \"missing value for --source-archive\"; SOURCE_ARCHIVE=\"$2\"; shift 2 ;;
    --source-url) [ $# -ge 2 ] || usage_error \"missing value for --source-url\"; SOURCE_URL=\"$2\"; SOURCE_URL_EXPLICIT=1; shift 2 ;;
    --artifact-dir) [ $# -ge 2 ] || usage_error \"missing value for --artifact-dir\"; ARTIFACT_DIR=\"$2\"; shift 2 ;;
    --work-dir) [ $# -ge 2 ] || usage_error \"missing value for --work-dir\"; WORK_DIR=\"$2\"; shift 2 ;;
    --rpm-system) [ $# -ge 2 ] || usage_error \"missing value for --rpm-system\"; RPM_SYSTEM=\"$2\"; shift 2 ;;
    --rpm-release) [ $# -ge 2 ] || usage_error \"missing value for --rpm-release\"; RPM_RELEASE=\"$2\"; shift 2 ;;
    --prefix) [ $# -ge 2 ] || usage_error \"missing value for --prefix\"; PREFIX=\"$2\"; shift 2 ;;
    --rpm-arch) [ $# -ge 2 ] || usage_error \"missing value for --rpm-arch\"; RPM_ARCH=\"$2\"; shift 2 ;;
    --jobs) [ $# -ge 2 ] || usage_error \"missing value for --jobs\"; JOBS=\"$2\"; shift 2 ;;
    --rpmbuild-arg) [ $# -ge 2 ] || usage_error \"missing value for --rpmbuild-arg\"; RPMBUILD_ARGS+=(\"$2\"); shift 2 ;;
    --dry-run) DRY_RUN=1; shift ;;
    --help) usage; exit 0 ;;
    *) usage_error \"unknown option: $1\" ;;
  esac
done

REPO_ROOT=$(repo_root_from_script)
require_repo_root \"$REPO_ROOT\"
[ -n \"$ARTIFACT_DIR\" ] || usage_error \"--artifact-dir is required\"
[ -n \"$WORK_DIR\" ] || usage_error \"--work-dir is required\"
[ -n \"$RPM_SYSTEM\" ] || usage_error \"--rpm-system is required\"
[ -n \"$RPM_RELEASE\" ] || usage_error \"--rpm-release is required\"
[ -n \"$RPM_ARCH\" ] || usage_error \"--rpm-arch is required\"
validate_rpm_system \"$RPM_SYSTEM\"
validate_rpm_release \"$RPM_RELEASE\"
NORMALIZED_ARCH=$(normalize_arch \"$RPM_ARCH\")
if [ -n \"$SOURCE_ARCHIVE\" ] && [ \"$SOURCE_URL_EXPLICIT\" = 1 ]; then
  usage_error \"use either --source-archive or --source-url, not both\"
fi

maybe_require_exe \"$DRY_RUN\" tar
maybe_require_exe \"$DRY_RUN\" rpmbuild

RPMBUILD_ROOT=\"$WORK_DIR/rpmbuild-srpm\"
SPEC_PATH=\"$RPMBUILD_ROOT/SPECS/$SPEC_NAME\"
SOURCE_PATH=\"$RPMBUILD_ROOT/SOURCES/$SOURCE_ARCHIVE_NAME\"
RPM_FULL_RELEASE=$(rpm_full_release \"$RPM_RELEASE\" \"$RPM_SYSTEM\")
SRPM_NAME=$(srpm_name \"$RPM_RELEASE\" \"$RPM_SYSTEM\")
SRPM_OUTPUT=\"$RPMBUILD_ROOT/SRPMS/$SRPM_NAME\"

printf 'Repository root: %s\\n' \"$REPO_ROOT\"
printf 'RPM system: %s\\n' \"$RPM_SYSTEM\"
printf 'RPM release: %s\\n' \"$RPM_RELEASE\"
printf 'RPM full release: %s\\n' \"$RPM_FULL_RELEASE\"
printf 'Source archive: %s\\n' \"${{SOURCE_ARCHIVE:-$SOURCE_URL}}\"
printf 'SRPM output: %s\\n' \"$ARTIFACT_DIR/$SRPM_NAME\"

prepare_rpmbuild_tree \"$DRY_RUN\" \"$RPMBUILD_ROOT\"
if [ \"$DRY_RUN\" = 0 ]; then
  cp \"$REPO_ROOT/SPECS/$SPEC_NAME\" \"$SPEC_PATH\"
fi
prepare_source_archive \"$DRY_RUN\" \"$SOURCE_ARCHIVE\" \"$SOURCE_URL\" \"$SOURCE_PATH\"
if [ \"${{#RPMBUILD_ARGS[@]}}\" -gt 0 ]; then
  run_cmd \"$DRY_RUN\" rpmbuild -bs --target \"$NORMALIZED_ARCH\" \\
    --define \"_topdir $RPMBUILD_ROOT\" \\
    --define \"_sysconfdir /etc\" \\
    --define \"package_prefix $PREFIX\" \\
    --define \"package_system $RPM_SYSTEM\" \\
    --define \"package_release $RPM_RELEASE\" \\
    --define \"_smp_mflags -j$JOBS\" \\
    \"${{RPMBUILD_ARGS[@]}}\" \\
    \"$SPEC_PATH\"
else
  run_cmd \"$DRY_RUN\" rpmbuild -bs --target \"$NORMALIZED_ARCH\" \\
    --define \"_topdir $RPMBUILD_ROOT\" \\
    --define \"_sysconfdir /etc\" \\
    --define \"package_prefix $PREFIX\" \\
    --define \"package_system $RPM_SYSTEM\" \\
    --define \"package_release $RPM_RELEASE\" \\
    --define \"_smp_mflags -j$JOBS\" \\
    \"$SPEC_PATH\"
fi

if [ \"$DRY_RUN\" = 1 ]; then
  printf 'Would copy SRPM artifact: %s -> %s\\n' \"$SRPM_OUTPUT\" \"$ARTIFACT_DIR/$SRPM_NAME\"
else
  require_nonempty_file \"$SRPM_OUTPUT\"
  mkdir -p \"$ARTIFACT_DIR\"
  cp \"$SRPM_OUTPUT\" \"$ARTIFACT_DIR/$SRPM_NAME\"
  printf 'SRPM package: %s\\n' \"$ARTIFACT_DIR/$SRPM_NAME\"
fi
")

(define (rpm-verify-script-content c)
  f"{(rpm-script-header "verify-rpm.sh")}SCRIPT_DIR=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)
source \"$SCRIPT_DIR/rpm-common.sh\"

usage() {{
  cat <<'USAGE'
Usage: scripts/verify-rpm.sh --rpm PATH --rpm-system SYSTEM --rpm-release RELEASE --rpm-arch ARCH [--cache-mode MODE] [--dry-run]

Validate RPM metadata and payload ownership boundaries.
USAGE
}}

DRY_RUN=0
RPM_PATH=
RPM_SYSTEM=
RPM_RELEASE=
RPM_ARCH=
CACHE_MODE=\"$DEFAULT_CACHE_MODE\"

while [ $# -gt 0 ]; do
  case \"$1\" in
    --rpm) [ $# -ge 2 ] || usage_error \"missing value for --rpm\"; RPM_PATH=\"$2\"; shift 2 ;;
    --rpm-system) [ $# -ge 2 ] || usage_error \"missing value for --rpm-system\"; RPM_SYSTEM=\"$2\"; shift 2 ;;
    --rpm-release) [ $# -ge 2 ] || usage_error \"missing value for --rpm-release\"; RPM_RELEASE=\"$2\"; shift 2 ;;
    --rpm-arch) [ $# -ge 2 ] || usage_error \"missing value for --rpm-arch\"; RPM_ARCH=\"$2\"; shift 2 ;;
    --cache-mode) [ $# -ge 2 ] || usage_error \"missing value for --cache-mode\"; CACHE_MODE=\"$2\"; shift 2 ;;
    --dry-run) DRY_RUN=1; shift ;;
    --help) usage; exit 0 ;;
    *) usage_error \"unknown option: $1\" ;;
  esac
done

REPO_ROOT=$(repo_root_from_script)
require_repo_root \"$REPO_ROOT\"
[ -n \"$RPM_PATH\" ] || usage_error \"--rpm is required\"
[ -n \"$RPM_SYSTEM\" ] || usage_error \"--rpm-system is required\"
[ -n \"$RPM_RELEASE\" ] || usage_error \"--rpm-release is required\"
[ -n \"$RPM_ARCH\" ] || usage_error \"--rpm-arch is required\"
validate_rpm_system \"$RPM_SYSTEM\"
validate_rpm_release \"$RPM_RELEASE\"
validate_cache_mode \"$CACHE_MODE\"
NORMALIZED_ARCH=$(normalize_arch \"$RPM_ARCH\")
RPM_PACKAGE_NAME=$(package_name_for_cache_mode \"$CACHE_MODE\")
RPM_FULL_RELEASE=$(rpm_full_release \"$RPM_RELEASE\" \"$RPM_SYSTEM\")
EXPECTED_RPM=$(rpm_name_for_arch \"$NORMALIZED_ARCH\" \"$RPM_RELEASE\" \"$RPM_SYSTEM\" \"$CACHE_MODE\")

if [ \"$DRY_RUN\" = 1 ]; then
  printf 'Would verify RPM: %s\\n' \"$RPM_PATH\"
  printf 'Would expect RPM system: %s\\n' \"$RPM_SYSTEM\"
  printf 'Would expect RPM release: %s\\n' \"$RPM_RELEASE\"
  printf 'Would expect RPM full release: %s\\n' \"$RPM_FULL_RELEASE\"
  printf 'Would expect RPM cache mode: %s\\n' \"$CACHE_MODE\"
  printf 'Would expect RPM package name: %s\\n' \"$RPM_PACKAGE_NAME\"
  printf 'Would expect RPM basename: %s\\n' \"$EXPECTED_RPM\"
  exit 0
fi

require_exe rpm
require_nonempty_file \"$RPM_PATH\"
[ \"$(basename \"$RPM_PATH\")\" = \"$EXPECTED_RPM\" ] || die \"RPM basename does not match expected $EXPECTED_RPM: $RPM_PATH\"

metadata=$(rpm -qip \"$RPM_PATH\")
printf '%s\\n' \"$metadata\"
printf '%s\\n' \"$metadata\" | grep -F \"Name        : $RPM_PACKAGE_NAME\" >/dev/null || die \"RPM metadata missing expected name\"
printf '%s\\n' \"$metadata\" | grep -F \"Version     : $PACKAGE_VERSION\" >/dev/null || die \"RPM metadata missing expected version\"
printf '%s\\n' \"$metadata\" | grep -F \"Release     : $RPM_FULL_RELEASE\" >/dev/null || die \"RPM metadata missing expected release\"
printf '%s\\n' \"$metadata\" | grep -F \"Architecture: $NORMALIZED_ARCH\" >/dev/null || die \"RPM metadata missing expected architecture\"

payload=$(rpm -qpl \"$RPM_PATH\")
if printf '%s\\n' \"$payload\" | grep -Fx '/var/cache/racket/racket-compiled-cache.log' >/dev/null; then
  die \"RPM payload unexpectedly includes racket compiled cache debug log\"
fi
if printf '%s\\n' \"$payload\" | grep -Eq '^/usr$|^/usr/(bin|lib|lib64|share)$'; then
  die \"RPM payload claims shared /usr parent directory\"
fi
if [ \"$CACHE_MODE\" = postinstall ]; then
  if printf '%s\\n' \"$payload\" | grep -E '^/var/cache/racket/compiled/.+[.]zo$' >/dev/null; then
    die \"postinstall RPM payload unexpectedly includes system compiled cache .zo files\"
  fi
else
  printf '%s\\n' \"$payload\" | grep -E '^/var/cache/racket/compiled/.+[.]zo$' >/dev/null \\
    || die \"cached RPM payload does not include system compiled cache .zo files\"
  runtime_collects_cache=\"/var/cache/racket/compiled/${{DEFAULT_PREFIX#/}}/share/racket/collects\"
  printf '%s\\n' \"$payload\" | grep -F \"$runtime_collects_cache/\" | grep -E '[.]zo$' >/dev/null \\
    || die \"cached RPM payload does not include runtime-keyed collects cache .zo files\"
  runtime_pkgs_cache=\"/var/cache/racket/compiled/${{DEFAULT_PREFIX#/}}/share/racket/pkgs\"
  printf '%s\\n' \"$payload\" | grep -F \"$runtime_pkgs_cache/\" | grep -E '[.]zo$' >/dev/null \\
    || die \"cached RPM payload does not include runtime-keyed package cache .zo files\"
  rhombus_ephemeral_cache=\"$DEFAULT_PREFIX/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod\"
  printf '%s\\n' \"$payload\" | grep -F \"$rhombus_ephemeral_cache/\" | grep -E '[.]zo$' >/dev/null \\
    || die \"cached RPM payload does not include Rhombus demod cache .zo files\"
fi
scripts=$(rpm -qp --scripts \"$RPM_PATH\")
if [ \"$CACHE_MODE\" = postinstall ]; then
  printf '%s\\n' \"$scripts\" | grep -F 'raco setup --system --no-user --reset-cache -D --no-pkg-deps' >/dev/null \\
    || die \"RPM scriptlets do not build the system compiled cache\"
  printf '%s\\n' \"$scripts\" | grep -F 'raco setup --system --delete-cache' >/dev/null \\
    || die \"RPM scriptlets do not delete the system compiled cache\"
  printf '%s\\n' \"$scripts\" | grep -F \"rpm -q --quiet $CACHED_PACKAGE_NAME >/dev/null 2>&1\" >/dev/null \\
    || die \"RPM preun does not guard cache deletion for package replacement\"
  printf '%s\\n' \"$scripts\" | grep -F 'package-racket-rhombus-cache' >/dev/null \\
    || die \"RPM scriptlets do not warm the Rhombus demod cache\"
  printf '%s\\n' \"$scripts\" | grep -F 'racket -U -R \"$compiled_cache_root\" -N rhombus -l- rhombus/run.rhm --version' >/dev/null \\
    || die \"RPM scriptlets do not warm the Rhombus version cache into the system cache\"
else
  if printf '%s\\n' \"$scripts\" | grep -F 'raco setup --system --no-user --reset-cache -D --no-pkg-deps' >/dev/null; then
    die \"cached RPM scriptlets unexpectedly build the system compiled cache\"
  fi
  if printf '%s\\n' \"$scripts\" | grep -F 'raco setup --system --delete-cache' >/dev/null; then
    die \"cached RPM scriptlets unexpectedly delete the system compiled cache through raco\"
  fi
fi
printf '%s\\n' \"$scripts\" | grep -F 'rm -rf /var/cache/racket/compiled' >/dev/null \\
  || die \"RPM scriptlets do not purge the system compiled cache directory\"
printf '%s\\n' \"$scripts\" | grep -F 'rhombus-lib/rhombus/private/compiled/ephemeral/demod' >/dev/null \\
  || die \"RPM scriptlets do not purge the Rhombus demod cache directory\"
printf '%s\\n' \"$scripts\" | grep -F \"rmdir $DEFAULT_PREFIX/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral\" >/dev/null \\
  || die \"RPM scriptlets do not remove empty Rhombus ephemeral cache parents\"
printf '%s\\n' \"$scripts\" | grep -F 'rpm -q --quiet \"$other_package\" >/dev/null 2>&1' >/dev/null \\
  || die \"RPM postun does not guard shared cache deletion for package replacement\"
printf 'Validated RPM: %s\\n' \"$RPM_PATH\"
")

(define (assert-rpm-repo-root! c #:write? [write? #t])
  (begin
    (define root (cfg-rpm-repo-root c))
    (assert-directory 'rpm-repo root)
    (assert-directory 'rpm-repo (build-path root ".git"))
    (when write?
      (assert-writable-directory 'rpm-repo root)
    ) ; end when write check requested
  ) ; end begin assert-rpm-repo-root!
) ; end define assert-rpm-repo-root!

(define (delete-generated-file-if-present! who path markers)
  (begin
    (define p (complete-path* path))
    (when (file-exists? p)
      (define content (file->string p))
      (unless (for/or ([marker (in-list markers)])
                (string-contains? content marker))
        (raise-user-error who
                          f"refusing to delete file without generated marker: {(clean-path-string p)}")
      ) ; end unless generated marker present
      (delete-file p)
      (println/flush f"Removed stale generated file: {(clean-path-string p)}")
    ) ; end when generated file exists
  ) ; end begin delete-generated-file-if-present!
) ; end define delete-generated-file-if-present!

(define (delete-empty-directory-if-present! who path)
  (begin
    (define d (complete-path* path))
    (cond
      [(directory-exists? d)
       (if (empty-directory? d)
           (begin
             (delete-directory d)
             (println/flush f"Removed stale empty directory: {(clean-path-string d)}")
           ) ; end begin delete empty directory
           (raise-user-error who
                             f"refusing to delete non-empty directory: {(clean-path-string d)}")
       ) ; end if directory empty
      ]
      [(file-exists? d)
       (raise-user-error who
                         f"path exists but is not a directory: {(clean-path-string d)}")]
      [else
       (void)]
    ) ; end cond directory state
  ) ; end begin delete-empty-directory-if-present!
) ; end define delete-empty-directory-if-present!

(define (remove-rpm-spec-stale-outputs! c)
  (begin
    (delete-generated-file-if-present!
     'write-rpm-spec-scaffold!
     (rpm-repository-file-path c)
     (list generated-rpm-repository-notice-marker
           generated-rpm-repo-notice-marker))
    (delete-empty-directory-if-present!
     'write-rpm-spec-scaffold!
     (build-path (cfg-rpm-repo-root c) "repo"))
  ) ; end begin remove-rpm-spec-stale-outputs!
) ; end define remove-rpm-spec-stale-outputs!

(define (rpm-spec-output-paths c)
  (append
   (list (rpm-repo-gitignore-path c)
         (rpm-definition-spec-path c)
         (rpm-definition-source-keep-path c)
         (rpm-script-path c "rpm-common.sh")
         (rpm-script-path c "build-rpm.sh")
         (rpm-script-path c "build-srpm.sh")
         (rpm-script-path c "verify-rpm.sh")
         (rpm-repo-readme-path c))
  ) ; end append rpm spec outputs
) ; end define rpm-spec-output-paths

(define (validate-generated-rpm-script! c name required-needles)
  (begin
    (define path (rpm-script-path c name))
    (assert-nonempty-file 'validate-rpm-spec-scaffold! path)
    (define content (file->string path))
    (define bits (file-or-directory-permissions path 'bits))
    (unless (not (zero? (bitwise-and bits #o111)))
      (raise-user-error 'validate-rpm-spec-scaffold!
                        f"generated script is not executable: {(clean-path-string path)}")
    ) ; end unless executable bit present
    (for ([needle (in-list (cons "set -euo pipefail" required-needles))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-rpm-spec-scaffold!
                          f"generated script {name} is missing: {needle}")
      ) ; end unless script content contains needle
    ) ; end for script needle
  ) ; end begin validate-generated-rpm-script!
) ; end define validate-generated-rpm-script!

(define (validate-rpm-spec-scaffold! c)
  (begin
    (assert-nonempty-file 'validate-rpm-spec-scaffold! (rpm-repo-readme-path c))
    (assert-nonempty-file 'validate-rpm-spec-scaffold! (rpm-repo-gitignore-path c))
    (assert-file 'validate-rpm-spec-scaffold! (rpm-definition-source-keep-path c))
    (validate-rpm-spec! c
                        (rpm-definition-spec-path c)
                        (formula-source-url c))
    (define readme-content (file->string (rpm-repo-readme-path c)))
    (for ([needle (in-list (list "RPM SPEC and build-script repository"
                                 "not an RPM artifact repository"
                                 "SPECS/"
                                 "SOURCES/"
                                 "scripts/build-rpm.sh"))])
      (unless (string-contains? readme-content needle)
        (raise-user-error 'validate-rpm-spec-scaffold!
                          f"generated README is missing: {needle}")
      ) ; end unless readme content contains needle
    ) ; end for readme needle
    (validate-generated-rpm-script! c
                                    "rpm-common.sh"
                                    '("prepare_source_archive"
                                      "validate_source_archive"
                                      "require_repo_root"
                                      "validate_cache_mode"
                                      "package_name_for_cache_mode"
                                      "racket9-cached"))
    (validate-generated-rpm-script! c
                                    "build-rpm.sh"
                                    '("Usage: scripts/build-rpm.sh"
                                      "rpmbuild -bb"
                                      "--define \"_sysconfdir /etc\""
                                      "--cache-mode"
                                      "cache_mode $CACHE_MODE"
                                      "--dry-run"))
    (validate-generated-rpm-script! c
                                    "build-srpm.sh"
                                    '("Usage: scripts/build-srpm.sh"
                                      "rpmbuild -bs"
                                      "--define \"_sysconfdir /etc\""
                                      "--dry-run"))
    (validate-generated-rpm-script! c
                                    "verify-rpm.sh"
                                    '("rpm -qip"
                                      "rpm -qpl"
                                      "rpm -qp --scripts"
                                      "--cache-mode"
                                      "cached RPM payload does not include system compiled cache"
                                      "cached RPM payload does not include runtime-keyed collects cache"
	                                      "cached RPM payload does not include runtime-keyed package cache"
	                                      "cached RPM payload does not include Rhombus demod cache"
	                                      "RPM scriptlets do not warm the Rhombus demod cache"
	                                      "RPM scriptlets do not warm the Rhombus version cache into the system cache"
	                                      "RPM preun does not guard cache deletion for package replacement"
	                                      "RPM scriptlets do not purge the Rhombus demod cache directory"
	                                      "RPM scriptlets do not remove empty Rhombus ephemeral cache parents"
	                                      "RPM postun does not guard shared cache deletion for package replacement"
                                      "racket compiled cache debug log"
                                      "--dry-run"))
  ) ; end begin validate-rpm-spec-scaffold!
) ; end define validate-rpm-spec-scaffold!

(define (write-rpm-spec-scaffold! c)
  (begin
    (assert-rpm-repo-root! c #:write? #t)
    (remove-rpm-spec-stale-outputs! c)
    (make-directory* (rpm-spec-dir c))
    (make-directory* (rpm-sources-dir c))
    (make-directory* (rpm-scripts-dir c))
    (define source-url (formula-source-url c))
    (define source-sha256 (resolve-rpm-source-sha256! c source-url))
    (write-text-file! (rpm-repo-gitignore-path c) rpm-spec-gitignore-content)
    (write-rpm-spec! c
                     (rpm-definition-spec-path c)
                     source-url
                     source-sha256)
    (write-text-file! (rpm-definition-source-keep-path c) "")
    (write-executable-text-file! (rpm-script-path c "rpm-common.sh")
                                 (rpm-common-script-content c source-sha256))
    (write-executable-text-file! (rpm-script-path c "build-rpm.sh")
                                 (rpm-build-script-content c))
    (write-executable-text-file! (rpm-script-path c "build-srpm.sh")
                                 (rpm-build-srpm-script-content c))
    (write-executable-text-file! (rpm-script-path c "verify-rpm.sh")
                                 (rpm-verify-script-content c))
    (write-text-file! (rpm-repo-readme-path c) (rpm-spec-readme-content c))
    (validate-rpm-spec-scaffold! c)
    (println/flush f"Generated RPM SPEC scaffold: {(clean-path-string (cfg-rpm-repo-root c))}")
  ) ; end begin write-rpm-spec-scaffold!
) ; end define write-rpm-spec-scaffold!

(define (print-rpm-spec-scaffold-dry-run! c)
  (begin
    (assert-rpm-repo-root! c #:write? #f)
    (println/flush f"Would generate RPM SPEC scaffold in: {(clean-path-string (cfg-rpm-repo-root c))}")
    (for ([path (in-list (rpm-spec-output-paths c))])
      (println/flush f"Would generate RPM SPEC file: {(clean-path-string path)}")
    ) ; end for rpm spec output path
  ) ; end begin print-rpm-spec-scaffold-dry-run!
) ; end define print-rpm-spec-scaffold-dry-run!

(define (build-rpm-spec! c)
  (begin
    (println/flush f"RPM SPEC config: {(clean-path-string (cfg-rpm-repo-config c))}")
    (println/flush f"RPM SPEC root: {(clean-path-string (cfg-rpm-repo-root c))}")
    (println/flush f"RPM SPEC package: {(cfg-package-name c)}")
    (if (cfg-dry-run? c)
        (print-rpm-spec-scaffold-dry-run! c)
        (write-rpm-spec-scaffold! c))
  ) ; end begin build-rpm-spec!
) ; end define build-rpm-spec!

(define (rpm-workflows-dir c)
  (build-path (cfg-rpm-repo-root c) ".github" "workflows"))

(define (rpm-ci-workflow-path c)
  (build-path (rpm-workflows-dir c) "build-rpm.yml"))

(define (generated-rpm-code-notice comment-prefix)
  f"{comment-prefix} {generated-rpm-repo-notice-marker}
{comment-prefix} Source of truth: {(generated-source-root)}
{comment-prefix} Humans and LLM agents must change package-racket and regenerate; manual rpm-racket edits are not production-safe.

")

(define (yaml-single-quote value)
  (string-append "'" (regexp-replace* #rx"'" value "''") "'"))

(define (ci-targets-with-cache-modes c targets)
  (for*/list ([target (in-list targets)]
              [cache-mode (in-list package-cache-modes)])
    (define base-id (hash-ref target 'id))
    (define package-name (package-name-for-cache-mode (cfg-package-name c) cache-mode))
    (hash-set
     (hash-set
      (hash-set target 'id f"{base-id}-{cache-mode}")
      'cache-mode cache-mode)
     'package-name package-name)
  ) ; end for/list expanded cache targets
) ; end define ci-targets-with-cache-modes

(define (assert-single-line-string who key value)
  (begin
    (when (or (string-contains? value "\n")
              (string-contains? value "\r"))
      (raise-user-error who f"config key must be a single line: {key}")
    ) ; end when newline in config value
    value
  ) ; end begin assert-single-line-string
) ; end define assert-single-line-string

(define (assert-rpm-ci-id value)
  (begin
    (assert-single-line-string 'rpm-ci-config 'id value)
    (unless (regexp-match? #px"^[A-Za-z0-9_.+-]+$" value)
      (raise-user-error 'rpm-ci-config
                        f"target id must contain only letters, digits, _, ., +, or -: {value}")
    ) ; end unless safe target id
    value
  ) ; end begin assert-rpm-ci-id
) ; end define assert-rpm-ci-id

(define (assert-rpm-ci-runner value)
  (begin
    (assert-single-line-string 'rpm-ci-config 'runner value)
    (unless (regexp-match? #px"^[A-Za-z0-9_.:-]+$" value)
      (raise-user-error 'rpm-ci-config
                        f"runner must contain only letters, digits, _, ., :, or -: {value}")
    ) ; end unless safe runner
    value
  ) ; end begin assert-rpm-ci-runner
) ; end define assert-rpm-ci-runner

(define (assert-rpm-ci-container value)
  (begin
    (assert-single-line-string 'rpm-ci-config 'container value)
    (unless (regexp-match? #px"^[A-Za-z0-9._/:@-]+$" value)
      (raise-user-error 'rpm-ci-config
                        f"container must contain only image-reference characters: {value}")
    ) ; end unless safe container
    value
  ) ; end begin assert-rpm-ci-container
) ; end define assert-rpm-ci-container

(define (assert-rpm-ci-package value)
  (begin
    (assert-single-line-string 'rpm-ci-config 'setup-packages value)
    (unless (regexp-match? #px"^[A-Za-z0-9._+:@/-]+$" value)
      (raise-user-error 'rpm-ci-config
                        f"setup package contains unsupported characters: {value}")
    ) ; end unless safe package
    value
  ) ; end begin assert-rpm-ci-package
) ; end define assert-rpm-ci-package

(define (assert-rpm-ci-artifact-prefix value)
  (begin
    (assert-single-line-string 'rpm-ci-config 'artifact-prefix value)
    (unless (regexp-match? #px"^[A-Za-z0-9_.+-]+$" value)
      (raise-user-error 'rpm-ci-config
                        f"artifact-prefix must contain only letters, digits, _, ., +, or -: {value}")
    ) ; end unless safe artifact prefix
    value
  ) ; end begin assert-rpm-ci-artifact-prefix
) ; end define assert-rpm-ci-artifact-prefix

(define (assert-rpm-ci-release-tag value)
  (begin
    (assert-single-line-string 'rpm-ci-config 'release-tag value)
    (unless (regexp-match? #px"^[A-Za-z0-9._/-]+$" value)
      (raise-user-error 'rpm-ci-config
                        f"release-tag must contain only letters, digits, _, ., /, or -: {value}")
    ) ; end unless safe release tag
    value
  ) ; end begin assert-rpm-ci-release-tag
) ; end define assert-rpm-ci-release-tag

(define (assert-rpm-ci-release-name value)
  (assert-single-line-string 'rpm-ci-config 'release-name value))

(define (rpm-ci-normalize-target target)
  (begin
    (unless (hash? target)
      (raise-user-error 'rpm-ci-config "each target must be a hash")
    ) ; end unless target hash
    (define id
      (assert-rpm-ci-id (config-required-string 'rpm-ci-config target 'id)))
    (define system
      (assert-rpm-system (config-required-string 'rpm-ci-config target 'rpm-system)))
    (define release
      (assert-rpm-release (config-required-string 'rpm-ci-config target 'rpm-release)))
    (define arch
      (normalize-rpm-arch (config-required-string 'rpm-ci-config target 'rpm-arch)))
    (define runner
      (assert-rpm-ci-runner (config-required-string 'rpm-ci-config target 'runner)))
    (define container
      (assert-rpm-ci-container (config-required-string 'rpm-ci-config target 'container)))
    (define packages
      (config-required-list 'rpm-ci-config target 'setup-packages))
    (when (null? packages)
      (raise-user-error 'rpm-ci-config f"target {id} setup-packages must not be empty")
    ) ; end when empty setup packages
    (for ([pkg (in-list packages)])
      (unless (string? pkg)
        (raise-user-error 'rpm-ci-config f"target {id} setup-packages must contain only strings")
      ) ; end unless package string
      (assert-rpm-ci-package pkg)
    ) ; end for setup package
    (define jobs
      (required-config-positive-integer target 'jobs))
    (hash 'id id
          'rpm-system system
          'rpm-release release
          'rpm-arch arch
          'runner runner
          'container container
          'setup-packages packages
          'jobs jobs)
  ) ; end begin rpm-ci-normalize-target
) ; end define rpm-ci-normalize-target

(define (rpm-ci-target-artifact-name artifact-prefix target)
  f"{artifact-prefix}-{(hash-ref target 'id)}")

(define (rpm-ci-normalized-targets config)
  (begin
    (define raw-targets (config-required-list 'rpm-ci-config config 'targets))
    (when (null? raw-targets)
      (raise-user-error 'rpm-ci-config "targets must not be empty")
    ) ; end when no targets
    (define targets
      (map rpm-ci-normalize-target raw-targets)
    ) ; end define normalized targets
    (define ids (map (lambda (target) (hash-ref target 'id)) targets))
    (unless (= (length ids) (length (remove-duplicates ids string=?)))
      (raise-user-error 'rpm-ci-config "target ids must be unique")
    ) ; end unless unique target ids
    targets
  ) ; end begin rpm-ci-normalized-targets
) ; end define rpm-ci-normalized-targets

(define (validate-rpm-ci-config! config)
  (begin
    (assert-rpm-ci-release-tag (config-required-string 'rpm-ci-config config 'release-tag))
    (assert-rpm-ci-release-name (config-required-string 'rpm-ci-config config 'release-name))
    (assert-rpm-ci-artifact-prefix (config-required-string 'rpm-ci-config config 'artifact-prefix))
    (config-required-boolean 'rpm-ci-config config 'create-release)
    (rpm-ci-normalized-targets config)
    (void)
  ) ; end begin validate-rpm-ci-config!
) ; end define validate-rpm-ci-config!

(define (read-rpm-ci-config c)
  (begin
    (define path (cfg-rpm-ci-config c))
    (define config (read-rktd-hash 'rpm-ci-config path))
    (validate-rpm-ci-config! config)
    config
  ) ; end begin read-rpm-ci-config
) ; end define read-rpm-ci-config

(define (rpm-ci-matrix-lines artifact-prefix targets)
  (apply
   string-append
   (for/list ([target (in-list targets)])
     (define id (hash-ref target 'id))
     (define packages (string-join (hash-ref target 'setup-packages) " "))
     f"          - id: {(yaml-single-quote id)}
            rpm_system: {(yaml-single-quote (hash-ref target 'rpm-system))}
            rpm_release: {(yaml-single-quote (hash-ref target 'rpm-release))}
            rpm_arch: {(yaml-single-quote (hash-ref target 'rpm-arch))}
            cache_mode: {(yaml-single-quote (hash-ref target 'cache-mode))}
            package_name: {(yaml-single-quote (hash-ref target 'package-name))}
            runner: {(yaml-single-quote (hash-ref target 'runner))}
            container: {(yaml-single-quote (hash-ref target 'container))}
            jobs: {(number->string (hash-ref target 'jobs))}
            artifact_name: {(yaml-single-quote (rpm-ci-target-artifact-name artifact-prefix target))}
            setup_packages: {(yaml-single-quote packages)}
"
   ) ; end for/list matrix target
  ) ; end apply string-append matrix lines
) ; end define rpm-ci-matrix-lines

(define (rpm-ci-workflow-content c config)
  (begin
    (define base-targets (rpm-ci-normalized-targets config))
    (define targets (ci-targets-with-cache-modes c base-targets))
    (define artifact-prefix
      (assert-rpm-ci-artifact-prefix (config-required-string 'rpm-ci-config config 'artifact-prefix)))
    (define release-tag
      (assert-rpm-ci-release-tag (config-required-string 'rpm-ci-config config 'release-tag)))
    (define release-name
      (assert-rpm-ci-release-name (config-required-string 'rpm-ci-config config 'release-name)))
    (define create-release?
      (config-required-boolean 'rpm-ci-config config 'create-release))
    (define matrix-id "${{ matrix.id }}")
    (define matrix-system "${{ matrix.rpm_system }}")
    (define matrix-release "${{ matrix.rpm_release }}")
    (define matrix-arch "${{ matrix.rpm_arch }}")
    (define matrix-cache-mode "${{ matrix.cache_mode }}")
    (define matrix-package-name "${{ matrix.package_name }}")
    (define matrix-runner "${{ matrix.runner }}")
    (define matrix-container "${{ matrix.container }}")
    (define matrix-jobs "${{ matrix.jobs }}")
    (define matrix-artifact-name "${{ matrix.artifact_name }}")
    (define matrix-setup-packages "${{ matrix.setup_packages }}")
    (define token-expr "${{ secrets.GITHUB_TOKEN }}")
    (define rpm-files-count "${#rpm_files[@]}")
    (define rpm-files-array "\"${rpm_files[@]}\"")
    f"{(generated-rpm-code-notice "#")}name: rpm build and release

on:
  push:
    branches:
      - main
  workflow_dispatch:

permissions:
  contents: read

jobs:
  build-rpm:
    name: build {matrix-system} {matrix-arch} {matrix-cache-mode}
    strategy:
      fail-fast: false
      matrix:
        include:
{(rpm-ci-matrix-lines artifact-prefix targets)}    runs-on: {matrix-runner}
    container:
      image: {matrix-container}
      options: --user root
    permissions:
      contents: read
    steps:
      - name: Checkout rpm-racket
        uses: actions/checkout@v6

      - name: Check generated repository layout
        shell: bash
        run: |
          set -euo pipefail
          test -f SPECS/{(cfg-package-name c)}.spec
          test -x scripts/build-rpm.sh
          test -x scripts/verify-rpm.sh
          test ! -e {(cfg-package-name c)}.repo
          test ! -d repo

      - name: Install RPM build dependencies
        shell: bash
        run: |
          set -euo pipefail
          packages=\"{matrix-setup-packages}\"
          [ -n \"$packages\" ] || {{ echo 'matrix setup_packages is empty'; exit 1; }}
          if command -v dnf >/dev/null 2>&1; then
            pm=dnf
          elif command -v yum >/dev/null 2>&1; then
            pm=yum
          else
            echo 'dnf or yum is required in the build container'
            exit 1
          fi
          if [ \"{matrix-system}\" = \"el9\" ]; then
            $pm -y install 'dnf-command(config-manager)' || $pm -y install dnf-plugins-core || true
            if command -v dnf >/dev/null 2>&1; then
              dnf config-manager --set-enabled crb || true
            fi
          fi
          $pm -y install $packages

      - name: Build RPM
        shell: bash
        run: |
          set -euo pipefail
          rm -rf \"$GITHUB_WORKSPACE/artifacts\" \"$GITHUB_WORKSPACE/.build/{matrix-id}\"
          mkdir -p \"$GITHUB_WORKSPACE/artifacts\" \"$GITHUB_WORKSPACE/.build\"
          scripts/build-rpm.sh \\
            --artifact-dir \"$GITHUB_WORKSPACE/artifacts\" \\
            --work-dir \"$GITHUB_WORKSPACE/.build/{matrix-id}\" \\
            --rpm-system \"{matrix-system}\" \\
            --rpm-release \"{matrix-release}\" \\
            --rpm-arch \"{matrix-arch}\" \\
            --cache-mode \"{matrix-cache-mode}\" \\
            --prefix {(shell-single-quoted (cfg-prefix c))} \\
            --jobs \"{matrix-jobs}\"

      - name: Install and uninstall smoke test
        shell: bash
        env:
          PACKAGE_NAME: {matrix-package-name}
          PACKAGE_VERSION: {(yaml-single-quote (cfg-source-version c))}
        run: |
          set -euo pipefail
          mapfile -t rpm_files < <(find \"$GITHUB_WORKSPACE/artifacts\" -maxdepth 1 -name '*.rpm' ! -name '*.src.rpm' -type f | sort)
          if [ \"{rpm-files-count}\" -ne 1 ]; then
            printf 'Expected exactly one RPM, got %s\\n' \"{rpm-files-count}\"
            printf '  %s\\n' {rpm-files-array}
            exit 1
          fi
          if command -v dnf >/dev/null 2>&1; then
            pm=dnf
          elif command -v yum >/dev/null 2>&1; then
            pm=yum
          else
            echo 'dnf or yum is required for RPM install smoke test'
            exit 1
          fi
          $pm -y install \"${{rpm_files[0]}}\"
          rpm -qa | grep -Ei '^(libedit|libedit-devel|editline)' || true
          cache_count=$(find /var/cache/racket/compiled -path '*/compiled/*.zo' 2>/dev/null | wc -l)
          [ \"$cache_count\" -gt 0 ] || {{ echo 'system compiled cache is empty after RPM install'; exit 1; }}
          runtime_collects_cache=\"/var/cache/racket/compiled{(cfg-prefix c)}/share/racket/collects\"
          runtime_pkgs_cache=\"/var/cache/racket/compiled{(cfg-prefix c)}/share/racket/pkgs\"
          rhombus_ephemeral_cache=\"{(cfg-prefix c)}/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod\"
          find \"$runtime_collects_cache\" -path '*/compiled/*.zo' -type f -print -quit | grep -q . \\
            || {{ echo \"runtime-keyed collects cache is empty after RPM install: $runtime_collects_cache\"; exit 1; }}
          find \"$runtime_pkgs_cache\" -path '*/compiled/*.zo' -type f -print -quit | grep -q . \\
            || {{ echo \"runtime-keyed package cache is empty after RPM install: $runtime_pkgs_cache\"; exit 1; }}
          find \"$rhombus_ephemeral_cache\" -path '*/compiled/*.zo' -type f -print -quit | grep -q . \\
            || {{ echo \"Rhombus demod cache is empty after RPM install: $rhombus_ephemeral_cache\"; exit 1; }}
          racket -e '(displayln (version))' | grep -F \"$PACKAGE_VERSION\"
          racket -e '(displayln f\"rpm-ci-ok\")' | grep -F 'rpm-ci-ok'
          racket -e '(require readline/readline) (displayln f\"rpm-readline-ok\")' | grep -F 'rpm-readline-ok'
          empty_home=$(mktemp -d)
          HOME=\"$empty_home\" racket -e '(require racket/list racket/match racket/file) (displayln f\"rpm-empty-home-ok\")' | grep -F 'rpm-empty-home-ok'
          HOME=\"$empty_home\" timeout 30s rhombus --version | grep -F 'Rhombus'
          HOME=\"$empty_home\" timeout 30s rhombus -e 'println(\"rpm-rhombus-ok\")' | grep -F 'rpm-rhombus-ok'
          rm -rf \"$empty_home\"
          empty_home=$(mktemp -d)
          HOME=\"$empty_home\" timeout 30s rhombus -e 'println(\"rpm-rhombus-fresh-home-ok\")' | grep -F 'rpm-rhombus-fresh-home-ok'
          rm -rf \"$empty_home\"
          raco pkg show --all >/tmp/raco-pkgs.txt
          rpm -e \"$PACKAGE_NAME\"
          if rpm -q \"$PACKAGE_NAME\" >/dev/null 2>&1; then
            echo \"Package still installed after rpm -e: $PACKAGE_NAME\"
            exit 1
          fi
          [ ! -d /var/cache/racket/compiled ] || {{ echo 'system compiled cache remains after RPM erase'; exit 1; }}

      - name: Upload RPM artifact
        uses: actions/upload-artifact@v6
        with:
          name: {matrix-artifact-name}
          path: artifacts/*.rpm
          if-no-files-found: error

  publish-rpm:
    needs: build-rpm
    runs-on: ubuntu-24.04
    permissions:
      actions: read
      contents: write
    steps:
      - name: Download RPM artifacts
        uses: actions/download-artifact@v6
        with:
          pattern: {(yaml-single-quote f"{artifact-prefix}-*")}
          path: release-assets
          merge-multiple: true

      - name: Publish RPM release assets
        shell: bash
        env:
          GH_TOKEN: {token-expr}
          GH_REPO: ${{{{ github.repository }}}}
          RELEASE_TAG: {(yaml-single-quote release-tag)}
          RELEASE_NAME: {(yaml-single-quote release-name)}
          CREATE_RELEASE: {(yaml-single-quote (if create-release? "true" "false"))}
          PACKAGE_NAME: {(yaml-single-quote (cfg-package-name c))}
          PACKAGE_VERSION: {(yaml-single-quote (cfg-source-version c))}
          EXPECTED_RPM_COUNT: {(number->string (length targets))}
        run: |
          set -euo pipefail
          command -v gh >/dev/null 2>&1 || {{ echo 'gh CLI is required on the publish runner'; exit 1; }}
          gh --version | sed -n '1,2p'
          gh auth status -h github.com || true
          mapfile -t rpm_files < <(find \"$GITHUB_WORKSPACE/release-assets\" -maxdepth 2 -name '*.rpm' ! -name '*.src.rpm' -type f | sort)
          printf 'Downloaded RPM files (%s):\\n' \"{rpm-files-count}\"
          printf '  %s\\n' {rpm-files-array}
          if [ \"{rpm-files-count}\" -ne \"$EXPECTED_RPM_COUNT\" ]; then
            printf 'Expected %s RPM assets, got %s\\n' \"$EXPECTED_RPM_COUNT\" \"{rpm-files-count}\"
            exit 1
          fi
          declare -A seen
          for rpm_file in \"${{rpm_files[@]}}\"; do
            asset_name=\"${{rpm_file##*/}}\"
            if [ -n \"${{seen[$asset_name]:-}}\" ]; then
              echo \"Duplicate RPM release asset name: $asset_name\"
              exit 1
            fi
            seen[\"$asset_name\"]=1
          done
          printf 'Release assets before upload for %s:\\n' \"$RELEASE_TAG\"
          gh api \"repos/${{GITHUB_REPOSITORY}}/releases/tags/$RELEASE_TAG\" --jq '.assets[].name' || true
          if ! gh release view \"$RELEASE_TAG\" --repo \"$GITHUB_REPOSITORY\" >/dev/null 2>&1; then
            if [ \"$CREATE_RELEASE\" != true ]; then
              echo \"GitHub release does not exist and create-release is false: $RELEASE_TAG\"
              exit 1
            fi
            gh release create \"$RELEASE_TAG\" --repo \"$GITHUB_REPOSITORY\" --title \"$RELEASE_NAME\" --notes \"Generated RPM artifacts for $PACKAGE_NAME $PACKAGE_VERSION.\"
          fi
          printf 'Uploading RPM files to release %s:\\n' \"$RELEASE_TAG\"
          printf '  %s\\n' {rpm-files-array}
          gh release upload \"$RELEASE_TAG\" --repo \"$GITHUB_REPOSITORY\" {rpm-files-array} --clobber
          printf 'Release assets after upload for %s:\\n' \"$RELEASE_TAG\"
          gh api \"repos/${{GITHUB_REPOSITORY}}/releases/tags/$RELEASE_TAG\" --jq '.assets[].name'
"
  ) ; end begin rpm-ci-workflow-content
) ; end define rpm-ci-workflow-content

(define (assert-rpm-ci-workflow-replaceable! path)
  (begin
    (when (file-exists? path)
      (define content (file->string path))
      (unless (string-contains? content generated-rpm-repo-notice-marker)
        (raise-user-error 'write-rpm-ci-workflow!
                          f"refusing to overwrite workflow without generated marker: {(clean-path-string path)}")
      ) ; end unless generated marker present
    ) ; end when workflow exists
  ) ; end begin assert-rpm-ci-workflow-replaceable!
) ; end define assert-rpm-ci-workflow-replaceable!

(define (validate-rpm-ci-workflow! c config path)
  (begin
    (validate-yaml! c path)
    (define content (file->string path))
    (define targets (ci-targets-with-cache-modes c (rpm-ci-normalized-targets config)))
    (define artifact-prefix
      (assert-rpm-ci-artifact-prefix (config-required-string 'rpm-ci-config config 'artifact-prefix)))
    (for ([needle (in-list (list "name: rpm build and release"
                                 generated-rpm-repo-notice-marker
                                 "push:"
                                 "workflow_dispatch:"
                                 "build-rpm:"
                                 "publish-rpm:"
                                 "scripts/build-rpm.sh"
                                 "scripts/verify-rpm.sh"
                                 "cache_mode:"
                                 "package_name:"
                                 "--cache-mode \"${{ matrix.cache_mode }}\""
                                 "actions/checkout@v6"
                                 "actions/upload-artifact@v6"
                                 "actions/download-artifact@v6"
                                 "GH_REPO:"
                                 "gh release upload"
                                 "--repo \"$GITHUB_REPOSITORY\""
                                 "contents: write"
                                 "Downloaded RPM files"
                                 "Release assets before upload"
                                 "Release assets after upload"
                                 "dnf-command(config-manager)"
                                 "config-manager --set-enabled crb"
                                 "$pm -y install \"${rpm_files[0]}\""
                                 "rpm -qa | grep -Ei"
                                 "system compiled cache is empty after RPM install"
                                 "runtime-keyed collects cache is empty after RPM install"
                                 "runtime-keyed package cache is empty after RPM install"
                                 "Rhombus demod cache is empty after RPM install"
                                 "rpm-empty-home-ok"
                                 "timeout 30s rhombus --version"
                                 "rpm-rhombus-ok"
                                 "rpm-rhombus-fresh-home-ok"
                                 "system compiled cache remains after RPM erase"
                                 "racket -e '(displayln f\"rpm-ci-ok\")'"
                                 "racket -e '(require readline/readline) (displayln f\"rpm-readline-ok\")'"
                                 "EXPECTED_RPM_COUNT:"))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-rpm-ci-workflow! f"RPM CI workflow missing: {needle}")
      ) ; end unless workflow contains required needle
    ) ; end for workflow needle
    (for ([target (in-list targets)])
      (for ([needle (in-list (list (hash-ref target 'id)
                                   (hash-ref target 'rpm-system)
                                   (hash-ref target 'rpm-arch)
                                   (hash-ref target 'cache-mode)
                                   (hash-ref target 'package-name)
                                   (hash-ref target 'runner)
                                   (hash-ref target 'container)
                                   (rpm-ci-target-artifact-name artifact-prefix target)))])
        (unless (string-contains? content needle)
          (raise-user-error 'validate-rpm-ci-workflow! f"RPM CI workflow missing target field: {needle}")
        ) ; end unless workflow contains target needle
      ) ; end for target needle
    ) ; end for target
  ) ; end begin validate-rpm-ci-workflow!
) ; end define validate-rpm-ci-workflow!

(define (write-rpm-ci-workflow! c config)
  (begin
    (assert-rpm-repo-root! c #:write? #t)
    (assert-executable 'write-rpm-ci-workflow! (cfg-ruby-bin c))
    (define workflow-path (rpm-ci-workflow-path c))
    (assert-rpm-ci-workflow-replaceable! workflow-path)
    (make-directory* (rpm-workflows-dir c))
    (write-text-file! workflow-path (rpm-ci-workflow-content c config))
    (validate-rpm-ci-workflow! c config workflow-path)
    (println/flush f"Generated RPM CI workflow: {(clean-path-string workflow-path)}")
  ) ; end begin write-rpm-ci-workflow!
) ; end define write-rpm-ci-workflow!

(define (print-rpm-ci-dry-run-plan! c config)
  (begin
    (assert-rpm-repo-root! c #:write? #f)
    (define workflow-path (rpm-ci-workflow-path c))
    (define targets (ci-targets-with-cache-modes c (rpm-ci-normalized-targets config)))
    (println/flush f"Would read RPM CI config: {(clean-path-string (cfg-rpm-ci-config c))}")
    (println/flush f"Would generate RPM CI workflow: {(clean-path-string workflow-path)}")
    (println/flush f"Would validate RPM CI workflow YAML with: {(cfg-ruby-bin c)}")
    (println/flush f"Would configure RPM CI target count: {(number->string (length targets))}")
    (for ([target (in-list targets)])
      (println/flush
       f"  - {(hash-ref target 'rpm-system)} {(hash-ref target 'rpm-arch)} {(hash-ref target 'cache-mode)} as {(hash-ref target 'package-name)} on {(hash-ref target 'runner)} in {(hash-ref target 'container)}")
    ) ; end for target
  ) ; end begin print-rpm-ci-dry-run-plan!
) ; end define print-rpm-ci-dry-run-plan!

(define (build-rpm-ci! c)
  (begin
    (define config (read-rpm-ci-config c))
    (if (cfg-dry-run? c)
        (print-rpm-ci-dry-run-plan! c config)
        (write-rpm-ci-workflow! c config))
  ) ; end begin build-rpm-ci!
) ; end define build-rpm-ci!

(define (deb-workflows-dir c)
  (build-path (cfg-deb-repo-root c) ".github" "workflows"))

(define (deb-ci-workflow-path c)
  (build-path (deb-workflows-dir c) "build-deb.yml"))

(define (generated-deb-code-notice comment-prefix)
  f"{comment-prefix} {generated-deb-repo-notice-marker}
{comment-prefix} Source of truth: {(generated-source-root)}
{comment-prefix} Humans and LLM agents must change package-racket and regenerate; manual deb-racket edits are not production-safe.

")

(define (assert-deb-ci-id value)
  (begin
    (assert-single-line-string 'deb-ci-config 'id value)
    (unless (regexp-match? #px"^[A-Za-z0-9_.+-]+$" value)
      (raise-user-error 'deb-ci-config
                        f"target id must contain only letters, digits, _, ., +, or -: {value}")
    ) ; end unless safe target id
    value
  ) ; end begin assert-deb-ci-id
) ; end define assert-deb-ci-id

(define (assert-deb-ci-runner value)
  (begin
    (assert-single-line-string 'deb-ci-config 'runner value)
    (unless (regexp-match? #px"^[A-Za-z0-9_.:-]+$" value)
      (raise-user-error 'deb-ci-config
                        f"runner must contain only letters, digits, _, ., :, or -: {value}")
    ) ; end unless safe runner
    value
  ) ; end begin assert-deb-ci-runner
) ; end define assert-deb-ci-runner

(define (assert-deb-ci-container value)
  (begin
    (assert-single-line-string 'deb-ci-config 'container value)
    (unless (regexp-match? #px"^[A-Za-z0-9._/:@-]+$" value)
      (raise-user-error 'deb-ci-config
                        f"container must contain only image-reference characters: {value}")
    ) ; end unless safe container
    value
  ) ; end begin assert-deb-ci-container
) ; end define assert-deb-ci-container

(define (assert-deb-ci-package value)
  (begin
    (assert-single-line-string 'deb-ci-config 'setup-packages value)
    (unless (regexp-match? #px"^[A-Za-z0-9._+:@/-]+$" value)
      (raise-user-error 'deb-ci-config
                        f"setup package contains unsupported characters: {value}")
    ) ; end unless safe package
    value
  ) ; end begin assert-deb-ci-package
) ; end define assert-deb-ci-package

(define (assert-deb-ci-artifact-prefix value)
  (begin
    (assert-single-line-string 'deb-ci-config 'artifact-prefix value)
    (unless (regexp-match? #px"^[A-Za-z0-9_.+-]+$" value)
      (raise-user-error 'deb-ci-config
                        f"artifact-prefix must contain only letters, digits, _, ., +, or -: {value}")
    ) ; end unless safe artifact prefix
    value
  ) ; end begin assert-deb-ci-artifact-prefix
) ; end define assert-deb-ci-artifact-prefix

(define (assert-deb-ci-release-tag value)
  (begin
    (assert-single-line-string 'deb-ci-config 'release-tag value)
    (unless (regexp-match? #px"^[A-Za-z0-9._/-]+$" value)
      (raise-user-error 'deb-ci-config
                        f"release-tag must contain only letters, digits, _, ., /, or -: {value}")
    ) ; end unless safe release tag
    value
  ) ; end begin assert-deb-ci-release-tag
) ; end define assert-deb-ci-release-tag

(define (assert-deb-ci-release-name value)
  (assert-single-line-string 'deb-ci-config 'release-name value))

(define (deb-ci-normalize-target target)
  (begin
    (unless (hash? target)
      (raise-user-error 'deb-ci-config "each target must be a hash")
    ) ; end unless target hash
    (define id
      (assert-deb-ci-id (config-required-string 'deb-ci-config target 'id)))
    (define system
      (assert-deb-system (config-required-string 'deb-ci-config target 'deb-system)))
    (define release
      (assert-deb-release (config-required-string 'deb-ci-config target 'deb-release)))
    (define arch
      (normalize-deb-arch (config-required-string 'deb-ci-config target 'deb-arch)))
    (define runner
      (assert-deb-ci-runner (config-required-string 'deb-ci-config target 'runner)))
    (define container
      (assert-deb-ci-container (config-required-string 'deb-ci-config target 'container)))
    (define packages
      (config-required-list 'deb-ci-config target 'setup-packages))
    (when (null? packages)
      (raise-user-error 'deb-ci-config f"target {id} setup-packages must not be empty")
    ) ; end when empty setup packages
    (for ([pkg (in-list packages)])
      (unless (string? pkg)
        (raise-user-error 'deb-ci-config f"target {id} setup-packages must contain only strings")
      ) ; end unless package string
      (assert-deb-ci-package pkg)
    ) ; end for setup package
    (define jobs
      (required-config-positive-integer target 'jobs))
    (hash 'id id
          'deb-system system
          'deb-release release
          'deb-arch arch
          'runner runner
          'container container
          'setup-packages packages
          'jobs jobs)
  ) ; end begin deb-ci-normalize-target
) ; end define deb-ci-normalize-target

(define (deb-ci-target-artifact-name artifact-prefix target)
  f"{artifact-prefix}-{(hash-ref target 'id)}")

(define (deb-ci-normalized-targets config)
  (begin
    (define raw-targets (config-required-list 'deb-ci-config config 'targets))
    (when (null? raw-targets)
      (raise-user-error 'deb-ci-config "targets must not be empty")
    ) ; end when no targets
    (define targets (map deb-ci-normalize-target raw-targets))
    (define ids (map (lambda (target) (hash-ref target 'id)) targets))
    (unless (= (length ids) (length (remove-duplicates ids string=?)))
      (raise-user-error 'deb-ci-config "target ids must be unique")
    ) ; end unless unique target ids
    targets
  ) ; end begin deb-ci-normalized-targets
) ; end define deb-ci-normalized-targets

(define (validate-deb-ci-config! config)
  (begin
    (assert-deb-ci-release-tag (config-required-string 'deb-ci-config config 'release-tag))
    (assert-deb-ci-release-name (config-required-string 'deb-ci-config config 'release-name))
    (assert-deb-ci-artifact-prefix (config-required-string 'deb-ci-config config 'artifact-prefix))
    (config-required-boolean 'deb-ci-config config 'create-release)
    (deb-ci-normalized-targets config)
    (void)
  ) ; end begin validate-deb-ci-config!
) ; end define validate-deb-ci-config!

(define (read-deb-ci-config c)
  (begin
    (define path (cfg-deb-ci-config c))
    (define config (read-rktd-hash 'deb-ci-config path))
    (validate-deb-ci-config! config)
    config
  ) ; end begin read-deb-ci-config
) ; end define read-deb-ci-config

(define (deb-ci-matrix-lines artifact-prefix targets)
  (apply
   string-append
   (for/list ([target (in-list targets)])
     (define id (hash-ref target 'id))
     (define packages (string-join (hash-ref target 'setup-packages) " "))
     f"          - id: {(yaml-single-quote id)}
            deb_system: {(yaml-single-quote (hash-ref target 'deb-system))}
            deb_release: {(yaml-single-quote (hash-ref target 'deb-release))}
            deb_arch: {(yaml-single-quote (hash-ref target 'deb-arch))}
            cache_mode: {(yaml-single-quote (hash-ref target 'cache-mode))}
            package_name: {(yaml-single-quote (hash-ref target 'package-name))}
            runner: {(yaml-single-quote (hash-ref target 'runner))}
            container: {(yaml-single-quote (hash-ref target 'container))}
            jobs: {(number->string (hash-ref target 'jobs))}
            artifact_name: {(yaml-single-quote (deb-ci-target-artifact-name artifact-prefix target))}
            setup_packages: {(yaml-single-quote packages)}
"
   ) ; end for/list matrix target
  ) ; end apply string-append matrix lines
) ; end define deb-ci-matrix-lines

(define (deb-ci-workflow-content c config)
  (begin
    (define base-targets (deb-ci-normalized-targets config))
    (define targets (ci-targets-with-cache-modes c base-targets))
    (define artifact-prefix
      (assert-deb-ci-artifact-prefix (config-required-string 'deb-ci-config config 'artifact-prefix)))
    (define release-tag
      (assert-deb-ci-release-tag (config-required-string 'deb-ci-config config 'release-tag)))
    (define release-name
      (assert-deb-ci-release-name (config-required-string 'deb-ci-config config 'release-name)))
    (define create-release?
      (config-required-boolean 'deb-ci-config config 'create-release))
    (define matrix-system "${{ matrix.deb_system }}")
    (define matrix-release "${{ matrix.deb_release }}")
    (define matrix-arch "${{ matrix.deb_arch }}")
    (define matrix-cache-mode "${{ matrix.cache_mode }}")
    (define matrix-package-name "${{ matrix.package_name }}")
    (define matrix-runner "${{ matrix.runner }}")
    (define matrix-container "${{ matrix.container }}")
    (define matrix-jobs "${{ matrix.jobs }}")
    (define matrix-id "${{ matrix.id }}")
    (define matrix-artifact-name "${{ matrix.artifact_name }}")
    (define matrix-setup-packages "${{ matrix.setup_packages }}")
    (define token-expr "${{ secrets.GITHUB_TOKEN }}")
    (define deb-files-count "${#deb_files[@]}")
    (define deb-files-array "\"${deb_files[@]}\"")
    f"{(generated-deb-code-notice "#")}name: deb build and release

on:
  push:
    branches:
      - main
  workflow_dispatch:

permissions:
  contents: read

jobs:
  build-deb:
    name: build {matrix-system} {matrix-arch} {matrix-cache-mode}
    strategy:
      fail-fast: false
      matrix:
        include:
{(deb-ci-matrix-lines artifact-prefix targets)}    runs-on: {matrix-runner}
    container:
      image: {matrix-container}
      options: --user root
    permissions:
      contents: read
    steps:
      - name: Checkout deb-racket
        uses: actions/checkout@v6

      - name: Check generated repository layout
        shell: bash
        run: |
          set -euo pipefail
          test -x scripts/build-deb.sh
          test -x scripts/verify-deb.sh
          test -f SOURCES/.gitkeep

      - name: Install Debian build dependencies
        shell: bash
        env:
          DEBIAN_FRONTEND: noninteractive
        run: |
          set -euo pipefail
          packages=\"{matrix-setup-packages}\"
          [ -n \"$packages\" ] || {{ echo 'matrix setup_packages is empty'; exit 1; }}
          apt-get update
          apt-get install -y --no-install-recommends $packages

      - name: Build DEB
        shell: bash
        run: |
          set -euo pipefail
          rm -rf \"$GITHUB_WORKSPACE/artifacts\" \"$GITHUB_WORKSPACE/.build/{matrix-id}\"
          mkdir -p \"$GITHUB_WORKSPACE/artifacts\" \"$GITHUB_WORKSPACE/.build\"
          scripts/build-deb.sh \\
            --artifact-dir \"$GITHUB_WORKSPACE/artifacts\" \\
            --work-dir \"$GITHUB_WORKSPACE/.build/{matrix-id}\" \\
            --deb-system \"{matrix-system}\" \\
            --deb-release \"{matrix-release}\" \\
            --deb-arch \"{matrix-arch}\" \\
            --cache-mode \"{matrix-cache-mode}\" \\
            --prefix {(shell-single-quoted (cfg-prefix c))} \\
            --jobs \"{matrix-jobs}\"

      - name: Install and uninstall smoke test
        shell: bash
        env:
          DEBIAN_FRONTEND: noninteractive
          PACKAGE_NAME: {matrix-package-name}
          PACKAGE_VERSION: {(yaml-single-quote (cfg-source-version c))}
        run: |
          set -euo pipefail
          smoke_step() {{ printf 'DEB smoke: %s\\n' \"$1\"; }}
          mapfile -t deb_files < <(find \"$GITHUB_WORKSPACE/artifacts\" -maxdepth 1 -name '*.deb' -type f | sort)
          if [ \"{deb-files-count}\" -ne 1 ]; then
            printf 'Expected exactly one DEB, got %s\\n' \"{deb-files-count}\"
            printf '  %s\\n' {deb-files-array}
            exit 1
          fi
          smoke_step 'apt install package'
          apt-get install -y \"${{deb_files[0]}}\"
          smoke_step 'dpkg package status'
          dpkg -s \"$PACKAGE_NAME\" >/dev/null
          smoke_step 'system compiled cache count'
          cache_count=$(find /var/cache/racket/compiled -path '*/compiled/*.zo' 2>/dev/null | wc -l)
          [ \"$cache_count\" -gt 0 ] || {{ echo 'system compiled cache is empty after DEB install'; exit 1; }}
          runtime_collects_cache=\"/var/cache/racket/compiled{(cfg-prefix c)}/share/racket/collects\"
          runtime_pkgs_cache=\"/var/cache/racket/compiled{(cfg-prefix c)}/share/racket/pkgs\"
          rhombus_ephemeral_cache=\"{(cfg-prefix c)}/share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod\"
          smoke_step 'runtime-keyed collects cache'
          find \"$runtime_collects_cache\" -path '*/compiled/*.zo' -type f -print -quit 2>/dev/null | grep -q . \\
            || {{ echo \"runtime-keyed collects cache is empty after DEB install: $runtime_collects_cache\"; exit 1; }}
          smoke_step 'runtime-keyed package cache'
          find \"$runtime_pkgs_cache\" -path '*/compiled/*.zo' -type f -print -quit 2>/dev/null | grep -q . \\
            || {{ echo \"runtime-keyed package cache is empty after DEB install: $runtime_pkgs_cache\"; exit 1; }}
          smoke_step 'Rhombus demod cache'
          find \"$rhombus_ephemeral_cache\" -path '*/compiled/*.zo' -type f -print -quit 2>/dev/null | grep -q . \\
            || {{ echo \"Rhombus demod cache is empty after DEB install: $rhombus_ephemeral_cache\"; exit 1; }}
          smoke_step 'racket version'
          racket -e '(displayln (version))' | grep -F \"$PACKAGE_VERSION\"
          smoke_step 'racket f-string'
          racket -e '(displayln f\"deb-ci-ok\")' | grep -F 'deb-ci-ok'
          smoke_step 'readline package'
          racket -e '(require readline/readline) (displayln f\"deb-readline-ok\")' | grep -F 'deb-readline-ok'
          smoke_step 'fresh HOME racket libraries'
          empty_home=$(mktemp -d)
          HOME=\"$empty_home\" racket -e '(require racket/list racket/match racket/file) (displayln f\"deb-empty-home-ok\")' | grep -F 'deb-empty-home-ok'
          smoke_step 'Rhombus version'
          HOME=\"$empty_home\" timeout 30s rhombus --version | grep -F 'Rhombus'
          smoke_step 'Rhombus expression'
          HOME=\"$empty_home\" timeout 30s rhombus -e 'println(\"deb-rhombus-ok\")' | grep -F 'deb-rhombus-ok'
          rm -rf \"$empty_home\"
          smoke_step 'Rhombus fresh HOME expression'
          empty_home=$(mktemp -d)
          HOME=\"$empty_home\" timeout 30s rhombus -e 'println(\"deb-rhombus-fresh-home-ok\")' | grep -F 'deb-rhombus-fresh-home-ok'
          rm -rf \"$empty_home\"
          smoke_step 'raco package database'
          raco pkg show --all >/tmp/raco-pkgs.txt
          smoke_step 'apt purge package'
          apt-get purge -y \"$PACKAGE_NAME\"
          smoke_step 'dpkg package absent after purge'
          if dpkg -s \"$PACKAGE_NAME\" >/dev/null 2>&1; then
            echo \"Package still installed after apt-get purge: $PACKAGE_NAME\"
            dpkg -s \"$PACKAGE_NAME\" || true
            exit 1
          fi
          smoke_step 'system compiled cache removed after purge'
          [ ! -d /var/cache/racket/compiled ] || {{ echo 'system compiled cache remains after DEB purge'; find /var/cache/racket/compiled -maxdepth 5 -print; exit 1; }}

      - name: Upload DEB artifact
        uses: actions/upload-artifact@v6
        with:
          name: {matrix-artifact-name}
          path: artifacts/*.deb
          if-no-files-found: error

  publish-deb:
    needs: build-deb
    runs-on: ubuntu-24.04
    permissions:
      actions: read
      contents: write
    steps:
      - name: Download DEB artifacts
        uses: actions/download-artifact@v6
        with:
          pattern: {(yaml-single-quote f"{artifact-prefix}-*")}
          path: release-assets
          merge-multiple: true

      - name: Publish DEB release assets
        shell: bash
        env:
          GH_TOKEN: {token-expr}
          GH_REPO: ${{{{ github.repository }}}}
          RELEASE_TAG: {(yaml-single-quote release-tag)}
          RELEASE_NAME: {(yaml-single-quote release-name)}
          CREATE_RELEASE: {(yaml-single-quote (if create-release? "true" "false"))}
          PACKAGE_NAME: {(yaml-single-quote (cfg-package-name c))}
          PACKAGE_VERSION: {(yaml-single-quote (cfg-source-version c))}
          EXPECTED_DEB_COUNT: {(number->string (length targets))}
        run: |
          set -euo pipefail
          command -v gh >/dev/null 2>&1 || {{ echo 'gh CLI is required on the publish runner'; exit 1; }}
          gh --version | sed -n '1,2p'
          gh auth status -h github.com || true
          mapfile -t deb_files < <(find \"$GITHUB_WORKSPACE/release-assets\" -maxdepth 2 -name '*.deb' -type f | sort)
          printf 'Downloaded DEB files (%s):\\n' \"{deb-files-count}\"
          printf '  %s\\n' {deb-files-array}
          if [ \"{deb-files-count}\" -ne \"$EXPECTED_DEB_COUNT\" ]; then
            printf 'Expected %s DEB assets, got %s\\n' \"$EXPECTED_DEB_COUNT\" \"{deb-files-count}\"
            exit 1
          fi
          declare -A seen
          for deb_file in \"${{deb_files[@]}}\"; do
            asset_name=\"${{deb_file##*/}}\"
            if [ -n \"${{seen[$asset_name]:-}}\" ]; then
              echo \"Duplicate DEB release asset name: $asset_name\"
              exit 1
            fi
            seen[\"$asset_name\"]=1
          done
          printf 'Release assets before upload for %s:\\n' \"$RELEASE_TAG\"
          gh api \"repos/${{GITHUB_REPOSITORY}}/releases/tags/$RELEASE_TAG\" --jq '.assets[].name' || true
          if ! gh release view \"$RELEASE_TAG\" --repo \"$GITHUB_REPOSITORY\" >/dev/null 2>&1; then
            if [ \"$CREATE_RELEASE\" != true ]; then
              echo \"GitHub release does not exist and create-release is false: $RELEASE_TAG\"
              exit 1
            fi
            gh release create \"$RELEASE_TAG\" --repo \"$GITHUB_REPOSITORY\" --title \"$RELEASE_NAME\" --notes \"Generated DEB artifacts for $PACKAGE_NAME $PACKAGE_VERSION.\"
          fi
          printf 'Uploading DEB files to release %s:\\n' \"$RELEASE_TAG\"
          printf '  %s\\n' {deb-files-array}
          gh release upload \"$RELEASE_TAG\" --repo \"$GITHUB_REPOSITORY\" {deb-files-array} --clobber
          printf 'Release assets after upload for %s:\\n' \"$RELEASE_TAG\"
          gh api \"repos/${{GITHUB_REPOSITORY}}/releases/tags/$RELEASE_TAG\" --jq '.assets[].name'
"
  ) ; end begin deb-ci-workflow-content
) ; end define deb-ci-workflow-content

(define (assert-deb-ci-workflow-replaceable! path)
  (begin
    (when (file-exists? path)
      (define content (file->string path))
      (unless (string-contains? content generated-deb-repo-notice-marker)
        (raise-user-error 'write-deb-ci-workflow!
                          f"refusing to overwrite workflow without generated marker: {(clean-path-string path)}")
      ) ; end unless generated marker present
    ) ; end when workflow exists
  ) ; end begin assert-deb-ci-workflow-replaceable!
) ; end define assert-deb-ci-workflow-replaceable!

(define (validate-deb-ci-workflow! c config path)
  (begin
    (validate-yaml! c path)
    (define content (file->string path))
    (define targets (ci-targets-with-cache-modes c (deb-ci-normalized-targets config)))
    (define artifact-prefix
      (assert-deb-ci-artifact-prefix (config-required-string 'deb-ci-config config 'artifact-prefix)))
    (for ([needle (in-list (list "name: deb build and release"
                                 generated-deb-repo-notice-marker
                                 "build-deb:"
                                 "publish-deb:"
                                 "scripts/build-deb.sh"
                                 "scripts/verify-deb.sh"
                                 "cache_mode:"
                                 "package_name:"
                                 "--cache-mode \"${{ matrix.cache_mode }}\""
                                 "actions/upload-artifact@v6"
                                 "actions/download-artifact@v6"
                                 "GH_REPO:"
                                 "gh release upload"
                                 "--repo \"$GITHUB_REPOSITORY\""
                                 "smoke_step()"
                                 "DEB smoke: %s"
                                 "smoke_step 'apt install package'"
                                 "apt-get install -y \"${deb_files[0]}\""
                                 "system compiled cache is empty after DEB install"
                                 "runtime-keyed collects cache is empty after DEB install"
                                 "runtime-keyed package cache is empty after DEB install"
                                 "Rhombus demod cache is empty after DEB install"
                                 "deb-empty-home-ok"
                                 "timeout 30s rhombus --version"
                                 "deb-rhombus-ok"
                                 "deb-rhombus-fresh-home-ok"
                                 "system compiled cache remains after DEB purge"
                                 "Downloaded DEB files"
                                 "Release assets before upload"
                                 "Release assets after upload"
                                 "racket -e '(displayln f\"deb-ci-ok\")'"
                                 "racket -e '(require readline/readline) (displayln f\"deb-readline-ok\")'"
                                 "EXPECTED_DEB_COUNT:"))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-deb-ci-workflow! f"DEB CI workflow missing: {needle}")
      ) ; end unless workflow contains required needle
    ) ; end for workflow needle
    (for ([target (in-list targets)])
      (for ([needle (in-list (list (hash-ref target 'id)
                                   (hash-ref target 'deb-system)
                                   (hash-ref target 'deb-arch)
                                   (hash-ref target 'cache-mode)
                                   (hash-ref target 'package-name)
                                   (hash-ref target 'runner)
                                   (hash-ref target 'container)
                                   (deb-ci-target-artifact-name artifact-prefix target)))])
        (unless (string-contains? content needle)
          (raise-user-error 'validate-deb-ci-workflow! f"DEB CI workflow missing target field: {needle}")
        ) ; end unless workflow contains target needle
      ) ; end for target needle
    ) ; end for target
  ) ; end begin validate-deb-ci-workflow!
) ; end define validate-deb-ci-workflow!

(define (write-deb-ci-workflow! c config)
  (begin
    (assert-deb-repo-root! c #:write? #t)
    (assert-executable 'write-deb-ci-workflow! (cfg-ruby-bin c))
    (define workflow-path (deb-ci-workflow-path c))
    (assert-deb-ci-workflow-replaceable! workflow-path)
    (make-directory* (deb-workflows-dir c))
    (write-text-file! workflow-path (deb-ci-workflow-content c config))
    (validate-deb-ci-workflow! c config workflow-path)
    (println/flush f"Generated DEB CI workflow: {(clean-path-string workflow-path)}")
  ) ; end begin write-deb-ci-workflow!
) ; end define write-deb-ci-workflow!

(define (print-deb-ci-dry-run-plan! c config)
  (begin
    (assert-deb-repo-root! c #:write? #f)
    (define workflow-path (deb-ci-workflow-path c))
    (define targets (ci-targets-with-cache-modes c (deb-ci-normalized-targets config)))
    (println/flush f"Would read DEB CI config: {(clean-path-string (cfg-deb-ci-config c))}")
    (println/flush f"Would generate DEB CI workflow: {(clean-path-string workflow-path)}")
    (println/flush f"Would validate DEB CI workflow YAML with: {(cfg-ruby-bin c)}")
    (println/flush f"Would configure DEB CI target count: {(number->string (length targets))}")
    (for ([target (in-list targets)])
      (println/flush
       f"  - {(hash-ref target 'deb-system)} {(hash-ref target 'deb-arch)} {(hash-ref target 'cache-mode)} as {(hash-ref target 'package-name)} on {(hash-ref target 'runner)} in {(hash-ref target 'container)}")
    ) ; end for target
  ) ; end begin print-deb-ci-dry-run-plan!
) ; end define print-deb-ci-dry-run-plan!

(define (build-deb-ci! c)
  (begin
    (define config (read-deb-ci-config c))
    (if (cfg-dry-run? c)
        (print-deb-ci-dry-run-plan! c config)
        (write-deb-ci-workflow! c config))
  ) ; end begin build-deb-ci!
) ; end define build-deb-ci!

(define generated-windows-ci-notice-marker
  "GENERATED WINDOWS PORTABLE PACKAGING METADATA - DO NOT EDIT.")

(define (generated-windows-ci-code-notice comment-prefix)
  f"{comment-prefix} {generated-windows-ci-notice-marker}
{comment-prefix} Source of truth: {(generated-source-root)}
{comment-prefix} Humans and LLM agents must change package-racket and regenerate; manual workflow edits are not production-safe.

")

(define (windows-ci-readme-path c)
  (build-path (cfg-windows-repo-root c) "README.md"))

(define (assert-windows-ci-safe-token value)
  (begin
    (assert-single-line-string 'windows-ci-config 'token-secret value)
    (unless (regexp-match? #px"^[A-Z0-9_]+$" value)
      (raise-user-error 'windows-ci-config
                        f"token-secret must contain only uppercase letters, digits, or _: {value}")
    ) ; end unless safe secret name
    value
  ) ; end begin assert-windows-ci-safe-token
) ; end define assert-windows-ci-safe-token

(define (assert-windows-ci-runner value)
  (begin
    (assert-single-line-string 'windows-ci-config 'runner value)
    (unless (regexp-match? #px"^[A-Za-z0-9_.:-]+$" value)
      (raise-user-error 'windows-ci-config
                        f"runner must contain only letters, digits, _, ., :, or -: {value}")
    ) ; end unless safe runner
    value
  ) ; end begin assert-windows-ci-runner
) ; end define assert-windows-ci-runner

(define (assert-windows-ci-artifact-prefix value)
  (begin
    (assert-single-line-string 'windows-ci-config 'artifact-prefix value)
    (unless (regexp-match? #px"^[A-Za-z0-9_.+-]+$" value)
      (raise-user-error 'windows-ci-config
                        f"artifact-prefix must contain only letters, digits, _, ., +, or -: {value}")
    ) ; end unless safe artifact prefix
    value
  ) ; end begin assert-windows-ci-artifact-prefix
) ; end define assert-windows-ci-artifact-prefix

(define (assert-windows-ci-release-tag value)
  (begin
    (assert-single-line-string 'windows-ci-config 'release-tag value)
    (unless (regexp-match? #px"^[A-Za-z0-9._/-]+$" value)
      (raise-user-error 'windows-ci-config
                        f"release-tag must contain only letters, digits, _, ., /, or -: {value}")
    ) ; end unless safe release tag
    value
  ) ; end begin assert-windows-ci-release-tag
) ; end define assert-windows-ci-release-tag

(define (assert-windows-ci-release-name value)
  (assert-single-line-string 'windows-ci-config 'release-name value))

(define (assert-windows-ci-arch value)
  (begin
    (assert-single-line-string 'windows-ci-config 'arch value)
    (match (string-downcase value)
      [(or "x86_64" "amd64" "x64") "x86_64"]
      [_ (raise-user-error 'windows-ci-config
                           f"arch must be x86_64, amd64, or x64 for this first portable target: {value}")]
    ) ; end match arch value
  ) ; end begin assert-windows-ci-arch
) ; end define assert-windows-ci-arch

(define (assert-windows-ci-msvc-arch value)
  (begin
    (assert-single-line-string 'windows-ci-config 'msvc-arch value)
    (unless (member value '("x64" "x86" "x86_amd64" "x64_arm64") string=?)
      (raise-user-error 'windows-ci-config
                        f"msvc-arch must be x64, x86, x86_amd64, or x64_arm64: {value}")
    ) ; end unless known msvc mode
    value
  ) ; end begin assert-windows-ci-msvc-arch
) ; end define assert-windows-ci-msvc-arch

(define (assert-windows-ci-nmake-target value)
  (begin
    (assert-single-line-string 'windows-ci-config 'nmake-target value)
    (unless (regexp-match? #px"^[A-Za-z0-9_.:+-]+$" value)
      (raise-user-error 'windows-ci-config
                        f"nmake-target contains unsupported characters: {value}")
    ) ; end unless safe target
    value
  ) ; end begin assert-windows-ci-nmake-target
) ; end define assert-windows-ci-nmake-target

(define (assert-windows-ci-portable-dir value)
  (begin
    (assert-single-line-string 'windows-ci-config 'portable-dir-name value)
    (unless (regexp-match? #px"^[A-Za-z0-9_.+-]+$" value)
      (raise-user-error 'windows-ci-config
                        f"portable-dir-name must contain only letters, digits, _, ., +, or -: {value}")
    ) ; end unless safe portable dir
    value
  ) ; end begin assert-windows-ci-portable-dir
) ; end define assert-windows-ci-portable-dir

(define (assert-windows-ci-release-repo value)
  (begin
    (assert-single-line-string 'windows-ci-config 'release-repo value)
    (unless (regexp-match? #px"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$" value)
      (raise-user-error 'windows-ci-config
                        f"release-repo must be OWNER/REPO: {value}")
    ) ; end unless owner/repo
    value
  ) ; end begin assert-windows-ci-release-repo
) ; end define assert-windows-ci-release-repo

(define (validate-windows-ci-config! config)
  (begin
    (config-required-string 'windows-ci-config config 'windows-repo-root)
    (assert-windows-ci-runner (config-required-string 'windows-ci-config config 'runner))
    (assert-windows-ci-arch (config-required-string 'windows-ci-config config 'arch))
    (assert-windows-ci-msvc-arch (config-required-string 'windows-ci-config config 'msvc-arch))
    (assert-windows-ci-nmake-target (config-required-string 'windows-ci-config config 'nmake-target))
    (assert-windows-ci-portable-dir (config-required-string 'windows-ci-config config 'portable-dir-name))
    (assert-windows-ci-artifact-prefix (config-required-string 'windows-ci-config config 'artifact-prefix))
    (assert-windows-ci-release-repo (config-required-string 'windows-ci-config config 'release-repo))
    (assert-windows-ci-release-tag (config-required-string 'windows-ci-config config 'release-tag))
    (assert-windows-ci-release-name (config-required-string 'windows-ci-config config 'release-name))
    (config-required-boolean 'windows-ci-config config 'create-release)
    (config-required-boolean 'windows-ci-config config 'publish-release)
    (assert-windows-ci-safe-token (config-required-string 'windows-ci-config config 'token-secret))
    (required-config-positive-integer config 'build-jobs)
    (void)
  ) ; end begin validate-windows-ci-config!
) ; end define validate-windows-ci-config!

(define (read-windows-ci-config c)
  (begin
    (define path (cfg-windows-ci-config c))
    (define config (read-rktd-hash 'windows-ci-config path))
    (validate-windows-ci-config! config)
    config
  ) ; end begin read-windows-ci-config
) ; end define read-windows-ci-config

(define (read-windows-ci-config-values config-path)
  (begin
    (define raw (read-rktd-hash 'windows-ci-config config-path))
    (validate-windows-ci-config! raw)
    (define root-value
      (config-required-string 'windows-ci-config raw 'windows-repo-root))
    (values (resolve-config-path config-path root-value))
  ) ; end begin read-windows-ci-config-values
) ; end define read-windows-ci-config-values

(define (windows-ci-workflows-dir c)
  (build-path (cfg-windows-repo-root c) ".github" "workflows"))

(define (windows-ci-workflow-path c)
  (build-path (windows-ci-workflows-dir c) "build-windows-portable.yml"))

(define (assert-windows-ci-repo-root! c #:write? [write? #t])
  (begin
    (define root (cfg-windows-repo-root c))
    (assert-directory 'windows-ci root)
    (assert-directory 'windows-ci (build-path root ".git"))
    (when write?
      (assert-writable-directory 'windows-ci root)
    ) ; end when write check requested
  ) ; end begin assert-windows-ci-repo-root!
) ; end define assert-windows-ci-repo-root!

(define (windows-ci-portable-zip-name c config)
  (begin
    (define arch
      (assert-windows-ci-arch (config-required-string 'windows-ci-config config 'arch)))
    f"{(cfg-package-name c)}-{(cfg-formula-version c)}-windows-{arch}.zip"
  ) ; end begin windows-ci-portable-zip-name
) ; end define windows-ci-portable-zip-name

(define (windows-ci-installer-exe-name c config)
  (begin
    (define arch
      (assert-windows-ci-arch (config-required-string 'windows-ci-config config 'arch)))
    f"{(cfg-package-name c)}-{(cfg-formula-version c)}-windows-{arch}-setup.exe"
  ) ; end begin windows-ci-installer-exe-name
) ; end define windows-ci-installer-exe-name

(define (windows-ci-artifact-name config)
  (begin
    (define artifact-prefix
      (assert-windows-ci-artifact-prefix (config-required-string 'windows-ci-config config 'artifact-prefix)))
    (define arch
      (assert-windows-ci-arch (config-required-string 'windows-ci-config config 'arch)))
    f"{artifact-prefix}-{arch}"
  ) ; end begin windows-ci-artifact-name
) ; end define windows-ci-artifact-name

(define (windows-ci-readme-content c config)
  (begin
    (define arch
      (assert-windows-ci-arch (config-required-string 'windows-ci-config config 'arch)))
    (define runner
      (assert-windows-ci-runner (config-required-string 'windows-ci-config config 'runner)))
    (define zip-name (windows-ci-portable-zip-name c config))
    (define exe-name (windows-ci-installer-exe-name c config))
    (define release-note
      (if (config-required-boolean 'windows-ci-config config 'publish-release)
          (let ([release-repo
                 (assert-windows-ci-release-repo
                  (config-required-string 'windows-ci-config config 'release-repo))]
                [release-tag
                 (assert-windows-ci-release-tag
                  (config-required-string 'windows-ci-config config 'release-tag))]
                [token-secret
                 (assert-windows-ci-safe-token
                  (config-required-string 'windows-ci-config config 'token-secret))])
            f"Release asset publishing is enabled. The workflow uploads `{zip-name}` and `{exe-name}` to `{release-repo}` release `{release-tag}` using the `{token-secret}` repository secret.")
          "Release asset publishing is disabled; successful runs retain the zip and installer as GitHub Actions artifacts."))
    f"# win-racket

{generated-windows-ci-notice-marker}

This repository is the Windows portable package build repository generated by
`package-racket`. It intentionally stays small: the only generated production
files are this README and `.github/workflows/build-windows-portable.yml`.

Do not hand-edit production packaging behavior here. Change `package-racket`
and regenerate this repository instead.

## Build

The generated workflow builds Racket on `{runner}` for `{arch}` and uploads
`{zip-name}` and `{exe-name}` as GitHub Actions artifacts. It runs `nmake all` before the
configured `nmake` target so a clean CI checkout never tries to install missing
build outputs. The portable archive and Inno Setup installer copy only the
installed runtime tree, not the source/build tree. The installer accepts
`/DIR=...` for the install path and `/CACHEPATH=...` for the Racket cache path;
the default cache path is inside the install directory. {release-note}

## Regenerate

Run from `package-racket`:

```sh
racket package-racket.rkt \\
  --target windows-portable-ci \\
  --windows-ci-config {(clean-path-string (cfg-windows-ci-config c))}
```
"
  ) ; end begin windows-ci-readme-content
) ; end define windows-ci-readme-content

(define (resolve-source-archive-sha256! who c source-url label)
  (begin
    (define local-source (brew-output-tgz c))
    (cond
      [(file-exists? local-source)
       (define sha (sha256-file local-source))
       (println/flush f"{label} sha256 from local artifact: {sha}")
       sha]
      [else
       (define-values (owner repo tag asset-name)
         (github-release-download-url-values who source-url)
       ) ; end define-values release URL parts
       (define remote-digest
         (github-release-asset-sha256/digest who owner repo tag asset-name)
       ) ; end define remote digest
       (cond
         [remote-digest
          (println/flush f"{label} sha256 from GitHub release digest: {remote-digest}")
          remote-digest]
         [else
          (define source-dir (build-path (cfg-work-dir c) "source-archive-sha256"))
          (define downloaded-source (build-path source-dir asset-name))
          (reset-managed-dir! who source-dir)
          (println/flush f"Downloading {label} for sha256: {source-url}")
          (download-https-url! who source-url downloaded-source)
          (assert-nonempty-file who downloaded-source)
          (define sha (sha256-file downloaded-source))
          (println/flush f"{label} sha256 from downloaded artifact: {sha}")
          sha]
       ) ; end cond remote digest
      ]
    ) ; end cond local or remote source
  ) ; end begin resolve-source-archive-sha256!
) ; end define resolve-source-archive-sha256!

(define (windows-ci-publish-job-content config)
  (begin
    (define publish-release?
      (config-required-boolean 'windows-ci-config config 'publish-release))
    (if publish-release?
        (let* ([release-repo
                (assert-windows-ci-release-repo
                 (config-required-string 'windows-ci-config config 'release-repo))]
               [release-tag
                (assert-windows-ci-release-tag
                 (config-required-string 'windows-ci-config config 'release-tag))]
               [release-name
                (assert-windows-ci-release-name
                 (config-required-string 'windows-ci-config config 'release-name))]
               [create-release?
                (config-required-boolean 'windows-ci-config config 'create-release)]
               [token-secret
                (assert-windows-ci-safe-token
                 (config-required-string 'windows-ci-config config 'token-secret))]
               [token-expr
                (string-append "${{ secrets." token-secret " }}")])
          f"
  publish-windows-portable:
    needs: build-windows-portable
    if: github.ref == 'refs/heads/main'
    runs-on: ubuntu-24.04
    permissions:
      actions: read
      contents: write
    steps:
      - name: Download Windows portable artifact
        uses: actions/download-artifact@v6
        with:
          name: {(yaml-single-quote (windows-ci-artifact-name config))}
          path: release-assets

      - name: Publish Windows release assets
        shell: bash
        env:
          GH_TOKEN: {token-expr}
          RELEASE_REPO: {(yaml-single-quote release-repo)}
          RELEASE_TAG: {(yaml-single-quote release-tag)}
          RELEASE_NAME: {(yaml-single-quote release-name)}
          CREATE_RELEASE: {(yaml-single-quote (if create-release? "true" "false"))}
        run: |
          set -euo pipefail
          command -v gh >/dev/null 2>&1 || {{ echo 'gh CLI is required on the publish runner'; exit 1; }}
          mapfile -t zip_files < <(find \"$GITHUB_WORKSPACE/release-assets\" -maxdepth 1 -name '*.zip' -type f | sort)
          mapfile -t exe_files < <(find \"$GITHUB_WORKSPACE/release-assets\" -maxdepth 1 -name '*.exe' -type f | sort)
          if [ \"${{#zip_files[@]}}\" -ne 1 ]; then
            printf 'Expected exactly one Windows portable zip, got %s\\n' \"${{#zip_files[@]}}\"
            printf '  %s\\n' \"${{zip_files[@]}}\"
            exit 1
          fi
          if [ \"${{#exe_files[@]}}\" -ne 1 ]; then
            printf 'Expected exactly one Windows installer exe, got %s\\n' \"${{#exe_files[@]}}\"
            printf '  %s\\n' \"${{exe_files[@]}}\"
            exit 1
          fi
          if ! gh release view \"$RELEASE_TAG\" -R \"$RELEASE_REPO\" >/dev/null 2>&1; then
            if [ \"$CREATE_RELEASE\" != true ]; then
              echo \"GitHub release does not exist and create-release is false: $RELEASE_REPO $RELEASE_TAG\"
              exit 1
            fi
            gh release create \"$RELEASE_TAG\" -R \"$RELEASE_REPO\" --title \"$RELEASE_NAME\" --notes \"Generated Windows Racket artifacts.\"
          fi
          gh release upload \"$RELEASE_TAG\" -R \"$RELEASE_REPO\" \"${{zip_files[0]}}\" \"${{exe_files[0]}}\" --clobber
")
        "")
  ) ; end begin windows-ci-publish-job-content
) ; end define windows-ci-publish-job-content

(define (windows-ci-workflow-content c config)
  (begin
    (define runner
      (assert-windows-ci-runner (config-required-string 'windows-ci-config config 'runner)))
    (define arch
      (assert-windows-ci-arch (config-required-string 'windows-ci-config config 'arch)))
    (define msvc-arch
      (assert-windows-ci-msvc-arch (config-required-string 'windows-ci-config config 'msvc-arch)))
    (define nmake-target
      (assert-windows-ci-nmake-target (config-required-string 'windows-ci-config config 'nmake-target)))
    (define portable-dir
      (assert-windows-ci-portable-dir (config-required-string 'windows-ci-config config 'portable-dir-name)))
    (define jobs
      (required-config-positive-integer config 'build-jobs))
    (define source-url (formula-source-url c))
    (define source-sha
      (resolve-source-archive-sha256! 'windows-ci-workflow-content
                                      c
                                      source-url
                                      "Windows source archive"))
    (define zip-name (windows-ci-portable-zip-name c config))
    (define exe-name (windows-ci-installer-exe-name c config))
    f"{(generated-windows-ci-code-notice "#")}name: windows portable build

on:
  push:
    branches:
      - main
  workflow_dispatch:

permissions:
  contents: read

jobs:
  build-windows-portable:
    name: build windows {arch}
    runs-on: {runner}
    permissions:
      contents: read
    env:
      PACKAGE_NAME: {(yaml-single-quote (cfg-package-name c))}
      SOURCE_VERSION: {(yaml-single-quote (cfg-source-version c))}
      FORMULA_VERSION: {(yaml-single-quote (cfg-formula-version c))}
      SOURCE_URL: {(yaml-single-quote source-url)}
      SOURCE_SHA256: {(yaml-single-quote source-sha)}
      ZIP_NAME: {(yaml-single-quote zip-name)}
      EXE_NAME: {(yaml-single-quote exe-name)}
      PORTABLE_DIR: {(yaml-single-quote portable-dir)}
      BUILD_JOBS: {(yaml-single-quote (number->string jobs))}
      MSVC_ARCH: {(yaml-single-quote msvc-arch)}
      NMAKE_TARGET: {(yaml-single-quote nmake-target)}
    steps:
      - name: Checkout packaging workflow
        uses: actions/checkout@v6

      - name: Locate Visual Studio build tools
        shell: pwsh
        run: |
          $programFilesX86 = [Environment]::GetFolderPath(\"ProgramFilesX86\")
          $vswhere = Join-Path $programFilesX86 \"Microsoft Visual Studio\\Installer\\vswhere.exe\"
          if (!(Test-Path $vswhere)) {{
            throw \"vswhere.exe not found: $vswhere\"
          }}
          $vs = & $vswhere -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
          if (!$vs) {{
            throw \"Visual Studio with VC tools was not found\"
          }}
          $vcvarsall = Join-Path $vs \"VC\\Auxiliary\\Build\\vcvarsall.bat\"
          if (!(Test-Path $vcvarsall)) {{
            throw \"vcvarsall.bat not found: $vcvarsall\"
          }}
          \"VCVARSALL_BAT=$vcvarsall\" | Out-File -FilePath $env:GITHUB_ENV -Encoding utf8 -Append

      - name: Download and verify source archive
        shell: pwsh
        run: |
          Remove-Item -Recurse -Force source -ErrorAction SilentlyContinue
          New-Item -ItemType Directory -Force source | Out-Null
          Invoke-WebRequest -Uri $env:SOURCE_URL -OutFile source.tgz
          $actual = (Get-FileHash source.tgz -Algorithm SHA256).Hash.ToLowerInvariant()
          if ($actual -ne $env:SOURCE_SHA256) {{
            throw \"Source sha256 mismatch: expected $env:SOURCE_SHA256 got $actual\"
          }}
          tar -xzf source.tgz -C source
          $entries = @(Get-ChildItem source -Directory)
          if ($entries.Count -ne 1) {{
            Get-ChildItem source | Format-Table -AutoSize
            throw \"source archive must contain exactly one top-level directory\"
          }}
          \"SRC_DIR=$($entries[0].FullName)\" | Out-File -FilePath $env:GITHUB_ENV -Encoding utf8 -Append

      - name: Build Racket with nmake
        shell: pwsh
        run: |
          $buildScript = @\"
          @echo on
          call \"%VCVARSALL_BAT%\" %MSVC_ARCH%
          cd /d \"%SRC_DIR%\"
          if exist Makefile (
            echo Reusing existing top-level Makefile in %CD%
          ) else if exist src\\winfig.bat (
            dir src\\winfig.bat
            dir src\\Makefile.nt
            dir src\\buildmain.zuo
            call src\\winfig.bat
            if not exist Makefile (
              echo winfig.bat did not create Makefile in %CD%
              dir
              exit /b 1
            )
          ) else (
            dir
            echo Neither top-level Makefile nor src\\winfig.bat exists in %SRC_DIR%
            exit /b 1
          )
          for %%D in (bin lib share include etc) do (
            if exist \"%%D\" (
              if not exist \"%%D\\\" (
                echo Expected install layout directory but found non-directory: %%D
                exit /b 1
              )
            ) else (
              mkdir \"%%D\"
            )
          )
          dir lib
          call :RunNmake all
          if errorlevel 1 exit /b %errorlevel%
          if /I \"%NMAKE_TARGET%\"==\"all\" goto after_nmake
          call :RunNmake %NMAKE_TARGET%
          if errorlevel 1 exit /b %errorlevel%
          goto after_nmake
          :RunNmake
          echo Running nmake target %*
          nmake /f Makefile %* JOBS=%BUILD_JOBS%
          exit /b %errorlevel%
          :after_nmake
          \"@
          Set-Content -Path build-racket.cmd -Value $buildScript -Encoding ASCII
          cmd.exe /c build-racket.cmd
          if ($LASTEXITCODE -ne 0) {{
            if ($env:SRC_DIR) {{
              Write-Host \"Source root after failed nmake:\"
              Get-ChildItem $env:SRC_DIR -Force | Select-Object -First 80 Mode, Length, FullName
              foreach ($relative in @(\"bin\", \"lib\", \"share\", \"include\", \"etc\", \"cs\\c\", \"cs\\c\\lib\")) {{
                $candidate = Join-Path $env:SRC_DIR $relative
                if (Test-Path $candidate) {{
                  Write-Host \"Directory snapshot: $candidate\"
                  Get-ChildItem $candidate -Force | Select-Object -First 80 Mode, Length, FullName
                }} else {{
                  Write-Host \"Missing directory snapshot target: $candidate\"
                }}
              }}
              Write-Host \"Known Racket CS DLL files after failed nmake:\"
              Get-ChildItem $env:SRC_DIR -Recurse -Filter libracketcs*.dll -ErrorAction SilentlyContinue |
                Select-Object -First 80 Mode, Length, FullName
            }}
            throw \"nmake build failed with exit $LASTEXITCODE\"
          }}

      - name: Install Inno Setup
        shell: pwsh
        run: |
          choco install innosetup --no-progress --yes
          $programFilesX86 = [Environment]::GetFolderPath(\"ProgramFilesX86\")
          $iscc = Join-Path $programFilesX86 \"Inno Setup 6\\ISCC.exe\"
          if (!(Test-Path $iscc)) {{
            $iscc = Get-ChildItem $programFilesX86 -Recurse -Filter ISCC.exe -ErrorAction SilentlyContinue |
              Sort-Object FullName |
              Select-Object -First 1 -ExpandProperty FullName
          }}
          if (!$iscc -or !(Test-Path $iscc)) {{
            throw \"ISCC.exe not found after installing Inno Setup\"
          }}
          \"ISCC_EXE=$iscc\" | Out-File -FilePath $env:GITHUB_ENV -Encoding utf8 -Append

      - name: Assemble portable zip
        shell: pwsh
        run: |
          Remove-Item -Recurse -Force portable, artifacts -ErrorAction SilentlyContinue
          New-Item -ItemType Directory -Force portable, artifacts | Out-Null
          $portableRoot = Join-Path \"portable\" $env:PORTABLE_DIR
          New-Item -ItemType Directory -Force $portableRoot | Out-Null
          foreach ($name in @(\"collects\", \"etc\", \"lib\", \"share\", \"include\", \"doc\")) {{
            $source = Join-Path $env:SRC_DIR $name
            if (Test-Path $source) {{
              Copy-Item -LiteralPath $source -Destination (Join-Path $portableRoot $name) -Recurse -Force
            }} else {{
              Write-Host \"Skipping absent optional runtime directory: $source\"
            }}
          }}
          Get-ChildItem -LiteralPath $env:SRC_DIR -File |
            Where-Object {{ $_.Name -match '\\.(exe|dll|def)$' -or $_.Name -eq \"README\" }} |
            ForEach-Object {{ Copy-Item -LiteralPath $_.FullName -Destination $portableRoot -Force }}
          foreach ($required in @(\"collects\", \"etc\", \"lib\", \"share\")) {{
            $requiredPath = Join-Path $portableRoot $required
            if (!(Test-Path $requiredPath)) {{
              Get-ChildItem -LiteralPath $env:SRC_DIR -Force | Select-Object -First 120 Mode, Length, FullName
              throw \"required portable runtime path missing: $requiredPath\"
            }}
          }}
          $racketExe = Get-ChildItem $portableRoot -Recurse -Filter Racket.exe | Sort-Object FullName | Select-Object -First 1
          $racoExe = Get-ChildItem $portableRoot -Recurse -Filter raco.exe | Sort-Object FullName | Select-Object -First 1
          if (!$racketExe) {{
            Write-Host \"Source root files:\"
            Get-ChildItem -LiteralPath $env:SRC_DIR -Force | Select-Object -First 120 Mode, Length, FullName
            Write-Host \"Portable files:\"
            Get-ChildItem $portableRoot -Recurse -File | Select-Object -First 120 FullName
            throw \"racket.exe not found in portable tree\"
          }}
          if ($racoExe) {{
            $racoCommand = $racoExe.FullName
            $racoArgs = @(\"pkg\", \"show\", \"--all\")
          }} else {{
            $racoCmd = Join-Path $racketExe.DirectoryName \"raco.cmd\"
            @(
              \"@echo off\",
              \"`\"%~dp0$($racketExe.Name)`\" -N raco -l- raco %*\"
            ) | Set-Content -Path $racoCmd -Encoding ASCII
            $racoCommand = $racketExe.FullName
            $racoArgs = @(\"-N\", \"raco\", \"-l-\", \"raco\", \"pkg\", \"show\", \"--all\")
          }}
          & $racketExe.FullName -e '(displayln (version))' | Tee-Object -Variable versionOut
          if ($versionOut -notmatch [regex]::Escape($env:SOURCE_VERSION)) {{
            throw \"Racket version output does not include $env:SOURCE_VERSION: $versionOut\"
          }}
          & $racketExe.FullName -e '(displayln \"windows-portable-ok\")'
          if ($LASTEXITCODE -ne 0) {{
            throw \"Racket smoke check failed with exit $LASTEXITCODE\"
          }}
          & $racoCommand @racoArgs | Select-Object -First 40
          if ($LASTEXITCODE -ne 0) {{
            Write-Warning \"raco package listing failed with exit $LASTEXITCODE; keeping portable artifact because Racket smoke check passed\"
            $global:LASTEXITCODE = 0
          }}
          @(
            \"Racket portable build\",
            \"Source: $env:SOURCE_URL\",
            \"Version: $env:SOURCE_VERSION\",
            \"Package version: $env:FORMULA_VERSION\",
            \"Find Racket.exe inside this directory. If raco.exe is absent, use raco.cmd next to Racket.exe.\"
          ) | Set-Content -Path (Join-Path $portableRoot \"README-portable.txt\") -Encoding UTF8
          $artifactPath = Join-Path \"artifacts\" $env:ZIP_NAME
          Compress-Archive -Path $portableRoot -DestinationPath $artifactPath -Force
          if ((Get-Item $artifactPath).Length -le 0) {{
            throw \"portable zip is empty: $artifactPath\"
          }}
          Get-FileHash $artifactPath -Algorithm SHA256

      - name: Build Inno Setup installer
        shell: pwsh
        run: |
          $portableRoot = Join-Path \"portable\" $env:PORTABLE_DIR
          if (!(Test-Path $portableRoot)) {{
            throw \"portable tree missing: $portableRoot\"
          }}
          @'
          param(
            [Parameter(Mandatory=$true)] [string] $InstallRoot,
            [Parameter(Mandatory=$true)] [string] $CacheRoot
          )

          $ErrorActionPreference = 'Stop'

          function Test-DirectoryEmpty([string] $Path) {{
            if (!(Test-Path -LiteralPath $Path)) {{ return $true }}
            return -not (Get-ChildItem -LiteralPath $Path -Force -ErrorAction Stop | Select-Object -First 1)
          }}

          $CacheRoot = [IO.Path]::GetFullPath($CacheRoot)
          $root = [IO.Path]::GetPathRoot($CacheRoot).TrimEnd('\\')
          if ([string]::IsNullOrWhiteSpace($CacheRoot) -or ($CacheRoot.TrimEnd('\\') -eq $root)) {{
            throw \"Refusing unsafe cache path: $CacheRoot\"
          }}

          $marker = Join-Path $CacheRoot '.racket-installer-cache'
          if ((Test-Path -LiteralPath $CacheRoot) -and
              !(Test-Path -LiteralPath $marker) -and
              !(Test-DirectoryEmpty $CacheRoot)) {{
            throw \"Cache path exists and is not empty or installer-owned: $CacheRoot\"
          }}

          New-Item -ItemType Directory -Force $CacheRoot | Out-Null
          Set-Content -LiteralPath $marker -Value 'Racket installer owned cache directory' -Encoding ASCII

          $configPath = Join-Path $InstallRoot 'etc\\config.rktd'
          if (!(Test-Path -LiteralPath $configPath)) {{
            throw \"Racket config not found: $configPath\"
          }}
          $configText = Get-Content -LiteralPath $configPath -Raw
          foreach ($key in @('default-scope', 'compiled-file-cache-roots', 'compiled-file-system-cache-root')) {{
            $pattern = '\\s*\\(' + [regex]::Escape($key) + '\\s+\\.\\s+(\"[^\"]*\"|\\([^)]*\\)|[^)]*)\\)'
            $configText = [regex]::Replace($configText, $pattern, '')
          }}
          $cacheForConfig = $CacheRoot.Replace('\\', '/').Replace('\"', '\\\"')
          $entries = ' (default-scope . \"installation\") (compiled-file-cache-roots . (user system)) (compiled-file-system-cache-root . \"' + $cacheForConfig + '\")'
          $configText = [regex]::Replace($configText, '\\s*\\)\\)\\s*$', $entries + \"))`r`n\")
          if ($configText -notmatch '\\(compiled-file-system-cache-root \\.' ) {{
            throw 'Unable to configure Racket system cache root'
          }}
          Set-Content -LiteralPath $configPath -Value $configText -Encoding UTF8

          $racoExe = Join-Path $InstallRoot 'raco.exe'
          $racketExe = Join-Path $InstallRoot 'Racket.exe'
          if (Test-Path -LiteralPath $racoExe) {{
            & $racoExe setup --system --no-user --reset-cache -D --no-pkg-deps
          }} elseif (Test-Path -LiteralPath $racketExe) {{
            & $racketExe -N raco -l- raco setup --system --no-user --reset-cache -D --no-pkg-deps
          }} else {{
            throw \"Neither raco.exe nor Racket.exe exists in $InstallRoot\"
          }}
          if ($LASTEXITCODE -ne 0) {{
            exit $LASTEXITCODE
          }}
          if (Test-Path -LiteralPath $racketExe) {{
            & $racketExe -N rhombus -l- rhombus/run.rhm -e 'println(\"package-racket-rhombus-cache\")'
            if ($LASTEXITCODE -ne 0) {{
              exit $LASTEXITCODE
            }}
            $rhombusCache = Join-Path $InstallRoot 'share\\racket\\pkgs\\rhombus-lib\\rhombus\\private\\compiled\\ephemeral\\demod'
            if (!(Get-ChildItem -LiteralPath $rhombusCache -Recurse -Filter *.zo -ErrorAction SilentlyContinue | Select-Object -First 1)) {{
              throw \"Rhombus demod cache is empty: $rhombusCache\"
            }}
          }}
          '@ | Set-Content -Path (Join-Path $portableRoot \"installer-configure.ps1\") -Encoding UTF8

          $outputBase = [IO.Path]::GetFileNameWithoutExtension($env:EXE_NAME)
          @\"
          [Setup]
          AppId={(cfg-package-name c)}
          AppName=Racket
          AppVersion=$env:FORMULA_VERSION
          DefaultDirName={{autopf}}\\Racket9
          DefaultGroupName=Racket 9
          DisableProgramGroupPage=yes
          OutputDir=artifacts
          OutputBaseFilename=$outputBase
          Compression=lzma2
          SolidCompression=yes
          ArchitecturesAllowed=x64compatible
          ArchitecturesInstallIn64BitMode=x64compatible
          PrivilegesRequired=admin
          WizardStyle=modern

          [Files]
          Source: \"portable\\$env:PORTABLE_DIR\\*\"; DestDir: \"{{app}}\"; Flags: ignoreversion recursesubdirs createallsubdirs

          [Icons]
          Name: \"{{group}}\\Racket\"; Filename: \"{{app}}\\Racket.exe\"
          Name: \"{{group}}\\Uninstall Racket\"; Filename: \"{{uninstallexe}}\"

          [Code]
          var
            CachePage: TInputDirWizardPage;
            CacheDefaultRoot: String;

          function DefaultCacheRoot(): String;
          begin
            Result := ExpandConstant('{{app}}\\var\\cache\\racket\\compiled');
          end;

          function GetCacheRoot(Param: String): String;
          var
            Value: String;
          begin
            Value := ExpandConstant('{{param:CACHEPATH|}}');
            if Value <> '' then
              Result := Value
            else if Assigned(CachePage) then
              Result := CachePage.Values[0]
            else
              Result := DefaultCacheRoot();
          end;

          procedure InitializeWizard;
          begin
            CachePage := CreateInputDirPage(wpSelectDir, 'Racket Cache Directory',
              'Choose the compiled cache directory.',
              'Racket builds the system cache during installation. For unattended installs, pass /CACHEPATH=...',
              False, '');
            CachePage.Add('');
            CacheDefaultRoot := DefaultCacheRoot();
            CachePage.Values[0] := CacheDefaultRoot;
          end;

          procedure CurPageChanged(CurPageID: Integer);
          begin
            if Assigned(CachePage) and (CurPageID = CachePage.ID) then
              if (CachePage.Values[0] = '') or (CachePage.Values[0] = CacheDefaultRoot) then begin
                CacheDefaultRoot := DefaultCacheRoot();
                CachePage.Values[0] := CacheDefaultRoot;
              end;
          end;

          function NextButtonClick(CurPageID: Integer): Boolean;
          begin
            Result := True;
            if Assigned(CachePage) and (CurPageID = CachePage.ID) and (Trim(CachePage.Values[0]) = '') then begin
              MsgBox('Cache path is required.', mbError, MB_OK);
              Result := False;
            end;
          end;

          procedure CurStepChanged(CurStep: TSetupStep);
          var
            ResultCode: Integer;
            Params: String;
          begin
            if CurStep = ssPostInstall then begin
              if not RegWriteStringValue(HKLM, 'Software\\Racket9', 'CacheRoot', GetCacheRoot('')) then
                RaiseException('Failed to save Racket cache path.');
              Params := '-NoProfile -ExecutionPolicy Bypass -File \"' + ExpandConstant('{{app}}\\installer-configure.ps1') +
                '\" -InstallRoot \"' + ExpandConstant('{{app}}') + '\" -CacheRoot \"' + GetCacheRoot('') + '\"';
              if not Exec('powershell.exe', Params, '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
                RaiseException('Failed to run Racket cache setup.');
              if ResultCode <> 0 then
                RaiseException('Racket cache setup failed with exit code ' + IntToStr(ResultCode) + '.');
            end;
          end;

          procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
          var
            CacheRoot: String;
          begin
            if CurUninstallStep = usUninstall then begin
              if RegQueryStringValue(HKLM, 'Software\\Racket9', 'CacheRoot', CacheRoot) then begin
                if FileExists(AddBackslash(CacheRoot) + '.racket-installer-cache') then
                  DelTree(CacheRoot, True, True, True);
              end;
              RegDeleteKeyIncludingSubkeys(HKLM, 'Software\\Racket9');
            end;
          end;
          \"@ | Set-Content -Path racket-installer.iss -Encoding UTF8

          & $env:ISCC_EXE racket-installer.iss
          if ($LASTEXITCODE -ne 0) {{
            throw \"Inno Setup failed with exit $LASTEXITCODE\"
          }}
          $installerPath = Join-Path \"artifacts\" $env:EXE_NAME
          if (!(Test-Path $installerPath) -or (Get-Item $installerPath).Length -le 0) {{
            throw \"installer exe missing or empty: $installerPath\"
          }}
          Get-FileHash $installerPath -Algorithm SHA256

      - name: Upload Windows artifacts
        uses: actions/upload-artifact@v6
        with:
          name: {(yaml-single-quote (windows-ci-artifact-name config))}
          path: |
            artifacts/*.zip
            artifacts/*.exe
          if-no-files-found: error
{(windows-ci-publish-job-content config)}"
  ) ; end begin windows-ci-workflow-content
) ; end define windows-ci-workflow-content

(define (assert-windows-ci-workflow-replaceable! path)
  (begin
    (when (file-exists? path)
      (define content (file->string path))
      (unless (string-contains? content generated-windows-ci-notice-marker)
        (raise-user-error 'write-windows-ci-workflow!
                          f"refusing to overwrite workflow without generated marker: {(clean-path-string path)}")
      ) ; end unless generated marker present
    ) ; end when workflow exists
  ) ; end begin assert-windows-ci-workflow-replaceable!
) ; end define assert-windows-ci-workflow-replaceable!

(define (assert-windows-ci-readme-replaceable! path)
  (begin
    (when (file-exists? path)
      (define content (file->string path))
      (unless (string-contains? content generated-windows-ci-notice-marker)
        (raise-user-error 'write-windows-ci-readme!
                          f"refusing to overwrite README without generated marker: {(clean-path-string path)}")
      ) ; end unless generated marker present
    ) ; end when README exists
  ) ; end begin assert-windows-ci-readme-replaceable!
) ; end define assert-windows-ci-readme-replaceable!

(define (validate-windows-ci-readme! c config path)
  (begin
    (define content (file->string path))
    (define zip-name (windows-ci-portable-zip-name c config))
    (define exe-name (windows-ci-installer-exe-name c config))
    (for ([needle (in-list (list "# win-racket"
                                 generated-windows-ci-notice-marker
                                 "package-racket"
                                 ".github/workflows/build-windows-portable.yml"
                                 zip-name
                                 exe-name
                                 "/CACHEPATH"))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-windows-ci-readme! f"Windows README missing: {needle}")
      ) ; end unless README contains required text
    ) ; end for README needle
  ) ; end begin validate-windows-ci-readme!
) ; end define validate-windows-ci-readme!

(define (validate-windows-ci-workflow! c config path)
  (begin
    (validate-yaml! c path)
    (define content (file->string path))
    (define zip-name (windows-ci-portable-zip-name c config))
    (define exe-name (windows-ci-installer-exe-name c config))
    (for ([needle (in-list (list "name: windows portable build"
                                 generated-windows-ci-notice-marker
                                 "runs-on:"
                                 "windows-"
                                 "actions/checkout@v6"
                                 "Invoke-WebRequest"
                                 "Get-FileHash source.tgz -Algorithm SHA256"
                                 "src\\winfig.bat"
                                 "nmake /f Makefile"
                                 "Compress-Archive"
                                 "choco install innosetup"
                                 "ISCC.exe"
                                 "Build Inno Setup installer"
	                                 "/CACHEPATH"
	                                 "raco setup --system --no-user --reset-cache -D --no-pkg-deps"
	                                 "package-racket-rhombus-cache"
	                                 "Rhombus demod cache is empty"
	                                 "RegDeleteKeyIncludingSubkeys"
                                 "windows-portable-ok"
                                 "actions/upload-artifact@v6"
                                 zip-name
                                 exe-name))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-windows-ci-workflow! f"Windows CI workflow missing: {needle}")
      ) ; end unless workflow contains required needle
    ) ; end for workflow needle
    (when (config-required-boolean 'windows-ci-config config 'publish-release)
      (for ([needle (in-list '("actions/download-artifact@v6"
                               "gh release upload"
                               "RELEASE_REPO:"))])
        (unless (string-contains? content needle)
          (raise-user-error 'validate-windows-ci-workflow! f"Windows publish workflow missing: {needle}")
        ) ; end unless workflow contains publish needle
      ) ; end for publish needle
    ) ; end when publish release
  ) ; end begin validate-windows-ci-workflow!
) ; end define validate-windows-ci-workflow!

(define (write-windows-ci-workflow! c config)
  (begin
    (assert-windows-ci-repo-root! c #:write? #t)
    (assert-executable 'write-windows-ci-workflow! (cfg-ruby-bin c))
    (define readme-path (windows-ci-readme-path c))
    (define workflow-path (windows-ci-workflow-path c))
    (assert-windows-ci-readme-replaceable! readme-path)
    (assert-windows-ci-workflow-replaceable! workflow-path)
    (write-text-file! readme-path (windows-ci-readme-content c config))
    (validate-windows-ci-readme! c config readme-path)
    (make-directory* (windows-ci-workflows-dir c))
    (write-text-file! workflow-path (windows-ci-workflow-content c config))
    (validate-windows-ci-workflow! c config workflow-path)
    (println/flush f"Generated Windows portable README: {(clean-path-string readme-path)}")
    (println/flush f"Generated Windows portable CI workflow: {(clean-path-string workflow-path)}")
  ) ; end begin write-windows-ci-workflow!
) ; end define write-windows-ci-workflow!

(define (print-windows-ci-dry-run-plan! c config)
  (begin
    (assert-windows-ci-repo-root! c #:write? #f)
    (define readme-path (windows-ci-readme-path c))
    (define workflow-path (windows-ci-workflow-path c))
    (println/flush f"Would read Windows CI config: {(clean-path-string (cfg-windows-ci-config c))}")
    (println/flush f"Would generate Windows portable README: {(clean-path-string readme-path)}")
    (println/flush f"Would generate Windows portable CI workflow: {(clean-path-string workflow-path)}")
    (println/flush f"Would validate Windows CI workflow YAML with: {(cfg-ruby-bin c)}")
    (println/flush f"Would configure Windows runner: {(config-required-string 'windows-ci-config config 'runner)}")
    (println/flush f"Would configure Windows portable zip: {(windows-ci-portable-zip-name c config)}")
    (println/flush f"Would configure Windows Inno installer: {(windows-ci-installer-exe-name c config)}")
    (println/flush f"Would publish Windows release asset: {(if (config-required-boolean 'windows-ci-config config 'publish-release) "yes" "no")}")
  ) ; end begin print-windows-ci-dry-run-plan!
) ; end define print-windows-ci-dry-run-plan!

(define (build-windows-ci! c)
  (begin
    (define config (read-windows-ci-config c))
    (if (cfg-dry-run? c)
        (print-windows-ci-dry-run-plan! c config)
        (write-windows-ci-workflow! c config))
  ) ; end begin build-windows-ci!
) ; end define build-windows-ci!

(define (validate-rpm-repo-metadata! c)
  (begin
    (assert-nonempty-file 'validate-rpm-repo-metadata!
                          (build-path (rpm-repo-arch-root c) "repodata" "repomd.xml"))
    (println/flush f"Validated RPM repo metadata: {(clean-path-string (rpm-repo-arch-root c))}")
  ) ; end begin validate-rpm-repo-metadata!
) ; end define validate-rpm-repo-metadata!

(define (copy-rpm-into-repo! c rpm-path)
  (begin
    (define packages-dir (rpm-repo-packages-dir c))
    (define dest (build-path packages-dir (rpm-package-name c)))
    (make-directory* packages-dir)
    (copy-file rpm-path dest #t)
    (assert-nonempty-file 'copy-rpm-into-repo! dest)
    (unless (string=? (sha256-file rpm-path) (sha256-file dest))
      (raise-user-error 'copy-rpm-into-repo!
                        f"copied RPM sha256 mismatch: {(clean-path-string dest)}")
    ) ; end unless copied rpm sha matches
    (println/flush f"Installed RPM repo package: {(clean-path-string dest)}")
  ) ; end begin copy-rpm-into-repo!
) ; end define copy-rpm-into-repo!

(define (update-rpm-repo! c)
  (begin
    (define produced-by-rpm? (target-selected? c "rpm"))
    (define rpm-name (rpm-package-name c))
    (define rpm-path (rpm-package-path c))
    (define dry-run-planned? (and (cfg-dry-run? c) produced-by-rpm?))
    (assert-rpm-repo-root! c #:write? (not (cfg-dry-run? c)))
    (unless dry-run-planned?
      (validate-rpm! c rpm-path)
    ) ; end unless dry-run planned rpm artifact
    (define local-sha (if dry-run-planned?
                          "<dry-run: artifact not built>"
                          (sha256-file rpm-path)))
    (println/flush f"RPM repo config: {(clean-path-string (cfg-rpm-repo-config c))}")
    (println/flush f"RPM repo root: {(clean-path-string (cfg-rpm-repo-root c))}")
    (println/flush f"RPM repo id: {(cfg-rpm-repo-id c)}")
    (println/flush f"RPM repo baseurl: {(cfg-rpm-repo-baseurl c)}")
    (println/flush f"RPM repo package: {rpm-name}")
    (println/flush f"RPM repo sha256: {local-sha}")
    (if (cfg-dry-run? c)
        (begin
          (println/flush
           (if dry-run-planned?
               f"Would update RPM repo from planned rpm output {(clean-path-string rpm-path)}"
               f"Would update RPM repo from {(clean-path-string rpm-path)}"))
          (println/flush f"Would copy RPM into repo: {(clean-path-string rpm-path)} -> {(clean-path-string (build-path (rpm-repo-packages-dir c) rpm-name))}")
          (println/flush f"Would run createrepo_c --update {(clean-path-string (rpm-repo-arch-root c))}")
        ) ; end begin dry-run rpm repo
        (begin
          (assert-executable 'update-rpm-repo! (cfg-createrepo-bin c))
          (copy-rpm-into-repo! c rpm-path)
          (run! 'update-rpm-repo!
                (cfg-createrepo-bin c)
                (list "--update" (clean-path-string (rpm-repo-arch-root c)))
                #:dry-run? #f)
          (validate-rpm-repo-metadata! c)
        ) ; end begin update rpm repo
    ) ; end if dry-run
  ) ; end begin update-rpm-repo!
) ; end define update-rpm-repo!

(define (build-rpm-repo! c)
  (list (lambda ()
          (update-rpm-repo! c)
        ) ; end lambda update rpm repo
  ) ; end list rpm repo finalizer
) ; end define build-rpm-repo!

(define (brew-work-root c)
  (build-path (cfg-work-dir c) "brew"))

(define (brew-generated-formula c)
  (build-path (brew-work-root c) "Formula" (file-name-from-path (cfg-formula c))))

(define (brew-source-stage-root c)
  (build-path (cfg-stage-dir c) "brew-source"))

(define (brew-source-dist-root c)
  (build-path (brew-source-stage-root c) f"racket-{(cfg-source-version c)}"))

(define (brew-source-tgz-name c)
  f"racket-minimal-{(cfg-source-version c)}-src.tgz")

(define (brew-output-tgz c)
  (build-path (cfg-artifact-dir c) (brew-source-tgz-name c)))

(define generated-code-notice-marker
  "GENERATED CODE - DO NOT EDIT IN homebrew-racket.")

(define generated-source-url
  "https://github.com/CutieDeng/package-racket")

(define (generated-source-root)
  generated-source-url)

(define (generated-code-notice comment-prefix)
  f"{comment-prefix} {generated-code-notice-marker}
{comment-prefix} Source of truth: {(generated-source-root)}
{comment-prefix} Humans and LLM agents must change package-racket and regenerate; manual tap edits are not production-safe.

")

(define (generated-code-notice-rx comment-prefix)
  (pregexp
   (string-append
    "^"
    (regexp-quote (string-append comment-prefix " " generated-code-notice-marker "\n"))
    (regexp-quote (string-append comment-prefix " Source of truth: "))
    "[^\n]*\n"
    (regexp-quote (string-append comment-prefix " Humans and LLM agents must change package-racket and regenerate; manual tap edits are not production-safe.\n\n")))))

(define (ensure-generated-code-notice! who path comment-prefix)
  (begin
    (assert-nonempty-file who path)
    (define content (file->string path))
    (define stripped
      (regexp-replace (generated-code-notice-rx comment-prefix) content ""))
    (define normalized
      (string-append (generated-code-notice comment-prefix) stripped))
    (unless (string=? content normalized)
      (write-text-file! path normalized)
    ) ; end unless notice normalized
  ) ; end begin ensure-generated-code-notice!
) ; end define ensure-generated-code-notice!

(define brew-custom-core-packages
  '("sandbox-lib"
    "errortrace-lib"
    "source-syntax"))

(define brew-doc-packages
  '("racket-index"
    "scribble-lib"
    "scribble-html-lib"
    "net-lib"
    "srfi-lite-lib"
    "compatibility-lib"
    "planet-lib"
    "draw-lib"))

(define brew-default-packages
  '("base"
    "racket-lib"
    "racket-tstring"
    "tstring"
    "xrepl-lib"
    "expeditor-lib"
    "readline-lib"
    "scribble-text-lib"
    "syntax-color-lib"
    "parser-tools-lib"
    "option-contract-lib"
    "scheme-lib"
    "at-exp-lib"
    "pretty-expressive-lib"
    "shrubbery-lib"
    "enforest-lib"
    "rhombus-lib"
    "rhombus-exe"
    "rackunit-lib"
    "testing-util-lib"
    "sandbox-lib"
    "errortrace-lib"
    "source-syntax"
    "compiler-lib"
    "zo-lib"))

(define brew-package-links
  '(("base" . root)
    ("racket-lib" . root)
    ("racket-tstring" . "racket-tstring")
    ("tstring" . "tstring")
    ("xrepl-lib" . root)
    ("expeditor-lib" . "expeditor")
    ("readline-lib" . root)
    ("scribble-text-lib" . root)
    ("syntax-color-lib" . root)
    ("parser-tools-lib" . root)
    ("option-contract-lib" . root)
    ("scheme-lib" . root)
    ("at-exp-lib" . root)
    ("pretty-expressive-lib" . "pretty-expressive")
    ("shrubbery-lib" . root)
    ("enforest-lib" . root)
    ("rhombus-lib" . root)
    ("rhombus-exe" . root)
    ("rackunit-lib" . root)
    ("testing-util-lib" . root)
    ("sandbox-lib" . root)
    ("errortrace-lib" . root)
    ("source-syntax" . "syntax")
    ("compiler-lib" . root)
    ("zo-lib" . root)
    ("racket-index" . root)
    ("scribble-lib" . root)
    ("scribble-html-lib" . root)
    ("net-lib" . root)
    ("srfi-lite-lib" . root)
    ("compatibility-lib" . root)
    ("planet-lib" . root)
    ("draw-lib" . root)))

(define brew-core-required-package-files
  '("share/pkgs/sandbox-lib/racket/sandbox.rkt"
    "share/pkgs/sandbox-lib/scheme/sandbox.rkt"
    "share/pkgs/errortrace-lib/errortrace/stacktrace.rkt"
    "share/pkgs/source-syntax/source-syntax.rkt"
    "share/pkgs/at-exp-lib/at-exp/lang/reader.rkt"
    "share/pkgs/at-exp-lib/scribble/reader.rkt"
    "share/pkgs/at-exp-lib/scribble/base/reader.rkt"
    "share/pkgs/pretty-expressive-lib/main.rkt"
    "share/pkgs/shrubbery-lib/shrubbery/main.rkt"
    "share/pkgs/enforest-lib/enforest/main.rkt"
    "share/pkgs/rhombus-lib/rhombus/reader.rkt"
    "share/pkgs/rhombus-lib/rhombus/main.rkt"
    "share/pkgs/rhombus-lib/rhombus/private/amalgam/srcloc.rkt"
    "share/pkgs/rhombus-exe/rhombus/run.rhm"))

(define brew-doc-required-package-files
  '("share/pkgs/racket-index/help/info.rkt"
    "share/pkgs/racket-index/help/private/command.rkt"
    "share/pkgs/racket-index/help/private/search.rkt"
    "share/pkgs/scribble-lib/scribble/xref.rkt"
    "share/pkgs/scribble-html-lib/scribble/html.rkt"
    "share/pkgs/net-lib/net/sendurl.rkt"
    "share/pkgs/draw-lib/racket/draw/gif.rkt"))

(define brew-core-required-link-needles
  '("root (#\"pkgs\" #\"sandbox-lib\")"
    "root (#\"pkgs\" #\"errortrace-lib\")"
    "\"syntax\" (#\"pkgs\" #\"source-syntax\")"
    "root (#\"pkgs\" #\"at-exp-lib\")"
    "\"pretty-expressive\" (#\"pkgs\" #\"pretty-expressive-lib\")"
    "root (#\"pkgs\" #\"shrubbery-lib\")"
    "root (#\"pkgs\" #\"enforest-lib\")"
    "root (#\"pkgs\" #\"rhombus-lib\")"
    "root (#\"pkgs\" #\"rhombus-exe\")"))

(define brew-doc-required-link-needles
  '("root (#\"pkgs\" #\"racket-index\")"
    "root (#\"pkgs\" #\"scribble-lib\")"
    "root (#\"pkgs\" #\"scribble-html-lib\")"
    "root (#\"pkgs\" #\"net-lib\")"
    "root (#\"pkgs\" #\"draw-lib\")"))

(define brew-core-required-pkgs-db-needles
  '("\"sandbox-lib\""
    "\"errortrace-lib\""
    "\"source-syntax\""
    "\"at-exp-lib\""
    "\"pretty-expressive-lib\""
    "\"shrubbery-lib\""
    "\"enforest-lib\""
    "\"rhombus-lib\""
    "\"rhombus-exe\""))

(define brew-doc-required-pkgs-db-needles
  '("\"racket-index\""
    "\"scribble-lib\""
    "\"scribble-html-lib\""
    "\"net-lib\""
    "\"draw-lib\""))

(define brew-racket-lib-excluded-dependency
  "(\"racket-aarch64-macosx-4\" #:platform \"aarch64-macosx\")")

(define brew-racket-lib-excluded-dependency-line
  f"    {brew-racket-lib-excluded-dependency}
")

(define brew-draw-lib-excluded-dependencies
  '("(\"draw-i386-macosx-3\" #:platform \"i386-macosx\")"
    "(\"draw-x86_64-macosx-3\" #:platform \"x86_64-macosx\")"
    "(\"draw-ppc-macosx-3\" #:platform \"ppc-macosx\")"
    "(\"draw-aarch64-macosx-3\" #:platform \"aarch64-macosx\")"
    "(\"draw-win32-i386-3\" #:platform \"win32\\\\i386\")"
    "(\"draw-win32-x86_64-3\" #:platform \"win32\\\\x86_64\")"
    "(\"draw-win32-arm64-3\" #:platform \"win32\\\\arm64\")"
    "(\"draw-x86_64-linux-natipkg-3\" #:platform \"x86_64-linux-natipkg\")"
    "(\"draw-x11-x86_64-linux-natipkg\" #:platform \"x86_64-linux-natipkg\")"
    "(\"draw-ttf-x86_64-linux-natipkg\" #:platform \"x86_64-linux-natipkg\")"
    "(\"draw-aarch64-linux-natipkg-3\" #:platform \"aarch64-linux-natipkg\")"
    "(\"draw-x11-aarch64-linux-natipkg\" #:platform \"aarch64-linux-natipkg\")"
    "(\"draw-ttf-aarch64-linux-natipkg\" #:platform \"aarch64-linux-natipkg\")"))

(define (brew-source-packages c)
  (remove-duplicates
   (append brew-default-packages
           brew-custom-core-packages
           (if (cfg-with-docs? c) brew-doc-packages '())
           (cfg-brew-packages c))
   string=?))

(define (brew-required-package-files c)
  (append brew-core-required-package-files
          (if (cfg-with-docs? c) brew-doc-required-package-files '())))

(define (brew-required-link-needles c)
  (append brew-core-required-link-needles
          (if (cfg-with-docs? c) brew-doc-required-link-needles '())))

(define (brew-required-pkgs-db-needles c)
  (append brew-core-required-pkgs-db-needles
          (if (cfg-with-docs? c) brew-doc-required-pkgs-db-needles '())))

(define (release-catalog-url version)
  (match (regexp-match #rx"^([0-9]+)[.]([0-9]+)" version)
    [(list _ major minor)
     f"https://download.racket-lang.org/releases/{major}.{minor}/catalog/"]
    [_ (raise-user-error 'release-catalog-url
                         f"cannot derive release catalog from version: {version}")]
  ) ; end match version
) ; end define release-catalog-url

(define (write-brew-source-readme! dest version)
  (write-text-file!
   dest
   f"The Racket Programming Language
===============================

This is the
  Minimal Racket | All Platforms | Source
distribution for version {version}.

This distribution provides source for the Racket run-time system;
for build and installation instructions, see \"src/README.txt\".
(The distribution also includes the core Racket collections and any
installed packages in source form.)

The distribution has been configured so that when you install or
update packages, the package catalogs at
  {(release-catalog-url version)}
  https://download.rhombus-lang.org/releases/current/catalog/
are consulted first.

Visit http://racket-lang.org/ for more Racket resources.


License
-------

Racket is distributed under the MIT license and the Apache version 2.0
license, at your option.

The Racket runtime system includes components distributed under
other licenses. See \"src/LICENSE.txt\" for more information.

Racket packages that are included in the distribution have their own
licenses. See the package files in \"pkgs\" within \"share\" for more
information.
")
) ; end define write-brew-source-readme!

(define (write-brew-config! dest version)
  (begin
    (define catalogs
      (list (release-catalog-url version)
            "https://download.rhombus-lang.org/releases/current/catalog/"
            #f))
    (call-with-output-file dest
      #:exists 'truncate/replace
      (lambda (out)
        (write `#hash((catalogs . ,catalogs)
                      (gui-interactive-file . racket/gui/interactive)
                      (installation-name . ,version)
                      (pkg-catalog-lookup-version . ,(catalog-lookup-version version))
                      (interactive-file . racket/interactive/tstring))
               out)
        (newline out)
      ) ; end lambda out
    ) ; end call-with-output-file
  ) ; end begin write-brew-config!
) ; end define write-brew-config!

(define (skip-brew-source-path? rel)
  (begin
    (define elems (map path->string (explode-path rel)))
    (define base (if (null? elems) "" (last elems)))
    (or (for/or ([elem (in-list elems)])
          (member elem '(".git" ".hg" ".svn" ".github" "compiled"))
        ) ; end for/or ignored path element
        (member base '(".gitattributes" ".gitignore"))
        (string-prefix? base ".LOCK")
        (member base '(".DS_Store" "_zuo.db" "_zuo_tc.db"))
        (regexp-match? #rx"[.]zo$" base)
        (regexp-match? #rx"[.]dep$" base)
        (regexp-match? #rx"[.]bak$" base)
        (regexp-match? #rx"[.]orig$" base)
        (regexp-match? #rx"[.]rej$" base)
        (regexp-match? #rx"~$" base))
  ) ; end begin skip-brew-source-path?
) ; end define skip-brew-source-path?

(define (copy-brew-tree! src dest #:skip-first-components [skip-first-components '()])
  (begin
    (assert-directory 'copy-brew-tree! src)
    (when (directory-exists? dest)
      (delete-directory/files dest)
    ) ; end when old dest exists
    (make-directory* dest)
    (define src/ (path->directory-path src))
    (for ([path (in-list (sort (find-files (lambda (_) #t) src)
                               path<?
                               #:key (lambda (p) (find-relative-path src/ p))))])
      (unless (equal? (simplify-path path) (simplify-path src))
        (define rel (find-relative-path src/ path))
        (define rel-elems (map path->string (explode-path rel)))
        (unless (or (skip-brew-source-path? rel)
                    (and (pair? rel-elems)
                         (member (car rel-elems) skip-first-components)))
          (define target (build-path dest rel))
          (cond
            [(directory-exists? path)
             (make-directory* target)
             (file-or-directory-permissions target (file-or-directory-permissions path 'bits))]
            [(file-exists? path)
             (make-directory* (path-only target))
             (copy-file path target #t)
             (file-or-directory-permissions target (file-or-directory-permissions path 'bits))]
            [(link-exists? path)
             (make-directory* (path-only target))
             (copy-file path target #t)]
          ) ; end cond source path kind
        ) ; end unless skipped path
      ) ; end unless root path
    ) ; end for source path
  ) ; end begin copy-brew-tree!
) ; end define copy-brew-tree!

(define brew-source-pruned-source-dirs
  '(("ChezScheme" "csug")
    ("ChezScheme" "release_notes")
    ("ChezScheme" "nanopass" "doc")
    ("ChezScheme" "stex" "doc")
    ("ChezScheme" "mats")
    ("ChezScheme" "examples")))

(define (replace-exact-source! who path old new)
  (begin
    (assert-file who path)
    (define content (file->string path))
    (define old-rx (regexp (regexp-quote old)))
    (define count (regexp-match-count old-rx content))
    (unless (= count 1)
      (raise-user-error who
                        f"expected exactly one source patch match in {(clean-path-string path)}, found {count}")
    ) ; end unless exactly one match
    (write-text-file! path (regexp-replace old-rx content new))
  ) ; end begin replace-exact-source!
) ; end define replace-exact-source!

(define (patch-brew-chez-minimal-build! src-dir)
  (begin
    (define chez-dir (build-path src-dir "ChezScheme"))
    (replace-exact-source!
     'patch-brew-chez-minimal-build!
     (build-path chez-dir "build.zuo")
     "  (define bounce-dirs
    '(\"c\" \"s\" \"mats\" \"examples\"))"
     "  (define bounce-dirs
    '(\"c\" \"s\"))")
    (replace-exact-source!
     'patch-brew-chez-minimal-build!
     (build-path chez-dir "build.zuo")
     "  (define (cross-build-boot/safe+examples token args)
    (cross-build-boot token args (hash 'o \"2\" 'd \"3\" 'i \"t\") '(\"all\" \"examples\") #t))"
     "  (define (cross-build-boot/safe+examples token args)
    (cross-build-boot token args (hash 'o \"2\" 'd \"3\" 'i \"t\") '(\"all\") #t))")
    (replace-exact-source!
     'patch-brew-chez-minimal-build!
     (build-path chez-dir "s" "build.zuo")
     "(require \"../makefiles/lib.zuo\"
         \"machine.zuo\"
         (only-in \"../examples/build.zuo\"
                  [targets-at examples-targets-at]))"
     "(require \"../makefiles/lib.zuo\"
         \"machine.zuo\")")
    (replace-exact-source!
     'patch-brew-chez-minimal-build!
     (build-path chez-dir "s" "build.zuo")
     "       [:target examples ()
                ,(lambda (token)
                   (mkdir-p (at-dir \"../examples\"))
                   (build (find-target \"all\" (examples-targets-at (make-at-dir (at-dir \"../examples\")) vars))
                          token))]"
     "       [:target examples ()
                ,(lambda (token)
                   (error \"examples target is not included in the minimal source archive\"))]")
    (replace-exact-source!
     'patch-brew-chez-minimal-build!
     (build-path chez-dir "makefiles" "install.zuo")
     "    (apply I (list* \"-m\" \"444\" (append
                                (map (lambda (n) (at-source* \"../examples\" n))
                                     (ls (at-source \"../examples\")))
                                (list LibExamples)))))"
     "    (when (directory-exists? (at-source \"../examples\"))
      (apply I (list* \"-m\" \"444\" (append
                                  (map (lambda (n) (at-source* \"../examples\" n))
                                       (ls (at-source \"../examples\")))
                                  (list LibExamples))))))")
    (replace-exact-source!
     'patch-brew-chez-minimal-build!
     (build-path chez-dir "makefiles" "bintar.zuo")
     "  (immediate \"examples\")"
     "  (when (directory-exists? (at-source \"../examples\"))
    (immediate \"examples\"))")
  ) ; end begin patch-brew-chez-minimal-build!
) ; end define patch-brew-chez-minimal-build!

(define (prune-brew-source-assets! src-dir)
  (begin
    (for ([rel (in-list brew-source-pruned-source-dirs)])
      (define dir (apply build-path src-dir rel))
      (when (directory-exists? dir)
        (delete-directory/files dir))
    ) ; end for pruned source dir
  ) ; end begin prune-brew-source-assets!
) ; end define prune-brew-source-assets!

(define (brew-package-source racket-root name)
  (or (for/or ([candidate (in-list (list (build-path racket-root "pkgs" name)
                                         (build-path racket-root "racket" "share" "pkgs" name)))])
        (and (directory-exists? candidate) candidate)
      ) ; end for/or candidates
      (raise-user-error 'brew-package-source
                        f"cannot find package {name} in {(clean-path-string racket-root)}/pkgs or {(clean-path-string racket-root)}/racket/share/pkgs")))

(define (datum->source v)
  (call-with-output-string (lambda (out) (write v out))))

(define (brew-package-link-name name)
  (begin
    (define pair (assoc name brew-package-links))
    (unless pair
      (raise-user-error 'brew-package-link-name
                        f"package has no explicit collection link mapping: {name}")
    ) ; end unless known package link
    (cdr pair)
  ) ; end begin brew-package-link-name
) ; end define brew-package-link-name

(define (write-brew-links! dest packages)
  (begin
    (define entries
      (for/list ([name (in-list packages)])
        (define link-name (brew-package-link-name name))
        (cond
          [(eq? link-name 'root)
           `(root (#"pkgs" ,(string->bytes/utf-8 name)))]
          [else
           `(,link-name (#"pkgs" ,(string->bytes/utf-8 name)))]
        ) ; end cond link entry
      ) ; end for/list entries
    ) ; end define entries
    (call-with-output-file dest
      #:exists 'truncate/replace
      (lambda (out)
        (write entries out)
        (newline out)
      ) ; end lambda out
    ) ; end call-with-output-file
  ) ; end begin write-brew-links!
) ; end define write-brew-links!

(define (brew-sc-pkg? name)
  (member name '("racket-tstring" "tstring")))

(define (brew-auto-pkg? name)
  (not (member name '("racket-lib" "tstring"))))

(define (brew-pkg-info-value name)
  (begin
    (define auto? (brew-auto-pkg? name))
    (cond
      [(brew-sc-pkg? name)
       f"#s((sc-pkg-info pkg-info 3) (catalog {(datum->source name)}) \"\" {(if auto? "#t" "#f")} {(datum->source name)})"]
      [else
       f"#s(pkg-info (catalog {(datum->source name)}) \"\" {(if auto? "#t" "#f")})"]
    ) ; end cond pkg info kind
  ) ; end begin brew-pkg-info-value
) ; end define brew-pkg-info-value

(define (write-brew-pkgs-db! dest packages)
  (begin
    (define entries
      (for/hash ([name (in-list packages)])
        (values name (brew-pkg-info-value name))
      ) ; end for/hash entries
    ) ; end define entries
    (call-with-output-file dest
      #:exists 'truncate/replace
      (lambda (out)
        (display "#hash(" out)
        (for ([name (in-list (sort packages string<?))]
              [idx (in-naturals)])
          (unless (zero? idx)
            (display " " out)
          ) ; end unless first entry
          (display "(" out)
          (write name out)
          (display " . " out)
          (display (hash-ref entries name) out)
          (display ")" out)
        ) ; end for package entry
        (display ")\n" out)
      ) ; end lambda out
    ) ; end call-with-output-file
  ) ; end begin write-brew-pkgs-db!
) ; end define write-brew-pkgs-db!

(define (patch-brew-racket-lib-info! pkgs-dir)
  (begin
    (define info-path (build-path pkgs-dir "racket-lib" "info.rkt"))
    (assert-file 'patch-brew-racket-lib-info! info-path)
    (define content (file->string info-path))
    (define excluded-dependency-line-rx
      (regexp (regexp-quote brew-racket-lib-excluded-dependency-line)))
    (define dependency-count
      (regexp-match-count excluded-dependency-line-rx content))
    (unless (= dependency-count 1)
      (raise-user-error 'patch-brew-racket-lib-info!
                        f"expected exactly one excluded platform dependency in {(clean-path-string info-path)}, found {dependency-count}")
    ) ; end unless exactly one excluded dependency
    (define patched-content
      (regexp-replace excluded-dependency-line-rx content ""))
    (when (string-contains? patched-content brew-racket-lib-excluded-dependency)
      (raise-user-error 'patch-brew-racket-lib-info!
                        f"excluded platform dependency still present after patch: {(clean-path-string info-path)}")
    ) ; end when dependency still present
    (write-text-file! info-path patched-content)
  ) ; end begin patch-brew-racket-lib-info!
) ; end define patch-brew-racket-lib-info!

(define (remove-exact-dependency! who info-path content dependency)
  (begin
    (define dependency-rx (regexp (regexp-quote dependency)))
    (define dependency-count (regexp-match-count dependency-rx content))
    (unless (= dependency-count 1)
      (raise-user-error who
                        f"expected exactly one excluded platform dependency in {(clean-path-string info-path)}, found {dependency-count}: {dependency}")
    ) ; end unless exactly one dependency
    (regexp-replace dependency-rx content "")
  ) ; end begin remove-exact-dependency!
) ; end define remove-exact-dependency!

(define (patch-brew-draw-lib-info! pkgs-dir)
  (begin
    (define info-path (build-path pkgs-dir "draw-lib" "info.rkt"))
    (assert-file 'patch-brew-draw-lib-info! info-path)
    (define patched-content
      (for/fold ([content (file->string info-path)])
                ([dependency (in-list brew-draw-lib-excluded-dependencies)])
        (remove-exact-dependency! 'patch-brew-draw-lib-info!
                                  info-path
                                  content
                                  dependency)
      ) ; end for/fold remove dependencies
    ) ; end define patched-content
    (for ([dependency (in-list brew-draw-lib-excluded-dependencies)])
      (when (string-contains? patched-content dependency)
        (raise-user-error 'patch-brew-draw-lib-info!
                          f"excluded platform dependency still present after patch: {(clean-path-string info-path)}")
      ) ; end when dependency still present
    ) ; end for dependency check
    (write-text-file! info-path patched-content)
  ) ; end begin patch-brew-draw-lib-info!
) ; end define patch-brew-draw-lib-info!

(define (copy-brew-licenses! racket-root dest-share)
  (begin
    (for ([name (in-list '("LICENSE-APACHE.txt"
                          "LICENSE-GPL.txt"
                          "LICENSE-LGPL.txt"
                          "LICENSE-MIT.txt"
                          "LICENSE-libscheme.txt"
                          "LICENSE.txt"))])
      (define share-src (build-path racket-root "racket" "share" name))
      (define src-src (build-path racket-root "racket" "src" name))
      (define src
        (cond
          [(file-exists? share-src) share-src]
          [(file-exists? src-src) src-src]
          [else
           (raise-user-error 'copy-brew-licenses! f"missing license file: {name}")]
        ) ; end cond license source
      ) ; end define src
      (copy-file src (build-path dest-share name) #t)
    ) ; end for license
  ) ; end begin copy-brew-licenses!
) ; end define copy-brew-licenses!

(define (stage-brew-source! c packages)
  (begin
    (define stage-root (brew-source-stage-root c))
    (define dist-root (brew-source-dist-root c))
    (reset-managed-dir! 'stage-brew-source! stage-root)
    (make-directory* dist-root)
    (write-brew-source-readme! (build-path dist-root "README") (cfg-source-version c))
    (make-directory* (build-path dist-root "etc"))
    (write-brew-config! (build-path dist-root "etc" "config.rktd") (cfg-source-version c))
    (copy-brew-tree! (build-path (cfg-racket-root c) "racket" "collects")
                     (build-path dist-root "collects"))
    (define staged-src (build-path dist-root "src"))
    (copy-brew-tree! (build-path (cfg-racket-root c) "racket" "src")
                     staged-src
                     #:skip-first-components '("build"))
    (patch-brew-chez-minimal-build! staged-src)
    (prune-brew-source-assets! staged-src)
    (define share-dir (build-path dist-root "share"))
    (define pkgs-dir (build-path share-dir "pkgs"))
    (make-directory* pkgs-dir)
    (copy-brew-licenses! (cfg-racket-root c) share-dir)
    (write-brew-links! (build-path share-dir "links.rktd") packages)
    (write-brew-pkgs-db! (build-path pkgs-dir "pkgs.rktd") packages)
    (for ([name (in-list packages)])
      (copy-brew-tree! (brew-package-source (cfg-racket-root c) name)
                       (build-path pkgs-dir name))
    ) ; end for package copy
    (patch-brew-racket-lib-info! pkgs-dir)
    (when (cfg-with-docs? c)
      (patch-brew-draw-lib-info! pkgs-dir)
    ) ; end when docs require draw-lib metadata patch
    dist-root
  ) ; end begin stage-brew-source!
) ; end define stage-brew-source!

(define (relative-files-from base root)
  (begin
    (define base/ (path->directory-path base))
    (sort
     (for/list ([p (in-list (find-files file-exists? root))])
       (find-relative-path base/ p)
     ) ; end for/list relative files
     path<?)
  ) ; end begin relative-files-from
) ; end define relative-files-from

(define (make-brew-tgz! c dist-root)
  (begin
    (define tgz-path (brew-output-tgz c))
    (define parent (path-only dist-root))
    (define tar-path (path-replace-extension tgz-path #".tar"))
    (assert-executable 'make-brew-tgz! "gzip")
    (make-directory* (cfg-artifact-dir c))
    (when (file-exists? tar-path)
      (delete-file tar-path)
    ) ; end when old tar exists
    (when (file-exists? tgz-path)
      (delete-file tgz-path)
    ) ; end when old tgz exists
    (parameterize ([current-directory parent])
      (call-with-output-file tar-path
        #:exists 'truncate/replace
        (lambda (out)
          (tar->output (relative-files-from parent (file-name-from-path dist-root))
                       out
                       #:timestamp 0
                       #:format 'pax)
        ) ; end lambda tar out
      ) ; end call-with-output-file tar
    ) ; end parameterize parent dir
    (define gzip (resolve-executable 'make-brew-tgz! "gzip"))
    (define-values (proc out in err)
      (subprocess #f #f #f gzip "-n" "-c" (clean-path-string tar-path))
    ) ; end define-values gzip process
    (close-output-port in)
    (call-with-output-file tgz-path
      #:exists 'truncate/replace
      (lambda (tgz-out)
        (copy-port out tgz-out)
      ) ; end lambda tgz out
    ) ; end call-with-output-file tgz
    (define stderr (port->string err))
    (subprocess-wait proc)
    (define status (subprocess-status proc))
    (close-input-port out)
    (close-input-port err)
    (delete-file tar-path)
    (unless (zero? status)
      (raise-user-error 'make-brew-tgz! f"gzip failed with exit {status}: {stderr}")
    ) ; end unless gzip success
    tgz-path
  ) ; end begin make-brew-tgz!
) ; end define make-brew-tgz!

(define (brew-tgz-member-path c relative-path)
  f"racket-{(cfg-source-version c)}/{relative-path}")

(define (brew-tgz-file-content c relative-path)
  (capture! 'validate-brew-tgz!
            (cfg-tar-bin c)
            (list "-xOf"
                  (clean-path-string (brew-output-tgz c))
                  (brew-tgz-member-path c relative-path))))

(define (validate-brew-tgz-file! c relative-path)
  (begin
    (brew-tgz-file-content c relative-path)
    (void)
  ) ; end begin validate-brew-tgz-file!
) ; end define validate-brew-tgz-file!

(define (validate-brew-tgz-content! c)
  (begin
    (assert-executable 'validate-brew-tgz! (cfg-tar-bin c))
    (for ([relative-path (in-list (brew-required-package-files c))])
      (validate-brew-tgz-file! c relative-path)
    ) ; end for required package files
    (define links-content (brew-tgz-file-content c "share/links.rktd"))
    (for ([needle (in-list (brew-required-link-needles c))])
      (unless (string-contains? links-content needle)
        (raise-user-error 'validate-brew-tgz!
                          f"brew source tgz links.rktd is missing: {needle}")
      ) ; end unless link needle
    ) ; end for required links
    (define pkgs-db-content (brew-tgz-file-content c "share/pkgs/pkgs.rktd"))
    (for ([needle (in-list (brew-required-pkgs-db-needles c))])
      (unless (string-contains? pkgs-db-content needle)
        (raise-user-error 'validate-brew-tgz!
                          f"brew source tgz pkgs.rktd is missing: {needle}")
      ) ; end unless pkgs db needle
    ) ; end for required pkgs db entries
    (define racket-lib-info-content
      (brew-tgz-file-content c "share/pkgs/racket-lib/info.rkt"))
    (when (string-contains? racket-lib-info-content brew-racket-lib-excluded-dependency)
      (raise-user-error 'validate-brew-tgz!
                        f"brew source tgz racket-lib/info.rkt still depends on excluded package: {brew-racket-lib-excluded-dependency}")
    ) ; end when excluded dependency still present
    (when (cfg-with-docs? c)
      (define draw-lib-info-content
        (brew-tgz-file-content c "share/pkgs/draw-lib/info.rkt"))
      (for ([dependency (in-list brew-draw-lib-excluded-dependencies)])
        (when (string-contains? draw-lib-info-content dependency)
          (raise-user-error 'validate-brew-tgz!
                            f"brew source tgz draw-lib/info.rkt still depends on excluded package: {dependency}")
        ) ; end when excluded draw dependency still present
      ) ; end for excluded draw dependency
    ) ; end when docs enabled
  ) ; end begin validate-brew-tgz-content!
) ; end define validate-brew-tgz-content!

(define (formula-mode-incremental? c)
  (string=? (cfg-formula-build-mode c) "incremental"))

(define (formula-mode-full? c)
  (string=? (cfg-formula-build-mode c) "full"))

(define (default-source-tag who source-version)
  (begin
    (when (string=? source-version "unknown")
      (raise-user-error who "release tag must be configured when Racket source version is unknown")
    ) ; end when source version unknown
    f"v{source-version}"
  ) ; end begin default-source-tag
) ; end define default-source-tag

(define (release-tag-from-config-or-source-version who config-path tag-key source-version)
  (begin
    (define fallback (lambda () (default-source-tag who source-version)))
    (cond
      [(file-exists? config-path)
       (define raw (read-rktd-hash who config-path))
       (define value (hash-ref raw tag-key #f))
       (cond
         [(not value) (fallback)]
         [(and (string? value) (not (string=? value ""))) value]
         [else (raise-user-error who f"config key must be a non-empty string: {tag-key}")]
       ) ; end cond configured tag
      ]
      [else
       (fallback)]
    ) ; end cond config exists
  ) ; end begin release-tag-from-config-or-source-version
) ; end define release-tag-from-config-or-source-version

(define (formula-source-tag c)
  (release-tag-from-config-or-source-version
   'formula-source-url
   (cfg-source-release-config c)
   'source-release-tag
   (cfg-source-version c)))

(define (source-release-default-tag c)
  (release-tag-from-config-or-source-version
   'source-release-config
   (cfg-source-release-config c)
   'source-release-tag
   (cfg-source-version c)))

(define (apt-release-default-tag c)
  (release-tag-from-config-or-source-version
   'apt-release-config
   (cfg-apt-release-config c)
   'apt-release-tag
   (cfg-source-version c)))

(define (github-release-tag-from-root-url who root-url)
  (match (regexp-match #rx"^https://github[.]com/[^/]+/[^/]+/releases/download/([^/]+)$" root-url)
    [(list _ tag) tag]
    [_ (raise-user-error who
                         f"--bottle-root-url must be a GitHub release download URL: {root-url}")]
  ) ; end match github release root url
) ; end define github-release-tag-from-root-url

(define (assert-homebrew-tap! c)
  (begin
    (assert-directory 'assert-homebrew-tap! (cfg-homebrew-tap c))
    (assert-directory 'assert-homebrew-tap! (build-path (cfg-homebrew-tap c) ".git"))
    (assert-directory 'assert-homebrew-tap! (build-path (cfg-homebrew-tap c) "Formula"))
    (when (cfg-update-formula? c)
      (when (formula-mode-incremental? c)
        (assert-file 'assert-homebrew-tap! (cfg-formula c))
      ) ; end when incremental formula requires existing file
      (unless (path-contained-in? (cfg-formula c) (build-path (cfg-homebrew-tap c) "Formula"))
        (raise-user-error 'assert-homebrew-tap!
                          f"formula must be inside tap Formula directory: {(clean-path-string (cfg-formula c))}")
      ) ; end unless formula inside tap Formula
      (unless (equal? (file-name-from-path (cfg-formula c)) (string->path "racket@9.rb"))
        (raise-user-error 'assert-homebrew-tap!
                          f"formula file must be racket@9.rb: {(clean-path-string (cfg-formula c))}")
      ) ; end unless formula basename expected
    ) ; end when update formula
  ) ; end begin assert-homebrew-tap!
) ; end define assert-homebrew-tap!

(define (formula-source-url c)
  f"https://github.com/CutieDeng/racket/releases/download/{(formula-source-tag c)}/{(brew-source-tgz-name c)}")

(define (formula-root-url c)
  f"root_url \"{(cfg-bottle-root-url c)}\"")

(define (formula-root-url-line c)
  f"    {(formula-root-url c)}")

(define (formula-source-url-line c)
  f"  url \"{(formula-source-url c)}\"")

(define (formula-source-sha256-line digest)
  f"  sha256 \"{digest}\"")

(define (formula-version-line c)
  f"  version \"{(cfg-formula-version c)}\"")

(define (formula-docs-test-content rb-bin)
  f"
    output = shell_output(\"{rb-bin}/raco docs --help\")
    assert_match \"search-terms\", output

    output = shell_output(\"{rb-bin}/raco doc --help\")
    assert_match \"search-terms\", output
")

(define (formula-sha256 formula-path)
  (begin
    (define content (file->string formula-path))
    (match (regexp-match #px"(?m:^  sha256 \"([0-9a-f]{64})\")" content)
      [(list _ digest) digest]
      [_ (raise-user-error 'formula-sha256
                           f"formula has no source sha256 line: {(clean-path-string formula-path)}")]
    ) ; end match formula sha
  ) ; end begin formula-sha256
) ; end define formula-sha256

(define (validate-formula-file! c formula-path)
  (begin
    (assert-nonempty-file 'validate-formula-file! formula-path)
    (define content (file->string formula-path))
    (for ([needle (in-list (list "class RacketAT9 < Formula"
                                 generated-code-notice-marker
                                 f"url \"{(formula-source-url c)}\""
                                 f"version \"{(cfg-formula-version c)}\""
	                                 "depends_on \"openssl@3\""
		                                 "depends_on \"ncurses\""
		                                 "racket_config.atomic_write content"
		                                 "(compiled-file-system-cache-root . \\\"#{system_cache_root}\\\")"
		                                 "content.sub!(/\\)\\s*\\z/"
			                                 "setup_system_cache"
			                                 "preserve_compiled_cache_dir?"
			                                 "system_cache_populated?"
			                                 "rhombus_demod_cache_populated?"
			                                 "package-racket-rhombus-cache"
			                                 "prefix/\"var/cache/racket/compiled#{share}/racket/collects\""
			                                 "system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"raco\", \"-l-\", \"raco\", \"setup\",\n           \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\", \"--no-launcher\""
	                                 "test do"
	                                 f"assert_match \"{(cfg-source-version c)}\""))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-formula-file!
                          f"formula is missing expected content: {needle}")
      ) ; end unless needle present
    ) ; end for formula needle
    (when (cfg-with-docs? c)
      (for ([needle (in-list (list "raco docs --help"
                                   "raco doc --help"))])
        (unless (string-contains? content needle)
          (raise-user-error 'validate-formula-file!
                            f"formula with --within-docs is missing expected content: {needle}")
        ) ; end unless docs needle present
      ) ; end for docs formula needle
    ) ; end when docs enabled
    (unless (= 1 (regexp-match-count #px"(?m:^  sha256 \"[0-9a-f]{64}\")" content))
      (raise-user-error 'validate-formula-file!
                        f"formula must contain exactly one source sha256 line: {(clean-path-string formula-path)}")
    ) ; end unless one source sha
    (unless (= 1 (regexp-match-count #px"(?m:^  version \"[^\"]+\")" content))
      (raise-user-error 'validate-formula-file!
                        f"formula must contain exactly one version line: {(clean-path-string formula-path)}")
    ) ; end unless one formula version
    (validate-formula-version-before-sha! 'validate-formula-file! content formula-path)
    (when (> (regexp-match-count #px"(?m:^    root_url \"[^\"]+\")" content) 1)
      (raise-user-error 'validate-formula-file!
                        f"formula must contain at most one bottle root_url line: {(clean-path-string formula-path)}")
    ) ; end when too many root_url lines
    (formula-sha256 formula-path)
    (void)
  ) ; end begin validate-formula-file!
) ; end define validate-formula-file!

(define (validate-formula-template! formula-path)
  (begin
    (assert-nonempty-file 'validate-formula-template! formula-path)
    (define content (file->string formula-path))
    (for ([needle (in-list (list "class RacketAT9 < Formula"
                                 generated-code-notice-marker
                                 "racket-minimal-"
                                 "test do"))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-formula-template!
                          f"formula template is missing expected content: {needle}")
      ) ; end unless needle present
    ) ; end for template needle
    (unless (= 1 (regexp-match-count #px"(?m:^  url \"[^\"]+racket-minimal-[^\"]+-src[.]tgz\")" content))
      (raise-user-error 'validate-formula-template!
                        f"formula template must contain exactly one source url line: {(clean-path-string formula-path)}")
    ) ; end unless one source url
    (unless (= 1 (regexp-match-count #px"(?m:^  sha256 \"[0-9a-f]{64}\")" content))
      (raise-user-error 'validate-formula-template!
                        f"formula template must contain exactly one source sha256 line: {(clean-path-string formula-path)}")
    ) ; end unless one source sha
    (when (> (regexp-match-count #px"(?m:^  version \"[^\"]+\")" content) 1)
      (raise-user-error 'validate-formula-template!
                        f"formula template must contain at most one version line: {(clean-path-string formula-path)}")
    ) ; end when too many formula versions
    (validate-formula-version-before-sha! 'validate-formula-template! content formula-path)
    (when (> (regexp-match-count #px"(?m:^    root_url \"[^\"]+\")" content) 1)
      (raise-user-error 'validate-formula-template!
                        f"formula template must contain at most one bottle root_url line: {(clean-path-string formula-path)}")
    ) ; end when too many root_url lines
  ) ; end begin validate-formula-template!
) ; end define validate-formula-template!

(define (set-formula-bottle-root-url! c formula-path)
  (begin
    (assert-nonempty-file 'set-formula-bottle-root-url! formula-path)
    (define content (file->string formula-path))
    (define root-url-rx #px"(?m:^    root_url \"[^\"]+\")")
    (define root-url-count (regexp-match-count root-url-rx content))
    (cond
      [(= root-url-count 0)
       (validate-formula-file! c formula-path)]
      [(= root-url-count 1)
       (write-text-file! formula-path (regexp-replace root-url-rx content (formula-root-url-line c)))
       (validate-formula-file! c formula-path)]
      [else
       (raise-user-error 'set-formula-bottle-root-url!
                         f"formula must contain at most one bottle root_url line: {(clean-path-string formula-path)}")]
    ) ; end cond root_url count
  ) ; end begin set-formula-bottle-root-url!
) ; end define set-formula-bottle-root-url!

(define brew-configure-racket-method
"  def configure_racket
    config_entries = [
      \"(default-scope . \\\"installation\\\")\",
      \"(compiled-file-cache-roots . (user system))\",
      \"(compiled-file-system-cache-root . \\\"#{system_cache_root}\\\")\",
    ].join(\" \")
    content = racket_config.read
    %w[
      default-scope
      compiled-file-cache-roots
      compiled-file-system-cache-root
    ].each do |key|
      content = content.gsub(/\\s*\\(#{Regexp.escape(key)}\\s+\\.\\s+(?:\"[^\"]*\"|\\([^)]*\\)|[^\\s)]*)\\)/, \"\")
    end
    raise \"could not append Racket config entries\" unless content.sub!(/\\)\\s*\\z/, \" #{config_entries})\\n\")

    racket_config.atomic_write content
  end")

(define (replace-brew-configure-racket-method content)
  (begin
    (define start-match
      (or (regexp-match-positions #px"  def source_racket_config\n" content)
          (regexp-match-positions #px"  def configure_racket" content)))
    (unless start-match
      (raise-user-error 'set-formula-source! "formula has no configure_racket method to replace")
    ) ; end unless start match
    (define start (car (car start-match)))
    (define install-match (regexp-match-positions #px"\n  def install\n" content start))
    (unless install-match
      (raise-user-error 'set-formula-source! "formula configure_racket method is not followed by install")
    ) ; end unless install match
    (define install-start (car (car install-match)))
    (string-append (substring content 0 start)
                   brew-configure-racket-method
                   "\n"
                   (substring content install-start))
  ) ; end begin replace-brew-configure-racket-method
) ; end define replace-brew-configure-racket-method

(define (set-formula-source! c formula-path digest)
  (begin
    (assert-nonempty-file 'set-formula-source! formula-path)
    (define content (file->string formula-path))
    (define source-url-rx #px"(?m:^  url \"[^\"]+racket-minimal-[^\"]+-src[.]tgz\")")
    (define source-sha-rx #px"(?m:^  sha256 \"[0-9a-f]{64}\")")
    (define formula-version-rx #px"(?m:^  version \"[^\"]+\")")
    (define formula-version-line-rx #px"(?m:^  version \"[^\"]+\"\n)")
    (unless (= 1 (regexp-match-count source-url-rx content))
      (raise-user-error 'set-formula-source!
                        f"formula must contain exactly one source url line: {(clean-path-string formula-path)}")
    ) ; end unless exactly one source url
    (unless (= 1 (regexp-match-count source-sha-rx content))
      (raise-user-error 'set-formula-source!
                        f"formula must contain exactly one source sha256 line: {(clean-path-string formula-path)}")
    ) ; end unless exactly one source sha
    (when (> (regexp-match-count formula-version-rx content) 1)
      (raise-user-error 'set-formula-source!
                        f"formula must contain at most one version line: {(clean-path-string formula-path)}")
    ) ; end when too many version lines
    (define with-source-url
      (regexp-replace source-url-rx content (formula-source-url-line c))
    ) ; end define with-source-url
    (define without-version
      (if (regexp-match? formula-version-line-rx with-source-url)
          (regexp-replace formula-version-line-rx with-source-url "")
          with-source-url)
    ) ; end define without-version
    (define with-version
      (regexp-replace source-sha-rx
                      without-version
                      f"{(formula-version-line c)}
{(formula-source-sha256-line digest)}")
    ) ; end define with-version
    (define with-cache-root
      (string-replace with-version
                      "(compiled-file-system-cache-root . \\\"#{var}/cache/racket/compiled\\\")"
                      "(compiled-file-system-cache-root . \\\"#{prefix}/var/cache/racket/compiled\\\")")
    ) ; end define with-cache-root
    (define with-post-install
      (string-replace with-cache-root
                      "  def post_install
    system bin/\"raco\", \"setup\", \"--no-user\", \"--no-zo\"
    remove_precompiled_cache
  end"
                      "  def post_install
    system bin/\"raco\", \"setup\", \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\"
  end")
    ) ; end define with-post-install
    (define with-system-cache-methods
      (string-replace with-post-install
                      "  def post_install
    system bin/\"raco\", \"setup\", \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\"
  end

  def remove_precompiled_cache
    rm_r Dir[\"#{prefix}/**/compiled\"].sort_by(&:length).reverse
  end"
                      "  def system_cache_root
    prefix/\"var/cache/racket/compiled\"
  end

  def system_cache_roots
    [
      prefix/\"var/cache/racket/compiled#{share}/racket/collects\",
      prefix/\"var/cache/racket/compiled#{share}/racket/pkgs\",
    ]
  end

  def system_cache_populated?
    system_cache_roots.all? { |root| !Dir[\"#{root}/**/compiled/*.zo\"].empty? }
  end

  def setup_system_cache
    system bin/\"racket\", \"-N\", \"raco\", \"-l-\", \"raco\", \"setup\",
           \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\"
  end

  def post_install
    setup_system_cache unless system_cache_populated?
    remove_precompiled_cache
  end

  def preserve_compiled_cache_dir?(path)
    path = Pathname(path).cleanpath
    preserved_roots = [system_cache_root, rhombus_demod_cache].map(&:cleanpath)
    preserved_roots.any? do |root|
      path == root || path.to_s.start_with?(\"#{root}/\") || root.to_s.start_with?(\"#{path}/\")
    end
  end

  def remove_precompiled_cache
    Dir[\"#{prefix}/**/compiled\"].sort_by(&:length).reverse_each do |dir|
      next if preserve_compiled_cache_dir?(dir)

      rm_r dir
    end
  end")
    ) ; end define with-system-cache-methods
    (define with-system-cache-methods/fallback
      (if (string-contains? with-system-cache-methods "def system_cache_roots")
          with-system-cache-methods
          (string-replace with-system-cache-methods
                          "  def post_install
    system bin/\"raco\", \"setup\", \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\"
  end"
                          "  def system_cache_root
    prefix/\"var/cache/racket/compiled\"
  end

  def system_cache_roots
    [
      prefix/\"var/cache/racket/compiled#{share}/racket/collects\",
      prefix/\"var/cache/racket/compiled#{share}/racket/pkgs\",
    ]
  end

  def system_cache_populated?
    system_cache_roots.all? { |root| !Dir[\"#{root}/**/compiled/*.zo\"].empty? }
  end

  def setup_system_cache
    system bin/\"racket\", \"-N\", \"raco\", \"-l-\", \"raco\", \"setup\",
           \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\"
  end

  def post_install
    setup_system_cache unless system_cache_populated?
    remove_precompiled_cache
  end"))
    ) ; end define with-system-cache-methods/fallback
    (define with-bottle-cache-setup
      (string-replace with-system-cache-methods/fallback
                      "    system bin/\"raco\", \"setup\", \"--no-user\", \"--no-zo\"
    remove_precompiled_cache
  end"
                      "    system bin/\"raco\", \"setup\", \"--no-user\", \"--no-zo\"
    if build.bottle?
      setup_system_cache
      remove_precompiled_cache
    end
  end")
    ) ; end define with-bottle-cache-setup
    (define with-cache-test
      (string-replace with-bottle-cache-setup
                      "    assert !Dir[\"#{prefix}/var/cache/racket/compiled/**/*.zo\"].empty?, \"system compiled cache is empty\""
                      "    assert system_cache_populated?, \"system compiled cache is empty\"")
    ) ; end define with-cache-test
	    (define with-homebrew-style
	      (string-replace
	       (string-replace with-cache-test
	                       "var/cache/racket/compiled#{prefix}/share"
	                       "var/cache/racket/compiled#{share}")
	       "    system bin/\"racket\", \"-N\", \"raco\", \"-l-\", \"raco\", \"setup\", \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\""
	       "    system bin/\"racket\", \"-N\", \"raco\", \"-l-\", \"raco\", \"setup\",
	           \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\"")
	    ) ; end define with-homebrew-style
	    (define with-rhombus-cache-methods
	      (if (string-contains? with-homebrew-style "rhombus_demod_cache_populated?")
	          with-homebrew-style
	          (string-replace
	           (string-replace with-homebrew-style
	                           "    system_cache_roots.all? { |root| !Dir[\"#{root}/**/compiled/*.zo\"].empty? }"
	                           (string-append
	                            "    system_cache_roots.all? { |root| !Dir[\"#{root}/**/compiled/*.zo\"].empty? } &&\n"
	                            "      rhombus_demod_cache_populated?"))
	           "  def setup_system_cache"
	           (string-append
	            "  def rhombus_demod_cache\n"
	            "    prefix/\"share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod\"\n"
	            "  end\n\n"
	            "  def rhombus_demod_cache_populated?\n"
	            "    !Dir[\"#{rhombus_demod_cache}/**/compiled/*.zo\"].empty?\n"
	            "  end\n\n"
	            "  def setup_system_cache")))
	    ) ; end define with-rhombus-cache-methods
	(define with-rhombus-cache-setup
	  (if (string-contains? with-rhombus-cache-methods "package-racket-rhombus-cache")
	      with-rhombus-cache-methods
	      (string-replace with-rhombus-cache-methods
	                          "           \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\""
	                          (string-append
		                       "           \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\"\n"
		                       "    system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"rhombus\",\n"
		                       "           \"-l-\", \"rhombus/run.rhm\", \"--version\"\n"
		                       "    system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"rhombus\",\n"
		                       "           \"-l-\", \"rhombus/run.rhm\", \"-e\", \"println(\\\"package-racket-rhombus-cache\\\")\"")))
	) ; end define with-rhombus-cache-setup
	(define without-source-configure
	  (string-replace
	   (string-replace with-rhombus-cache-setup
	                   "    # Configure racket's package tool (raco) to use installation scope.
    config_entries = [
      \"(default-scope . \\\"installation\\\")\",
      \"(compiled-file-cache-roots . (user system))\",
      \"(compiled-file-system-cache-root . \\\"#{prefix}/var/cache/racket/compiled\\\")\",
    ].join(\" \")
    inreplace \"etc/config.rktd\", /\\)\\)\\n$/, \") \" + config_entries + \")\\n\"

"
	                   "")
	   "    config_entries = [
      \"(default-scope . \\\"installation\\\")\",
      \"(compiled-file-cache-roots . (user system))\",
      \"(compiled-file-system-cache-root . \\\"#{prefix}/var/cache/racket/compiled\\\")\",
    ].join(\" \")
    inreplace \"etc/config.rktd\", /\\)\\)\\n$/, \") \" + config_entries + \")\\n\"
"
	   "")
	) ; end define without-source-configure
	(define without-source-configure-call
	  (string-replace without-source-configure
	                  "    configure_racket source_racket_config

"
	                  "")
	) ; end define without-source-configure-call
	(define with-installed-configure-call
	  (string-replace without-source-configure-call
	                  "    system bin/\"raco\", \"setup\", \"--no-user\", \"--no-zo\"
    if build.bottle?"
	                  "    system bin/\"raco\", \"setup\", \"--no-user\", \"--no-zo\"
    configure_racket
    if build.bottle?")
	) ; end define with-installed-configure-call
		(define with-configure-method
		  (if (string-contains? with-installed-configure-call "def configure_racket")
		      (replace-brew-configure-racket-method with-installed-configure-call)
		      (string-replace with-installed-configure-call
		                      "  def system_cache_root
    prefix/\"var/cache/racket/compiled\"
  end"
		                      (string-append "  def system_cache_root
    prefix/\"var/cache/racket/compiled\"
  end

" brew-configure-racket-method)))
		) ; end define with-configure-method
	(define with-forced-raco-cache-root
	  (string-replace with-configure-method
	                  "    system bin/\"racket\", \"-N\", \"raco\", \"-l-\", \"raco\", \"setup\",
           \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\""
	                  "    system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"raco\", \"-l-\", \"raco\", \"setup\",
           \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\"")
	) ; end define with-forced-raco-cache-root
	(define with-cache-root-directory
	  (if (string-contains? with-forced-raco-cache-root "system_cache_root.mkpath")
	      with-forced-raco-cache-root
	      (string-replace with-forced-raco-cache-root
	                      "  def setup_system_cache
"
	                      "  def setup_system_cache
    system_cache_root.mkpath
"))
	) ; end define with-cache-root-directory
	(define with-forced-rhombus-cache-root
	  (string-replace with-cache-root-directory
		                  "    system bin/\"racket\", \"-N\", \"rhombus\", \"-l-\", \"rhombus/run.rhm\",
	           \"-e\", \"println(\\\"package-racket-rhombus-cache\\\")\""
		                  "    system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"rhombus\",
	           \"-l-\", \"rhombus/run.rhm\", \"--version\"
	    system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"rhombus\",
	           \"-l-\", \"rhombus/run.rhm\", \"-e\", \"println(\\\"package-racket-rhombus-cache\\\")\"")
		) ; end define with-forced-rhombus-cache-root
		(define with-no-launcher-cache-setup
		  (regexp-replace* #px"\"--no-pkg-deps\"(?:, \"--no-launcher\")*"
		                   with-forced-rhombus-cache-root
		                   "\"--no-pkg-deps\", \"--no-launcher\"")
		) ; end define with-no-launcher-cache-setup
		(write-text-file!
		 formula-path
		 with-no-launcher-cache-setup)
    (ensure-formula-docs-test! c formula-path)
    (validate-formula-file! c formula-path)
  ) ; end begin set-formula-source!
) ; end define set-formula-source!

(define (ensure-formula-docs-test! c formula-path)
  (begin
    (when (cfg-with-docs? c)
      (assert-nonempty-file 'ensure-formula-docs-test! formula-path)
      (define content (file->string formula-path))
      (unless (string-contains? content "raco docs --help")
        (unless (= 1 (regexp-match-count #px"(?m:^  test do\n)" content))
          (raise-user-error 'ensure-formula-docs-test!
                            f"formula must contain exactly one test block for docs test insertion: {(clean-path-string formula-path)}")
        ) ; end unless exactly one test block
        (write-text-file!
         formula-path
         (regexp-replace #px"(?m:^  test do\n)"
                         content
                         (string-append "  test do\n"
                                        (formula-docs-test-content (ruby-interpolate "bin")))))
      ) ; end unless docs test already present
      (validate-formula-file! c formula-path)
    ) ; end when docs enabled
  ) ; end begin ensure-formula-docs-test!
) ; end define ensure-formula-docs-test!

(define (ruby-interpolate expression)
  (string-append "#{" expression "}"))

(define (formula-content/full c digest)
  (begin
    (define version (cfg-source-version c))
    (define rb-prefix (ruby-interpolate "prefix"))
    (define rb-man (ruby-interpolate "man"))
    (define rb-etc (ruby-interpolate "etc"))
    (define rb-openssl-rpath (ruby-interpolate "formula_opt_lib(\"openssl@3\")"))
    (define rb-openssl-libssl (ruby-interpolate "formula_opt_lib(\"openssl@3\")/shared_library(\"libssl\")"))
    (define rb-bin (ruby-interpolate "bin"))
    (define rb-lib (ruby-interpolate "lib"))
    (define rb-root (ruby-interpolate "root"))
    (define rb-share (ruby-interpolate "share"))
    (define rb-empty-home (ruby-interpolate "empty_home"))
    (define rb-test-script (ruby-interpolate "testpath/\"interactive-packages.rkt\""))
    (define rb-rhombus-script (ruby-interpolate "testpath/\"rhombus-smoke.rhm\""))
    (define macos-openssl-rx "%r{.*openssl@3/.*/libssl.*\\.dylib}")
    f"{(generated-code-notice "#")}class RacketAT9 < Formula
  desc \"Modern programming language in the Lisp/Scheme family\"
  homepage \"https://racket-lang.org/\"
  url \"{(formula-source-url c)}\"
  version \"{(cfg-formula-version c)}\"
  sha256 \"{digest}\"
  license any_of: [\"MIT\", \"Apache-2.0\"]

  livecheck do
    skip \"Private Racket fork releases are managed manually\"
  end

  depends_on \"openssl@3\"

  uses_from_macos \"libffi\"

  on_linux do
    depends_on \"libedit\"
    depends_on \"ncurses\"
    depends_on \"zlib-ng-compat\"
  end

  # These files are amended when packages are installed or removed.
  skip_clean \"lib/racket/launchers.rktd\", \"lib/racket/mans.rktd\"

  def racket_config
    etc/\"racket/config.rktd\"
  end

  def system_cache_root
    prefix/\"var/cache/racket/compiled\"
  end

  def configure_racket
    config_entries = [
      \"(default-scope . \\\"installation\\\")\",
      \"(compiled-file-cache-roots . (user system))\",
      \"(compiled-file-system-cache-root . \\\"#{{system_cache_root}}\\\")\",
    ].join(\" \")
    content = racket_config.read
    %w[
      default-scope
      compiled-file-cache-roots
      compiled-file-system-cache-root
    ].each do |key|
      content = content.gsub(/\\s*\\(#{{Regexp.escape(key)}}\\s+\\.\\s+(?:\"[^\"]*\"|\\([^)]*\\)|[^\\s)]*)\\)/, \"\")
    end
    raise \"could not append Racket config entries\" unless content.sub!(/\\)\\s*\\z/, \" #{{config_entries}})\\n\")

    racket_config.atomic_write content
  end

  def install
    # Prefer Homebrew OpenSSL 3 over older OpenSSL variants.
    inreplace %w[libssl.rkt libcrypto.rkt].map {{ |file| buildpath/\"collects/openssl\"/file }},
              '\"1.1\"', '\"3\"'

    cd \"src\" do
      args = %W[
        --disable-debug
        --disable-dependency-tracking
        --enable-origtree=no
        --enable-sharezo
        --enable-macprefix
        --prefix={rb-prefix}
        --mandir={rb-man}
        --sysconfdir={rb-etc}
        --enable-useprefix
      ]

      ENV[\"LDFLAGS\"] = \"-rpath {rb-openssl-rpath}\"
      ENV[\"LDFLAGS\"] = \"-Wl,-rpath={rb-openssl-rpath}\" if OS.linux?

      system \"./configure\", *args
      system \"make\"
      system \"make\", \"install\"

      if OS.mac?
        openssl_opt_lib = formula_opt_lib(\"openssl@3\")
        racket_libdir = lib/\"racket\"

        %w[libssl.3.dylib libcrypto.3.dylib].each do |dylib|
          path = racket_libdir/dylib
          path.unlink if path.exist?
        end

        ln_s openssl_opt_lib/\"libssl.3.dylib\",    racket_libdir/\"libssl.3.dylib\"
        ln_s openssl_opt_lib/\"libcrypto.3.dylib\", racket_libdir/\"libcrypto.3.dylib\"
      end
    end

    system bin/\"raco\", \"setup\", \"--no-user\", \"--no-zo\"
    configure_racket
    if build.bottle?
      setup_system_cache
      remove_precompiled_cache
    end
  end

  def system_cache_roots
    [
      prefix/\"var/cache/racket/compiled{rb-share}/racket/collects\",
      prefix/\"var/cache/racket/compiled{rb-share}/racket/pkgs\",
    ]
  end

  def system_cache_populated?
    system_cache_roots.all? {{ |root| !Dir[\"{rb-root}/**/compiled/*.zo\"].empty? }} &&
      rhombus_demod_cache_populated?
  end

  def rhombus_demod_cache
    prefix/\"share/racket/pkgs/rhombus-lib/rhombus/private/compiled/ephemeral/demod\"
  end

  def rhombus_demod_cache_populated?
    !Dir[\"#{{rhombus_demod_cache}}/**/compiled/*.zo\"].empty?
  end

  def setup_system_cache
    system_cache_root.mkpath
    system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"raco\", \"-l-\", \"raco\", \"setup\",
           \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\", \"--no-launcher\"
    system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"rhombus\",
           \"-l-\", \"rhombus/run.rhm\", \"--version\"
    system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"rhombus\",
           \"-l-\", \"rhombus/run.rhm\", \"-e\", \"println(\\\"package-racket-rhombus-cache\\\")\"
  end

  def post_install
    setup_system_cache unless system_cache_populated?
    remove_precompiled_cache
  end

  def preserve_compiled_cache_dir?(path)
    path = Pathname(path).cleanpath
    preserved_roots = [system_cache_root, rhombus_demod_cache].map(&:cleanpath)
    preserved_roots.any? do |root|
      path == root || path.to_s.start_with?(\"#{{root}}/\") || root.to_s.start_with?(\"#{{path}}/\")
    end
  end

  def remove_precompiled_cache
    Dir[\"{rb-prefix}/**/compiled\"].sort_by(&:length).reverse_each do |dir|
      next if preserve_compiled_cache_dir?(dir)

      rm_r dir
    end
  end

  def caveats
    <<~EOS
      This formula is intended to provide the active Homebrew `racket` and
      `raco` commands.

      If an official Racket formula or cask is already installed, remove it
      before installing this formula:
        brew uninstall minimal-racket
        brew uninstall --cask racket
    EOS
  end

  test do
    require \"pty\"
    require \"timeout\"

    assert_match \"{version}\", shell_output(\"{rb-bin}/racket -e '(displayln (version))'\"){(if (cfg-with-docs? c) (formula-docs-test-content rb-bin) "")}
    output = shell_output(\"{rb-bin}/racket -e '(require racket/pvector) (displayln (pvector->list (pvector 1 2 3)))'\")
    assert_match \"(1 2 3)\", output
    assert system_cache_populated?, \"system compiled cache is empty\"
    assert rhombus_demod_cache_populated?, \"Rhombus demod cache is empty\"

    empty_home = testpath/\"empty-home\"
    empty_home.mkpath
    output = shell_output(
      \"HOME={rb-empty-home} {rb-bin}/racket \" \\
      \"-e '(require racket/list racket/match racket/file) (displayln \\\"brew-empty-home-ok\\\")'\",
    )
    assert_match \"brew-empty-home-ok\", output

    (testpath/\"interactive-packages.rkt\").write <<~RACKET
      #lang racket/base
      (for ([p '((\"main.rkt\" \"xrepl\")
                 (\"main.rkt\" \"expeditor\")
                 (\"pread.rkt\" \"readline\"))])
        (unless (collection-file-path (car p) (cadr p) #:fail (lambda _ #f))
          (error (cadr p) \"collection missing\")))
      (displayln \"interactive-packages-ok\")
    RACKET
    output = shell_output(\"{rb-bin}/racket {rb-test-script}\")
    assert_match \"interactive-packages-ok\", output

    (testpath/\"rhombus-smoke.rhm\").write <<~RHOMBUS
      #lang rhombus
      println(\"rhombus-lang-ok\")
    RHOMBUS
    output = shell_output(\"{rb-bin}/racket {rb-rhombus-script}\")
    assert_match \"rhombus-lang-ok\", output

    output = shell_output(\"{rb-bin}/rhombus --version\")
    assert_match \"Welcome to Rhombus v1.0\", output

    output = shell_output(\"{rb-bin}/rhombus -e '1 + 2'\")
    assert_match \"3\", output

    output = shell_output(\"printf '1\\\\n' | {rb-bin}/racket\")
    assert_match \"Welcome to Racket v{version} [cs].\", output
    assert_match(/^> 1$/, output)

    output = shell_output(\"printf 'f\\\"hi\\\"\\\\n' | {rb-bin}/racket\")
    assert_match(/^> \"hi\"$/, output)

    pty_output = +\"\"
    read_available = lambda do |reader, timeout|
      loop do
        pty_output << Timeout.timeout(timeout) {{ reader.readpartial(4096) }}
        timeout = 0.1
      end
    rescue Timeout::Error, EOFError
      pty_output
    end
    read_until_result = lambda do |reader|
      loop do
        pty_output << Timeout.timeout(0.5) {{ reader.readpartial(4096) }}
        break if pty_output.include?(\"#t\")
      end
    rescue Timeout::Error, EOFError
      pty_output
    end
    Timeout.timeout(5) do
      PTY.spawn({{ \"TERM\" => \"xterm-256color\" }}, \"{rb-bin}/racket\") do |r, w, pid|
        read_available.call(r, 0.5)
        w.write \"\\n\"
        read_available.call(r, 0.5)
        w.puts \"(= 1 1)\"
        read_until_result.call(r)
        w.write \"\\x04\"
        Process.kill(\"KILL\", pid)
        Process.detach(pid)
      end
    end
    assert_match \"Welcome to Racket v{version} [cs].\", pty_output
    assert_match \"\\n#t\", pty_output
    refute_match(/no readline support/, pty_output)
    assert !pty_output.match?(/> \\r?\\n\\(/), \"empty input fell back to the plain REPL reader\"

    assert_match '(default-scope . \"installation\")', racket_config.read

    if OS.mac?
      output = shell_output(\"DYLD_PRINT_LIBRARIES=1 {rb-bin}/racket -e '(require openssl)' 2>&1\")
      assert_match({macos-openssl-rx}, output)
    else
      output = shell_output(\"LD_DEBUG=libs {rb-bin}/racket -e '(require openssl)' 2>&1\")
      assert_match \"init: {rb-openssl-libssl}\", output
    end
  end
end
"
  ) ; end begin formula-content/full
) ; end define formula-content/full

(define (write-full-formula! c formula-path digest)
  (begin
    (make-directory* (path-only formula-path))
    (write-text-file! formula-path (formula-content/full c digest))
    (validate-formula-file! c formula-path)
  ) ; end begin write-full-formula!
) ; end define write-full-formula!

(define (validate-brew-tgz! c)
  (begin
    (define tgz-path (brew-output-tgz c))
    (assert-nonempty-file 'validate-brew-tgz! tgz-path)
    (unless (equal? (file-name-from-path tgz-path) (string->path (brew-source-tgz-name c)))
      (raise-user-error 'validate-brew-tgz!
                        f"brew source tgz name must be {(brew-source-tgz-name c)}: {(clean-path-string tgz-path)}")
    ) ; end unless brew tgz basename matches formula
    (validate-brew-tgz-content! c)
    (println/flush f"Validated brew source tgz: {(clean-path-string tgz-path)}")
  ) ; end begin validate-brew-tgz!
) ; end define validate-brew-tgz!

(define (validate-brew-artifact! c formula-path)
  (begin
    (validate-formula-file! c formula-path)
    (validate-brew-tgz! c)
    (define formula-digest (formula-sha256 formula-path))
    (define actual-digest (sha256-file (brew-output-tgz c)))
    (unless (string=? formula-digest actual-digest)
      (raise-user-error 'validate-brew-artifact!
                        f"formula sha256 {formula-digest} does not match generated tgz sha256 {actual-digest}")
    ) ; end unless formula sha matches tgz
    (println/flush f"Validated brew formula: {(clean-path-string formula-path)}")
  ) ; end begin validate-brew-artifact!
) ; end define validate-brew-artifact!

(define (print-brew-dry-run-plan! c packages)
  (begin
    (println/flush f"Would stage brew source root: {(clean-path-string (brew-source-stage-root c))}")
    (println/flush f"Would stage brew source directory: {(clean-path-string (brew-source-dist-root c))}")
    (println/flush f"Would create brew source tgz: {(clean-path-string (brew-output-tgz c))}")
    (println/flush f"Would set Formula source URL: {(formula-source-url c)}")
    (println/flush f"Would set Formula bottle root URL: {(cfg-bottle-root-url c)}")
    (println/flush f"Would use Formula build mode: {(cfg-formula-build-mode c)}")
    (println/flush f"Would include brew docs: {(if (cfg-with-docs? c) "yes" "no")}")
    (println/flush f"Would include brew package count: {(number->string (length packages))}")
    (if (cfg-update-formula? c)
        (begin
          (println/flush f"Would generate brew formula: {(clean-path-string (brew-generated-formula c))}")
          (println/flush f"Would update brew formula: {(clean-path-string (cfg-formula c))}")
        ) ; end begin update formula plan
        (println/flush "Would skip brew formula update")
    ) ; end if formula update plan
  ) ; end begin print-brew-dry-run-plan!
) ; end define print-brew-dry-run-plan!

(define (prepare-generated-formula! c)
  (begin
    (assert-homebrew-tap! c)
    (define work-root (brew-work-root c))
    (define generated (brew-generated-formula c))
    (define original-digest
      (and (file-exists? (cfg-formula c))
           (sha256-file (cfg-formula c))))
    (reset-managed-dir! 'prepare-generated-formula! work-root)
    (make-directory* (path-only generated))
    (cond
      [(formula-mode-incremental? c)
       (unless original-digest
         (raise-user-error 'prepare-generated-formula!
                           f"incremental formula mode requires an existing formula: {(clean-path-string (cfg-formula c))}")
      ) ; end unless existing formula digest
       (copy-file (cfg-formula c) generated #t)
       (ensure-generated-code-notice! 'prepare-generated-formula! generated "#")
       (validate-formula-template! generated)]
      [(formula-mode-full? c)
       (void)]
      [else
       (raise-user-error 'prepare-generated-formula!
                         f"unsupported formula build mode: {(cfg-formula-build-mode c)}")]
    ) ; end cond formula mode
    (values generated original-digest)
  ) ; end begin prepare-generated-formula!
) ; end define prepare-generated-formula!

(define (install-generated-formula! c generated original-digest)
  (begin
    (define dest (cfg-formula c))
    (define dest-dir (path-only dest))
    (assert-homebrew-tap! c)
    (assert-writable-directory 'install-generated-formula! dest-dir)
    (validate-brew-artifact! c generated)
    (cond
      [original-digest
       (let ([current-digest (sha256-file dest)])
         (unless (string=? current-digest original-digest)
           (raise-user-error 'install-generated-formula!
                             f"refusing to replace formula because it changed during this run: {(clean-path-string dest)}")
         ) ; end unless formula unchanged
       ) ; end let current digest
      ]
      [(file-exists? dest)
       (raise-user-error 'install-generated-formula!
                         f"refusing to replace formula because it appeared during this run: {(clean-path-string dest)}")]
      [else
       (void)]
    ) ; end cond existing destination
    (define temp (make-temporary-file ".racket@9.rb.tmp~a" #f dest-dir))
    (copy-file generated temp #t)
    (validate-formula-file! c temp)
    (rename-file-or-directory temp dest #t)
    (validate-brew-artifact! c dest)
    (println/flush f"Installed brew formula: {(clean-path-string dest)}")
  ) ; end begin install-generated-formula!
) ; end define install-generated-formula!

(define (build-brew! c)
  (begin
    (define finalizers '())
    (define generated-formula #f)
    (define packages (brew-source-packages c))
    (println/flush f"Brew source packages: {(string-join packages " ")}")
    (when (cfg-dry-run? c)
      (assert-homebrew-tap! c)
      (print-brew-dry-run-plan! c packages)
    ) ; end when dry-run brew plan
    (unless (cfg-dry-run? c)
      (assert-homebrew-tap! c)
      (make-directory* (cfg-artifact-dir c))
      (when (cfg-update-formula? c)
        (define-values (generated original-digest)
          (prepare-generated-formula! c)
        ) ; end define-values generated/original-digest
        (set! generated-formula generated)
        (set! finalizers
              (list (lambda ()
                      (install-generated-formula! c generated original-digest)
                    ) ; end lambda install generated formula
              ) ; end list finalizer
        ) ; end set! finalizers
      ) ; end when update formula
      (define dist-root (stage-brew-source! c packages))
      (make-brew-tgz! c dist-root)
      (define digest (sha256-file (brew-output-tgz c)))
      (println/flush f"sha256: {digest}")
      (when (cfg-update-formula? c)
        (cond
          [(formula-mode-incremental? c)
           (set-formula-source! c generated-formula digest)
           (set-formula-bottle-root-url! c generated-formula)]
          [(formula-mode-full? c)
           (write-full-formula! c generated-formula digest)]
          [else
           (raise-user-error 'build-brew!
                             f"unsupported formula build mode: {(cfg-formula-build-mode c)}")]
        ) ; end cond formula mode
      ) ; end when update formula fields
    ) ; end unless dry-run prepare brew
    (when (and (cfg-dry-run? c) (cfg-update-formula? c))
      (set! generated-formula (brew-generated-formula c))
      (set! finalizers
            (list (lambda ()
                    (println/flush f"Would install brew formula: {(clean-path-string (cfg-formula c))}")
                  ) ; end lambda dry-run install notice
            ) ; end list dry-run finalizer
      ) ; end set! dry-run finalizers
    ) ; end when dry-run update formula
    (unless (cfg-dry-run? c)
      (if (cfg-update-formula? c)
          (validate-brew-artifact! c generated-formula)
          (validate-brew-tgz! c))
    ) ; end unless dry-run validate brew
    finalizers
  ) ; end begin build-brew!
) ; end define build-brew!

(define github-api-host "api.github.com")
(define github-api-version "2022-11-28")
(define github-max-request-attempts 5)

(define (read-single-rktd who path)
  (begin
    (assert-nonempty-file who path)
    (call-with-input-file path
      (lambda (in)
        (define datum (read in))
        (define next (read in))
        (unless (eof-object? next)
          (raise-user-error who f"file must contain exactly one Racket datum: {(clean-path-string path)}")
        ) ; end unless exactly one datum
        datum
      ) ; end lambda in
    ) ; end call-with-input-file
  ) ; end begin read-single-rktd
) ; end define read-single-rktd

(define (read-rktd-hash who path)
  (begin
    (define datum (read-single-rktd who path))
    (unless (hash? datum)
      (raise-user-error who f"config must be a hash: {(clean-path-string path)}")
    ) ; end unless hash config
    datum
  ) ; end begin read-rktd-hash
) ; end define read-rktd-hash

(define (resolve-config-path config-path value)
  (complete-path* (if (absolute-path? (string->path value))
                      value
                      (build-path (or (path-only config-path) (current-directory)) value))))

(define (config-required-string who config key)
  (begin
    (define value (hash-ref config key #f))
    (unless (and (string? value) (not (string=? value "")))
      (raise-user-error who f"missing non-empty string config key: {key}")
    ) ; end unless required string
    value
  ) ; end begin config-required-string
) ; end define config-required-string

(define (config-required-boolean who config key)
  (begin
    (define value (hash-ref config key #f))
    (unless (boolean? value)
      (raise-user-error who f"missing boolean config key: {key}")
    ) ; end unless required boolean
    value
  ) ; end begin config-required-boolean
) ; end define config-required-boolean

(define (config-required-list who config key)
  (begin
    (define value (hash-ref config key #f))
    (unless (list? value)
      (raise-user-error who f"missing list config key: {key}")
    ) ; end unless required list
    value
  ) ; end begin config-required-list
) ; end define config-required-list

(define (config-optional-string who config key default)
  (begin
    (define value (hash-ref config key default))
    (unless (and (string? value) (not (string=? value "")))
      (raise-user-error who f"config key must be a non-empty string: {key}")
    ) ; end unless optional string
    value
  ) ; end begin config-optional-string
) ; end define config-optional-string

(define (config-optional-boolean who config key default)
  (begin
    (define value (hash-ref config key default))
    (unless (boolean? value)
      (raise-user-error who f"config key must be boolean: {key}")
    ) ; end unless boolean
    value
  ) ; end begin config-optional-boolean
) ; end define config-optional-boolean

(define (assert-rpm-repo-id value)
  (unless (and (string? value)
               (regexp-match? #px"^[A-Za-z0-9_.:-]+$" value))
    (raise-user-error 'rpm-repo-config
                      f"rpm-repo-id must contain only letters, digits, _, ., :, or -: {value}")
  ) ; end unless valid rpm repo id
  value)

(define (assert-rpm-repo-name value)
  (when (or (string-contains? value "\n")
            (string-contains? value "\r"))
    (raise-user-error 'rpm-repo-config "rpm-repo-name must be a single line")
  ) ; end when newline in rpm repo name
  value)

(define (assert-rpm-repo-baseurl value)
  (begin
    (unless (regexp-match? #px"^(https?|file)://" value)
      (raise-user-error 'rpm-repo-config
                        f"rpm-repo-baseurl must start with https://, http://, or file://: {value}")
    ) ; end unless supported repo baseurl
    (when (or (string-contains? value " ")
              (string-contains? value "\"")
              (string-contains? value "'")
              (string-contains? value "\n")
              (string-contains? value "\r"))
      (raise-user-error 'rpm-repo-config
                        f"rpm-repo-baseurl contains unsafe characters: {value}")
    ) ; end when unsafe repo baseurl
    value
  ) ; end begin assert-rpm-repo-baseurl
) ; end define assert-rpm-repo-baseurl

(define (config-or-cli-boolean who config key cli-value default)
  (if (eq? cli-value 'unset)
      (config-optional-boolean who config key default)
      cli-value))

(define (read-deb-repo-config-values config-path)
  (begin
    (define raw (read-rktd-hash 'deb-repo-config config-path))
    (define root-value
      (config-required-string 'deb-repo-config raw 'deb-repo-root))
    (define root (resolve-config-path config-path root-value))
    (define system
      (assert-deb-system
       (config-required-string 'deb-repo-config raw 'deb-system)))
    (define release
      (assert-deb-release
       (config-required-string 'deb-repo-config raw 'deb-release)))
    (define arch
      (normalize-deb-arch
       (config-required-string 'deb-repo-config raw 'deb-arch)))
    (values root system release arch)
  ) ; end begin read-deb-repo-config-values
) ; end define read-deb-repo-config-values

(define (read-rpm-repo-config-values config-path root-arg id-arg name-arg baseurl-arg
                                     enabled-arg gpgcheck-arg)
  (begin
    (define raw (read-rktd-hash 'rpm-repo-config config-path))
    (define root-value (or root-arg
                           (config-required-string 'rpm-repo-config raw 'rpm-repo-root)))
    (define root (if root-arg
                     (complete-path* root-value)
                     (resolve-config-path config-path root-value)))
    (define id
      (assert-rpm-repo-id
       (or id-arg
           (config-required-string 'rpm-repo-config raw 'rpm-repo-id))))
    (define name
      (assert-rpm-repo-name
       (or name-arg
           (config-required-string 'rpm-repo-config raw 'rpm-repo-name))))
    (define baseurl
      (assert-rpm-repo-baseurl
       (or baseurl-arg
           (config-required-string 'rpm-repo-config raw 'rpm-repo-baseurl))))
    (define enabled?
      (config-or-cli-boolean 'rpm-repo-config raw 'rpm-repo-enabled enabled-arg #t))
    (define gpgcheck?
      (config-or-cli-boolean 'rpm-repo-config raw 'rpm-repo-gpgcheck gpgcheck-arg #f))
    (values root id name baseurl enabled? gpgcheck?)
  ) ; end begin read-rpm-repo-config-values
) ; end define read-rpm-repo-config-values

(define (split-github-repo who repo)
  (match (string-split repo "/")
    [(list owner name)
     (when (or (string=? owner "") (string=? name ""))
       (raise-user-error who f"GitHub repo must be OWNER/REPO: {repo}")
     ) ; end when empty owner/name
     (values owner name)]
    [_ (raise-user-error who f"GitHub repo must be OWNER/REPO: {repo}")]
  ) ; end match repo pieces
) ; end define split-github-repo

(define (safe-secret-preview value)
  (begin
    (define text
      (with-output-to-string
        (lambda ()
          (write value)
        ) ; end lambda write value
      ) ; end with-output-to-string
    ) ; end define text
    (define len (string-length text))
    (if (<= len 8)
        f"stringified length {len}, preview \"{text}\""
        f"stringified length {len}, preview hidden")
  ) ; end begin safe-secret-preview
) ; end define safe-secret-preview

(define (assert-secret-file-mode! path)
  (begin
    (define bits (file-or-directory-permissions path 'bits))
    (when (not (zero? (bitwise-and bits #o077)))
      (raise-user-error 'read-github-token
                        f"token file must not be group/world readable or writable; run chmod 600 {(shell-quote (clean-path-string path))}")
    ) ; end when unsafe secret file permissions
  ) ; end begin assert-secret-file-mode!
) ; end define assert-secret-file-mode!

(define (read-github-token token-file)
  (begin
    (assert-nonempty-file 'read-github-token token-file)
    (assert-secret-file-mode! token-file)
    (define datum
      (with-handlers ([exn:fail?
                       (lambda (exn)
                         (raise-user-error 'read-github-token
                                           f"could not read token file as one Racket datum: {(clean-path-string token-file)}")
                       )])
        (read-single-rktd 'read-github-token token-file)))
    (unless (string? datum)
      (raise-user-error 'read-github-token
                        f"token file must contain a string datum, got non-string value ({(safe-secret-preview datum)}): {(clean-path-string token-file)}")
    ) ; end unless token string
    (define token (string-trim datum))
    (when (string=? token "")
      (raise-user-error 'read-github-token f"token file string is empty: {(clean-path-string token-file)}")
    ) ; end when empty token
    token
  ) ; end begin read-github-token
) ; end define read-github-token

(define (github-request-path parts #:query [query ""])
  (begin
    (define path f"/{(string-join (map uri-encode parts) "/")}")
    (if (string=? query "")
        path
        f"{path}?{query}")
  ) ; end begin github-request-path
) ; end define github-request-path

(define (https-url->host/path who url)
  (match (regexp-match #px"^https://([^/]+)(/.*)?$" url)
    [(list _ host path)
     (values host (if path path "/"))]
    [_ (raise-user-error who f"expected https URL from GitHub API: {url}")]
  ) ; end match url
) ; end define https-url->host/path

(define (github-release-download-url-values who url)
  (match (regexp-match #px"^https://github[.]com/([^/]+)/([^/]+)/releases/download/([^/]+)/([^/?#]+)" url)
    [(list _ owner repo tag asset-name)
     (values owner repo tag asset-name)]
    [_ (raise-user-error who
                         f"expected GitHub release download URL: {url}")]
  ) ; end match GitHub release URL
) ; end define github-release-download-url-values

(define (http-status-code status)
  (begin
    (define pieces (string-split (bytes->string/utf-8 status)))
    (cond
      [(>= (length pieces) 2)
       (or (string->number (second pieces))
           (raise-user-error 'http-status-code f"could not parse HTTP status: {(bytes->string/utf-8 status)}"))]
      [else
       (raise-user-error 'http-status-code f"could not parse HTTP status: {(bytes->string/utf-8 status)}")]
    ) ; end cond status pieces
  ) ; end begin http-status-code
) ; end define http-status-code

(define (safe-response-body body)
  (define trimmed (string-trim body))
  (if (> (string-length trimmed) 1000)
      f"{(substring trimmed 0 1000)}..."
      trimmed))

(define (github-header-value headers name)
  (for/or ([header (in-list headers)])
    (define text (bytes->string/utf-8 header))
    (match (regexp-match #px"^([^:]+):[ \t]*(.*)$" text)
      [(list _ key value)
       (and (string-ci=? key name) value)]
      [_ #f]
    ) ; end match header
  ) ; end for/or header value
) ; end define github-header-value

(define (github-headers token accept)
  (append
   (list "User-Agent: package-racket"
         f"Accept: {accept}"
         f"X-GitHub-Api-Version: {github-api-version}")
   (if (and (string? token)
            (not (string=? token "")))
       (list f"Authorization: Bearer {token}")
       '()))
) ; end define github-headers

(define (strip-http-line-cr line)
  (regexp-replace #rx"\r$" line ""))

(define (github-response-port method url headers data)
  (match method
    ["GET" (get-impure-port url headers)]
    ["DELETE" (delete-impure-port url headers)]
    ["POST"
     (unless data
       (raise-user-error 'github-response-port "POST requires request data")
     ) ; end unless post data
     (post-impure-port url data headers)]
    [_ (raise-user-error 'github-response-port f"unsupported HTTP method: {method}")]
  ) ; end match method
) ; end define github-response-port

(define (read-github-impure-response! who port)
  (begin
    (define raw-status (read-line port 'any))
    (when (eof-object? raw-status)
      (raise-user-error who "HTTP response ended before status line")
    ) ; end when missing status line
    (define status-line (strip-http-line-cr raw-status))
    (define headers
      (let loop ([acc '()])
        (define raw-line (read-line port 'any))
        (cond
          [(eof-object? raw-line)
           (raise-user-error who "HTTP response ended before headers completed")]
          [else
           (define line (strip-http-line-cr raw-line))
           (if (string=? line "")
               (reverse acc)
               (loop (cons (string->bytes/utf-8 line) acc)))]
        ) ; end cond header line
      ) ; end let loop headers
    ) ; end define headers
    (values (string->bytes/utf-8 status-line) headers port)
  ) ; end begin read-github-impure-response!
) ; end define read-github-impure-response!

(define (http-send/port! who method host path headers data)
  (let loop ([attempt 1])
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (cond
                         [(< attempt github-max-request-attempts)
                          (sleep (min attempt 5))
                          (loop (add1 attempt))]
                         [else
                          (raise-user-error who
                                            f"HTTP request failed after {attempt} attempts: {method} https://{host}{path}
{(exn-message exn)}")]
                       ) ; end cond retry
                     )])
      (define url (string->url f"https://{host}{path}"))
      (define response-port (github-response-port method url headers data))
      (read-github-impure-response! who response-port))
  ) ; end let loop http send
) ; end define http-send/port!

(define (github-json-request! who method host path token
                              #:data [data #f]
                              #:content-type [content-type #f]
                              #:ok [ok-statuses '(200)])
  (begin
    (define headers
      (append (github-headers token "application/vnd.github+json")
              (if content-type (list f"Content-Type: {content-type}") '())
              (if data (list f"Content-Length: {(bytes-length data)}") '())))
    (define-values (status header-lines body-port)
      (http-send/port! who method host path headers data)
    ) ; end define-values response
    (define code (http-status-code status))
    (define body (port->string body-port))
    (close-input-port body-port)
    (unless (member code ok-statuses =)
      (raise-user-error who
                        f"GitHub API request failed: {method} https://{host}{path}
status: {(bytes->string/utf-8 status)}
body: {(safe-response-body body)}")
    ) ; end unless ok status
    (if (string=? (string-trim body) "")
        #f
        (string->jsexpr body))
  ) ; end begin github-json-request!
) ; end define github-json-request!

(define (github-download-asset! who owner repo asset-id token dest)
  (begin
    (define api-path
      (github-request-path (list "repos" owner repo "releases" "assets" (number->string asset-id))))
    (let loop ([host github-api-host]
               [path api-path]
               [headers (github-headers token "application/octet-stream")]
               [redirects 0])
      (when (> redirects 5)
        (raise-user-error who "too many redirects while downloading GitHub release asset")
      ) ; end when too many redirects
      (define-values (status header-lines body-port)
        (http-send/port! who "GET" host path headers #f)
      ) ; end define-values download response
      (define code (http-status-code status))
      (cond
        [(= code 200)
         (make-directory* (or (path-only dest) (current-directory)))
         (call-with-output-file dest
           #:exists 'truncate/replace
           (lambda (out)
             (copy-port body-port out)
           ) ; end lambda copy download
         ) ; end call-with-output-file dest
         (close-input-port body-port)]
        [(member code '(301 302 303 307 308) =)
         (define location (github-header-value header-lines "Location"))
         (close-input-port body-port)
         (unless location
           (raise-user-error who f"GitHub redirect missing Location header: {code}")
         ) ; end unless location
         (define-values (next-host next-path)
           (https-url->host/path who location)
         ) ; end define-values next url
         (loop next-host next-path (list "User-Agent: package-racket") (add1 redirects))]
        [else
         (define body (port->string body-port))
         (close-input-port body-port)
         (raise-user-error who
                           f"GitHub asset download failed: {code}
body: {(safe-response-body body)}")]
      ) ; end cond download response
    ) ; end let loop
  ) ; end begin github-download-asset!
) ; end define github-download-asset!

(define (github-release-by-tag! owner repo tag token)
  (github-json-request!
   'github-release-by-tag!
   "GET"
   github-api-host
   (github-request-path (list "repos" owner repo "releases" "tags" tag))
   token))

(define (github-release-assets! owner repo release-id token)
  (github-json-request!
   'github-release-assets!
   "GET"
   github-api-host
   (github-request-path (list "repos" owner repo "releases" (number->string release-id) "assets")
                        #:query "per_page=100")
   token))

(define (github-upload-asset! upload-url asset-name asset-path token
                              #:content-type [content-type "application/octet-stream"])
  (begin
    (define clean-upload-url (regexp-replace #rx"\\{.*\\}$" upload-url ""))
    (define-values (host path)
      (https-url->host/path 'github-upload-asset! f"{clean-upload-url}?name={(uri-encode asset-name)}")
    ) ; end define-values upload endpoint
    (define data (file->bytes asset-path))
    (github-json-request!
     'github-upload-asset!
     "POST"
     host
     path
     token
     #:data data
     #:content-type content-type
     #:ok '(201))
  ) ; end begin github-upload-asset!
) ; end define github-upload-asset!

(define (github-delete-asset! owner repo asset-id token)
  (github-json-request!
   'github-delete-asset!
   "DELETE"
   github-api-host
   (github-request-path (list "repos" owner repo "releases" "assets" (number->string asset-id)))
   token
   #:ok '(204)))

(define (release-asset-by-name assets asset-name)
  (begin
    (define matches
      (filter (lambda (asset)
                (string=? (hash-ref asset 'name "") asset-name))
              assets))
    (when (> (length matches) 1)
      (raise-user-error 'release-asset-by-name
                        f"release has multiple assets named {asset-name}; refusing to continue")
    ) ; end when duplicate assets
    (and (pair? matches) (car matches))
  ) ; end begin release-asset-by-name
) ; end define release-asset-by-name

(define (github-asset-digest-sha256 digest)
  (and (string? digest)
       (match (regexp-match #px"^sha256:([0-9A-Fa-f]{64})$" digest)
         [(list _ sha) (string-downcase sha)]
         [_ #f]))
) ; end define github-asset-digest-sha256

(define (github-release-asset-sha256/digest who owner repo tag asset-name)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (println/flush
                      f"Could not read GitHub release asset digest; will download Source0 to calculate sha256: {(exn-message exn)}")
                     #f)])
    (define release (github-release-by-tag! owner repo tag ""))
    (define release-id (hash-ref release 'id))
    (define assets (github-release-assets! owner repo release-id ""))
    (define asset (release-asset-by-name assets asset-name))
    (and asset
         (github-asset-digest-sha256 (hash-ref asset 'digest #f)))
  ) ; end with-handlers GitHub release digest
) ; end define github-release-asset-sha256/digest

(define (download-https-url! who url dest)
  (begin
    (define-values (initial-host initial-path)
      (https-url->host/path who url)
    ) ; end define-values initial URL parts
    (let loop ([host initial-host]
               [path initial-path]
               [redirects 0])
      (when (> redirects 5)
        (raise-user-error who f"too many redirects while downloading: {url}")
      ) ; end when too many redirects
      (define-values (status header-lines body-port)
        (http-send/port! who "GET" host path (list "User-Agent: package-racket") #f)
      ) ; end define-values download response
      (define code (http-status-code status))
      (cond
        [(= code 200)
         (make-directory* (or (path-only dest) (current-directory)))
         (call-with-output-file dest
           #:exists 'truncate/replace
           (lambda (out)
             (copy-port body-port out)
           ) ; end lambda copy URL body
         ) ; end call-with-output-file dest
         (close-input-port body-port)]
        [(member code '(301 302 303 307 308) =)
         (define location (github-header-value header-lines "Location"))
         (close-input-port body-port)
         (unless location
           (raise-user-error who f"HTTP redirect missing Location header: {code}")
         ) ; end unless location header
         (cond
           [(string-prefix? location "https://")
            (define-values (next-host next-path)
              (https-url->host/path who location)
            ) ; end define-values absolute redirect
            (loop next-host next-path (add1 redirects))]
           [(string-prefix? location "/")
            (loop host location (add1 redirects))]
           [else
            (raise-user-error who f"unsupported redirect Location: {location}")]
         ) ; end cond redirect location
        ]
        [else
         (define body (port->string body-port))
         (close-input-port body-port)
         (raise-user-error who
                           f"download failed: {code} https://{host}{path}
body: {(safe-response-body body)}")]
      ) ; end cond response status
    ) ; end let loop
  ) ; end begin download-https-url!
) ; end define download-https-url!

(define (resolve-rpm-source-sha256! c source-url)
  (begin
    (define local-source (brew-output-tgz c))
    (cond
      [(file-exists? local-source)
       (define sha (sha256-file local-source))
       (println/flush f"RPM Source0 sha256 from local artifact: {sha}")
       sha]
      [else
       (define-values (owner repo tag asset-name)
         (github-release-download-url-values 'resolve-rpm-source-sha256! source-url)
       ) ; end define-values release URL parts
       (define remote-digest
         (github-release-asset-sha256/digest 'resolve-rpm-source-sha256!
                                             owner
                                             repo
                                             tag
                                             asset-name)
       ) ; end define remote digest
       (cond
         [remote-digest
          (println/flush f"RPM Source0 sha256 from GitHub release digest: {remote-digest}")
          remote-digest]
         [else
          (define source-dir (build-path (cfg-work-dir c) "rpm-source"))
          (define downloaded-source (build-path source-dir asset-name))
          (reset-managed-dir! 'resolve-rpm-source-sha256! source-dir)
          (println/flush f"Downloading RPM Source0 for sha256: {source-url}")
          (download-https-url! 'resolve-rpm-source-sha256! source-url downloaded-source)
          (assert-nonempty-file 'resolve-rpm-source-sha256! downloaded-source)
          (define sha (sha256-file downloaded-source))
          (println/flush f"RPM Source0 sha256 from downloaded artifact: {sha}")
          sha]
       ) ; end cond remote digest
      ]
    ) ; end cond local or remote source
  ) ; end begin resolve-rpm-source-sha256!
) ; end define resolve-rpm-source-sha256!

(define (release-upload-config c who config-path repo-arg tag-arg asset-arg token-file-arg
                               repo-key tag-key asset-key token-key
                               default-tag default-asset-name)
  (begin
    (define raw (read-rktd-hash who config-path))
    (define repo (or repo-arg
                     (config-required-string who raw repo-key)))
    (define tag (or tag-arg
                    (config-optional-string who raw tag-key default-tag)))
    (define asset-name (or asset-arg
                           (config-optional-string who raw asset-key default-asset-name)))
    (define token-file-value (or token-file-arg
                                 (config-optional-string who raw token-key "secret/ghtoken.rktd")))
    (define token-file (resolve-config-path config-path token-file-value))
    (define replace? (if (eq? (cfg-replace-release-asset c) 'unset)
                         (config-optional-boolean who raw 'replace-release-asset #f)
                         (cfg-replace-release-asset c)))
    (hash 'repo repo
          'tag tag
          'asset-name asset-name
          'token-file token-file
          'replace? replace?
          'config-path config-path)
  ) ; end begin release-upload-config
) ; end define release-upload-config

(define (source-release-config c)
  (release-upload-config c
                         'source-release-config
                         (cfg-source-release-config c)
                         (cfg-source-release-repo c)
                         (cfg-source-release-tag c)
                         (cfg-source-release-asset c)
                         (cfg-source-release-token-file c)
                         'source-release-repo
                         'source-release-tag
                         'source-release-asset
                         'source-release-token-file
                         (source-release-default-tag c)
                         (brew-source-tgz-name c)))

(define (apt-release-config c)
  (release-upload-config c
                         'apt-release-config
                         (cfg-apt-release-config c)
                         (cfg-apt-release-repo c)
                         (cfg-apt-release-tag c)
                         (cfg-apt-release-asset c)
                         (cfg-apt-release-token-file c)
                         'apt-release-repo
                         'apt-release-tag
                         'apt-release-asset
                         'apt-release-token-file
                         (apt-release-default-tag c)
                         (apt-deb-name c)))

(define (validate-source-release-artifact! c asset-name)
  (begin
    (define asset-path (build-path (cfg-artifact-dir c) asset-name))
    (assert-nonempty-file 'validate-source-release-artifact! asset-path)
    (unless (equal? (file-name-from-path asset-path) (string->path asset-name))
      (raise-user-error 'validate-source-release-artifact!
                        f"source release asset path basename does not match config asset name: {(clean-path-string asset-path)}")
    ) ; end unless asset basename
    asset-path
  ) ; end begin validate-source-release-artifact!
) ; end define validate-source-release-artifact!

(define (validate-apt-release-artifact! c asset-name)
  (begin
    (define asset-path (build-path (cfg-artifact-dir c) asset-name))
    (assert-nonempty-file 'validate-apt-release-artifact! asset-path)
    (unless (regexp-match? #rx"[.]deb$" asset-name)
      (raise-user-error 'validate-apt-release-artifact!
                        f"apt release asset must end with .deb: {asset-name}")
    ) ; end unless deb asset name
    (unless (equal? (file-name-from-path asset-path) (string->path asset-name))
      (raise-user-error 'validate-apt-release-artifact!
                        f"apt release asset path basename does not match config asset name: {(clean-path-string asset-path)}")
    ) ; end unless asset basename
    (validate-deb! c asset-path)
    asset-path
  ) ; end begin validate-apt-release-artifact!
) ; end define validate-apt-release-artifact!

(define (assert-release-asset-matches-producer! who producer-name expected-name asset-name)
  (unless (string=? asset-name expected-name)
    (raise-user-error who
                      f"{asset-name} does not match {producer-name} output {expected-name}; refusing to upload a stale or unrelated release asset")
  ) ; end unless asset matches producer
) ; end define assert-release-asset-matches-producer!

(define (verify-uploaded-asset! owner repo token asset-id expected-sha)
  (begin
    (define tmp (make-temporary-file "package-racket-release-asset~a"))
    (github-download-asset! 'verify-uploaded-asset! owner repo asset-id token tmp)
    (define actual-sha (sha256-file tmp))
    (delete-file tmp)
    (unless (string=? actual-sha expected-sha)
      (raise-user-error 'verify-uploaded-asset!
                        f"uploaded asset sha256 {actual-sha} does not match local sha256 {expected-sha}")
    ) ; end unless uploaded sha matches
  ) ; end begin verify-uploaded-asset!
) ; end define verify-uploaded-asset!

(define (release-asset-current? existing local-sha)
  (and existing
       (string=? (hash-ref existing 'digest "")
                 f"sha256:{local-sha}")))

(define (verify-uploaded-asset-digest! owner repo-name token uploaded expected-sha)
  (begin
    (define uploaded-id (hash-ref uploaded 'id))
    (define uploaded-digest (hash-ref uploaded 'digest #f))
    (cond
      [(and (string? uploaded-digest)
            (string=? uploaded-digest f"sha256:{expected-sha}"))
       (println/flush f"Uploaded asset digest matches local sha256: {uploaded-digest}")]
      [(string? uploaded-digest)
       (raise-user-error 'verify-uploaded-asset!
                         f"uploaded asset digest {uploaded-digest} does not match local sha256 {expected-sha}")]
      [else
       (verify-uploaded-asset! owner repo-name token uploaded-id expected-sha)]
    ) ; end cond digest
  ) ; end begin verify-uploaded-asset-digest!
) ; end define verify-uploaded-asset-digest!

(define (upload-github-release-asset-real! who owner repo-name tag asset-name asset-path local-sha
                                           token-file replace? content-type)
  (begin
    (define token (read-github-token token-file))
    (define release (github-release-by-tag! owner repo-name tag token))
    (define release-id (hash-ref release 'id))
    (define upload-url (hash-ref release 'upload_url))
    (define assets (github-release-assets! owner repo-name release-id token))
    (define existing (release-asset-by-name assets asset-name))
    (define existing-current? (release-asset-current? existing local-sha))
    (when existing
      (define existing-id (hash-ref existing 'id))
      (define existing-digest (hash-ref existing 'digest #f))
      (cond
        [(and (string? existing-digest)
              (string=? existing-digest f"sha256:{local-sha}"))
         (println/flush f"Release asset already matches local sha256: {asset-name}")]
        [replace?
         (println/flush f"Deleting existing release asset before upload: {asset-name}")
         (github-delete-asset! owner repo-name existing-id token)]
        [else
         (raise-user-error who
                           f"release asset already exists and differs; set replace-release-asset to #t: {asset-name}")]
      ) ; end cond existing asset
    ) ; end when existing asset
    (unless existing-current?
      (println/flush f"Uploading GitHub release asset: {asset-name}")
      (let ([uploaded (github-upload-asset! upload-url asset-name asset-path token
                                            #:content-type content-type)])
        (verify-uploaded-asset-digest! owner repo-name token uploaded local-sha)
        (println/flush f"Uploaded and verified GitHub release asset: {asset-name}")
      ) ; end let uploaded asset
    ) ; end unless existing current
  ) ; end begin upload-github-release-asset-real!
) ; end define upload-github-release-asset-real!

(define (upload-source-release-real! owner repo-name tag asset-name asset-path local-sha token-file replace?)
  (upload-github-release-asset-real! 'upload-source-release!
                                     owner
                                     repo-name
                                     tag
                                     asset-name
                                     asset-path
                                     local-sha
                                     token-file
                                     replace?
                                     "application/gzip"))

(define (upload-source-release! c)
  (begin
    (define config (source-release-config c))
    (define repo (hash-ref config 'repo))
    (define tag (hash-ref config 'tag))
    (define asset-name (hash-ref config 'asset-name))
    (define token-file (hash-ref config 'token-file))
    (define replace? (hash-ref config 'replace?))
    (define produced-by-brew? (target-selected? c "brew"))
    (when produced-by-brew?
      (assert-release-asset-matches-producer! 'upload-source-release!
                                              "brew"
                                              (brew-source-tgz-name c)
                                              asset-name)
    ) ; end when produced by brew
    (define asset-path (build-path (cfg-artifact-dir c) asset-name))
    (define dry-run-planned? (and (cfg-dry-run? c) produced-by-brew?))
    (unless dry-run-planned?
      (validate-source-release-artifact! c asset-name)
    ) ; end unless dry-run planned source release artifact
    (define local-sha (if dry-run-planned?
                          "<dry-run: artifact not built>"
                          (sha256-file asset-path)))
    (define-values (owner repo-name)
      (split-github-repo 'upload-source-release! repo)
    ) ; end define-values owner/repo
    (println/flush f"Source release repo: {repo}")
    (println/flush f"Source release tag: {tag}")
    (println/flush f"Source release asset: {asset-name}")
    (println/flush f"Source release sha256: {local-sha}")
    (if (cfg-dry-run? c)
        (println/flush
         (if dry-run-planned?
             f"Would upload source release asset from planned brew output {(clean-path-string asset-path)}"
             f"Would upload source release asset from {(clean-path-string asset-path)}"))
        (upload-source-release-real! owner repo-name tag asset-name asset-path local-sha token-file replace?)
    ) ; end if dry-run
  ) ; end begin upload-source-release!
) ; end define upload-source-release!

(define (build-source-release! c)
  (list (lambda ()
          (upload-source-release! c)
        ) ; end lambda source release upload
  ) ; end list source release finalizer
) ; end define build-source-release!

(define (upload-apt-release! c)
  (begin
    (define config (apt-release-config c))
    (define repo (hash-ref config 'repo))
    (define tag (hash-ref config 'tag))
    (define asset-name (hash-ref config 'asset-name))
    (define token-file (hash-ref config 'token-file))
    (define replace? (hash-ref config 'replace?))
    (define produced-by-apt? (target-selected? c "apt"))
    (when produced-by-apt?
      (assert-release-asset-matches-producer! 'upload-apt-release!
                                              "apt"
                                              (apt-deb-name c)
                                              asset-name)
    ) ; end when produced by apt
    (define asset-path (build-path (cfg-artifact-dir c) asset-name))
    (define dry-run-planned? (and (cfg-dry-run? c) produced-by-apt?))
    (unless dry-run-planned?
      (validate-apt-release-artifact! c asset-name)
    ) ; end unless dry-run planned apt release artifact
    (define local-sha (if dry-run-planned?
                          "<dry-run: artifact not built>"
                          (sha256-file asset-path)))
    (define-values (owner repo-name)
      (split-github-repo 'upload-apt-release! repo)
    ) ; end define-values owner/repo
    (println/flush f"APT release repo: {repo}")
    (println/flush f"APT release tag: {tag}")
    (println/flush f"APT release asset: {asset-name}")
    (println/flush f"APT release sha256: {local-sha}")
    (if (cfg-dry-run? c)
        (println/flush
         (if dry-run-planned?
             f"Would upload apt release asset from planned apt output {(clean-path-string asset-path)}"
             f"Would upload apt release asset from {(clean-path-string asset-path)}"))
        (upload-github-release-asset-real! 'upload-apt-release!
                                           owner
                                           repo-name
                                           tag
                                           asset-name
                                           asset-path
                                           local-sha
                                           token-file
                                           replace?
                                           "application/vnd.debian.binary-package")
    ) ; end if dry-run
  ) ; end begin upload-apt-release!
) ; end define upload-apt-release!

(define (build-apt-release! c)
  (list (lambda ()
          (upload-apt-release! c)
        ) ; end lambda apt release upload
  ) ; end list apt release finalizer
) ; end define build-apt-release!

(define (brew-ci-work-root c)
  (build-path (cfg-work-dir c) "brew-ci"))

(define (brew-ci-workflows-dir c)
  (build-path (brew-ci-work-root c) ".github" "workflows"))

(define (tap-workflows-dir c)
  (build-path (cfg-homebrew-tap c) ".github" "workflows"))

(define (workflow-path dir name)
  (build-path dir name))

(define (read-brew-ci-config c)
  (begin
    (define path (cfg-brew-ci-config c))
    (assert-nonempty-file 'read-brew-ci-config path)
    (define value
      (call-with-input-file path read)
    ) ; end define value
    (unless (hash? value)
      (raise-user-error 'read-brew-ci-config f"config must be a hash: {(clean-path-string path)}")
    ) ; end unless hash config
    value
  ) ; end begin read-brew-ci-config
) ; end define read-brew-ci-config

(define (config-ref* config key default)
  (hash-ref config key (lambda () default)))

(define (required-config-string config key)
  (begin
    (define value (hash-ref config key #f))
    (unless (and (string? value) (not (string=? value "")))
      (raise-user-error 'required-config-string f"missing string config key: {key}")
    ) ; end unless valid string
    value
  ) ; end begin required-config-string
) ; end define required-config-string

(define (required-config-positive-integer config key)
  (begin
    (define value (hash-ref config key #f))
    (unless (exact-positive-integer? value)
      (raise-user-error 'required-config-positive-integer f"missing positive integer config key: {key}")
    ) ; end unless valid positive integer
    value
  ) ; end begin required-config-positive-integer
) ; end define required-config-positive-integer

(define (runner-ref runner key default)
  (hash-ref runner key (lambda () default)))

(define (assert-runner-list who value)
  (unless (and (list? value) (andmap hash? value))
    (raise-user-error who "runner list must be a list of hash values")
  ) ; end unless runner list
) ; end define assert-runner-list

(define (validate-brew-ci-config! config)
  (begin
    (required-config-string config 'formula)
    (required-config-string config 'artifact-prefix)
    (required-config-positive-integer config 'bottle-rebuild)
    (define bottle-runners (config-ref* config 'bottle-runners '()))
    (define syntax-runners (config-ref* config 'syntax-runners '()))
    (assert-runner-list 'validate-brew-ci-config! bottle-runners)
    (assert-runner-list 'validate-brew-ci-config! syntax-runners)
    (when (null? bottle-runners)
      (raise-user-error 'validate-brew-ci-config! "at least one bottle runner is required")
    ) ; end when no bottle runners
    (for ([runner (in-list (append bottle-runners syntax-runners))])
      (define os (runner-ref runner 'os #f))
      (unless (and (string? os) (not (string=? os "")))
        (raise-user-error 'validate-brew-ci-config! "each runner requires a non-empty os string")
      ) ; end unless runner os
    ) ; end for runner
  ) ; end begin validate-brew-ci-config!
) ; end define validate-brew-ci-config!

(define (workflow-runner-lines runner test-formula?)
  (begin
    (define os (runner-ref runner 'os #f))
    (define container (runner-ref runner 'container #f))
    (string-append
     f"          - os: {os}
            test_formula: {(if test-formula? "true" "false")}
"
     (if container
         f"            container: {container}
"
         "")
    ) ; end string-append runner lines
  ) ; end begin workflow-runner-lines
) ; end define workflow-runner-lines

(define (workflow-bottle-runner-lines runner)
  (begin
    (define os (runner-ref runner 'os #f))
    (define container (runner-ref runner 'container #f))
    (string-append
     f"          - os: {os}
"
     (if container
         f"            container: {container}
"
         "")
    ) ; end string-append bottle runner lines
  ) ; end begin workflow-bottle-runner-lines
) ; end define workflow-bottle-runner-lines

(define (tests-workflow-content c config)
  (begin
    (define formula (required-config-string config 'formula))
    (define artifact-prefix (required-config-string config 'artifact-prefix))
    (define bottle-runners (config-ref* config 'bottle-runners '()))
	    (define syntax-runners (config-ref* config 'syntax-runners '()))
	    (define root-url (cfg-bottle-root-url c))
	    (define tap-name (homebrew-tap-name (cfg-homebrew-tap c)))
	    (define matrix-os "${{ matrix.os }}")
    (define container-expr "${{ matrix.container }}")
    (define token-expr "${{ secrets.GITHUB_TOKEN }}")
    (define test-formula-if "matrix.test_formula")
    (define runner-lines
      (string-append
       (apply string-append (map (lambda (runner) (workflow-runner-lines runner #t)) bottle-runners))
       (apply string-append (map (lambda (runner) (workflow-runner-lines runner #f)) syntax-runners))
      ) ; end string-append runner lines
    ) ; end define runner-lines
    f"{(generated-code-notice "#")}name: brew test-bot

on:
  pull_request:

jobs:
  test-bot:
    strategy:
      fail-fast: false
      matrix:
        include:
{runner-lines}    runs-on: {matrix-os}
    container: {container-expr}
    permissions:
      actions: read
      checks: read
      contents: read
      pull-requests: read
    steps:
      - name: Set up Homebrew
        uses: Homebrew/actions/setup-homebrew@main
        with:
          token: {token-expr}

      - run: brew test-bot --only-cleanup-before

      - run: brew test-bot --only-setup

      - run: brew test-bot --only-tap-syntax --tap={tap-name}

      - run: brew test-bot --only-formulae --testing-formulae={formula} --skip-dependents --root-url={root-url}
        if: {test-formula-if}

      - name: Upload bottles as artifact
        if: {test-formula-if}
        uses: actions/upload-artifact@v6
        with:
          name: {artifact-prefix}_{matrix-os}
          path: |
            *.bottle.json
            *.bottle*.tar.gz
            **/*.bottle.json
            **/*.bottle*.tar.gz
          if-no-files-found: error
"
  ) ; end begin tests-workflow-content
) ; end define tests-workflow-content

(define (publish-workflow-content c config)
  (begin
    (define formula (required-config-string config 'formula))
    (define artifact-prefix (required-config-string config 'artifact-prefix))
    (define bottle-rebuild (required-config-positive-integer config 'bottle-rebuild))
	    (define bottle-runners (config-ref* config 'bottle-runners '()))
	    (define root-url (cfg-bottle-root-url c))
	    (define tap-name (homebrew-tap-name (cfg-homebrew-tap c)))
	    (define release-tag (github-release-tag-from-root-url 'publish-workflow-content root-url))
    (define release-name f"Racket {(cfg-source-version c)} Homebrew bottles")
    (define matrix-os "${{ matrix.os }}")
    (define container-expr "${{ matrix.container }}")
    (define token-expr "${{ secrets.GITHUB_TOKEN }}")
    (define github-repository-expr "${{ github.repository }}")
    (define skip-bottles-if "github.event_name != 'push' || contains(github.event.head_commit.message, '[skip bottles]') == false")
    (define runner-lines
      (apply string-append (map workflow-bottle-runner-lines bottle-runners))
    ) ; end define runner-lines
    (define bottle-json-count "${#bottle_jsons[@]}")
    (define bottle-tarball-count "${#bottle_tarballs[@]}")
    (define bottle-json-array "\"${bottle_jsons[@]}\"")
    (define normalized-bottle-json-array "\"${normalized_bottle_jsons[@]}\"")
    (define bottle-tarball-array "\"${bottle_tarballs[@]}\"")
    f"{(generated-code-notice "#")}name: brew publish bottles

on:
  push:
    branches:
      - main
  workflow_dispatch:

jobs:
  build-bottles:
    if: {skip-bottles-if}
    strategy:
      fail-fast: false
      matrix:
        include:
{runner-lines}    runs-on: {matrix-os}
    container: {container-expr}
    permissions:
      actions: read
      contents: read
    steps:
      - name: Set up Homebrew
        uses: Homebrew/actions/setup-homebrew@main
        with:
          token: {token-expr}

      - run: brew test-bot --only-cleanup-before

      - run: brew test-bot --only-setup

      - run: brew test-bot --only-tap-syntax --tap={tap-name}

      - run: brew test-bot --only-formulae --testing-formulae={formula} --skip-dependents --root-url={root-url}

      - name: List bottle files
        shell: bash
        run: |
          set -euo pipefail
          pwd
          find . -maxdepth 3 \\( -name '*.bottle.json' -o -name '*.bottle*.tar.gz' \\) -type f -print | sort

      - name: Upload bottles as artifact
        uses: actions/upload-artifact@v6
        with:
          name: {artifact-prefix}_{matrix-os}
          path: |
            *.bottle.json
            *.bottle*.tar.gz
            **/*.bottle.json
            **/*.bottle*.tar.gz
          if-no-files-found: error

  publish-bottles:
    needs: build-bottles
    if: {skip-bottles-if}
    runs-on: ubuntu-latest
    permissions:
      actions: read
      contents: write
    steps:
      - name: Set up Homebrew
        uses: Homebrew/actions/setup-homebrew@main
        with:
          token: {token-expr}

      - name: Set up git
        uses: Homebrew/actions/git-user-config@main

      - name: Download bottle artifacts
        uses: actions/download-artifact@v6
        with:
          pattern: {artifact-prefix}_*
          path: bottles
          merge-multiple: true

      - name: Publish bottles and update Formula
        shell: bash
        env:
          GH_TOKEN: {token-expr}
          GH_REPO: {github-repository-expr}
          BOTTLE_ROOT_URL: {root-url}
          RELEASE_TAG: {release-tag}
          RELEASE_NAME: {release-name}
          BOTTLE_REBUILD: {bottle-rebuild}
        run: |
          set -euo pipefail

          mapfile -t bottle_jsons < <(find \"$GITHUB_WORKSPACE/bottles\" -name '*.bottle.json' -type f | sort)
          mapfile -t bottle_tarballs < <(find \"$GITHUB_WORKSPACE/bottles\" -name '*.bottle*.tar.gz' -type f | sort)

          if [ \"{bottle-json-count}\" -eq 0 ]; then
            echo \"No bottle JSON files were produced.\"
            exit 1
          fi

          if [ \"{bottle-tarball-count}\" -eq 0 ]; then
            echo \"No bottle tarballs were produced.\"
            exit 1
          fi

          if [ \"{bottle-json-count}\" -ne \"{bottle-tarball-count}\" ]; then
            echo \"Bottle JSON count does not match bottle tarball count.\"
            printf 'Bottle JSON files:\\n'
            printf '  %s\\n' {bottle-json-array}
            printf 'Bottle tarballs:\\n'
            printf '  %s\\n' {bottle-tarball-array}
            exit 1
          fi

          if ! gh release view \"$RELEASE_TAG\" >/dev/null 2>&1; then
            gh release create \"$RELEASE_TAG\" --title \"$RELEASE_NAME\" --notes \"Generated Homebrew bottle artifacts for {formula} {(cfg-formula-version c)}.\"
          fi

          release_asset_dir=\"${{RUNNER_TEMP:-$GITHUB_WORKSPACE}}/bottle-release-assets\"
          normalized_json_dir=\"${{RUNNER_TEMP:-$GITHUB_WORKSPACE}}/normalized-bottle-json\"
          rm -rf \"$release_asset_dir\"
          rm -rf \"$normalized_json_dir\"
          mkdir -p \"$release_asset_dir\"
          mkdir -p \"$normalized_json_dir\"
          release_assets=()
          normalized_bottle_jsons=()
          declare -A tarballs_by_basename

          normalize_bottle_json() {{
            ruby -rjson -e '
              desired = Integer(ENV.fetch(\"BOTTLE_REBUILD\"), 10)
              abort \"BOTTLE_REBUILD must be positive\" unless desired.positive?

              input_path = ARGV.fetch(0)
              output_path = ARGV.fetch(1)
              data = JSON.parse(File.read(input_path))

              data.each_value do |formula|
                formula.fetch(\"bottle\")[\"rebuild\"] = desired
              end

              File.write(output_path, JSON.pretty_generate(data))
            ' \"$1\" \"$2\"
          }}

          for bottle_tarball in \"${{bottle_tarballs[@]}}\"; do
            bottle_basename=\"${{bottle_tarball##*/}}\"

            if [ -n \"${{tarballs_by_basename[$bottle_basename]:-}}\" ]; then
              echo \"Duplicate bottle tarball basename: $bottle_basename\"
              exit 1
            fi

            tarballs_by_basename[\"$bottle_basename\"]=\"$bottle_tarball\"
          done

          for bottle_json in \"${{bottle_jsons[@]}}\"; do
            normalized_json=\"$normalized_json_dir/${{bottle_json##*/}}\"
            normalize_bottle_json \"$bottle_json\" \"$normalized_json\"
            normalized_bottle_jsons+=(\"$normalized_json\")
          done

          extract_bottle_metadata() {{
            ruby -rjson -ruri -e '
              desired = ENV.fetch(\"BOTTLE_REBUILD\")

              def normalize_bottle_asset_name(name, desired)
                replacement = \".bottle.\" + desired + \".tar.gz\"
                unless name.match?(/\\.bottle(?:\\.\\d+)?\\.tar\\.gz\\z/)
                  abort \"not a Homebrew bottle tarball name: \" + name
                end

                name.sub(/\\.bottle(?:\\.\\d+)?\\.tar\\.gz\\z/, replacement)
              end

              path = ARGV.fetch(0)
              JSON.parse(File.read(path)).each_value do |formula|
                formula.fetch(\"bottle\").fetch(\"tags\").each_value do |tag|
                  local_filename = tag.fetch(\"local_filename\")
                  url_filename = tag.fetch(\"filename\")
                  sha256 = tag.fetch(\"sha256\")
                  release_asset_name =
                    normalize_bottle_asset_name(URI.decode_www_form_component(url_filename), desired)

                  if local_filename.empty? || release_asset_name.empty? || sha256.empty?
                    abort \"bottle JSON contains empty filename or sha256: #{{path}}\"
                  end

                  puts [local_filename, release_asset_name, sha256].join(\"\\t\")
                end
              end
            ' \"$1\"
          }}

          for bottle_json in \"${{normalized_bottle_jsons[@]}}\"; do
            while IFS=$'\\t' read -r local_filename release_asset_name expected_sha256; do
              case \"$local_filename\" in
                \"\"|*/*)
                  echo \"Invalid bottle local_filename in metadata: $local_filename\"
                  exit 1
                  ;;
              esac

              case \"$release_asset_name\" in
                \"\"|*/*)
                  echo \"Invalid bottle release asset filename in metadata: $release_asset_name\"
                  exit 1
                  ;;
              esac

              bottle_tarball=\"${{tarballs_by_basename[$local_filename]:-}}\"
              if [ -z \"$bottle_tarball\" ]; then
                echo \"Bottle metadata references missing local tarball: $local_filename\"
                exit 1
              fi

              actual_sha256=\"$(ruby -rdigest -e 'puts Digest::SHA256.file(ARGV.fetch(0)).hexdigest' \"$bottle_tarball\")\"
              if [ \"$actual_sha256\" != \"$expected_sha256\" ]; then
                echo \"Bottle tarball sha256 does not match metadata: $local_filename\"
                exit 1
              fi

              release_asset_path=\"$release_asset_dir/$release_asset_name\"
              if [ -e \"$release_asset_path\" ]; then
                echo \"Duplicate release asset filename from Homebrew metadata: $release_asset_name\"
                exit 1
              fi

              cp \"$bottle_tarball\" \"$release_asset_path\"
              release_assets+=(\"$release_asset_path\")
              printf 'Release asset: %s -> %s\\n' \"$local_filename\" \"$release_asset_name\"
            done < <(extract_bottle_metadata \"$bottle_json\")
          done

          if [ \"${{#release_assets[@]}}\" -ne \"{bottle-tarball-count}\" ]; then
            echo \"Release asset count does not match bottle tarball count.\"
            exit 1
          fi

          cd \"$(brew --repository \"$GITHUB_REPOSITORY\")\"
          brew bottle --merge --write --no-commit --root-url=\"$BOTTLE_ROOT_URL\" {normalized-bottle-json-array}
          ruby <<'RUBY'
            path = \"Formula/{formula}.rb\"
            desired = ENV.fetch(\"BOTTLE_REBUILD\")
            rebuild_line = File.readlines(path).find do |line|
              line.match?(/^\\s*rebuild\\s+/)
            end

            unless rebuild_line && rebuild_line.split.fetch(1, nil) == desired
              abort \"Formula bottle rebuild did not stay at \" + desired
            end
          RUBY

          gh release upload \"$RELEASE_TAG\" \"${{release_assets[@]}}\" --clobber

          if git diff --quiet -- Formula/racket@9.rb; then
            echo \"Formula bottle block is already current.\"
            exit 0
          fi

          git add Formula/racket@9.rb
          git commit -F - <<'COMMIT_MESSAGE'
          (BUILD \"Update racket@9 bottles\"

          ((CI \"brew publish bottles\" update-bottle-metadata)
           \"[skip bottles]\"
           \"[skip ci]\")
          \"Update Homebrew bottle metadata.\"
          )
          COMMIT_MESSAGE
          git push origin HEAD:main
"
  ) ; end begin publish-workflow-content
) ; end define publish-workflow-content

(define (validate-yaml! c path)
  (begin
    (assert-nonempty-file 'validate-yaml! path)
    (capture! 'validate-yaml!
              (cfg-ruby-bin c)
              (list "-e" "require 'yaml'; ARGV.each { |path| YAML.load_file(path) }" (clean-path-string path)))
    (void)
  ) ; end begin validate-yaml!
) ; end define validate-yaml!

(define (validate-tests-workflow! c config path)
  (begin
    (validate-yaml! c path)
    (define content (file->string path))
    (define formula (required-config-string config 'formula))
    (for ([needle (in-list (list "name: brew test-bot"
                                 generated-code-notice-marker
                                 "pull_request:"
                                 f"--testing-formulae={formula}"
                                 f"--root-url={(cfg-bottle-root-url c)}"
                                 "test_formula: true"
                                 "if: matrix.test_formula"
                                 "actions/upload-artifact@v6"
                                 "*.bottle*.tar.gz"
                                 "if-no-files-found: error"))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-tests-workflow! f"tests workflow missing: {needle}")
      ) ; end unless tests workflow needle
    ) ; end for needle
  ) ; end begin validate-tests-workflow!
) ; end define validate-tests-workflow!

(define (validate-publish-workflow! c config path)
  (begin
    (validate-yaml! c path)
    (define content (file->string path))
    (define formula (required-config-string config 'formula))
    (for ([needle (in-list (list "name: brew publish bottles"
                                 generated-code-notice-marker
                                 "push:"
                                 "workflow_dispatch:"
                                 "build-bottles:"
                                 "publish-bottles:"
                                 "BOTTLE_REBUILD:"
                                 "normalize_bottle_json"
                                 "normalized_bottle_jsons"
                                 f"--testing-formulae={formula}"
                                 f"--root-url={(cfg-bottle-root-url c)}"
                                 "actions/download-artifact@v6"
                                 "GH_REPO:"
                                 "gh release upload"
                                 "brew bottle --merge --write --no-commit"
                                 "[skip bottles]"
                                 "[skip ci]"
                                 "contents: write"))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-publish-workflow! f"publish workflow missing: {needle}")
      ) ; end unless publish workflow needle
    ) ; end for needle
  ) ; end begin validate-publish-workflow!
) ; end define validate-publish-workflow!

(define (generated-workflow-paths c)
  (values (workflow-path (brew-ci-workflows-dir c) "tests.yml")
          (workflow-path (brew-ci-workflows-dir c) "publish.yml")))

(define (tap-workflow-paths c)
  (values (workflow-path (tap-workflows-dir c) "tests.yml")
          (workflow-path (tap-workflows-dir c) "publish.yml")))

(define (runner->dry-run-text runner)
  (begin
    (define os (runner-ref runner 'os #f))
    (define container (runner-ref runner 'container #f))
    (if container
        f"{os} in {container}"
        os)
  ) ; end begin runner->dry-run-text
) ; end define runner->dry-run-text

(define (print-runner-dry-run-plan! label runners)
  (begin
    (println/flush f"Would configure {label} runner count: {(number->string (length runners))}")
    (for ([runner (in-list runners)])
      (println/flush f"  - {(runner->dry-run-text runner)}")
    ) ; end for runner
  ) ; end begin print-runner-dry-run-plan!
) ; end define print-runner-dry-run-plan!

(define (print-brew-ci-dry-run-plan! c config)
  (begin
    (define bottle-runners (config-ref* config 'bottle-runners '()))
    (define syntax-runners (config-ref* config 'syntax-runners '()))
    (define-values (generated-tests generated-publish)
      (generated-workflow-paths c)
    ) ; end define-values generated workflow paths
    (define-values (tap-tests tap-publish)
      (tap-workflow-paths c)
    ) ; end define-values tap workflow paths
    (println/flush f"Would read brew CI config: {(clean-path-string (cfg-brew-ci-config c))}")
    (println/flush f"Would generate brew CI workflow: {(clean-path-string generated-tests)}")
    (println/flush f"Would generate brew CI workflow: {(clean-path-string generated-publish)}")
    (println/flush f"Would install brew CI workflow: {(clean-path-string tap-tests)}")
    (println/flush f"Would install brew CI workflow: {(clean-path-string tap-publish)}")
    (println/flush f"Would validate brew CI workflow YAML with: {(cfg-ruby-bin c)}")
    (print-runner-dry-run-plan! "bottle" bottle-runners)
    (print-runner-dry-run-plan! "syntax" syntax-runners)
  ) ; end begin print-brew-ci-dry-run-plan!
) ; end define print-brew-ci-dry-run-plan!

(define (prepare-brew-ci-workflows! c config)
  (begin
    (assert-homebrew-tap! c)
    (assert-executable 'prepare-brew-ci-workflows! (cfg-ruby-bin c))
    (reset-managed-dir! 'prepare-brew-ci-workflows! (brew-ci-work-root c))
    (make-directory* (brew-ci-workflows-dir c))
    (define-values (tests-path publish-path)
      (generated-workflow-paths c)
    ) ; end define-values generated workflow paths
    (write-text-file! tests-path (tests-workflow-content c config))
    (write-text-file! publish-path (publish-workflow-content c config))
    (validate-tests-workflow! c config tests-path)
    (validate-publish-workflow! c config publish-path)
    (values tests-path publish-path)
  ) ; end begin prepare-brew-ci-workflows!
) ; end define prepare-brew-ci-workflows!

(define (replace-file-atomically! who src dest)
  (begin
    (assert-nonempty-file who src)
    (make-directory* (path-only dest))
    (assert-writable-directory who (path-only dest))
    (define temp (make-temporary-file f".{(path-basename dest)}.tmp~a" #f (path-only dest)))
    (copy-file src temp #t)
    (rename-file-or-directory temp dest #t)
  ) ; end begin replace-file-atomically!
) ; end define replace-file-atomically!

(define (install-brew-ci-workflows! c config tests-path publish-path)
  (begin
    (assert-homebrew-tap! c)
    (define-values (tap-tests tap-publish)
      (tap-workflow-paths c)
    ) ; end define-values tap workflow paths
    (replace-file-atomically! 'install-brew-ci-workflows! tests-path tap-tests)
    (replace-file-atomically! 'install-brew-ci-workflows! publish-path tap-publish)
    (validate-tests-workflow! c config tap-tests)
    (validate-publish-workflow! c config tap-publish)
    (println/flush f"Installed brew CI workflow: {(clean-path-string tap-tests)}")
    (println/flush f"Installed brew CI workflow: {(clean-path-string tap-publish)}")
  ) ; end begin install-brew-ci-workflows!
) ; end define install-brew-ci-workflows!

(define (build-brew-ci! c)
  (begin
    (define config (read-brew-ci-config c))
    (validate-brew-ci-config! config)
    (if (cfg-dry-run? c)
        (begin
          (assert-homebrew-tap! c)
          (print-brew-ci-dry-run-plan! c config)
          '()
        ) ; end begin dry-run brew-ci plan
        (let-values ([(tests-path publish-path) (prepare-brew-ci-workflows! c config)])
          (list (lambda ()
                  (install-brew-ci-workflows! c config tests-path publish-path)
                ) ; end lambda install brew-ci workflows
          ) ; end list brew-ci finalizer
        ) ; end let-values generated workflow paths
    ) ; end if dry-run
  ) ; end begin build-brew-ci!
) ; end define build-brew-ci!

(define (default-jobs)
  (number->string (max 1 (processor-count))))

(define (make-config)
  (define target-args '())
  (define racket-root-arg #f)
  (define make-dir-arg #f)
  (define package-config-arg #f)
  (define formula-version-arg #f)
  (define package-name-arg "racket9")
  (define release-arg "1")
  (define prefix-arg "/usr")
  (define artifact-dir-arg "artifacts")
  (define work-dir-arg ".build")
  (define stage-dir-arg #f)
  (define install-root-arg #f)
  (define jobs-arg (default-jobs))
  (define skip-build? #f)
  (define keep-work? #f)
  (define dry-run? #f)
  (define make-bin-arg "make")
  (define tar-bin-arg "tar")
  (define dpkg-deb-bin-arg "dpkg-deb")
  (define ar-bin-arg "ar")
  (define xz-bin-arg "xz")
  (define deb-backend-arg "auto")
  (define rpmbuild-bin-arg "rpmbuild")
  (define rpm-bin-arg "rpm")
  (define createrepo-bin-arg "createrepo_c")
  (define deb-arch-arg #f)
  (define rpm-system-arg #f)
  (define rpm-release-arg #f)
  (define rpm-arch-arg #f)
  (define maintainer-arg "Cutie Deng <cutiedeng@users.noreply.github.com>")
  (define summary-arg "Racket programming language")
  (define license-arg "MIT OR Apache-2.0")
  (define url-arg "https://racket-lang.org/")
  (define bottle-root-url-arg #f)
  (define homebrew-tap-arg #f)
  (define formula-arg #f)
  (define update-formula? #t)
  (define formula-build-mode-arg "incremental")
  (define brew-ci-config-arg #f)
  (define source-release-config-arg #f)
  (define source-release-repo-arg #f)
  (define source-release-tag-arg #f)
  (define source-release-asset-arg #f)
  (define source-release-token-file-arg #f)
  (define apt-release-config-arg #f)
  (define apt-release-repo-arg #f)
  (define apt-release-tag-arg #f)
  (define apt-release-asset-arg #f)
  (define apt-release-token-file-arg #f)
  (define deb-repo-config-arg #f)
  (define deb-ci-config-arg #f)
  (define rpm-repo-config-arg #f)
  (define rpm-ci-config-arg #f)
  (define windows-ci-config-arg #f)
  (define rpm-repo-root-arg #f)
  (define rpm-repo-id-arg #f)
  (define rpm-repo-name-arg #f)
  (define rpm-repo-baseurl-arg #f)
  (define rpm-repo-enabled-arg 'unset)
  (define rpm-repo-gpgcheck-arg 'unset)
  (define replace-release-asset-arg 'unset)
  (define ruby-bin-arg "ruby")
  (define with-docs? #f)
  (define brew-package-args '())
  (define make-args '())
  (command-line
   #:program "package-racket.rkt"
   #:once-each
   [("--racket-root") path "Racket checkout root (default: current directory)"
                      (set! racket-root-arg path)]
   [("--make-dir") path "Directory that contains the unix-style make target (default: --racket-root)"
                  (set! make-dir-arg path)]
   [("--package-config") path "Package metadata config with formula-version (default: ./package-config.rktd)"
                         (set! package-config-arg path)]
   [("--formula-version") version "Override package-manager formula-version from package-config.rktd"
                          (set! formula-version-arg version)]
   [("--version") version "Compatibility alias for --formula-version"
                (set! formula-version-arg version)]
   [("--package-name") name "Package name (default: racket9)"
                      (set! package-name-arg name)]
   [("--release") release "Debian package release value (default: 1)"
                 (set! release-arg release)]
   [("--prefix") path "Install prefix inside the package (default: /usr)"
                (set! prefix-arg path)]
   [("--artifact-dir" "--output-dir") path "Directory for package artifacts (default: ./artifacts)"
                                        (set! artifact-dir-arg path)]
   [("--work-dir") path "Directory for generated package metadata (default: ./.build)"
                  (set! work-dir-arg path)]
   [("--stage-dir") path "Directory for brew source staging (default: --work-dir/stage)"
                  (set! stage-dir-arg path)]
   [("--install-root") path "Filesystem root staged by make unix-style (default: --work-dir/install-root)"
                     (set! install-root-arg path)]
   [("--jobs") jobs "Parallel setup jobs passed as JOBS=... (default: processor count)"
              (set! jobs-arg jobs)]
   [("--skip-build") "For install-root targets, package an existing --install-root instead of running make unix-style"
                    (set! skip-build? #t)]
   [("--keep-work") "Keep generated working directories after success"
                  (set! keep-work? #t)]
   [("--dry-run") "Print commands and resolved paths without writing package artifacts"
                (set! dry-run? #t)]
   [("--make-bin") path "make executable (default: make)"
                 (set! make-bin-arg path)]
   [("--tar-bin") path "tar executable for archive validation and assembly (default: tar)"
                (set! tar-bin-arg path)]
   [("--dpkg-deb-bin") path "dpkg-deb executable (default: dpkg-deb)"
                     (set! dpkg-deb-bin-arg path)]
   [("--ar-bin") path "ar executable for .deb fallback assembly (default: ar)"
               (set! ar-bin-arg path)]
   [("--xz-bin") path "xz executable for .deb fallback assembly (default: xz)"
               (set! xz-bin-arg path)]
   [("--deb-backend") backend "Debian backend: auto, dpkg-deb, or ar (default: auto)"
                    (set! deb-backend-arg backend)]
   [("--rpmbuild-bin") path "rpmbuild executable (default: rpmbuild)"
                    (set! rpmbuild-bin-arg path)]
   [("--rpm-bin") path "rpm executable for package validation (default: rpm)"
                (set! rpm-bin-arg path)]
   [("--createrepo-bin") path "createrepo_c executable for RPM repo metadata (default: createrepo_c)"
                         (set! createrepo-bin-arg path)]
   [("--deb-arch") arch "Debian architecture (default: amd64)"
                  (set! deb-arch-arg arch)]
   [("--rpm-system") system "RPM target system: el9, fc40, fc43, fc44, openeuler2203, or openeuler2403. Required for RPM targets"
                     (set! rpm-system-arg system)]
   [("--rpm-release") release "RPM release base before .rpm-system, for example 1. Required for RPM targets"
                     (set! rpm-release-arg release)]
   [("--rpm-arch") arch "RPM target architecture: x86_64, amd64, x64, aarch64, or arm64. Required for RPM targets"
                  (set! rpm-arch-arg arch)]
   [("--maintainer") value "Debian Maintainer field"
                    (set! maintainer-arg value)]
   [("--summary") value "Package summary"
                (set! summary-arg value)]
   [("--license") value "RPM license field"
                (set! license-arg value)]
   [("--url") value "Package URL/Homepage"
            (set! url-arg value)]
   [("--bottle-root-url") url "Required for brew and brew-ci; bottle release root URL"
                         (set! bottle-root-url-arg url)]
   [("--homebrew-tap") path "Required for brew and brew-ci; Homebrew tap root"
                      (set! homebrew-tap-arg path)]
   [("--formula") path "Homebrew formula to update (derived from --homebrew-tap when omitted)"
                (set! formula-arg path)]
   [("--no-update-formula") "Do not update the Homebrew formula"
                          (set! update-formula? #f)]
   [("--formula-build-mode") mode "Formula build mode: incremental or full (default: incremental)"
                             (set! formula-build-mode-arg mode)]
   [("--brew-ci-config") path "Package-racket source config for generated tap workflows (default: ./brew-ci-config.rktd)"
                       (set! brew-ci-config-arg path)]
   [("--source-release-config") path "Config for uploading the source release asset (default: ./source-release-config.rktd)"
                              (set! source-release-config-arg path)]
   [("--github-repo") repo "Override source release GitHub repo, OWNER/REPO"
                    (set! source-release-repo-arg repo)]
   [("--github-release-tag") tag "Override source release tag"
                            (set! source-release-tag-arg tag)]
   [("--github-asset-name") name "Override source release asset name"
                          (set! source-release-asset-arg name)]
   [("--github-token-file") path "Override token file read as one Racket string datum"
                           (set! source-release-token-file-arg path)]
   [("--apt-release-config") path "Config for uploading the apt .deb release asset (default: ./apt-release-config.rktd)"
                            (set! apt-release-config-arg path)]
   [("--apt-github-repo") repo "Override apt release GitHub repo, OWNER/REPO"
                         (set! apt-release-repo-arg repo)]
   [("--apt-github-release-tag") tag "Override apt release tag"
                                 (set! apt-release-tag-arg tag)]
   [("--apt-github-asset-name") name "Override apt release asset name"
                               (set! apt-release-asset-arg name)]
   [("--apt-github-token-file") path "Override apt token file read as one Racket string datum"
                              (set! apt-release-token-file-arg path)]
   [("--deb-repo-config") path "Config for generating/updating the deb-racket build-script repository (default: ./deb-repo-config.rktd)"
			  (set! deb-repo-config-arg path)]
   [("--deb-ci-config") path "Config for generated deb-racket GitHub Actions workflow (default: ./deb-ci-config.rktd)"
		       (set! deb-ci-config-arg path)]
   [("--rpm-repo-config") path "Config for generating/updating the RPM repository (default: ./rpm-repo-config.rktd)"
			  (set! rpm-repo-config-arg path)]
   [("--rpm-ci-config") path "Config for generated rpm-racket GitHub Actions workflow (default: ./rpm-ci-config.rktd)"
		       (set! rpm-ci-config-arg path)]
   [("--windows-ci-config") path "Config for generated Windows portable GitHub Actions workflow (default: ./windows-ci-config.rktd)"
			    (set! windows-ci-config-arg path)]
   [("--rpm-repo-root") path "Override RPM repository root from config"
		       (set! rpm-repo-root-arg path)]
   [("--rpm-repo-id") value "Override RPM repository id from config"
                     (set! rpm-repo-id-arg value)]
   [("--rpm-repo-name") value "Override RPM repository display name from config"
                       (set! rpm-repo-name-arg value)]
   [("--rpm-repo-baseurl") value "Override RPM repository baseurl from config"
                          (set! rpm-repo-baseurl-arg value)]
   [("--rpm-repo-disabled") "Write enabled=0 in the generated .repo file"
                            (set! rpm-repo-enabled-arg #f)]
   [("--rpm-repo-gpgcheck") "Write gpgcheck=1 in the generated .repo file"
                            (set! rpm-repo-gpgcheck-arg #t)]
   [("--no-rpm-repo-gpgcheck") "Write gpgcheck=0 in the generated .repo file"
                               (set! rpm-repo-gpgcheck-arg #f)]
   [("--replace-release-asset") "Delete an existing differing GitHub release asset before uploading"
                              (set! replace-release-asset-arg #t)]
   [("--no-replace-release-asset") "Refuse to replace an existing differing GitHub release asset"
                                 (set! replace-release-asset-arg #f)]
   [("--ruby-bin") path "Ruby executable for YAML validation (default: ruby)"
                  (set! ruby-bin-arg path)]
   [("--within-docs" "--with-docs") "Include raco docs support and the core documentation runtime package group in the Homebrew source archive"
                                   (set! with-docs? #t)]
   #:multi
   [("--target") target "Packaging target: brew, brew-ci, source-release, apt, apt-release, deb-spec, deb-ci, rpm, rpm-spec, rpm-ci, rpm-repo, windows-portable-ci, or all. May be repeated."
		(set! target-args (append target-args (list target)))]
   [("--brew-package") name "Extra package to include in the Homebrew source archive"
                     (set! brew-package-args (append brew-package-args (list name)))]
   [("--make-arg") arg "Extra VAR=VALUE argument passed to make unix-style. May be repeated."
                 (set! make-args (append make-args (list arg)))]
   #:args ()
   (void)
  ) ; end command-line
  (define targets (normalize-targets target-args))
  (define racket-root (complete-path* (or racket-root-arg (current-directory))))
  (define make-dir (complete-path* (or make-dir-arg racket-root)))
  (define artifact-dir (complete-path* artifact-dir-arg))
  (define work-dir (complete-path* work-dir-arg))
  (define stage-dir (complete-path* (or stage-dir-arg (build-path work-dir "stage"))))
  (define install-root (complete-path* (or install-root-arg (build-path work-dir "install-root"))))
  (define package-config (complete-path* (or package-config-arg (build-path script-dir "package-config.rktd"))))
  (when (and (needs-homebrew-tap? targets) (not homebrew-tap-arg))
    (raise-user-error 'main "--homebrew-tap is required when --target includes brew or brew-ci")
  ) ; end when missing homebrew tap
  (when (and (needs-bottle-root-url? targets) (not bottle-root-url-arg))
    (raise-user-error 'main "--bottle-root-url is required when --target includes brew or brew-ci")
  ) ; end when missing bottle root url
  (assert-formula-build-mode formula-build-mode-arg)
  (define homebrew-tap (and homebrew-tap-arg (complete-path* homebrew-tap-arg)))
  (define formula
    (cond
      [formula-arg
       (complete-path* formula-arg)]
      [homebrew-tap
       (complete-path* (build-path homebrew-tap "Formula" "racket@9.rb"))]
      [else
       #f]
    ) ; end cond formula path
  ) ; end define formula
  (define brew-ci-config (complete-path* (or brew-ci-config-arg (build-path script-dir "brew-ci-config.rktd"))))
  (define source-release-config
    (complete-path* (or source-release-config-arg (build-path script-dir "source-release-config.rktd"))))
  (define apt-release-config
    (complete-path* (or apt-release-config-arg (build-path script-dir "apt-release-config.rktd"))))
  (define deb-repo-config
    (complete-path* (or deb-repo-config-arg (build-path script-dir "deb-repo-config.rktd"))))
  (define deb-ci-config
    (complete-path* (or deb-ci-config-arg (build-path script-dir "deb-ci-config.rktd"))))
  (define rpm-repo-config
    (complete-path* (or rpm-repo-config-arg (build-path script-dir "rpm-repo-config.rktd"))))
  (define rpm-ci-config
    (complete-path* (or rpm-ci-config-arg (build-path script-dir "rpm-ci-config.rktd"))))
  (define windows-ci-config
    (complete-path* (or windows-ci-config-arg (build-path script-dir "windows-ci-config.rktd"))))
  (assert-prefix prefix-arg)
  (when (needs-racket-root? targets)
    (assert-racket-root racket-root)
  ) ; end when needs racket root
  (define source-version
    (cond
      [(needs-racket-root? targets)
       (assert-version-string 'main 'source-version (read-racket-version racket-root))]
      [else
       (read-package-source-version package-config)]
    ) ; end cond source version
  ) ; end define source-version
  (when (needs-racket-root? targets)
    (define configured-source-version (read-package-source-version package-config))
    (unless (string=? configured-source-version source-version)
      (raise-user-error 'main
                        f"package-config source-version {configured-source-version} does not match racket-root source version {source-version}")
    ) ; end unless configured source version matches checkout
  ) ; end when compare source version
  (define formula-version
    (assert-version-string
     'main
     'formula-version
     (or formula-version-arg
         (read-package-formula-version package-config)))
  ) ; end define formula-version
  (when bottle-root-url-arg
    (assert-bottle-root-url bottle-root-url-arg)
  ) ; end when bottle root url provided
  (define-values (deb-repo-root deb-system deb-release deb-config-arch)
    (if (needs-deb-repo-config? targets)
        (read-deb-repo-config-values deb-repo-config)
        (values #f #f #f #f))
  ) ; end define-values deb repo config
  (define deb-arch
    (cond
      [deb-arch-arg
       (normalize-deb-arch deb-arch-arg)]
      [deb-config-arch
       deb-config-arch]
      [else
       "amd64"]
    ) ; end cond deb arch
  ) ; end define deb-arch
  (define-values (rpm-repo-root rpm-repo-id rpm-repo-name rpm-repo-baseurl
                                rpm-repo-enabled? rpm-repo-gpgcheck?)
    (if (needs-rpm-repo-config? targets)
        (read-rpm-repo-config-values rpm-repo-config
                                     rpm-repo-root-arg
                                     rpm-repo-id-arg
                                     rpm-repo-name-arg
                                     rpm-repo-baseurl-arg
                                     rpm-repo-enabled-arg
                                     rpm-repo-gpgcheck-arg)
        (values #f #f #f #f #t #f))
  ) ; end define-values rpm repo config
  (define-values (windows-repo-root)
    (if (needs-windows-ci-config? targets)
        (read-windows-ci-config-values windows-ci-config)
        (values #f))
  ) ; end define-values windows ci config
  (define rpm-system
    (cond
      [(needs-rpm-target? targets)
       (unless rpm-system-arg
	         (raise-user-error 'main "--rpm-system is required when --target includes rpm, rpm-spec, or rpm-repo")
       ) ; end unless missing rpm system
       (assert-rpm-system rpm-system-arg)]
      [rpm-system-arg
       (assert-rpm-system rpm-system-arg)]
      [else #f]
    ) ; end cond rpm system
  ) ; end define rpm-system
  (define rpm-release
    (cond
      [(needs-rpm-target? targets)
       (unless rpm-release-arg
	         (raise-user-error 'main "--rpm-release is required when --target includes rpm, rpm-spec, or rpm-repo")
       ) ; end unless missing rpm release
       (assert-rpm-release rpm-release-arg)]
      [rpm-release-arg
       (assert-rpm-release rpm-release-arg)]
      [else #f]
    ) ; end cond rpm release
  ) ; end define rpm-release
  (define rpm-arch
    (cond
      [(needs-rpm-target? targets)
       (unless rpm-arch-arg
	         (raise-user-error 'main "--rpm-arch is required when --target includes rpm, rpm-spec, or rpm-repo")
       ) ; end unless missing rpm arch
       (normalize-rpm-arch rpm-arch-arg)]
      [rpm-arch-arg
       (normalize-rpm-arch rpm-arch-arg)]
      [else #f]
    ) ; end cond rpm arch
  ) ; end define rpm-arch
  (cfg targets
       racket-root
       make-dir
       package-config
       source-version
       formula-version
       package-name-arg
       release-arg
       prefix-arg
       artifact-dir
       work-dir
       stage-dir
       install-root
       jobs-arg
       skip-build?
       keep-work?
       dry-run?
       make-bin-arg
       tar-bin-arg
       dpkg-deb-bin-arg
       ar-bin-arg
       xz-bin-arg
       deb-backend-arg
       rpmbuild-bin-arg
       rpm-bin-arg
       createrepo-bin-arg
       deb-arch
       rpm-system
       rpm-release
       rpm-arch
       maintainer-arg
       summary-arg
       license-arg
       url-arg
       bottle-root-url-arg
       homebrew-tap
       formula
       update-formula?
       formula-build-mode-arg
       brew-ci-config
       source-release-config
       source-release-repo-arg
       source-release-tag-arg
       source-release-asset-arg
       source-release-token-file-arg
       apt-release-config
       apt-release-repo-arg
	       apt-release-tag-arg
	       apt-release-asset-arg
	       apt-release-token-file-arg
	       deb-repo-config
	       deb-ci-config
	       rpm-repo-config
	       rpm-ci-config
	       windows-ci-config
	       deb-repo-root
	       deb-system
	       deb-release
	       rpm-repo-root
	       windows-repo-root
	       rpm-repo-id
       rpm-repo-name
       rpm-repo-baseurl
       rpm-repo-enabled?
       rpm-repo-gpgcheck?
       replace-release-asset-arg
       ruby-bin-arg
       with-docs?
       brew-package-args
       make-args
  ) ; end cfg
) ; end define make-config

(define (needs-install-root? targets)
  (member "apt" targets string=?))

(define (print-config c)
  (println/flush f"Targets: {(string-join (cfg-targets c) ", ")}")
  (println/flush f"Package config: {(clean-path-string (cfg-package-config c))}")
  (println/flush f"Formula/package version: {(cfg-formula-version c)}")
  (when (needs-racket-root? (cfg-targets c))
    (println/flush f"Racket root: {(clean-path-string (cfg-racket-root c))}")
    (println/flush f"Racket source version: {(cfg-source-version c)}")
  ) ; end when needs racket root
  (println/flush f"Artifact dir: {(clean-path-string (cfg-artifact-dir c))}")
  (println/flush f"Work dir: {(clean-path-string (cfg-work-dir c))}")
  (when (needs-install-root? (cfg-targets c))
    (println/flush f"Install root: {(clean-path-string (cfg-install-root c))}")
    (println/flush f"Prefix: {(cfg-prefix c)}")
  ) ; end when install-root target
  (when (or (member "rpm-spec" (cfg-targets c) string=?)
            (member "rpm" (cfg-targets c) string=?)
            (member "rpm-repo" (cfg-targets c) string=?))
    (println/flush f"RPM target arch: {(cfg-rpm-arch c)}")
    (println/flush f"RPM target system: {(cfg-rpm-system c)}")
    (println/flush f"RPM package version: {(rpm-version c)}")
    (println/flush f"RPM package release base: {(cfg-rpm-release c)}")
    (println/flush f"RPM package release: {(rpm-release c)}")
    (println/flush f"RPM package prefix: {(cfg-prefix c)}")
  ) ; end when rpm target or repo target
  (when (needs-deb-repo-config? (cfg-targets c))
    (println/flush f"DEB repo config: {(clean-path-string (cfg-deb-repo-config c))}")
    (println/flush f"DEB repo root: {(clean-path-string (cfg-deb-repo-root c))}")
    (println/flush f"DEB target arch: {(cfg-deb-arch c)}")
    (println/flush f"DEB target system: {(cfg-deb-system c)}")
    (println/flush f"DEB package version: {(deb-package-version c (cfg-deb-release c) (cfg-deb-system c))}")
    (println/flush f"DEB package release base: {(cfg-deb-release c)}")
  ) ; end when deb repo config target
  (when (needs-rpm-repo-config? (cfg-targets c))
    (println/flush f"RPM repo config: {(clean-path-string (cfg-rpm-repo-config c))}")
    (println/flush f"RPM repo root: {(clean-path-string (cfg-rpm-repo-root c))}")
  ) ; end when rpm repo target
  (when (needs-rpm-ci-config? (cfg-targets c))
    (println/flush f"RPM CI config: {(clean-path-string (cfg-rpm-ci-config c))}")
  ) ; end when rpm ci target
  (when (needs-deb-ci-config? (cfg-targets c))
    (println/flush f"DEB CI config: {(clean-path-string (cfg-deb-ci-config c))}")
  ) ; end when deb ci target
  (when (needs-windows-ci-config? (cfg-targets c))
    (println/flush f"Windows CI config: {(clean-path-string (cfg-windows-ci-config c))}")
    (println/flush f"Windows CI repo root: {(clean-path-string (cfg-windows-repo-root c))}")
  ) ; end when windows ci target
  (when (cfg-bottle-root-url c)
    (println/flush f"Bottle root URL: {(cfg-bottle-root-url c)}")
  ) ; end when bottle root url
  (when (member "brew" (cfg-targets c) string=?)
    (println/flush f"Formula build mode: {(cfg-formula-build-mode c)}")
    (println/flush f"Brew docs: {(if (cfg-with-docs? c) "enabled" "disabled")}")
  ) ; end when brew target
  (when (member "source-release" (cfg-targets c) string=?)
    (println/flush f"Source release config: {(clean-path-string (cfg-source-release-config c))}")
  ) ; end when source release target
  (when (member "apt-release" (cfg-targets c) string=?)
    (println/flush f"APT release config: {(clean-path-string (cfg-apt-release-config c))}")
  ) ; end when apt release target
) ; end define print-config

(define (main)
  (define c (make-config))
  (print-config c)
  (when (needs-install-root? (cfg-targets c))
    (build-install-root! c)
  ) ; end when needs install root
  (define finalizers
    (append-map
     (lambda (target)
       (match target
         ["brew" (build-brew! c)]
         ["brew-ci" (build-brew-ci! c)]
         ["source-release" (build-source-release! c)]
	 ["apt" (build-apt! c) '()]
	 ["apt-release" (build-apt-release! c)]
	 ["deb-spec" (build-deb-spec! c) '()]
	 ["deb-ci" (build-deb-ci! c) '()]
	 ["rpm-spec" (build-rpm-spec! c) '()]
	 ["rpm-ci" (build-rpm-ci! c) '()]
	 ["rpm" (build-rpm! c) '()]
         ["rpm-repo" (build-rpm-repo! c)]
         ["windows-portable-ci" (build-windows-ci! c) '()]
         [_ (error 'main f"unreachable target: {target}")]
       ) ; end match target
     ) ; end lambda target
     (cfg-targets c)
    ) ; end append-map finalizers
  ) ; end define finalizers
  (for ([finish! (in-list finalizers)])
    (finish!)
  ) ; end for finalizer
  (unless (or (cfg-keep-work? c) (cfg-dry-run? c))
    (delete-managed-dir-if-present! (build-path (cfg-work-dir c) "apt-root"))
    (delete-managed-dir-if-present! (build-path (cfg-work-dir c) "deb-parts"))
    (delete-managed-dir-if-present! (build-path (cfg-work-dir c) "brew"))
    (delete-managed-dir-if-present! (build-path (cfg-work-dir c) "brew-ci"))
    (delete-managed-dir-if-present! (build-path (cfg-work-dir c) "rpm"))
    (delete-managed-dir-if-present! (build-path (cfg-work-dir c) "rpm-source"))
    (delete-managed-dir-if-present! (build-path (cfg-stage-dir c) "brew-source"))
  ) ; end unless cleanup work dirs
  (println/flush "Done.")
) ; end define main

(module+ test
  (define test-root (find-system-path 'temp-dir))
  (define test-bottle-root-url "https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.2")
  (define test-sha256 (make-string 64 #\a))

  (define (test-cfg #:targets [targets '("brew")]
                    #:dry-run? [dry-run? #t]
                    #:update-formula? [update-formula? #t]
                    #:formula-build-mode [formula-build-mode "full"]
                    #:source-version [source-version "9.2.2"]
                    #:formula-version [formula-version "9.2.2"]
                    #:rpm-system [rpm-system "el9"]
                    #:rpm-release [rpm-release "1"]
                    #:rpm-arch [rpm-arch "x86_64"]
                    #:with-docs? [with-docs? #f]
                    #:brew-packages [brew-packages '()])
    (cfg targets
         (build-path test-root "racket-root")
         (build-path test-root "racket-root")
         (build-path test-root "package-config.rktd")
         source-version
         formula-version
         "racket9"
         "1"
         "/usr"
         (build-path test-root "artifacts")
         (build-path test-root "work")
         (build-path test-root "stage")
         (build-path test-root "install-root")
         "1"
         #f
         #f
         dry-run?
         "make"
         "tar"
         "dpkg-deb"
         "ar"
         "xz"
         "auto"
         "rpmbuild"
         "rpm"
         "createrepo_c"
         "amd64"
         rpm-system
         rpm-release
         rpm-arch
         "Cutie Deng <cutiedeng@users.noreply.github.com>"
         "Racket programming language"
         "MIT OR Apache-2.0"
         "https://racket-lang.org/"
         test-bottle-root-url
         (build-path test-root "homebrew-racket")
         (build-path test-root "homebrew-racket" "Formula" "racket@9.rb")
         update-formula?
         formula-build-mode
         (build-path test-root "brew-ci-config.rktd")
         (build-path test-root "source-release-config.rktd")
         #f
         #f
         #f
         #f
         (build-path test-root "apt-release-config.rktd")
         #f
         #f
	         #f
	         #f
	         (build-path test-root "deb-repo-config.rktd")
	         (build-path test-root "deb-ci-config.rktd")
	         (build-path test-root "rpm-repo-config.rktd")
	         (build-path test-root "rpm-ci-config.rktd")
	         (build-path test-root "windows-ci-config.rktd")
	         (build-path test-root "deb-racket")
	         "ubuntu2404"
	         "1"
	         (build-path test-root "rpm-racket")
	         (build-path test-root "package-racket")
         "cutiedeng-racket"
         "CutieDeng Racket RPM Repository"
         "https://raw.githubusercontent.com/CutieDeng/rpm-racket/main/repo/$basearch"
         #t
         #f
         'unset
         "ruby"
         with-docs?
         brew-packages
         '())
  ) ; end define test-cfg

  (define test-brew-ci-config
    #hash((formula . "racket@9")
          (artifact-prefix . "bottles")
          (bottle-rebuild . 1)
          (bottle-runners . (#hash((os . "macos-26"))
                             #hash((os . "ubuntu-latest")
                                   (container . "ghcr.io/homebrew/brew:main"))
                             #hash((os . "ubuntu-24.04-arm")
                                   (container . "ghcr.io/homebrew/brew:main"))))
          (syntax-runners . (#hash((os . "macos-15-intel"))))))

  (define (formula-version-before-sha? content)
    (begin
      (define version-start (regexp-first-start #px"(?m:^  version \"[^\"]+\")" content))
      (define sha-start (regexp-first-start #px"(?m:^  sha256 \"[0-9a-f]{64}\")" content))
      (and version-start sha-start (< version-start sha-start))
    ) ; end begin formula-version-before-sha?
  ) ; end define formula-version-before-sha?

  (test-case "brew target names and package closure stay stable"
    (define c (test-cfg #:brew-packages '("sandbox-lib" "custom-extra")))
    (define packages (brew-source-packages c))
    (check-equal? (catalog-lookup-version "9.2") "9.2")
    (check-equal? (catalog-lookup-version "9.2.2") "9.2")
    (define config-path (make-temporary-file "package-racket-config~a.rktd"))
    (dynamic-wind
      void
      (lambda ()
        (write-brew-config! config-path "9.2.2")
        (define cfg (call-with-input-file config-path read))
        (check-equal? (hash-ref cfg 'installation-name) "9.2.2")
        (check-equal? (hash-ref cfg 'pkg-catalog-lookup-version) "9.2"))
      (lambda ()
        (when (file-exists? config-path)
          (delete-file config-path))))
    (check-equal? (normalize-targets '("all" "brew-ci" "source-release" "apt-release"))
                  '("brew-ci" "brew" "source-release" "apt" "apt-release" "rpm"))
    (check-equal? (normalize-targets '("rpm-repo" "rpm" "rpm-spec"))
                  '("rpm-spec" "rpm" "rpm-repo"))
    (check-equal? (normalize-targets '("rpm-repo" "rpm"))
                  '("rpm" "rpm-repo"))
    (check-equal? (normalize-targets '("windows-portable-ci"))
                  '("windows-portable-ci"))
    (check-equal? (normalize-rpm-arch "arm64") "aarch64")
    (check-equal? (normalize-rpm-arch "amd64") "x86_64")
    (check-equal? (brew-source-tgz-name c) "racket-minimal-9.2.2-src.tgz")
    (check-equal? (rpm-package-name c) "racket9-9.2.2-1.el9.x86_64.rpm")
    (check-true (and (member "sandbox-lib" packages string=?) #t))
    (check-true (and (member "errortrace-lib" packages string=?) #t))
    (check-true (and (member "source-syntax" packages string=?) #t))
    (check-true (and (member "at-exp-lib" packages string=?) #t))
    (for ([name (in-list '("pretty-expressive-lib"
                           "shrubbery-lib"
                           "enforest-lib"
                           "rhombus-lib"
                           "rhombus-exe"))])
      (check-true (and (member name packages string=?) #t) name)
    ) ; end for rhombus core packages
    (check-equal? (brew-package-link-name "at-exp-lib") 'root)
    (check-equal? (brew-package-link-name "pretty-expressive-lib") "pretty-expressive")
    (check-equal? (brew-package-link-name "rhombus-lib") 'root)
    (check-equal? (brew-package-link-name "rhombus-exe") 'root)
    (check-true (and (member "share/pkgs/at-exp-lib/at-exp/lang/reader.rkt"
                             (brew-required-package-files c)
                             string=?)
                     #t))
    (check-true (and (member "share/pkgs/rhombus-lib/rhombus/reader.rkt"
                             (brew-required-package-files c)
                             string=?)
                     #t))
    (check-true (and (member "share/pkgs/rhombus-exe/rhombus/run.rhm"
                             (brew-required-package-files c)
                             string=?)
                     #t))
    (check-true (and (member "root (#\"pkgs\" #\"at-exp-lib\")"
                             (brew-required-link-needles c)
                             string=?)
                     #t))
    (check-true (and (member "\"pretty-expressive\" (#\"pkgs\" #\"pretty-expressive-lib\")"
                             (brew-required-link-needles c)
                             string=?)
                     #t))
    (check-true (and (member "root (#\"pkgs\" #\"rhombus-lib\")"
                             (brew-required-link-needles c)
                             string=?)
                     #t))
    (check-true (and (member "\"at-exp-lib\""
                             (brew-required-pkgs-db-needles c)
                             string=?)
                     #t))
    (check-true (and (member "\"rhombus-lib\""
                             (brew-required-pkgs-db-needles c)
                             string=?)
                     #t))
    (check-true (and (member "custom-extra" packages string=?) #t))
    (check-equal? (count (lambda (name) (string=? name "sandbox-lib")) packages) 1)
    (check-false (member "racket-aarch64-macosx-4" packages string=?))
    (check-false (member "racket-index" packages string=?))
    (define docs-packages (brew-source-packages (test-cfg #:with-docs? #t)))
    (for ([name (in-list '("racket-index"
                           "scribble-lib"
                           "scribble-html-lib"
                           "net-lib"
                           "srfi-lite-lib"
                           "compatibility-lib"
                           "planet-lib"
                           "draw-lib"))])
      (check-true (and (member name docs-packages string=?) #t) name)
      (check-equal? (brew-package-link-name name) 'root)
    ) ; end for docs package
    (check-false (member "draw-aarch64-linux-natipkg-3" docs-packages string=?))
  ) ; end test-case brew package closure

  (test-case "brew docs patch removes draw platform package dependencies exactly"
    (define pkgs-dir (make-temporary-file "package-racket-draw-pkgs~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (make-directory* (build-path pkgs-dir "draw-lib"))
        (write-text-file!
         (build-path pkgs-dir "draw-lib" "info.rkt")
         (string-append "(define deps '(\"base\" "
                        (string-join brew-draw-lib-excluded-dependencies " ")
                        "))\n"))
        (patch-brew-draw-lib-info! pkgs-dir)
        (define patched-content
          (file->string (build-path pkgs-dir "draw-lib" "info.rkt")))
        (for ([dependency (in-list brew-draw-lib-excluded-dependencies)])
          (check-false (string-contains? patched-content dependency))
        ) ; end for removed draw dependency
      ) ; end lambda patch draw info
      (lambda ()
        (delete-directory/files pkgs-dir)
      ) ; end lambda cleanup draw info
    ) ; end dynamic-wind draw info
  ) ; end test-case brew docs patch

  (test-case "brew source pruning removes Chez documentation assets"
    (define src-dir (make-temporary-file "package-racket-src~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (for ([rel (in-list brew-source-pruned-source-dirs)])
          (define dir (apply build-path src-dir rel))
          (make-directory* dir)
          (write-text-file! (build-path dir "asset") "unused")
        ) ; end for create pruned dirs
        (define kept-dir (build-path src-dir "ChezScheme" "c"))
        (make-directory* kept-dir)
        (write-text-file! (build-path kept-dir "scheme.c") "kept")
        (prune-brew-source-assets! src-dir)
        (for ([rel (in-list brew-source-pruned-source-dirs)])
          (check-false (directory-exists? (apply build-path src-dir rel)))
        ) ; end for pruned dirs gone
        (check-true (file-exists? (build-path kept-dir "scheme.c")))
      ) ; end lambda prune source assets
      (lambda ()
        (delete-directory/files src-dir)
      ) ; end lambda cleanup source assets
    ) ; end dynamic-wind source assets
  ) ; end test-case brew source pruning

  (test-case "rpm system release and arch stay explicit"
    (define el9 (test-cfg #:rpm-system "el9"
                          #:rpm-release "1"
                          #:rpm-arch "x86_64"))
    (define fc40 (test-cfg #:rpm-system "fc40"
                           #:rpm-release "2"
                           #:rpm-arch "x86_64"))
    (define fc43 (test-cfg #:rpm-system "fc43"
                           #:rpm-release "2"
                           #:rpm-arch "x86_64"))
    (define fc44 (test-cfg #:rpm-system "fc44"
                           #:rpm-release "2"
                           #:rpm-arch "x86_64"))
    (define openeuler2203 (test-cfg #:rpm-system "openeuler2203"
                                    #:rpm-release "1"
                                    #:rpm-arch "aarch64"))
    (define openeuler2403 (test-cfg #:rpm-system "openeuler2403"
                                    #:rpm-release "1"
                                    #:rpm-arch "aarch64"))
    (check-equal? (rpm-release el9) "1.el9")
    (check-equal? (rpm-package-name el9) "racket9-9.2.2-1.el9.x86_64.rpm")
    (check-equal? (rpm-release fc40) "2.fc40")
    (check-equal? (rpm-package-name fc40) "racket9-9.2.2-2.fc40.x86_64.rpm")
    (check-equal? (rpm-release fc43) "2.fc43")
    (check-equal? (rpm-package-name fc43) "racket9-9.2.2-2.fc43.x86_64.rpm")
    (check-equal? (rpm-release fc44) "2.fc44")
    (check-equal? (rpm-package-name fc44) "racket9-9.2.2-2.fc44.x86_64.rpm")
    (check-equal? (rpm-release openeuler2203) "1.openeuler2203")
    (check-equal? (rpm-package-name openeuler2203) "racket9-9.2.2-1.openeuler2203.aarch64.rpm")
    (check-equal? (rpm-release openeuler2403) "1.openeuler2403")
    (check-equal? (rpm-package-name openeuler2403) "racket9-9.2.2-1.openeuler2403.aarch64.rpm")
    (check-exn exn:fail?
               (lambda ()
                 (assert-rpm-system "openeuler")
               ) ; end lambda generic openeuler
    ) ; end check-exn generic openeuler
  ) ; end test-case rpm explicit system release arch

  (test-case "deb md5sums track payload files only"
    (define deb-root (make-temporary-file "package-racket-deb-root~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (make-directory* (build-path deb-root "usr" "bin"))
        (make-directory* (build-path deb-root "DEBIAN"))
        (write-text-file! (build-path deb-root "usr" "bin" "racket") "abc")
        (write-text-file! (build-path deb-root "DEBIAN" "control") "Package: racket9\n")
        (define lines (deb-md5sum-lines deb-root))
        (check-true (and (member "900150983cd24fb0d6963f7d28e17f72  usr/bin/racket"
                                 lines
                                 string=?)
                         #t))
        (check-false
         (ormap (lambda (line) (string-contains? line "DEBIAN/control")) lines))
        (write-deb-md5sums! deb-root)
        (define md5-content (file->string (build-path deb-root "DEBIAN" "md5sums")))
        (check-true (string-contains? md5-content "usr/bin/racket"))
        (check-false (string-contains? md5-content "DEBIAN/control"))
      ) ; end lambda write md5sums
      (lambda ()
        (delete-directory/files deb-root)
      ) ; end lambda cleanup md5sums
    ) ; end dynamic-wind deb md5sums
  ) ; end test-case deb md5sums

  (test-case "formula-version drives brew and apt while rpm and deb-racket keep version plus release"
    (define c (test-cfg #:source-version "9.2.2"
                        #:formula-version "9.2.2.1"))
    (check-equal? (brew-source-tgz-name c) "racket-minimal-9.2.2-src.tgz")
    (check-equal? (apt-deb-name c) "racket9_9.2.2.1-1_amd64.deb")
    (check-equal? (deb-generated-package-name c "1" "ubuntu2404" "amd64")
                  "racket9_9.2.2-1.ubuntu2404_amd64.deb")
    (check-equal? (rpm-package-name c) "racket9-9.2.2-1.el9.x86_64.rpm")
    (check-equal? (brew-tgz-member-path c "src/README.txt")
                  "racket-9.2.2/src/README.txt")
    (define content (formula-content/full c test-sha256))
    (check-true
     (string-contains? content
                       "url \"https://github.com/CutieDeng/racket/releases/download/v9.2.2/racket-minimal-9.2.2-src.tgz\""))
    (check-true (string-contains? content "version \"9.2.2.1\""))
    (check-true (formula-version-before-sha? content))
    (check-true (string-contains? content "assert_match \"9.2.2\""))
    (check-false (string-contains? content "Welcome to Racket v9.2.2.1 [cs]."))
    (define publish-content (publish-workflow-content c test-brew-ci-config))
    (check-true (string-contains? publish-content "RELEASE_TAG: v9.2.2"))
    (check-true (string-contains? publish-content "gh release create \"$RELEASE_TAG\""))
    (define rpm-root (make-temporary-file "package-racket-rpm-spec~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (make-directory* (build-path (cfg-install-root c) "usr" "bin"))
        (make-directory* (build-path (cfg-install-root c) "usr" "lib" "racket" "collects"))
        (write-text-file! (build-path (cfg-install-root c) "usr" "bin" "racket")
                          "#!/bin/sh\n")
        (write-text-file! (build-path (cfg-install-root c) "usr" "lib" "racket" "collects" "main.rkt")
                          "#lang racket/base\n")
        (define spec-path (build-path rpm-root "racket9.spec"))
        (write-rpm-spec! c
                         spec-path
                         "https://github.com/CutieDeng/racket/releases/download/v9.2.2/racket-minimal-9.2.2-src.tgz"
                         test-sha256)
        (define file-list (rpm-file-list c))
        (define spec-content (file->string spec-path))
        (check-true (string-contains? spec-content "Version: 9.2.2"))
        (check-false (string-contains? spec-content "Version: 9.2.2.1"))
        (check-true (string-contains? spec-content "%{!?package_system:%global package_system el9}"))
        (check-true (string-contains? spec-content "%{!?package_release:%global package_release 1}"))
        (check-true (string-contains? spec-content "Release: %{package_release}.%{package_system}"))
        (check-true (string-contains? spec-content "Source0: https://github.com/CutieDeng/racket/releases/download/v9.2.2/racket-minimal-9.2.2-src.tgz"))
        (check-true (string-contains? spec-content "Requires: libedit"))
        (check-false (string-contains? spec-content "Source1:"))
        (check-true (string-contains? spec-content "%global __brp_compress %{nil}"))
        (check-true (string-contains? spec-content "%global debug_package %{nil}"))
        (check-true (string-contains? spec-content ".rackboot ELF section"))
        (check-true (string-contains? spec-content "%global source_sha256"))
        (check-true (string-contains? spec-content "Source0 sha256 mismatch"))
        (check-true (string-contains? spec-content "%files -f %{name}.files"))
        (check-true
         (string-contains? spec-content
                           "printf '%s %s\\n' '%%dir' \"$rel\" >> \"$manifest\""))
        (check-false (string-contains? spec-content "printf '%%dir %s\\n'"))
        (check-true (string-contains? spec-content "/etc"))
        (check-true
         (string-contains? spec-content
                           "grep -Eq '^(%dir )?(/bin|/boot|/dev|/etc"))
        (check-false (string-contains? spec-content "/usr/bin/racket"))
        (check-true (and (member "/usr/bin/racket" file-list string=?) #t))
        (check-true (and (member "%dir /usr/lib/racket" file-list string=?) #t))
        (check-true (and (member "/usr/lib/racket/collects/main.rkt" file-list string=?) #t))
        (check-false (member "/usr" file-list string=?))
        (check-false (member "%dir /usr" file-list string=?))
      ) ; end lambda write rpm spec
      (lambda ()
        (delete-directory/files rpm-root)
        (delete-directory/files (cfg-install-root c))
      ) ; end lambda cleanup rpm spec
    ) ; end dynamic-wind rpm spec
  ) ; end test-case formula-version package-manager outputs

  (test-case "incremental Formula source update normalizes Homebrew component order"
    (define c (test-cfg #:formula-build-mode "incremental"))
    (define formula-dir (make-temporary-file "package-racket-formula-order~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (define formula-path (build-path formula-dir "racket@9.rb"))
        (write-text-file!
         formula-path
         f"{(generated-code-notice "#")}class RacketAT9 < Formula
  desc \"Modern programming language in the Lisp/Scheme family\"
  homepage \"https://racket-lang.org/\"
  url \"https://github.com/CutieDeng/racket/releases/download/v9.2.2/racket-minimal-9.2.2-src.tgz\"
  sha256 \"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"
  version \"9.2.2\"
  license any_of: [\"MIT\", \"Apache-2.0\"]

  depends_on \"openssl@3\"

  on_linux do
    depends_on \"ncurses\"
  end

  def install
    config_entries = [
      \"(default-scope . \\\"installation\\\")\",
      \"(compiled-file-cache-roots . (user system))\",
      \"(compiled-file-system-cache-root . \\\"{(ruby-interpolate "var")}/cache/racket/compiled\\\")\",
    ].join(\" \")
    inreplace \"etc/config.rktd\", /\\)\\)\\n$/, \") \" + config_entries + \")\\n\"
    system bin/\"raco\", \"setup\", \"--no-user\", \"--no-zo\"
    remove_precompiled_cache
  end

  def post_install
    system bin/\"raco\", \"setup\", \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\"
  end

  def remove_precompiled_cache
    rm_r Dir[\"#{{prefix}}/**/compiled\"].sort_by(&:length).reverse
  end

  test do
    assert_match \"9.2.2\", shell_output(\"racket -e '(displayln (version))'\")
  end
end
")
        (set-formula-source! c formula-path test-sha256)
        (define content (file->string formula-path))
        (check-true (formula-version-before-sha? content))
        (check-true (string-contains? content f"sha256 \"{test-sha256}\""))
        (check-true (string-contains? content "(compiled-file-system-cache-root . \\\"#{system_cache_root}\\\")"))
        (check-true (string-contains? content "content.sub!(/\\)\\s*\\z/"))
	        (check-true (string-contains? content "system bin/\"raco\", \"setup\", \"--no-user\", \"--no-zo\"\n    configure_racket\n    if build.bottle?"))
	        (check-false (string-contains? content "source_racket_config"))
	        (check-false (string-contains? content "configure_racket source_racket_config"))
	        (check-true (string-contains? content "racket_config.atomic_write content"))
	        (check-false (string-contains? content "inreplace racket_config"))
	        (check-false (string-contains? content "inreplace \"etc/config.rktd\""))
	        (check-false (string-contains? content "racket_config.write"))
	        (check-true (string-contains? content "if build.bottle?"))
	        (check-true (string-contains? content "preserve_compiled_cache_dir?"))
	        (check-true (string-contains? content "system_cache_populated?"))
	        (check-true (string-contains? content "rhombus_demod_cache_populated?"))
	        (check-true (string-contains? content "package-racket-rhombus-cache"))
	        (check-true (string-contains? content "prefix/\"var/cache/racket/compiled#{share}/racket/collects\""))
		        (check-true (string-contains? content "system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"raco\", \"-l-\", \"raco\", \"setup\",\n           \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\", \"--no-launcher\""))
	        (check-true (string-contains? content "\"-l-\", \"rhombus/run.rhm\", \"--version\""))
        (check-false (string-contains? content "var/cache/racket/compiled#{prefix}/share"))
        (check-false (string-contains? content "#{var}/cache/racket/compiled"))
      ) ; end lambda update formula
      (lambda ()
        (delete-directory/files formula-dir)
      ) ; end lambda cleanup formula
    ) ; end dynamic-wind formula order
  ) ; end test-case incremental Formula order

  (test-case "generated code notice uses public source URL"
    (define notice (generated-code-notice "#"))
    (check-true (string-contains? notice f"Source of truth: {generated-source-url}"))
    (check-false (string-contains? notice (clean-path-string script-dir)))
  ) ; end test-case generated code notice source URL

  (test-case "full brew Formula template keeps runtime checks and dependencies"
    (define content (formula-content/full (test-cfg) test-sha256))
    (for ([needle (in-list (list "class RacketAT9 < Formula"
                                 generated-code-notice-marker
                                 "url \"https://github.com/CutieDeng/racket/releases/download/v9.2.2/racket-minimal-9.2.2-src.tgz\""
                                 f"sha256 \"{test-sha256}\""
                                 "version \"9.2.2\""
                                 "depends_on \"openssl@3\""
                                 "formula_opt_lib(\"openssl@3\")"
                                 "depends_on \"ncurses\""
                                 "depends_on \"zlib-ng-compat\""
                                 "require \"pty\""
                                 "require racket/pvector"
                                 "interactive-packages-ok"
                                 "rhombus-lang-ok"
	                                 "rhombus --version"
	                                 "rhombus -e '1 + 2'"
	                                 "compiled-file-cache-roots"
	                                 "(compiled-file-system-cache-root . \\\"#{system_cache_root}\\\")"
		                                 "system bin/\"raco\", \"setup\", \"--no-user\", \"--no-zo\"\n    configure_racket"
		                                 "racket_config.atomic_write content"
		                                 "if build.bottle?"
		                                 "system_cache_populated?"
		                                 "rhombus_demod_cache_populated?"
		                                 "package-racket-rhombus-cache"
			                                 "system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"raco\", \"-l-\", \"raco\", \"setup\",\n           \"--system\", \"--no-user\", \"--reset-cache\", \"-D\", \"--no-pkg-deps\", \"--no-launcher\""
		                                 "system_cache_root.mkpath"
		                                 "system bin/\"racket\", \"-U\", \"-R\", system_cache_root.to_s, \"-N\", \"rhombus\""
		                                 "\"-l-\", \"rhombus/run.rhm\", \"--version\""
		                                 "remove_precompiled_cache"
                                 "preserve_compiled_cache_dir?"
                                 "Dir[\"#{prefix}/**/compiled\"].sort_by(&:length).reverse_each"
                                 "prefix/\"var/cache/racket/compiled#{share}/racket/collects\""
	                                 "assert system_cache_populated?"
	                                 "assert rhombus_demod_cache_populated?"
                                 "brew-empty-home-ok"
                                 "printf 'f\\\"hi\\\""
                                 "refute_match(/no readline support/"
                                 "LD_DEBUG=libs"
                                 "DYLD_PRINT_LIBRARIES=1"))])
      (check-true (string-contains? content needle) needle)
    ) ; end for formula needle
    (check-true (formula-version-before-sha? content))
    (check-false (string-contains? content "inreplace racket_config, prefix, opt_prefix"))
	    (check-true (string-contains? content "content.sub!(/\\)\\s*\\z/"))
	    (check-false (string-contains? content "inreplace racket_config"))
    (check-false (string-contains? content "inreplace \"etc/config.rktd\""))
    (check-false (string-contains? content "Fixing up Cellar references"))
    (check-false (string-contains? content "Formula[\"openssl@3\"].opt_lib"))
    (check-false (string-contains? content "#{var}/cache/racket/compiled"))
    (check-false (string-contains? content "assert_match(/\\e\\["))
  ) ; end test-case full Formula template

  (test-case "within-docs Formula template checks raco docs command"
    (define content (formula-content/full (test-cfg #:with-docs? #t) test-sha256))
    (check-true (string-contains? content "raco docs --help"))
    (check-true (string-contains? content "raco doc --help"))
    (check-true (string-contains? content "assert_match \"search-terms\", output"))
    (define formula-path (make-temporary-file "package-racket-formula~a.rb"))
    (dynamic-wind
      void
      (lambda ()
        (write-text-file! formula-path content)
        (validate-formula-file! (test-cfg #:with-docs? #t) formula-path)
      ) ; end lambda validate formula
      (lambda ()
        (delete-file formula-path)
      ) ; end lambda cleanup formula
    ) ; end dynamic-wind formula
  ) ; end test-case within-docs Formula template

  (test-case "real rpm CI config avoids el9 minimal package conflicts"
    (define config (read-rktd-hash 'rpm-ci-config (build-path script-dir "rpm-ci-config.rktd")))
    (validate-rpm-ci-config! config)
    (define targets (rpm-ci-normalized-targets config))
    (define el9
      (for/first ([target (in-list targets)]
                  #:when (string=? (hash-ref target 'id) "el9-x86_64"))
        target
      ) ; end for/first el9 target
    ) ; end define el9
    (check-true (and el9 #t))
    (define el9-packages (hash-ref el9 'setup-packages))
    (check-false (member "coreutils" el9-packages string=?))
    (check-false (member "curl" el9-packages string=?))
    (check-true (and (member "findutils" el9-packages string=?) #t))
    (check-true (and (member "rpm-build" el9-packages string=?) #t))
  ) ; end test-case real rpm CI config el9

  (test-case "brew CI config and workflows keep bottle publication contract"
    (validate-brew-ci-config! test-brew-ci-config)
    (define c (test-cfg #:targets '("brew-ci")))
    (define tests-content (tests-workflow-content c test-brew-ci-config))
    (define publish-content (publish-workflow-content c test-brew-ci-config))
    (for ([needle (in-list (list "macos-26"
                                 generated-code-notice-marker
                                 "ubuntu-latest"
                                 "ubuntu-24.04-arm"
                                 "ghcr.io/homebrew/brew:main"
                                 "brew test-bot --only-tap-syntax --tap="
                                 "*.bottle*.tar.gz"
                                 "if-no-files-found: error"))])
      (check-true (string-contains? tests-content needle) needle)
    ) ; end for tests workflow needle
    (for ([needle (in-list (list "shell: bash"
                                 generated-code-notice-marker
                                 "set -euo pipefail"
                                 "BOTTLE_REBUILD: 1"
                                 "brew test-bot --only-tap-syntax --tap="
                                 "normalize_bottle_json"
                                 "normalized_bottle_jsons"
                                 "URI.decode_www_form_component"
                                 "Digest::SHA256.file"
                                 "brew bottle --merge --write --no-commit"
                                 "gh release upload"
                                 "((CI \"brew publish bottles\" update-bottle-metadata)"
                                 "\"[skip bottles]\""
                                 "\"[skip ci]\""
                                 "ubuntu-24.04-arm"))])
      (check-true (string-contains? publish-content needle) needle)
    ) ; end for publish workflow needle
  ) ; end test-case brew CI publication contract

  (test-case "brew CI config rejects missing bottle runners"
    (check-exn exn:fail:user?
               (lambda ()
                 (validate-brew-ci-config!
                  #hash((formula . "racket@9")
                        (artifact-prefix . "bottles")
                        (bottle-rebuild . 1)
                        (bottle-runners . ())
                        (syntax-runners . ()))))
    ) ; end check-exn missing bottle runners
  ) ; end test-case brew CI config validation
) ; end module+ test

(module+ main
  (main))
