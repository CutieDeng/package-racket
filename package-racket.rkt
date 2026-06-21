#lang reader tstring/lang/reader racket/base

(require file/tar
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
   deb-arch
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
   replace-release-asset
   ruby-bin
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
                        f"{label} must be a dotted numeric version such as 9.2.1 or 9.2.1.1: {version}")
    ) ; end unless dotted numeric version
    version
  ) ; end begin assert-version-string
) ; end define assert-version-string

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
      (raise-user-error 'main "missing --target; use brew, brew-ci, source-release, apt, apt-release, rpm, or all")
    ) ; end when missing target
    (define expanded
      (append-map
       (lambda (target)
         (match target
           ["all" '("brew" "apt" "rpm")]
           [(or "brew" "apt" "apt-release" "rpm" "brew-ci" "source-release") (list target)]
           [_ (raise-user-error 'main f"unknown --target: {target}")]
         ) ; end match target
       ) ; end lambda target
       pieces
      ) ; end append-map
    ) ; end define expanded
    (filter (lambda (target) (member target expanded string=?))
            '("brew-ci" "brew" "source-release" "apt" "apt-release" "rpm"))
  ) ; end begin normalize-targets
) ; end define normalize-targets

(define (upload-only-target? target)
  (or (string=? target "source-release")
      (string=? target "apt-release")))

(define (needs-racket-root? targets)
  (not (andmap upload-only-target? targets)))

(define (needs-homebrew-tap? targets)
  (or (member "brew" targets string=?)
      (member "brew-ci" targets string=?)))

(define (needs-bottle-root-url? targets)
  (or (member "brew" targets string=?)
      (member "brew-ci" targets string=?)))

(define (target-selected? c target)
  (and (member target (cfg-targets c) string=?) #t))

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

(define (path-contained-in? child parent)
  (define parent-str (path->string (path->directory-path (complete-path* parent))))
  (define child-str (path->string (complete-path* child)))
  (string-prefix? child-str parent-str))

(define (regexp-match-count rx content)
  (length (regexp-match* rx content)))

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

(define (write-rpm-spec! c spec-path source-name)
  (begin
    (write-text-file!
     spec-path
     f"Name: {(cfg-package-name c)}
Version: {(cfg-formula-version c)}
Release: {(cfg-release c)}
Summary: {(cfg-summary c)}
License: {(cfg-license c)}
URL: {(cfg-url c)}
Source0: {source-name}
AutoReqProv: no

%description
Racket packaged from a local checkout.

%prep

%build

%install
rm -rf %{{buildroot}}
mkdir -p %{{buildroot}}
tar -xzf %{{SOURCE0}} -C %{{buildroot}}

%files
%defattr(-,root,root,-)
{(cfg-prefix c)}
"
    )
    (validate-rpm-spec! c spec-path source-name)
  ) ; end begin write-rpm-spec!
) ; end define write-rpm-spec!

(define (validate-rpm-spec! c spec-path source-name)
  (begin
    (assert-nonempty-file 'validate-rpm-spec! spec-path)
    (define content (file->string spec-path))
    (for ([needle (in-list (list f"Name: {(cfg-package-name c)}"
                                 f"Version: {(cfg-formula-version c)}"
                                 f"Release: {(cfg-release c)}"
                                 f"Source0: {source-name}"
                                 "tar -xzf %{SOURCE0} -C %{buildroot}"
                                 f"{(cfg-prefix c)}"))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-rpm-spec!
                          f"generated RPM spec is missing: {needle}")
      ) ; end unless needle present
    ) ; end for needle
  ) ; end begin validate-rpm-spec!
) ; end define validate-rpm-spec!

(define (validate-rpm! c rpm-path)
  (begin
    (assert-nonempty-file 'validate-rpm! rpm-path)
    (define metadata
      (capture! 'validate-rpm! (cfg-rpm-bin c) (list "-qip" (clean-path-string rpm-path)))
    ) ; end define metadata
    (for ([needle (in-list (list f"Name        : {(cfg-package-name c)}"
                                 f"Version     : {(cfg-formula-version c)}"
                                 f"Release     : {(cfg-release c)}"))])
      (unless (string-contains? metadata needle)
        (raise-user-error 'validate-rpm!
                          f"RPM metadata is missing: {needle}")
      ) ; end unless metadata contains needle
    ) ; end for needle
    (println/flush f"Validated .rpm: {(clean-path-string rpm-path)}")
  ) ; end begin validate-rpm!
) ; end define validate-rpm!

(define (copy-built-rpms! c rpm-root)
  (begin
    (define rpms-dir (build-path rpm-root "RPMS"))
    (define rpms
      (sort (find-files (lambda (p)
                          (and (file-exists? p)
                               (regexp-match? #rx"[.]rpm$" (path->string p)))
                        ) ; end lambda p
                        rpms-dir)
            path<?)
    ) ; end define rpms
    (when (null? rpms)
      (raise-user-error 'build-rpm! f"rpmbuild produced no .rpm under {(clean-path-string rpms-dir)}")
    ) ; end when no rpms
    (for ([rpm (in-list rpms)])
      (define dest (build-path (cfg-artifact-dir c) (file-name-from-path rpm)))
      (copy-file rpm dest #t)
      (validate-rpm! c dest)
      (println/flush f"RPM package: {(clean-path-string dest)}")
    ) ; end for rpm
  ) ; end begin copy-built-rpms!
) ; end define copy-built-rpms!

(define (build-rpm! c)
  (begin
    (define install-root (cfg-install-root c))
    (define rpm-root (build-path (cfg-work-dir c) "rpm"))
    (define sources-dir (build-path rpm-root "SOURCES"))
    (define specs-dir (build-path rpm-root "SPECS"))
    (define source-name f"{(cfg-package-name c)}-{(cfg-formula-version c)}-payload.tar.gz")
    (define payload-tar (build-path sources-dir source-name))
    (define spec-path (build-path specs-dir f"{(cfg-package-name c)}.spec"))
    (unless (cfg-dry-run? c)
      (make-directory* (cfg-artifact-dir c))
      (assert-executable 'build-rpm! (cfg-rpmbuild-bin c))
      (assert-executable 'build-rpm! (cfg-rpm-bin c))
      (assert-executable 'build-rpm! (cfg-tar-bin c))
      (assert-nonempty-directory 'build-rpm! (prefix-install-path install-root (cfg-prefix c)))
      (reset-managed-dir! 'build-rpm! rpm-root)
      (for ([dir (in-list '("BUILD" "BUILDROOT" "RPMS" "SOURCES" "SPECS" "SRPMS"))])
        (make-directory* (build-path rpm-root dir))
      ) ; end for rpmbuild dir
    ) ; end unless dry-run prepare rpm
    (run! 'build-rpm!
          (cfg-tar-bin c)
          (list "-C" (clean-path-string install-root) "-czf" (clean-path-string payload-tar) ".")
          #:dry-run? (cfg-dry-run? c))
    (unless (cfg-dry-run? c)
      (write-rpm-spec! c spec-path source-name)
    ) ; end unless dry-run write spec
    (run! 'build-rpm!
          (cfg-rpmbuild-bin c)
          (list "-bb"
                "--target" (cfg-rpm-arch c)
                "--define" f"_topdir {(clean-path-string rpm-root)}"
                "--define" "_build_id_links none"
                (clean-path-string spec-path))
          #:dry-run? (cfg-dry-run? c))
    (unless (cfg-dry-run? c)
      (copy-built-rpms! c rpm-root)
    ) ; end unless dry-run copy rpms
  ) ; end begin build-rpm!
) ; end define build-rpm!

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

(define (generated-source-root)
  (regexp-replace #rx"/$"
                  (regexp-replace #rx"/[.]$" (clean-path-string script-dir) "")
                  ""))

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
    ("rackunit-lib" . root)
    ("testing-util-lib" . root)
    ("sandbox-lib" . root)
    ("errortrace-lib" . root)
    ("source-syntax" . "syntax")
    ("compiler-lib" . root)
    ("zo-lib" . root)))

(define brew-required-package-files
  '("share/pkgs/sandbox-lib/racket/sandbox.rkt"
    "share/pkgs/sandbox-lib/scheme/sandbox.rkt"
    "share/pkgs/errortrace-lib/errortrace/stacktrace.rkt"
    "share/pkgs/source-syntax/source-syntax.rkt"))

(define brew-required-link-needles
  '("root (#\"pkgs\" #\"sandbox-lib\")"
    "root (#\"pkgs\" #\"errortrace-lib\")"
    "\"syntax\" (#\"pkgs\" #\"source-syntax\")"))

(define brew-required-pkgs-db-needles
  '("\"sandbox-lib\""
    "\"errortrace-lib\""
    "\"source-syntax\""))

(define brew-racket-lib-excluded-dependency
  "(\"racket-aarch64-macosx-4\" #:platform \"aarch64-macosx\")")

(define brew-racket-lib-excluded-dependency-line
  f"    {brew-racket-lib-excluded-dependency}
")

(define (brew-source-packages c)
  (remove-duplicates
   (append brew-default-packages brew-custom-core-packages (cfg-brew-packages c))
   string=?))

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
    (copy-brew-tree! (build-path (cfg-racket-root c) "racket" "src")
                     (build-path dist-root "src")
                     #:skip-first-components '("build"))
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
    (for ([relative-path (in-list brew-required-package-files)])
      (validate-brew-tgz-file! c relative-path)
    ) ; end for required package files
    (define links-content (brew-tgz-file-content c "share/links.rktd"))
    (for ([needle (in-list brew-required-link-needles)])
      (unless (string-contains? links-content needle)
        (raise-user-error 'validate-brew-tgz!
                          f"brew source tgz links.rktd is missing: {needle}")
      ) ; end unless link needle
    ) ; end for required links
    (define pkgs-db-content (brew-tgz-file-content c "share/pkgs/pkgs.rktd"))
    (for ([needle (in-list brew-required-pkgs-db-needles)])
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
                                 "test do"
                                 f"assert_match \"{(cfg-source-version c)}\""))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-formula-file!
                          f"formula is missing expected content: {needle}")
      ) ; end unless needle present
    ) ; end for formula needle
    (unless (= 1 (regexp-match-count #px"(?m:^  sha256 \"[0-9a-f]{64}\")" content))
      (raise-user-error 'validate-formula-file!
                        f"formula must contain exactly one source sha256 line: {(clean-path-string formula-path)}")
    ) ; end unless one source sha
    (unless (= 1 (regexp-match-count #px"(?m:^  version \"[^\"]+\")" content))
      (raise-user-error 'validate-formula-file!
                        f"formula must contain exactly one version line: {(clean-path-string formula-path)}")
    ) ; end unless one formula version
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

(define (set-formula-source! c formula-path digest)
  (begin
    (assert-nonempty-file 'set-formula-source! formula-path)
    (define content (file->string formula-path))
    (define source-url-rx #px"(?m:^  url \"[^\"]+racket-minimal-[^\"]+-src[.]tgz\")")
    (define source-sha-rx #px"(?m:^  sha256 \"[0-9a-f]{64}\")")
    (define formula-version-rx #px"(?m:^  version \"[^\"]+\")")
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
    (define with-source-sha
      (regexp-replace source-sha-rx with-source-url (formula-source-sha256-line digest))
    ) ; end define with-source-sha
    (define with-version
      (if (regexp-match? formula-version-rx with-source-sha)
          (regexp-replace formula-version-rx with-source-sha (formula-version-line c))
          (regexp-replace source-sha-rx
                          with-source-sha
                          f"{(formula-source-sha256-line digest)}
{(formula-version-line c)}"))
    ) ; end define with-version
    (write-text-file!
     formula-path
     with-version)
    (validate-formula-file! c formula-path)
  ) ; end begin set-formula-source!
) ; end define set-formula-source!

(define (ruby-interpolate expression)
  (string-append "#{" expression "}"))

(define (formula-content/full c digest)
  (begin
    (define version (cfg-source-version c))
    (define rb-prefix (ruby-interpolate "prefix"))
    (define rb-man (ruby-interpolate "man"))
    (define rb-etc (ruby-interpolate "etc"))
    (define rb-openssl-rpath (ruby-interpolate "Formula[\"openssl@3\"].opt_lib"))
    (define rb-openssl-libssl (ruby-interpolate "Formula[\"openssl@3\"].opt_lib/shared_library(\"libssl\")"))
    (define rb-bin (ruby-interpolate "bin"))
    (define rb-test-script (ruby-interpolate "testpath/\"interactive-packages.rkt\""))
    (define rb-racket-config (ruby-interpolate "racket_config"))
    (define rb-cellar-regexp
      (string-append "%r{"
                     (ruby-interpolate "Regexp.escape(HOMEBREW_CELLAR)")
                     "/racket@9/[^/]+}o"))
    (define macos-openssl-rx "%r{.*openssl@3/.*/libssl.*\\.dylib}")
    f"{(generated-code-notice "#")}class RacketAT9 < Formula
  desc \"Modern programming language in the Lisp/Scheme family\"
  homepage \"https://racket-lang.org/\"
  url \"{(formula-source-url c)}\"
  sha256 \"{digest}\"
  version \"{(cfg-formula-version c)}\"
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

  def install
    # Configure racket's package tool (raco) to use installation scope.
    inreplace \"etc/config.rktd\", /\\)\\)\\n$/, \") (default-scope . \\\"installation\\\"))\\n\"

    # Prefer Homebrew OpenSSL 3 over older OpenSSL variants.
    inreplace %w[libssl.rkt libcrypto.rkt].map {{ |file| buildpath/\"collects/openssl\"/file }},
              '\"1.1\"', '\"3\"'

    cd \"src\" do
      args = %W[
        --disable-debug
        --disable-dependency-tracking
        --enable-origtree=no
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
        openssl = Formula[\"openssl@3\"]
        racket_libdir = lib/\"racket\"

        %w[libssl.3.dylib libcrypto.3.dylib].each do |dylib|
          path = racket_libdir/dylib
          path.unlink if path.exist?
        end

        ln_s openssl.opt_lib/\"libssl.3.dylib\",    racket_libdir/\"libssl.3.dylib\"
        ln_s openssl.opt_lib/\"libcrypto.3.dylib\", racket_libdir/\"libcrypto.3.dylib\"
      end
    end

    inreplace racket_config, prefix, opt_prefix
  end

  def post_install
    system bin/\"raco\", \"setup\"

    return unless racket_config.read.include?(HOMEBREW_CELLAR)

    ohai \"Fixing up Cellar references in {rb-racket-config}...\"
    inreplace racket_config, {rb-cellar-regexp}, opt_prefix
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

    assert_match \"{version}\", shell_output(\"{rb-bin}/racket -e '(displayln (version))'\")

    output = shell_output(\"{rb-bin}/racket -e '(require racket/pvector) (displayln (pvector->list (pvector 1 2 3)))'\")
    assert_match \"(1 2 3)\", output

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
  (list "User-Agent: package-racket"
        f"Accept: {accept}"
        f"Authorization: Bearer {token}"
        f"X-GitHub-Api-Version: {github-api-version}"))

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

      - run: brew test-bot --only-tap-syntax

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
    (define release-tag (github-release-tag-from-root-url 'publish-workflow-content root-url))
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

      - run: brew test-bot --only-tap-syntax

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

          gh release view \"$RELEASE_TAG\" >/dev/null

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
  (define prefix-arg "/opt/racket9")
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
  (define deb-arch-arg "amd64")
  (define rpm-arch-arg "x86_64")
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
  (define replace-release-asset-arg 'unset)
  (define ruby-bin-arg "ruby")
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
   [("--release") release "Package release value (default: 1)"
                 (set! release-arg release)]
   [("--prefix") path "Install prefix inside the package (default: /opt/racket9)"
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
   [("--skip-build") "Package an existing --install-root instead of running make unix-style"
                    (set! skip-build? #t)]
   [("--keep-work") "Keep generated working directories after success"
                  (set! keep-work? #t)]
   [("--dry-run") "Print commands and resolved paths without writing package artifacts"
                (set! dry-run? #t)]
   [("--make-bin") path "make executable (default: make)"
                 (set! make-bin-arg path)]
   [("--tar-bin") path "tar executable for RPM payloads (default: tar)"
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
   [("--deb-arch") arch "Debian architecture (default: amd64)"
                  (set! deb-arch-arg arch)]
   [("--rpm-arch") arch "RPM target architecture (default: x86_64)"
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
   [("--replace-release-asset") "Delete an existing differing GitHub release asset before uploading"
                              (set! replace-release-asset-arg #t)]
   [("--no-replace-release-asset") "Refuse to replace an existing differing GitHub release asset"
                                 (set! replace-release-asset-arg #f)]
   [("--ruby-bin") path "Ruby executable for YAML validation (default: ruby)"
                  (set! ruby-bin-arg path)]
   #:multi
   [("--target") target "Packaging target: brew, brew-ci, source-release, apt, apt-release, rpm, or all. May be repeated."
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
       deb-arch-arg
       rpm-arch-arg
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
       replace-release-asset-arg
       ruby-bin-arg
       brew-package-args
       make-args
  ) ; end cfg
) ; end define make-config

(define (needs-install-root? targets)
  (or (member "apt" targets string=?)
      (member "rpm" targets string=?)))

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
  (when (cfg-bottle-root-url c)
    (println/flush f"Bottle root URL: {(cfg-bottle-root-url c)}")
  ) ; end when bottle root url
  (when (member "brew" (cfg-targets c) string=?)
    (println/flush f"Formula build mode: {(cfg-formula-build-mode c)}")
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
         ["rpm" (build-rpm! c) '()]
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
    (delete-managed-dir-if-present! (build-path (cfg-stage-dir c) "brew-source"))
  ) ; end unless cleanup work dirs
  (println/flush "Done.")
) ; end define main

(module+ test
  (define test-root (find-system-path 'temp-dir))
  (define test-bottle-root-url "https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1")
  (define test-sha256 (make-string 64 #\a))

  (define (test-cfg #:targets [targets '("brew")]
                    #:dry-run? [dry-run? #t]
                    #:update-formula? [update-formula? #t]
                    #:formula-build-mode [formula-build-mode "full"]
                    #:source-version [source-version "9.2.1"]
                    #:formula-version [formula-version "9.2.1"]
                    #:brew-packages [brew-packages '()])
    (cfg targets
         (build-path test-root "racket-root")
         (build-path test-root "racket-root")
         (build-path test-root "package-config.rktd")
         source-version
         formula-version
         "racket9"
         "1"
         "/opt/racket9"
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
         "amd64"
         "x86_64"
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
         'unset
         "ruby"
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

  (test-case "brew target names and package closure stay stable"
    (define c (test-cfg #:brew-packages '("sandbox-lib" "custom-extra")))
    (define packages (brew-source-packages c))
    (check-equal? (normalize-targets '("all" "brew-ci" "source-release" "apt-release"))
                  '("brew-ci" "brew" "source-release" "apt" "apt-release" "rpm"))
    (check-equal? (brew-source-tgz-name c) "racket-minimal-9.2.1-src.tgz")
    (check-true (and (member "sandbox-lib" packages string=?) #t))
    (check-true (and (member "errortrace-lib" packages string=?) #t))
    (check-true (and (member "source-syntax" packages string=?) #t))
    (check-true (and (member "custom-extra" packages string=?) #t))
    (check-equal? (count (lambda (name) (string=? name "sandbox-lib")) packages) 1)
    (check-false (member "racket-aarch64-macosx-4" packages string=?))
  ) ; end test-case brew package closure

  (test-case "formula-version drives package-manager outputs without changing runtime version"
    (define c (test-cfg #:source-version "9.2.1"
                        #:formula-version "9.2.1.1"))
    (check-equal? (brew-source-tgz-name c) "racket-minimal-9.2.1-src.tgz")
    (check-equal? (apt-deb-name c) "racket9_9.2.1.1-1_amd64.deb")
    (check-equal? (brew-tgz-member-path c "src/README.txt")
                  "racket-9.2.1/src/README.txt")
    (define content (formula-content/full c test-sha256))
    (check-true
     (string-contains? content
                       "url \"https://github.com/CutieDeng/racket/releases/download/v9.2.1/racket-minimal-9.2.1-src.tgz\""))
    (check-true (string-contains? content "version \"9.2.1.1\""))
    (check-true (string-contains? content "assert_match \"9.2.1\""))
    (check-false (string-contains? content "Welcome to Racket v9.2.1.1 [cs]."))
    (define publish-content (publish-workflow-content c test-brew-ci-config))
    (check-true (string-contains? publish-content "RELEASE_TAG: v9.2.1"))
    (define rpm-root (make-temporary-file "package-racket-rpm-spec~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (define spec-path (build-path rpm-root "racket9.spec"))
        (write-rpm-spec! c spec-path "racket9-9.2.1.1-payload.tar.gz")
        (define spec-content (file->string spec-path))
        (check-true (string-contains? spec-content "Version: 9.2.1.1"))
        (check-true (string-contains? spec-content "Source0: racket9-9.2.1.1-payload.tar.gz"))
      ) ; end lambda write rpm spec
      (lambda ()
        (delete-directory/files rpm-root)
      ) ; end lambda cleanup rpm spec
    ) ; end dynamic-wind rpm spec
  ) ; end test-case formula-version package-manager outputs

  (test-case "full brew Formula template keeps runtime checks and dependencies"
    (define content (formula-content/full (test-cfg) test-sha256))
    (for ([needle (in-list (list "class RacketAT9 < Formula"
                                 generated-code-notice-marker
                                 "url \"https://github.com/CutieDeng/racket/releases/download/v9.2.1/racket-minimal-9.2.1-src.tgz\""
                                 f"sha256 \"{test-sha256}\""
                                 "version \"9.2.1\""
                                 "depends_on \"openssl@3\""
                                 "depends_on \"ncurses\""
                                 "depends_on \"zlib-ng-compat\""
                                 "require \"pty\""
                                 "require racket/pvector"
                                 "interactive-packages-ok"
                                 "printf 'f\\\"hi\\\""
                                 "refute_match(/no readline support/"
                                 "LD_DEBUG=libs"
                                 "DYLD_PRINT_LIBRARIES=1"))])
      (check-true (string-contains? content needle) needle)
    ) ; end for formula needle
    (check-false (string-contains? content "assert_match(/\\e\\["))
  ) ; end test-case full Formula template

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
                                 "*.bottle*.tar.gz"
                                 "if-no-files-found: error"))])
      (check-true (string-contains? tests-content needle) needle)
    ) ; end for tests workflow needle
    (for ([needle (in-list (list "shell: bash"
                                 generated-code-notice-marker
                                 "set -euo pipefail"
                                 "BOTTLE_REBUILD: 1"
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
