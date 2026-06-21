#lang reader tstring/lang/reader racket/base

(require racket/cmdline
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
   version
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
   homebrew-tap
   brew-helper
   formula
   update-formula?
   racket-bin
   brew-ci-config
   ruby-bin
   brew-packages
   make-args)
  #:transparent)

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
      (raise-user-error 'main "missing --target; use brew, brew-ci, apt, rpm, or all")
    ) ; end when missing target
    (define expanded
      (append-map
       (lambda (target)
         (match target
           ["all" '("brew" "apt" "rpm")]
           [(or "brew" "apt" "rpm" "brew-ci") (list target)]
           [_ (raise-user-error 'main f"unknown --target: {target}")]
         ) ; end match target
       ) ; end lambda target
       pieces
      ) ; end append-map
    ) ; end define expanded
    (filter (lambda (target) (member target expanded string=?))
            '("brew-ci" "brew" "apt" "rpm"))
  ) ; end begin normalize-targets
) ; end define normalize-targets

(define (needs-homebrew-tap? targets)
  (or (member "brew" targets string=?)
      (member "brew-ci" targets string=?)))

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
Version: {(cfg-version c)}-{(cfg-release c)}
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
                                 f"Version: {(cfg-version c)}-{(cfg-release c)}"
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

(define (build-apt! c)
  (begin
    (define install-root (cfg-install-root c))
    (define deb-root (build-path (cfg-work-dir c) "apt-root"))
    (define deb-name f"{(cfg-package-name c)}_{(cfg-version c)}-{(cfg-release c)}_{(cfg-deb-arch c)}.deb")
    (define deb-path (build-path (cfg-artifact-dir c) deb-name))
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
Version: {(cfg-version c)}
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
                                 f"Version: {(cfg-version c)}"
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
                                 f"Version     : {(cfg-version c)}"
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
    (define source-name f"{(cfg-package-name c)}-{(cfg-version c)}-payload.tar.gz")
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

(define (brew-source-tgz-name c)
  f"racket-minimal-{(cfg-version c)}-src.tgz")

(define (brew-output-tgz c)
  (build-path (cfg-artifact-dir c) (brew-source-tgz-name c)))

(define (assert-homebrew-tap! c)
  (begin
    (assert-directory 'assert-homebrew-tap! (cfg-homebrew-tap c))
    (assert-directory 'assert-homebrew-tap! (build-path (cfg-homebrew-tap c) ".git"))
    (assert-directory 'assert-homebrew-tap! (build-path (cfg-homebrew-tap c) "Formula"))
    (assert-file 'assert-homebrew-tap! (cfg-brew-helper c))
    (when (cfg-update-formula? c)
      (assert-file 'assert-homebrew-tap! (cfg-formula c))
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
  f"https://github.com/CutieDeng/racket/releases/download/v{(cfg-version c)}/{(brew-source-tgz-name c)}")

(define (formula-root-url c)
  f"root_url \"https://github.com/CutieDeng/racket/releases/download/v{(cfg-version c)}\"")

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
                                 f"url \"{(formula-source-url c)}\""
                                 (formula-root-url c)
                                 "test do"
                                 f"assert_match \"{(cfg-version c)}\""))])
      (unless (string-contains? content needle)
        (raise-user-error 'validate-formula-file!
                          f"formula is missing expected content: {needle}")
      ) ; end unless needle present
    ) ; end for formula needle
    (unless (= 1 (regexp-match-count #px"(?m:^  sha256 \"[0-9a-f]{64}\")" content))
      (raise-user-error 'validate-formula-file!
                        f"formula must contain exactly one source sha256 line: {(clean-path-string formula-path)}")
    ) ; end unless one source sha
    (unless (= 1 (regexp-match-count #px"(?m:^    root_url \"[^\"]+\")" content))
      (raise-user-error 'validate-formula-file!
                        f"formula must contain exactly one bottle root_url line: {(clean-path-string formula-path)}")
    ) ; end unless one root_url
    (formula-sha256 formula-path)
    (void)
  ) ; end begin validate-formula-file!
) ; end define validate-formula-file!

(define (validate-formula-template! formula-path)
  (begin
    (assert-nonempty-file 'validate-formula-template! formula-path)
    (define content (file->string formula-path))
    (for ([needle (in-list (list "class RacketAT9 < Formula"
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
    (unless (= 1 (regexp-match-count #px"(?m:^    root_url \"[^\"]+\")" content))
      (raise-user-error 'validate-formula-template!
                        f"formula template must contain exactly one bottle root_url line: {(clean-path-string formula-path)}")
    ) ; end unless one root_url
  ) ; end begin validate-formula-template!
) ; end define validate-formula-template!

(define (validate-brew-tgz! c)
  (begin
    (define tgz-path (brew-output-tgz c))
    (assert-nonempty-file 'validate-brew-tgz! tgz-path)
    (unless (equal? (file-name-from-path tgz-path) (string->path (brew-source-tgz-name c)))
      (raise-user-error 'validate-brew-tgz!
                        f"brew source tgz name must be {(brew-source-tgz-name c)}: {(clean-path-string tgz-path)}")
    ) ; end unless brew tgz basename matches formula
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

(define (prepare-generated-formula! c)
  (begin
    (assert-homebrew-tap! c)
    (define work-root (brew-work-root c))
    (define generated (brew-generated-formula c))
    (define original-digest (sha256-file (cfg-formula c)))
    (reset-managed-dir! 'prepare-generated-formula! work-root)
    (make-directory* (path-only generated))
    (copy-file (cfg-formula c) generated #t)
    (validate-formula-template! generated)
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
    (define current-digest (sha256-file dest))
    (unless (string=? current-digest original-digest)
      (raise-user-error 'install-generated-formula!
                        f"refusing to replace formula because it changed during this run: {(clean-path-string dest)}")
    ) ; end unless formula unchanged
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
    (define helper (cfg-brew-helper c))
    (define brew-stage (build-path (cfg-stage-dir c) "brew-source"))
    (define finalizers '())
    (define formula-for-helper #f)
    (unless (cfg-dry-run? c)
      (assert-homebrew-tap! c)
      (assert-file 'build-brew! (cfg-racket-bin c))
      (make-directory* (cfg-artifact-dir c))
      (when (cfg-update-formula? c)
        (define-values (generated original-digest)
          (prepare-generated-formula! c)
        ) ; end define-values generated/original-digest
        (set! formula-for-helper generated)
        (set! finalizers
              (list (lambda ()
                      (install-generated-formula! c generated original-digest)
                    ) ; end lambda install generated formula
              ) ; end list finalizer
        ) ; end set! finalizers
      ) ; end when update formula
    ) ; end unless dry-run prepare brew
    (when (and (cfg-dry-run? c) (cfg-update-formula? c))
      (set! formula-for-helper (brew-generated-formula c))
      (set! finalizers
            (list (lambda ()
                    (println/flush f"Would install brew formula: {(clean-path-string (cfg-formula c))}")
                  ) ; end lambda dry-run install notice
            ) ; end list dry-run finalizer
      ) ; end set! dry-run finalizers
    ) ; end when dry-run update formula
    (define args
      (append
       (list (clean-path-string helper)
             "--racket-root" (clean-path-string (cfg-racket-root c))
             "--artifact-dir" (clean-path-string (cfg-artifact-dir c))
             "--stage-dir" (clean-path-string brew-stage)
             "--version" (cfg-version c))
       (if (cfg-update-formula? c)
           (list "--formula" (clean-path-string formula-for-helper))
           (list "--no-update-formula"))
       (append-map (lambda (pkg) (list "--package" pkg)) (cfg-brew-packages c))
      ) ; end append brew args
    ) ; end define args
    (run! 'build-brew! (clean-path-string (cfg-racket-bin c)) args #:dry-run? (cfg-dry-run? c))
    (unless (cfg-dry-run? c)
      (if (cfg-update-formula? c)
          (validate-brew-artifact! c formula-for-helper)
          (validate-brew-tgz! c))
    ) ; end unless dry-run validate brew
    finalizers
  ) ; end begin build-brew!
) ; end define build-brew!

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
    (required-config-string config 'publish-label)
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

(define (tests-workflow-content c config)
  (begin
    (define formula (required-config-string config 'formula))
    (define artifact-prefix (required-config-string config 'artifact-prefix))
    (define bottle-runners (config-ref* config 'bottle-runners '()))
    (define syntax-runners (config-ref* config 'syntax-runners '()))
    (define root-url (formula-source-root-url c))
    (define matrix-os "${{ matrix.os }}")
    (define container-expr "${{ matrix.container }}")
    (define token-expr "${{ secrets.GITHUB_TOKEN }}")
    (define test-formula-if "matrix.test_formula")
    (define event-name-if "github.event_name")
    (define runner-lines
      (string-append
       (apply string-append (map (lambda (runner) (workflow-runner-lines runner #t)) bottle-runners))
       (apply string-append (map (lambda (runner) (workflow-runner-lines runner #f)) syntax-runners))
      ) ; end string-append runner lines
    ) ; end define runner-lines
    f"name: brew test-bot

on:
  push:
    branches:
      - main
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
        if: always() && {event-name-if} == 'pull_request'
        uses: actions/upload-artifact@v6
        with:
          name: {artifact-prefix}_{matrix-os}
          path: '*.bottle.*'
"
  ) ; end begin tests-workflow-content
) ; end define tests-workflow-content

(define (publish-workflow-content c config)
  (begin
    (define publish-label (required-config-string config 'publish-label))
    (define root-url (formula-source-root-url c))
    (define label-if "github.event.pull_request.labels.*.name")
    (define token-expr "${{ secrets.GITHUB_TOKEN }}")
    (define repo-expr "$GITHUB_REPOSITORY")
    (define pr-expr "${{ github.event.pull_request.number }}")
    (define fork-if "github.event.pull_request.head.repo.fork == false")
    (define branch-expr "${{ github.event.pull_request.head.ref }}")
    f"name: brew pr-pull

on:
  pull_request_target:
    types:
      - labeled

jobs:
  pr-pull:
    if: contains({label-if}, '{publish-label}')
    runs-on: ubuntu-latest
    permissions:
      actions: read
      checks: read
      contents: write
      issues: read
      pull-requests: write
    steps:
      - name: Set up Homebrew
        uses: Homebrew/actions/setup-homebrew@main
        with:
          token: {token-expr}

      - name: Set up git
        uses: Homebrew/actions/git-user-config@main

      - name: Pull bottles
        env:
          HOMEBREW_GITHUB_API_TOKEN: {token-expr}
          PULL_REQUEST: {pr-expr}
        run: brew pr-pull --debug --tap=\"{repo-expr}\" --root-url=\"{root-url}\" \"$PULL_REQUEST\"

      - name: Push commits
        uses: Homebrew/actions/git-try-push@main
        with:
          branch: main

      - name: Delete branch
        if: {fork-if}
        env:
          BRANCH: {branch-expr}
        run: git push --delete origin \"$BRANCH\"
"
  ) ; end begin publish-workflow-content
) ; end define publish-workflow-content

(define (formula-source-root-url c)
  f"https://github.com/CutieDeng/racket/releases/download/v{(cfg-version c)}")

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
                                 f"--testing-formulae={formula}"
                                 f"--root-url={(formula-source-root-url c)}"
                                 "test_formula: true"
                                 "if: matrix.test_formula"
                                 "if: always() && github.event_name == 'pull_request'"
                                 "actions/upload-artifact@v6"))])
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
    (define publish-label (required-config-string config 'publish-label))
    (for ([needle (in-list (list "name: brew pr-pull"
                                 f"contains(github.event.pull_request.labels.*.name, '{publish-label}')"
                                 "brew pr-pull --debug"
                                 f"--root-url=\"{(formula-source-root-url c)}\""
                                 "if: github.event.pull_request.head.repo.fork == false"
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
        (let-values ([(tap-tests tap-publish) (tap-workflow-paths c)])
          (println/flush f"Would install brew CI workflow: {(clean-path-string tap-tests)}")
          (println/flush f"Would install brew CI workflow: {(clean-path-string tap-publish)}")
          '()
        ) ; end let-values dry-run tap workflow paths
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
  (define version-arg #f)
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
  (define homebrew-tap-arg #f)
  (define brew-helper-arg #f)
  (define formula-arg #f)
  (define update-formula? #t)
  (define racket-bin-arg #f)
  (define brew-ci-config-arg #f)
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
   [("--version") version "Override version derived from racket_version.h"
                (set! version-arg version)]
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
   [("--homebrew-tap") path "Required for brew and brew-ci; Homebrew tap root"
                      (set! homebrew-tap-arg path)]
   [("--brew-helper") path "Homebrew source helper (derived from --homebrew-tap when omitted)"
                    (set! brew-helper-arg path)]
   [("--formula") path "Homebrew formula to update (derived from --homebrew-tap when omitted)"
                (set! formula-arg path)]
   [("--no-update-formula") "Do not update the Homebrew formula"
                          (set! update-formula? #f)]
   [("--racket-bin") path "Racket executable for running the brew helper (default: --racket-root/racket/bin/racket)"
                  (set! racket-bin-arg path)]
   [("--brew-ci-config") path "Package-racket source config for generated tap workflows (default: ./brew-ci-config.rktd)"
                       (set! brew-ci-config-arg path)]
   [("--ruby-bin") path "Ruby executable for YAML validation (default: ruby)"
                  (set! ruby-bin-arg path)]
   #:multi
   [("--target") target "Packaging target: brew, brew-ci, apt, rpm, or all. May be repeated."
                (set! target-args (append target-args (list target)))]
   [("--brew-package") name "Extra package to pass to the brew source helper"
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
  (when (and (needs-homebrew-tap? targets) (not homebrew-tap-arg))
    (raise-user-error 'main "--homebrew-tap is required when --target includes brew or brew-ci")
  ) ; end when missing homebrew tap
  (define homebrew-tap (and homebrew-tap-arg (complete-path* homebrew-tap-arg)))
  (define brew-helper
    (cond
      [brew-helper-arg
       (complete-path* brew-helper-arg)]
      [homebrew-tap
       (complete-path* (build-path homebrew-tap "racket-to-brew-tgz.rkt"))]
      [else
       #f]
    ) ; end cond brew helper path
  ) ; end define brew-helper
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
  (define racket-bin (complete-path* (or racket-bin-arg (build-path racket-root "racket" "bin" "racket"))))
  (define brew-ci-config (complete-path* (or brew-ci-config-arg (build-path script-dir "brew-ci-config.rktd"))))
  (assert-prefix prefix-arg)
  (assert-racket-root racket-root)
  (cfg targets
       racket-root
       make-dir
       (or version-arg (read-racket-version racket-root))
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
       homebrew-tap
       brew-helper
       formula
       update-formula?
       racket-bin
       brew-ci-config
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
  (println/flush f"Racket root: {(clean-path-string (cfg-racket-root c))}")
  (println/flush f"Version: {(cfg-version c)}")
  (println/flush f"Artifact dir: {(clean-path-string (cfg-artifact-dir c))}")
  (println/flush f"Work dir: {(clean-path-string (cfg-work-dir c))}")
  (println/flush f"Install root: {(clean-path-string (cfg-install-root c))}")
  (println/flush f"Prefix: {(cfg-prefix c)}")
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
         ["apt" (build-apt! c) '()]
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
  ) ; end unless cleanup work dirs
  (println/flush "Done.")
) ; end define main

(main)
