#lang reader tstring/lang/reader racket/base

(require rackunit
         racket/file
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system)

(define-runtime-path package-root "..")
(define package-script (build-path package-root "package-racket.rkt"))
(define racket-bin
  (or (find-executable-path "racket")
      (raise-user-error 'package-racket-test "racket executable not found")))

(define (path-arg path)
  (path->string (simplify-path (path->complete-path path))))

(define (write-text! path content)
  (begin
    (make-directory* (or (path-only path) (current-directory)))
    (call-with-output-file path
      #:exists 'truncate/replace
      (lambda (out)
        (display content out)
      ) ; end lambda out
    ) ; end call-with-output-file
  ) ; end begin write-text!
) ; end define write-text!

(define (with-temp-dir proc)
  (begin
    (define dir (make-temporary-file "package-racket-test~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (proc dir)
      ) ; end lambda run temp test
      (lambda ()
        (delete-directory/files dir)
      ) ; end lambda cleanup temp dir
    ) ; end dynamic-wind
  ) ; end begin with-temp-dir
) ; end define with-temp-dir

(define (make-fake-racket-root! base)
  (begin
    (define root (build-path base "racket-root"))
    (make-directory* (build-path root "racket" "src" "version"))
    (make-directory* (build-path root "racket" "collects"))
    (write-text!
     (build-path root "racket" "src" "version" "racket_version.h")
     "#define MZSCHEME_VERSION_X 9
#define MZSCHEME_VERSION_Y 2
#define MZSCHEME_VERSION_Z 1
#define MZSCHEME_VERSION_W 0
")
    root
  ) ; end begin make-fake-racket-root!
) ; end define make-fake-racket-root!

(define (write-source-release-config! path asset-name)
  (write-text!
   path
   f"#hash((source-release-repo . \"CutieDeng/racket\")
      (source-release-tag . \"v9.2.1\")
      (source-release-asset . \"{asset-name}\")
      (source-release-token-file . \"missing-token.rktd\")
      (replace-release-asset . #t))
"))

(define (write-apt-release-config! path asset-name)
  (write-text!
   path
   f"#hash((apt-release-repo . \"CutieDeng/racket\")
      (apt-release-tag . \"v9.2.1\")
      (apt-release-asset . \"{asset-name}\")
      (apt-release-token-file . \"missing-token.rktd\")
      (replace-release-asset . #t))
"))

(define (run-command! who program args #:cwd [cwd #f])
  (begin
    (define out (open-output-string))
    (define err (open-output-string))
    (define exit-code
      (parameterize ([current-output-port out]
                     [current-error-port err]
                     [current-input-port (open-input-string "")]
                     [current-directory (or cwd (current-directory))])
        (apply system*/exit-code program args)
      ) ; end parameterize command
    ) ; end define exit-code
    (unless (zero? exit-code)
      (raise-user-error who
                        f"command failed with exit {exit-code}: {(string-join args " ")}
stdout:
{(get-output-string out)}
stderr:
{(get-output-string err)}")
    ) ; end unless command success
  ) ; end begin run-command!
) ; end define run-command!

(define (make-fake-deb! deb-path)
  (begin
    (define ar-bin
      (or (find-executable-path "ar")
          (raise-user-error 'make-fake-deb! "ar executable not found")))
    (define parts (make-temporary-file "package-racket-deb-parts~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (write-text! (build-path parts "debian-binary") "2.0\n")
        (write-text! (build-path parts "control.tar.xz") "control")
        (write-text! (build-path parts "data.tar.xz") "data")
        (make-directory* (or (path-only deb-path) (current-directory)))
        (run-command!
         'make-fake-deb!
         ar-bin
         (list "-qSc"
               (path-arg deb-path)
               "debian-binary"
               "control.tar.xz"
               "data.tar.xz")
         #:cwd parts)
      ) ; end lambda make deb
      (lambda ()
        (delete-directory/files parts)
      ) ; end lambda cleanup deb parts
    ) ; end dynamic-wind
  ) ; end begin make-fake-deb!
) ; end define make-fake-deb!

(define (run-package args)
  (begin
    (define out (open-output-string))
    (define err (open-output-string))
    (define exit-code
      (parameterize ([current-output-port out]
                     [current-error-port err]
                     [current-input-port (open-input-string "")]
                     [current-directory package-root])
        (apply system*/exit-code
               racket-bin
               (cons (path-arg package-script) args))
      ) ; end parameterize package run
    ) ; end define exit-code
    (values exit-code (get-output-string out) (get-output-string err))
  ) ; end begin run-package
) ; end define run-package

(define (run-package/success args)
  (begin
    (define-values (exit-code out err) (run-package args))
    (check-equal? exit-code
                  0
                  f"package-racket failed
stdout:
{out}
stderr:
{err}")
    (values out err)
  ) ; end begin run-package/success
) ; end define run-package/success

(define (combined-output out err)
  f"{out}
{err}")

(define (check-contains text needle)
  (check-true (and (string-contains? text needle) #t)
              f"missing output text: {needle}
actual output:
{text}"))

(define (check-not-contains text needle)
  (check-false (and (string-contains? text needle) #t)
               f"unexpected output text: {needle}
actual output:
{text}"))

(module+ test
  (test-case "apt dry-run is isolated from apt-release config and writes no artifacts"
    (with-temp-dir
     (lambda (tmp)
       (define racket-root (make-fake-racket-root! tmp))
       (define artifact-dir (build-path tmp "artifacts"))
       (define work-dir (build-path tmp "work"))
       (define-values (out err)
         (run-package/success
          (list "--target" "apt"
                "--racket-root" (path-arg racket-root)
                "--artifact-dir" (path-arg artifact-dir)
                "--work-dir" (path-arg work-dir)
                "--apt-release-config" (path-arg (build-path tmp "missing-apt-release.rktd"))
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: apt")
       (check-contains text "APT package:")
       (check-contains text "racket9_9.2.1-1_amd64.deb")
       (check-not-contains text "APT release config:")
       (check-false (directory-exists? artifact-dir))
       (check-false (directory-exists? work-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case apt dry-run isolation

  (test-case "apt-release dry-run validates an existing deb and does not read token"
    (with-temp-dir
     (lambda (tmp)
       (define artifact-dir (build-path tmp "artifacts"))
       (define deb-name "racket9_9.2.1-1_amd64.deb")
       (define deb-path (build-path artifact-dir deb-name))
       (define config-path (build-path tmp "apt-release-config.rktd"))
       (make-fake-deb! deb-path)
       (write-apt-release-config! config-path deb-name)
       (define-values (out err)
         (run-package/success
          (list "--target" "apt-release"
                "--artifact-dir" (path-arg artifact-dir)
                "--apt-release-config" (path-arg config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: apt-release")
       (check-contains text "Validated .deb:")
       (check-contains text "Would upload apt release asset from")
       (check-not-contains text "read-github-token")
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case apt-release existing deb dry-run

  (test-case "apt plus apt-release dry-run uses planned apt output"
    (with-temp-dir
     (lambda (tmp)
       (define racket-root (make-fake-racket-root! tmp))
       (define artifact-dir (build-path tmp "artifacts"))
       (define work-dir (build-path tmp "work"))
       (define deb-name "racket9_9.2.1-1_amd64.deb")
       (define config-path (build-path tmp "apt-release-config.rktd"))
       (write-apt-release-config! config-path deb-name)
       (define-values (out err)
         (run-package/success
          (list "--target" "apt"
                "--target" "apt-release"
                "--racket-root" (path-arg racket-root)
                "--artifact-dir" (path-arg artifact-dir)
                "--work-dir" (path-arg work-dir)
                "--apt-release-config" (path-arg config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: apt, apt-release")
       (check-contains text "APT package:")
       (check-contains text "APT release sha256: <dry-run: artifact not built>")
       (check-contains text "Would upload apt release asset from planned apt output")
       (check-false (directory-exists? artifact-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case apt plus apt-release dry-run

  (test-case "source-release dry-run validates an existing tgz without racket root"
    (with-temp-dir
     (lambda (tmp)
       (define artifact-dir (build-path tmp "artifacts"))
       (define asset-name "racket-minimal-9.2.1-src.tgz")
       (define config-path (build-path tmp "source-release-config.rktd"))
       (write-text! (build-path artifact-dir asset-name) "source archive")
       (write-source-release-config! config-path asset-name)
       (define-values (out err)
         (run-package/success
          (list "--target" "source-release"
                "--artifact-dir" (path-arg artifact-dir)
                "--source-release-config" (path-arg config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: source-release")
       (check-contains text "Would upload source release asset from")
       (check-not-contains text "Racket root:")
       (check-not-contains text "read-github-token")
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case source-release upload-only dry-run

  (test-case "brew plus source-release dry-run uses planned brew output"
    (with-temp-dir
     (lambda (tmp)
       (define racket-root (make-fake-racket-root! tmp))
       (define artifact-dir (build-path tmp "artifacts"))
       (define work-dir (build-path tmp "work"))
       (define tap-dir (build-path tmp "tap"))
       (define asset-name "racket-minimal-9.2.1-src.tgz")
       (define config-path (build-path tmp "source-release-config.rktd"))
       (write-source-release-config! config-path asset-name)
       (define-values (out err)
         (run-package/success
          (list "--target" "brew"
                "--target" "source-release"
                "--racket-root" (path-arg racket-root)
                "--homebrew-tap" (path-arg tap-dir)
                "--bottle-root-url" "https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1"
                "--artifact-dir" (path-arg artifact-dir)
                "--work-dir" (path-arg work-dir)
                "--source-release-config" (path-arg config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: brew, source-release")
       (check-contains text "Source release sha256: <dry-run: artifact not built>")
       (check-contains text "Would upload source release asset from planned brew output")
       (check-false (directory-exists? artifact-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case brew plus source-release dry-run

  (test-case "combined apt release rejects mismatched asset names"
    (with-temp-dir
     (lambda (tmp)
       (define racket-root (make-fake-racket-root! tmp))
       (define config-path (build-path tmp "apt-release-config.rktd"))
       (write-apt-release-config! config-path "old-racket9.deb")
       (define-values (exit-code out err)
         (run-package
          (list "--target" "apt"
                "--target" "apt-release"
                "--racket-root" (path-arg racket-root)
                "--artifact-dir" (path-arg (build-path tmp "artifacts"))
                "--work-dir" (path-arg (build-path tmp "work"))
                "--apt-release-config" (path-arg config-path)
                "--dry-run")))
       (check-not-equal? exit-code 0)
       (check-contains (combined-output out err) "does not match apt output")
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case mismatch guard
) ; end module+ test
