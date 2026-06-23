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

(define (make-fake-homebrew-tap! base)
  (begin
    (define tap (build-path base "homebrew-racket"))
    (make-directory* (build-path tap ".git"))
    (make-directory* (build-path tap ".github" "workflows"))
    (write-text!
     (build-path tap "Formula" "racket@9.rb")
     "class RacketAT9 < Formula
  url \"https://github.com/CutieDeng/racket/releases/download/v9.2.1/racket-minimal-9.2.1-src.tgz\"
  sha256 \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"
  test do
    assert_match \"9.2.1\", shell_output(\"racket -v\")
  end
end
")
    tap
  ) ; end begin make-fake-homebrew-tap!
) ; end define make-fake-homebrew-tap!

(define (make-fake-rpm-repo! base)
  (begin
    (define repo (build-path base "rpm-racket"))
    (make-directory* (build-path repo ".git"))
    repo
  ) ; end begin make-fake-rpm-repo!
) ; end define make-fake-rpm-repo!

(define (make-fake-deb-repo! base)
  (begin
    (define repo (build-path base "deb-racket"))
    (make-directory* (build-path repo ".git"))
    repo
  ) ; end begin make-fake-deb-repo!
) ; end define make-fake-deb-repo!

(define (make-fake-windows-repo! base)
  (begin
    (define repo (build-path base "win-racket"))
    (make-directory* (build-path repo ".git"))
    repo
  ) ; end begin make-fake-windows-repo!
) ; end define make-fake-windows-repo!

(define (write-brew-ci-config! path)
  (write-text!
   path
   "#hash((formula . \"racket@9\")
      (artifact-prefix . \"bottles\")
      (bottle-rebuild . 1)
      (bottle-runners . (#hash((os . \"macos-26\"))
                         #hash((os . \"ubuntu-latest\")
                               (container . \"ghcr.io/homebrew/brew:main\"))
                         #hash((os . \"ubuntu-24.04-arm\")
                               (container . \"ghcr.io/homebrew/brew:main\"))))
      (syntax-runners . (#hash((os . \"macos-15-intel\")))))
"))

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

  (define (write-derived-apt-release-config! path)
    (write-text!
     path
     "#hash((apt-release-repo . \"CutieDeng/racket\")
      (apt-release-token-file . \"missing-token.rktd\")
      (replace-release-asset . #t))
"))

  (define (write-rpm-repo-config! path root)
    (write-text!
     path
     f"#hash((rpm-repo-root . \"{(path-arg root)}\")
      (rpm-repo-id . \"cutiedeng-racket\")
      (rpm-repo-name . \"CutieDeng Racket RPM Repository\")
      (rpm-repo-baseurl . \"https://raw.githubusercontent.com/CutieDeng/rpm-racket/main/repo/$basearch\")
      (rpm-repo-enabled . #t)
      (rpm-repo-gpgcheck . #f))
"))

  (define (write-deb-repo-config! path root)
    (write-text!
     path
     f"#hash((deb-repo-root . \"{(path-arg root)}\")
      (deb-system . \"ubuntu2404\")
      (deb-release . \"1\")
      (deb-arch . \"amd64\"))
"))

  (define (write-deb-ci-config! path)
    (write-text!
     path
     "#hash((release-tag . \"v9.2.1\")
      (release-name . \"Racket 9.2.1 DEB packages\")
      (artifact-prefix . \"deb\")
      (create-release . #t)
      (targets . (#hash((id . \"debian12-amd64\")
                        (deb-system . \"debian12\")
                        (deb-release . \"1\")
                        (deb-arch . \"amd64\")
                        (runner . \"ubuntu-24.04\")
                        (container . \"debian:12\")
                        (jobs . 2)
                        (setup-packages . (\"build-essential\" \"curl\" \"dpkg-dev\" \"libedit-dev\" \"libffi-dev\" \"libssl-dev\" \"zlib1g-dev\")))
                  #hash((id . \"ubuntu2404-arm64\")
                        (deb-system . \"ubuntu2404\")
                        (deb-release . \"1\")
                        (deb-arch . \"arm64\")
                        (runner . \"ubuntu-24.04-arm\")
                        (container . \"ubuntu:24.04\")
                        (jobs . 2)
                        (setup-packages . (\"build-essential\" \"curl\" \"dpkg-dev\" \"libedit-dev\" \"libffi-dev\" \"libssl-dev\" \"zlib1g-dev\"))))))
"))

  (define (write-rpm-ci-config! path)
    (write-text!
     path
     "#hash((release-tag . \"v9.2.1\")
      (release-name . \"Racket 9.2.1 RPM packages\")
      (artifact-prefix . \"rpm\")
      (create-release . #t)
      (targets . (#hash((id . \"el9-x86_64\")
                        (rpm-system . \"el9\")
                        (rpm-release . \"1\")
                        (rpm-arch . \"x86_64\")
                        (runner . \"ubuntu-24.04\")
                        (container . \"quay.io/centos/centos:stream9\")
                        (jobs . 2)
                        (setup-packages . (\"bash\" \"curl\" \"rpm\" \"rpm-build\" \"tar\")))
                  #hash((id . \"fc40-x86_64\")
                        (rpm-system . \"fc40\")
                        (rpm-release . \"1\")
                        (rpm-arch . \"x86_64\")
                        (runner . \"ubuntu-24.04\")
                        (container . \"fedora:40\")
                        (jobs . 2)
                        (setup-packages . (\"bash\" \"curl\" \"rpm\" \"rpm-build\" \"tar\")))
                  #hash((id . \"openeuler2203-aarch64\")
                        (rpm-system . \"openeuler2203\")
                        (rpm-release . \"1\")
                        (rpm-arch . \"arm64\")
                        (runner . \"ubuntu-24.04-arm\")
                        (container . \"openeuler/openeuler:22.03-lts\")
                        (jobs . 2)
                        (setup-packages . (\"bash\" \"curl\" \"rpm\" \"rpm-build\" \"tar\")))
                  #hash((id . \"openeuler2203-x86_64\")
                        (rpm-system . \"openeuler2203\")
                        (rpm-release . \"1\")
                        (rpm-arch . \"x86_64\")
                        (runner . \"ubuntu-24.04\")
                        (container . \"openeuler/openeuler:22.03-lts\")
                        (jobs . 2)
                        (setup-packages . (\"bash\" \"curl\" \"rpm\" \"rpm-build\" \"tar\")))
                  #hash((id . \"openeuler2403-aarch64\")
                        (rpm-system . \"openeuler2403\")
                        (rpm-release . \"1\")
                        (rpm-arch . \"arm64\")
                        (runner . \"ubuntu-24.04-arm\")
                        (container . \"openeuler/openeuler:24.03-lts\")
                        (jobs . 2)
                        (setup-packages . (\"bash\" \"curl\" \"rpm\" \"rpm-build\" \"tar\")))
                  #hash((id . \"openeuler2403-x86_64\")
                        (rpm-system . \"openeuler2403\")
                        (rpm-release . \"1\")
                        (rpm-arch . \"x86_64\")
                        (runner . \"ubuntu-24.04\")
                        (container . \"openeuler/openeuler:24.03-lts\")
                        (jobs . 2)
                        (setup-packages . (\"bash\" \"curl\" \"rpm\" \"rpm-build\" \"tar\"))))))
"))

  (define (write-invalid-rpm-ci-config! path)
    (write-text!
     path
     "#hash((release-tag . \"v9.2.1\")
      (release-name . \"Racket 9.2.1 RPM packages\")
      (artifact-prefix . \"rpm\")
      (create-release . #t)
      (targets . (#hash((id . \"bad-openeuler\")
                        (rpm-system . \"openeuler\")
                        (rpm-release . \"1\")
                        (rpm-arch . \"arm64\")
                        (runner . \"ubuntu-24.04-arm\")
                        (container . \"openeuler/openeuler:24.03-lts\")
                        (jobs . 2)
                        (setup-packages . (\"bash\" \"curl\" \"rpm\" \"rpm-build\" \"tar\"))))))
"))

  (define (write-windows-ci-config! path root #:publish-release? [publish-release? #t])
    (write-text!
     path
     f"#hash((windows-repo-root . \"{(path-arg root)}\")
      (runner . \"windows-2022\")
      (arch . \"x86_64\")
      (msvc-arch . \"x64\")
      (nmake-target . \"plain-install\")
      (build-jobs . 2)
      (portable-dir-name . \"racket9\")
      (artifact-prefix . \"windows\")
      (publish-release . {(if publish-release? "#t" "#f")})
      (release-repo . \"CutieDeng/racket\")
      (release-tag . \"v9.2.1\")
      (release-name . \"Racket 9.2.1 Windows portable\")
      (create-release . #f)
      (token-secret . \"WINDOWS_RELEASE_TOKEN\"))
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
       (check-contains text "racket9_9.2.1.5-1_amd64.deb")
       (check-not-contains text "APT release config:")
       (check-false (directory-exists? artifact-dir))
       (check-false (directory-exists? work-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case apt dry-run isolation

  (test-case "rpm arm64 dry-run normalizes to aarch64 and writes no artifacts"
    (with-temp-dir
     (lambda (tmp)
       (define artifact-dir (build-path tmp "artifacts"))
       (define work-dir (build-path tmp "work"))
       (define-values (out err)
         (run-package/success
          (list "--target" "rpm"
                "--artifact-dir" (path-arg artifact-dir)
                "--work-dir" (path-arg work-dir)
                "--rpm-system" "openeuler2403"
                "--rpm-release" "1"
                "--rpm-arch" "arm64"
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: rpm")
       (check-contains text "Formula/package version: 9.2.1.5")
       (check-contains text "RPM target arch: aarch64")
       (check-contains text "RPM target system: openeuler2403")
       (check-contains text "RPM package version: 9.2.1")
       (check-contains text "RPM package release base: 1")
       (check-contains text "RPM package release: 1.openeuler2403")
       (check-contains text "RPM package prefix: /usr")
       (check-contains text "RPM source archive: https://github.com/CutieDeng/racket/releases/download/v9.2.1/racket-minimal-9.2.1-src.tgz")
       (check-contains text "racket9-9.2.1-1.openeuler2403.aarch64.rpm")
       (check-contains text "--target aarch64")
       (check-contains text "package_system openeuler2403")
       (check-contains text "package_release 1")
       (check-not-contains text "make -C")
       (check-not-contains text "payload.tar.gz")
       (check-false (directory-exists? artifact-dir))
       (check-false (directory-exists? work-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case rpm arm64 dry-run

  (test-case "rpm dry-run rejects missing explicit system"
    (with-temp-dir
     (lambda (tmp)
       (define-values (exit-code out err)
         (run-package
          (list "--target" "rpm"
                "--artifact-dir" (path-arg (build-path tmp "artifacts"))
                "--work-dir" (path-arg (build-path tmp "work"))
                "--rpm-release" "1"
                "--rpm-arch" "arm64"
                "--dry-run")))
       (check-not-equal? exit-code 0)
       (check-contains (combined-output out err) "--rpm-system is required")
       (check-false (directory-exists? (build-path tmp "artifacts")))
       (check-false (directory-exists? (build-path tmp "work")))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case rpm missing system

  (test-case "rpm dry-run rejects generic openeuler system"
    (with-temp-dir
     (lambda (tmp)
       (define-values (exit-code out err)
         (run-package
          (list "--target" "rpm"
                "--artifact-dir" (path-arg (build-path tmp "artifacts"))
                "--work-dir" (path-arg (build-path tmp "work"))
                "--rpm-system" "openeuler"
                "--rpm-release" "1"
                "--rpm-arch" "arm64"
                "--dry-run")))
       (check-not-equal? exit-code 0)
       (check-contains (combined-output out err)
                       "--rpm-system must be one of el9, fc40, openeuler2203, openeuler2403")
       (check-false (directory-exists? (build-path tmp "artifacts")))
       (check-false (directory-exists? (build-path tmp "work")))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case rpm generic openeuler

  (test-case "rpm-spec dry-run generates no files and does not require racket root"
    (with-temp-dir
     (lambda (tmp)
       (define rpm-repo-root (make-fake-rpm-repo! tmp))
       (define config-path (build-path tmp "rpm-repo-config.rktd"))
       (write-rpm-repo-config! config-path rpm-repo-root)
       (define-values (out err)
         (run-package/success
          (list "--target" "rpm-spec"
                "--rpm-system" "openeuler2403"
                "--rpm-release" "1"
                "--rpm-arch" "arm64"
                "--rpm-repo-config" (path-arg config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: rpm-spec")
       (check-contains text "RPM target arch: aarch64")
       (check-contains text "RPM target system: openeuler2403")
       (check-contains text "RPM package release: 1.openeuler2403")
       (check-contains text "RPM repo config:")
       (check-contains text "Would generate RPM SPEC scaffold in:")
       (check-contains text "Would generate RPM SPEC file:")
       (check-not-contains text "Racket root:")
       (check-false (directory-exists? (build-path rpm-repo-root "SPECS")))
       (check-false (directory-exists? (build-path rpm-repo-root "scripts")))
       (check-false (file-exists? (build-path rpm-repo-root "racket9.repo")))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case rpm-spec dry-run

  (test-case "rpm-spec writes spec sources and scripts only"
    (with-temp-dir
     (lambda (tmp)
       (define rpm-repo-root (make-fake-rpm-repo! tmp))
       (write-text!
        (build-path rpm-repo-root "racket9.repo")
        "# GENERATED RPM REPOSITORY METADATA - DO NOT EDIT IN rpm-racket.\n")
       (make-directory* (build-path rpm-repo-root "repo"))
       (define artifact-dir (build-path tmp "artifacts"))
       (write-text! (build-path artifact-dir "racket-minimal-9.2.1-src.tgz") "fake source artifact")
       (define config-path (build-path tmp "rpm-repo-config.rktd"))
       (write-rpm-repo-config! config-path rpm-repo-root)
       (define-values (out err)
         (run-package/success
          (list "--target" "rpm-spec"
                "--rpm-system" "openeuler2403"
                "--rpm-release" "1"
                "--rpm-arch" "arm64"
                "--artifact-dir" (path-arg artifact-dir)
                "--rpm-repo-config" (path-arg config-path))))
       (define text (combined-output out err))
       (check-contains text "Generated RPM SPEC scaffold:")
       (define spec-path (build-path rpm-repo-root "SPECS" "racket9.spec"))
       (define common-path (build-path rpm-repo-root "scripts" "rpm-common.sh"))
       (define build-path* (build-path rpm-repo-root "scripts" "build-rpm.sh"))
       (define srpm-path (build-path rpm-repo-root "scripts" "build-srpm.sh"))
       (define verify-path (build-path rpm-repo-root "scripts" "verify-rpm.sh"))
       (define readme-file (build-path rpm-repo-root "README.md"))
       (for ([path (in-list (list spec-path
                                   common-path
                                   build-path*
                                   srpm-path
                                   verify-path
                                   readme-file
                                   (build-path rpm-repo-root ".gitignore")
                                   (build-path rpm-repo-root "SOURCES" ".gitkeep")))])
         (check-true (file-exists? path) f"expected generated file: {(path-arg path)}")
       ) ; end for generated file
       (for ([path (in-list (list common-path build-path* srpm-path verify-path))])
         (check-true (not (zero? (bitwise-and (file-or-directory-permissions path 'bits) #o111)))
                     f"expected executable script: {(path-arg path)}")
       ) ; end for executable script
       (define spec-content (file->string spec-path))
       (check-contains spec-content "%{!?package_system:%global package_system openeuler2403}")
       (check-contains spec-content "%{!?package_release:%global package_release 1}")
       (check-contains spec-content "Release: %{package_release}.%{package_system}")
       (check-contains spec-content "Source0: https://github.com/CutieDeng/racket/releases/download/v9.2.1/racket-minimal-9.2.1-src.tgz")
       (check-contains spec-content "Requires: libedit")
       (check-contains spec-content "%global source_sha256")
       (check-contains spec-content "Source0 sha256 mismatch")
       (check-contains spec-content "%setup -q -n racket-9.2.1")
       (check-contains spec-content "make install DESTDIR=%{buildroot}")
       (check-contains spec-content "%files -f %{name}.files")
       (check-not-contains spec-content "Source1:")
       (check-contains (file->string common-path) "prepare_source_archive")
       (check-not-contains (file->string common-path) "repository root is not a Git repository")
       (check-contains (file->string build-path*) "--source-archive")
       (check-not-contains (file->string build-path*) "--racket-root")
       (check-contains (file->string build-path*) "rpmbuild -bb")
       (check-contains (file->string srpm-path) "rpmbuild -bs")
       (check-contains (file->string verify-path) "rpm -qip")
       (check-contains (file->string readme-file) "not an RPM artifact repository")
       (check-contains (file->string readme-file) "scripts/build-rpm.sh")
       (check-false (file-exists? (build-path rpm-repo-root "racket9.repo")))
       (check-false (directory-exists? (build-path rpm-repo-root "repo")))
       (delete-directory/files (build-path rpm-repo-root ".git"))
       (define bash-bin (find-executable-path "bash"))
       (when bash-bin
         (for ([path (in-list (list common-path build-path* srpm-path verify-path))])
           (run-command! 'rpm-spec-script-syntax bash-bin (list "-n" (path-arg path)))
         ) ; end for bash syntax
         (run-command! 'rpm-spec-script-help bash-bin (list (path-arg build-path*) "--help"))
         (run-command! 'rpm-spec-build-rpm-dry-run
                       bash-bin
                       (list (path-arg build-path*)
                             "--artifact-dir" (path-arg (build-path tmp "artifacts"))
                             "--work-dir" (path-arg (build-path tmp "rpm-work"))
                             "--rpm-system" "openeuler2403"
                             "--rpm-release" "1"
                             "--rpm-arch" "arm64"
                             "--prefix" "/usr"
                             "--dry-run"))
         (run-command! 'rpm-spec-build-srpm-dry-run
                       bash-bin
                       (list (path-arg srpm-path)
                             "--artifact-dir" (path-arg (build-path tmp "artifacts"))
                             "--work-dir" (path-arg (build-path tmp "srpm-work"))
                             "--rpm-system" "openeuler2403"
                             "--rpm-release" "1"
                             "--rpm-arch" "arm64"
                             "--prefix" "/usr"
                             "--dry-run"))
       ) ; end when bash exists
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case rpm-spec writes scaffold

  (test-case "rpm-ci dry-run validates matrix config and writes no workflow"
    (with-temp-dir
     (lambda (tmp)
       (define rpm-repo-root (make-fake-rpm-repo! tmp))
       (define repo-config-path (build-path tmp "rpm-repo-config.rktd"))
       (define ci-config-path (build-path tmp "rpm-ci-config.rktd"))
       (define work-dir (build-path tmp "work"))
       (write-rpm-repo-config! repo-config-path rpm-repo-root)
       (write-rpm-ci-config! ci-config-path)
       (define-values (out err)
         (run-package/success
          (list "--target" "rpm-ci"
                "--prefix" "/usr"
                "--work-dir" (path-arg work-dir)
                "--rpm-repo-config" (path-arg repo-config-path)
                "--rpm-ci-config" (path-arg ci-config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: rpm-ci")
       (check-contains text "RPM CI config:")
       (check-contains text "Would read RPM CI config:")
       (check-contains text "Would generate RPM CI workflow:")
       (check-contains text "Would configure RPM CI target count: 6")
       (check-contains text "openeuler2203 aarch64 on ubuntu-24.04-arm in openeuler/openeuler:22.03-lts")
       (check-contains text "openeuler2203 x86_64 on ubuntu-24.04 in openeuler/openeuler:22.03-lts")
       (check-not-contains text "RPM target system:")
       (check-false (file-exists? (build-path rpm-repo-root ".github" "workflows" "build-rpm.yml")))
       (check-false (directory-exists? work-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case rpm-ci dry-run

  (test-case "deb-spec dry-run generates no files and does not require racket root"
    (with-temp-dir
     (lambda (tmp)
       (define deb-repo-root (make-fake-deb-repo! tmp))
       (define config-path (build-path tmp "deb-repo-config.rktd"))
       (define work-dir (build-path tmp "work"))
       (write-deb-repo-config! config-path deb-repo-root)
       (define-values (out err)
         (run-package/success
          (list "--target" "deb-spec"
                "--work-dir" (path-arg work-dir)
                "--deb-repo-config" (path-arg config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: deb-spec")
       (check-contains text "DEB repo config:")
       (check-contains text "DEB repo root:")
       (check-contains text "DEB target system: ubuntu2404")
       (check-contains text "DEB package version: 9.2.1-1.ubuntu2404")
       (check-contains text "Would generate DEB scaffold in:")
       (check-false (file-exists? (build-path deb-repo-root "scripts" "build-deb.sh")))
       (check-false (directory-exists? work-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case deb-spec dry-run

  (test-case "deb-spec writes generated scaffold into a temporary deb repository"
    (with-temp-dir
     (lambda (tmp)
       (define deb-repo-root (make-fake-deb-repo! tmp))
       (define config-path (build-path tmp "deb-repo-config.rktd"))
       (write-deb-repo-config! config-path deb-repo-root)
       (define-values (out err)
         (run-package/success
          (list "--target" "deb-spec"
                "--deb-repo-config" (path-arg config-path))))
       (define text (combined-output out err))
       (check-contains text "DEB source archive sha256 from local artifact:")
       (check-contains text "Generated DEB scaffold:")
       (define readme-file (build-path deb-repo-root "README.md"))
       (define common-script (build-path deb-repo-root "scripts" "deb-common.sh"))
       (define build-script (build-path deb-repo-root "scripts" "build-deb.sh"))
       (define verify-script (build-path deb-repo-root "scripts" "verify-deb.sh"))
       (check-true (file-exists? readme-file))
       (check-true (file-exists? common-script))
       (check-true (file-exists? build-script))
       (check-true (file-exists? verify-script))
       (check-contains (file->string readme-file) "not an apt repository")
       (check-contains (file->string common-script) "SOURCE_SHA256='")
       (check-contains (file->string build-script) "dpkg-deb --root-owner-group --build")
       (check-contains (file->string build-script) "Depends: libc6, libedit2")
       (check-contains (file->string verify-script) "dpkg-deb --field")
       (check-false (directory-exists? (build-path deb-repo-root "repo")))
       (when (find-executable-path "bash")
         (run-command! 'deb-spec-build-deb-dry-run
                       (find-executable-path "bash")
                       (list (path-arg build-script)
                             "--artifact-dir" (path-arg (build-path tmp "artifacts"))
                             "--work-dir" (path-arg (build-path tmp "work"))
                             "--deb-system" "ubuntu2404"
                             "--deb-release" "1"
                             "--deb-arch" "amd64"
                             "--dry-run"))
         (run-command! 'deb-spec-verify-deb-dry-run
                       (find-executable-path "bash")
                       (list (path-arg verify-script)
                             "--deb" (path-arg (build-path tmp "artifacts" "racket9_9.2.1-1.ubuntu2404_amd64.deb"))
                             "--deb-system" "ubuntu2404"
                             "--deb-release" "1"
                             "--deb-arch" "amd64"
                             "--dry-run"))
       ) ; end when bash exists
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case deb-spec writes scaffold

  (test-case "deb-ci dry-run validates matrix config and writes no workflow"
    (with-temp-dir
     (lambda (tmp)
       (define deb-repo-root (make-fake-deb-repo! tmp))
       (define repo-config-path (build-path tmp "deb-repo-config.rktd"))
       (define ci-config-path (build-path tmp "deb-ci-config.rktd"))
       (define work-dir (build-path tmp "work"))
       (write-deb-repo-config! repo-config-path deb-repo-root)
       (write-deb-ci-config! ci-config-path)
       (define-values (out err)
         (run-package/success
          (list "--target" "deb-ci"
                "--work-dir" (path-arg work-dir)
                "--deb-repo-config" (path-arg repo-config-path)
                "--deb-ci-config" (path-arg ci-config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: deb-ci")
       (check-contains text "DEB CI config:")
       (check-contains text "Would read DEB CI config:")
       (check-contains text "Would generate DEB CI workflow:")
       (check-contains text "Would configure DEB CI target count: 2")
       (check-contains text "ubuntu2404 arm64 on ubuntu-24.04-arm in ubuntu:24.04")
       (check-false (file-exists? (build-path deb-repo-root ".github" "workflows" "build-deb.yml")))
       (check-false (directory-exists? work-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case deb-ci dry-run

  (test-case "deb-ci installs generated workflow into a temporary deb repository"
    (with-temp-dir
     (lambda (tmp)
       (define deb-repo-root (make-fake-deb-repo! tmp))
       (define repo-config-path (build-path tmp "deb-repo-config.rktd"))
       (define ci-config-path (build-path tmp "deb-ci-config.rktd"))
       (write-deb-repo-config! repo-config-path deb-repo-root)
       (write-deb-ci-config! ci-config-path)
       (run-package/success
        (list "--target" "deb-spec"
              "--deb-repo-config" (path-arg repo-config-path)))
       (define-values (out err)
         (run-package/success
          (list "--target" "deb-ci"
                "--deb-repo-config" (path-arg repo-config-path)
                "--deb-ci-config" (path-arg ci-config-path))))
       (define text (combined-output out err))
       (define workflow-path (build-path deb-repo-root ".github" "workflows" "build-deb.yml"))
       (check-contains text "Generated DEB CI workflow:")
       (check-true (file-exists? workflow-path))
       (define workflow-content (file->string workflow-path))
       (check-contains workflow-content "GENERATED DEB PACKAGING METADATA - DO NOT EDIT IN deb-racket.")
       (check-contains workflow-content "name: deb build and release")
       (check-contains workflow-content "debian:12")
       (check-contains workflow-content "ubuntu-24.04-arm")
       (check-contains workflow-content "EXPECTED_DEB_COUNT: 2")
       (check-contains workflow-content "apt-get install -y \"${deb_files[0]}\"")
       (check-contains workflow-content "racket -e '(displayln f\"deb-ci-ok\")'")
       (check-contains workflow-content "racket -e '(require readline/readline) (displayln f\"deb-readline-ok\")'")
       (check-contains workflow-content "GH_REPO: ${{ github.repository }}")
       (check-contains workflow-content "--repo \"$GITHUB_REPOSITORY\"")
       (check-contains workflow-content "Downloaded DEB files")
       (check-contains workflow-content "Release assets before upload")
       (check-contains workflow-content "Release assets after upload")
       (check-contains workflow-content "gh release upload")
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case deb-ci workflow install

  (test-case "rpm-ci installs generated workflow into a temporary rpm repository"
    (with-temp-dir
     (lambda (tmp)
       (define rpm-repo-root (make-fake-rpm-repo! tmp))
       (define repo-config-path (build-path tmp "rpm-repo-config.rktd"))
       (define ci-config-path (build-path tmp "rpm-ci-config.rktd"))
       (write-rpm-repo-config! repo-config-path rpm-repo-root)
       (write-rpm-ci-config! ci-config-path)
       (define-values (out err)
         (run-package/success
          (list "--target" "rpm-ci"
                "--prefix" "/usr"
                "--rpm-repo-config" (path-arg repo-config-path)
                "--rpm-ci-config" (path-arg ci-config-path))))
       (define text (combined-output out err))
       (define workflow-path (build-path rpm-repo-root ".github" "workflows" "build-rpm.yml"))
       (check-contains text "Generated RPM CI workflow:")
       (check-true (file-exists? workflow-path))
       (define workflow-content (file->string workflow-path))
       (check-contains workflow-content "GENERATED RPM PACKAGING METADATA - DO NOT EDIT IN rpm-racket.")
       (check-contains workflow-content "name: rpm build and release")
       (check-contains workflow-content "actions/checkout@v6")
       (check-contains workflow-content "actions/upload-artifact@v6")
       (check-contains workflow-content "actions/download-artifact@v6")
       (check-contains workflow-content "openeuler2203")
       (check-contains workflow-content "openeuler/openeuler:24.03-lts")
       (check-contains workflow-content "EXPECTED_RPM_COUNT: 6")
       (check-contains workflow-content "$pm -y install \"${rpm_files[0]}\"")
       (check-contains workflow-content "rpm -q libedit >/dev/null")
       (check-contains workflow-content "Downloaded RPM files")
       (check-contains workflow-content "Release assets before upload")
       (check-contains workflow-content "Release assets after upload")
       (check-contains workflow-content "GH_REPO: ${{ github.repository }}")
       (check-contains workflow-content "--repo \"$GITHUB_REPOSITORY\"")
       (check-contains workflow-content "racket -e '(displayln f\"rpm-ci-ok\")'")
       (check-contains workflow-content "racket -e '(require readline/readline) (displayln f\"rpm-readline-ok\")'")
       (check-contains workflow-content "gh release upload")
       (check-false (directory-exists? (build-path rpm-repo-root "repo")))
       (check-false (file-exists? (build-path rpm-repo-root "racket9.repo")))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case rpm-ci workflow install

  (test-case "rpm-ci rejects generic openeuler system in matrix config"
    (with-temp-dir
     (lambda (tmp)
       (define rpm-repo-root (make-fake-rpm-repo! tmp))
       (define repo-config-path (build-path tmp "rpm-repo-config.rktd"))
       (define ci-config-path (build-path tmp "rpm-ci-config.rktd"))
       (write-rpm-repo-config! repo-config-path rpm-repo-root)
       (write-invalid-rpm-ci-config! ci-config-path)
       (define-values (exit-code out err)
         (run-package
          (list "--target" "rpm-ci"
                "--prefix" "/usr"
                "--rpm-repo-config" (path-arg repo-config-path)
                "--rpm-ci-config" (path-arg ci-config-path)
                "--dry-run")))
       (check-not-equal? exit-code 0)
       (check-contains (combined-output out err)
                       "--rpm-system must be one of el9, fc40, openeuler2203, openeuler2403")
       (check-false (file-exists? (build-path rpm-repo-root ".github" "workflows" "build-rpm.yml")))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case rpm-ci generic openeuler

  (test-case "windows-portable-ci dry-run validates config and writes no workflow"
    (with-temp-dir
     (lambda (tmp)
       (define windows-repo-root (make-fake-windows-repo! tmp))
       (define ci-config-path (build-path tmp "windows-ci-config.rktd"))
       (define work-dir (build-path tmp "work"))
       (write-windows-ci-config! ci-config-path windows-repo-root #:publish-release? #f)
       (define-values (out err)
         (run-package/success
          (list "--target" "windows-portable-ci"
                "--work-dir" (path-arg work-dir)
                "--windows-ci-config" (path-arg ci-config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: windows-portable-ci")
       (check-contains text "Windows CI config:")
       (check-contains text "Windows CI repo root:")
       (check-contains text "Would read Windows CI config:")
       (check-contains text "Would generate Windows portable README:")
       (check-contains text "Would generate Windows portable CI workflow:")
       (check-contains text "Would configure Windows runner: windows-2022")
       (check-contains text "Would configure Windows portable zip: racket9-9.2.1.5-windows-x86_64.zip")
       (check-contains text "Would publish Windows release asset: no")
       (check-false (file-exists? (build-path windows-repo-root "README.md")))
       (check-false (file-exists? (build-path windows-repo-root ".github" "workflows" "build-windows-portable.yml")))
       (check-false (directory-exists? work-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case windows portable ci dry-run

  (test-case "windows-portable-ci installs generated workflow into configured repository"
    (with-temp-dir
     (lambda (tmp)
       (define windows-repo-root (make-fake-windows-repo! tmp))
       (define ci-config-path (build-path tmp "windows-ci-config.rktd"))
       (write-windows-ci-config! ci-config-path windows-repo-root #:publish-release? #t)
       (define-values (out err)
         (run-package/success
          (list "--target" "windows-portable-ci"
                "--windows-ci-config" (path-arg ci-config-path))))
       (define text (combined-output out err))
       (define readme-file (build-path windows-repo-root "README.md"))
       (define workflow-path (build-path windows-repo-root ".github" "workflows" "build-windows-portable.yml"))
       (check-contains text "Windows source archive sha256 from local artifact:")
       (check-contains text "Generated Windows portable README:")
       (check-contains text "Generated Windows portable CI workflow:")
       (check-true (file-exists? readme-file))
       (check-true (file-exists? workflow-path))
       (check-contains (file->string readme-file) "GENERATED WINDOWS PORTABLE PACKAGING METADATA - DO NOT EDIT.")
       (check-contains (file->string readme-file) "build-windows-portable.yml")
       (check-contains (file->string readme-file) "racket9-9.2.1.5-windows-x86_64.zip")
       (check-contains (file->string readme-file) "Release asset publishing is enabled")
       (check-contains (file->string readme-file) "CutieDeng/racket")
       (check-contains (file->string readme-file) "WINDOWS_RELEASE_TOKEN")
       (define workflow-content (file->string workflow-path))
       (check-contains workflow-content "GENERATED WINDOWS PORTABLE PACKAGING METADATA - DO NOT EDIT.")
       (check-contains workflow-content "name: windows portable build")
       (check-contains workflow-content "runs-on: windows-2022")
       (check-contains workflow-content "SOURCE_URL: 'https://github.com/CutieDeng/racket/releases/download/v9.2.1/racket-minimal-9.2.1-src.tgz'")
       (check-contains workflow-content "SOURCE_SHA256: '")
       (check-contains workflow-content "ZIP_NAME: 'racket9-9.2.1.5-windows-x86_64.zip'")
       (check-contains workflow-content "NMAKE_TARGET: 'plain-install'")
       (check-contains workflow-content "dir src\\Makefile.nt")
       (check-contains workflow-content "dir src\\buildmain.zuo")
       (check-contains workflow-content "call src\\winfig.bat")
       (check-contains workflow-content "if not exist Makefile")
       (check-contains workflow-content "winfig.bat did not create Makefile")
       (check-not-contains workflow-content "call src\\winfig.bat %MSVC_ARCH%")
       (check-contains workflow-content "Expected install layout directory but found non-directory")
       (check-contains workflow-content "call :RunNmake all")
       (check-contains workflow-content "call :RunNmake %NMAKE_TARGET%")
       (check-contains workflow-content "nmake /f Makefile %* JOBS=%BUILD_JOBS%")
       (check-contains workflow-content "Known Racket CS DLL files after failed nmake")
       (check-contains workflow-content "New-Item -ItemType Directory -Force $portableRoot")
       (check-contains workflow-content "foreach ($name in @(\"collects\", \"etc\", \"lib\", \"share\", \"include\", \"doc\"))")
       (check-contains workflow-content "required portable runtime path missing")
       (check-not-contains workflow-content "Copy-Item -Recurse -Force $packageRoot $portableRoot")
       (check-contains workflow-content "raco.cmd")
       (check-contains workflow-content "-N raco -l- raco")
       (check-contains workflow-content "raco package listing failed")
       (check-contains workflow-content "Compress-Archive")
       (check-contains workflow-content "windows-portable-ok")
       (check-contains workflow-content "actions/upload-artifact@v6")
       (check-contains workflow-content "actions/download-artifact@v6")
       (check-contains workflow-content "GH_TOKEN: ${{ secrets.WINDOWS_RELEASE_TOKEN }}")
       (check-contains workflow-content "gh release upload \"$RELEASE_TAG\" -R \"$RELEASE_REPO\"")
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case windows portable ci workflow install

  (test-case "rpm plus rpm-repo dry-run uses planned rpm output and writes no repo files"
    (with-temp-dir
     (lambda (tmp)
       (define rpm-repo-root (make-fake-rpm-repo! tmp))
       (define config-path (build-path tmp "rpm-repo-config.rktd"))
       (define artifact-dir (build-path tmp "artifacts"))
       (define work-dir (build-path tmp "work"))
       (write-rpm-repo-config! config-path rpm-repo-root)
       (define-values (out err)
         (run-package/success
          (list "--target" "rpm"
                "--target" "rpm-repo"
                "--artifact-dir" (path-arg artifact-dir)
                "--work-dir" (path-arg work-dir)
                "--rpm-system" "openeuler2403"
                "--rpm-release" "1"
                "--rpm-arch" "arm64"
                "--rpm-repo-config" (path-arg config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: rpm, rpm-repo")
       (check-contains text "RPM target arch: aarch64")
       (check-contains text "RPM target system: openeuler2403")
       (check-contains text "RPM repo config:")
       (check-contains text "RPM repo root:")
       (check-contains text "RPM package version: 9.2.1")
       (check-contains text "RPM package release base: 1")
       (check-contains text "RPM package release: 1.openeuler2403")
       (check-contains text "RPM repo package: racket9-9.2.1-1.openeuler2403.aarch64.rpm")
       (check-contains text "RPM repo sha256: <dry-run: artifact not built>")
       (check-contains text "Would update RPM repo from planned rpm output")
       (check-contains text "Would copy RPM into repo:")
       (check-contains text "Would run createrepo_c --update")
       (check-false (file-exists? (build-path rpm-repo-root "racket9.repo")))
       (check-false (directory-exists? (build-path rpm-repo-root "repo")))
       (check-false (directory-exists? artifact-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case rpm plus rpm-repo dry-run

  (test-case "formula-version override drives apt package version"
    (with-temp-dir
     (lambda (tmp)
       (define racket-root (make-fake-racket-root! tmp))
       (define artifact-dir (build-path tmp "artifacts"))
       (define work-dir (build-path tmp "work"))
       (define-values (out err)
         (run-package/success
          (list "--target" "apt"
                "--racket-root" (path-arg racket-root)
                "--formula-version" "9.2.1.1"
                "--artifact-dir" (path-arg artifact-dir)
                "--work-dir" (path-arg work-dir)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Formula/package version: 9.2.1.1")
       (check-contains text "Racket source version: 9.2.1")
       (check-contains text "racket9_9.2.1.1-1_amd64.deb")
       (check-false (directory-exists? artifact-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case formula-version override apt

  (test-case "apt-release dry-run validates an existing deb and does not read token"
    (with-temp-dir
     (lambda (tmp)
       (define artifact-dir (build-path tmp "artifacts"))
       (define deb-name "racket9_9.2.1.5-1_amd64.deb")
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
       (define deb-name "racket9_9.2.1.5-1_amd64.deb")
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

  (test-case "apt-release derives tag and asset from formula-version"
    (with-temp-dir
     (lambda (tmp)
       (define racket-root (make-fake-racket-root! tmp))
       (define artifact-dir (build-path tmp "artifacts"))
       (define work-dir (build-path tmp "work"))
       (define config-path (build-path tmp "apt-release-config.rktd"))
       (write-derived-apt-release-config! config-path)
       (define-values (out err)
         (run-package/success
          (list "--target" "apt"
                "--target" "apt-release"
                "--racket-root" (path-arg racket-root)
                "--formula-version" "9.2.1.1"
                "--artifact-dir" (path-arg artifact-dir)
                "--work-dir" (path-arg work-dir)
                "--apt-release-config" (path-arg config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "APT package:")
       (check-contains text "racket9_9.2.1.1-1_amd64.deb")
       (check-contains text "APT release tag: v9.2.1")
       (check-contains text "APT release asset: racket9_9.2.1.1-1_amd64.deb")
       (check-contains text "Would upload apt release asset from planned apt output")
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case apt release derives formula-version

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
       (define tap-dir (make-fake-homebrew-tap! tmp))
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
       (check-contains text "Would stage brew source directory:")
       (check-contains text "Would create brew source tgz:")
       (check-contains text "Would set Formula source URL:")
       (check-contains text "Would update brew formula:")
       (check-contains text "Source release sha256: <dry-run: artifact not built>")
       (check-contains text "Would upload source release asset from planned brew output")
       (check-false (directory-exists? artifact-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case brew plus source-release dry-run

  (test-case "brew within-docs dry-run reports optional docs package group"
    (with-temp-dir
     (lambda (tmp)
       (define racket-root (make-fake-racket-root! tmp))
       (define artifact-dir (build-path tmp "artifacts"))
       (define work-dir (build-path tmp "work"))
       (define tap-dir (make-fake-homebrew-tap! tmp))
       (define-values (out err)
         (run-package/success
          (list "--target" "brew"
                "--racket-root" (path-arg racket-root)
                "--homebrew-tap" (path-arg tap-dir)
                "--bottle-root-url" "https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1"
                "--artifact-dir" (path-arg artifact-dir)
                "--work-dir" (path-arg work-dir)
                "--within-docs"
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: brew")
       (check-contains text "Brew docs: enabled")
       (check-contains text "Would include brew docs: yes")
       (check-false (directory-exists? artifact-dir))
       (check-false (directory-exists? work-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case brew within-docs dry-run

  (test-case "brew-ci accepts release tag independent of formula-version"
    (with-temp-dir
     (lambda (tmp)
       (define racket-root (make-fake-racket-root! tmp))
       (define tap-dir (make-fake-homebrew-tap! tmp))
       (define config-path (build-path tmp "brew-ci-config.rktd"))
       (define work-dir (build-path tmp "work"))
       (write-brew-ci-config! config-path)
       (define-values (out err)
         (run-package/success
          (list "--target" "brew-ci"
                "--racket-root" (path-arg racket-root)
                "--formula-version" "9.2.1.5"
                "--homebrew-tap" (path-arg tap-dir)
                "--bottle-root-url" "https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1"
                "--work-dir" (path-arg work-dir)
                "--brew-ci-config" (path-arg config-path))))
       (define publish-yml (build-path tap-dir ".github" "workflows" "publish.yml"))
       (check-contains (combined-output out err) "Formula/package version: 9.2.1.5")
       (check-contains (file->string publish-yml) "RELEASE_TAG: v9.2.1")
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case brew ci independent release tag

  (test-case "brew-ci dry-run validates tap config and writes no workflows"
    (with-temp-dir
     (lambda (tmp)
       (define racket-root (make-fake-racket-root! tmp))
       (define tap-dir (make-fake-homebrew-tap! tmp))
       (define config-path (build-path tmp "brew-ci-config.rktd"))
       (define work-dir (build-path tmp "work"))
       (write-brew-ci-config! config-path)
       (define-values (out err)
         (run-package/success
          (list "--target" "brew-ci"
                "--racket-root" (path-arg racket-root)
                "--homebrew-tap" (path-arg tap-dir)
                "--bottle-root-url" "https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1"
                "--work-dir" (path-arg work-dir)
                "--brew-ci-config" (path-arg config-path)
                "--dry-run")))
       (define text (combined-output out err))
       (check-contains text "Targets: brew-ci")
       (check-contains text "Would read brew CI config:")
       (check-contains text "Would configure bottle runner count: 3")
       (check-contains text "ubuntu-24.04-arm in ghcr.io/homebrew/brew:main")
       (check-contains text "Would validate brew CI workflow YAML with:")
       (check-false (file-exists? (build-path tap-dir ".github" "workflows" "tests.yml")))
       (check-false (file-exists? (build-path tap-dir ".github" "workflows" "publish.yml")))
       (check-false (directory-exists? work-dir))
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case brew-ci dry-run

  (test-case "brew-ci installs generated workflows into a temporary tap"
    (with-temp-dir
     (lambda (tmp)
       (define racket-root (make-fake-racket-root! tmp))
       (define tap-dir (make-fake-homebrew-tap! tmp))
       (define config-path (build-path tmp "brew-ci-config.rktd"))
       (define work-dir (build-path tmp "work"))
       (write-brew-ci-config! config-path)
       (define-values (out err)
         (run-package/success
          (list "--target" "brew-ci"
                "--racket-root" (path-arg racket-root)
                "--homebrew-tap" (path-arg tap-dir)
                "--bottle-root-url" "https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1"
                "--work-dir" (path-arg work-dir)
                "--brew-ci-config" (path-arg config-path))))
       (define text (combined-output out err))
       (define tests-yml (build-path tap-dir ".github" "workflows" "tests.yml"))
       (define publish-yml (build-path tap-dir ".github" "workflows" "publish.yml"))
       (check-contains text "Installed brew CI workflow:")
       (check-true (file-exists? tests-yml))
       (check-true (file-exists? publish-yml))
       (check-contains (file->string tests-yml) "ubuntu-24.04-arm")
       (check-contains (file->string tests-yml) "if-no-files-found: error")
       (check-contains (file->string publish-yml) "URI.decode_www_form_component")
       (check-contains (file->string publish-yml) "\"[skip ci]\"")
      ) ; end lambda temp dir
    ) ; end with-temp-dir
  ) ; end test-case brew-ci workflow install

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
