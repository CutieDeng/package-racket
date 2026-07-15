#hash((release-tag . "v9.2.3-r1")
      (release-name . "Racket 9.2.3 RPM packages R1")
      (artifact-prefix . "rpm")
      (create-release . #t)
      (targets . (#hash((id . "el9-x86_64")
                        (rpm-system . "el9")
                        (rpm-release . "1")
                        (rpm-arch . "x86_64")
                        (runner . "ubuntu-24.04")
                        (container . "quay.io/centos/centos:stream9")
                        (jobs . 2)
	                        (setup-packages . ("bash"
	                                           "diffutils"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libffi-devel"
                                           "make"
                                           "ncurses-devel"
                                           "openssl-devel"
                                           "perl"
                                           "rpm"
                                           "rpm-build"
                                           "sed"
                                           "sqlite-devel"
                                           "tar"
                                           "which"
                                           "zlib-devel")))
                  #hash((id . "fc40-x86_64")
                        (rpm-system . "fc40")
                        (rpm-release . "1")
                        (rpm-arch . "x86_64")
                        (runner . "ubuntu-24.04")
                        (container . "fedora:40")
                        (jobs . 2)
                        (setup-packages . ("bash"
                                           "coreutils"
                                           "curl"
                                           "diffutils"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libffi-devel"
                                           "make"
                                           "ncurses-devel"
                                           "openssl-devel"
                                           "perl"
                                           "rpm"
                                           "rpm-build"
                                           "sed"
                                           "sqlite-devel"
                                           "tar"
                                           "which"
                                           "zlib-devel")))
                  #hash((id . "fc43-x86_64")
                        (rpm-system . "fc43")
                        (rpm-release . "1")
                        (rpm-arch . "x86_64")
                        (runner . "ubuntu-24.04")
                        (container . "fedora:43")
                        (jobs . 2)
                        (setup-packages . ("bash"
                                           "coreutils"
                                           "curl"
                                           "diffutils"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libffi-devel"
                                           "make"
                                           "ncurses-devel"
                                           "openssl-devel"
                                           "perl"
                                           "rpm"
                                           "rpm-build"
                                           "sed"
                                           "sqlite-devel"
                                           "tar"
                                           "which"
                                           "zlib-devel")))
                  #hash((id . "fc44-x86_64")
                        (rpm-system . "fc44")
                        (rpm-release . "1")
                        (rpm-arch . "x86_64")
                        (runner . "ubuntu-24.04")
                        (container . "fedora:44")
                        (jobs . 2)
                        (setup-packages . ("bash"
                                           "coreutils"
                                           "curl"
                                           "diffutils"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libffi-devel"
                                           "make"
                                           "ncurses-devel"
                                           "openssl-devel"
                                           "perl"
                                           "rpm"
                                           "rpm-build"
                                           "sed"
                                           "sqlite-devel"
                                           "tar"
                                           "which"
                                           "zlib-devel")))
                  #hash((id . "openeuler2203-aarch64")
                        (rpm-system . "openeuler2203")
                        (rpm-release . "1")
                        (rpm-arch . "aarch64")
                        (runner . "ubuntu-24.04-arm")
                        (container . "openeuler/openeuler:22.03-lts")
                        (jobs . 2)
                        (setup-packages . ("bash"
                                           "coreutils"
                                           "curl"
                                           "diffutils"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libffi-devel"
                                           "make"
                                           "ncurses-devel"
                                           "openssl-devel"
                                           "perl"
                                           "rpm"
                                           "rpm-build"
                                           "sed"
                                           "sqlite-devel"
                                           "tar"
                                           "which"
                                           "zlib-devel")))
                  #hash((id . "openeuler2203-x86_64")
                        (rpm-system . "openeuler2203")
                        (rpm-release . "1")
                        (rpm-arch . "x86_64")
                        (runner . "ubuntu-24.04")
                        (container . "openeuler/openeuler:22.03-lts")
                        (jobs . 2)
                        (setup-packages . ("bash"
                                           "coreutils"
                                           "curl"
                                           "diffutils"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libffi-devel"
                                           "make"
                                           "ncurses-devel"
                                           "openssl-devel"
                                           "perl"
                                           "rpm"
                                           "rpm-build"
                                           "sed"
                                           "sqlite-devel"
                                           "tar"
                                           "which"
                                           "zlib-devel")))
                  #hash((id . "openeuler2403-aarch64")
                        (rpm-system . "openeuler2403")
                        (rpm-release . "1")
                        (rpm-arch . "aarch64")
                        (runner . "ubuntu-24.04-arm")
                        (container . "openeuler/openeuler:24.03-lts")
                        (jobs . 2)
                        (setup-packages . ("bash"
                                           "coreutils"
                                           "curl"
                                           "diffutils"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libffi-devel"
                                           "make"
                                           "ncurses-devel"
                                           "openssl-devel"
                                           "perl"
                                           "rpm"
                                           "rpm-build"
                                           "sed"
                                           "sqlite-devel"
                                           "tar"
                                           "which"
                                           "zlib-devel")))
                  #hash((id . "openeuler2403-x86_64")
                        (rpm-system . "openeuler2403")
                        (rpm-release . "1")
                        (rpm-arch . "x86_64")
                        (runner . "ubuntu-24.04")
                        (container . "openeuler/openeuler:24.03-lts")
                        (jobs . 2)
                        (setup-packages . ("bash"
                                           "coreutils"
                                           "curl"
                                           "diffutils"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libffi-devel"
                                           "make"
                                           "ncurses-devel"
                                           "openssl-devel"
                                           "perl"
                                           "rpm"
                                           "rpm-build"
                                           "sed"
                                           "sqlite-devel"
                                           "tar"
                                           "which"
                                           "zlib-devel"))))))
