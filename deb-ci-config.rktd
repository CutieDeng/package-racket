#hash((release-tag . "v9.2.1")
      (release-name . "Racket 9.2.1 DEB packages")
      (artifact-prefix . "deb")
      (create-release . #t)
      (targets . (#hash((id . "debian12-amd64")
                        (deb-system . "debian12")
                        (deb-release . "2")
                        (deb-arch . "amd64")
                        (runner . "ubuntu-24.04")
                        (container . "debian:12")
                        (jobs . 2)
                        (setup-packages . ("build-essential"
                                           "ca-certificates"
                                           "coreutils"
                                           "curl"
                                           "dpkg-dev"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libedit-dev"
                                           "libffi-dev"
                                           "libsqlite3-dev"
                                           "libssl-dev"
                                           "make"
                                           "sed"
                                           "tar"
                                           "xz-utils"
                                           "zlib1g-dev")))
                  #hash((id . "ubuntu2404-amd64")
                        (deb-system . "ubuntu2404")
                        (deb-release . "2")
                        (deb-arch . "amd64")
                        (runner . "ubuntu-24.04")
                        (container . "ubuntu:24.04")
                        (jobs . 2)
                        (setup-packages . ("build-essential"
                                           "ca-certificates"
                                           "coreutils"
                                           "curl"
                                           "dpkg-dev"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libedit-dev"
                                           "libffi-dev"
                                           "libsqlite3-dev"
                                           "libssl-dev"
                                           "make"
                                           "sed"
                                           "tar"
                                           "xz-utils"
                                           "zlib1g-dev")))
                  #hash((id . "debian12-arm64")
                        (deb-system . "debian12")
                        (deb-release . "2")
                        (deb-arch . "arm64")
                        (runner . "ubuntu-24.04-arm")
                        (container . "debian:12")
                        (jobs . 2)
                        (setup-packages . ("build-essential"
                                           "ca-certificates"
                                           "coreutils"
                                           "curl"
                                           "dpkg-dev"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libedit-dev"
                                           "libffi-dev"
                                           "libsqlite3-dev"
                                           "libssl-dev"
                                           "make"
                                           "sed"
                                           "tar"
                                           "xz-utils"
                                           "zlib1g-dev")))
                  #hash((id . "ubuntu2404-arm64")
                        (deb-system . "ubuntu2404")
                        (deb-release . "2")
                        (deb-arch . "arm64")
                        (runner . "ubuntu-24.04-arm")
                        (container . "ubuntu:24.04")
                        (jobs . 2)
                        (setup-packages . ("build-essential"
                                           "ca-certificates"
                                           "coreutils"
                                           "curl"
                                           "dpkg-dev"
                                           "file"
                                           "findutils"
                                           "gcc"
                                           "grep"
                                           "gzip"
                                           "libedit-dev"
                                           "libffi-dev"
                                           "libsqlite3-dev"
                                           "libssl-dev"
                                           "make"
                                           "sed"
                                           "tar"
                                           "xz-utils"
                                           "zlib1g-dev"))))))
