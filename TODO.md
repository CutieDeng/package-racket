# TODO

## RPM release artifacts

- Add SRPM generation to the generated `rpm-racket` CI workflow later.

  Scope:
  - Run `scripts/build-srpm.sh` in CI before or beside `scripts/build-rpm.sh`.
  - Upload `*.src.rpm` together with binary RPM artifacts.
  - Keep install/runtime verification limited to binary RPMs.
  - Avoid duplicate SRPM uploads when multiple architecture jobs share the same
    source package identity.

  Rationale:
  SRPMs are not required for end-user installation, but they improve release
  completeness, auditability, and rebuild workflows such as
  `rpmbuild --rebuild`.
