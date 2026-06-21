# package-racket

Packaging helper for a local Racket checkout. The command supports named
packaging flows:

- `brew`: creates the Homebrew source `.tgz`, generates a staged
  `brew/Formula/racket@9.rb`, validates it, then replaces the formula in the
  Homebrew tap at the end of a successful script run unless disabled.
  The default Formula mode is `incremental`; pass `--formula-build-mode full`
  to generate the complete Formula from the package-racket template instead of
  using the tap Formula as a starting point.
- `source-release`: uploads the Homebrew source `.tgz` to the configured
  GitHub release with the GitHub REST API. This is a local tool action, not a
  CI workflow.
- `brew-ci`: generates Homebrew tap GitHub Actions workflows from
  `brew-ci-config.rktd`, validates them, then replaces the workflow files in
  the Homebrew tap. The generated release workflow builds and publishes bottles
  automatically on each push to `main`. This target is intentionally not part
  of `all`.
- `apt`: installs into a staged root with `make unix-style` and builds a `.deb`.
- `apt-release`: uploads the configured `.deb` from `--artifact-dir` to a
  GitHub release with the GitHub REST API. This target can be combined with
  `apt` or used to upload an already generated `.deb`.
- `rpm-spec`: overwrites the configured `rpm-racket` repository with generated
  RPM build definitions, scripts, `.repo`, README, and publishing directories.
  This target does not require `--racket-root`.
- `rpm`: installs into a staged root with `make unix-style` and builds a `.rpm`.
- `rpm-repo`: copies the configured `.rpm` into an explicit RPM repository
  root and regenerates metadata with `createrepo_c`. This target can be
  combined with `rpm` or used to publish an already generated `.rpm`.

All inputs are passed by named options. There are no positional arguments.
Generated metadata text in the Racket script is assembled with `f"..."` strings,
so package fields stay close to the self-hosted tstring syntax used by the
Racket checkout. Any flow that can write a Homebrew tap requires
`--homebrew-tap` and `--bottle-root-url`; the script intentionally has no
implicit tap path or bottle upload target.

For maintainability, complex Racket blocks in `package-racket.rkt` use an
explicit block-ending style: close important `define`, `begin`, `cond`, `when`,
`unless`, `for`, `match`, and `lambda` forms on their own line with an
`; end ...` comment. Keep that style when extending the script.

The script stops on safety-check failures instead of continuing. It checks
required directories and tools, validates non-empty staged install roots,
checks package metadata files, verifies `.deb` archive members, verifies RPM
metadata with `rpm -qip`, and compares the generated Homebrew formula sha256
against the generated source `.tgz`. The `brew` flow also verifies that the
source `.tgz` contains this fork's custom core sandbox profile:
`sandbox-lib`, `errortrace-lib`, and `source-syntax`, including the collection
links needed for `racket/sandbox` and `syntax/source-syntax`. The `brew-ci`
flow also validates generated workflow YAML and required workflow content before
replacing files in the tap. The publish workflow updates release assets and the
Formula only after every bottle runner succeeds.
The `rpm-spec` flow checks that the target root is a writable Git repository,
generates the complete build-definition layer, validates the generated spec and
script contents, and marks generated shell entrypoints executable. The
`rpm-repo` flow checks the target repository root, checks RPM metadata before
copying it, runs `createrepo_c --update`, and requires `repodata/repomd.xml` to
exist before reporting success.

`package-racket` is the source of truth for generated packaging metadata. The
Homebrew tap receives overwritten generated outputs such as `Formula/racket@9.rb`
and `.github/workflows/*.yml`; keep their maintainable configuration here.
Those generated tap files include a generated-code header. Humans and LLM
agents must not make production changes directly in `homebrew-racket`; change
`package-racket` and regenerate the tap outputs instead.
The same rule applies to `rpm-racket`: generated `SPECS/`, `SOURCES/`,
`scripts/`, `.repo`, README, package, and metadata outputs are produced from
this repository.

## Version Model

`package-config.rktd` contains the explicit package-manager version:

```racket
#hash((source-version . "9.2.1")
      (formula-version . "9.2.1.1"))
```

`formula-version` is the version visible to package managers. It drives the
explicit Homebrew Formula `version`, the Homebrew bottle version, the Debian
`.deb` version and filename, and the RPM `Version:` field. Bump it to a
four-level value such as `9.2.1.1` when you need users to see an update even
though the Racket runtime still reports `9.2.1`.

The Racket source/runtime version is read from
`racket/src/version/racket_version.h` in `--racket-root`, and must match
`source-version` in `package-config.rktd`. Upload-only targets without
`--racket-root` use the configured `source-version` to derive stable source
asset names. Formula runtime tests continue to check that source version. Use
`--formula-version` only as a temporary command-line override; keeping the
stable value in `package-config.rktd` is the normal workflow. The old
`--version` flag is a compatibility alias for `--formula-version`.

## Git Commit Messages

Do not use free-form git commit messages for this project. A commit message is a
single Racket datum. Use this shape:

```racket
(FEAT "title"

()
"detail info"
)
```

The first element is an uppercase change-kind symbol. The title and detail
fields are strings. The third field is a feature information list; use `()` when
there is no extra structured metadata to record. Do not write a literal
`(feature ...)` form there; that notation is meta-syntax, not the datum shape.

The repository includes a `commit-msg` hook dispatcher and validator:

- `tools/git-hooks/commit-msg`
- `tools/check-commit-message.rkt`

From the `package-racket` repository root, install the dispatcher as a global
Git hook path:

```sh
git config --global core.hooksPath "$(pwd)/tools/git-hooks"
```

The global hook is intentionally opt-in per repository. Enable the check in a
repository with:

```sh
git config commit.datumMessage true
```

The hook only blocks commits when `commit.datumMessage` is true, or when the
repository root contains `.git-commit-message.rktd`. This keeps unrelated
repositories from being forced into this message format.
Both `package-racket` and the generated Homebrew tap include that marker.

During `git commit`, an invalid message is rejected with a warning and the
expected shape. You can also run the same check directly:

```sh
tools/git-hooks/commit-msg .commit
racket tools/check-commit-message.rkt --message-file .commit
```

## Tests

Run the packaging regression tests from the repository root:

```sh
raco test package-racket.rkt tests/package-racket-test.rkt
```

These tests combine brew unit checks with real `package-racket.rkt` CLI runs in
temporary directories. They cover Homebrew Formula and workflow semantics,
dry-run isolation between targets, release-upload validation without reading
local tokens, and combined producer/release targets such as `apt + apt-release`
and `brew + source-release`. They also cover `rpm-spec` dry-run isolation,
complete `rpm-racket` definition generation, generated shell syntax checks, and
`rpm + rpm-repo` dry-run isolation so repository generation cannot silently
write files during planning.

`--dry-run` still performs safety checks for configured paths, but it does not
write package artifacts, generated Homebrew workflow files, or tap Formula
updates.

## Requirements

- Racket with the `tstring` reader available.
- For `brew`: an explicit `--homebrew-tap` whose root contains
  `Formula/racket@9.rb`, plus an explicit `--bottle-root-url` for the
  Homebrew bottle release assets. The brew source `.tgz` is generated directly
  by `package-racket`; no helper script is required in the Homebrew tap.
- For `brew-ci`: Ruby for YAML validation, plus an explicit
  `--bottle-root-url` for `brew test-bot`, release uploads, and
  `brew bottle --merge`.
- For `source-release` and `apt-release`: a fine-grained GitHub personal
  access token for the configured release repository, with
  `Contents: Read and write`, stored locally as one Racket string datum in
  `secret/ghtoken.rktd`. The file is ignored by Git and should be mode `600`.
- For `apt`: `dpkg-deb`, or `ar` + `tar` + `xz` through the automatic fallback.
- For `rpm-spec`: an explicit `--rpm-repo-config` and an explicit repository
  root in that config or `--rpm-repo-root`.
- For `rpm`: `rpmbuild`.
- For `rpm-repo`: `rpm` for package metadata validation, `createrepo_c` for
  repository metadata, an explicit `--rpm-repo-config`, and an explicit
  repository root in that config or `--rpm-repo-root`.

## Examples

### Common Entrypoints

Create the Homebrew source archive and update the tap formula:

```sh
racket package-racket.rkt \
  --target brew \
  --racket-root /Users/cutiedeng/Y2026/M04/D03/racket.git \
  --homebrew-tap /opt/homebrew/Library/Taps/cutiedeng/homebrew-racket \
  --bottle-root-url https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1
```

Create the Homebrew source archive and overwrite the tap formula from the
package-racket full Formula template:

```sh
racket package-racket.rkt \
  --target brew \
  --formula-build-mode full \
  --racket-root /Users/cutiedeng/Y2026/M04/D03/racket.git \
  --homebrew-tap /opt/homebrew/Library/Taps/cutiedeng/homebrew-racket \
  --bottle-root-url https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1
```

Create the Homebrew source archive, upload it to the configured
`CutieDeng/racket` release, then update the tap formula only if the upload
succeeds:

```sh
racket package-racket.rkt \
  --target brew \
  --target source-release \
  --racket-root /Users/cutiedeng/Y2026/M04/D03/racket.git \
  --homebrew-tap /opt/homebrew/Library/Taps/cutiedeng/homebrew-racket \
  --bottle-root-url https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1
```

Upload an already generated source `.tgz` without rebuilding it:

```sh
racket package-racket.rkt \
  --target source-release \
  --artifact-dir /Users/cutiedeng/Y2026/M06/D21/package-racket/artifacts
```

Create a Debian package and upload the resulting `.deb` to the configured
GitHub release:

```sh
racket package-racket.rkt \
  --target apt \
  --target apt-release \
  --racket-root /path/to/racket.git \
  --package-name racket9 \
  --release 1 \
  --prefix /usr \
  --deb-arch amd64 \
  --artifact-dir /Users/cutiedeng/Y2026/M06/D21/package-racket/artifacts \
  --work-dir /Users/cutiedeng/Y2026/M06/D21/package-racket/.build \
  --apt-release-config /Users/cutiedeng/Y2026/M06/D21/package-racket/apt-release-config.rktd
```

Upload an already generated `.deb` without rebuilding it:

```sh
racket package-racket.rkt \
  --target apt-release \
  --artifact-dir /Users/cutiedeng/Y2026/M06/D21/package-racket/artifacts \
  --apt-release-config /Users/cutiedeng/Y2026/M06/D21/package-racket/apt-release-config.rktd
```

Generate the Homebrew tap CI workflows from `brew-ci-config.rktd` and overwrite
the tap workflow files after validation:

```sh
racket package-racket.rkt \
  --target brew-ci \
  --racket-root /Users/cutiedeng/Y2026/M04/D03/racket.git \
  --homebrew-tap /opt/homebrew/Library/Taps/cutiedeng/homebrew-racket \
  --bottle-root-url https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1
```

Use `--dry-run` with any entrypoint to print the resolved paths and planned
actions without writing artifacts, replacing tap files, or uploading release
assets.

### GitHub Release Token

The stable GitHub release settings live in `source-release-config.rktd`. The
release tag is configured there, while the source asset defaults to
`racket-minimal-<source-version>-src.tgz`. The default token file is
`secret/ghtoken.rktd`, and it must contain exactly one Racket string datum:

```racket
"github_pat_..."
```

Create the local token file without committing it:

```sh
mkdir -p secret
chmod 700 secret
$EDITOR secret/ghtoken.rktd
chmod 600 secret/ghtoken.rktd
```

The stable Debian package release settings live in `apt-release-config.rktd`.
The release tag is configured there, while the asset defaults to the `.deb`
basename generated from `formula-version`, `--release`, and `--deb-arch`, for
example `racket9_9.2.1.1-1_amd64.deb`.

For `brew`, `--formula` means the final tap formula path. When omitted, it is
derived from the explicit `--homebrew-tap` as `Formula/racket@9.rb`.

`--formula-build-mode incremental` is the default. It copies the tap Formula
into `.build/brew/Formula/`, updates only the source URL, source sha256, and
bottle `root_url` when a bottle block is present, then replaces the tap formula
only after all selected targets succeed.

`--formula-build-mode full` treats package-racket as the complete Formula
template source. It writes every Formula section from `package-racket.rkt` and
does not preserve the tap Formula body. The full template intentionally omits
bottle sha256 metadata because that is generated by the Homebrew bottle CI; the
publish workflow writes the bottle block back after successful bottle builds.

The generated Homebrew source archive is
`racket-minimal-<source-version>-src.tgz`, matching the basename used by the
Formula source URL. `package-racket` writes an explicit Formula
`version "<formula-version>"`, so Homebrew can detect packaging-only updates
even when the source archive basename and release tag stay at the source
version. In incremental mode, the Formula bottle `root_url` is taken from
`--bottle-root-url`; in full mode, the same value is still required for brew CI
and bottle publishing. The Formula source URL uses the source release tag from
`source-release-config.rktd`, and the bottle publish workflow uses the release
tag embedded in `--bottle-root-url`.
For this fork, the brew source archive intentionally includes `sandbox-lib` and
its transitive runtime packages so `racket/sandbox` is available as part of the
custom minimal profile.
The brew archive also excludes the official macOS arm64 binary platform package
and rewrites the staged `racket-lib/info.rkt` dependency entry for it, because
Homebrew builds from source and links platform libraries through Formula
dependencies instead.

Use `--brew-ci-config` to point at another workflow config file, and
`--homebrew-tap` to explicitly select the tap that receives the generated
workflows. The generated workflows pass `--bottle-root-url` to Homebrew as
`--root-url`. If any bottle build fails, the publish job does not run.

The generated `publish.yml` runs on pushes to `main`, builds bottles for the
configured bottle runners, normalizes generated bottle JSON to the explicit
`bottle-rebuild` value from `brew-ci-config.rktd`, uploads the normalized asset
names to the release selected by `--bottle-root-url`, merges the normalized
bottle JSON back into `Formula/racket@9.rb`, then pushes a Formula update whose
feature metadata includes `[skip bottles]` and `[skip ci]` to avoid a publish
loop.

### Package Prefix

`--prefix` controls where the staged installation is rooted inside `apt` and
`rpm` packages. For system-level Linux packages, use `--prefix /usr`. With that
setting, the staged installation should be under
`/tmp/racket-package-root/usr`.
The generated `.deb` file itself is written to `--artifact-dir`, not into the
install prefix.
After a user installs that `.deb`, the package payload is installed under the
configured prefix, such as `/usr`.

For RPM, `package-racket` generates the `%files` list from the staged
filesystem. It lists real package files and package-owned directories instead
of claiming the whole prefix. Shared parent directories such as `/usr`,
`/usr/bin`, `/usr/lib`, and `/usr/share` are intentionally skipped so uninstall
remains precise while the package still installs as a system-level package.

For Debian packages, `package-racket` also writes `DEBIAN/md5sums` for payload
files so `dpkg --verify` can check installed file contents.

This value is not the Homebrew installation prefix. Homebrew chooses its own
Cellar and opt paths when building the Formula, and `source-release` only
uploads an existing `.tgz`. Therefore the command output prints `Prefix:` only
for targets that actually stage an install root, such as `apt` and `rpm`.

Create a Debian package from a Linux x64 build:

```sh
racket package-racket.rkt \
  --target apt \
  --racket-root /path/to/racket.git \
  --prefix /usr \
  --deb-arch amd64
```

Force the portable `.deb` backend when `dpkg-deb` is not installed:

```sh
racket package-racket.rkt \
  --target apt \
  --racket-root /path/to/racket.git \
  --skip-build \
  --install-root /tmp/racket-package-root \
  --deb-backend ar
```

Generate or refresh the `rpm-racket` build definition repository:

```sh
racket package-racket.rkt \
  --target rpm-spec \
  --prefix /usr \
  --rpm-arch arm64 \
  --rpm-repo-config /Users/cutiedeng/Y2026/M06/D21/package-racket/rpm-repo-config.rktd
```

`rpm-spec` writes `SPECS/racket9.spec`, `scripts/build-rpm.sh`,
`scripts/build-srpm.sh`, `scripts/verify-rpm.sh`, `scripts/update-repo.sh`,
`racket9.repo`, README, and the `repo/x86_64` / `repo/aarch64` publishing
directories in the configured `rpm-racket` root.

Build an RPM from the generated `rpm-racket` repository:

```sh
/Users/cutiedeng/Y2026/M06/D22/rpm-racket/scripts/build-rpm.sh \
  --racket-root /path/to/clean-racket.git \
  --artifact-dir /path/to/package-racket/artifacts \
  --work-dir /path/to/package-racket/.build/rpm-racket \
  --prefix /usr \
  --rpm-arch arm64
```

Build the matching SRPM:

```sh
/Users/cutiedeng/Y2026/M06/D22/rpm-racket/scripts/build-srpm.sh \
  --racket-root /path/to/clean-racket.git \
  --artifact-dir /path/to/package-racket/artifacts \
  --work-dir /path/to/package-racket/.build/rpm-racket-srpm \
  --prefix /usr \
  --rpm-arch arm64
```

Publish an existing RPM into `rpm-racket/repo/$basearch`:

```sh
/Users/cutiedeng/Y2026/M06/D22/rpm-racket/scripts/update-repo.sh \
  --rpm /path/to/package-racket/artifacts/racket9-9.2.1.1-1.aarch64.rpm \
  --rpm-arch arm64
```

The generated scripts support `--dry-run`, named mutable paths, and safety
checks before build or publish actions.

Create an RPM package directly from `package-racket` on a Linux x64 build:

```sh
racket package-racket.rkt \
  --target rpm \
  --racket-root /path/to/racket.git \
  --prefix /usr \
  --rpm-arch x86_64
```

Create an RPM package directly from `package-racket` on a Linux arm64 build:

```sh
racket package-racket.rkt \
  --target rpm \
  --racket-root /path/to/racket.git \
  --prefix /usr \
  --rpm-arch arm64
```

`--rpm-arch arm64` is normalized to RPM's `aarch64` target. The accepted RPM
architecture spellings are `x86_64`, `amd64`, `x64`, `aarch64`, and `arm64`.

Create an RPM package directly from `package-racket` and update the generated
RPM repository:

```sh
racket package-racket.rkt \
  --target rpm \
  --target rpm-repo \
  --racket-root /path/to/clean-racket.git \
  --prefix /usr \
  --rpm-arch arm64 \
  --artifact-dir /Users/cutiedeng/Y2026/M06/D21/package-racket/artifacts \
  --work-dir /Users/cutiedeng/Y2026/M06/D21/package-racket/.build \
  --rpm-repo-config /Users/cutiedeng/Y2026/M06/D21/package-racket/rpm-repo-config.rktd
```

Update the RPM repository from an already generated RPM:

```sh
racket package-racket.rkt \
  --target rpm-repo \
  --artifact-dir /Users/cutiedeng/Y2026/M06/D21/package-racket/artifacts \
  --rpm-arch arm64 \
  --rpm-repo-config /Users/cutiedeng/Y2026/M06/D21/package-racket/rpm-repo-config.rktd
```

The default RPM repository config is `rpm-repo-config.rktd`; it explicitly sets
`rpm-repo-root` to `/Users/cutiedeng/Y2026/M06/D22/rpm-racket`, plus the repo
id, display name, baseurl, and gpgcheck/enabled flags. Use command-line
overrides such as `--rpm-repo-root`, `--rpm-repo-baseurl`, and
`--createrepo-bin` when testing another repository root or host.

The RPM flow runs Racket's `make unix-style`, so `--racket-root` must point to a
clean source checkout that has not already been built in `in-place` mode. If a
host needs an already-built Racket just to run `package-racket.rkt`, keep that
bootstrap build in a separate checkout and pass the clean checkout as
`--racket-root`.

For a faster RPM smoke build, pass an explicit package set through
`--make-arg`. This is useful for validating RPM metadata, architecture, payload
layout, and runtime startup without building the full distribution package set:

```sh
racket package-racket.rkt \
  --target rpm \
  --racket-root /path/to/clean-racket.git \
  --prefix /usr \
  --rpm-arch arm64 \
  --make-arg "PKGS=racket-lib sandbox-lib errortrace-lib source-syntax tstring racket-tstring"
```

Reuse an already installed staging root instead of running `make unix-style`:

```sh
racket package-racket.rkt \
  --target apt \
  --target rpm \
  --racket-root /path/to/racket.git \
  --skip-build \
  --install-root /tmp/racket-package-root \
  --prefix /usr
```

The `--install-root` directory must contain the package filesystem root.

Use `--dry-run` to print the commands without writing package artifacts.
