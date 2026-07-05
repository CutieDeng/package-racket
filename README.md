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
- `deb-spec`: overwrites the configured `deb-racket` build-script repository
  scaffold: `SOURCES/`, `scripts/`, README, and `.gitignore`. This target does
  not require `--racket-root`; generated DEB scripts build from the stable
  GitHub Release source archive by default, or from an explicitly named local
  source archive.
- `deb-ci`: generates `deb-racket/.github/workflows/build-deb.yml` from
  `deb-ci-config.rktd`, validates the workflow YAML, and publishes `.deb`
  release assets only after all configured matrix builds pass.
- `rpm-spec`: overwrites the configured `rpm-racket` SPEC repository scaffold:
  `SPECS/`, `SOURCES/`, `scripts/`, README, and `.gitignore`. This target does
  not require `--racket-root`; generated RPM scripts build from the stable
  GitHub Release source archive by default, or from an explicitly named local
  source archive.
- `rpm-ci`: generates `rpm-racket/.github/workflows/build-rpm.yml` from
  `rpm-ci-config.rktd`, validates the workflow YAML, and publishes RPM release
  assets only after all configured matrix builds pass.
- `windows-portable-ci`: generates the lightweight `win-racket` README and
  GitHub Actions workflow from `windows-ci-config.rktd`. The workflow builds
  Racket on a Windows runner, assembles a portable `.zip`, and uploads it as
  an Actions artifact. Release asset publishing is supported only when
  explicitly enabled in the config.
- `rpm`: builds a `.rpm` from the same source-archive SPEC model used by the
  generated `rpm-racket` scripts.
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
source `.tgz` contains this fork's custom core language profile:
`at-exp-lib` with its `base` dependency, plus `sandbox-lib`, `errortrace-lib`,
`source-syntax`, and a trimmed Rhombus v1.0 runtime profile. The Rhombus
profile includes `rhombus-lib`, `rhombus-exe`, `shrubbery-lib`,
`enforest-lib`, and `pretty-expressive-lib`, including the collection links
needed for `#lang rhombus` and the `rhombus` launcher. The `brew-ci` flow also validates
generated workflow YAML and required workflow content before replacing files in
the tap. The publish workflow updates release assets and the Formula only after
every bottle runner succeeds.
The `rpm-spec` flow checks that the target root is a writable Git repository,
generates only the SPEC/SOURCES/scripts scaffold, validates the generated spec
and script contents, resolves the source archive sha256 before writing `.spec`,
and marks generated shell entrypoints executable. The
`deb-spec` flow checks that the target root is a writable Git repository,
generates only the SOURCES/scripts scaffold, pins the source archive sha256 in
the generated shell code, validates script contents, and marks generated shell
entrypoints executable. The
`rpm-ci` flow checks the target repository root, validates generated workflow
YAML, and requires every configured target to name its system, release, arch,
runner, container, dependency list, and job count. The generated workflow checks
the repository layout, builds with the generated RPM scripts, performs an
install/uninstall smoke test, checks Release asset count and duplicate names,
then uploads with the workflow `GITHUB_TOKEN`.
The `deb-ci` flow applies the same generated workflow checks for Debian-family
targets and verifies install/purge behavior with `apt-get` before uploading
release assets with the workflow `GITHUB_TOKEN`.
The `rpm-repo` flow checks the target repository root, checks RPM metadata before
copying it, runs `createrepo_c --update`, and requires `repodata/repomd.xml` to
exist before reporting success.
The `windows-portable-ci` flow checks that the configured workflow repository is
a writable Git repository, validates the generated workflow YAML, pins the
source archive sha256, verifies `racket.exe` and `raco.exe` after the Windows
build, and uploads the zip with `actions/upload-artifact`. If release publishing
is enabled, the generated publish job requires the named GitHub Actions secret
to have release-write access to the configured `release-repo`.

`package-racket` is the source of truth for generated packaging metadata. The
Homebrew tap receives overwritten generated outputs such as `Formula/racket@9.rb`
and `.github/workflows/*.yml`; keep their maintainable configuration here.
Those generated tap files include a generated-code header. Humans and LLM
agents must not make production changes directly in `homebrew-racket`; change
`package-racket` and regenerate the tap outputs instead.
The same rule applies to `rpm-racket`: generated `SPECS/`, `SOURCES/`,
`scripts/`, `.github/workflows/`, and README outputs are produced from this
repository. `rpm-racket` is a SPEC/build-script repository, not an RPM artifact
repository.
The same rule applies to `deb-racket`: generated `SOURCES/`, `scripts/`,
`.github/workflows/`, and README outputs are produced from this repository.
`deb-racket` is a DEB build-script repository, not an apt repository.
The same rule applies to `win-racket`: generated README and
`.github/workflows/` outputs are produced from this repository. `win-racket`
is a lightweight Windows portable build repository and intentionally carries no
packaging scripts.
Change `windows-ci-config.rktd` and regenerate instead of editing generated
workflow YAML by hand.

## Version Model

`package-config.rktd` contains the explicit package-manager version:

```racket
#hash((source-version . "9.2.1")
      (formula-version . "9.2.1.5"))
```

`formula-version` drives the Homebrew Formula `version`, the Homebrew bottle
version, and the direct `apt` target `.deb` package version and filename. Bump
it to a four-level value such as `9.2.1.5` when those package managers need
users to see an update even though the Racket runtime still reports `9.2.1`.

The direct `apt` target produces filenames such as
`racket9_9.2.1.5-1_amd64.deb`, where `1` is `--release`. The generated
`deb-racket` scripts use the same source-version plus release model as RPM:
the Debian upstream version stays equal to `source-version`, while the Debian
revision is derived from explicit `deb-release` and `deb-system` fields, such
as `racket9_9.2.1-5.ubuntu2404_amd64.deb`.

RPM uses the same release-oriented model. The RPM `Version:` field stays equal
to `source-version`, while the RPM `Release:` field is derived from explicit
`--rpm-release` and `--rpm-system` fields. For example, source version `9.2.1`
with `--rpm-release 5` and `--rpm-system el9` produces
`racket9-9.2.1-5.el9.<arch>.rpm`, not
`racket9-9.2.1.5-5.<arch>.rpm`.

The Racket source/runtime version is read from
`racket/src/version/racket_version.h` in `--racket-root`, and must match
`source-version` in `package-config.rktd`. Targets that do not read
`--racket-root`, including `rpm`, use the configured `source-version` to derive
stable source asset names. Formula runtime tests continue to check that source
version. Use
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
and `brew + source-release`. They also cover `rpm-spec` dry-run isolation, SPEC
scaffold generation, generated shell syntax checks, generated `deb-spec`
scaffold generation, generated `deb-ci` and `rpm-ci` workflow validation, and
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
- For `deb-spec` and `deb-ci`: an explicit `--deb-repo-config` and an explicit
  repository root in that config. `deb-ci` also requires Ruby for YAML
  validation. GitHub Actions uses the generated workflow's same-repository
  `GITHUB_TOKEN` with `contents: write` to create or update release assets; no
  local token is read for this target.
- For `rpm-spec` and `rpm-ci`: an explicit `--rpm-repo-config` and an explicit
  repository root in that config or `--rpm-repo-root`.
- For `rpm-ci`: Ruby for YAML validation. GitHub Actions uses the generated
  workflow's same-repository `GITHUB_TOKEN` with `contents: write` to create or
  update release assets; no local token is read for this target.
- For `windows-portable-ci`: an explicit `--windows-ci-config`, a configured
  writable Git repository root, and Ruby for YAML validation. The generated
  workflow uses Microsoft Visual Studio Build Tools on the Windows runner. By
  default it only uploads an Actions artifact; release publishing needs
  `publish-release` set to `#t` and the configured `token-secret` created in
  the workflow repository.
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

Create the Homebrew source archive with `raco docs` support:

```sh
racket package-racket.rkt \
  --target brew \
  --within-docs \
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

Generate or refresh the `deb-racket` DEB build-script repository scaffold:

```sh
racket package-racket.rkt \
  --target deb-spec \
  --prefix /usr \
  --deb-repo-config /Users/cutiedeng/Y2026/M06/D21/package-racket/deb-repo-config.rktd
```

Generate or refresh the `deb-racket` GitHub Actions DEB workflow:

```sh
racket package-racket.rkt \
  --target deb-ci \
  --prefix /usr \
  --deb-repo-config /Users/cutiedeng/Y2026/M06/D21/package-racket/deb-repo-config.rktd \
  --deb-ci-config /Users/cutiedeng/Y2026/M06/D21/package-racket/deb-ci-config.rktd
```

Refresh both the generated DEB scripts and CI workflow in one run:

```sh
racket package-racket.rkt \
  --target deb-spec \
  --target deb-ci \
  --prefix /usr \
  --deb-repo-config /Users/cutiedeng/Y2026/M06/D21/package-racket/deb-repo-config.rktd \
  --deb-ci-config /Users/cutiedeng/Y2026/M06/D21/package-racket/deb-ci-config.rktd
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
example `racket9_9.2.1.5-1_amd64.deb`.

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
For this fork, the brew source archive intentionally includes `at-exp-lib`
and its `base` dependency so `#lang at-exp` and `@`-expression readers are
available as part of the custom minimal profile. It also includes `sandbox-lib`
and its transitive runtime packages so `racket/sandbox` is available. Rhombus
is included as a trimmed runtime language profile, not as the full
`rhombus-main-distribution`; GUI, draw, pict, HTML, XML, JSON, HTTP, Scribble,
and documentation-oriented Rhombus packages stay outside the default core set.
By default, the brew source archive does not include the `raco docs` command
or its documentation runtime package group. Pass `--within-docs` to include
`racket-index`, `scribble-lib`, `net-lib`, `draw-lib`, and their required
runtime packages; with that option, generated Formula tests also verify
`raco docs --help` and the unambiguous `raco doc --help` prefix.
The brew archive also excludes the official macOS arm64 binary platform package
and rewrites the staged `racket-lib/info.rkt` dependency entry for it, because
Homebrew builds from source and links platform libraries through Formula
dependencies instead.
The minimal source archive prunes Chez Scheme documentation, test, and example
source trees such as `csug`, `release_notes`, `nanopass/doc`, `stex/doc`,
`mats`, and `examples`; those are not used by the package build.
When `--within-docs` is enabled, the staged `draw-lib/info.rkt` platform native
package dependencies are removed with the same source-build rationale.

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

`deb-spec` writes only `.gitignore`, README, `SOURCES/`, and `scripts/` in the
configured `deb-racket` root. It must not create `repo/` or apt repository
metadata.

`deb-ci` writes only `.github/workflows/build-deb.yml` in the same
`deb-racket` root. The matrix lives in `deb-ci-config.rktd`; each target
explicitly names `deb-system`, `deb-release`, `deb-arch`, GitHub runner,
container image, dependency package list, and job count. The generated workflow
runs on push to `main` and manual dispatch, expands every target into
`postinstall` and `cached` cache modes, installs and purges each package inside
its target container, uploads Actions artifacts, then publishes the DEB files
to the configured GitHub Release with `--clobber`.

The `postinstall` mode emits the normal `racket9` package and builds the system
compiled cache after install. The `cached` mode emits `racket9-cached`, embeds
the generated system compiled cache in the package payload, and skips install
time cache generation.

The default DEB repository config is `deb-repo-config.rktd`; it explicitly sets
`deb-repo-root` to `/Users/cutiedeng/Y2026/M06/D23/deb-racket`, plus the
default `deb-system`, `deb-release`, and `deb-arch` used for local scaffold
validation. With `source-version` `9.2.1`, `deb-release` `5`, and `deb-system`
`ubuntu2404`, the generated Debian package version is `9.2.1-5.ubuntu2404`.
Supported generated DEB systems are `debian12` and `ubuntu2404`.
Supported DEB architecture spellings are `amd64`, `x86_64`, `x64`, `arm64`,
and `aarch64`; they normalize to Debian's `amd64` or `arm64`.

When generating `deb-racket/scripts/deb-common.sh`, `package-racket` resolves
the source archive sha256 in this order:

- use the local `artifacts/racket-minimal-9.2.1-src.tgz` when it exists;
- otherwise read the GitHub Release asset digest for the generated source URL;
- otherwise download the source archive into `.build/deb-source/` and
  calculate sha256.

That sha is pinned into the generated shell code as `SOURCE_SHA256`.

Build a DEB from the generated `deb-racket` repository:

```sh
/Users/cutiedeng/Y2026/M06/D23/deb-racket/scripts/build-deb.sh \
  --artifact-dir /path/to/package-racket/artifacts \
  --work-dir /path/to/package-racket/.build/deb-racket \
  --deb-system ubuntu2404 \
  --deb-release 2 \
  --prefix /usr \
  --deb-arch amd64 \
  --cache-mode postinstall
```

Build the cached DEB variant:

```sh
/Users/cutiedeng/Y2026/M06/D23/deb-racket/scripts/build-deb.sh \
  --artifact-dir /path/to/package-racket/artifacts \
  --work-dir /path/to/package-racket/.build/deb-racket-cached \
  --deb-system ubuntu2404 \
  --deb-release 2 \
  --prefix /usr \
  --deb-arch amd64 \
  --cache-mode cached
```

Use `deb-racket/scripts/build-deb.sh --source-archive ...` when a build host
must use an explicit local source archive instead of downloading the generated
source URL.

Generate or refresh the `rpm-racket` SPEC repository scaffold:

```sh
racket package-racket.rkt \
  --target rpm-spec \
  --prefix /usr \
  --rpm-system openeuler2403 \
  --rpm-release 2 \
  --rpm-arch arm64 \
  --rpm-repo-config /Users/cutiedeng/Y2026/M06/D21/package-racket/rpm-repo-config.rktd
```

Generate or refresh the `rpm-racket` GitHub Actions RPM workflow:

```sh
racket package-racket.rkt \
  --target rpm-ci \
  --prefix /usr \
  --rpm-repo-config /Users/cutiedeng/Y2026/M06/D21/package-racket/rpm-repo-config.rktd \
  --rpm-ci-config /Users/cutiedeng/Y2026/M06/D21/package-racket/rpm-ci-config.rktd
```

Refresh both the SPEC/scripts scaffold and the CI workflow in one run:

```sh
racket package-racket.rkt \
  --target rpm-spec \
  --target rpm-ci \
  --prefix /usr \
  --rpm-system openeuler2403 \
  --rpm-release 2 \
  --rpm-arch arm64 \
  --rpm-repo-config /Users/cutiedeng/Y2026/M06/D21/package-racket/rpm-repo-config.rktd \
  --rpm-ci-config /Users/cutiedeng/Y2026/M06/D21/package-racket/rpm-ci-config.rktd
```

`rpm-spec` writes only `.gitignore`, README, `SPECS/`, `SOURCES/`, and
`scripts/` in the configured `rpm-racket` root. It must not create `repo/` or
`racket9.repo`.

`rpm-ci` writes only `.github/workflows/build-rpm.yml` in the same
`rpm-racket` root. The matrix lives in `rpm-ci-config.rktd`; each target
explicitly names `rpm-system`, `rpm-release`, `rpm-arch`, GitHub runner,
container image, dependency package list, and job count. The generated workflow
runs on push to `main` and manual dispatch, expands every target into
`postinstall` and `cached` cache modes, installs and uninstalls each package
inside its target container, uploads Actions artifacts, then publishes the RPM
files to the configured GitHub Release with `--clobber`.

The `postinstall` mode emits the normal `racket9` package and builds the system
compiled cache after install. The `cached` mode emits `racket9-cached`, embeds
the generated system compiled cache in the package payload, and skips install
time cache generation.

When writing `SPECS/racket9.spec`, `package-racket` resolves the `Source0`
sha256 in this order:

- use the local `artifacts/racket-minimal-9.2.1-src.tgz` when it was just built;
- otherwise read the GitHub Release asset digest for the generated `Source0`;
- otherwise download `Source0` into `.build/rpm-source/` and calculate sha256.

That sha is written into the generated `.spec` as `%global source_sha256`.

Generate or refresh the Windows portable CI workflow:

```sh
racket package-racket.rkt \
  --target windows-portable-ci \
  --windows-ci-config /Users/cutiedeng/Y2026/M06/D21/package-racket/windows-ci-config.rktd
```

`windows-ci-config.rktd` explicitly names the workflow repository root, runner,
architecture, Visual Studio build mode, `nmake` target, artifact prefix, release
repository, release tag, and token secret. The current config writes README and
`.github/workflows/build-windows-portable.yml` in
`/Users/cutiedeng/Y2026/M06/D23/win-racket` and enables `publish-release`, so
successful pushes upload the zip to the configured `win-racket` GitHub Release.
The configured target is the workflow repository itself, so the generated
workflow uses the built-in `GITHUB_TOKEN` with `contents: write` instead of a
cross-repository PAT.

Build an RPM from the generated `rpm-racket` repository:

```sh
/Users/cutiedeng/Y2026/M06/D22/rpm-racket/scripts/build-rpm.sh \
  --artifact-dir /path/to/package-racket/artifacts \
  --work-dir /path/to/package-racket/.build/rpm-racket \
  --rpm-system openeuler2403 \
  --rpm-release 2 \
  --prefix /usr \
  --rpm-arch arm64 \
  --cache-mode postinstall
```

Build the cached RPM variant:

```sh
/Users/cutiedeng/Y2026/M06/D22/rpm-racket/scripts/build-rpm.sh \
  --artifact-dir /path/to/package-racket/artifacts \
  --work-dir /path/to/package-racket/.build/rpm-racket-cached \
  --rpm-system openeuler2403 \
  --rpm-release 2 \
  --prefix /usr \
  --rpm-arch arm64 \
  --cache-mode cached
```

Build the matching SRPM:

```sh
/Users/cutiedeng/Y2026/M06/D22/rpm-racket/scripts/build-srpm.sh \
  --artifact-dir /path/to/package-racket/artifacts \
  --work-dir /path/to/package-racket/.build/rpm-racket-srpm \
  --rpm-system openeuler2403 \
  --rpm-release 2 \
  --prefix /usr \
  --rpm-arch arm64
```

Create an RPM package directly from `package-racket` on a Linux x64 build:

```sh
racket package-racket.rkt \
  --target rpm \
  --rpm-system el9 \
  --rpm-release 2 \
  --prefix /usr \
  --rpm-arch x86_64
```

Create an RPM package directly from `package-racket` on a Linux arm64 build:

```sh
racket package-racket.rkt \
  --target rpm \
  --rpm-system openeuler2403 \
  --rpm-release 2 \
  --prefix /usr \
  --rpm-arch arm64
```

`--rpm-system` must be explicit. Supported values are `el9`, `fc40`, `fc43`,
`fc44`, `openeuler2203`, and `openeuler2403`. The generic `openeuler` value is
rejected because production RPM artifacts must name the concrete target system.
`--rpm-release` is the release base before the system suffix, so
`--rpm-release 2 --rpm-system fc40` becomes RPM `Release: 2.fc40`.
`--rpm-arch arm64` is normalized to RPM's `aarch64` target. The accepted RPM
architecture spellings are `x86_64`, `amd64`, `x64`, `aarch64`, and `arm64`.

Common RPM target examples:

```sh
--rpm-system el9 --rpm-release 2 --rpm-arch x86_64
--rpm-system fc40 --rpm-release 2 --rpm-arch x86_64
--rpm-system fc43 --rpm-release 2 --rpm-arch x86_64
--rpm-system fc44 --rpm-release 2 --rpm-arch x86_64
--rpm-system openeuler2203 --rpm-release 2 --rpm-arch arm64
--rpm-system openeuler2403 --rpm-release 2 --rpm-arch arm64
```

Create an RPM package directly from `package-racket` and update the generated
RPM repository:

```sh
racket package-racket.rkt \
  --target rpm \
  --target rpm-repo \
  --rpm-system openeuler2403 \
  --rpm-release 2 \
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
  --rpm-system openeuler2403 \
  --rpm-release 2 \
  --rpm-arch arm64 \
  --rpm-repo-config /Users/cutiedeng/Y2026/M06/D21/package-racket/rpm-repo-config.rktd
```

The default RPM repository config is `rpm-repo-config.rktd`; it explicitly sets
`rpm-repo-root` to `/Users/cutiedeng/Y2026/M06/D22/rpm-racket`, plus the repo
id, display name, baseurl, and gpgcheck/enabled flags. Use command-line
overrides such as `--rpm-repo-root`, `--rpm-repo-baseurl`, and
`--createrepo-bin` when testing another repository root or host.

The direct RPM flow does not run Racket's `make unix-style` and does not use an
installed staging root. It writes the same source-archive `.spec` model used by
the generated `rpm-racket` scripts, uses
`artifacts/racket-minimal-<source-version>-src.tgz` when that file already
exists, and otherwise downloads the configured GitHub Release `Source0`.

Use `rpm-racket/scripts/build-rpm.sh --source-archive ...` when a build host
must use an explicit local source archive instead of downloading `Source0`.

Use `--dry-run` to print the commands without writing package artifacts.
