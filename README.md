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
- `rpm`: installs into a staged root with `make unix-style` and builds a `.rpm`.

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

`package-racket` is the source of truth for generated packaging metadata. The
Homebrew tap receives overwritten generated outputs such as `Formula/racket@9.rb`
and `.github/workflows/*.yml`; keep their maintainable configuration here.

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

## Requirements

- Racket with the `tstring` reader available.
- For `brew`: an explicit `--homebrew-tap` whose root contains
  `Formula/racket@9.rb`, plus an explicit `--bottle-root-url` for the
  Homebrew bottle release assets. The brew source `.tgz` is generated directly
  by `package-racket`; no helper script is required in the Homebrew tap.
- For `brew-ci`: Ruby for YAML validation, plus an explicit
  `--bottle-root-url` for `brew test-bot`, release uploads, and
  `brew bottle --merge`.
- For `source-release`: a fine-grained GitHub personal access token for
  `CutieDeng/racket` with `Contents: Read and write`, stored locally as one
  Racket string datum in `secret/ghtoken.rktd`. The file is ignored by Git and
  should be mode `600`.
- For `apt`: `dpkg-deb`, or `ar` + `tar` + `xz` through the automatic fallback.
- For `rpm`: `rpmbuild`.

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

### Source Release Token

The stable GitHub release settings live in `source-release-config.rktd`. The
default token file is `secret/ghtoken.rktd`, and it must contain exactly one
Racket string datum:

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
`racket-minimal-<version>-src.tgz`, matching the basename used by the Formula
source URL. In incremental mode, the Formula bottle `root_url` is taken from
`--bottle-root-url`; in full mode, the same value is still required for brew
CI and bottle publishing.
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

The generated `publish.yml` runs on pushes to `main`, builds macOS arm64 and
Linux x64 bottles, uploads them to the release selected by
`--bottle-root-url`, merges the bottle JSON back into `Formula/racket@9.rb`,
then pushes a `[skip bottles]` Formula update to avoid a publish loop.

### Package Prefix

`--prefix` controls where the staged installation is rooted inside `apt` and
`rpm` packages. For example, with `--prefix /opt/racket9`, the staged
installation should be under `/tmp/racket-package-root/opt/racket9`.

This value is not the Homebrew installation prefix. Homebrew chooses its own
Cellar and opt paths when building the Formula, and `source-release` only
uploads an existing `.tgz`. Therefore the command output prints `Prefix:` only
for targets that actually stage an install root, such as `apt` and `rpm`.

Create a Debian package from a Linux x64 build:

```sh
racket package-racket.rkt \
  --target apt \
  --racket-root /path/to/racket.git \
  --prefix /opt/racket9 \
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

Create an RPM package from a Linux x64 build:

```sh
racket package-racket.rkt \
  --target rpm \
  --racket-root /path/to/racket.git \
  --prefix /opt/racket9 \
  --rpm-arch x86_64
```

Reuse an already installed staging root instead of running `make unix-style`:

```sh
racket package-racket.rkt \
  --target apt \
  --target rpm \
  --racket-root /path/to/racket.git \
  --skip-build \
  --install-root /tmp/racket-package-root \
  --prefix /opt/racket9
```

The `--install-root` directory must contain the package filesystem root.

Use `--dry-run` to print the commands without writing package artifacts.
