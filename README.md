# package-racket

Packaging helper for a local Racket checkout. The command supports named
packaging flows:

- `brew`: creates the Homebrew source `.tgz`, generates a staged
  `brew/Formula/racket@9.rb`, validates it, then replaces the formula in the
  Homebrew tap at the end of a successful script run unless disabled.
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
against the generated source `.tgz`. The `brew-ci` flow also validates generated
workflow YAML and required workflow content before replacing files in the tap.
The publish workflow updates release assets and the Formula only after every
bottle runner succeeds.

`package-racket` is the source of truth for generated packaging metadata. The
Homebrew tap receives overwritten generated outputs such as `Formula/racket@9.rb`
and `.github/workflows/*.yml`; keep their maintainable configuration here.

## Requirements

- Racket with the `tstring` reader available.
- For `brew`: an explicit `--homebrew-tap` whose root contains
  `racket-to-brew-tgz.rkt` and `Formula/racket@9.rb`, plus an explicit
  `--bottle-root-url` for the Homebrew bottle release assets.
- For `brew-ci`: Ruby for YAML validation, plus an explicit
  `--bottle-root-url` for `brew test-bot`, release uploads, and
  `brew bottle --merge`.
- For `apt`: `dpkg-deb`, or `ar` + `tar` + `xz` through the automatic fallback.
- For `rpm`: `rpmbuild`.

## Examples

Create the Homebrew source archive and update the formula:

```sh
racket package-racket.rkt \
  --target brew \
  --racket-root /Users/cutiedeng/Y2026/M04/D03/racket.git \
  --homebrew-tap /opt/homebrew/Library/Taps/cutiedeng/homebrew-racket \
  --bottle-root-url https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1
```

For `brew`, `--formula` means the final tap formula path. When omitted, it is
derived from the explicit `--homebrew-tap` as `Formula/racket@9.rb`. The script
copies that file into `.build/brew/Formula/`, lets the brew helper update the
staged copy, then replaces the tap formula only after all selected targets
succeed. The generated Homebrew source archive is
`racket-minimal-<version>-src.tgz`, matching the basename used by the Formula
source URL. The Formula bottle `root_url` is taken from `--bottle-root-url`.

Generate the Homebrew tap CI workflows from `brew-ci-config.rktd` and overwrite
the tap workflow files after validation. The generated `publish.yml` runs on
pushes to `main`, builds macOS arm64 and Linux x64 bottles, uploads them to the
release selected by `--bottle-root-url`, merges the bottle JSON back into
`Formula/racket@9.rb`, then pushes a `[skip bottles]` Formula update to avoid a
publish loop:

```sh
racket package-racket.rkt \
  --target brew-ci \
  --racket-root /Users/cutiedeng/Y2026/M04/D03/racket.git \
  --homebrew-tap /opt/homebrew/Library/Taps/cutiedeng/homebrew-racket \
  --bottle-root-url https://github.com/CutieDeng/homebrew-racket/releases/download/v9.2.1
```

Use `--brew-ci-config` to point at another workflow config file, and
`--homebrew-tap` to explicitly select the tap that receives the generated
workflows. The generated workflows pass `--bottle-root-url` to Homebrew as
`--root-url`. If any bottle build fails, the publish job does not run.

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

The `--install-root` directory must contain the package filesystem root. For
example, with `--prefix /opt/racket9`, the staged installation should be under
`/tmp/racket-package-root/opt/racket9`.

Use `--dry-run` to print the commands without writing package artifacts.
