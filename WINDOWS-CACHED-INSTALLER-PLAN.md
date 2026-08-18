# Windows cached installer — Requirements & Design

Status: PLANNED (design settled, PoC not yet run).
Audience: whoever implements the `cached` install variant for Windows.
Written 2026-08-18 against the 9.3.3 cycle. Verify file:line references
in `package-racket.rkt` before relying on them.

## 1. Motivation / current state

Windows currently ships two artifacts per arch (see `windows-ci-build-job`):

- **Portable zip**: the nmake-built tree. CORRECTION (verified from the
  released 9.3.3 zip, 2026-08-18): it ships **ZERO compiled files** — 1011
  `.rkt` sources and no `.zo` anywhere, so every launch interprets from
  source (`raco help` takes ~18s on a CI runner). Sources carry the
  reproducible source archive's 1980-01-01 timestamps.
- **`-setup.exe` (Inno)**: copies the same tree into `{autopf}\Racket9`,
  asks for a cache dir (default `{app}\var\cache\racket\compiled`,
  `/CACHEPATH=` for unattended), records it in `HKLM\Software\Racket9`,
  rewrites `etc/config.rktd` (default-scope installation,
  compiled-file-cache-roots `(user system)`, compiled-file-system-cache-root),
  then runs **`raco setup --system --no-user --reset-cache -D --no-pkg-deps`
  on the user's machine** to build the system cache. This is exactly the
  DEB `postinstall` model — and on a low-end machine that raco setup is the
  slow part of installation.

DEB/RPM solve this with a second variant: `cached` embeds the prebuilt
system cache in the payload (bigger download, zero install-time compile)
and outranks `postinstall` at the same release. Windows has no equivalent.
Goal: a `-setup-cached.exe` that installs with **no raco setup at all**.

## 2. Key technical facts (verified in-tree, 9.3.3)

1. **The cache is keyed by absolute source path.**
   `racket/collects/setup/compiled-cache.rkt` `path->cache-relative-path`
   strips the path root and mirrors the rest under the cache root
   (`C:\Program Files\Racket9\collects\...` →
   `<cache-root>\Program Files\Racket9\collects\...`). Consumption side:
   `racket/src/expander/eval/collection.rkt` `find-compiled-file-roots`
   feeds the roots into `current-compiled-file-roots` (standard rerooting).
   ⇒ **A CI-built cache is only valid for the exact install path it was
   built against.** The cached installer therefore only fast-paths the
   default `C:\Program Files\Racket9`; any custom dir must fall back to
   the existing postinstall raco setup.
2. **Validity is mtime-based** (default `use-compiled-file-check`
   'modify-seconds). Inno preserves archive file times on extract, and the
   cache is built after the sources on CI, so zo ≥ source holds after
   install. (PoC must confirm; fallback: touch the cache tree newest-last
   during CI staging.)
3. **Cache miss degrades to portable-zip behavior** — which (see §1
   correction) means from-source interpretation: functional but painfully
   slow. The cached variant is therefore not a minor optimization; it is
   the first actually-precompiled Windows Racket artifact.
3b. **The first setup on a zo-less tree cannot self-validate.** raco setup
   bootstraps from source ("ignoring compiled files, rebuilding from
   source"), and the cache it writes gets rewritten wholesale by the next
   setup run (1910/1985 files, PoC run 6) even though mtimes are valid.
   Convergence: build the cache with TWO setup passes on CI; passes three
   and four (in-place and post-repackage) must then no-op — PoC-gated.
4. The DEB `cached` flow already proves build-elsewhere/run-at-target works
   when the absolute path matches (staged root → installed root, same
   `/usr` layout; `scripts/build-deb.sh` rewrites
   compiled-file-system-cache-root staged→runtime).

## 3. Design

Extend `windows-ci-build-job` to emit a third artifact per arch,
`racket9-<ver>-windows-<arch>-setup-cached.exe`:

1. **CI cache build**: after the portable tree is assembled, copy it to
   `C:\Program Files\Racket9` (runners are admin), rewrite config exactly
   as `installer-configure.ps1` does with cache root
   `C:\Program Files\Racket9\var\cache\racket\compiled`, run the same
   `raco setup --system --no-user --reset-cache -D --no-pkg-deps`, then
   move the populated `var\cache\racket\compiled` (plus the marker file)
   back into the staging tree. Keep the rewritten `etc/config.rktd` in
   staging too (it already points at the default cache path).
2. **Cached Inno script**: same [Setup]/[Files] as today (payload now
   includes `var\cache\racket\compiled\**`), but [Code] logic:
   - If install dir == `{autopf}\Racket9` AND `/CACHEPATH` not overridden:
     write the registry key and **skip raco setup entirely** (config in the
     payload is already correct).
   - Else: delete the shipped cache payload from `{app}`, then run the
     existing `installer-configure.ps1` path (rewrite config to the chosen
     cache dir + raco setup). Graceful degradation, identical UX to today.
3. **Naming/precedence**: keep `-setup.exe` (postinstall) published as
   today; add `-setup-cached.exe`. Mirror the DEB doc language: cached is
   the recommended default for end users, postinstall remains for custom
   install paths and minimal downloads.
4. **Uninstall**: unchanged (marker-guarded DelTree already handles the
   cache dir).

Costs: installer size grows by the cache payload (measure in PoC; the
in-tree compiled dirs already dominate, cache adds the demod layer);
CI time grows by one raco setup per arch (already paid once today inside
the smoke? no — today's CI never runs the system-cache setup, so this is
net-new, expect minutes).

## 4. PoC plan (win-racket, hand-authored, before touching production)

Follow the poc-*.yml pattern; windows-2022 first, then windows-11-arm:

1. Download the released portable zip (no source build — fast PoC).
2. Extract to `C:\Program Files\Racket9`; rewrite config; run the cache
   raco setup; record cache file count and wall time.
3. Re-zip tree+cache; delete `C:\Program Files\Racket9`; re-extract
   (simulates Inno's file copy); assert racket healthy
   (version/TLS/rktrandom smokes) and cache still validates: compare cold
   `racket -l racket/base -e '(void)'` wall time with the shipped cache vs
   after deleting the cache dir — the demod cache should show a clear gap;
   also assert no new files appear under the cache root during runs
   (mtime-validity proof).
4. Only then wire §3 into `windows-ci-build-job` + configs + tests.

## 5. Open questions for the PoC to answer

- Inno vs. archive timestamps in practice (§2.2) — if extraction resets
  mtimes, add the touch-ordering step.
- Cache dir under Program Files is admin-writable only; runtime only reads
  the system cache (user-scope writes go to `'cache-dir`), so read-only is
  fine — confirm no code path insists on writing the system root at run
  time (delete-compiled-cache! is raco-mediated, admin).
- Actual size/time deltas per arch, to state in release notes.
