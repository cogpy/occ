---
name: guix-ci-workflow
description: Change, fix, or test the GNU Guix CI for the OpenCog Collection — the `.github/workflows/ci-guix.yml` workflow that syntax-checks the Guix files and builds `guix.scm` (one Guix package per OpenCog component). Use this whenever the user mentions ci-guix.yml, guix-build.yml, a Guix CI job, `guix build -f guix.scm` in CI, a Guix workflow that hangs/freezes/times out, `guix pull` in Actions, a Guix syntax-validation failure, or asks to "test the guix build", or says the Guix build is skipped or not running — even if they don't say "workflow". Also use it when adding or fixing a component package in `guix.scm`.
---

# Guix CI for OCC

`main` consolidated CI (PRs #167/#168): **`ci-guix.yml` is the only Guix
workflow.** `guix-build.yml` and `occ-build.yml` exist only as
`*.temp_disabled`. Make Guix CI changes in `ci-guix.yml`; don't re-enable or
re-create a separate Guix workflow unless the user explicitly asks — that
reverses a deliberate decision and duplicates runs.

## How ci-guix.yml is laid out

| Job | Role |
|-----|------|
| `syntax-validation` | Reads `guix.scm`, `guix-simple.scm`, `occ-hurdcog-unified.scm` with plain Guile's reader (S-expressions only, Guix modules not loaded), via a small `read-guix.scm` it writes to `$RUNNER_TEMP`. |
| `guix-build` | Installs the **Guix 1.5.0 release** from its signed binary tarball, then runs `guix build -f guix.scm` on every push and PR that touches the Guix files or a packaged component directory. Fails when the build fails. |
| `summary` | `if: always()`; fails if either job didn't succeed. |

This job used to skip the build on everything but `workflow_dispatch` and
still pass (`continue-on-error` on the step *and* the job). Don't bring
either back. A green check has to mean `guix.scm` built.

`guix-simple.scm` is syntax-checked only. It was never buildable: it
quasi-quotes `,(modify-phases …)` (so phases run on the host) and copies the
whole 2.3 GB checkout.

## How guix.scm is laid out

Each OpenCog component is a standalone CMake project that expects its
dependencies to be **installed** first. So `guix.scm` defines one package
per component (`occ-cogutil`, `occ-atomspace`, …), each built from its own
subdirectory with `occ-source`. The packages are chained in dependency order, and
`opencog-collection` (the file's return value) propagates them all. A single
combined CMake build can't work: the root `CMakeLists.txt` `add_subdirectory`s
projects that define clashing targets (`tests`, `check`) and look for
installed `*Config.cmake` files.

Build one component with
`guix build -e '(begin (load "guix.scm") occ-atomspace)'`.

## Rules learned the hard way

**`guix pull` stays opt-in.** It froze CI runs. It only runs from the
`update_channels` dispatch input (default `false`), with a `timeout-minutes`
limit. Never add an unconditional pull when fixing something else. CI uses the
Guix 1.5.0 release as installed; `guix.scm` builds with it.

**Don't let `tee` hide build results.** `guix build ... | tee log || true`
throws the exit code away. Read `${PIPESTATUS[0]}` after the pipe and write
✅/❌ per file into `$GITHUB_STEP_SUMMARY`.

**PATH doesn't survive steps.** `source /etc/profile.d/guix.sh` only lasts for
that step; append `/var/guix/profiles/per-user/root/current-guix/bin` to
`$GITHUB_PATH` or later `command -v guix` checks silently skip the build.

**The triggers are path-filtered.** They cover the Guix files, the
workflow, and each packaged component directory. When you package a new
component, add its directory to both `push` and `pull_request` `paths:`, or
changes to it won't run the build.

**Plain Guile can't read G-expressions.** `#~`, `#$`, `#$@`, `#+` and
`#+@` are defined by `(guix gexp)`, and the syntax job doesn't load Guix.
Without stand-in `read-hash-extend` handlers, every file that uses gexps
fails with `Unknown # object: "#~"`. That kept the required job red on `main`
from the start. Keep those handlers in `read-guix.scm`, and add one if a new
Guix reader prefix appears.

**Separate prefixes expose hidden `/usr/local` assumptions.** In Guix every
package has its own store prefix. Things that "worked" because everything
shared `/usr/local` break, and each fix belongs in the component's CMake
(keep the `/usr/local` result unchanged) or in `guix.scm`:
- CMake modules from atomspace (`OpenCogMacros`, …) aren't on the module
  path. Use `occ-atomspace-configure-flags` for anything built on the AtomSpace.
- Hard-coded include paths (e.g. `-I/usr/local/include/opencog/cython`): use
  the `*_INCLUDE_DIR` that `find_package` sets.
- Installing into another package's data dir (`${COGUTIL_DATA_DIR}/…`):
  install under `${CMAKE_INSTALL_PREFIX}` instead.
- Anything that writes to `/etc` (cogutil's `ld.so.conf.d` snippet): point its
  cache variable (`CMAKE_CONF_PREFIX`) into the output.

**Newer toolchains in Guix catch real bugs.** GCC 14 drops transitive
includes (add `#include <algorithm>`), Asio ≥ 1.33 removed `io_service` (use
`io_context`), Cython 3 is stricter. Fix the source, not the toolchain.

**Run-time Guile deps go in `propagated-inputs`.** A package can build and
still fail on `(use-modules …)`, e.g. `(opencog learn)` needs
`guile-readline` and `occ-lg-atomese`. Load every component's modules in a
pure shell before calling it done (see Testing).

**Keep large unused folders out of the store.** `occ-source` takes
`#:exclude` (learn leaves out `attic/` and `learn-lang-diary/`, ~700 MB).

**Watch `guix.scm` after merges.** A bad merge once left duplicate
description lines and a stray `")` after the package's closing parens, which
broke the required syntax job on `main`. Anything after the package's
closing `)))` other than the final `opencog-collection` line is a leftover.

## Testing

Check what the sandbox can do first. Some sessions have no `sudo`/Docker
and blocked downloads. Others run as root with working `sudo`, Docker and
access to ftp.gnu.org and the substitute servers. Then a real build works:

1. Install the same Guix 1.5.0 release CI uses: download the tarball and `.sig`
   from ftp.gnu.org, verify against the keys in `etc/guix-install.sh`, unpack
   as root, create `guixbuild` users, `guix archive --authorize` the ci and
   bordeaux keys, start `guix-daemon --build-users-group=guixbuild`. The daemon
   doesn't survive a sandbox restart. Check `ps -C guix-daemon` and restart it.
2. Run long builds detached (`setsid nohup … > log &`) and watch the log. Tool
   calls time out after 10 minutes. Use `--keep-going` so independent packages
   still build, and read `/var/log/guix/drvs/…drv.gz` for failures.
3. Smoke-test in a pure shell, e.g.
   `guix shell --pure -f guix.scm guile -- guile -c '(use-modules (opencog learn))'`.
4. To test the workflow's *Install Guix* step itself, extract it from the
   YAML and run it in `ubuntu:22.04` (`mirror.gcr.io/library/ubuntu:22.04` if
   Docker Hub rate-limits) as a non-root user with `sudo`, like the runner.

Without Guix, still run the CI syntax step with real Guile. `apt-get
download` + `dpkg -x` need no root:

```bash
export PATH="$(sh .claude/skills/guix-ci-workflow/scripts/local-guile.sh "$SCRATCH/guile"):$PATH"
```

Pull the step's `run:` script out of the YAML with PyYAML and execute it with
`bash -e`, with `RUNNER_TEMP` and `GITHUB_STEP_SUMMARY` pointed at scratch
paths. Run it on `origin/main` too, to reproduce a failure before fixing it.

Also run the bundled static checks:

```bash
python3 .claude/skills/guix-ci-workflow/scripts/validate.py
# defaults: --scm guix.scm --workflow .github/workflows/ci-guix.yml
```

They check `.scm` paren/string balance, package fields and that the file ends
by returning one of its packages. For the workflow they check YAML,
triggers, `needs:`, that `guix pull` is gated and timed out, and that a
piped `guix build` uses `PIPESTATUS`. They don't run a build. PyYAML reads
`on:` as `True`; the script handles that.

After pushing, check the PR's runs, and read the **Guix Build** job log, not
only its status. `pull_request` workflows don't start while a PR has a merge
conflict. To read a job log through the API, don't send the GitHub auth header
to the blob-storage redirect, or it returns 401.

## Finish

Commit on the designated branch with a message that says what changed and
why, push, and open a draft PR if none exists. Some other checks have been
failing on every PR, independently of Guix: Cloudflare Pages, Workers
Builds, and `ci-windows.yml` (CogUtil (fast), Windows Build Summary). Don't chase them as part of Guix
work, but do mention them.
