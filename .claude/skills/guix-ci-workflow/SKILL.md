---
name: guix-ci-workflow
description: Create, redesign, fix, or test the GNU Guix GitHub Actions workflow (.github/workflows/guix-build.yml) that builds guix.scm for the OpenCog Collection. Use this whenever the user mentions guix-build.yml, a Guix CI job, `guix build -f guix.scm` in CI, a Guix workflow that hangs/freezes/times out, `guix pull` in Actions, or asks to "test the guix build" — even if they don't say "workflow".
---

# Guix CI workflow for OCC

This repo builds its C++ stack two ways in CI: component-by-component with CMake
(`occ-build.yml`, `debian-packages.yml`) and as one reproducible package via
`guix build -f guix.scm` (`guix-build.yml`). This skill covers the Guix one.

## 1. Match the house style first

Read `.github/workflows/occ-build.yml` before editing. New Guix workflow code
should look like it belongs next to it:

- Triggers: `push` / `pull_request` on `main` and `master`, plus `workflow_dispatch`.
- Top-level `env:` for knobs (`GUIX_BUILD_CORES`, `GUIX_BUILD_OPTIONS`).
- `# Stage N: ...` comments above each job, `needs:` chains between stages.
- `actions/checkout@v4` with `submodules: false`; `actions/upload-artifact@v4`
  and `actions/cache@v4`.
- Non-essential steps (tests, analysis) get `continue-on-error: true`; the
  build step itself must fail loudly.
- A final `if: always()` job writes a table to `$GITHUB_STEP_SUMMARY`.

## 2. Workflow shape

| Job | Purpose |
|-----|---------|
| `validate-guix-scm` | Cheap sanity check of `guix.scm` before spending runner time |
| `guix-build` | Free disk space → install Guix → configure PATH → (optional) pull → build → report/upload |
| `analyze-derivation` | Optional: `guix build --derivation`, graph; `continue-on-error` |
| `report-status` | `if: always()`, summarises `needs.*.result` |

Build step essentials:

```bash
sudo guix build --file=guix.scm --cores=${{ env.GUIX_BUILD_CORES }} \
  --keep-going --fallback 2>&1 | tee guix-build.log
BUILD_STATUS=${PIPESTATUS[0]}   # without this, tee masks failures
```

Expose `build_status`, `build_duration`, `store_path` via `$GITHUB_OUTPUT` so
the report steps can use them. Free disk first (`/usr/share/dotnet`,
`/usr/local/lib/android`, `/opt/ghc`) — the store fills a stock runner quickly.
Install via `guix-install.sh` with the binary-tarball fallback, and persist
`/var/guix/profiles/per-user/root/current-guix/bin` onto `PATH` via `$GITHUB_ENV`.

## 3. `guix pull` stays off by default

The user found that `guix pull` freezes CI runs. Treat that as a hard
requirement, not a preference:

- Add a `workflow_dispatch` boolean input `update_channels`, default `false`.
- Gate the pull step with `if: github.event.inputs.update_channels == 'true'`
  and give it `timeout-minutes: 10` plus `continue-on-error: true`.
- Add a sibling "Skip Channel Update" step (inverse condition) that just echoes
  why it was skipped, so logs make the choice obvious.
- A `guix_channel` commit input is fine, but it only applies when
  `update_channels` is true — say so in its description.

Push and PR runs therefore never pull. Don't reintroduce an unconditional pull
when "fixing" something else.

## 4. Testing

A real Guix build is usually impossible in the cloud sandbox: no `guix`, no
`guile`, no Docker, and `sudo` is broken (`/etc/sudoers is owned by uid 999`),
and fetching `guix-install.sh` is blocked. Check quickly (`which guix guile docker`)
and move on rather than repeatedly trying to install.

Run the bundled static checks instead:

```bash
python3 .claude/skills/guix-ci-workflow/scripts/validate.py
# options: --scm guix.scm --workflow .github/workflows/guix-build.yml
```

It checks `guix.scm` paren balance (string/comment aware), required package
fields, build-system imports, and that the file returns the package; and for
the workflow: YAML parse, triggers, `needs:` targets, that `guix pull` is gated
behind `update_channels`, and the `tee`/`PIPESTATUS` trap. Note PyYAML reads a
bare `on:` key as `True` — the script handles it, so don't "fix" the workflow
for it.

When reporting, be explicit that these are static checks and the real
verification is a CI run: open/push the PR, or trigger the workflow manually
from the Actions tab (leave `update_channels` off).

## 5. Finish

Commit on the designated branch with a message that says what changed and why
(e.g. "Disable Guix channel update by default to prevent freezing"), push, and
open a draft PR if none exists.
