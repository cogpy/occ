---
name: guix-ci-workflow
description: Change, fix, or test the GNU Guix CI for the OpenCog Collection — the `.github/workflows/ci-guix.yml` workflow that syntax-checks and builds `guix.scm` / `guix-simple.scm`. Use this whenever the user mentions ci-guix.yml, guix-build.yml, a Guix CI job, `guix build -f guix.scm` in CI, a Guix workflow that hangs/freezes/times out, `guix pull` in Actions, a Guix syntax-validation failure, or asks to "test the guix build" — even if they don't say "workflow".
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
| `syntax-validation` | **Required.** Reads `guix.scm`, `guix-simple.scm`, `occ-hurdcog-unified.scm` with Guile's reader (S-expressions only, Guix modules not loaded). |
| `guix-build` | **Experimental, non-blocking** (`continue-on-error`). Installs Guix, optionally pulls, builds on `workflow_dispatch`. |
| `summary` | `if: always()`; fails only if syntax validation failed. |

Keep that split: the build is expected to fail in CI sometimes, so it
reports rather than blocks. Non-blocking is not the same as silent, though
— see below.

## Rules learned the hard way

**`guix pull` stays opt-in.** It froze CI runs. It only runs from the
`update_channels` dispatch input (default `false`), with `timeout-minutes: 10`
and `continue-on-error: true`. Never add an unconditional pull when fixing
something else.

**Don't let `tee` hide build results.** `guix build ... | tee log || true`
throws the exit code away. Read `${PIPESTATUS[0]}` after the pipe and write
✅/❌ per file into `$GITHUB_STEP_SUMMARY`.

**PATH doesn't survive steps.** `source /etc/profile.d/guix.sh` only lasts for
that step; append `/var/guix/profiles/per-user/root/current-guix/bin` to
`$GITHUB_PATH` or later `command -v guix` checks silently skip the build.

**The PR trigger is path-filtered.** `pull_request` runs only for `*.scm`
and `.github/workflows/ci-guix.yml`. If you change something else the Guix
job depends on (e.g. `test-guix-syntax.sh`), add it to `paths:` or the PR
won't exercise it.

**Watch `guix.scm` after merges.** A bad merge once left duplicate
description lines and a stray `")` after the package's closing parens, which
broke the required syntax job on `main`. Anything after the package's
closing `)))` other than the final `opencog-collection` line is a leftover.

## Testing

The cloud sandbox usually can't run a real build: no `guix`, no `guile`, no
Docker, `sudo` is broken (`/etc/sudoers is owned by uid 999`), and
`guix-install.sh` downloads are blocked. Check once (`which guix guile docker`)
and move on instead of trying to install.

Run the bundled static checks:

```bash
python3 .claude/skills/guix-ci-workflow/scripts/validate.py
# defaults: --scm guix.scm --workflow .github/workflows/ci-guix.yml
```

For the `.scm` file it checks balanced parens and strings (it understands
strings, `;` comments and `#\` characters). This is the same thing the CI's
Guile reader enforces. It also checks for the required package fields and
build-system imports. For the workflow it checks YAML parsing, triggers and
`needs:` targets. Per step, it checks that `guix pull` has an
`update_channels` condition and a timeout, and that a piped `guix build` uses
`PIPESTATUS`. PyYAML reads a bare `on:` key as `True`. The script handles that,
so don't "fix" it in the workflow. The package-field checks only fit
package-definition files. Expect "missing (use-modules" on a file like
`occ-hurdcog-unified.scm`.

Say plainly that these are static checks. The real verification is the PR's
`Guix Build` run, or a manual dispatch with `attempt_build` on and
`update_channels` off.

## Finish

Commit on the designated branch with a message that says what changed and
why, push, and open a draft PR if none exists. The Cloudflare Pages and
Workers Builds checks have been failing on every PR, independently of Guix.
Don't chase them as part of Guix work, but do mention them.
