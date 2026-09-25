#!/usr/bin/env python3
"""Static checks for a Guix package file and the GitHub Actions workflow that builds it.

Use this when guix/guile/docker/sudo are unavailable (e.g. cloud sandboxes).
It catches structural mistakes; it is NOT a substitute for a real `guix build`.

Usage:
    python3 validate.py [--scm guix.scm] [--workflow .github/workflows/guix-build.yml]
Exit code is non-zero if any hard error is found.
"""
import argparse
import re
import sys

import yaml


def check_scm(path):
    errors, warnings = [], []
    try:
        text = open(path).read()
    except OSError as e:
        return [f"cannot read {path}: {e}"], []

    # Paren balance, ignoring strings, #\ char literals and ; comments.
    depth, line, in_str, i = 0, 1, False, 0
    while i < len(text):
        c = text[i]
        if c == "\n":
            line += 1
        if in_str:
            if c == "\\":
                i += 1
            elif c == '"':
                in_str = False
        elif c == '"':
            in_str = True
        elif c == ";":
            while i < len(text) and text[i] != "\n":
                i += 1
            continue
        elif c == "#" and text[i + 1:i + 2] == "\\":
            i += 3
            continue
        elif c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth < 0:
                errors.append(f"{path}:{line}: unmatched ')'")
                depth = 0
        i += 1
    if in_str:
        errors.append(f"{path}: unterminated string literal")
    if depth:
        errors.append(f"{path}: {depth} unclosed '('")

    for needle in ("(use-modules", "(package", "(name", "(version", "(source",
                   "(build-system", "(synopsis", "(description", "(license"):
        if needle not in text:
            errors.append(f"{path}: missing {needle}")

    # `guix build -f` builds whatever the file's last expression returns.
    m = re.search(r"\(define-public\s+([\w-]+)", text)
    if m and not re.search(rf"^\s*{re.escape(m.group(1))}\s*$", text, re.M):
        warnings.append(f"{path}: file should end by returning `{m.group(1)}` for `guix build -f`")

    for bs, mod in (("cmake-build-system", "(guix build-system cmake)"),
                    ("gnu-build-system", "(guix build-system gnu)"),
                    ("python-build-system", "(guix build-system python)")):
        if bs in text and mod not in text:
            errors.append(f"{path}: uses {bs} but does not import {mod}")
    return errors, warnings


def check_workflow(path):
    errors, warnings = [], []
    try:
        wf = yaml.safe_load(open(path))
    except (OSError, yaml.YAMLError) as e:
        return [f"{path}: {e}"], []

    # PyYAML (YAML 1.1) parses a bare `on:` key as boolean True.
    on = wf.get("on", wf.get(True))
    if on is None:
        errors.append(f"{path}: missing `on:` triggers")
    for key in ("name", "jobs"):
        if key not in wf:
            errors.append(f"{path}: missing `{key}:`")

    jobs = wf.get("jobs") or {}
    for name, job in jobs.items():
        if "runs-on" not in job:
            errors.append(f"{path}: job {name} missing runs-on")
        needs = job.get("needs", [])
        for dep in [needs] if isinstance(needs, str) else needs:
            if dep not in jobs:
                errors.append(f"{path}: job {name} needs unknown job {dep}")

    raw = open(path).read()
    # `guix pull` hangs CI runners; it must only run on explicit opt-in.
    for m in re.finditer(r"guix pull", raw):
        block = raw[max(0, raw.rfind("- name:", 0, m.start())):m.start()]
        if "update_channels" not in block:
            errors.append(f"{path}: `guix pull` is not gated behind the update_channels input")
            break
    if "guix pull" in raw and "timeout-minutes" not in raw:
        warnings.append(f"{path}: `guix pull` step has no timeout-minutes")
    if "guix build" not in raw:
        warnings.append(f"{path}: no `guix build` invocation found")
    if "PIPESTATUS" not in raw and "| tee" in raw:
        warnings.append(f"{path}: `| tee` without PIPESTATUS hides build failures")
    return errors, warnings


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--scm", default="guix.scm")
    ap.add_argument("--workflow", default=".github/workflows/guix-build.yml")
    args = ap.parse_args()

    errors, warnings = [], []
    for fn, p in ((check_scm, args.scm), (check_workflow, args.workflow)):
        e, w = fn(p)
        errors += e
        warnings += w

    for w in warnings:
        print(f"WARN  {w}")
    for e in errors:
        print(f"ERROR {e}")
    print("PASSED (static checks only)" if not errors else "FAILED")
    sys.exit(1 if errors else 0)


if __name__ == "__main__":
    main()
