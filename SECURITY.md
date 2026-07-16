# Security Policy

## Supported Versions

qfyaml does not maintain long-term-support branches. Security fixes are
only provided for the most recently released version, listed in
`CHANGELOG.md`.

## Reporting a Vulnerability

If you believe you have found a security vulnerability in qfyaml — for
example, in the YAML parsing engine in `src/qfyaml_mod.F90`, which
processes user-supplied `.yml` config files — please report it
privately rather than opening a public GitHub issue.

**Preferred method:** Use GitHub's private vulnerability reporting
feature for this repository:
[https://github.com/yantosca/qfyaml/security/advisories/new](https://github.com/yantosca/qfyaml/security/advisories/new)

**Alternative:** Contact the maintainer directly at
yantosca [at] seas.harvard.edu (see `SUPPORT.md`).

Please include:
- A description of the vulnerability and its potential impact
- Steps to reproduce it (a minimal `.yml` file that triggers the issue
  is ideal)
- The qfyaml version or commit affected

## What to Expect

This project is maintained on a best-effort basis, so there is no
guaranteed response SLA, but we will acknowledge receipt of your
report, investigate, and work with you to understand and address the
issue. We will credit reporters (unless they prefer to remain
anonymous) when a fix is released.

## Out of Scope

General parsing bugs, incorrect output, or "how do I..." questions are
not security reports. Please use the normal channels described in
`SUPPORT.md` and `CONTRIBUTING.md` ([GitHub
issues](https://github.com/yantosca/qfyaml/issues)) for those instead.
