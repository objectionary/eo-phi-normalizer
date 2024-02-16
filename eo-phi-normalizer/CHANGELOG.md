# Changelog for `eo-phi-normalizer`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.2.0 — 2024-02-16

- Complete implementation of Yegor's rules (see [#109](https://github.com/objectionary/normalizer/pull/109), [#112](https://github.com/objectionary/normalizer/pull/112))
  - Support global counter in user-defined rules (see [#105](https://github.com/objectionary/normalizer/pull/105))
  - Context matching (global object and this object, see [#99](https://github.com/objectionary/normalizer/pull/99))
- Fix grammar for $\varphi$-calculus (see [#97](https://github.com/objectionary/normalizer/pull/97) and [#127](https://github.com/objectionary/normalizer/pull/127))
- Improve documentation:
  - Set up wesbite for documentation (see [#104](https://github.com/objectionary/normalizer/pull/104), [#124](https://github.com/objectionary/normalizer/pull/124), and [#128](https://github.com/objectionary/normalizer/pull/128))
  - Update CLI documentation (see [#113](https://github.com/objectionary/normalizer/pull/113))
- Improve command line interface:
  - Support `--output`/`-o` command line option (see [#92](https://github.com/objectionary/normalizer/pull/92))
  - Remove logs from default output (see [#106](https://github.com/objectionary/normalizer/pull/106))
- Allow collection of metrics for $\varphi$-terms (see [#121](https://github.com/objectionary/normalizer/pull/121))

## 0.1.0 - 2024-02-02

First version of the normalizer.
