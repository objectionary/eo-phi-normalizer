# Changelog for `eo-phi-normalizer`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## v0.4.0 — 2024-06-03

This version supports fast dataization with built-in rules and improves metrics with both built-in and Yegor's rules.

New:

- Add built-in rules
- Add more built-in dataization functions ([#291](https://github.com/objectionary/normalizer/pull/291))
- Support LaTeX format in output ([#308](https://github.com/objectionary/normalizer/pull/308))
- Speed up pipeline by caching EO compilation results ([#340](https://github.com/objectionary/normalizer/pull/340))
- Write generated PHI files as eo-phi-normalizer data files ([#286](https://github.com/objectionary/normalizer/pull/286))
- Update and commit docs in CI ([#286](https://github.com/objectionary/normalizer/pull/286))

Changes and fixes:

- Switch to EO 0.38.0 ([#335](https://github.com/objectionary/normalizer/pull/335))
- Remove VTX and Sigma ([#335](https://github.com/objectionary/normalizer/pull/335))
- Fix normalization and dataization rules w.r.t. xi and rho (see [#297](https://github.com/objectionary/normalizer/pull/297))
- Fix confluence tests ([#319](https://github.com/objectionary/normalizer/pull/319))
- Fix pipeline tests ([#338](https://github.com/objectionary/normalizer/pull/338))
- Integrate `transform-eo-tests` into normalizer ([#365](https://github.com/objectionary/normalizer/pull/365))
- Switch to GHC 9.6.4 ([#263](https://github.com/objectionary/normalizer/pull/263))

Documentation and maintenance:

- Add the `Quick Start` page ([#317](https://github.com/objectionary/normalizer/pull/317))
- Add the `Pipeline` page ([#261](https://github.com/objectionary/normalizer/pull/261))

## v0.3.1 — 2024-04-12

This version supports proper dataization of test programs with dependencies.

New:

- Add `--dependency-file FILE` option to enable dependencies (e.g. `bool.phi`, `bytes.phi`) (see [#265](https://github.com/objectionary/normalizer/pull/265))

Changes and fixes:

- Fix handling of `Package` atom (also improves related code, see [#239](https://github.com/objectionary/normalizer/pull/239) and [#265](https://github.com/objectionary/normalizer/pull/265))
- Update the dataless objects metric (see [#234](https://github.com/objectionary/normalizer/pull/234))
- Improve reports (see [#225](https://github.com/objectionary/normalizer/pull/225))

Documentation and maintenance:

- Establish proposals format and process (see [#248](https://github.com/objectionary/normalizer/pull/248))
- Improve the docs job, improve the site, fix minor errors (see [#228](https://github.com/objectionary/normalizer/pull/228))
- Fix `hlint` warnings (see [#241](https://github.com/objectionary/normalizer/pull/241))
- CI: run doctests (see [#230](https://github.com/objectionary/normalizer/pull/230))
- CI: switch back to the official `stack-action` (see [#246](https://github.com/objectionary/normalizer/pull/246))
- CI: publish extra files in artifacts (in particular, dependencies like `bool.phi`, see [#245](https://github.com/objectionary/normalizer/pull/245))

## v0.3.0 — 2024-03-26

In this version the main executable has been renamed to `normalizer`
and several commands have been implemented for normalization, dataization,
and reporting.

New:

- Command Line Interface:
  - Add commands:
    - `normalizer transform` to perform normalization without executing any atoms (was default behavior before)
    - `normalizer metrics` to compute metrics of a given φ-expression (see [#153](https://github.com/objectionary/normalizer/pull/153))
    - `normalizer dataize` to run partial evaluation of φ-expressions with atoms (see [#187](https://github.com/objectionary/normalizer/pull/187))
    - `normalizer report` to generate report based on the results of testing against the EO compiler (see [#213](https://github.com/objectionary/normalizer/pull/213))
  - Add `--single` flag (see [#131](https://github.com/objectionary/normalizer/pull/131))
  - Add `--json` flag for machine-readable output format (see [#143](https://github.com/objectionary/normalizer/pull/143))
  - Add `--max-depth=N` and `--max-term-size=N` options to control limits for the normalizer (see [#173](https://github.com/objectionary/normalizer/pull/173))
  - Improve `--chain` option to provide better elaboration on the applied normalization rules and the dataization process (see [#195](https://github.com/objectionary/normalizer/pull/195))
- Update rule set for φ-calculus (see [#152](https://github.com/objectionary/normalizer/pull/152) and some changes in [#136](https://github.com/objectionary/normalizer/pull/136) and [#166](https://github.com/objectionary/normalizer/pull/166))
- Add property-based and regression tests for confluence (see [#136](https://github.com/objectionary/normalizer/pull/136) and [#166](https://github.com/objectionary/normalizer/pull/166))
- Tests and metrics against the EO compiler and standard EO test suite (see [#98](https://github.com/objectionary/normalizer/pull/98), [#191](https://github.com/objectionary/normalizer/pull/191))

Fixes:

- Count metrics, including dataless objects, correctly (see [#142](https://github.com/objectionary/normalizer/pull/142), [#193](https://github.com/objectionary/normalizer/pull/193), [#211](https://github.com/objectionary/normalizer/pull/211))
- Fix metavariables in context patterns (see [#174](https://github.com/objectionary/normalizer/pull/174))
- Support empty Δ-bindings (see [#184](https://github.com/objectionary/normalizer/pull/184))

Documentation has been improved (see [#134](https://github.com/objectionary/normalizer/pull/134), [#221](https://github.com/objectionary/normalizer/pull/221)).

Maintenance:

- Run CI on all pull requests (see [#156](https://github.com/objectionary/normalizer/pull/156))
- Downgrade Stackage snapshot (see [#146](https://github.com/objectionary/normalizer/pull/146))
- Add HLint to CI (see [#157](https://github.com/objectionary/normalizer/pull/157))

## v0.2.0 — 2024-02-16

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

## v0.1.0 - 2024-02-02

First version of the normalizer.
