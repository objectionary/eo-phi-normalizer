# Changelog for `eo-phi-normalizer`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## v3.1.0 - 2025-01-01

### Breaking changes

- Use `new.yaml` in the CLI by default ([#658](https://github.com/objectionary/eo-phi-normalizer/pull/658))

### Changes and fixes

- Simplify pipeline config format ([#648](https://github.com/objectionary/eo-phi-normalizer/pull/648))
- Use dependencies for EO 0.50.0 ([#653](https://github.com/objectionary/eo-phi-normalizer/pull/653))
- Add tests from hone-maven-plugin ([#655](https://github.com/objectionary/eo-phi-normalizer/pull/655))

## v3.0.0 - 2024-12-28

### Breaking changes

- Switch to built-in rules in the pipeline until `new.yaml` rules work ([#619](https://github.com/objectionary/eo-phi-normalizer/pull/619))
- Update EO to 0.50.0, use `yegor.yaml` by default ([#617](https://github.com/objectionary/eo-phi-normalizer/pull/617), [#641](https://github.com/objectionary/eo-phi-normalizer/pull/641))

### Changes and fixes

- Rename `Normal form` to `NF` ([#627](https://github.com/objectionary/eo-phi-normalizer/pull/627))
- Run CI on PRs to `develop` ([#638](https://github.com/objectionary/eo-phi-normalizer/pull/638))
- Sort options in command help messages ([#642](https://github.com/objectionary/eo-phi-normalizer/pull/642))

### Documentation and maintenance

- Add quick start to the README ([#632](https://github.com/objectionary/eo-phi-normalizer/pull/632))
- Remove the description of dataization ([#635](https://github.com/objectionary/eo-phi-normalizer/pull/635))
- chore(deps): update baptiste0928/cargo-install action to v3.2.0 ([#640](https://github.com/objectionary/eo-phi-normalizer/pull/640))

## v2.3.0 - 2024-12-05

### Breaking changes

- Fix issues related to LaTeX ([#598](https://github.com/objectionary/eo-phi-normalizer/pull/598))
- Support syntax for constant objects (strings, ints, floats) ([#602](https://github.com/objectionary/eo-phi-normalizer/pull/602))

### Changes and fixes

- Set the correct locale automatically ([#590](https://github.com/objectionary/eo-phi-normalizer/pull/590), [#609](https://github.com/objectionary/eo-phi-normalizer/pull/609), [#611](https://github.com/objectionary/eo-phi-normalizer/pull/611))

### Documentation and maintenance

- Fix site generation ([#612](https://github.com/objectionary/eo-phi-normalizer/pull/612))
- Add examples to `rewrite` docs ([#608](https://github.com/objectionary/eo-phi-normalizer/pull/608))
- chore(deps): update dependency prettier to v3.4.2 ([#597](https://github.com/objectionary/eo-phi-normalizer/pull/597))

## v2.2.2 - 2024-11-29

### New

- Add `--single-line` option to the `rewrite` command ([#588](https://github.com/objectionary/eo-phi-normalizer/pull/588))

### Changes and fixes

- Fix pretty printing ([#577](https://github.com/objectionary/eo-phi-normalizer/pull/577), [#586](https://github.com/objectionary/eo-phi-normalizer/pull/586), [#589](https://github.com/objectionary/eo-phi-normalizer/pull/589))
- Allow optional `when` and `tests` keys in rule files ([#578](https://github.com/objectionary/eo-phi-normalizer/pull/578))
- Improve TeX output ([#579](https://github.com/objectionary/eo-phi-normalizer/pull/579))
- Fix typo in rule `R_rho` and add more tests ([#587](https://github.com/objectionary/eo-phi-normalizer/pull/587))

### Documentation and maintenance

- Update haskell-actions/run-fourmolu action to v11 ([#516](https://github.com/objectionary/eo-phi-normalizer/pull/516))
- Update dependency prettier to v3.4.1 ([#552](https://github.com/objectionary/eo-phi-normalizer/pull/552))
- Update dependency pre-commit to v4 - autoclosed ([#503](https://github.com/objectionary/eo-phi-normalizer/pull/503))
- Update dependency eolang to ^0.24.0 ([#520](https://github.com/objectionary/eo-phi-normalizer/pull/520))
- Add `CONTRIBUTING.md` ([#576](https://github.com/objectionary/eo-phi-normalizer/pull/576))

## v2.2.1 - 2024-11-27

### Changes and fixes

- Fix rules file not included into the released Haskell package ([#568](https://github.com/objectionary/eo-phi-normalizer/pull/568))

## v2.2.0 - 2024-11-26

### Breaking changes

- Use rules from `new.yaml` by default ([#555](https://github.com/objectionary/eo-phi-normalizer/pull/555))

### Changes and fixes

- Fix pretty-printing ([#550](https://github.com/objectionary/eo-phi-normalizer/pull/550))

### Documentation and maintenance

- Fix menu on the site ([#551](https://github.com/objectionary/eo-phi-normalizer/pull/551))

## v2.1.0 - 2024-11-22

### Breaking changes

- Rename `transform` to `rewrite` in the `eo-phi-normalizer` CLI ([#369](https://github.com/objectionary/eo-phi-normalizer/pull/369))

### New

- Add new rules ([#523](https://github.com/objectionary/eo-phi-normalizer/pull/523))
- Add new rules tests ([#540](https://github.com/objectionary/eo-phi-normalizer/pull/540))

### Documentation and maintenance

- Annotate files with copyright ([#536](https://github.com/objectionary/eo-phi-normalizer/pull/536))
- Move docs for user-defined rules to a separate page ([#524](https://github.com/objectionary/eo-phi-normalizer/pull/524))
- Document the release procedure ([#532](https://github.com/objectionary/eo-phi-normalizer/pull/532))

## v2.0.0 - 2024-11-18

### Breaking changes

- Rename `normalizer` to `eo-phi-normalizer` everywhere ([#494](https://github.com/objectionary/eo-phi-normalizer/pull/494))

### New

- Add `eo-phi-normalizer version` command ([#493](https://github.com/objectionary/eo-phi-normalizer/pull/493))
- Add `eo-phi-normalizer test` command ([#498](https://github.com/objectionary/eo-phi-normalizer/pull/498))

### Changes and fixes

- Fix pretty-printer for applications and program ([#529](https://github.com/objectionary/eo-phi-normalizer/pull/529))

### Documentation and maintenance

- Make site Markdown generation reproducible ([#513](https://github.com/objectionary/eo-phi-normalizer/pull/513), [#514](https://github.com/objectionary/eo-phi-normalizer/pull/514))
- Describe how to install a specific `normalizer` version ([#525](https://github.com/objectionary/eo-phi-normalizer/pull/525))
- Speed up CI ([#533](https://github.com/objectionary/eo-phi-normalizer/pull/533))
- Switch the EO version from `0.38.4` to `0.41.2` in CI ([#528](https://github.com/objectionary/eo-phi-normalizer/pull/528))
- Switch to `macos-latest` in CI ([#539](https://github.com/objectionary/eo-phi-normalizer/pull/539))

## v1.1.0 - 2024-10-03

### New

- Add unit tests ([#461](https://github.com/objectionary/eo-phi-normalizer/pull/461))
- Add `R_DOT_ρ` ([#468](https://github.com/objectionary/eo-phi-normalizer/pull/468))
- Support printing rules in LaTeX ([#474](https://github.com/objectionary/eo-phi-normalizer/pull/474))
- Automatically create an output directory ([#499](https://github.com/objectionary/eo-phi-normalizer/pull/499))

### Changes and fixes

- Update the `COPY` rule as in phi-paper ([#445](https://github.com/objectionary/eo-phi-normalizer/pull/445))
- Remove special case for delta binding since VTX is removed ([#356](https://github.com/objectionary/eo-phi-normalizer/pull/356))
- Replace `normalizer report` command with the `normalizer pipeline report` command ([#456](https://github.com/objectionary/eo-phi-normalizer/pull/456))
- Fix sorting in numeric columns ([#460](https://github.com/objectionary/eo-phi-normalizer/pull/460))
- Run mdbook in the pipeline job ([#463](https://github.com/objectionary/eo-phi-normalizer/pull/463))

### Documentation and maintenance

- Document all dependencies in a single doc in `eo/phi/normalizer/data` for each eo version ([#446](https://github.com/objectionary/eo-phi-normalizer/pull/446)).
- Create a metrics page on the site ([#453](https://github.com/objectionary/eo-phi-normalizer/pull/453))
- Chore(deps): update dependency pre-commit to v3.8.0 ([#455](https://github.com/objectionary/eo-phi-normalizer/pull/455))
- Provide instructions for the customer to check that we've completed the contract requirements ([#458](https://github.com/objectionary/eo-phi-normalizer/pull/458))
- Bump stack action version ([#459](https://github.com/objectionary/eo-phi-normalizer/pull/459))
- Update proposal process ([#464](https://github.com/objectionary/eo-phi-normalizer/pull/464))
- Document creating custom atoms on the site ([#467](https://github.com/objectionary/eo-phi-normalizer/pull/467))

## v1.0.0 - 2024-07-19

### New

- Align reductions from application to `⊥` with the paper ([#418](https://github.com/objectionary/eo-phi-normalizer/pull/418), [#435](https://github.com/objectionary/eo-phi-normalizer/pull/435))
- Support enabling/disabling particular atoms ([#426](https://github.com/objectionary/eo-phi-normalizer/pull/426))
- Support custom rules ([#432](https://github.com/objectionary/eo-phi-normalizer/pull/432))
  - Test ruleset `streams.yaml` with the example rule equipped with a passing test, matching <https://github.com/objectionary/eo-phi-normalizer/issues/423#issue-2366637410>
  - Allow rules to go inside abstract objects (and add explicit `apply_in_abstract_subformations` option to maintain correctness for `yegor.yaml` rules)
  - Support tail patterns (restricted one-hole contexts)
    - Matching against objects inside dispatch
    - Matching against objects inside application (left only)
  - Properly generate fresh names
  - Allow explicit `forall` to keep track of all metavariables for extra safety (force in the future, see [#441](https://github.com/objectionary/eo-phi-normalizer/issues/441)).
  - Distinguish types of metavariables for extra safety.

### Changes and fixes

- Update decoration rule ([#420](https://github.com/objectionary/eo-phi-normalizer/pull/420))
- Generalize `Φ-dispatch` to rewrite `Φ` instead of `Φ.a` ([#419](https://github.com/objectionary/eo-phi-normalizer/pull/419))

### Documentation and maintenance

- Update EO to 0.38.4 ([#426](https://github.com/objectionary/eo-phi-normalizer/pull/426))

## v0.4.1 — 2024-06-12

Changes and fixes:

- Undo injection of top-level Package lambda ([#392](https://github.com/objectionary/eo-phi-normalizer/pull/392))
- Fix dataization ([#395](https://github.com/objectionary/eo-phi-normalizer/pull/395))

  - Fix dataization inside $\varphi$ when `--minimize-stuck-terms` is enabled (closes [#393](https://github.com/objectionary/eo-phi-normalizer/pull/393))
  - Fix evaluation of atoms stuck on other atoms (fixes `while-dataizes-only-first-cycle` in `while-tests.phi`)
  - Improve pretty-printing (closes [#292](https://github.com/objectionary/eo-phi-normalizer/pull/292))

- Fix pipeline script to run tests on normalized EO ([#396](https://github.com/objectionary/eo-phi-normalizer/pull/396))

- Changes to normalizer ([#396](https://github.com/objectionary/eo-phi-normalizer/pull/396))

  - Add `--wrap-raw-bytes` to automatically convert raw bytes (and terminations) in the output. This is a temporary fix, pending the change mentioned in <https://github.com/objectionary/eo/issues/3213#issuecomment-2150032168>
  - Fix builtin normalizer to produce termination in some situations
  - Fix dataization inside application/dispatch
  - Fix encoding for strings to follow UTF-8 (compatibility with EO)
  - Fix bool representation to require one byte (compatibility with EO)
  - Fix integer division to truncate toward zero (compatibility with EO)
  - Improve pretty-printer (use indentation)
  - Update some examples/docs on the site
  - Support up to 3 positional arguments in the builtin normalizer

- Fix directory used in CI for:
  - Job summary ([#402](https://github.com/objectionary/eo-phi-normalizer/pull/402))
  - Report ([#412](https://github.com/objectionary/eo-phi-normalizer/pull/412))

Documentation and maintenance:

- Bring rules up to date ([#401](https://github.com/objectionary/eo-phi-normalizer/pull/401))
- Update dependency prettier to v3.3.2 ([#385](https://github.com/objectionary/eo-phi-normalizer/pull/385))

## v0.4.0 — 2024-06-03

This version supports fast dataization with built-in rules and improves metrics with both built-in and user-defined rules (via YAML).

New:

- Add built-in rules
- Add more built-in dataization functions ([#291](https://github.com/objectionary/eo-phi-normalizer/pull/291))
- Support LaTeX format in output ([#308](https://github.com/objectionary/eo-phi-normalizer/pull/308))
- Speed up pipeline by caching EO compilation results ([#340](https://github.com/objectionary/eo-phi-normalizer/pull/340))
- Write generated PHI files as eo-phi-normalizer data files ([#286](https://github.com/objectionary/eo-phi-normalizer/pull/286))
- Update and commit docs in CI ([#286](https://github.com/objectionary/eo-phi-normalizer/pull/286))

Changes and fixes:

- Switch to EO 0.38.0 ([#335](https://github.com/objectionary/eo-phi-normalizer/pull/335))
- Remove VTX and Sigma ([#335](https://github.com/objectionary/eo-phi-normalizer/pull/335))
- Fix normalization and dataization rules w.r.t. xi and rho (see [#297](https://github.com/objectionary/eo-phi-normalizer/pull/297))
- Fix confluence tests ([#319](https://github.com/objectionary/eo-phi-normalizer/pull/319))
- Fix pipeline tests ([#338](https://github.com/objectionary/eo-phi-normalizer/pull/338))
- Integrate `transform-eo-tests` into normalizer ([#365](https://github.com/objectionary/eo-phi-normalizer/pull/365))
- Switch to GHC 9.6.4 ([#263](https://github.com/objectionary/eo-phi-normalizer/pull/263))

Documentation and maintenance:

- Add the `Quick Start` page ([#317](https://github.com/objectionary/eo-phi-normalizer/pull/317))
- Add the `Pipeline` page ([#261](https://github.com/objectionary/eo-phi-normalizer/pull/261))

## v0.3.1 — 2024-04-12

This version supports proper dataization of test programs with dependencies.

New:

- Add `--dependency-file FILE` option to enable dependencies (e.g. `bool.phi`, `bytes.phi`) (see [#265](https://github.com/objectionary/eo-phi-normalizer/pull/265))

Changes and fixes:

- Fix handling of `Package` atom (also improves related code, see [#239](https://github.com/objectionary/eo-phi-normalizer/pull/239) and [#265](https://github.com/objectionary/eo-phi-normalizer/pull/265))
- Update the dataless objects metric (see [#234](https://github.com/objectionary/eo-phi-normalizer/pull/234))
- Improve reports (see [#225](https://github.com/objectionary/eo-phi-normalizer/pull/225))

Documentation and maintenance:

- Establish proposals format and process (see [#248](https://github.com/objectionary/eo-phi-normalizer/pull/248))
- Improve the docs job, improve the site, fix minor errors (see [#228](https://github.com/objectionary/eo-phi-normalizer/pull/228))
- Fix `hlint` warnings (see [#241](https://github.com/objectionary/eo-phi-normalizer/pull/241))
- CI: run doctests (see [#230](https://github.com/objectionary/eo-phi-normalizer/pull/230))
- CI: switch back to the official `stack-action` (see [#246](https://github.com/objectionary/eo-phi-normalizer/pull/246))
- CI: publish extra files in artifacts (in particular, dependencies like `bool.phi`, see [#245](https://github.com/objectionary/eo-phi-normalizer/pull/245))

## v0.3.0 — 2024-03-26

In this version the main executable has been renamed to `normalizer`
and several commands have been implemented for normalization, dataization,
and reporting.

New:

- Command Line Interface:
  - Add commands:
    - `normalizer transform` to perform normalization without executing any atoms (was default behavior before)
    - `normalizer metrics` to compute metrics of a given φ-expression (see [#153](https://github.com/objectionary/eo-phi-normalizer/pull/153))
    - `normalizer dataize` to run partial evaluation of φ-expressions with atoms (see [#187](https://github.com/objectionary/eo-phi-normalizer/pull/187))
    - `normalizer report` to generate report based on the results of testing against the EO compiler (see [#213](https://github.com/objectionary/eo-phi-normalizer/pull/213))
  - Add `--single` flag (see [#131](https://github.com/objectionary/eo-phi-normalizer/pull/131))
  - Add `--json` flag for machine-readable output format (see [#143](https://github.com/objectionary/eo-phi-normalizer/pull/143))
  - Add `--max-depth=N` and `--max-term-size=N` options to control limits for the normalizer (see [#173](https://github.com/objectionary/eo-phi-normalizer/pull/173))
  - Improve `--chain` option to provide better elaboration on the applied normalization rules and the dataization process (see [#195](https://github.com/objectionary/eo-phi-normalizer/pull/195))
- Update rule set for φ-calculus (see [#152](https://github.com/objectionary/eo-phi-normalizer/pull/152) and some changes in [#136](https://github.com/objectionary/eo-phi-normalizer/pull/136) and [#166](https://github.com/objectionary/eo-phi-normalizer/pull/166))
- Add property-based and regression tests for confluence (see [#136](https://github.com/objectionary/eo-phi-normalizer/pull/136) and [#166](https://github.com/objectionary/eo-phi-normalizer/pull/166))
- Tests and metrics against the EO compiler and standard EO test suite (see [#98](https://github.com/objectionary/eo-phi-normalizer/pull/98), [#191](https://github.com/objectionary/eo-phi-normalizer/pull/191))

Fixes:

- Count metrics, including dataless objects, correctly (see [#142](https://github.com/objectionary/eo-phi-normalizer/pull/142), [#193](https://github.com/objectionary/eo-phi-normalizer/pull/193), [#211](https://github.com/objectionary/eo-phi-normalizer/pull/211))
- Fix metavariables in context patterns (see [#174](https://github.com/objectionary/eo-phi-normalizer/pull/174))
- Support empty Δ-bindings (see [#184](https://github.com/objectionary/eo-phi-normalizer/pull/184))

Documentation has been improved (see [#134](https://github.com/objectionary/eo-phi-normalizer/pull/134), [#221](https://github.com/objectionary/eo-phi-normalizer/pull/221)).

Maintenance:

- Run CI on all pull requests (see [#156](https://github.com/objectionary/eo-phi-normalizer/pull/156))
- Downgrade Stackage snapshot (see [#146](https://github.com/objectionary/eo-phi-normalizer/pull/146))
- Add HLint to CI (see [#157](https://github.com/objectionary/eo-phi-normalizer/pull/157))

## v0.2.0 — 2024-02-16

- Complete implementation of Yegor's rules (see [#109](https://github.com/objectionary/eo-phi-normalizer/pull/109), [#112](https://github.com/objectionary/eo-phi-normalizer/pull/112))
  - Support global counter in user-defined rules (see [#105](https://github.com/objectionary/eo-phi-normalizer/pull/105))
  - Context matching (global object and this object, see [#99](https://github.com/objectionary/eo-phi-normalizer/pull/99))
- Fix grammar for $\varphi$-calculus (see [#97](https://github.com/objectionary/eo-phi-normalizer/pull/97) and [#127](https://github.com/objectionary/eo-phi-normalizer/pull/127))
- Improve documentation:
  - Set up wesbite for documentation (see [#104](https://github.com/objectionary/eo-phi-normalizer/pull/104), [#124](https://github.com/objectionary/eo-phi-normalizer/pull/124), and [#128](https://github.com/objectionary/eo-phi-normalizer/pull/128))
  - Update CLI documentation (see [#113](https://github.com/objectionary/eo-phi-normalizer/pull/113))
- Improve command line interface:
  - Support `--output`/`-o` command line option (see [#92](https://github.com/objectionary/eo-phi-normalizer/pull/92))
  - Remove logs from default output (see [#106](https://github.com/objectionary/eo-phi-normalizer/pull/106))
- Allow collection of metrics for $\varphi$-terms (see [#121](https://github.com/objectionary/eo-phi-normalizer/pull/121))

## v0.1.0 - 2024-02-02

First version of the normalizer.
