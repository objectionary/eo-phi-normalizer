# Normalizer for 𝜑-calculus

[![`eo-phi-normalizer` on Hackage](https://img.shields.io/hackage/v/eo-phi-normalizer)](http://hackage.haskell.org/package/eo-phi-normalizer)
[![Haddock](<https://shields.io/badge/Haddock%20(master)-Code%20documentation-informational>)](https://www.objectionary.com/normalizer/haddock/)

Command line normalizer of 𝜑-calculus expressions (as produced by the [EO compiler](https://github.com/objectionary/eo)).

## About

This project aims to apply term rewriting techniques to "simplify" an input 𝜑-expression
and prepare it for further optimization passes. The simplification procedure is expected
to be a form of partial evaluation and normalization.
Contrary to traditional normalization in λ-calculus, we aim at rewriting rules that would
help reduce certain metrics of expressions. In particular, we are interested in reducing
attribute access (`t.a`) that amounts to _dynamic dispatch_.

## Install

You can install the `normalizer-phi` executable globally via [stack](https://docs.haskellstack.org/en/stable).
Then, the `normalize-phi` executable will be available on `PATH`.

### Install from the repository

```sh
git clone https://github.com/objectionary/normalizer
cd normalizer
export LANG=C.UTF-8
stack install normalize-phi
normalize-phi --help
```

### Install from Hackage

```sh
stack update
export LANG=C.UTF-8
stack install --resolver lts-22.11 eo-phi-normalizer-0.1.0
```

### Uninstall

Learn where `stack` installs programs.

```sh
stack path --programs
```

Learn how to uninstall a program.

```sh
stack uninstall
```

## Use

Learn about `normalize-phi` options.

```sh
normalize-phi --help
```

```console
Normalizer

Usage: normalize-phi [-c|--chain] [--rules-yaml STRING] [-o|--output STRING]
                     [STRING]

Available options:
  -h,--help                Show this help text
  -c,--chain               Print out steps of reduction
  --rules-yaml STRING      Path to the Yaml file with custom rules
  -o,--output STRING       Output file path (defaults to stdout)
```

### Sample program

Save a $\varphi$-calculus program to a file.
This program will be used in subsequent commands.

```sh
cat > program.phi <<EOM
{
  a ↦
    ⟦
      b ↦
          ⟦
            c ↦ ∅,
            d ↦ ⟦ φ ↦ ξ.ρ.c ⟧
          ⟧,
      e ↦ ξ.b(c ↦ ⟦⟧).d
    ⟧.e
}
EOM
```

### Prepare environment

The commands in the following sections access files that are available in the project repository.
Clone and enter the repository directory.

```sh
git clone https://github.com/objectionary/normalizer
cd normalizer
```

#### `--ruleset-yaml`

Normalize a 𝜑-expression from `program.phi` using a ruleset (See [Rulesets](#rulesets)).

There can be multiple numbered results that correspond to multiple rule application sequences.

```sh
normalize-phi --rules-yaml ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml program.phi
```

```console
Rule set based on Yegor's draft
Input:
{ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b (c ↦ ⟦ ⟧).d ⟧.e }
====================================================
Result 1 out of 1:
⟦ a ↦ ξ.b (c ↦ ⟦ ⟧).d (ρ ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧ ⟧) ⟧
----------------------------------------------------
```

#### Input

Normalize an expression using a ruleset (See [Rulesets](#rulesets)).
Read the expression from stdin.

```sh
cat program.phi | normalize-phi --rules-yaml ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml
```

```console
Rule set based on Yegor's draft
Input:
{ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b (c ↦ ⟦ ⟧).d ⟧.e }
====================================================
Result 1 out of 1:
⟦ a ↦ ξ.b (c ↦ ⟦ ⟧).d (ρ ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧ ⟧) ⟧
----------------------------------------------------
```

Alternatively, the path to the file containing a Phi expression can be passed as a positional argument:

```sh
normalize-phi --rules-yaml ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml program.phi
```

#### `--chain`

Use `--chain` to see numbered normalization steps for each normalization result.

```sh
cat program.phi | normalize-phi --chain --rules-yaml ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml
```

```console
Rule set based on Yegor's draft
Input:
{ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b (c ↦ ⟦ ⟧).d ⟧.e }
====================================================
Result 1 out of 1:
[ 1 / 2 ]⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b (c ↦ ⟦ ⟧).d ⟧.e ⟧
[ 2 / 2 ]⟦ a ↦ ξ.b (c ↦ ⟦ ⟧).d (ρ ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧ ⟧) ⟧
----------------------------------------------------
```

#### `--single`

Use `--single` to print a single normalized program.

```sh
normalize-phi --single --rules-yaml ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml program.phi
```

```console
⟦ a ↦ ξ.b (c ↦ ⟦ ⟧).d (ρ ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧ ⟧) ⟧
```

#### Output

By default, the normalization output will be printed to `stdout`.
The `--output` CLI option (or its shorthand `-o`) can be used to specify the path to the file to which the output will be written instead.
The file will be created if it didn't originally exist.

This is equivalent to redirecting the output from `stdout` to a file using `>`.

## Rulesets

A ruleset describes a set of user-defined rewriting rules.

Here is a sample ruleset (see the full ruleset in [yegor.yaml](./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml)).

```yaml
title: "Rule set based on Yegor's draft"
rules:
  - name: Rule 6
    description: "Accessing an α-binding"
    pattern: |
      ⟦ !a ↦ !n, !B ⟧.!a
    result: |
      !n(ρ ↦ ⟦ !B ⟧)
    when:
      - nf: ["!n"]
    tests:
      - name: Should match
        input: ⟦ hello ↦ ⟦⟧ ⟧.hello
        output: ⟦⟧(ρ ↦ ⟦⟧)
        matches: true
      - name: Shouldn't match
        input: ⟦ ⟧.hello
        output: ""
        matches: false
```

A ruleset has a number of rules. Each rule describes a `pattern`, `when` to apply that pattern, and a `result`.

The `pattern` has metavariables denoted as `!<Identifier>`.
The `pattern` is matched against an `input` expression. The rules in the `when` list help avoid unwanted matches. For example, `nf: ["!n"]` means that only an expression in a normal form can be matched with `!n`.

When there is a match, the matched parts of the expression are bound to metavariables. Next, these metavariables are used to construct the `result`.

Additionally, there are unit tests for rules. Each unit test provides `input` and `output` expressions. An `output` expression is not reused in other tests, so it is safe to let it be an empty string when no match can happen.

## Develop

We recommend using [stack](https://docs.haskellstack.org/en/stable/README/) for quick local development and testing.

Clone this project and run `stack build`.

```sh
git clone https://github.com/objectionary/normalizer
cd normalizer
stack build
```

### Run

Run the executable via `stack run`.

```sh
stack run normalize-phi -- --help
```

Or, omit the executable name.

```sh
stack run -- --help
```

### Test

Run all tests

```sh
stack test
```

## Contribute

### pre-commit

We use [pre-commit](https://pre-commit.com/) to ensure code quality.

Collaborators **MUST** set them up before commiting any code to our repository.

Otherwise, the triggered CI jobs will fail.

#### Set up pre-commit

##### Single command

```console
pip3 install
pre-commit install
stack install fourmolu
chmod +x scripts/run-fourmolu.sh
```

##### Step by step

1. Install [Python 3](https://www.python.org/downloads/) (e.g., Python 3.10).
1. [Install pre-commit](https://pre-commit.com/#1-install-pre-commit).
   - Alternatively, run `pip3 install`.
1. [Install the git hook scripts](https://pre-commit.com/#3-install-the-git-hook-scripts).
1. Install [fourmolu](https://github.com/fourmolu/fourmolu).

   ```console
   stack install fourmolu
   ```

   - You can remove `fourmolu` later (see [SO post](https://stackoverflow.com/a/38639959))

1. Make a script executable.

   ```console
   chmod +x scripts/run-fourmolu.sh
   ```

#### pre-commit configs

See [docs](https://pre-commit.com/#adding-pre-commit-plugins-to-your-project).

See [.pre-commit-config.yaml](.pre-commit-config.yaml).

You can run a specific hook (see [docs](https://pre-commit.com/#pre-commit-run)):

```console
pre-commit run -c .pre-commit-config.yaml fourmolu-format --all
```

#### pre-commit workflow

- `pre-commit` runs before a commit (at the [pre-commit phase](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks#_committing_workflow_hooks))

  > The pre-commit hook is run first, before you even type in a commit message. It's used to inspect the snapshot that's about to be committed, to see if you've forgotten something, to make sure tests run, or to examine whatever you need to inspect in the code. Exiting non-zero from this hook aborts the commit ...

- `pre-commit` stashes ([link](https://git-scm.com/docs/git-stash)) unstaged ([link](https://git-scm.com/book/en/v2/Getting-Started-What-is-Git%3F#_the_three_states)) files.

  ```console
  [WARNING] Unstaged files detected.
  [INFO] Stashing unstaged files to /home/eyjafjallajokull/.cache/pre-commit/patch1705090051-437857.
  ```

- `pre-commit` runs hooks.
- A hook may exit with an error, e.g.:

  ```md
  Format Haskell (.hs) files...............................................Failed

  - hook id: fourmolu
  - exit code: 102
  - files were modified by this hook
  ```

  - In case of the [fourmolu](https://github.com/fourmolu/fourmolu) formatter,
    it's assumed that formatting a formatted `Haskell` file doesn't modify it.
    However, `pre-commit` runs the `fourmolu` hook and reports that it has modified some files.
    This error won't allow you to commit.

- `pre-commit` unstashes files.

- You should stage all changes so that `pre-commit` does not complain.

  - In case of `fourmolu`, stage the formatted code regions.

- Now, you can commit.
