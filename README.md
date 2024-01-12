# Normalizer for 𝜑-calculus

Command line normalizer of 𝜑-calculus expressions (as produced by the [EO compiler](https://github.com/objectionary/eo)).

## About

This project aims to apply term rewriting techniques to "simplify" an input 𝜑-expression
and prepare it for further optimization passes. The simplification procedure is expected
to be a form of partial evaluation and normalization.
Contrary to traditional normalization in λ-calculus, we aim at rewriting rules that would
help reduce certain metrics of expressions. In particular, we are interested in reducing
attribute access (`t.a`) that amounts to _dynamic dispatch_.

## Usage

You can install the normalizer locally with [Stack](https://docs.haskellstack.org/en/stable/README/):

```sh
git clone https://github.com/objectionary/normalizer.git
cd normalizer
stack install
```

This should install `normalize-phi` executable (usually, to `~/.local/bin/` on Linux and macOS).
You can pass a 𝜑-program (e.g. from a file) to it:

```sh
normalize-phi < FILE
```

The output should be a 𝜑-term after normalization.

```sh
echo "{ φ ↦ {}, a ↦ ξ.a }" | normalize-phi
```

```
{ φ ↦ { }, a ↦ { } }
```

## Development

The project is developed with the [Stack tool](https://docs.haskellstack.org/en/stable/README/).

For quick local development and testing it is recommended to use `stack`. Clone this project and run `stack build`:

```sh
git clone https://github.com/objectionary/normalizer.git
cd normalizer
stack build
```

The build provides an executable `normalize-phi` which can be used to normalize input expressions:

```sh
stack exec -- normalize-phi < FILE
```

You can also build and run the (default) executable using `stack run`:

```sh
stack run < FILE
```

To run (all) tests, simply use

```sh
stack test
```

## Contribute

### pre-commit

We use [pre-commit](https://pre-commit.com/) to ensure code quality.

Collaborators **MUST** set them up before commiting any code to our repository.

Otherwise, the triggered CI jobs will fail.

#### Set up pre-commit

1. Install [Python 3](https://www.python.org/downloads/) (e.g., Python 3.10).
1. [Install pre-commit](https://pre-commit.com/#1-install-pre-commit).
    - Alternatively, run `pip3 install`.
1. [Install the git hook scripts](https://pre-commit.com/#3-install-the-git-hook-scripts).
1. Install [fourmolu](https://github.com/fourmolu/fourmolu)

    ```console
    stack install fourmolu
    ```

    - You can remove `fourmolu` later (see [SO post](https://stackoverflow.com/a/38639959))

#### pre-commit configs

See [docs](https://pre-commit.com/#adding-pre-commit-plugins-to-your-project).

Hooks:

- run before a commit - [.pre-commit-config.yaml](.pre-commit-config.yaml)
- run in CI - [.pre-commit-checks.yaml](.pre-commit-checks.yaml)

You can run a specific hook (see [docs](https://pre-commit.com/#pre-commit-run)):

```console
pre-commit run -c .pre-commit-checks.yaml fourmolu-check --all
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
