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

[^1]:
