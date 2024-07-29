# Installation

## Environment requirements

- OS: `Ubuntu 22.04` or `macOS 12`.
- `stack` is installable.
- `normalizer` is installable.

## Install `stack`

{{#include ./common/install-stack.md}}

## Install `normalizer`

Install the `normalizer` executable globally via [stack](https://docs.haskellstack.org/en/stable).
Then, the `normalizer` executable will be available on `PATH`.

### Install `normalizer` from the repository

{{#include ./common/enter-repository.md}}

{{#include ./common/install.md}}

### Install `normalizer` from Hackage

```sh
stack update
export LC_ALL=C.UTF-8
stack install --resolver lts-22.16 eo-phi-normalizer
```

## Update `normalizer`

Run `stack install ...` as in one of the methods above.

## Uninstall `normalizer`

Learn where `stack` installs programs.

```sh
stack path --programs
```

Learn how to uninstall a program.

```sh
stack uninstall
```
