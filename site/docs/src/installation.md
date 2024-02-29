# Installation

Install the `normalizer` executable globally via [stack](https://docs.haskellstack.org/en/stable).
Then, the `normalizer` executable will be available on `PATH`.

## Install from the repository

```sh
git clone https://github.com/objectionary/normalizer --recurse-submodules
cd normalizer
export LANG=C.UTF-8
stack install normalizer
```

## Install from Hackage

```sh
stack update
export LANG=C.UTF-8
stack install --resolver lts-22.11 eo-phi-normalizer
```

## Uninstall

Learn where `stack` installs programs.

```sh
stack path --programs
```

Learn how to uninstall a program.

```sh
stack uninstall
```
