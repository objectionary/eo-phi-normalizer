# Installation

Install the `normalizer` executable globally via [stack](https://docs.haskellstack.org/en/stable).
Then, the `normalizer` executable will be available on `PATH`.

## Install from the repository

{{ #include ./common/install.md }}

## Install from Hackage

```sh
stack update
export LC_ALL=C.UTF-8
stack install --resolver lts-22.6 eo-phi-normalizer
```

## Update

Run `stack install` as in one of the methods above.
The command will update the installed executable.

## Uninstall

Learn where `stack` installs programs.

```sh
stack path --programs
```

Learn how to uninstall a program.

```sh
stack uninstall
```
