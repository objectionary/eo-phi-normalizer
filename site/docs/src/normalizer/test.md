# `normalizer test`

## CLI

### `--help`

```$ as console
normalizer test --help
```

```console
Usage: normalizer test [-r|--rules FILE]

  Run unit tests in given files with user-defined rules.

Available options:
  -r,--rules FILE          FILE with user-defined rules.
  -h,--help                Show this help text
```

### Run tests in a file

```console
normalizer test --rules eo-phi-normalizer/test/eo/phi/rules/yegor.yaml
```

<!--
The command output is modified to make documentation generation reproducible.

The full command is hidden to not show unnecessary details in the documentation.

`$ normalizer test --rules eo-phi-normalizer/test/eo/phi/rules/yegor.yaml | sed -e 's/\(Finished in\) \([0-9]\.[0-9]\+\)/\1 0.0062/'` as console -->

```console
User-defined rules unit tests
  Rule set based on Yegor's draft
    xi
      Does not replace ξ inside a subformation [✔]
    DOT
      Should match [✔]
      Shouldn't match [✔]
      Shouldn't match [✔]
      Should apply in subformations [✔]
      Should respect surrounding context [✔]
    DOTrho
      Should match [✔]
    phi
      Attribute does not exist [✔]
      Attribute exists [✔]
      Both attributes do not exist [✔]
    COPY
      Should match [✔]
      Should not match in subformations [✔]
    COPY1
      Should match first void attribute [✔]
    COPY2
      Should match positional arguments [✔]
    EMPTY
      Should match [✔]
      Should not match [✔]
      Should match in subformation [✔]
      Should work with empty formation [✔]
    OVER
      Language.EO.Test.YamlSpec[21:13] [✔]
    STOP
      Accessing nonexistent attribute [✔]
    MISS
      Language.EO.Test.YamlSpec[21:13] [✔]
      Should not match if attr is present [✔]
      Should not match if phi is present [✔]
      Should apply in subformations [✔]
    DD
      Dispatch on bottom is bottom [✔]
      Dispatch on anything else is not touched [✔]
    DC
      Should apply in subformations [✔]

Finished in 0.0061 seconds
27 examples, 0 failures
```
