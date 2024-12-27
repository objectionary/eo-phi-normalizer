# `eo-phi-normalizer test`

## CLI

### `--help`

```$ as console
eo-phi-normalizer test --help
```

```console
Usage: eo-phi-normalizer test [-r|--rules FILE]

  Run unit tests in given files with user-defined rules.

Available options:
  -r,--rules FILE          FILE with user-defined rules.
  -h,--help                Show this help text
```

### Run tests in a file

```console
eo-phi-normalizer test --rules eo-phi-normalizer/test/eo/phi/rules/new.yaml
```

<!--
The command output is modified to make documentation generation reproducible.

The full command is hidden to not show unnecessary details in the documentation.

`$ eo-phi-normalizer test --rules eo-phi-normalizer/test/eo/phi/rules/new.yaml | sed -e 's/\(Finished in\) \([0-9]\.[0-9]\+\)/\1 0.0062/'` as console -->

```console
User-defined rules unit tests
  Rule set following Nov 2024 revision
    DOT
      Contextualization changes ξ [✔]
      Contextualization applies recursively [✔]
      Phi Paper - Example E2 [✔]
      Phi Paper - Example E3 - first R_dot [✔]
      Phi Paper - Example E3 - second R_dot [✔]
      Phi Paper - Example E4 - first R_dot [✔]
      Phi Paper - Example E4 - second R_dot [✔]
      Phi Paper - Example E5 - first R_dot [✔]
      Phi Paper - Example E5 - second R_dot [✔]
    COPY
      Should match [✔]
      Should match [✔]
      Phi Paper - Example E1 [✔]
      Phi Paper - Example E4 - dispatch on y [✔]
      Phi Paper - Example E4 - remove dispatch on y [✔]
    RHO
      Phi Paper - Example E5 - first R_rho [✔]
    phi
      Phi Paper - Example E5 - R_phi [✔]
    STAY
      Phi Paper - Example E3 - first R_stay [✔]
      Should match [✔]
    OVER
      Language.EO.Test.YamlSpec[46:13] [✔]
    STOP
      Accessing nonexistent attribute [✔]
    NULL
      Phi Paper Example E2 second dispatch [✔]
    DUP
      Should match [✔]
      Should not match [✔]
      Should match in subformation [✔]
      Should work with empty formation [✔]
      Phi Paper - Example E5 - first R_rho [✔]
    MISS
      Language.EO.Test.YamlSpec[46:13] [✔]
      Should not match if attr is present [✔]
      Should not match for rho [✔]
      Should apply in subformations [✔]
    DD
      Dispatch on bottom is bottom [✔]
      Dispatch on anything else is not touched [✔]
    DC
      Should apply in subformations [✔]
      Phi Paper Example E2 last application [✔]

Finished in 0.0062 seconds
34 examples, 0 failures
```
