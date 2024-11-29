# `eo-phi-normalizer rewrite`

## Environment

### Repository

The commands in the following sections access files that are available in the project repository.

{{#include ../common/enter-repository.md}}

### Sample program

{{#include ../common/celsius.md}}

## CLI

### `--help`

```$ as console
eo-phi-normalizer rewrite --help
```

```console
Usage: eo-phi-normalizer rewrite [-r|--rules FILE] [-c|--chain] [-j|--json]
                                 [--tex] [-o|--output-file FILE] [-s|--single]
                                 [-l|--single-line] [--max-depth INT]
                                 [--max-growth-factor INT] [FILE]
                                 [-d|--dependency-file FILE]

  Rewrite a PHI program.

Available options:
  -r,--rules FILE          FILE with user-defined rules. If unspecified, builtin
                           set of rules is used.
  -c,--chain               Output rewriting steps.
  -j,--json                Output JSON.
  --tex                    Output LaTeX.
  -o,--output-file FILE    Output to FILE. When this option is not specified,
                           output to stdout.
  -s,--single              Output a single expression.
  -l,--single-line         Output a single expression on a single line. Has
                           effect only if the --single is enabled.
  --max-depth INT          Maximum depth of rules application. Defaults to 10.
  --max-growth-factor INT  The factor by which to allow the input term to grow
                           before stopping. Defaults to 10.
  FILE                     FILE to read input from. When no FILE is specified,
                           read from stdin.
  -d,--dependency-file FILE
                           FILE to read dependencies from (zero or more
                           dependency files allowed).
  -h,--help                Show this help text
```

### `--rules FILE`

Normalize a 𝜑-expression from `celsius.phi` using the rules from a given file (e.g. [yegor.yaml](#yegoryaml)).

The output may contain multiple numbered results that correspond to different possible rule application sequences
(even if the final result is the same).

```$ as console
eo-phi-normalizer rewrite --rules ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml celsius.phi
```

```console
Rule set based on Yegor's draft
Input:
{
  ⟦
    c ↦ Φ.org.eolang.float (
      as-bytes ↦ Φ.org.eolang.bytes (
        Δ ⤍ 40-39-00-00-00-00-00-00
      )
    ),
    result ↦ ξ.c.times (
      x ↦ ⟦
        Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD
      ⟧
    )
    .plus (
      x ↦ ⟦
        Δ ⤍ 40-40-00-00-00-00-00-00
      ⟧
    ),
    λ ⤍ Package
  ⟧
}
====================================================
Result 1 out of 1:
{
  ⟦
    c ↦ Φ.org.eolang.float (
      as-bytes ↦ Φ.org.eolang.bytes (
        Δ ⤍ 40-39-00-00-00-00-00-00
      )
    ),
    result ↦ ξ.c.times (
      x ↦ ⟦
        Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD
      ⟧
    )
    .plus (
      x ↦ ⟦
        Δ ⤍ 40-40-00-00-00-00-00-00
      ⟧
    ),
    λ ⤍ Package
  ⟧
}
----------------------------------------------------
```

### `--chain`

Use `--chain` to see numbered normalization steps for each normalization result.

```$ as console
eo-phi-normalizer rewrite --chain --rules ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml celsius.phi
```

```console
Rule set based on Yegor's draft
Input:
{
  ⟦
    c ↦ Φ.org.eolang.float (
      as-bytes ↦ Φ.org.eolang.bytes (
        Δ ⤍ 40-39-00-00-00-00-00-00
      )
    ),
    result ↦ ξ.c.times (
      x ↦ ⟦
        Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD
      ⟧
    )
    .plus (
      x ↦ ⟦
        Δ ⤍ 40-40-00-00-00-00-00-00
      ⟧
    ),
    λ ⤍ Package
  ⟧
}
====================================================
Result 1 out of 1:
[ 1 / 1 ] Normal form: {
  ⟦
    c ↦ Φ.org.eolang.float (
      as-bytes ↦ Φ.org.eolang.bytes (
        Δ ⤍ 40-39-00-00-00-00-00-00
      )
    ),
    result ↦ ξ.c.times (
      x ↦ ⟦
        Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD
      ⟧
    )
    .plus (
      x ↦ ⟦
        Δ ⤍ 40-40-00-00-00-00-00-00
      ⟧
    ),
    λ ⤍ Package
  ⟧
}
----------------------------------------------------
```

### `--json`

```$ as json
eo-phi-normalizer rewrite --json --chain --rules ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml celsius.phi
```

```json
{
  "input": "{\n  ⟦\n    c ↦ Φ.org.eolang.float (\n      as-bytes ↦ Φ.org.eolang.bytes (\n        Δ ⤍ 40-39-00-00-00-00-00-00\n      )\n    ),\n    result ↦ ξ.c.times (\n      x ↦ ⟦\n        Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD\n      ⟧\n    )\n    .plus (\n      x ↦ ⟦\n        Δ ⤍ 40-40-00-00-00-00-00-00\n      ⟧\n    ),\n    λ ⤍ Package\n  ⟧\n}",
  "output": [
    [
      [
        "Normal form",
        "{\n  ⟦\n    c ↦ Φ.org.eolang.float (\n      as-bytes ↦ Φ.org.eolang.bytes (\n        Δ ⤍ 40-39-00-00-00-00-00-00\n      )\n    ),\n    result ↦ ξ.c.times (\n      x ↦ ⟦\n        Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD\n      ⟧\n    )\n    .plus (\n      x ↦ ⟦\n        Δ ⤍ 40-40-00-00-00-00-00-00\n      ⟧\n    ),\n    λ ⤍ Package\n  ⟧\n}"
      ]
    ]
  ]
}
```

### `--single`

```$ as console
eo-phi-normalizer rewrite --single --rules ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml celsius.phi
```

```console
{
  ⟦
    c ↦ Φ.org.eolang.float (
      as-bytes ↦ Φ.org.eolang.bytes (
        Δ ⤍ 40-39-00-00-00-00-00-00
      )
    ),
    result ↦ ξ.c.times (
      x ↦ ⟦
        Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD
      ⟧
    )
    .plus (
      x ↦ ⟦
        Δ ⤍ 40-40-00-00-00-00-00-00
      ⟧
    ),
    λ ⤍ Package
  ⟧
}
```

### `--single --single-line`

```$ as console
eo-phi-normalizer rewrite --single --single-line --rules ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml celsius.phi
```

```console
{ ⟦ c ↦ Φ.org.eolang.float ( as-bytes ↦ Φ.org.eolang.bytes ( Δ ⤍ 40-39-00-00-00-00-00-00 ) ), result ↦ ξ.c.times ( x ↦ ⟦ Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD ⟧ ) .plus ( x ↦ ⟦ Δ ⤍ 40-40-00-00-00-00-00-00 ⟧ ), λ ⤍ Package ⟧ }
```

### `--single` `--json`

```$ as console
eo-phi-normalizer rewrite --single --json --rules ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml celsius.phi
```

```console
"{\n  ⟦\n    c ↦ Φ.org.eolang.float (\n      as-bytes ↦ Φ.org.eolang.bytes (\n        Δ ⤍ 40-39-00-00-00-00-00-00\n      )\n    ),\n    result ↦ ξ.c.times (\n      x ↦ ⟦\n        Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD\n      ⟧\n    )\n    .plus (\n      x ↦ ⟦\n        Δ ⤍ 40-40-00-00-00-00-00-00\n      ⟧\n    ),\n    λ ⤍ Package\n  ⟧\n}"
```

### `--output-file FILE`

Redirects the output to file of the given path instead of `stdout`.

### `--dependency-file FILE`

Injects package dependencies from a given file into the context when transforming the input.
Can be used multiple times to inject multiple dependencies.

### `FILE` not specified (read from stdin)

```$ as console
cat celsius.phi | eo-phi-normalizer rewrite --single --json --rules ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml
```

```console
"{\n  ⟦\n    c ↦ Φ.org.eolang.float (\n      as-bytes ↦ Φ.org.eolang.bytes (\n        Δ ⤍ 40-39-00-00-00-00-00-00\n      )\n    ),\n    result ↦ ξ.c.times (\n      x ↦ ⟦\n        Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD\n      ⟧\n    )\n    .plus (\n      x ↦ ⟦\n        Δ ⤍ 40-40-00-00-00-00-00-00\n      ⟧\n    ),\n    λ ⤍ Package\n  ⟧\n}"
```
