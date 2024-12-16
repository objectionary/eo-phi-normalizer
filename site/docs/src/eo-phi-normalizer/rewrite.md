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
Usage: eo-phi-normalizer rewrite [-c|--chain] [-d|--dependency-file FILE]
                                 [-j|--json] [--max-depth INT]
                                 [--max-growth-factor INT]
                                 [-o|--output-file FILE] [-r|--rules FILE]
                                 [-s|--single] [-l|--single-line] [--tex] [FILE]

  Rewrite a PHI program.

Available options:
  -c,--chain               Output rewriting steps.
  -d,--dependency-file FILE
                           FILE to read dependencies from (zero or more
                           dependency files allowed).
  -j,--json                Output JSON.
  --max-depth INT          Maximum depth of rules application. Defaults to 10.
  --max-growth-factor INT  The factor by which to allow the input term to grow
                           before stopping. Defaults to 10.
  -o,--output-file FILE    Output to FILE. When this option is not specified,
                           output to stdout.
  -r,--rules FILE          FILE with user-defined rules. If unspecified, builtin
                           set of rules is used.
  -s,--single              Output a single expression.
  -l,--single-line         Output a single expression on a single line. Has
                           effect only if the --single is enabled.
  --tex                    Output LaTeX.
  FILE                     FILE to read input from. When no FILE is specified,
                           read from stdin.
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
      as-bytes ↦ 25.0
    ),
    result ↦ ξ.c.times (
      x ↦ 1.8
    )
    .plus (
      x ↦ 32.0
    ),
    λ ⤍ Package
  ⟧
}
====================================================
Result 1 out of 1:
{
  ⟦
    c ↦ Φ.org.eolang.float (
      as-bytes ↦ 25.0
    ),
    result ↦ ξ.c.times (
      x ↦ 1.8
    )
    .plus (
      x ↦ 32.0
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
      as-bytes ↦ 25.0
    ),
    result ↦ ξ.c.times (
      x ↦ 1.8
    )
    .plus (
      x ↦ 32.0
    ),
    λ ⤍ Package
  ⟧
}
====================================================
Result 1 out of 1:
[ 1 / 1 ] NF: {
  ⟦
    c ↦ Φ.org.eolang.float (
      as-bytes ↦ 25.0
    ),
    result ↦ ξ.c.times (
      x ↦ 1.8
    )
    .plus (
      x ↦ 32.0
    ),
    λ ⤍ Package
  ⟧
}
----------------------------------------------------
```

### `--chain` `--tex`

```$ as tex
printf "{⟦ m ↦ ⟦ x ↦ ⟦ t ↦ ⟦ Δ ⤍ 42- ⟧ ⟧.t ⟧.x ⟧}" > bar.phi

eo-phi-normalizer rewrite --chain --tex bar.phi
```

```tex
% Rule set following Nov 2024 revision

\documentclass{article}
\usepackage{eolang}
\begin{document}


This is the 1st possible chain of normalizing rewritings:

\begin{phiquation*}
[[ m -> [[ x -> [[ t -> [[ D> 42- ]] ]].t ]].x ]] \trans_{\rulename{DOT}}
  \trans [[ m -> [[ x -> [[ D> 42- ]]( ^ -> [[ t -> [[ D> 42- ]] ]] ) ]].x ]] \trans_{\rulename{DOT}}
  \trans [[ m -> [[ D> 42- ]]( ^ -> [[ t -> [[ D> 42- ]] ]] )( ^ -> [[ x -> [[ D> 42- ]]( ^ -> [[ t -> [[ D> 42- ]] ]] ) ]] ) ]] \trans_{\rulename{RHO}}
  \trans [[ m -> [[ D> 42-, ^ -> [[ t -> [[ D> 42- ]] ]] ]](  )( ^ -> [[ x -> [[ D> 42- ]]( ^ -> [[ t -> [[ D> 42- ]] ]] ) ]] ) ]] \trans_{\rulename{DUP}}
  \trans [[ m -> [[ D> 42-, ^ -> [[ t -> [[ D> 42- ]] ]] ]]( ^ -> [[ x -> [[ D> 42- ]]( ^ -> [[ t -> [[ D> 42- ]] ]] ) ]] ) ]] \trans_{\rulename{STAY}}
  \trans [[ m -> [[ ^ -> [[ t -> [[ D> 42- ]] ]], D> 42- ]](  ) ]] \trans_{\rulename{DUP}}
  \trans [[ m -> [[ ^ -> [[ t -> [[ D> 42- ]] ]], D> 42- ]] ]] \trans_{\rulename{NF}}
  \trans [[ m -> [[ ^ -> [[ t -> [[ D> 42- ]] ]], D> 42- ]] ]].
\end{phiquation*}

This is the 2nd possible chain of normalizing rewritings:

\begin{phiquation*}
[[ m -> [[ x -> [[ t -> [[ D> 42- ]] ]].t ]].x ]] \trans_{\rulename{DOT}}
  \trans [[ m -> [[ x -> [[ D> 42- ]]( ^ -> [[ t -> [[ D> 42- ]] ]] ) ]].x ]] \trans_{\rulename{RHO}}
  \trans [[ m -> [[ x -> [[ D> 42-, ^ -> [[ t -> [[ D> 42- ]] ]] ]](  ) ]].x ]] \trans_{\rulename{DUP}}
  \trans [[ m -> [[ x -> [[ D> 42-, ^ -> [[ t -> [[ D> 42- ]] ]] ]] ]].x ]] \trans_{\rulename{DOT}}
  \trans [[ m -> [[ D> 42-, ^ -> [[ t -> [[ D> 42- ]] ]] ]]( ^ -> [[ x -> [[ D> 42-, ^ -> [[ t -> [[ D> 42- ]] ]] ]] ]] ) ]] \trans_{\rulename{STAY}}
  \trans [[ m -> [[ ^ -> [[ t -> [[ D> 42- ]] ]], D> 42- ]](  ) ]] \trans_{\rulename{DUP}}
  \trans [[ m -> [[ ^ -> [[ t -> [[ D> 42- ]] ]], D> 42- ]] ]] \trans_{\rulename{NF}}
  \trans [[ m -> [[ ^ -> [[ t -> [[ D> 42- ]] ]], D> 42- ]] ]].
\end{phiquation*}

\end{document}
```

### `--json`

```$ as json
eo-phi-normalizer rewrite --json --chain --rules ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml celsius.phi
```

```json
{
  "input": "{\n  ⟦\n    c ↦ Φ.org.eolang.float (\n      as-bytes ↦ 25.0\n    ),\n    result ↦ ξ.c.times (\n      x ↦ 1.8\n    )\n    .plus (\n      x ↦ 32.0\n    ),\n    λ ⤍ Package\n  ⟧\n}",
  "output": [
    [
      [
        "NF",
        "{\n  ⟦\n    c ↦ Φ.org.eolang.float (\n      as-bytes ↦ 25.0\n    ),\n    result ↦ ξ.c.times (\n      x ↦ 1.8\n    )\n    .plus (\n      x ↦ 32.0\n    ),\n    λ ⤍ Package\n  ⟧\n}"
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
      as-bytes ↦ 25.0
    ),
    result ↦ ξ.c.times (
      x ↦ 1.8
    )
    .plus (
      x ↦ 32.0
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
{ ⟦ c ↦ Φ.org.eolang.float ( as-bytes ↦ 25.0 ), result ↦ ξ.c.times ( x ↦ 1.8 ) .plus ( x ↦ 32.0 ), λ ⤍ Package ⟧ }
```

### `--single` `--json`

```$ as console
eo-phi-normalizer rewrite --single --json --rules ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml celsius.phi
```

```console
"{\n  ⟦\n    c ↦ Φ.org.eolang.float (\n      as-bytes ↦ 25.0\n    ),\n    result ↦ ξ.c.times (\n      x ↦ 1.8\n    )\n    .plus (\n      x ↦ 32.0\n    ),\n    λ ⤍ Package\n  ⟧\n}"
```

### `--tex`

```$ as tex
eo-phi-normalizer rewrite --tex bar.phi
```

```tex
% Rule set following Nov 2024 revision

\documentclass{article}
\usepackage{eolang}
\begin{document}

\begin{phiquation*}
[[ m -> [[ ^ -> [[ t -> [[ D> 42- ]] ]], D> 42- ]] ]]
\end{phiquation*}

\end{document}
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
"{\n  ⟦\n    c ↦ Φ.org.eolang.float (\n      as-bytes ↦ 25.0\n    ),\n    result ↦ ξ.c.times (\n      x ↦ 1.8\n    )\n    .plus (\n      x ↦ 32.0\n    ),\n    λ ⤍ Package\n  ⟧\n}"
```
