# `eo-phi-normalizer dataize`

## Environment

{{#include ../common/celsius.md}}

## CLI

### `--help`

```$ as console
eo-phi-normalizer dataize --help
```

```console
Usage: eo-phi-normalizer dataize [--as-package] [--chain]
                                 [-d|--dependency-file FILE]
                                 [--disable-atom ATOM_NAME]
                                 [--enable-atom ATOM_NAME]
                                 [--minimize-stuck-terms]
                                 [-o|--output-file FILE] [--recursive]
                                 [-r|--rules FILE] [--tex] [--wrap-raw-bytes]
                                 [FILE]

  Dataize a PHI program.

Available options:
  --as-package             Automatically inject (λ → Package) in the program if
                           necessary, to dataize all fields.
  --chain                  Display all the intermediate steps.
  -d,--dependency-file FILE
                           FILE to read dependencies from (zero or more
                           dependency files allowed).
  --disable-atom ATOM_NAME Name of an atom to disable.
  --enable-atom ATOM_NAME  Name of an atom to enable.
  --minimize-stuck-terms   If a dataized (sub)term is stuck (cannot be fully
                           dataized), use the minimal (by size) intermediate
                           result.
  -o,--output-file FILE    Output to FILE. When this option is not specified,
                           output to stdout.
  --recursive              Apply dataization + normalization recursively.
  -r,--rules FILE          FILE with user-defined rules. If unspecified, builtin
                           set of rules is used.
  --tex                    Output LaTeX.
  --wrap-raw-bytes         Wrap raw bytes ⟦ Δ ⤍ 01- ⟧ as Φ.org.eolang.bytes(Δ ⤍
                           01-) in the final output.
  FILE                     FILE to read input from. When no FILE is specified,
                           read from stdin.
  -h,--help                Show this help text
```

### `--rules FILE`

Similar to `--rules` for the `transform` subcommand, this argument accepts the path to a YAML file containing the rules to be used in the normalization phase.

### `--chain`

If the `--chain` argument is passed, all the intermediate steps of normalization + dataization are printed to the console (or the output file if chosen).

```$ as console
eo-phi-normalizer dataize --chain --rules ./eo-phi-normalizer/test/eo/phi/rules/new.yaml celsius.phi
```

```console
Evaluating lambda 'Package' : ⟦
  c ↦ Φ̇.float(
    as-bytes ↦ 25.0
  ),
  result ↦ ξ.c.times(
    x ↦ 1.8
  ).plus(
    x ↦ 32.0
  ),
  λ ⤍ Package
⟧
 Dataizing: Φ̇.float(
  as-bytes ↦ 25.0
)
 NF: Φ̇.float(
  as-bytes ↦ 25.0
)
  Dataizing inside application: Φ̇.float
   Dataizing inside dispatch: Φ̇
    Dataizing inside dispatch: Φ.org
     Dataizing inside dispatch: Φ
     Nothing to dataize: Φ
 Dataization changed nothing: Φ̇.float(
  as-bytes ↦ 25.0
)
 Dataizing: ξ.c.times(
  x ↦ 1.8
).plus(
  x ↦ 32.0
)
 NF: ξ.c.times(
  x ↦ 1.8
).plus(
  x ↦ 32.0
)
  Dataizing inside application: ξ.c.times(
  x ↦ 1.8
).plus
   Dataizing inside dispatch: ξ.c.times(
  x ↦ 1.8
)
    Dataizing inside application: ξ.c.times
     Dataizing inside dispatch: ξ.c
      Dataizing inside dispatch: ξ
      Nothing to dataize: ξ
 Dataization changed nothing: ξ.c.times(
  x ↦ 1.8
).plus(
  x ↦ 32.0
)
Dataized 'Package' siblings: ⟦
  c ↦ Φ̇.float(
    as-bytes ↦ 25.0
  ),
  result ↦ ξ.c.times(
    x ↦ 1.8
  ).plus(
    x ↦ 32.0
  ),
  λ ⤍ Package
⟧
```

### `--output-file FILE`

Redirects the output to file of the given path instead of `stdout`.

### `--dependency-file FILE`

Injects package dependencies from a given file into the context when dataizing the input.
Can be used multiple times to inject multiple dependencies.

### `--recursive`

Applies the normalization+dataization process recursively until it reaches bytes or no longer modifies the object (stalls).

```$ as console
eo-phi-normalizer dataize --recursive --rules eo-phi-normalizer/test/eo/phi/rules/new.yaml celsius.phi
```

```console
{
  ⟦
    c ↦ Φ̇.float(
      as-bytes ↦ 25.0
    ),
    result ↦ ξ.c.times(
      x ↦ 1.8
    ).plus(
      x ↦ 32.0
    ),
    λ ⤍ Package
  ⟧
}
```

Can be combined with `--chain` to print all the intermediate steps of both normalization and dataization.

### `--enable-atom`

Enable an atom by name.

```$ as console
eo-phi-normalizer dataize --minimize-stuck-terms --recursive --enable-atom "Lorg_eolang_dataized" --rules eo-phi-normalizer/test/eo/phi/rules/new.yaml celsius.phi
```

```console
{
  ⟦
    c ↦ Φ̇.float(
      as-bytes ↦ 25.0
    ),
    result ↦ ξ.c.times(
      x ↦ 1.8
    ).plus(
      x ↦ 32.0
    ),
    λ ⤍ Package
  ⟧
}
```

### `--disable-atom`

Disable an atom by name.

```$ as console
eo-phi-normalizer dataize --minimize-stuck-terms --recursive --disable-atom "Lorg_eolang_dataized" --rules eo-phi-normalizer/test/eo/phi/rules/new.yaml celsius.phi
```

```console
{
  ⟦
    c ↦ Φ̇.float(
      as-bytes ↦ 25.0
    ),
    result ↦ ξ.c.times(
      x ↦ 1.8
    ).plus(
      x ↦ 32.0
    ),
    λ ⤍ Package
  ⟧
}
```

### `FILE` not specified (read from stdin)

If no argument is given for the input file, `stdin` is consumed until `EOF`.

```$ as console
cat celsius.phi | eo-phi-normalizer dataize --recursive --rules ./eo-phi-normalizer/test/eo/phi/rules/new.yaml
```

```console
{
  ⟦
    c ↦ Φ̇.float(
      as-bytes ↦ 25.0
    ),
    result ↦ ξ.c.times(
      x ↦ 1.8
    ).plus(
      x ↦ 32.0
    ),
    λ ⤍ Package
  ⟧
}
```
