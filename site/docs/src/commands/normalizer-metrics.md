# normalizer metrics

## Metrics

We count:

- [Object applications](#object-applications)
- [Object formations](#object-formations)
- [Dynamic dispatches](#dynamic-dispatches)
- [Dataless formations](#dataless-formations)

### PHI grammar

![phi-grammar](../media/phi-grammar.png)

### Object formations

- `⟦ d ↦ ∅, c ↦ ∅ ⟧`

### Object applications

- `ξ.b(c ↦ ⟦ ⟧)`

### Dynamic dispatches

- `ξ.ρ.c`

### Dataless formations

- `Primitive formation` - a formation that has a `Δ` attribute.
  - `⟦ Δ ⤍ 00- ⟧`
- `Dataless formation` - not primitive and does not have attributes that map to primitive formations.
  - `⟦ d ↦ ⟦ φ ↦ ξ.ρ.c, ν ↦ ⟦ Δ ⤍ 00- ⟧ ⟧, c ↦ ∅ ⟧`
    - the outermost formation with attributes `d` and `c` is dataless because it is not primitive and its attributes do not map to primitive formations.
    - `d` is not dataless because it has an attribute `ν` that maps to a primitive formation.

## Environment

{{#include ../common/sample-program.md}}

## CLI

### --help

```$ as console
normalizer metrics --help
```

```console
Usage: normalizer metrics [FILE] [-o|--output-file FILE]

  Collect metrics for a PHI program.

Available options:
  FILE                     FILE to read input from. When not specified, read
                           from stdin.
  -o,--output-file FILE    Output to FILE. Output to stdout otherwise.
  -h,--help                Show this help text
```

### PROGRAM

```$ as json
normalizer metrics program.phi
```

```json
{
  "applications": 1,
  "dataless": 5,
  "dispatches": 5,
  "formations": 5
}
```

### PROGRAM not specified

```$ as json
cat program.phi | normalizer metrics
```

```json
{
  "applications": 1,
  "dataless": 5,
  "dispatches": 5,
  "formations": 5
}
```
