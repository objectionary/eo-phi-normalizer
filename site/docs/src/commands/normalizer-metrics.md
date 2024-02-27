# normalizer metrics

- [normalizer metrics](#normalizer-metrics)
  - [Metrics](#metrics)
    - [PHI grammar](#phi-grammar)
    - [Object formations](#object-formations)
    - [Object applications](#object-applications)
    - [Dynamic dispatches](#dynamic-dispatches)
    - [Dataless formations](#dataless-formations)
  - [CLI](#cli)

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

- `Primitive formation` - a formation that has a Δ-attribute.
  - `⟦ Δ ⤍ 00- ⟧`
- `Dataless formation` - not primitive and does not have attributes bound to primitive formations.
  - `⟦ d ↦ ⟦ φ ↦ ξ.ρ.c, ν ↦ ⟦ Δ ⤍ 00- ⟧ ⟧, c ↦ ∅ ⟧`

## Environment

{{#include ../common/sample-program.md}}

## CLI

### --help

```$ as console
normalizer metrics --help
```

```console
Usage: normalizer metrics [-i|--input-file FILE] [-o|--output-file FILE]
                          [PROGRAM]

  Collect metrics for a PHI program.

Available options:
  -i,--input-file FILE     FILE to read input from. When FILE is -, read from
                           stdin. You must specify either this option or
                           PROGRAM.
  -o,--output-file FILE    Output to FILE. Output to stdout otherwise.
  PROGRAM                  Program to work with.
  -h,--help                Show this help text
```

### --input-file program.phi

```$ as json
normalizer metrics --input-file program.phi
```

```json
{
  "applications": 1,
  "dataless": 5,
  "dispatches": 5,
  "formations": 5
}
```

### --input-file -

```$ as json
cat program.phi | normalizer metrics --input-file -
```

```json
{
  "applications": 1,
  "dataless": 5,
  "dispatches": 5,
  "formations": 5
}
```
