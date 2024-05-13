# Quick start

{{#include common/enter-repository.md}}

Install `normalizer` - see [Installation](./installation.md).

{{#include common/celsius.md}}

Dataize the program recursively.

```$ as console
normalizer dataize --recursive --rules eo-phi-normalizer/test/eo/phi/rules/yegor.yaml celsius.phi
```

```console
⟦ α0 ↦ ⟦ Δ ⤍ 02- ⟧, λ ⤍ Times, ρ ↦ ⟦ Δ ⤍ 19-, plus ↦ ⟦ α0 ↦ ∅, λ ⤍ Plus ⟧, ρ ↦ ⟦ ρ ↦ ⟦ ⟧ ⟧ ⟧ ⟧.plus (α0 ↦ ⟦ Δ ⤍ 20- ⟧)
```
