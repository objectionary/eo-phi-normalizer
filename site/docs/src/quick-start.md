# Quick start

{{#include common/enter-repository.md}}

Install `eo-phi-normalizer` - see [Installation](./installation.md).

{{#include common/celsius.md}}

Dataize the program recursively.

```$ as console
eo-phi-normalizer dataize \
  --recursive \
  --rules eo-phi-normalizer/test/eo/phi/rules/new.yaml \
  --dependency-file 'eo-phi-normalizer/data/0.38.0/org/eolang/float.phi' \
  --dependency-file 'eo-phi-normalizer/data/0.38.0/org/eolang/bytes.phi' \
  celsius.phi
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
