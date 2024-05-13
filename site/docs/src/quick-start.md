# Quick start

{{#include common/enter-repository.md}}

Install `normalizer` - see [Installation](./installation.md).

{{#include common/celsius.md}}

Dataize the program recursively.

```$ as console
normalizer dataize --recursive --rules eo-phi-normalizer/test/eo/phi/rules/yegor.yaml celsius.phi
```

```console
Φ.org.eolang.float (as-bytes ↦ Φ.org.eolang.bytes (Δ ⤍ 01-01-00-00-00-00-00-00-00-07-00-00-00-00-00-00-19-FF-FF-FF-FF-FF-FF-FF-D0)).times (x ↦ ⟦ Δ ⤍ 01-01-00-00-00-00-00-00-00-07-CD-CC-CC-CC-CC-CC-1C-FF-FF-FF-FF-FF-FF-FF-CC ⟧).plus (x ↦ ⟦ Δ ⤍ 01-01-00-00-00-00-00-00-00-07-00-00-00-00-00-00-10-FF-FF-FF-FF-FF-FF-FF-D1 ⟧)
```
