Save a `PHI` program to a file.

```$
cat > celsius.phi <<EOM
{⟦
  c ↦ Φ.org.eolang.float(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ 40-39-00-00-00-00-00-00)), // 25.0
  result ↦
    ξ.c.times(x ↦ ⟦ Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD ⟧)  // 1.8
       .plus(x ↦ ⟦ Δ ⤍ 40-40-00-00-00-00-00-00 ⟧), // 32.0
  λ ⤍ Package
⟧}
EOM
```
