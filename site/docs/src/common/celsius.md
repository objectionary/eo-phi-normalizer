Save a `PHI` program to a file.

```$
cat > celsius.phi <<EOM
{⟦
  c ↦ Φ.org.eolang.float(
    as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ 01-01-00-00-00-00-00-00-00-07-00-00-00-00-00-00-19-FF-FF-FF-FF-FF-FF-FF-D0)
  ), // 25.0
  φ ↦ ξ.c.times(x ↦ ⟦ Δ ⤍ 01-01-00-00-00-00-00-00-00-07-CD-CC-CC-CC-CC-CC-1C-FF-FF-FF-FF-FF-FF-FF-CC ⟧)  // 1.8
        .plus(x ↦ ⟦ Δ ⤍ 01-01-00-00-00-00-00-00-00-07-00-00-00-00-00-00-10-FF-FF-FF-FF-FF-FF-FF-D1 ⟧), // 32.0
⟧}
EOM
```
