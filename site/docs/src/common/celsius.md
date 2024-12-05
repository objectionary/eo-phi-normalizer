Save a `PHI` program to a file.

```$
cat > celsius.phi <<EOM
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
EOM
```
