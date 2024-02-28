Save a `PHI` program to a file.
This program will be used in subsequent commands.

```sh
cat > program.phi <<EOM
{
  ⟦
    a ↦
      ⟦
        b ↦
            ⟦
              c ↦ ∅,
              d ↦ ⟦ φ ↦ ξ.ρ.c ⟧
            ⟧,
        e ↦ ξ.b(c ↦ ⟦⟧).d
      ⟧.e
  ⟧
}
EOM
```
