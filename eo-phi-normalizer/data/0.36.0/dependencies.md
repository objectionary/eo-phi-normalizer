# Dependencies

## [org/eolang/as-phi.phi](./org/eolang/as-phi.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        as-phi ↦ ⟦
          λ ⤍ Lorg_eolang_as_phi,
          x ↦ ∅
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/bool.phi](./org/eolang/bool.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        bool ↦ ⟦
          as-bytes ↦ ∅,
          φ ↦ ξ.as-bytes,
          eq ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.ρ.as-bytes.eq(
              α0 ↦ ξ.x.as-bytes
            )
          ⟧,
          not ↦ ⟦
            φ ↦ ξ.σ.σ.bool(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-
              )
            ).eq(
              α0 ↦ ξ.ρ
            )
          ⟧,
          and ↦ ⟦
            x ↦ ∅,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.ρ,
              α1 ↦ Φ.org.eolang.if(
                α0 ↦ ξ.x,
                α1 ↦ ξ.σ.σ.bool(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 01-
                  )
                ),
                α2 ↦ ξ.σ.σ.bool(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-
                  )
                )
              ),
              α2 ↦ ξ.σ.σ.bool(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-
                )
              )
            )
          ⟧,
          or ↦ ⟦
            x ↦ ∅,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.ρ,
              α1 ↦ ξ.σ.σ.bool(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 01-
                )
              ),
              α2 ↦ Φ.org.eolang.if(
                α0 ↦ ξ.x,
                α1 ↦ ξ.σ.σ.bool(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 01-
                  )
                ),
                α2 ↦ ξ.σ.σ.bool(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-
                  )
                )
              )
            )
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/bytes.phi](./org/eolang/bytes.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        bytes ↦ ⟦
          eq ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_eq,
            x ↦ ∅
          ⟧,
          size ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_size
          ⟧,
          slice ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_slice,
            start ↦ ∅,
            len ↦ ∅
          ⟧,
          as-string ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_as_string
          ⟧,
          as-int ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_as_int
          ⟧,
          as-float ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_as_float
          ⟧,
          and ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_and,
            b ↦ ∅
          ⟧,
          or ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_or,
            b ↦ ∅
          ⟧,
          xor ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_xor,
            b ↦ ∅
          ⟧,
          not ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_not
          ⟧,
          left ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.ρ.right(
              α0 ↦ ξ.x.neg
            )
          ⟧,
          right ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_right,
            x ↦ ∅
          ⟧,
          as-bool ↦ ⟦
            φ ↦ ξ.ρ.eq(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 01-
              )
            )
          ⟧,
          as-bytes ↦ ⟦
            φ ↦ ξ.ρ
          ⟧,
          concat ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_concat,
            b ↦ ∅
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/cage.phi](./org/eolang/cage.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        cage ↦ ⟦
          object ↦ ∅,
          new ↦ ξ.φ.self,
          φ ↦ ⟦
            λ ⤍ Lorg_eolang_cage_φ
          ⟧,
          encaged ↦ ⟦
            locator ↦ ∅,
            self ↦ ξ,
            φ ↦ ⟦
              λ ⤍ Lorg_eolang_cage_encaged_φ
            ⟧,
            encage ↦ ⟦
              λ ⤍ Lorg_eolang_cage_encaged_encage,
              object ↦ ∅
            ⟧
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/cti.phi](./org/eolang/cti.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        cti ↦ ⟦
          delegate ↦ ∅,
          level ↦ ∅,
          message ↦ ∅,
          φ ↦ ξ.delegate
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/dataized.phi](./org/eolang/dataized.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        dataized ↦ ⟦
          λ ⤍ Lorg_eolang_dataized,
          target ↦ ∅
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/error.phi](./org/eolang/error.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        error ↦ ⟦
          λ ⤍ Lorg_eolang_error,
          message ↦ ∅
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/float.phi](./org/eolang/float.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        float ↦ ⟦
          as-bytes ↦ ∅,
          φ ↦ ξ.as-bytes,
          eq ↦ ⟦
            x ↦ ∅,
            x-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            self-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.ρ
            ).as-bytes,
            nan-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ Φ.org.eolang.nan
            ).as-bytes,
            pos-zero-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.σ.σ.float(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
            ).as-bytes,
            neg-zero-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.σ.σ.float(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 80-00-00-00-00-00-00-00
                )
              )
            ).as-bytes,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.x-as-bytes.eq(
                α0 ↦ ξ.nan-as-bytes
              ).or(
                α0 ↦ ξ.self-as-bytes.eq(
                  α0 ↦ ξ.nan-as-bytes
                )
              ),
              α1 ↦ Φ.org.eolang.bool(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-
                )
              ),
              α2 ↦ ξ.x-as-bytes.eq(
                α0 ↦ ξ.pos-zero-as-bytes
              ).or(
                α0 ↦ ξ.x-as-bytes.eq(
                  α0 ↦ ξ.neg-zero-as-bytes
                )
              ).and(
                α0 ↦ ξ.self-as-bytes.eq(
                  α0 ↦ ξ.pos-zero-as-bytes
                ).or(
                  α0 ↦ ξ.self-as-bytes.eq(
                    α0 ↦ ξ.neg-zero-as-bytes
                  )
                )
              ).or(
                α0 ↦ ξ.self-as-bytes.eq(
                  α0 ↦ ξ.x-as-bytes
                )
              )
            )
          ⟧,
          lt ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.σ.σ.float(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ).gt(
              α0 ↦ ξ.ρ.minus(
                α0 ↦ ξ.σ.σ.float(
                  α0 ↦ ξ.value
                )
              )
            ),
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes
          ⟧,
          lte ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ ξ.ρ.eq(
              α0 ↦ ξ.value
            ).or(
              α0 ↦ ξ.ρ.lt(
                α0 ↦ ξ.value
              )
            )
          ⟧,
          gt ↦ ⟦
            λ ⤍ Lorg_eolang_float_gt,
            x ↦ ∅
          ⟧,
          gte ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ ξ.ρ.eq(
              α0 ↦ ξ.value
            ).or(
              α0 ↦ ξ.ρ.gt(
                α0 ↦ ξ.value
              )
            )
          ⟧,
          times ↦ ⟦
            λ ⤍ Lorg_eolang_float_times,
            x ↦ ∅
          ⟧,
          plus ↦ ⟦
            λ ⤍ Lorg_eolang_float_plus,
            x ↦ ∅
          ⟧,
          neg ↦ ⟦
            φ ↦ ξ.ρ.times(
              α0 ↦ ξ.σ.σ.float(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ BF-F0-00-00-00-00-00-00
                )
              )
            )
          ⟧,
          minus ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.ρ.plus(
              α0 ↦ ξ.x.neg
            )
          ⟧,
          div ↦ ⟦
            λ ⤍ Lorg_eolang_float_div,
            x ↦ ∅
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/go.phi](./org/eolang/go.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        go ↦ ⟦
          id ↦ Φ.org.eolang.malloc(
            α0 ↦ Φ.org.eolang.int(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-01
              )
            )
          ).id.as-bytes,
          to ↦ ⟦
            body ↦ ∅,
            φ ↦ Φ.org.eolang.try(
              α0 ↦ ξ.body(
                α0 ↦ ξ.token
              ),
              α1 ↦ ⟦
                e ↦ ∅,
                φ ↦ Φ.org.eolang.if(
                  α0 ↦ ξ.σ.ρ.id.eq(
                    α0 ↦ ξ.e.id
                  ),
                  α1 ↦ ξ.e.value,
                  α2 ↦ Φ.org.eolang.error(
                    α0 ↦ ξ.e
                  )
                )
              ⟧,
              α2 ↦ Φ.org.eolang.bool(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 01-
                )
              )
            ),
            token ↦ ⟦
              backward ↦ Φ.org.eolang.error(
                α0 ↦ ⟦
                  value ↦ ξ.σ.ρ.ρ.to(
                    α0 ↦ ξ.σ.ρ.body
                  ),
                  id ↦ ξ.σ.ρ.ρ.id
                ⟧
              ),
              forward ↦ ⟦
                res ↦ ∅,
                φ ↦ Φ.org.eolang.error(
                  α0 ↦ ⟦
                    value ↦ ξ.σ.res,
                    id ↦ ξ.σ.ρ.ρ.ρ.id
                  ⟧
                )
              ⟧
            ⟧
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/goto.phi](./org/eolang/goto.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        goto ↦ ⟦
          λ ⤍ Lambda,
          f ↦ ∅
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/heap.phi](./org/eolang/heap.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        heap ↦ ⟦
          size ↦ ∅,
          malloc ↦ ⟦
            s ↦ ∅,
            next ↦ Φ.org.eolang.memory(
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
            ),
            new-next ↦ ξ.s.plus(
              α0 ↦ ξ.ρ.malloc.next.as-int
            ),
            φ ↦ ξ.new-next.gt(
              α0 ↦ ξ.σ.size
            ).if(
              α0 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 41-6C-6C-6F-63-61-74-69-6F-6E-20-66-61-69-6C-65-64-3A-20-62-61-64-20-61-6C-6C-6F-63-20-28-6E-6F-74-20-65-6E-6F-75-67-68-20-6D-65-6D-6F-72-79-20-69-6E-20-74-68-65-20-68-65-61-70-29
                  )
                )
              ),
              α1 ↦ Φ.org.eolang.seq(
                α0 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.ρ.malloc.next.write(
                      α0 ↦ ξ.new-next
                    )
                  ),
                  α1 ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).plus(
                    α0 ↦ ξ.ρ.malloc.next.as-int
                  )
                )
              )
            )
          ⟧,
          free ↦ ⟦
            p ↦ ∅,
            φ ↦ Φ.org.eolang.seq(
              α0 ↦ Φ.org.eolang.tuple(
                α0 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ ξ.p
                ),
                α1 ↦ Φ.org.eolang.bool(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 01-
                  )
                )
              )
            )
          ⟧,
          pointer ↦ ⟦
            address ↦ ∅,
            length ↦ ∅,
            φ ↦ ξ.address,
            add ↦ ⟦
              x ↦ ∅,
              φ ↦ ξ.σ.ρ.pointer(
                α0 ↦ ξ.σ.address.plus(
                  α0 ↦ ξ.σ.length.times(
                    α0 ↦ ξ.x
                  )
                ),
                α1 ↦ ξ.σ.length
              )
            ⟧,
            sub ↦ ⟦
              x ↦ ∅,
              φ ↦ ξ.σ.add(
                α0 ↦ ξ.x.times(
                  α0 ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ FF-FF-FF-FF-FF-FF-FF-FF
                    )
                  )
                )
              )
            ⟧,
            block ↦ ⟦
              λ ⤍ Lambda,
              len ↦ ∅,
              inverse ↦ ∅
            ⟧
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/if.phi](./org/eolang/if.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        if ↦ ⟦
          λ ⤍ Lorg_eolang_if,
          condition ↦ ∅,
          left ↦ ∅,
          right ↦ ∅
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/int.phi](./org/eolang/int.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        int ↦ ⟦
          as-bytes ↦ ∅,
          φ ↦ ξ.as-bytes,
          eq ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.ρ.as-bytes.eq(
              α0 ↦ ξ.x.as-bytes
            )
          ⟧,
          lt ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.σ.σ.int(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ).gt(
              α0 ↦ ξ.ρ.minus(
                α0 ↦ ξ.σ.σ.int(
                  α0 ↦ ξ.value
                )
              )
            ),
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes
          ⟧,
          lte ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.ρ.gt(
              α0 ↦ ξ.value
            ).not,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes
          ⟧,
          gt ↦ ⟦
            λ ⤍ Lorg_eolang_int_gt,
            x ↦ ∅
          ⟧,
          gte ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.ρ.lt(
              α0 ↦ ξ.value
            ).not,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes
          ⟧,
          neg ↦ ⟦
            φ ↦ ξ.ρ.times(
              α0 ↦ ξ.σ.σ.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ FF-FF-FF-FF-FF-FF-FF-FF
                )
              )
            )
          ⟧,
          plus ↦ ⟦
            λ ⤍ Lorg_eolang_int_plus,
            x ↦ ∅
          ⟧,
          minus ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.ρ.plus(
              α0 ↦ ξ.x.neg
            )
          ⟧,
          times ↦ ⟦
            λ ⤍ Lorg_eolang_int_times,
            x ↦ ∅
          ⟧,
          div ↦ ⟦
            λ ⤍ Lorg_eolang_int_div,
            x ↦ ∅
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/io/stdin.phi](./org/eolang/io/stdin.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        io ↦ ⟦
          stdin ↦ ⟦
            next-line ↦ ⟦
              λ ⤍ Lorg_eolang_io_stdin_next_line
            ⟧,
            φ ↦ ⟦
              λ ⤍ Lorg_eolang_io_stdin_φ
            ⟧
          ⟧,
          λ ⤍ Package
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/io/stdout.phi](./org/eolang/io/stdout.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        io ↦ ⟦
          stdout ↦ ⟦
            λ ⤍ Lorg_eolang_io_stdout,
            text ↦ ∅
          ⟧,
          λ ⤍ Package
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/malloc.phi](./org/eolang/malloc.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        malloc ↦ ⟦
          size ↦ ∅,
          pointer ↦ ξ.φ.pointer,
          φ ↦ ⟦
            λ ⤍ Lorg_eolang_malloc_φ
          ⟧,
          memory-block-pointer ↦ ⟦
            id ↦ ∅,
            pointer ↦ ξ,
            size ↦ ξ.ρ.size,
            read ↦ ⟦
              λ ⤍ Lorg_eolang_malloc_memory_block_pointer_read,
              offset ↦ ∅,
              length ↦ ∅
            ⟧,
            write ↦ ⟦
              λ ⤍ Lorg_eolang_malloc_memory_block_pointer_write,
              offset ↦ ∅,
              data ↦ ∅
            ⟧,
            free ↦ ⟦
              λ ⤍ Lorg_eolang_malloc_memory_block_pointer_free
            ⟧
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/memory.phi](./org/eolang/memory.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        memory ↦ ⟦
          data ↦ ∅,
          alloc ↦ ξ.φ.alloc,
          φ ↦ ⟦
            bts ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.ρ.data
            ).as-bytes,
            p ↦ Φ.org.eolang.malloc(
              α0 ↦ ξ.bts.size
            ).pointer,
            φ ↦ Φ.org.eolang.seq(
              α0 ↦ Φ.org.eolang.tuple(
                α0 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ ξ.p.write(
                    α0 ↦ Φ.org.eolang.int(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    ),
                    α1 ↦ ξ.bts
                  )
                ),
                α1 ↦ ξ.ρ.allocated(
                  α0 ↦ ξ.p
                )
              )
            )
          ⟧,
          allocated ↦ ⟦
            pointer ↦ ∅,
            alloc ↦ ξ,
            φ ↦ ξ.pointer.read(
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ ξ.pointer.size
            ),
            write ↦ ⟦
              data ↦ ∅,
              φ ↦ Φ.org.eolang.seq(
                α0 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.ρ.pointer.write(
                      α0 ↦ Φ.org.eolang.int(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      ),
                      α1 ↦ ξ.data
                    )
                  ),
                  α1 ↦ ξ.ρ.pointer.read(
                    α0 ↦ Φ.org.eolang.int(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    ),
                    α1 ↦ ξ.ρ.pointer.size
                  )
                )
              )
            ⟧,
            free ↦ ⟦
              φ ↦ ξ.ρ.pointer.free
            ⟧
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/nan.phi](./org/eolang/nan.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        nan ↦ ⟦
          φ ↦ Φ.org.eolang.float(
            α0 ↦ Φ.org.eolang.bytes(
              Δ ⤍ 00-00-00-00-00-00-00-00
            )
          ).div(
            α0 ↦ Φ.org.eolang.float(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            )
          ),
          eq ↦ ⟦
            x ↦ ∅,
            φ ↦ Φ.org.eolang.bool(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-
              )
            )
          ⟧,
          lt ↦ ⟦
            x ↦ ∅,
            φ ↦ Φ.org.eolang.bool(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-
              )
            )
          ⟧,
          lte ↦ ⟦
            x ↦ ∅,
            φ ↦ Φ.org.eolang.bool(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-
              )
            )
          ⟧,
          gt ↦ ⟦
            x ↦ ∅,
            φ ↦ Φ.org.eolang.bool(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-
              )
            )
          ⟧,
          gte ↦ ⟦
            x ↦ ∅,
            φ ↦ Φ.org.eolang.bool(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-
              )
            )
          ⟧,
          times ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.σ
          ⟧,
          plus ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.σ
          ⟧,
          neg ↦ ⟦
            φ ↦ ξ.σ
          ⟧,
          minus ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.σ
          ⟧,
          div ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.σ
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/negative-infinity.phi](./org/eolang/negative-infinity.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        negative-infinity ↦ ⟦
          φ ↦ Φ.org.eolang.float(
            α0 ↦ Φ.org.eolang.bytes(
              Δ ⤍ BF-F0-00-00-00-00-00-00
            )
          ).div(
            α0 ↦ Φ.org.eolang.float(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            )
          ),
          eq ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.σ.as-bytes.eq(
              α0 ↦ ξ.x.as-bytes
            )
          ⟧,
          lt ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ ξ.value.eq(
              α0 ↦ Φ.org.eolang.nan.as-bytes
            ).or(
              α0 ↦ ξ.σ.eq(
                α0 ↦ ξ.value
              )
            ).not
          ⟧,
          lte ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.x.as-bytes.eq(
              α0 ↦ Φ.org.eolang.nan.as-bytes
            ).not
          ⟧,
          gt ↦ ⟦
            x ↦ ∅,
            φ ↦ Φ.org.eolang.bool(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-
              )
            )
          ⟧,
          gte ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.σ.eq(
              α0 ↦ ξ.x
            )
          ⟧,
          times ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            is-num-gt-zero ↦ ⟦
              num ↦ ∅,
              φ ↦ Φ.org.eolang.try(
                α0 ↦ ⟦
                  φ ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).lt(
                    α0 ↦ ξ.σ.num
                  )
                ⟧,
                α1 ↦ ⟦
                  e ↦ ∅,
                  φ ↦ Φ.org.eolang.float(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).lt(
                    α0 ↦ ξ.σ.num
                  )
                ⟧,
                α2 ↦ Φ.org.eolang.bool(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-
                  )
                )
              )
            ⟧,
            is-nan ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.nan.as-bytes
              )
            ⟧,
            is-nan-or-zero ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.σ.is-nan(
                α0 ↦ ξ.num
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ Φ.org.eolang.float(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 80-00-00-00-00-00-00-00
                    )
                  )
                )
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ Φ.org.eolang.float(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                )
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                )
              )
            ⟧,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.is-nan-or-zero(
                α0 ↦ ξ.value
              ),
              α1 ↦ Φ.org.eolang.nan,
              α2 ↦ Φ.org.eolang.if(
                α0 ↦ ξ.is-num-gt-zero(
                  α0 ↦ ξ.value
                ),
                α1 ↦ ξ.σ.σ.negative-infinity,
                α2 ↦ Φ.org.eolang.positive-infinity
              )
            )
          ⟧,
          plus ↦ ⟦
            x ↦ ∅,
            pos-inf-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ Φ.org.eolang.positive-infinity.as-bytes
            ).as-bytes,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            is-nan ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.nan.as-bytes
              )
            ⟧,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.is-nan(
                α0 ↦ ξ.value
              ).or(
                α0 ↦ ξ.value.eq(
                  α0 ↦ ξ.pos-inf-as-bytes
                )
              ),
              α1 ↦ Φ.org.eolang.nan,
              α2 ↦ ξ.σ.σ.negative-infinity
            )
          ⟧,
          minus ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            neg-inf-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.σ.σ.negative-infinity
            ).as-bytes,
            is-nan ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.nan.as-bytes
              )
            ⟧,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.is-nan(
                α0 ↦ ξ.value
              ).or(
                α0 ↦ ξ.value.eq(
                  α0 ↦ ξ.neg-inf-as-bytes
                )
              ),
              α1 ↦ Φ.org.eolang.nan,
              α2 ↦ ξ.σ.σ.negative-infinity
            )
          ⟧,
          div ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            is-nan ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.nan.as-bytes
              )
            ⟧,
            is-nan-or-infinite ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.σ.is-nan(
                α0 ↦ ξ.num
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ Φ.org.eolang.positive-infinity
                )
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ ξ.σ.σ.σ.negative-infinity
                )
              )
            ⟧,
            is-num-gte-zero ↦ ⟦
              num ↦ ∅,
              φ ↦ Φ.org.eolang.try(
                α0 ↦ ⟦
                  φ ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).lte(
                    α0 ↦ ξ.σ.num
                  )
                ⟧,
                α1 ↦ ⟦
                  e ↦ ∅,
                  φ ↦ Φ.org.eolang.float(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).lte(
                    α0 ↦ ξ.σ.num
                  )
                ⟧,
                α2 ↦ Φ.org.eolang.bool(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-
                  )
                )
              )
            ⟧,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.is-nan-or-infinite(
                α0 ↦ ξ.value
              ),
              α1 ↦ Φ.org.eolang.nan,
              α2 ↦ Φ.org.eolang.if(
                α0 ↦ ξ.is-num-gte-zero(
                  α0 ↦ ξ.value
                ),
                α1 ↦ ξ.σ.σ.negative-infinity,
                α2 ↦ Φ.org.eolang.positive-infinity
              )
            )
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/nop.phi](./org/eolang/nop.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        nop ↦ ⟦
          args ↦ ∅,
          φ ↦ Φ.org.eolang.bool(
            α0 ↦ Φ.org.eolang.bytes(
              Δ ⤍ 01-
            )
          )
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/positive-infinity.phi](./org/eolang/positive-infinity.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        positive-infinity ↦ ⟦
          φ ↦ Φ.org.eolang.float(
            α0 ↦ Φ.org.eolang.bytes(
              Δ ⤍ 3F-F0-00-00-00-00-00-00
            )
          ).div(
            α0 ↦ Φ.org.eolang.float(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            )
          ),
          eq ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.σ.as-bytes.eq(
              α0 ↦ ξ.x.as-bytes
            )
          ⟧,
          lt ↦ ⟦
            x ↦ ∅,
            φ ↦ Φ.org.eolang.bool(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-
              )
            )
          ⟧,
          lte ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.σ.eq(
              α0 ↦ ξ.x
            )
          ⟧,
          gt ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ ξ.value.eq(
              α0 ↦ Φ.org.eolang.nan.as-bytes
            ).or(
              α0 ↦ ξ.σ.eq(
                α0 ↦ ξ.value
              )
            ).not
          ⟧,
          gte ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.x.as-bytes.eq(
              α0 ↦ Φ.org.eolang.nan.as-bytes
            ).not
          ⟧,
          times ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            is-nan ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.nan.as-bytes
              )
            ⟧,
            is-nan-or-zero ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.σ.is-nan(
                α0 ↦ ξ.num
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ Φ.org.eolang.float(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 80-00-00-00-00-00-00-00
                    )
                  )
                )
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ Φ.org.eolang.float(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                )
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                )
              )
            ⟧,
            is-num-gt-zero ↦ ⟦
              num ↦ ∅,
              φ ↦ Φ.org.eolang.try(
                α0 ↦ ⟦
                  φ ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).lt(
                    α0 ↦ ξ.σ.num
                  )
                ⟧,
                α1 ↦ ⟦
                  e ↦ ∅,
                  φ ↦ Φ.org.eolang.float(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).lt(
                    α0 ↦ ξ.σ.num
                  )
                ⟧,
                α2 ↦ Φ.org.eolang.bool(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-
                  )
                )
              )
            ⟧,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.is-nan-or-zero(
                α0 ↦ ξ.value
              ),
              α1 ↦ Φ.org.eolang.nan,
              α2 ↦ Φ.org.eolang.if(
                α0 ↦ ξ.is-num-gt-zero(
                  α0 ↦ ξ.value
                ),
                α1 ↦ ξ.σ.σ.positive-infinity,
                α2 ↦ Φ.org.eolang.negative-infinity
              )
            )
          ⟧,
          plus ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            neg-inf-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ Φ.org.eolang.negative-infinity
            ).as-bytes,
            is-nan ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.nan.as-bytes
              )
            ⟧,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.is-nan(
                α0 ↦ ξ.value
              ).or(
                α0 ↦ ξ.value.eq(
                  α0 ↦ ξ.neg-inf-as-bytes
                )
              ),
              α1 ↦ Φ.org.eolang.nan,
              α2 ↦ ξ.σ.σ.positive-infinity
            )
          ⟧,
          minus ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            pos-inf-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.σ.σ.positive-infinity
            ).as-bytes,
            is-nan ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.nan.as-bytes
              )
            ⟧,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.is-nan(
                α0 ↦ ξ.value
              ).or(
                α0 ↦ ξ.value.eq(
                  α0 ↦ ξ.pos-inf-as-bytes
                )
              ),
              α1 ↦ Φ.org.eolang.nan,
              α2 ↦ ξ.σ.σ.positive-infinity
            )
          ⟧,
          div ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            is-nan ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.nan.as-bytes
              )
            ⟧,
            is-nan-or-infinite ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.σ.is-nan(
                α0 ↦ ξ.num
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ ξ.σ.σ.σ.positive-infinity
                )
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ Φ.org.eolang.negative-infinity
                )
              )
            ⟧,
            is-num-gte-zero ↦ ⟦
              num ↦ ∅,
              φ ↦ Φ.org.eolang.try(
                α0 ↦ ⟦
                  φ ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).lte(
                    α0 ↦ ξ.σ.num
                  )
                ⟧,
                α1 ↦ ⟦
                  e ↦ ∅,
                  φ ↦ Φ.org.eolang.float(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).lte(
                    α0 ↦ ξ.σ.num
                  )
                ⟧,
                α2 ↦ Φ.org.eolang.bool(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-
                  )
                )
              )
            ⟧,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.is-nan-or-infinite(
                α0 ↦ ξ.value
              ),
              α1 ↦ Φ.org.eolang.nan,
              α2 ↦ Φ.org.eolang.if(
                α0 ↦ ξ.is-num-gte-zero(
                  α0 ↦ ξ.value
                ),
                α1 ↦ ξ.σ.σ.positive-infinity,
                α2 ↦ Φ.org.eolang.negative-infinity
              )
            )
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/ram.phi](./org/eolang/ram.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        ram ↦ ⟦
          size ↦ ∅,
          write ↦ ⟦
            λ ⤍ Lambda,
            position ↦ ∅,
            data ↦ ∅
          ⟧,
          slice ↦ ⟦
            λ ⤍ Lambda,
            position ↦ ∅,
            size ↦ ∅
          ⟧,
          ram-slice ↦ ⟦
            position ↦ ∅,
            size ↦ ∅,
            φ ↦ ⟦
              λ ⤍ Lambda
            ⟧,
            write ↦ ⟦
              λ ⤍ Lambda,
              data ↦ ∅
            ⟧
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/rust.phi](./org/eolang/rust.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        rust ↦ ⟦
          λ ⤍ Lorg_eolang_rust,
          code ↦ ∅,
          portal ↦ ∅,
          params ↦ ∅
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/seq.phi](./org/eolang/seq.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        seq ↦ ⟦
          λ ⤍ Lorg_eolang_seq,
          steps ↦ ∅
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/string.phi](./org/eolang/string.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        string ↦ ⟦
          as-bytes ↦ ∅,
          φ ↦ ξ.as-bytes,
          eq ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.ρ.as-bytes.eq(
              α0 ↦ ξ.x.as-bytes
            )
          ⟧,
          length ↦ ⟦
            λ ⤍ Lorg_eolang_string_length
          ⟧,
          slice ↦ ⟦
            λ ⤍ Lorg_eolang_string_slice,
            start ↦ ∅,
            len ↦ ∅
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/switch.phi](./org/eolang/switch.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        switch ↦ ⟦
          cases ↦ ∅,
          len ↦ Φ.org.eolang.dataized(
            α0 ↦ ξ.cases.length
          ).as-bytes,
          case-at ↦ ⟦
            index ↦ ∅,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.index.eq(
                α0 ↦ ξ.ρ.len
              ),
              α1 ↦ Φ.org.eolang.bool(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 01-
                )
              ),
              α2 ↦ Φ.org.eolang.if(
                α0 ↦ ξ.case.at(
                  α0 ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                ),
                α1 ↦ ξ.case.at(
                  α0 ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-01
                    )
                  )
                ),
                α2 ↦ ξ.ρ.case-at(
                  α0 ↦ ξ.index.plus(
                    α0 ↦ Φ.org.eolang.int(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-01
                      )
                    )
                  )
                )
              )
            ),
            case ↦ ξ.ρ.cases.at(
              α0 ↦ ξ.index
            )
          ⟧,
          φ ↦ Φ.org.eolang.if(
            α0 ↦ ξ.len.eq(
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
            ),
            α1 ↦ Φ.org.eolang.error(
              α0 ↦ Φ.org.eolang.string(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 73-77-69-74-63-68-20-63-61-73-65-73-20-61-72-65-20-65-6D-70-74-79
                )
              )
            ),
            α2 ↦ ξ.case-at(
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
            )
          )
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/try.phi](./org/eolang/try.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        try ↦ ⟦
          λ ⤍ Lorg_eolang_try,
          main ↦ ∅,
          catch ↦ ∅,
          finally ↦ ∅
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/tuple.phi](./org/eolang/tuple.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        tuple ↦ ⟦
          head ↦ ∅,
          tail ↦ ∅,
          empty ↦ ⟦
            at ↦ ⟦
              i ↦ ∅,
              φ ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 43-61-6E-27-74-20-67-65-74-20-61-6E-20-6F-62-6A-65-63-74-20-66-72-6F-6D-20-74-68-65-20-65-6D-70-74-79-20-74-75-70-6C-65
                  )
                )
              )
            ⟧,
            with ↦ ⟦
              x ↦ ∅,
              φ ↦ ξ.σ.σ.σ.tuple(
                α0 ↦ ξ.σ.σ.σ.tuple.empty,
                α1 ↦ ξ.x
              )
            ⟧,
            length ↦ Φ.org.eolang.int(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            )
          ⟧,
          length ↦ ⟦
            φ ↦ Φ.org.eolang.int(
              α0 ↦ ξ.len
            ),
            len ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.ρ.head.length.plus(
                α0 ↦ Φ.org.eolang.int(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-01
                  )
                )
              )
            ).as-bytes
          ⟧,
          at ↦ ⟦
            i ↦ ∅,
            len ↦ ξ.ρ.length,
            index ↦ Φ.org.eolang.dataized(
              α0 ↦ Φ.org.eolang.if(
                α0 ↦ Φ.org.eolang.int(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).gt(
                  α0 ↦ ξ.idx
                ),
                α1 ↦ ξ.len.plus(
                  α0 ↦ ξ.idx
                ),
                α2 ↦ ξ.idx
              )
            ).as-bytes,
            φ ↦ Φ.org.eolang.if(
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).gt(
                α0 ↦ ξ.index
              ).or(
                α0 ↦ ξ.len.lte(
                  α0 ↦ ξ.index
                )
              ),
              α1 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 47-69-76-65-6E-20-69-6E-64-65-78-20-69-73-20-6F-75-74-20-6F-66-20-74-75-70-6C-65-20-62-6F-75-6E-64-73
                  )
                )
              ),
              α2 ↦ ξ.at-fast(
                α0 ↦ ξ.ρ,
                α1 ↦ ξ.len
              )
            ),
            at-fast ↦ ⟦
              tup ↦ ∅,
              len ↦ ∅,
              φ ↦ Φ.org.eolang.if(
                α0 ↦ ξ.len.plus(
                  α0 ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ FF-FF-FF-FF-FF-FF-FF-FF
                    )
                  )
                ).gt(
                  α0 ↦ ξ.ρ.index
                ),
                α1 ↦ ξ.ρ.at-fast(
                  α0 ↦ ξ.tup.head,
                  α1 ↦ ξ.len.plus(
                    α0 ↦ Φ.org.eolang.int(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ FF-FF-FF-FF-FF-FF-FF-FF
                      )
                    )
                  )
                ),
                α2 ↦ ξ.tup.tail
              )
            ⟧,
            idx ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.i
            ).as-bytes
          ⟧,
          with ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.σ.σ.tuple(
              α0 ↦ ξ.ρ,
              α1 ↦ ξ.x
            )
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/while.phi](./org/eolang/while.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        while ↦ ⟦
          condition ↦ ∅,
          body ↦ ∅,
          φ ↦ Φ.org.eolang.if(
            α0 ↦ ξ.condition,
            α1 ↦ ξ.start(
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
            ),
            α2 ↦ Φ.org.eolang.bool(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-
              )
            )
          ),
          start ↦ ⟦
            index ↦ ∅,
            φ ↦ Φ.org.eolang.seq(
              α0 ↦ Φ.org.eolang.tuple(
                α0 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ ξ.ρ.body(
                    α0 ↦ ξ.index
                  )
                ),
                α1 ↦ ξ.ρ.loop(
                  α0 ↦ ξ.index.plus(
                    α0 ↦ Φ.org.eolang.int(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-01
                      )
                    )
                  )
                )
              )
            )
          ⟧,
          loop ↦ ⟦
            index ↦ ∅,
            current ↦ ξ.ρ.body(
              α0 ↦ ξ.index
            ),
            φ ↦ Φ.org.eolang.if(
              α0 ↦ ξ.ρ.condition,
              α1 ↦ Φ.org.eolang.seq(
                α0 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.current
                  ),
                  α1 ↦ ξ.ρ.loop(
                    α0 ↦ ξ.index.plus(
                      α0 ↦ Φ.org.eolang.int(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-01
                        )
                      )
                    )
                  )
                )
              ),
              α2 ↦ ξ.current
            )
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```
