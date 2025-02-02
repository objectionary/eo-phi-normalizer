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

## [org/eolang/bytes.phi](./org/eolang/bytes.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        bytes ↦ ⟦
          Δ ⤍ ∅,
          as-bytes ↦ ξ,
          eq ↦ ⟦
            λ ⤍ Lorg_eolang_bytes_eq,
            b ↦ ∅
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
            φ ↦ Φ.org.eolang.string(
              α0 ↦ ξ.ρ
            )
          ⟧,
          as-int ↦ ⟦
            φ ↦ ξ.ρ.size.eq(
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-08
                )
              )
            ).if(
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ ξ.ρ
              ),
              α1 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-6E-74
                  )
                )
              )
            )
          ⟧,
          as-float ↦ ⟦
            φ ↦ ξ.ρ.size.eq(
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-08
                )
              )
            ).if(
              α0 ↦ Φ.org.eolang.float(
                α0 ↦ ξ.ρ
              ),
              α1 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-66-6C-6F-61-74
                  )
                )
              )
            )
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
            φ ↦ ξ.ρ.right(
              α0 ↦ ξ.x.neg
            ),
            x ↦ ∅
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
          φ ↦ ξ.delegate,
          delegate ↦ ∅,
          level ↦ ∅,
          message ↦ ∅
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

## [org/eolang/false.phi](./org/eolang/false.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        false ↦ ⟦
          φ ↦ Φ.org.eolang.bytes(
            Δ ⤍ 00-
          ),
          not ↦ Φ.org.eolang.true,
          if ↦ ⟦
            φ ↦ ξ.right,
            left ↦ ∅,
            right ↦ ∅
          ⟧,
          and ↦ ⟦
            φ ↦ ξ.ρ,
            x ↦ ∅
          ⟧,
          or ↦ ⟦
            φ ↦ Φ.org.eolang.bytes(
              Δ ⤍ 01-
            ).eq(
              α0 ↦ ξ.x
            ),
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
              α0 ↦ ξ.ρ.ρ.float(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
            ).as-bytes,
            neg-zero-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.ρ.ρ.float(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 80-00-00-00-00-00-00-00
                )
              )
            ).as-bytes,
            φ ↦ ξ.x-as-bytes.eq(
              α0 ↦ ξ.nan-as-bytes
            ).or(
              α0 ↦ ξ.self-as-bytes.eq(
                α0 ↦ ξ.nan-as-bytes
              )
            ).if(
              α0 ↦ Φ.org.eolang.false,
              α1 ↦ ξ.x-as-bytes.eq(
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
            φ ↦ ξ.ρ.ρ.float(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ).gt(
              α0 ↦ ξ.ρ.minus(
                α0 ↦ ξ.ρ.ρ.float(
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
              α0 ↦ ξ.ρ.ρ.float(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ BF-F0-00-00-00-00-00-00
                )
              )
            )
          ⟧,
          minus ↦ ⟦
            φ ↦ ξ.ρ.plus(
              α0 ↦ ξ.x.neg
            ),
            x ↦ ∅
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
          id ↦ Φ.org.eolang.dataized(
            α0 ↦ Φ.org.eolang.malloc.of(
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-08
                )
              ),
              α1 ↦ ⟦
                φ ↦ ξ.m.put(
                  α0 ↦ ξ.m.id
                ),
                m ↦ ∅
              ⟧
            )
          ).as-bytes,
          to ↦ ⟦
            body ↦ ∅,
            φ ↦ Φ.org.eolang.try(
              α0 ↦ ξ.body(
                α0 ↦ ξ.token
              ),
              α1 ↦ ξ.auto-named-attr-at-64-9,
              α2 ↦ Φ.org.eolang.true
            ),
            token ↦ ⟦
              backward ↦ Φ.org.eolang.error(
                α0 ↦ ξ.jump(
                  α0 ↦ ξ.ρ.ρ.to(
                    α0 ↦ ξ.ρ.body
                  )
                )
              ),
              jump ↦ ⟦
                value ↦ ∅,
                id ↦ ξ.ρ.ρ.ρ.id
              ⟧,
              forward ↦ ⟦
                res ↦ ∅,
                φ ↦ Φ.org.eolang.error(
                  α0 ↦ ξ.ρ.jump(
                    α0 ↦ ξ.res
                  )
                )
              ⟧
            ⟧,
            auto-named-attr-at-64-9 ↦ ⟦
              e ↦ ∅,
              φ ↦ ξ.ρ.ρ.id.eq(
                α0 ↦ ξ.e.id
              ).if(
                α0 ↦ ξ.e.value,
                α1 ↦ Φ.org.eolang.error(
                  α0 ↦ ξ.e
                )
              )
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
            φ ↦ ξ.ρ.ρ.int(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ).gt(
              α0 ↦ ξ.ρ.minus(
                α0 ↦ ξ.ρ.ρ.int(
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
              α0 ↦ ξ.ρ.ρ.int(
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
            φ ↦ ξ.ρ.plus(
              α0 ↦ ξ.x.neg
            ),
            x ↦ ∅
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
          for ↦ ⟦
            object ↦ ∅,
            scope ↦ ∅,
            bts ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.object
            ).as-bytes,
            φ ↦ ξ.ρ.ρ.malloc.of(
              α0 ↦ ξ.bts.size,
              α1 ↦ ξ.auto-named-attr-at-89-9
            ),
            auto-named-attr-at-89-9 ↦ ⟦
              m ↦ ∅,
              φ ↦ Φ.org.eolang.seq(
                α0 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.m.write(
                      α0 ↦ Φ.org.eolang.int(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      ),
                      α1 ↦ ξ.ρ.bts
                    )
                  ),
                  α1 ↦ ξ.ρ.scope(
                    α0 ↦ ξ.m
                  )
                )
              )
            ⟧
          ⟧,
          of ↦ ⟦
            size ↦ ∅,
            scope ↦ ∅,
            φ ↦ ⟦
              λ ⤍ Lorg_eolang_malloc_of_φ
            ⟧,
            allocated ↦ ⟦
              id ↦ ∅,
              size ↦ ξ.ρ.size,
              φ ↦ ξ.get,
              read ↦ ⟦
                λ ⤍ Lorg_eolang_malloc_of_allocated_read,
                offset ↦ ∅,
                length ↦ ∅
              ⟧,
              write ↦ ⟦
                λ ⤍ Lorg_eolang_malloc_of_allocated_write,
                offset ↦ ∅,
                data ↦ ∅
              ⟧,
              get ↦ ⟦
                φ ↦ ξ.ρ.read(
                  α0 ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ),
                  α1 ↦ ξ.ρ.size
                )
              ⟧,
              put ↦ ⟦
                object ↦ ∅,
                φ ↦ Φ.org.eolang.seq(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ ξ.ρ.write(
                        α0 ↦ Φ.org.eolang.int(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 00-00-00-00-00-00-00-00
                          )
                        ),
                        α1 ↦ ξ.object
                      )
                    ),
                    α1 ↦ ξ.ρ.get
                  )
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
            φ ↦ Φ.org.eolang.false,
            x ↦ ∅
          ⟧,
          lt ↦ ⟦
            φ ↦ Φ.org.eolang.false,
            x ↦ ∅
          ⟧,
          lte ↦ ⟦
            φ ↦ Φ.org.eolang.false,
            x ↦ ∅
          ⟧,
          gt ↦ ⟦
            φ ↦ Φ.org.eolang.false,
            x ↦ ∅
          ⟧,
          gte ↦ ⟦
            φ ↦ Φ.org.eolang.false,
            x ↦ ∅
          ⟧,
          times ↦ ⟦
            φ ↦ ξ.ρ,
            x ↦ ∅
          ⟧,
          plus ↦ ⟦
            φ ↦ ξ.ρ,
            x ↦ ∅
          ⟧,
          neg ↦ ⟦
            φ ↦ ξ.ρ
          ⟧,
          minus ↦ ⟦
            φ ↦ ξ.ρ,
            x ↦ ∅
          ⟧,
          div ↦ ⟦
            φ ↦ ξ.ρ,
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
            φ ↦ ξ.ρ.as-bytes.eq(
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
              α0 ↦ ξ.ρ.eq(
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
            φ ↦ Φ.org.eolang.false
          ⟧,
          gte ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.ρ.eq(
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
                α0 ↦ Φ.org.eolang.int(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).lt(
                  α0 ↦ ξ.num
                ),
                α1 ↦ ξ.auto-named-attr-at-69-26,
                α2 ↦ Φ.org.eolang.false
              ),
              auto-named-attr-at-69-26 ↦ ⟦
                φ ↦ Φ.org.eolang.float(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).lt(
                  α0 ↦ ξ.ρ.num
                ),
                e ↦ ∅
              ⟧
            ⟧,
            is-nan ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.nan.as-bytes
              )
            ⟧,
            is-nan-or-zero ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.ρ.is-nan(
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
            φ ↦ ξ.is-nan-or-zero(
              α0 ↦ ξ.value
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.is-num-gt-zero(
                α0 ↦ ξ.value
              ).if(
                α0 ↦ ξ.ρ.ρ.negative-infinity,
                α1 ↦ Φ.org.eolang.positive-infinity
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
            φ ↦ ξ.is-nan(
              α0 ↦ ξ.value
            ).or(
              α0 ↦ ξ.value.eq(
                α0 ↦ ξ.pos-inf-as-bytes
              )
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.ρ.ρ.negative-infinity
            )
          ⟧,
          minus ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            neg-inf-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.ρ.ρ.negative-infinity
            ).as-bytes,
            is-nan ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.nan.as-bytes
              )
            ⟧,
            φ ↦ ξ.is-nan(
              α0 ↦ ξ.value
            ).or(
              α0 ↦ ξ.value.eq(
                α0 ↦ ξ.neg-inf-as-bytes
              )
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.ρ.ρ.negative-infinity
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
              φ ↦ ξ.ρ.is-nan(
                α0 ↦ ξ.num
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ Φ.org.eolang.positive-infinity
                )
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ ξ.ρ.ρ.ρ.negative-infinity
                )
              )
            ⟧,
            is-num-gte-zero ↦ ⟦
              num ↦ ∅,
              φ ↦ Φ.org.eolang.try(
                α0 ↦ Φ.org.eolang.int(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).lte(
                  α0 ↦ ξ.num
                ),
                α1 ↦ ξ.auto-named-attr-at-136-27,
                α2 ↦ Φ.org.eolang.false
              ),
              auto-named-attr-at-136-27 ↦ ⟦
                φ ↦ Φ.org.eolang.float(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).lte(
                  α0 ↦ ξ.ρ.num
                ),
                e ↦ ∅
              ⟧
            ⟧,
            φ ↦ ξ.is-nan-or-infinite(
              α0 ↦ ξ.value
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.is-num-gte-zero(
                α0 ↦ ξ.value
              ).if(
                α0 ↦ ξ.ρ.ρ.negative-infinity,
                α1 ↦ Φ.org.eolang.positive-infinity
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
            φ ↦ ξ.ρ.as-bytes.eq(
              α0 ↦ ξ.x.as-bytes
            )
          ⟧,
          lt ↦ ⟦
            x ↦ ∅,
            φ ↦ Φ.org.eolang.false
          ⟧,
          lte ↦ ⟦
            x ↦ ∅,
            φ ↦ ξ.ρ.eq(
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
              α0 ↦ ξ.ρ.eq(
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
              φ ↦ ξ.ρ.is-nan(
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
                α0 ↦ Φ.org.eolang.int(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).lt(
                  α0 ↦ ξ.num
                ),
                α1 ↦ ξ.auto-named-attr-at-81-26,
                α2 ↦ Φ.org.eolang.false
              ),
              auto-named-attr-at-81-26 ↦ ⟦
                φ ↦ Φ.org.eolang.float(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).lt(
                  α0 ↦ ξ.ρ.num
                ),
                e ↦ ∅
              ⟧
            ⟧,
            φ ↦ ξ.is-nan-or-zero(
              α0 ↦ ξ.value
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.is-num-gt-zero(
                α0 ↦ ξ.value
              ).if(
                α0 ↦ ξ.ρ.ρ.positive-infinity,
                α1 ↦ Φ.org.eolang.negative-infinity
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
            φ ↦ ξ.is-nan(
              α0 ↦ ξ.value
            ).or(
              α0 ↦ ξ.value.eq(
                α0 ↦ ξ.neg-inf-as-bytes
              )
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.ρ.ρ.positive-infinity
            )
          ⟧,
          minus ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            pos-inf-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.ρ.ρ.positive-infinity
            ).as-bytes,
            is-nan ↦ ⟦
              num ↦ ∅,
              φ ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.nan.as-bytes
              )
            ⟧,
            φ ↦ ξ.is-nan(
              α0 ↦ ξ.value
            ).or(
              α0 ↦ ξ.value.eq(
                α0 ↦ ξ.pos-inf-as-bytes
              )
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.ρ.ρ.positive-infinity
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
              φ ↦ ξ.ρ.is-nan(
                α0 ↦ ξ.num
              ).or(
                α0 ↦ ξ.num.eq(
                  α0 ↦ ξ.ρ.ρ.ρ.positive-infinity
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
                α0 ↦ Φ.org.eolang.int(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).lte(
                  α0 ↦ ξ.num
                ),
                α1 ↦ ξ.auto-named-attr-at-136-27,
                α2 ↦ Φ.org.eolang.false
              ),
              auto-named-attr-at-136-27 ↦ ⟦
                φ ↦ Φ.org.eolang.float(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).lte(
                  α0 ↦ ξ.ρ.num
                ),
                e ↦ ∅
              ⟧
            ⟧,
            φ ↦ ξ.is-nan-or-infinite(
              α0 ↦ ξ.value
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.is-num-gte-zero(
                α0 ↦ ξ.value
              ).if(
                α0 ↦ ξ.ρ.ρ.positive-infinity,
                α1 ↦ Φ.org.eolang.negative-infinity
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
            φ ↦ ξ.index.eq(
              α0 ↦ ξ.ρ.len
            ).if(
              α0 ↦ Φ.org.eolang.true,
              α1 ↦ ξ.case.at(
                α0 ↦ Φ.org.eolang.int(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ ξ.case.at(
                  α0 ↦ Φ.org.eolang.int(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-01
                    )
                  )
                ),
                α1 ↦ ξ.ρ.case-at(
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
          φ ↦ ξ.len.eq(
            α0 ↦ Φ.org.eolang.int(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            )
          ).if(
            α0 ↦ Φ.org.eolang.error(
              α0 ↦ Φ.org.eolang.string(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 73-77-69-74-63-68-20-63-61-73-65-73-20-61-72-65-20-65-6D-70-74-79
                )
              )
            ),
            α1 ↦ ξ.case-at(
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

## [org/eolang/true.phi](./org/eolang/true.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        true ↦ ⟦
          φ ↦ Φ.org.eolang.bytes(
            Δ ⤍ 01-
          ),
          not ↦ Φ.org.eolang.false,
          if ↦ ⟦
            φ ↦ ξ.left,
            left ↦ ∅,
            right ↦ ∅
          ⟧,
          and ↦ ⟦
            φ ↦ Φ.org.eolang.bytes(
              Δ ⤍ 01-
            ).eq(
              α0 ↦ ξ.x
            ),
            x ↦ ∅
          ⟧,
          or ↦ ⟦
            φ ↦ ξ.ρ,
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
            length ↦ Φ.org.eolang.int(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ),
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
              φ ↦ ξ.ρ.ρ.ρ.tuple(
                α0 ↦ ξ.ρ.ρ.ρ.tuple.empty,
                α1 ↦ ξ.x
              )
            ⟧
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
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).gt(
                α0 ↦ ξ.idx
              ).if(
                α0 ↦ ξ.len.plus(
                  α0 ↦ ξ.idx
                ),
                α1 ↦ ξ.idx
              )
            ).as-bytes,
            φ ↦ Φ.org.eolang.int(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ).gt(
              α0 ↦ ξ.index
            ).or(
              α0 ↦ ξ.len.lte(
                α0 ↦ ξ.index
              )
            ).if(
              α0 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 47-69-76-65-6E-20-69-6E-64-65-78-20-69-73-20-6F-75-74-20-6F-66-20-74-75-70-6C-65-20-62-6F-75-6E-64-73
                  )
                )
              ),
              α1 ↦ ξ.at-fast(
                α0 ↦ ξ.ρ,
                α1 ↦ ξ.len
              )
            ),
            at-fast ↦ ⟦
              tup ↦ ∅,
              len ↦ ∅,
              φ ↦ ξ.len.plus(
                α0 ↦ Φ.org.eolang.int(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ FF-FF-FF-FF-FF-FF-FF-FF
                  )
                )
              ).gt(
                α0 ↦ ξ.ρ.index
              ).if(
                α0 ↦ ξ.ρ.at-fast(
                  α0 ↦ ξ.tup.head,
                  α1 ↦ ξ.len.plus(
                    α0 ↦ Φ.org.eolang.int(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ FF-FF-FF-FF-FF-FF-FF-FF
                      )
                    )
                  )
                ),
                α1 ↦ ξ.tup.tail
              )
            ⟧,
            idx ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.i
            ).as-bytes
          ⟧,
          with ↦ ⟦
            φ ↦ ξ.ρ.ρ.tuple(
              α0 ↦ ξ.ρ,
              α1 ↦ ξ.x
            ),
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

## [org/eolang/while.phi](./org/eolang/while.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        while ↦ ⟦
          condition ↦ ∅,
          body ↦ ∅,
          φ ↦ ξ.condition(
            α0 ↦ Φ.org.eolang.int(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            )
          ).as-bool.if(
            α0 ↦ ξ.loop(
              α0 ↦ Φ.org.eolang.int(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
            ),
            α1 ↦ Φ.org.eolang.false
          ),
          loop ↦ ⟦
            index ↦ ∅,
            current ↦ ξ.ρ.body(
              α0 ↦ ξ.index
            ),
            φ ↦ ξ.ρ.condition(
              α0 ↦ ξ.index.plus(
                α0 ↦ Φ.org.eolang.int(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-01
                  )
                )
              )
            ).as-bool.if(
              α0 ↦ Φ.org.eolang.seq(
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
              α1 ↦ ξ.current
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
