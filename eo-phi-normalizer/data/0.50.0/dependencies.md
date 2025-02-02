# Dependencies

## [org/eolang/bytes.phi](./org/eolang/bytes.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      bytes(data) ↦ ⟦
        φ ↦ ξ.data,
        as-bytes ↦ ξ,
        as-bool ↦ ξ.eq(Φ̇.bytes(⟦ Δ ⤍ 01- ⟧)),
        as-number() ↦ ⟦
          φ ↦ ξ.ρ.eq(Φ̇.nan.as-bytes).if(
            Φ̇.nan,
            ξ.ρ.eq(Φ̇.positive-infinity.as-bytes).if(
              Φ̇.positive-infinity,
              ξ.ρ.eq(Φ̇.negative-infinity.as-bytes).if(
                Φ̇.negative-infinity,
                ξ.ρ.size.eq(8).if(
                  Φ̇.number(ξ.ρ),
                  Φ̇.error(
                    Φ̇.txt.sprintf(
                      "Can't convert non 8 length bytes to a number, bytes are %x",
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ)
                    )
                  )
                )
              )
            )
          )
        ⟧,
        eq(b) ↦ ⟦
          λ ⤍ Lorg_eolang_bytes_eq
        ⟧,
        size() ↦ ⟦
          λ ⤍ Lorg_eolang_bytes_size
        ⟧,
        slice(start, len) ↦ ⟦
          λ ⤍ Lorg_eolang_bytes_slice
        ⟧,
        as-i64() ↦ ⟦
          φ ↦ ξ.ρ.size.eq(8).if(
            Φ̇.i64(ξ.ρ),
            Φ̇.error(
              Φ̇.txt.sprintf(
                "Can't convert non 8 length bytes to i64, bytes are %x",
                Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ)
              )
            )
          )
        ⟧,
        as-i32() ↦ ⟦
          φ ↦ ξ.ρ.size.eq(4).if(
            Φ̇.i32(ξ.ρ),
            Φ̇.error(
              Φ̇.txt.sprintf(
                "Can't convert non 4 length bytes to i32, bytes are %x",
                Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ)
              )
            )
          )
        ⟧,
        as-i16() ↦ ⟦
          φ ↦ ξ.ρ.size.eq(2).if(
            Φ̇.i16(ξ.ρ),
            Φ̇.error(
              Φ̇.txt.sprintf(
                "Can't convert non 2 length bytes to i16, bytes are %x",
                Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ)
              )
            )
          )
        ⟧,
        and(b) ↦ ⟦
          λ ⤍ Lorg_eolang_bytes_and
        ⟧,
        or(b) ↦ ⟦
          λ ⤍ Lorg_eolang_bytes_or
        ⟧,
        xor(b) ↦ ⟦
          λ ⤍ Lorg_eolang_bytes_xor
        ⟧,
        not() ↦ ⟦
          λ ⤍ Lorg_eolang_bytes_not
        ⟧,
        left(x) ↦ ⟦
          φ ↦ ξ.ρ.right(ξ.x.neg)
        ⟧,
        right(x) ↦ ⟦
          λ ⤍ Lorg_eolang_bytes_right
        ⟧,
        concat(b) ↦ ⟦
          λ ⤍ Lorg_eolang_bytes_concat
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/dataized.phi](./org/eolang/dataized.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      dataized(target) ↦ ⟦
        φ ↦ Φ̇.try(
          ξ.target,
          ⟦
            ex ↦ ∅,
            φ ↦ Φ̇.error(ξ.ex)
          ⟧,
          Φ̇.bytes(⟦ Δ ⤍ 01- ⟧)
        )
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/error.phi](./org/eolang/error.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      error(message) ↦ ⟦
        λ ⤍ Lorg_eolang_error
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/false.phi](./org/eolang/false.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      false() ↦ ⟦
        φ ↦ Φ̇.bytes(⟦ Δ ⤍ 00- ⟧),
        not ↦ Φ̇.true,
        if(left, right) ↦ ⟦
          φ ↦ ξ.right
        ⟧,
        and(x) ↦ ⟦
          φ ↦ ξ.ρ
        ⟧,
        or(x) ↦ ⟦
          φ ↦ Φ̇.bytes(⟦ Δ ⤍ 01- ⟧).eq(ξ.x)
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/i16.phi](./org/eolang/i16.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      i16(as-bytes) ↦ ⟦
        φ ↦ ξ.as-bytes,
        as-i16 ↦ ξ,
        neg ↦ ξ.times(-1.as-i64.as-i32.as-i16),
        as-i64 ↦ ξ.as-i32.as-i64,
        as-number ↦ ξ.as-i64.as-number,
        as-i32() ↦ ⟦
          λ ⤍ Lorg_eolang_i16_as_i32
        ⟧,
        lt(x) ↦ ⟦
          φ ↦ ξ.ρ.as-i32.lt(ξ.x.as-i16.as-i32)
        ⟧,
        lte(x) ↦ ⟦
          φ ↦ ξ.ρ.as-i32.lte(ξ.x.as-i16.as-i32)
        ⟧,
        gt(x) ↦ ⟦
          φ ↦ ξ.ρ.as-i32.gt(ξ.x.as-i16.as-i32)
        ⟧,
        gte(x) ↦ ⟦
          φ ↦ ξ.ρ.as-i32.gte(ξ.x.as-i16.as-i32)
        ⟧,
        times(x) ↦ ⟦
          bts ↦ ξ.ρ.as-i32.times(ξ.x.as-i16.as-i32).as-bytes,
          left ↦ ξ.bts.slice(0, 2),
          right ↦ ξ.bts.slice(2, 2),
          φ ↦ ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ 00-00 ⟧)).or(
            ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ FF-FF ⟧))
          ).if(ξ.ρ.ρ.i16(ξ.right), ξ.ρ.ρ.i16(ξ.left).plus(ξ.ρ.ρ.i16(ξ.right)))
        ⟧,
        plus(x) ↦ ⟦
          bts ↦ ξ.ρ.as-i32.plus(ξ.x.as-i16.as-i32).as-bytes,
          left ↦ ξ.bts.slice(0, 2),
          right ↦ ξ.bts.slice(2, 2),
          φ ↦ ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ 00-00 ⟧)).or(
            ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ FF-FF ⟧))
          ).if(ξ.ρ.ρ.i16(ξ.right), ξ.ρ.ρ.i16(ξ.left).plus(ξ.ρ.ρ.i16(ξ.right)))
        ⟧,
        minus(x) ↦ ⟦
          φ ↦ ξ.ρ.plus(ξ.x.as-i16.neg)
        ⟧,
        div(x) ↦ ⟦
          x-as-i16 ↦ ξ.x.as-i16,
          bts ↦ ξ.ρ.as-i32.div(ξ.x-as-i16.as-i32).as-bytes,
          left ↦ ξ.bts.slice(0, 2),
          right ↦ ξ.bts.slice(2, 2),
          zero ↦ Φ̇.bytes(⟦ Δ ⤍ 00-00 ⟧),
          φ ↦ ξ.x-as-i16.eq(ξ.zero).if(
            Φ̇.error(
              Φ̇.txt.sprintf(
                "Can't divide %d by i16 zero",
                Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.as-i32.as-i64.as-number)
              )
            ),
            ξ.left.eq(ξ.zero).or(ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ FF-FF ⟧))).if(
              ξ.ρ.ρ.i16(ξ.right), ξ.ρ.ρ.i16(ξ.left).plus(ξ.ρ.ρ.i16(ξ.right))
            )
          )
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/i32.phi](./org/eolang/i32.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      i32(as-bytes) ↦ ⟦
        φ ↦ ξ.as-bytes,
        as-i32 ↦ ξ,
        neg ↦ ξ.times(-1.as-i64.as-i32),
        as-number ↦ ξ.as-i64.as-number,
        as-i64() ↦ ⟦
          λ ⤍ Lorg_eolang_i32_as_i64
        ⟧,
        as-i16() ↦ ⟦
          left ↦ ξ.ρ.as-bytes.slice(0, 2).as-bytes,
          φ ↦ ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ 00-00 ⟧)).or(
            ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ FF-FF ⟧))
          ).if(
            Φ̇.i16(ξ.ρ.as-bytes.slice(2, 2)),
            Φ̇.error(
              Φ̇.txt.sprintf(
                "Can't convert i32 number %d to i16 because it's out of i16 bounds",
                Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.as-i64.as-number)
              )
            )
          )
        ⟧,
        lt(x) ↦ ⟦
          φ ↦ ξ.ρ.as-i64.lt(ξ.x.as-i32.as-i64)
        ⟧,
        lte(x) ↦ ⟦
          φ ↦ ξ.ρ.as-i64.lte(ξ.x.as-i32.as-i64)
        ⟧,
        gt(x) ↦ ⟦
          φ ↦ ξ.ρ.as-i64.gt(ξ.x.as-i32.as-i64)
        ⟧,
        gte(x) ↦ ⟦
          φ ↦ ξ.ρ.as-i64.gte(ξ.x.as-i32.as-i64)
        ⟧,
        times(x) ↦ ⟦
          bts ↦ ξ.ρ.as-i64.times(ξ.x.as-i32.as-i64).as-bytes,
          left ↦ ξ.bts.slice(0, 4),
          right ↦ ξ.bts.slice(4, 4),
          φ ↦ ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ 00-00-00-00 ⟧)).or(
            ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ FF-FF-FF-FF ⟧))
          ).if(ξ.ρ.ρ.i32(ξ.right), ξ.ρ.ρ.i32(ξ.left).plus(ξ.ρ.ρ.i32(ξ.right)))
        ⟧,
        plus(x) ↦ ⟦
          bts ↦ ξ.ρ.as-i64.plus(ξ.x.as-i32.as-i64).as-bytes,
          left ↦ ξ.bts.slice(0, 4),
          right ↦ ξ.bts.slice(4, 4),
          φ ↦ ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ 00-00-00-00 ⟧)).or(
            ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ FF-FF-FF-FF ⟧))
          ).if(ξ.ρ.ρ.i32(ξ.right), ξ.ρ.ρ.i32(ξ.left).plus(ξ.ρ.ρ.i32(ξ.right)))
        ⟧,
        minus(x) ↦ ⟦
          φ ↦ ξ.ρ.plus(ξ.x.as-i32.neg)
        ⟧,
        div(x) ↦ ⟦
          x-as-i32 ↦ ξ.x.as-i32,
          bts ↦ ξ.ρ.as-i64.div(ξ.x-as-i32.as-i64).as-bytes,
          left ↦ ξ.bts.slice(0, 4),
          right ↦ ξ.bts.slice(4, 4),
          zero ↦ Φ̇.bytes(⟦ Δ ⤍ 00-00-00-00 ⟧),
          φ ↦ ξ.x-as-i32.eq(ξ.zero).if(
            Φ̇.error(
              Φ̇.txt.sprintf(
                "Can't divide %d by i32 zero", Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.as-i64.as-number)
              )
            ),
            ξ.left.eq(ξ.zero).or(ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ FF-FF-FF-FF ⟧))).if(
              ξ.ρ.ρ.i32(ξ.right), ξ.ρ.ρ.i32(ξ.left).plus(ξ.ρ.ρ.i32(ξ.right))
            )
          )
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/i64.phi](./org/eolang/i64.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      i64(as-bytes) ↦ ⟦
        φ ↦ ξ.as-bytes,
        as-i64 ↦ ξ,
        neg ↦ ξ.times(-1.as-i64),
        as-i16 ↦ ξ.as-i32.as-i16,
        as-i32() ↦ ⟦
          left ↦ ξ.ρ.as-bytes.slice(0, 4).as-bytes,
          φ ↦ ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ 00-00-00-00 ⟧)).or(
            ξ.left.eq(Φ̇.bytes(⟦ Δ ⤍ FF-FF-FF-FF ⟧))
          ).if(
            Φ̇.i32(ξ.ρ.as-bytes.slice(4, 4)),
            Φ̇.error(
              Φ̇.txt.sprintf(
                "Can't convert i64 number %d to i32 because it's out of i32 bounds",
                Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.as-number)
              )
            )
          )
        ⟧,
        as-number() ↦ ⟦
          λ ⤍ Lorg_eolang_i64_as_number
        ⟧,
        lt(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ 0.as-i64.gt(ξ.ρ.minus(ξ.ρ.ρ.i64(ξ.value)))
        ⟧,
        lte(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ ξ.ρ.lt(ξ.value).or(ξ.ρ.eq(ξ.value))
        ⟧,
        gt(x) ↦ ⟦
          λ ⤍ Lorg_eolang_i64_gt
        ⟧,
        gte(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ ξ.ρ.gt(ξ.value).or(ξ.ρ.eq(ξ.value))
        ⟧,
        times(x) ↦ ⟦
          λ ⤍ Lorg_eolang_i64_times
        ⟧,
        plus(x) ↦ ⟦
          λ ⤍ Lorg_eolang_i64_plus
        ⟧,
        minus(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ ξ.ρ.plus(ξ.ρ.ρ.i64(ξ.value).neg)
        ⟧,
        div(x) ↦ ⟦
          λ ⤍ Lorg_eolang_i64_div
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/io/dead-input.phi](./org/eolang/io/dead-input.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      io() ↦ ⟦
        dead-input() ↦ ⟦
          read(size) ↦ ⟦
            φ ↦ ξ.input-block,
            input-block() ↦ ⟦
              φ ↦ Φ̇.bytes(⟦ Δ ⤍ -- ⟧),
              read(size) ↦ ⟦
                φ ↦ ξ.ρ.ρ.input-block
              ⟧
            ⟧
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/io/dead-output.phi](./org/eolang/io/dead-output.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      io() ↦ ⟦
        dead-output() ↦ ⟦
          write(buffer) ↦ ⟦
            φ ↦ ξ.output-block,
            output-block() ↦ ⟦
              φ ↦ Φ̇.true,
              write(buffer) ↦ ⟦
                φ ↦ ξ.ρ.ρ.output-block
              ⟧
            ⟧
          ⟧
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/nan.phi](./org/eolang/nan.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      nan() ↦ ⟦
        φ ↦ Φ̇.number(Φ̇.bytes(⟦ Δ ⤍ 7F-F8-00-00-00-00-00-00 ⟧)),
        floor ↦ ξ,
        neg ↦ ξ,
        is-nan ↦ Φ̇.true,
        is-finite ↦ Φ̇.false,
        is-integer ↦ Φ̇.false,
        as-i64 ↦ Φ̇.error("Can't convert NaN to i64"),
        eq(x) ↦ ⟦
          φ ↦ Φ̇.false
        ⟧,
        lt(x) ↦ ⟦
          φ ↦ Φ̇.false
        ⟧,
        lte(x) ↦ ⟦
          φ ↦ Φ̇.false
        ⟧,
        gt(x) ↦ ⟦
          φ ↦ Φ̇.false
        ⟧,
        gte(x) ↦ ⟦
          φ ↦ Φ̇.false
        ⟧,
        times(x) ↦ ⟦
          φ ↦ ξ.ρ
        ⟧,
        plus(x) ↦ ⟦
          φ ↦ ξ.ρ
        ⟧,
        minus(x) ↦ ⟦
          φ ↦ ξ.ρ
        ⟧,
        div(x) ↦ ⟦
          φ ↦ ξ.ρ
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/negative-infinity.phi](./org/eolang/negative-infinity.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      negative-infinity() ↦ ⟦
        φ ↦ Φ̇.number(Φ̇.bytes(⟦ Δ ⤍ FF-F0-00-00-00-00-00-00 ⟧)),
        floor ↦ ξ,
        neg ↦ Φ̇.positive-infinity,
        is-nan ↦ Φ̇.false,
        is-finite ↦ Φ̇.false,
        is-integer ↦ Φ̇.false,
        as-i64 ↦ Φ̇.error("Can't convert negative infinity to i64"),
        eq(x) ↦ ⟦
          φ ↦ ξ.ρ.as-bytes.eq(ξ.x.as-bytes)
        ⟧,
        lt(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ Φ̇.number(ξ.value).is-nan.or(ξ.ρ.eq(ξ.value)).not
        ⟧,
        lte(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ Φ̇.number(ξ.value).is-nan.not
        ⟧,
        gt(x) ↦ ⟦
          φ ↦ Φ̇.false
        ⟧,
        gte(x) ↦ ⟦
          φ ↦ ξ.ρ.eq(ξ.x)
        ⟧,
        times(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          num ↦ Φ̇.number(ξ.value),
          φ ↦ ξ.num.is-nan.or(ξ.num.eq(0)).if(
            Φ̇.nan, ξ.num.gt(0).if(ξ.ρ, Φ̇.positive-infinity)
          )
        ⟧,
        plus(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ Φ̇.number(ξ.value).is-nan.or(ξ.value.eq(Φ̇.positive-infinity)).if(
            Φ̇.nan, ξ.ρ
          )
        ⟧,
        minus(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ Φ̇.number(ξ.value).is-nan.or(ξ.value.eq(ξ.ρ)).if(Φ̇.nan, ξ.ρ)
        ⟧,
        div(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          num ↦ Φ̇.number(ξ.value),
          φ ↦ ξ.num.is-nan.or(ξ.num.is-finite.not).if(
            Φ̇.nan,
            ξ.value.eq(-0.as-bytes).or(0.gt(ξ.value)).if(
              Φ̇.positive-infinity, ξ.ρ
            )
          )
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/number.phi](./org/eolang/number.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      number(as-bytes) ↦ ⟦
        φ ↦ ξ.as-bytes,
        as-number ↦ ξ,
        neg ↦ ξ.times(-1),
        as-i32 ↦ ξ.as-i64.as-i32,
        as-i16 ↦ ξ.as-i32.as-i16,
        is-nan ↦ ξ.as-bytes.eq(Φ̇.nan.as-bytes),
        as-i64() ↦ ⟦
          λ ⤍ Lorg_eolang_number_as_i64
        ⟧,
        eq(x) ↦ ⟦
          x-as-bytes ↦ Φ̇.dataized(ξ.x).as-bytes,
          self-as-bytes ↦ ξ.ρ.as-bytes,
          pos-zero-as-bytes ↦ 0.as-bytes,
          neg-zero-as-bytes ↦ -0.as-bytes,
          φ ↦ ξ.ρ.is-nan.or(Φ̇.number(ξ.x-as-bytes).is-nan).if(
            Φ̇.false,
            ξ.x-as-bytes.eq(ξ.pos-zero-as-bytes).or(
              ξ.x-as-bytes.eq(ξ.neg-zero-as-bytes)
            ).and(
              ξ.self-as-bytes.eq(ξ.pos-zero-as-bytes).or(
                ξ.self-as-bytes.eq(ξ.neg-zero-as-bytes)
              )
            ).or(ξ.self-as-bytes.eq(ξ.x-as-bytes))
          )
        ⟧,
        lt(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ 0.gt(ξ.ρ.minus(Φ̇.number(ξ.value)))
        ⟧,
        lte(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ ξ.ρ.lt(ξ.value).or(ξ.ρ.eq(ξ.value))
        ⟧,
        gt(x) ↦ ⟦
          λ ⤍ Lorg_eolang_number_gt
        ⟧,
        gte(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ ξ.ρ.gt(ξ.value).or(ξ.ρ.eq(ξ.value))
        ⟧,
        times(x) ↦ ⟦
          λ ⤍ Lorg_eolang_number_times
        ⟧,
        plus(x) ↦ ⟦
          λ ⤍ Lorg_eolang_number_plus
        ⟧,
        minus(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ ξ.ρ.plus(Φ̇.number(ξ.value).neg)
        ⟧,
        div(x) ↦ ⟦
          λ ⤍ Lorg_eolang_number_div
        ⟧,
        floor() ↦ ⟦
          λ ⤍ Lorg_eolang_number_floor
        ⟧,
        is-integer() ↦ ⟦
          φ ↦ ξ.ρ.is-finite.and(ξ.ρ.eq(ξ.ρ.floor))
        ⟧,
        is-finite() ↦ ⟦
          φ ↦ ξ.ρ.is-nan.not.and(
            ξ.ρ.eq(Φ̇.positive-infinity).or(ξ.ρ.eq(Φ̇.negative-infinity)).not
          )
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/positive-infinity.phi](./org/eolang/positive-infinity.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      positive-infinity() ↦ ⟦
        φ ↦ Φ̇.number(Φ̇.bytes(⟦ Δ ⤍ 7F-F0-00-00-00-00-00-00 ⟧)),
        floor ↦ ξ,
        neg ↦ Φ̇.negative-infinity,
        is-nan ↦ Φ̇.false,
        is-finite ↦ Φ̇.false,
        is-integer ↦ Φ̇.false,
        as-i64 ↦ Φ̇.error("Can't convert positive infinity to i64"),
        eq(x) ↦ ⟦
          φ ↦ ξ.ρ.as-bytes.eq(ξ.x.as-bytes)
        ⟧,
        lt(x) ↦ ⟦
          φ ↦ Φ̇.false
        ⟧,
        lte(x) ↦ ⟦
          φ ↦ ξ.ρ.eq(ξ.x)
        ⟧,
        gt(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ Φ̇.number(ξ.value).is-nan.or(ξ.ρ.eq(ξ.value)).not
        ⟧,
        gte(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ Φ̇.number(ξ.value).is-nan.not
        ⟧,
        times(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          num ↦ Φ̇.number(ξ.value),
          φ ↦ ξ.num.is-nan.or(ξ.num.eq(0)).if(
            Φ̇.nan, ξ.num.gt(0).if(ξ.ρ, Φ̇.negative-infinity)
          )
        ⟧,
        plus(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ Φ̇.number(ξ.value).is-nan.or(ξ.value.eq(Φ̇.negative-infinity)).if(
            Φ̇.nan, ξ.ρ
          )
        ⟧,
        minus(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          φ ↦ Φ̇.number(ξ.value).is-nan.or(ξ.value.eq(ξ.ρ)).if(Φ̇.nan, ξ.ρ)
        ⟧,
        div(x) ↦ ⟦
          value ↦ Φ̇.dataized(ξ.x).as-bytes,
          num ↦ Φ̇.number(ξ.value),
          φ ↦ ξ.num.is-nan.or(ξ.num.is-finite.not).if(
            Φ̇.nan,
            ξ.value.eq(-0.as-bytes).or(0.gt(ξ.value)).if(
              Φ̇.negative-infinity, ξ.ρ
            )
          )
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/seq.phi](./org/eolang/seq.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      seq(steps) ↦ ⟦
        φ ↦ ξ.steps.length.eq(0).if(Φ̇.true, ξ.loop(0)),
        max-len ↦ Φ̇.dataized(ξ.steps.length.minus(1)).as-bytes,
        loop(index) ↦ ⟦
          φ ↦ ξ.index.lt(ξ.ρ.max-len).and(
            Φ̇.dataized(ξ.ρ.steps.at(ξ.index)).as-bool.or(Φ̇.true)
          ).if(ξ.ρ.loop(ξ.index.plus(1)), ξ.ρ.steps.at(ξ.index))
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/string.phi](./org/eolang/string.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      string(as-bytes) ↦ ⟦
        φ ↦ ξ.as-bytes,
        length() ↦ ⟦
          size ↦ ξ.ρ.as-bytes.size,
          pattern-one ↦ Φ̇.bytes(⟦ Δ ⤍ 80- ⟧),
          pattern-two ↦ Φ̇.bytes(⟦ Δ ⤍ E0- ⟧),
          pattern-three ↦ Φ̇.bytes(⟦ Δ ⤍ F0- ⟧),
          pattern-four ↦ Φ̇.bytes(⟦ Δ ⤍ F8- ⟧),
          result-one ↦ Φ̇.bytes(⟦ Δ ⤍ 00- ⟧),
          result-two ↦ Φ̇.bytes(⟦ Δ ⤍ C0- ⟧),
          result-three ↦ ξ.pattern-two,
          result-four ↦ ξ.pattern-three,
          φ ↦ ξ.size.eq(0).if(0, ξ.rec-length(0, 0)),
          increase-length(index, char-size, len) ↦ ⟦
            φ ↦ ξ.index.plus(ξ.char-size).gt(ξ.ρ.size).if(
              Φ̇.error(
                Φ̇.txt.sprintf(
                  "Expected %d byte character at %d index, but there are not enough bytes for it: %x",
                  Φ̇.tuple(
                    Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.char-size), ξ.index), ξ.ρ.ρ.as-bytes
                  )
                )
              ),
              ξ.ρ.rec-length(ξ.index.plus(ξ.char-size), ξ.len.plus(1))
            )
          ⟧,
          rec-length(index, accum) ↦ ⟦
            byte ↦ ξ.ρ.ρ.as-bytes.slice(ξ.index, 1),
            φ ↦ ξ.index.eq(ξ.ρ.size).if(
              ξ.accum,
              ξ.byte.and(ξ.ρ.pattern-one).eq(ξ.ρ.result-one).if(
                ξ.ρ.increase-length(ξ.index, 1, ξ.accum),
                ξ.byte.and(ξ.ρ.pattern-two).eq(ξ.ρ.result-two).if(
                  ξ.ρ.increase-length(ξ.index, 2, ξ.accum),
                  ξ.byte.and(ξ.ρ.pattern-three).eq(ξ.ρ.result-three).if(
                    ξ.ρ.increase-length(ξ.index, 3, ξ.accum),
                    ξ.byte.and(ξ.ρ.pattern-four).eq(ξ.ρ.result-four).if(
                      ξ.ρ.increase-length(ξ.index, 4, ξ.accum),
                      Φ̇.error(
                        Φ̇.txt.sprintf(
                          "Unknown byte format (%x), can't recognize character",
                          Φ̇.tuple(Φ̇.tuple.empty, ξ.byte)
                        )
                      )
                    )
                  )
                )
              )
            )
          ⟧
        ⟧,
        slice(start, len) ↦ ⟦
          start-bytes ↦ Φ̇.dataized(ξ.start).as-bytes,
          len-bytes ↦ Φ̇.dataized(ξ.len).as-bytes,
          num-start ↦ Φ̇.number(ξ.start-bytes),
          num-len ↦ Φ̇.number(ξ.len-bytes),
          size ↦ ξ.ρ.as-bytes.size,
          end ↦ ξ.num-start.plus(ξ.num-len),
          pattern-one ↦ Φ̇.bytes(⟦ Δ ⤍ 80- ⟧),
          pattern-two ↦ Φ̇.bytes(⟦ Δ ⤍ E0- ⟧),
          pattern-three ↦ Φ̇.bytes(⟦ Δ ⤍ F0- ⟧),
          pattern-four ↦ Φ̇.bytes(⟦ Δ ⤍ F8- ⟧),
          result-one ↦ Φ̇.bytes(⟦ Δ ⤍ 00- ⟧),
          result-two ↦ Φ̇.bytes(⟦ Δ ⤍ C0- ⟧),
          result-three ↦ ξ.pattern-two,
          result-four ↦ ξ.pattern-three,
          bts-start ↦ Φ̇.dataized(
            ξ.rec-index(
              0,
              0,
              ξ.num-start,
              Φ̇.txt.sprintf(
                "Start index (%d) is out of string bounds",
                Φ̇.tuple(Φ̇.tuple.empty, ξ.num-start)
              )
            )
          ).as-bytes,
          bts-length ↦ ξ.rec-index(
            Φ̇.number(ξ.bts-start),
            0,
            ξ.num-len,
            Φ̇.txt.sprintf(
              "Start index + length to slice (%d) is out of string bounds",
              Φ̇.tuple(Φ̇.tuple.empty, ξ.num-start.plus(ξ.num-len))
            )
          ).minus(ξ.bts-start),
          φ ↦ ξ.num-start.lt(0).if(
            Φ̇.error(
              Φ̇.txt.sprintf(
                "Start index must be >= 0, but was %d", Φ̇.tuple(Φ̇.tuple.empty, ξ.num-start)
              )
            ),
            ξ.num-len.lt(0).if(
              Φ̇.error(
                Φ̇.txt.sprintf(
                  "Length to slice must be >= 0, but was %d", Φ̇.tuple(Φ̇.tuple.empty, ξ.num-len)
                )
              ),
              ξ.num-len.eq(0).if(
                "", Φ̇.string(ξ.ρ.as-bytes.slice(ξ.bts-start, ξ.bts-length))
              )
            )
          ),
          increase(index, char-size, accum, result, cause) ↦ ⟦
            φ ↦ ξ.index.plus(ξ.char-size).gt(ξ.ρ.size).if(
              Φ̇.error(
                Φ̇.txt.sprintf(
                  "Expected %d byte character at %d index, but there are not enough bytes for it: %x",
                  Φ̇.tuple(
                    Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.char-size), ξ.index), ξ.ρ.ρ.as-bytes
                  )
                )
              ),
              ξ.ρ.rec-index(
                ξ.index.plus(ξ.char-size), ξ.accum.plus(1), ξ.result, ξ.cause
              )
            )
          ⟧,
          rec-index(index, accum, result, cause) ↦ ⟦
            byte ↦ ξ.ρ.ρ.as-bytes.slice(ξ.index, 1),
            φ ↦ ξ.accum.eq(ξ.result).if(
              ξ.index,
              ξ.index.eq(ξ.ρ.size).if(
                Φ̇.error(ξ.cause),
                ξ.byte.and(ξ.ρ.pattern-one).eq(ξ.ρ.result-one).if(
                  ξ.ρ.increase(ξ.index, 1, ξ.accum, ξ.result, ξ.cause),
                  ξ.byte.and(ξ.ρ.pattern-two).eq(ξ.ρ.result-two).if(
                    ξ.ρ.increase(ξ.index, 2, ξ.accum, ξ.result, ξ.cause),
                    ξ.byte.and(ξ.ρ.pattern-three).eq(ξ.ρ.result-three).if(
                      ξ.ρ.increase(ξ.index, 3, ξ.accum, ξ.result, ξ.cause),
                      ξ.byte.and(ξ.ρ.pattern-four).eq(ξ.ρ.result-four).if(
                        ξ.ρ.increase(ξ.index, 4, ξ.accum, ξ.result, ξ.cause),
                        Φ̇.error(
                          Φ̇.txt.sprintf(
                            "Unknown byte format (%x), can't recognize character",
                            Φ̇.tuple(Φ̇.tuple.empty, ξ.byte)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ⟧
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/true.phi](./org/eolang/true.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      true() ↦ ⟦
        φ ↦ Φ̇.bytes(⟦ Δ ⤍ 01- ⟧),
        not ↦ Φ̇.false,
        if(left, right) ↦ ⟦
          φ ↦ ξ.left
        ⟧,
        and(x) ↦ ⟦
          φ ↦ Φ̇.bytes(⟦ Δ ⤍ 01- ⟧).eq(ξ.x)
        ⟧,
        or(x) ↦ ⟦
          φ ↦ ξ.ρ
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/try.phi](./org/eolang/try.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      try(main, catch, finally) ↦ ⟦
        λ ⤍ Lorg_eolang_try
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/tuple.phi](./org/eolang/tuple.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      tuple(head, tail) ↦ ⟦
        empty() ↦ ⟦
          length ↦ 0,
          at(i) ↦ ⟦
            φ ↦ Φ̇.error("Can't get an object from the empty tuple")
          ⟧,
          with(x) ↦ ⟦
            φ ↦ ξ.ρ.ρ.ρ.tuple(ξ.ρ, ξ.x)
          ⟧
        ⟧,
        length() ↦ ⟦
          len ↦ Φ̇.dataized(ξ.ρ.head.length.plus(1)).as-bytes,
          φ ↦ Φ̇.number(ξ.len)
        ⟧,
        at(i) ↦ ⟦
          len ↦ ξ.ρ.length,
          idx ↦ Φ̇.dataized(ξ.i).as-bytes,
          index ↦ Φ̇.dataized(0.gt(ξ.idx).if(ξ.len.plus(ξ.idx), ξ.idx)).as-bytes,
          φ ↦ 0.gt(ξ.index).or(ξ.len.lte(ξ.index)).if(
            Φ̇.error("Given index is out of tuple bounds"), ξ.at-fast(ξ.ρ, ξ.len)
          ),
          at-fast(tup, len) ↦ ⟦
            φ ↦ ξ.len.plus(-1).gt(ξ.ρ.index).if(
              ξ.ρ.at-fast(ξ.tup.head, ξ.len.plus(-1)), ξ.tup.tail
            )
          ⟧
        ⟧,
        with(x) ↦ ⟦
          φ ↦ ξ.ρ.ρ.tuple(ξ.ρ, ξ.x)
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/txt/sprintf.phi](./org/eolang/txt/sprintf.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      txt() ↦ ⟦
        sprintf(format, args) ↦ ⟦
          λ ⤍ Lorg_eolang_txt_sprintf
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/while.phi](./org/eolang/while.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      while(condition, body) ↦ ⟦
        φ ↦ ξ.condition(0).as-bool.if(ξ.loop(0), Φ̇.false),
        loop(index) ↦ ⟦
          current ↦ ξ.ρ.body(ξ.index),
          φ ↦ ξ.ρ.condition(ξ.index.plus(1)).as-bool.if(
            Φ̇.seq(
              Φ̇.tuple(
                Φ̇.tuple(Φ̇.tuple.empty, ξ.current), ξ.ρ.loop(ξ.index.plus(1))
              )
            ),
            ξ.current
          )
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```
