# Quick start

{{#include common/enter-repository.md}}

Install `eo-phi-normalizer` - see [Installation](./installation.md).

{{#include common/celsius.md}}

Dataize the program recursively.

```$ as console
eo-phi-normalizer dataize \
  --recursive \
  --rules eo-phi-normalizer/test/eo/phi/rules/yegor.yaml \
  --dependency-file 'eo-phi-normalizer/data/0.38.0/org/eolang/float.phi' \
  --dependency-file 'eo-phi-normalizer/data/0.38.0/org/eolang/bytes.phi' \
  celsius.phi
```

```console
{
  ⟦
    c ↦ 25.0,
    result ↦ ⟦
      x ↦ 1.8,
      λ ⤍ Lorg_eolang_float_times,
      ρ ↦ ⟦
        times ↦ ⟦
          λ ⤍ Lorg_eolang_float_times,
          x ↦ ∅
        ⟧,
        as-bytes ↦ ⟦
          float ↦ ⟦
            as-bytes ↦ ∅,
            φ ↦ ξ.as-bytes,
            eq ↦ ⟦
              x ↦ ∅,
              x-as-bytes ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.x
              )
              .as-bytes,
              self-as-bytes ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.ρ
              )
              .as-bytes,
              nan-as-bytes ↦ Φ.org.eolang.dataized (
                α0 ↦ Φ.org.eolang.nan
              )
              .as-bytes,
              pos-zero-as-bytes ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.ρ.ρ.float (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              )
              .as-bytes,
              neg-zero-as-bytes ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.ρ.ρ.float (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 80-00-00-00-00-00-00-00
                  )
                )
              )
              .as-bytes,
              φ ↦ ξ.x-as-bytes.eq (
                α0 ↦ ξ.nan-as-bytes
              )
              .or (
                α0 ↦ ξ.self-as-bytes.eq (
                  α0 ↦ ξ.nan-as-bytes
                )
              )
              .if (
                α0 ↦ Φ.org.eolang.false,
                α1 ↦ ξ.x-as-bytes.eq (
                  α0 ↦ ξ.pos-zero-as-bytes
                )
                .or (
                  α0 ↦ ξ.x-as-bytes.eq (
                    α0 ↦ ξ.neg-zero-as-bytes
                  )
                )
                .and (
                  α0 ↦ ξ.self-as-bytes.eq (
                    α0 ↦ ξ.pos-zero-as-bytes
                  )
                  .or (
                    α0 ↦ ξ.self-as-bytes.eq (
                      α0 ↦ ξ.neg-zero-as-bytes
                    )
                  )
                )
                .or (
                  α0 ↦ ξ.self-as-bytes.eq (
                    α0 ↦ ξ.x-as-bytes
                  )
                )
              )
            ⟧,
            lt ↦ ⟦
              x ↦ ∅,
              φ ↦ ξ.ρ.ρ.float (
                α0 ↦ Φ.org.eolang.bytes (
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
              .gt (
                α0 ↦ ξ.ρ.minus (
                  α0 ↦ ξ.ρ.ρ.float (
                    α0 ↦ ξ.value
                  )
                )
              ),
              value ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.x
              )
              .as-bytes
            ⟧,
            lte ↦ ⟦
              x ↦ ∅,
              value ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.x
              )
              .as-bytes,
              φ ↦ ξ.ρ.eq (
                α0 ↦ ξ.value
              )
              .or (
                α0 ↦ ξ.ρ.lt (
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
              value ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.x
              )
              .as-bytes,
              φ ↦ ξ.ρ.eq (
                α0 ↦ ξ.value
              )
              .or (
                α0 ↦ ξ.ρ.gt (
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
              φ ↦ ξ.ρ.times (
                α0 ↦ ξ.ρ.ρ.float (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ BF-F0-00-00-00-00-00-00
                  )
                )
              )
            ⟧,
            minus ↦ ⟦
              φ ↦ ξ.ρ.plus (
                α0 ↦ ξ.x.neg
              ),
              x ↦ ∅
            ⟧,
            div ↦ ⟦
              λ ⤍ Lorg_eolang_float_div,
              x ↦ ∅
            ⟧
          ⟧,
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
              φ ↦ Φ.org.eolang.string (
                α0 ↦ ξ.ρ
              )
            ⟧,
            as-int ↦ ⟦
              φ ↦ ξ.ρ.size.eq (
                α0 ↦ Φ.org.eolang.int (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 00-00-00-00-00-00-00-08
                  )
                )
              )
              .if (
                α0 ↦ Φ.org.eolang.int (
                  α0 ↦ ξ.ρ
                ),
                α1 ↦ Φ.org.eolang.error (
                  α0 ↦ Φ.org.eolang.string (
                    α0 ↦ Φ.org.eolang.bytes (
                      Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-6E-74
                    )
                  )
                )
              )
            ⟧,
            as-float ↦ ⟦
              φ ↦ ξ.ρ.size.eq (
                α0 ↦ Φ.org.eolang.int (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 00-00-00-00-00-00-00-08
                  )
                )
              )
              .if (
                α0 ↦ Φ.org.eolang.float (
                  α0 ↦ ξ.ρ
                ),
                α1 ↦ Φ.org.eolang.error (
                  α0 ↦ Φ.org.eolang.string (
                    α0 ↦ Φ.org.eolang.bytes (
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
              φ ↦ ξ.ρ.right (
                α0 ↦ ξ.x.neg
              ),
              x ↦ ∅
            ⟧,
            right ↦ ⟦
              λ ⤍ Lorg_eolang_bytes_right,
              x ↦ ∅
            ⟧,
            as-bool ↦ ⟦
              φ ↦ ξ.ρ.eq (
                α0 ↦ Φ.org.eolang.bytes (
                  Δ ⤍ 01-
                )
              )
            ⟧,
            concat ↦ ⟦
              λ ⤍ Lorg_eolang_bytes_concat,
              b ↦ ∅
            ⟧
          ⟧,
          λ ⤍ Package,
          ρ ↦ ⟦
            eolang ↦ ⟦
              float ↦ ⟦
                as-bytes ↦ ∅,
                φ ↦ ξ.as-bytes,
                eq ↦ ⟦
                  x ↦ ∅,
                  x-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes,
                  self-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.ρ
                  )
                  .as-bytes,
                  nan-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ Φ.org.eolang.nan
                  )
                  .as-bytes,
                  pos-zero-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.ρ.ρ.float (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  )
                  .as-bytes,
                  neg-zero-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.ρ.ρ.float (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 80-00-00-00-00-00-00-00
                      )
                    )
                  )
                  .as-bytes,
                  φ ↦ ξ.x-as-bytes.eq (
                    α0 ↦ ξ.nan-as-bytes
                  )
                  .or (
                    α0 ↦ ξ.self-as-bytes.eq (
                      α0 ↦ ξ.nan-as-bytes
                    )
                  )
                  .if (
                    α0 ↦ Φ.org.eolang.false,
                    α1 ↦ ξ.x-as-bytes.eq (
                      α0 ↦ ξ.pos-zero-as-bytes
                    )
                    .or (
                      α0 ↦ ξ.x-as-bytes.eq (
                        α0 ↦ ξ.neg-zero-as-bytes
                      )
                    )
                    .and (
                      α0 ↦ ξ.self-as-bytes.eq (
                        α0 ↦ ξ.pos-zero-as-bytes
                      )
                      .or (
                        α0 ↦ ξ.self-as-bytes.eq (
                          α0 ↦ ξ.neg-zero-as-bytes
                        )
                      )
                    )
                    .or (
                      α0 ↦ ξ.self-as-bytes.eq (
                        α0 ↦ ξ.x-as-bytes
                      )
                    )
                  )
                ⟧,
                lt ↦ ⟦
                  x ↦ ∅,
                  φ ↦ ξ.ρ.ρ.float (
                    α0 ↦ Φ.org.eolang.bytes (
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                  .gt (
                    α0 ↦ ξ.ρ.minus (
                      α0 ↦ ξ.ρ.ρ.float (
                        α0 ↦ ξ.value
                      )
                    )
                  ),
                  value ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes
                ⟧,
                lte ↦ ⟦
                  x ↦ ∅,
                  value ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes,
                  φ ↦ ξ.ρ.eq (
                    α0 ↦ ξ.value
                  )
                  .or (
                    α0 ↦ ξ.ρ.lt (
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
                  value ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes,
                  φ ↦ ξ.ρ.eq (
                    α0 ↦ ξ.value
                  )
                  .or (
                    α0 ↦ ξ.ρ.gt (
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
                  φ ↦ ξ.ρ.times (
                    α0 ↦ ξ.ρ.ρ.float (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  )
                ⟧,
                minus ↦ ⟦
                  φ ↦ ξ.ρ.plus (
                    α0 ↦ ξ.x.neg
                  ),
                  x ↦ ∅
                ⟧,
                div ↦ ⟦
                  λ ⤍ Lorg_eolang_float_div,
                  x ↦ ∅
                ⟧
              ⟧,
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
                  φ ↦ Φ.org.eolang.string (
                    α0 ↦ ξ.ρ
                  )
                ⟧,
                as-int ↦ ⟦
                  φ ↦ ξ.ρ.size.eq (
                    α0 ↦ Φ.org.eolang.int (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 00-00-00-00-00-00-00-08
                      )
                    )
                  )
                  .if (
                    α0 ↦ Φ.org.eolang.int (
                      α0 ↦ ξ.ρ
                    ),
                    α1 ↦ Φ.org.eolang.error (
                      α0 ↦ Φ.org.eolang.string (
                        α0 ↦ Φ.org.eolang.bytes (
                          Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-6E-74
                        )
                      )
                    )
                  )
                ⟧,
                as-float ↦ ⟦
                  φ ↦ ξ.ρ.size.eq (
                    α0 ↦ Φ.org.eolang.int (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 00-00-00-00-00-00-00-08
                      )
                    )
                  )
                  .if (
                    α0 ↦ Φ.org.eolang.float (
                      α0 ↦ ξ.ρ
                    ),
                    α1 ↦ Φ.org.eolang.error (
                      α0 ↦ Φ.org.eolang.string (
                        α0 ↦ Φ.org.eolang.bytes (
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
                  φ ↦ ξ.ρ.right (
                    α0 ↦ ξ.x.neg
                  ),
                  x ↦ ∅
                ⟧,
                right ↦ ⟦
                  λ ⤍ Lorg_eolang_bytes_right,
                  x ↦ ∅
                ⟧,
                as-bool ↦ ⟦
                  φ ↦ ξ.ρ.eq (
                    α0 ↦ Φ.org.eolang.bytes (
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
            λ ⤍ Package,
            ρ ↦ ⟦
              org ↦ ⟦
                eolang ↦ ⟦
                  float ↦ ⟦
                    as-bytes ↦ ∅,
                    φ ↦ ξ.as-bytes,
                    eq ↦ ⟦
                      x ↦ ∅,
                      x-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes,
                      self-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.ρ
                      )
                      .as-bytes,
                      nan-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ Φ.org.eolang.nan
                      )
                      .as-bytes,
                      pos-zero-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.ρ.ρ.float (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 00-00-00-00-00-00-00-00
                          )
                        )
                      )
                      .as-bytes,
                      neg-zero-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.ρ.ρ.float (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 80-00-00-00-00-00-00-00
                          )
                        )
                      )
                      .as-bytes,
                      φ ↦ ξ.x-as-bytes.eq (
                        α0 ↦ ξ.nan-as-bytes
                      )
                      .or (
                        α0 ↦ ξ.self-as-bytes.eq (
                          α0 ↦ ξ.nan-as-bytes
                        )
                      )
                      .if (
                        α0 ↦ Φ.org.eolang.false,
                        α1 ↦ ξ.x-as-bytes.eq (
                          α0 ↦ ξ.pos-zero-as-bytes
                        )
                        .or (
                          α0 ↦ ξ.x-as-bytes.eq (
                            α0 ↦ ξ.neg-zero-as-bytes
                          )
                        )
                        .and (
                          α0 ↦ ξ.self-as-bytes.eq (
                            α0 ↦ ξ.pos-zero-as-bytes
                          )
                          .or (
                            α0 ↦ ξ.self-as-bytes.eq (
                              α0 ↦ ξ.neg-zero-as-bytes
                            )
                          )
                        )
                        .or (
                          α0 ↦ ξ.self-as-bytes.eq (
                            α0 ↦ ξ.x-as-bytes
                          )
                        )
                      )
                    ⟧,
                    lt ↦ ⟦
                      x ↦ ∅,
                      φ ↦ ξ.ρ.ρ.float (
                        α0 ↦ Φ.org.eolang.bytes (
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                      .gt (
                        α0 ↦ ξ.ρ.minus (
                          α0 ↦ ξ.ρ.ρ.float (
                            α0 ↦ ξ.value
                          )
                        )
                      ),
                      value ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes
                    ⟧,
                    lte ↦ ⟦
                      x ↦ ∅,
                      value ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes,
                      φ ↦ ξ.ρ.eq (
                        α0 ↦ ξ.value
                      )
                      .or (
                        α0 ↦ ξ.ρ.lt (
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
                      value ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes,
                      φ ↦ ξ.ρ.eq (
                        α0 ↦ ξ.value
                      )
                      .or (
                        α0 ↦ ξ.ρ.gt (
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
                      φ ↦ ξ.ρ.times (
                        α0 ↦ ξ.ρ.ρ.float (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ BF-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    ⟧,
                    minus ↦ ⟦
                      φ ↦ ξ.ρ.plus (
                        α0 ↦ ξ.x.neg
                      ),
                      x ↦ ∅
                    ⟧,
                    div ↦ ⟦
                      λ ⤍ Lorg_eolang_float_div,
                      x ↦ ∅
                    ⟧
                  ⟧,
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
                      φ ↦ Φ.org.eolang.string (
                        α0 ↦ ξ.ρ
                      )
                    ⟧,
                    as-int ↦ ⟦
                      φ ↦ ξ.ρ.size.eq (
                        α0 ↦ Φ.org.eolang.int (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 00-00-00-00-00-00-00-08
                          )
                        )
                      )
                      .if (
                        α0 ↦ Φ.org.eolang.int (
                          α0 ↦ ξ.ρ
                        ),
                        α1 ↦ Φ.org.eolang.error (
                          α0 ↦ Φ.org.eolang.string (
                            α0 ↦ Φ.org.eolang.bytes (
                              Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-6E-74
                            )
                          )
                        )
                      )
                    ⟧,
                    as-float ↦ ⟦
                      φ ↦ ξ.ρ.size.eq (
                        α0 ↦ Φ.org.eolang.int (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 00-00-00-00-00-00-00-08
                          )
                        )
                      )
                      .if (
                        α0 ↦ Φ.org.eolang.float (
                          α0 ↦ ξ.ρ
                        ),
                        α1 ↦ Φ.org.eolang.error (
                          α0 ↦ Φ.org.eolang.string (
                            α0 ↦ Φ.org.eolang.bytes (
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
                      φ ↦ ξ.ρ.right (
                        α0 ↦ ξ.x.neg
                      ),
                      x ↦ ∅
                    ⟧,
                    right ↦ ⟦
                      λ ⤍ Lorg_eolang_bytes_right,
                      x ↦ ∅
                    ⟧,
                    as-bool ↦ ⟦
                      φ ↦ ξ.ρ.eq (
                        α0 ↦ Φ.org.eolang.bytes (
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
              ⟧,
              c ↦ Φ.org.eolang.float (
                as-bytes ↦ Φ.org.eolang.number (
                  as-bytes ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 40-39-00-00-00-00-00-00
                  )
                )
              ),
              result ↦ ξ.c.times (
                x ↦ Φ.org.eolang.number (
                  as-bytes ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD
                  )
                )
              )
              .plus (
                x ↦ Φ.org.eolang.number (
                  as-bytes ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 40-40-00-00-00-00-00-00
                  )
                )
              ),
              λ ⤍ Package
            ⟧
          ⟧
        ⟧
        .number (
          as-bytes ↦ ⟦
            Δ ⤍ 40-39-00-00-00-00-00-00,
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
              φ ↦ Φ.org.eolang.string (
                α0 ↦ ξ.ρ
              )
            ⟧,
            as-int ↦ ⟦
              φ ↦ ξ.ρ.size.eq (
                α0 ↦ Φ.org.eolang.int (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 00-00-00-00-00-00-00-08
                  )
                )
              )
              .if (
                α0 ↦ Φ.org.eolang.int (
                  α0 ↦ ξ.ρ
                ),
                α1 ↦ Φ.org.eolang.error (
                  α0 ↦ Φ.org.eolang.string (
                    α0 ↦ Φ.org.eolang.bytes (
                      Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-6E-74
                    )
                  )
                )
              )
            ⟧,
            as-float ↦ ⟦
              φ ↦ ξ.ρ.size.eq (
                α0 ↦ Φ.org.eolang.int (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 00-00-00-00-00-00-00-08
                  )
                )
              )
              .if (
                α0 ↦ Φ.org.eolang.float (
                  α0 ↦ ξ.ρ
                ),
                α1 ↦ Φ.org.eolang.error (
                  α0 ↦ Φ.org.eolang.string (
                    α0 ↦ Φ.org.eolang.bytes (
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
              φ ↦ ξ.ρ.right (
                α0 ↦ ξ.x.neg
              ),
              x ↦ ∅
            ⟧,
            right ↦ ⟦
              λ ⤍ Lorg_eolang_bytes_right,
              x ↦ ∅
            ⟧,
            as-bool ↦ ⟦
              φ ↦ ξ.ρ.eq (
                α0 ↦ Φ.org.eolang.bytes (
                  Δ ⤍ 01-
                )
              )
            ⟧,
            concat ↦ ⟦
              λ ⤍ Lorg_eolang_bytes_concat,
              b ↦ ∅
            ⟧,
            ρ ↦ ⟦
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
                  φ ↦ Φ.org.eolang.string (
                    α0 ↦ ξ.ρ
                  )
                ⟧,
                as-int ↦ ⟦
                  φ ↦ ξ.ρ.size.eq (
                    α0 ↦ Φ.org.eolang.int (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 00-00-00-00-00-00-00-08
                      )
                    )
                  )
                  .if (
                    α0 ↦ Φ.org.eolang.int (
                      α0 ↦ ξ.ρ
                    ),
                    α1 ↦ Φ.org.eolang.error (
                      α0 ↦ Φ.org.eolang.string (
                        α0 ↦ Φ.org.eolang.bytes (
                          Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-6E-74
                        )
                      )
                    )
                  )
                ⟧,
                as-float ↦ ⟦
                  φ ↦ ξ.ρ.size.eq (
                    α0 ↦ Φ.org.eolang.int (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 00-00-00-00-00-00-00-08
                      )
                    )
                  )
                  .if (
                    α0 ↦ Φ.org.eolang.float (
                      α0 ↦ ξ.ρ
                    ),
                    α1 ↦ Φ.org.eolang.error (
                      α0 ↦ Φ.org.eolang.string (
                        α0 ↦ Φ.org.eolang.bytes (
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
                  φ ↦ ξ.ρ.right (
                    α0 ↦ ξ.x.neg
                  ),
                  x ↦ ∅
                ⟧,
                right ↦ ⟦
                  λ ⤍ Lorg_eolang_bytes_right,
                  x ↦ ∅
                ⟧,
                as-bool ↦ ⟦
                  φ ↦ ξ.ρ.eq (
                    α0 ↦ Φ.org.eolang.bytes (
                      Δ ⤍ 01-
                    )
                  )
                ⟧,
                concat ↦ ⟦
                  λ ⤍ Lorg_eolang_bytes_concat,
                  b ↦ ∅
                ⟧
              ⟧,
              float ↦ ⟦
                as-bytes ↦ ∅,
                φ ↦ ξ.as-bytes,
                eq ↦ ⟦
                  x ↦ ∅,
                  x-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes,
                  self-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.ρ
                  )
                  .as-bytes,
                  nan-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ Φ.org.eolang.nan
                  )
                  .as-bytes,
                  pos-zero-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.ρ.ρ.float (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  )
                  .as-bytes,
                  neg-zero-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.ρ.ρ.float (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 80-00-00-00-00-00-00-00
                      )
                    )
                  )
                  .as-bytes,
                  φ ↦ ξ.x-as-bytes.eq (
                    α0 ↦ ξ.nan-as-bytes
                  )
                  .or (
                    α0 ↦ ξ.self-as-bytes.eq (
                      α0 ↦ ξ.nan-as-bytes
                    )
                  )
                  .if (
                    α0 ↦ Φ.org.eolang.false,
                    α1 ↦ ξ.x-as-bytes.eq (
                      α0 ↦ ξ.pos-zero-as-bytes
                    )
                    .or (
                      α0 ↦ ξ.x-as-bytes.eq (
                        α0 ↦ ξ.neg-zero-as-bytes
                      )
                    )
                    .and (
                      α0 ↦ ξ.self-as-bytes.eq (
                        α0 ↦ ξ.pos-zero-as-bytes
                      )
                      .or (
                        α0 ↦ ξ.self-as-bytes.eq (
                          α0 ↦ ξ.neg-zero-as-bytes
                        )
                      )
                    )
                    .or (
                      α0 ↦ ξ.self-as-bytes.eq (
                        α0 ↦ ξ.x-as-bytes
                      )
                    )
                  )
                ⟧,
                lt ↦ ⟦
                  x ↦ ∅,
                  φ ↦ ξ.ρ.ρ.float (
                    α0 ↦ Φ.org.eolang.bytes (
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                  .gt (
                    α0 ↦ ξ.ρ.minus (
                      α0 ↦ ξ.ρ.ρ.float (
                        α0 ↦ ξ.value
                      )
                    )
                  ),
                  value ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes
                ⟧,
                lte ↦ ⟦
                  x ↦ ∅,
                  value ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes,
                  φ ↦ ξ.ρ.eq (
                    α0 ↦ ξ.value
                  )
                  .or (
                    α0 ↦ ξ.ρ.lt (
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
                  value ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes,
                  φ ↦ ξ.ρ.eq (
                    α0 ↦ ξ.value
                  )
                  .or (
                    α0 ↦ ξ.ρ.gt (
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
                  φ ↦ ξ.ρ.times (
                    α0 ↦ ξ.ρ.ρ.float (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  )
                ⟧,
                minus ↦ ⟦
                  φ ↦ ξ.ρ.plus (
                    α0 ↦ ξ.x.neg
                  ),
                  x ↦ ∅
                ⟧,
                div ↦ ⟦
                  λ ⤍ Lorg_eolang_float_div,
                  x ↦ ∅
                ⟧
              ⟧,
              λ ⤍ Package,
              ρ ↦ ⟦
                eolang ↦ ⟦
                  float ↦ ⟦
                    as-bytes ↦ ∅,
                    φ ↦ ξ.as-bytes,
                    eq ↦ ⟦
                      x ↦ ∅,
                      x-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes,
                      self-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.ρ
                      )
                      .as-bytes,
                      nan-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ Φ.org.eolang.nan
                      )
                      .as-bytes,
                      pos-zero-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.ρ.ρ.float (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 00-00-00-00-00-00-00-00
                          )
                        )
                      )
                      .as-bytes,
                      neg-zero-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.ρ.ρ.float (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 80-00-00-00-00-00-00-00
                          )
                        )
                      )
                      .as-bytes,
                      φ ↦ ξ.x-as-bytes.eq (
                        α0 ↦ ξ.nan-as-bytes
                      )
                      .or (
                        α0 ↦ ξ.self-as-bytes.eq (
                          α0 ↦ ξ.nan-as-bytes
                        )
                      )
                      .if (
                        α0 ↦ Φ.org.eolang.false,
                        α1 ↦ ξ.x-as-bytes.eq (
                          α0 ↦ ξ.pos-zero-as-bytes
                        )
                        .or (
                          α0 ↦ ξ.x-as-bytes.eq (
                            α0 ↦ ξ.neg-zero-as-bytes
                          )
                        )
                        .and (
                          α0 ↦ ξ.self-as-bytes.eq (
                            α0 ↦ ξ.pos-zero-as-bytes
                          )
                          .or (
                            α0 ↦ ξ.self-as-bytes.eq (
                              α0 ↦ ξ.neg-zero-as-bytes
                            )
                          )
                        )
                        .or (
                          α0 ↦ ξ.self-as-bytes.eq (
                            α0 ↦ ξ.x-as-bytes
                          )
                        )
                      )
                    ⟧,
                    lt ↦ ⟦
                      x ↦ ∅,
                      φ ↦ ξ.ρ.ρ.float (
                        α0 ↦ Φ.org.eolang.bytes (
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                      .gt (
                        α0 ↦ ξ.ρ.minus (
                          α0 ↦ ξ.ρ.ρ.float (
                            α0 ↦ ξ.value
                          )
                        )
                      ),
                      value ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes
                    ⟧,
                    lte ↦ ⟦
                      x ↦ ∅,
                      value ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes,
                      φ ↦ ξ.ρ.eq (
                        α0 ↦ ξ.value
                      )
                      .or (
                        α0 ↦ ξ.ρ.lt (
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
                      value ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes,
                      φ ↦ ξ.ρ.eq (
                        α0 ↦ ξ.value
                      )
                      .or (
                        α0 ↦ ξ.ρ.gt (
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
                      φ ↦ ξ.ρ.times (
                        α0 ↦ ξ.ρ.ρ.float (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ BF-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    ⟧,
                    minus ↦ ⟦
                      φ ↦ ξ.ρ.plus (
                        α0 ↦ ξ.x.neg
                      ),
                      x ↦ ∅
                    ⟧,
                    div ↦ ⟦
                      λ ⤍ Lorg_eolang_float_div,
                      x ↦ ∅
                    ⟧
                  ⟧,
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
                      φ ↦ Φ.org.eolang.string (
                        α0 ↦ ξ.ρ
                      )
                    ⟧,
                    as-int ↦ ⟦
                      φ ↦ ξ.ρ.size.eq (
                        α0 ↦ Φ.org.eolang.int (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 00-00-00-00-00-00-00-08
                          )
                        )
                      )
                      .if (
                        α0 ↦ Φ.org.eolang.int (
                          α0 ↦ ξ.ρ
                        ),
                        α1 ↦ Φ.org.eolang.error (
                          α0 ↦ Φ.org.eolang.string (
                            α0 ↦ Φ.org.eolang.bytes (
                              Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-6E-74
                            )
                          )
                        )
                      )
                    ⟧,
                    as-float ↦ ⟦
                      φ ↦ ξ.ρ.size.eq (
                        α0 ↦ Φ.org.eolang.int (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 00-00-00-00-00-00-00-08
                          )
                        )
                      )
                      .if (
                        α0 ↦ Φ.org.eolang.float (
                          α0 ↦ ξ.ρ
                        ),
                        α1 ↦ Φ.org.eolang.error (
                          α0 ↦ Φ.org.eolang.string (
                            α0 ↦ Φ.org.eolang.bytes (
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
                      φ ↦ ξ.ρ.right (
                        α0 ↦ ξ.x.neg
                      ),
                      x ↦ ∅
                    ⟧,
                    right ↦ ⟦
                      λ ⤍ Lorg_eolang_bytes_right,
                      x ↦ ∅
                    ⟧,
                    as-bool ↦ ⟦
                      φ ↦ ξ.ρ.eq (
                        α0 ↦ Φ.org.eolang.bytes (
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
                λ ⤍ Package,
                ρ ↦ ⟦
                  org ↦ ⟦
                    eolang ↦ ⟦
                      float ↦ ⟦
                        as-bytes ↦ ∅,
                        φ ↦ ξ.as-bytes,
                        eq ↦ ⟦
                          x ↦ ∅,
                          x-as-bytes ↦ Φ.org.eolang.dataized (
                            α0 ↦ ξ.x
                          )
                          .as-bytes,
                          self-as-bytes ↦ Φ.org.eolang.dataized (
                            α0 ↦ ξ.ρ
                          )
                          .as-bytes,
                          nan-as-bytes ↦ Φ.org.eolang.dataized (
                            α0 ↦ Φ.org.eolang.nan
                          )
                          .as-bytes,
                          pos-zero-as-bytes ↦ Φ.org.eolang.dataized (
                            α0 ↦ ξ.ρ.ρ.float (
                              α0 ↦ Φ.org.eolang.bytes (
                                Δ ⤍ 00-00-00-00-00-00-00-00
                              )
                            )
                          )
                          .as-bytes,
                          neg-zero-as-bytes ↦ Φ.org.eolang.dataized (
                            α0 ↦ ξ.ρ.ρ.float (
                              α0 ↦ Φ.org.eolang.bytes (
                                Δ ⤍ 80-00-00-00-00-00-00-00
                              )
                            )
                          )
                          .as-bytes,
                          φ ↦ ξ.x-as-bytes.eq (
                            α0 ↦ ξ.nan-as-bytes
                          )
                          .or (
                            α0 ↦ ξ.self-as-bytes.eq (
                              α0 ↦ ξ.nan-as-bytes
                            )
                          )
                          .if (
                            α0 ↦ Φ.org.eolang.false,
                            α1 ↦ ξ.x-as-bytes.eq (
                              α0 ↦ ξ.pos-zero-as-bytes
                            )
                            .or (
                              α0 ↦ ξ.x-as-bytes.eq (
                                α0 ↦ ξ.neg-zero-as-bytes
                              )
                            )
                            .and (
                              α0 ↦ ξ.self-as-bytes.eq (
                                α0 ↦ ξ.pos-zero-as-bytes
                              )
                              .or (
                                α0 ↦ ξ.self-as-bytes.eq (
                                  α0 ↦ ξ.neg-zero-as-bytes
                                )
                              )
                            )
                            .or (
                              α0 ↦ ξ.self-as-bytes.eq (
                                α0 ↦ ξ.x-as-bytes
                              )
                            )
                          )
                        ⟧,
                        lt ↦ ⟦
                          x ↦ ∅,
                          φ ↦ ξ.ρ.ρ.float (
                            α0 ↦ Φ.org.eolang.bytes (
                              Δ ⤍ 00-00-00-00-00-00-00-00
                            )
                          )
                          .gt (
                            α0 ↦ ξ.ρ.minus (
                              α0 ↦ ξ.ρ.ρ.float (
                                α0 ↦ ξ.value
                              )
                            )
                          ),
                          value ↦ Φ.org.eolang.dataized (
                            α0 ↦ ξ.x
                          )
                          .as-bytes
                        ⟧,
                        lte ↦ ⟦
                          x ↦ ∅,
                          value ↦ Φ.org.eolang.dataized (
                            α0 ↦ ξ.x
                          )
                          .as-bytes,
                          φ ↦ ξ.ρ.eq (
                            α0 ↦ ξ.value
                          )
                          .or (
                            α0 ↦ ξ.ρ.lt (
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
                          value ↦ Φ.org.eolang.dataized (
                            α0 ↦ ξ.x
                          )
                          .as-bytes,
                          φ ↦ ξ.ρ.eq (
                            α0 ↦ ξ.value
                          )
                          .or (
                            α0 ↦ ξ.ρ.gt (
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
                          φ ↦ ξ.ρ.times (
                            α0 ↦ ξ.ρ.ρ.float (
                              α0 ↦ Φ.org.eolang.bytes (
                                Δ ⤍ BF-F0-00-00-00-00-00-00
                              )
                            )
                          )
                        ⟧,
                        minus ↦ ⟦
                          φ ↦ ξ.ρ.plus (
                            α0 ↦ ξ.x.neg
                          ),
                          x ↦ ∅
                        ⟧,
                        div ↦ ⟦
                          λ ⤍ Lorg_eolang_float_div,
                          x ↦ ∅
                        ⟧
                      ⟧,
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
                          φ ↦ Φ.org.eolang.string (
                            α0 ↦ ξ.ρ
                          )
                        ⟧,
                        as-int ↦ ⟦
                          φ ↦ ξ.ρ.size.eq (
                            α0 ↦ Φ.org.eolang.int (
                              α0 ↦ Φ.org.eolang.bytes (
                                Δ ⤍ 00-00-00-00-00-00-00-08
                              )
                            )
                          )
                          .if (
                            α0 ↦ Φ.org.eolang.int (
                              α0 ↦ ξ.ρ
                            ),
                            α1 ↦ Φ.org.eolang.error (
                              α0 ↦ Φ.org.eolang.string (
                                α0 ↦ Φ.org.eolang.bytes (
                                  Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-6E-74
                                )
                              )
                            )
                          )
                        ⟧,
                        as-float ↦ ⟦
                          φ ↦ ξ.ρ.size.eq (
                            α0 ↦ Φ.org.eolang.int (
                              α0 ↦ Φ.org.eolang.bytes (
                                Δ ⤍ 00-00-00-00-00-00-00-08
                              )
                            )
                          )
                          .if (
                            α0 ↦ Φ.org.eolang.float (
                              α0 ↦ ξ.ρ
                            ),
                            α1 ↦ Φ.org.eolang.error (
                              α0 ↦ Φ.org.eolang.string (
                                α0 ↦ Φ.org.eolang.bytes (
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
                          φ ↦ ξ.ρ.right (
                            α0 ↦ ξ.x.neg
                          ),
                          x ↦ ∅
                        ⟧,
                        right ↦ ⟦
                          λ ⤍ Lorg_eolang_bytes_right,
                          x ↦ ∅
                        ⟧,
                        as-bool ↦ ⟦
                          φ ↦ ξ.ρ.eq (
                            α0 ↦ Φ.org.eolang.bytes (
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
                  ⟧,
                  c ↦ Φ.org.eolang.float (
                    as-bytes ↦ Φ.org.eolang.number (
                      as-bytes ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 40-39-00-00-00-00-00-00
                      )
                    )
                  ),
                  result ↦ ξ.c.times (
                    x ↦ Φ.org.eolang.number (
                      as-bytes ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD
                      )
                    )
                  )
                  .plus (
                    x ↦ Φ.org.eolang.number (
                      as-bytes ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 40-40-00-00-00-00-00-00
                      )
                    )
                  ),
                  λ ⤍ Package
                ⟧
              ⟧
            ⟧
          ⟧
        ),
        φ ↦ ξ.as-bytes,
        eq ↦ ⟦
          x ↦ ∅,
          x-as-bytes ↦ Φ.org.eolang.dataized (
            α0 ↦ ξ.x
          )
          .as-bytes,
          self-as-bytes ↦ Φ.org.eolang.dataized (
            α0 ↦ ξ.ρ
          )
          .as-bytes,
          nan-as-bytes ↦ Φ.org.eolang.dataized (
            α0 ↦ Φ.org.eolang.nan
          )
          .as-bytes,
          pos-zero-as-bytes ↦ Φ.org.eolang.dataized (
            α0 ↦ ξ.ρ.ρ.float (
              α0 ↦ Φ.org.eolang.bytes (
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            )
          )
          .as-bytes,
          neg-zero-as-bytes ↦ Φ.org.eolang.dataized (
            α0 ↦ ξ.ρ.ρ.float (
              α0 ↦ Φ.org.eolang.bytes (
                Δ ⤍ 80-00-00-00-00-00-00-00
              )
            )
          )
          .as-bytes,
          φ ↦ ξ.x-as-bytes.eq (
            α0 ↦ ξ.nan-as-bytes
          )
          .or (
            α0 ↦ ξ.self-as-bytes.eq (
              α0 ↦ ξ.nan-as-bytes
            )
          )
          .if (
            α0 ↦ Φ.org.eolang.false,
            α1 ↦ ξ.x-as-bytes.eq (
              α0 ↦ ξ.pos-zero-as-bytes
            )
            .or (
              α0 ↦ ξ.x-as-bytes.eq (
                α0 ↦ ξ.neg-zero-as-bytes
              )
            )
            .and (
              α0 ↦ ξ.self-as-bytes.eq (
                α0 ↦ ξ.pos-zero-as-bytes
              )
              .or (
                α0 ↦ ξ.self-as-bytes.eq (
                  α0 ↦ ξ.neg-zero-as-bytes
                )
              )
            )
            .or (
              α0 ↦ ξ.self-as-bytes.eq (
                α0 ↦ ξ.x-as-bytes
              )
            )
          )
        ⟧,
        lt ↦ ⟦
          x ↦ ∅,
          φ ↦ ξ.ρ.ρ.float (
            α0 ↦ Φ.org.eolang.bytes (
              Δ ⤍ 00-00-00-00-00-00-00-00
            )
          )
          .gt (
            α0 ↦ ξ.ρ.minus (
              α0 ↦ ξ.ρ.ρ.float (
                α0 ↦ ξ.value
              )
            )
          ),
          value ↦ Φ.org.eolang.dataized (
            α0 ↦ ξ.x
          )
          .as-bytes
        ⟧,
        lte ↦ ⟦
          x ↦ ∅,
          value ↦ Φ.org.eolang.dataized (
            α0 ↦ ξ.x
          )
          .as-bytes,
          φ ↦ ξ.ρ.eq (
            α0 ↦ ξ.value
          )
          .or (
            α0 ↦ ξ.ρ.lt (
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
          value ↦ Φ.org.eolang.dataized (
            α0 ↦ ξ.x
          )
          .as-bytes,
          φ ↦ ξ.ρ.eq (
            α0 ↦ ξ.value
          )
          .or (
            α0 ↦ ξ.ρ.gt (
              α0 ↦ ξ.value
            )
          )
        ⟧,
        plus ↦ ⟦
          λ ⤍ Lorg_eolang_float_plus,
          x ↦ ∅
        ⟧,
        neg ↦ ⟦
          φ ↦ ξ.ρ.times (
            α0 ↦ ξ.ρ.ρ.float (
              α0 ↦ Φ.org.eolang.bytes (
                Δ ⤍ BF-F0-00-00-00-00-00-00
              )
            )
          )
        ⟧,
        minus ↦ ⟦
          φ ↦ ξ.ρ.plus (
            α0 ↦ ξ.x.neg
          ),
          x ↦ ∅
        ⟧,
        div ↦ ⟦
          λ ⤍ Lorg_eolang_float_div,
          x ↦ ∅
        ⟧,
        ρ ↦ ⟦
          float ↦ ⟦
            as-bytes ↦ ∅,
            φ ↦ ξ.as-bytes,
            eq ↦ ⟦
              x ↦ ∅,
              x-as-bytes ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.x
              )
              .as-bytes,
              self-as-bytes ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.ρ
              )
              .as-bytes,
              nan-as-bytes ↦ Φ.org.eolang.dataized (
                α0 ↦ Φ.org.eolang.nan
              )
              .as-bytes,
              pos-zero-as-bytes ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.ρ.ρ.float (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              )
              .as-bytes,
              neg-zero-as-bytes ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.ρ.ρ.float (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 80-00-00-00-00-00-00-00
                  )
                )
              )
              .as-bytes,
              φ ↦ ξ.x-as-bytes.eq (
                α0 ↦ ξ.nan-as-bytes
              )
              .or (
                α0 ↦ ξ.self-as-bytes.eq (
                  α0 ↦ ξ.nan-as-bytes
                )
              )
              .if (
                α0 ↦ Φ.org.eolang.false,
                α1 ↦ ξ.x-as-bytes.eq (
                  α0 ↦ ξ.pos-zero-as-bytes
                )
                .or (
                  α0 ↦ ξ.x-as-bytes.eq (
                    α0 ↦ ξ.neg-zero-as-bytes
                  )
                )
                .and (
                  α0 ↦ ξ.self-as-bytes.eq (
                    α0 ↦ ξ.pos-zero-as-bytes
                  )
                  .or (
                    α0 ↦ ξ.self-as-bytes.eq (
                      α0 ↦ ξ.neg-zero-as-bytes
                    )
                  )
                )
                .or (
                  α0 ↦ ξ.self-as-bytes.eq (
                    α0 ↦ ξ.x-as-bytes
                  )
                )
              )
            ⟧,
            lt ↦ ⟦
              x ↦ ∅,
              φ ↦ ξ.ρ.ρ.float (
                α0 ↦ Φ.org.eolang.bytes (
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
              .gt (
                α0 ↦ ξ.ρ.minus (
                  α0 ↦ ξ.ρ.ρ.float (
                    α0 ↦ ξ.value
                  )
                )
              ),
              value ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.x
              )
              .as-bytes
            ⟧,
            lte ↦ ⟦
              x ↦ ∅,
              value ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.x
              )
              .as-bytes,
              φ ↦ ξ.ρ.eq (
                α0 ↦ ξ.value
              )
              .or (
                α0 ↦ ξ.ρ.lt (
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
              value ↦ Φ.org.eolang.dataized (
                α0 ↦ ξ.x
              )
              .as-bytes,
              φ ↦ ξ.ρ.eq (
                α0 ↦ ξ.value
              )
              .or (
                α0 ↦ ξ.ρ.gt (
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
              φ ↦ ξ.ρ.times (
                α0 ↦ ξ.ρ.ρ.float (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ BF-F0-00-00-00-00-00-00
                  )
                )
              )
            ⟧,
            minus ↦ ⟦
              φ ↦ ξ.ρ.plus (
                α0 ↦ ξ.x.neg
              ),
              x ↦ ∅
            ⟧,
            div ↦ ⟦
              λ ⤍ Lorg_eolang_float_div,
              x ↦ ∅
            ⟧
          ⟧,
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
              φ ↦ Φ.org.eolang.string (
                α0 ↦ ξ.ρ
              )
            ⟧,
            as-int ↦ ⟦
              φ ↦ ξ.ρ.size.eq (
                α0 ↦ Φ.org.eolang.int (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 00-00-00-00-00-00-00-08
                  )
                )
              )
              .if (
                α0 ↦ Φ.org.eolang.int (
                  α0 ↦ ξ.ρ
                ),
                α1 ↦ Φ.org.eolang.error (
                  α0 ↦ Φ.org.eolang.string (
                    α0 ↦ Φ.org.eolang.bytes (
                      Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-6E-74
                    )
                  )
                )
              )
            ⟧,
            as-float ↦ ⟦
              φ ↦ ξ.ρ.size.eq (
                α0 ↦ Φ.org.eolang.int (
                  α0 ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 00-00-00-00-00-00-00-08
                  )
                )
              )
              .if (
                α0 ↦ Φ.org.eolang.float (
                  α0 ↦ ξ.ρ
                ),
                α1 ↦ Φ.org.eolang.error (
                  α0 ↦ Φ.org.eolang.string (
                    α0 ↦ Φ.org.eolang.bytes (
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
              φ ↦ ξ.ρ.right (
                α0 ↦ ξ.x.neg
              ),
              x ↦ ∅
            ⟧,
            right ↦ ⟦
              λ ⤍ Lorg_eolang_bytes_right,
              x ↦ ∅
            ⟧,
            as-bool ↦ ⟦
              φ ↦ ξ.ρ.eq (
                α0 ↦ Φ.org.eolang.bytes (
                  Δ ⤍ 01-
                )
              )
            ⟧,
            concat ↦ ⟦
              λ ⤍ Lorg_eolang_bytes_concat,
              b ↦ ∅
            ⟧
          ⟧,
          λ ⤍ Package,
          ρ ↦ ⟦
            eolang ↦ ⟦
              float ↦ ⟦
                as-bytes ↦ ∅,
                φ ↦ ξ.as-bytes,
                eq ↦ ⟦
                  x ↦ ∅,
                  x-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes,
                  self-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.ρ
                  )
                  .as-bytes,
                  nan-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ Φ.org.eolang.nan
                  )
                  .as-bytes,
                  pos-zero-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.ρ.ρ.float (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  )
                  .as-bytes,
                  neg-zero-as-bytes ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.ρ.ρ.float (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 80-00-00-00-00-00-00-00
                      )
                    )
                  )
                  .as-bytes,
                  φ ↦ ξ.x-as-bytes.eq (
                    α0 ↦ ξ.nan-as-bytes
                  )
                  .or (
                    α0 ↦ ξ.self-as-bytes.eq (
                      α0 ↦ ξ.nan-as-bytes
                    )
                  )
                  .if (
                    α0 ↦ Φ.org.eolang.false,
                    α1 ↦ ξ.x-as-bytes.eq (
                      α0 ↦ ξ.pos-zero-as-bytes
                    )
                    .or (
                      α0 ↦ ξ.x-as-bytes.eq (
                        α0 ↦ ξ.neg-zero-as-bytes
                      )
                    )
                    .and (
                      α0 ↦ ξ.self-as-bytes.eq (
                        α0 ↦ ξ.pos-zero-as-bytes
                      )
                      .or (
                        α0 ↦ ξ.self-as-bytes.eq (
                          α0 ↦ ξ.neg-zero-as-bytes
                        )
                      )
                    )
                    .or (
                      α0 ↦ ξ.self-as-bytes.eq (
                        α0 ↦ ξ.x-as-bytes
                      )
                    )
                  )
                ⟧,
                lt ↦ ⟦
                  x ↦ ∅,
                  φ ↦ ξ.ρ.ρ.float (
                    α0 ↦ Φ.org.eolang.bytes (
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                  .gt (
                    α0 ↦ ξ.ρ.minus (
                      α0 ↦ ξ.ρ.ρ.float (
                        α0 ↦ ξ.value
                      )
                    )
                  ),
                  value ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes
                ⟧,
                lte ↦ ⟦
                  x ↦ ∅,
                  value ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes,
                  φ ↦ ξ.ρ.eq (
                    α0 ↦ ξ.value
                  )
                  .or (
                    α0 ↦ ξ.ρ.lt (
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
                  value ↦ Φ.org.eolang.dataized (
                    α0 ↦ ξ.x
                  )
                  .as-bytes,
                  φ ↦ ξ.ρ.eq (
                    α0 ↦ ξ.value
                  )
                  .or (
                    α0 ↦ ξ.ρ.gt (
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
                  φ ↦ ξ.ρ.times (
                    α0 ↦ ξ.ρ.ρ.float (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  )
                ⟧,
                minus ↦ ⟦
                  φ ↦ ξ.ρ.plus (
                    α0 ↦ ξ.x.neg
                  ),
                  x ↦ ∅
                ⟧,
                div ↦ ⟦
                  λ ⤍ Lorg_eolang_float_div,
                  x ↦ ∅
                ⟧
              ⟧,
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
                  φ ↦ Φ.org.eolang.string (
                    α0 ↦ ξ.ρ
                  )
                ⟧,
                as-int ↦ ⟦
                  φ ↦ ξ.ρ.size.eq (
                    α0 ↦ Φ.org.eolang.int (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 00-00-00-00-00-00-00-08
                      )
                    )
                  )
                  .if (
                    α0 ↦ Φ.org.eolang.int (
                      α0 ↦ ξ.ρ
                    ),
                    α1 ↦ Φ.org.eolang.error (
                      α0 ↦ Φ.org.eolang.string (
                        α0 ↦ Φ.org.eolang.bytes (
                          Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-6E-74
                        )
                      )
                    )
                  )
                ⟧,
                as-float ↦ ⟦
                  φ ↦ ξ.ρ.size.eq (
                    α0 ↦ Φ.org.eolang.int (
                      α0 ↦ Φ.org.eolang.bytes (
                        Δ ⤍ 00-00-00-00-00-00-00-08
                      )
                    )
                  )
                  .if (
                    α0 ↦ Φ.org.eolang.float (
                      α0 ↦ ξ.ρ
                    ),
                    α1 ↦ Φ.org.eolang.error (
                      α0 ↦ Φ.org.eolang.string (
                        α0 ↦ Φ.org.eolang.bytes (
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
                  φ ↦ ξ.ρ.right (
                    α0 ↦ ξ.x.neg
                  ),
                  x ↦ ∅
                ⟧,
                right ↦ ⟦
                  λ ⤍ Lorg_eolang_bytes_right,
                  x ↦ ∅
                ⟧,
                as-bool ↦ ⟦
                  φ ↦ ξ.ρ.eq (
                    α0 ↦ Φ.org.eolang.bytes (
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
            λ ⤍ Package,
            ρ ↦ ⟦
              org ↦ ⟦
                eolang ↦ ⟦
                  float ↦ ⟦
                    as-bytes ↦ ∅,
                    φ ↦ ξ.as-bytes,
                    eq ↦ ⟦
                      x ↦ ∅,
                      x-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes,
                      self-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.ρ
                      )
                      .as-bytes,
                      nan-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ Φ.org.eolang.nan
                      )
                      .as-bytes,
                      pos-zero-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.ρ.ρ.float (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 00-00-00-00-00-00-00-00
                          )
                        )
                      )
                      .as-bytes,
                      neg-zero-as-bytes ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.ρ.ρ.float (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 80-00-00-00-00-00-00-00
                          )
                        )
                      )
                      .as-bytes,
                      φ ↦ ξ.x-as-bytes.eq (
                        α0 ↦ ξ.nan-as-bytes
                      )
                      .or (
                        α0 ↦ ξ.self-as-bytes.eq (
                          α0 ↦ ξ.nan-as-bytes
                        )
                      )
                      .if (
                        α0 ↦ Φ.org.eolang.false,
                        α1 ↦ ξ.x-as-bytes.eq (
                          α0 ↦ ξ.pos-zero-as-bytes
                        )
                        .or (
                          α0 ↦ ξ.x-as-bytes.eq (
                            α0 ↦ ξ.neg-zero-as-bytes
                          )
                        )
                        .and (
                          α0 ↦ ξ.self-as-bytes.eq (
                            α0 ↦ ξ.pos-zero-as-bytes
                          )
                          .or (
                            α0 ↦ ξ.self-as-bytes.eq (
                              α0 ↦ ξ.neg-zero-as-bytes
                            )
                          )
                        )
                        .or (
                          α0 ↦ ξ.self-as-bytes.eq (
                            α0 ↦ ξ.x-as-bytes
                          )
                        )
                      )
                    ⟧,
                    lt ↦ ⟦
                      x ↦ ∅,
                      φ ↦ ξ.ρ.ρ.float (
                        α0 ↦ Φ.org.eolang.bytes (
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                      .gt (
                        α0 ↦ ξ.ρ.minus (
                          α0 ↦ ξ.ρ.ρ.float (
                            α0 ↦ ξ.value
                          )
                        )
                      ),
                      value ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes
                    ⟧,
                    lte ↦ ⟦
                      x ↦ ∅,
                      value ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes,
                      φ ↦ ξ.ρ.eq (
                        α0 ↦ ξ.value
                      )
                      .or (
                        α0 ↦ ξ.ρ.lt (
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
                      value ↦ Φ.org.eolang.dataized (
                        α0 ↦ ξ.x
                      )
                      .as-bytes,
                      φ ↦ ξ.ρ.eq (
                        α0 ↦ ξ.value
                      )
                      .or (
                        α0 ↦ ξ.ρ.gt (
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
                      φ ↦ ξ.ρ.times (
                        α0 ↦ ξ.ρ.ρ.float (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ BF-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    ⟧,
                    minus ↦ ⟦
                      φ ↦ ξ.ρ.plus (
                        α0 ↦ ξ.x.neg
                      ),
                      x ↦ ∅
                    ⟧,
                    div ↦ ⟦
                      λ ⤍ Lorg_eolang_float_div,
                      x ↦ ∅
                    ⟧
                  ⟧,
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
                      φ ↦ Φ.org.eolang.string (
                        α0 ↦ ξ.ρ
                      )
                    ⟧,
                    as-int ↦ ⟦
                      φ ↦ ξ.ρ.size.eq (
                        α0 ↦ Φ.org.eolang.int (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 00-00-00-00-00-00-00-08
                          )
                        )
                      )
                      .if (
                        α0 ↦ Φ.org.eolang.int (
                          α0 ↦ ξ.ρ
                        ),
                        α1 ↦ Φ.org.eolang.error (
                          α0 ↦ Φ.org.eolang.string (
                            α0 ↦ Φ.org.eolang.bytes (
                              Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-6E-74
                            )
                          )
                        )
                      )
                    ⟧,
                    as-float ↦ ⟦
                      φ ↦ ξ.ρ.size.eq (
                        α0 ↦ Φ.org.eolang.int (
                          α0 ↦ Φ.org.eolang.bytes (
                            Δ ⤍ 00-00-00-00-00-00-00-08
                          )
                        )
                      )
                      .if (
                        α0 ↦ Φ.org.eolang.float (
                          α0 ↦ ξ.ρ
                        ),
                        α1 ↦ Φ.org.eolang.error (
                          α0 ↦ Φ.org.eolang.string (
                            α0 ↦ Φ.org.eolang.bytes (
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
                      φ ↦ ξ.ρ.right (
                        α0 ↦ ξ.x.neg
                      ),
                      x ↦ ∅
                    ⟧,
                    right ↦ ⟦
                      λ ⤍ Lorg_eolang_bytes_right,
                      x ↦ ∅
                    ⟧,
                    as-bool ↦ ⟦
                      φ ↦ ξ.ρ.eq (
                        α0 ↦ Φ.org.eolang.bytes (
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
              ⟧,
              c ↦ Φ.org.eolang.float (
                as-bytes ↦ Φ.org.eolang.number (
                  as-bytes ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 40-39-00-00-00-00-00-00
                  )
                )
              ),
              result ↦ ξ.c.times (
                x ↦ Φ.org.eolang.number (
                  as-bytes ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 3F-FC-CC-CC-CC-CC-CC-CD
                  )
                )
              )
              .plus (
                x ↦ Φ.org.eolang.number (
                  as-bytes ↦ Φ.org.eolang.bytes (
                    Δ ⤍ 40-40-00-00-00-00-00-00
                  )
                )
              ),
              λ ⤍ Package
            ⟧
          ⟧
        ⟧
      ⟧
    ⟧
    .plus (
      x ↦ 32.0
    ),
    λ ⤍ Package
  ⟧
}
```
