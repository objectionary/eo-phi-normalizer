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
          as-bool ↦ ξ.eq(
            α0 ↦ Φ.org.eolang.bytes(
              Δ ⤍ 01-
            )
          ),
          as-string ↦ Φ.org.eolang.string(
            α0 ↦ ξ
          ),
          as-number ↦ ⟦
            φ ↦ ξ.ρ.eq(
              α0 ↦ Φ.org.eolang.nan.as-bytes
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.ρ.eq(
                α0 ↦ Φ.org.eolang.positive-infinity.as-bytes
              ).if(
                α0 ↦ Φ.org.eolang.positive-infinity,
                α1 ↦ ξ.ρ.eq(
                  α0 ↦ Φ.org.eolang.negative-infinity.as-bytes
                ).if(
                  α0 ↦ Φ.org.eolang.negative-infinity,
                  α1 ↦ ξ.ρ.size.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 40-20-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ ξ.ρ
                    ),
                    α1 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.txt.sprintf(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-61-20-6E-75-6D-62-65-72-2C-20-62-79-74-65-73-20-61-72-65-20-25-78
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple.empty
                      )
                    )
                  )
                )
              )
            )
          ⟧,
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
          as-i64 ↦ ⟦
            φ ↦ ξ.ρ.size.eq(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-20-00-00-00-00-00-00
                )
              )
            ).if(
              α0 ↦ Φ.org.eolang.i64(
                α0 ↦ ξ.ρ
              ),
              α1 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.txt.sprintf(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-38-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-36-34-2C-20-62-79-74-65-73-20-61-72-65-20-25-78
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.ρ
                  )
                )
              )
            )
          ⟧,
          as-i32 ↦ ⟦
            φ ↦ ξ.ρ.size.eq(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-10-00-00-00-00-00-00
                )
              )
            ).if(
              α0 ↦ Φ.org.eolang.i32(
                α0 ↦ ξ.ρ
              ),
              α1 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.txt.sprintf(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-34-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-33-32-2C-20-62-79-74-65-73-20-61-72-65-20-25-78
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.ρ
                  )
                )
              )
            )
          ⟧,
          as-i16 ↦ ⟦
            φ ↦ ξ.ρ.size.eq(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-00-00-00-00-00-00-00
                )
              )
            ).if(
              α0 ↦ Φ.org.eolang.i16(
                α0 ↦ ξ.ρ
              ),
              α1 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.txt.sprintf(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-6F-6E-20-32-20-6C-65-6E-67-74-68-20-62-79-74-65-73-20-74-6F-20-69-31-36-2C-20-62-79-74-65-73-20-61-72-65-20-25-78
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.ρ
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
          target ↦ ∅,
          φ ↦ Φ.org.eolang.try(
            α0 ↦ ξ.target,
            α1 ↦ ⟦
              φ ↦ Φ.org.eolang.error(
                α0 ↦ ξ.ex
              ),
              ex ↦ ∅
            ⟧,
            α2 ↦ Φ.org.eolang.bytes(
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

## [org/eolang/fs/dir.phi](./org/eolang/fs/dir.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        fs ↦ ⟦
          dir ↦ ⟦
            file ↦ ∅,
            φ ↦ ξ.file,
            is-directory ↦ Φ.org.eolang.true,
            made ↦ ⟦
              φ ↦ ξ.ρ.exists.if(
                α0 ↦ ξ.ρ,
                α1 ↦ Φ.org.eolang.seq(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ ξ.mkdir
                    ),
                    α1 ↦ ξ.ρ
                  )
                )
              ),
              mkdir ↦ ⟦
                λ ⤍ Lorg_eolang_fs_dir_made_mkdir
              ⟧
            ⟧,
            walk ↦ ⟦
              λ ⤍ Lorg_eolang_fs_dir_walk,
              glob ↦ ∅
            ⟧,
            deleted ↦ ⟦
              walked ↦ ξ.ρ.walk(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 2A-2A
                  )
                )
              ).at.ρ,
              len ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.walked.length
              ).as-bytes,
              φ ↦ ξ.ρ.exists.if(
                α0 ↦ Φ.org.eolang.seq(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ ξ.rec-delete(
                        α0 ↦ ξ.walked,
                        α1 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 00-00-00-00-00-00-00-00
                          )
                        )
                      )
                    ),
                    α1 ↦ ξ.ρ
                  )
                ),
                α1 ↦ ξ.ρ
              ),
              rec-delete ↦ ⟦
                tup ↦ ∅,
                index ↦ ∅,
                φ ↦ ξ.ρ.len.eq(
                  α0 ↦ ξ.index
                ).if(
                  α0 ↦ Φ.org.eolang.true,
                  α1 ↦ Φ.org.eolang.seq(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.tup.tail.deleted.exists
                      ),
                      α1 ↦ ξ.ρ.rec-delete(
                        α0 ↦ ξ.tup.head,
                        α1 ↦ ξ.index.plus(
                          α0 ↦ Φ.org.eolang.number(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ 3F-F0-00-00-00-00-00-00
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ⟧
            ⟧,
            tmpfile ↦ ⟦
              φ ↦ ξ.ρ.exists.if(
                α0 ↦ Φ.org.eolang.fs.file(
                  α0 ↦ ξ.touch.as-bytes.as-string
                ),
                α1 ↦ Φ.org.eolang.error(
                  α0 ↦ Φ.org.eolang.txt.sprintf(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 44-69-72-65-63-74-6F-72-79-20-25-73-20-64-6F-65-73-20-6E-6F-74-20-65-78-69-73-74-2C-20-63-61-6E-27-74-20-63-72-65-61-74-65-20-74-65-6D-70-6F-72-61-72-79-20-66-69-6C-65
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ ξ.ρ.path
                    )
                  )
                )
              ),
              touch ↦ ⟦
                λ ⤍ Lorg_eolang_fs_dir_tmpfile_touch
              ⟧
            ⟧,
            open ↦ ⟦
              mode ↦ ∅,
              scope ↦ ∅,
              φ ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.txt.sprintf(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 54-68-65-20-66-69-6C-65-20-25-73-20-69-73-20-61-20-64-69-72-65-63-74-6F-72-79-2C-20-63-61-6E-27-74-20-6F-70-65-6E-20-66-6F-72-20-49-2F-4F-20-6F-70-65-72-61-74-69-6F-6E-73
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.ρ.path
                  )
                )
              )
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

## [org/eolang/fs/file.phi](./org/eolang/fs/file.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        fs ↦ ⟦
          file ↦ ⟦
            path ↦ ∅,
            φ ↦ ξ.path,
            is-directory ↦ ⟦
              λ ⤍ Lorg_eolang_fs_file_is_directory
            ⟧,
            exists ↦ ⟦
              λ ⤍ Lorg_eolang_fs_file_exists
            ⟧,
            touched ↦ ⟦
              φ ↦ ξ.ρ.exists.if(
                α0 ↦ ξ.ρ,
                α1 ↦ Φ.org.eolang.seq(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ ξ.touch
                    ),
                    α1 ↦ ξ.ρ
                  )
                )
              ),
              touch ↦ ⟦
                λ ⤍ Lorg_eolang_fs_file_touched_touch
              ⟧
            ⟧,
            deleted ↦ ⟦
              φ ↦ ξ.ρ.exists.if(
                α0 ↦ Φ.org.eolang.seq(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ ξ.delete
                    ),
                    α1 ↦ ξ.ρ
                  )
                ),
                α1 ↦ ξ.ρ
              ),
              delete ↦ ⟦
                λ ⤍ Lorg_eolang_fs_file_deleted_delete
              ⟧
            ⟧,
            size ↦ ⟦
              λ ⤍ Lorg_eolang_fs_file_size
            ⟧,
            moved ↦ ⟦
              target ↦ ∅,
              φ ↦ Φ.org.eolang.fs.file(
                α0 ↦ ξ.move.as-bytes.as-string
              ),
              move ↦ ⟦
                λ ⤍ Lorg_eolang_fs_file_moved_move
              ⟧
            ⟧,
            as-path ↦ ⟦
              φ ↦ Φ.org.eolang.fs.path(
                α0 ↦ ξ.ρ.path
              ).determined
            ⟧,
            open ↦ ⟦
              mode ↦ ∅,
              scope ↦ ∅,
              access ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.mode
              ).as-bytes,
              read ↦ ξ.access.eq(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 72-
                  )
                )
              ),
              write ↦ ξ.access.eq(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 77-
                  )
                )
              ),
              append ↦ ξ.access.eq(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 61-
                  )
                )
              ),
              read-write ↦ ξ.access.eq(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 72-2B
                  )
                )
              ),
              write-read ↦ ξ.access.eq(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 77-2B
                  )
                )
              ),
              read-append ↦ ξ.access.eq(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 61-2B
                  )
                )
              ),
              can-read ↦ ξ.read.or(
                α0 ↦ ξ.read-write
              ).or(
                α0 ↦ ξ.write-read.or(
                  α0 ↦ ξ.read-append
                )
              ).as-bool,
              can-write ↦ ξ.write.or(
                α0 ↦ ξ.read-write
              ).or(
                α0 ↦ ξ.write-read.or(
                  α0 ↦ ξ.read-append
                )
              ).or(
                α0 ↦ ξ.append
              ).as-bool,
              must-exists ↦ ξ.read.or(
                α0 ↦ ξ.read-write
              ).as-bool,
              truncate ↦ ξ.write.or(
                α0 ↦ ξ.write-read
              ).as-bool,
              φ ↦ ξ.can-read.not.and(
                α0 ↦ ξ.can-write.not
              ).if(
                α0 ↦ Φ.org.eolang.error(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 57-72-6F-6E-67-20-61-63-63-65-73-73-20-6D-6F-64-2E-20-4F-6E-6C-79-20-6E-65-78-74-20-6D-6F-64-65-73-20-61-72-65-20-61-76-61-69-6C-61-62-6C-65-3A-20-27-72-27-2C-20-27-77-27-2C-20-27-61-27-2C-20-27-72-2B-27-2C-20-27-77-2B-27-2C-20-27-61-2B-27
                    )
                  )
                ),
                α1 ↦ ξ.ρ.exists.not.if(
                  α0 ↦ ξ.must-exists.if(
                    α0 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.txt.sprintf(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 46-69-6C-65-20-6D-75-73-74-20-65-78-69-73-74-20-66-6F-72-20-67-69-76-65-6E-20-61-63-63-65-73-73-20-6D-6F-64-3A-20-27-25-73-27
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.access
                        )
                      )
                    ),
                    α1 ↦ Φ.org.eolang.seq(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ ξ.ρ.touched.touch
                          ),
                          α1 ↦ ξ.process-file
                        ),
                        α1 ↦ ξ.ρ
                      )
                    )
                  ),
                  α1 ↦ ξ.truncate.if(
                    α0 ↦ Φ.org.eolang.seq(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple.empty,
                              α1 ↦ ξ.ρ.deleted.delete
                            ),
                            α1 ↦ ξ.ρ.touched.touch
                          ),
                          α1 ↦ ξ.process-file
                        ),
                        α1 ↦ ξ.ρ
                      )
                    ),
                    α1 ↦ Φ.org.eolang.seq(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.process-file
                        ),
                        α1 ↦ ξ.ρ
                      )
                    )
                  )
                )
              ),
              process-file ↦ ⟦
                λ ⤍ Lorg_eolang_fs_file_open_process_file
              ⟧,
              file-stream ↦ ⟦
                read ↦ ⟦
                  size ↦ ∅,
                  φ ↦ ξ.input-block(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ --
                    )
                  ).read(
                    α0 ↦ ξ.size
                  ).self,
                  input-block ↦ ⟦
                    buffer ↦ ∅,
                    self ↦ ξ,
                    φ ↦ ξ.buffer,
                    read ↦ ⟦
                      size ↦ ∅,
                      read-bytes ↦ Φ.org.eolang.dataized(
                        α0 ↦ ξ.ρ.ρ.read-bytes(
                          α0 ↦ ξ.size
                        )
                      ).as-bytes,
                      φ ↦ ξ.ρ.ρ.ρ.ρ.can-read.not.if(
                        α0 ↦ ξ.auto-named-attr-at-215-18,
                        α1 ↦ Φ.org.eolang.seq(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple.empty,
                              α1 ↦ ξ.read-bytes
                            ),
                            α1 ↦ ξ.ρ.ρ.input-block(
                              α0 ↦ ξ.read-bytes
                            )
                          )
                        )
                      ).self,
                      auto-named-attr-at-215-18 ↦ ⟦
                        self ↦ ξ,
                        φ ↦ Φ.org.eolang.error(
                          α0 ↦ Φ.org.eolang.txt.sprintf(
                            α0 ↦ Φ.org.eolang.string(
                              α0 ↦ Φ.org.eolang.bytes(
                                Δ ⤍ 43-61-6E-27-74-20-72-65-61-64-20-66-72-6F-6D-20-66-69-6C-65-20-77-69-74-68-20-70-72-6F-76-69-64-65-64-20-61-63-63-65-73-73-20-6D-6F-64-65-20-27-25-73-27
                              )
                            ),
                            α1 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple.empty,
                              α1 ↦ ξ.ρ.ρ.ρ.ρ.ρ.access
                            )
                          )
                        )
                      ⟧
                    ⟧
                  ⟧,
                  read-bytes ↦ ⟦
                    λ ⤍ Lorg_eolang_fs_file_open_file_stream_read_read_bytes,
                    size ↦ ∅
                  ⟧
                ⟧,
                write ↦ ⟦
                  buffer ↦ ∅,
                  φ ↦ ξ.output-block.write(
                    α0 ↦ ξ.buffer
                  ).self,
                  output-block ↦ ⟦
                    self ↦ ξ,
                    φ ↦ Φ.org.eolang.true,
                    write ↦ ⟦
                      buffer ↦ ∅,
                      φ ↦ ξ.ρ.ρ.ρ.ρ.can-write.not.if(
                        α0 ↦ ξ.auto-named-attr-at-257-18,
                        α1 ↦ Φ.org.eolang.seq(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple.empty,
                              α1 ↦ ξ.ρ.ρ.written-bytes(
                                α0 ↦ ξ.buffer
                              )
                            ),
                            α1 ↦ ξ.ρ.ρ.output-block
                          )
                        )
                      ).self,
                      auto-named-attr-at-257-18 ↦ ⟦
                        self ↦ ξ,
                        φ ↦ Φ.org.eolang.error(
                          α0 ↦ Φ.org.eolang.txt.sprintf(
                            α0 ↦ Φ.org.eolang.string(
                              α0 ↦ Φ.org.eolang.bytes(
                                Δ ⤍ 43-61-6E-27-74-20-77-72-69-74-65-20-74-6F-20-66-69-6C-65-20-77-69-74-68-20-70-72-6F-76-69-64-65-64-20-61-63-63-65-73-73-20-6D-6F-64-65-20-27-25-73-27
                              )
                            ),
                            α1 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple.empty,
                              α1 ↦ ξ.ρ.ρ.ρ.ρ.ρ.access
                            )
                          )
                        )
                      ⟧
                    ⟧
                  ⟧,
                  written-bytes ↦ ⟦
                    λ ⤍ Lorg_eolang_fs_file_open_file_stream_write_written_bytes,
                    buffer ↦ ∅
                  ⟧
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
  ⟧
}
```

## [org/eolang/fs/path.phi](./org/eolang/fs/path.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        fs ↦ ⟦
          path ↦ ⟦
            uri ↦ ∅,
            φ ↦ Φ.org.eolang.sys.os.is-windows.if(
              α0 ↦ ξ.win32(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ ξ.uri.as-bytes
                )
              ),
              α1 ↦ ξ.posix(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ ξ.uri.as-bytes
                )
              )
            ).determined,
            joined ↦ ⟦
              paths ↦ ∅,
              joined-path ↦ Φ.org.eolang.txt.text(
                α0 ↦ ξ.ρ.separator
              ).joined(
                α0 ↦ ξ.paths
              ).as-bytes.as-string,
              φ ↦ Φ.org.eolang.sys.os.is-windows.if(
                α0 ↦ ξ.ρ.win32(
                  α0 ↦ ξ.joined-path
                ),
                α1 ↦ ξ.ρ.posix(
                  α0 ↦ ξ.joined-path
                )
              ).normalized
            ⟧,
            separator ↦ ⟦
              φ ↦ Φ.org.eolang.sys.os.is-windows.if(
                α0 ↦ ξ.ρ.win32.separator,
                α1 ↦ ξ.ρ.posix.separator
              )
            ⟧,
            posix ↦ ⟦
              uri ↦ ∅,
              determined ↦ ξ,
              separator ↦ Φ.org.eolang.string(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 2F-
                )
              ),
              as-file ↦ Φ.org.eolang.fs.file(
                α0 ↦ ξ.uri
              ).size.ρ,
              as-dir ↦ Φ.org.eolang.fs.dir(
                α0 ↦ Φ.org.eolang.fs.file(
                  α0 ↦ ξ.uri
                )
              ).made.ρ,
              φ ↦ ξ.uri,
              is-absolute ↦ ⟦
                φ ↦ ξ.ρ.uri.length.gt(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                ).and(
                  α0 ↦ ξ.ρ.uri.as-bytes.slice(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    ),
                    α1 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 3F-F0-00-00-00-00-00-00
                      )
                    )
                  ).eq(
                    α0 ↦ ξ.ρ.separator
                  )
                )
              ⟧,
              normalized ↦ ⟦
                uri-as-bytes ↦ ξ.ρ.uri.as-bytes,
                is-absolute ↦ ξ.ρ.is-absolute.as-bool,
                has-trailing-slash ↦ ξ.uri-as-bytes.size.gt(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                ).and(
                  α0 ↦ ξ.uri-as-bytes.slice(
                    α0 ↦ ξ.uri-as-bytes.size.plus(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ BF-F0-00-00-00-00-00-00
                        )
                      )
                    ),
                    α1 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 3F-F0-00-00-00-00-00-00
                      )
                    )
                  ).eq(
                    α0 ↦ ξ.ρ.separator
                  )
                ),
                path ↦ Φ.org.eolang.txt.text(
                  α0 ↦ ξ.ρ.separator
                ).joined(
                  α0 ↦ Φ.org.eolang.structs.list(
                    α0 ↦ Φ.org.eolang.txt.text(
                      α0 ↦ ξ.ρ.uri
                    ).split(
                      α0 ↦ ξ.ρ.separator
                    )
                  ).reduced(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.auto-named-attr-at-104-25
                  )
                ),
                normalized ↦ ξ.ρ.uri.length.eq(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                ).if(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 2E-
                    )
                  ),
                  α1 ↦ ξ.is-absolute.if(
                    α0 ↦ ξ.ρ.separator.concat(
                      α0 ↦ ξ.path
                    ),
                    α1 ↦ ξ.path
                  ).concat(
                    α0 ↦ ξ.has-trailing-slash.if(
                      α0 ↦ ξ.ρ.separator,
                      α1 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ --
                      )
                    )
                  )
                ).as-bytes,
                φ ↦ ξ.ρ.ρ.posix(
                  α0 ↦ ξ.normalized.eq(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 2F-2F
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 2F-
                      )
                    ),
                    α1 ↦ ξ.normalized
                  ).as-string
                ).determined,
                auto-named-attr-at-104-25 ↦ ⟦
                  accum ↦ ∅,
                  segment ↦ ∅,
                  φ ↦ ξ.segment.eq(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 2E-2E
                      )
                    )
                  ).if(
                    α0 ↦ ξ.accum.length.gt(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                    ).and(
                      α0 ↦ ξ.accum.tail.eq(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 2E-2E
                          )
                        )
                      ).not
                    ).if(
                      α0 ↦ ξ.accum.head,
                      α1 ↦ ξ.ρ.is-absolute.not.if(
                        α0 ↦ ξ.accum.with(
                          α0 ↦ ξ.segment
                        ),
                        α1 ↦ ξ.accum
                      )
                    ),
                    α1 ↦ ξ.segment.eq(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 2E-
                        )
                      )
                    ).or(
                      α0 ↦ ξ.segment.eq(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ --
                          )
                        )
                      )
                    ).if(
                      α0 ↦ ξ.accum,
                      α1 ↦ ξ.accum.with(
                        α0 ↦ ξ.segment
                      )
                    )
                  )
                ⟧
              ⟧,
              resolved ↦ ⟦
                other ↦ ∅,
                other-as-bytes ↦ ξ.other.as-bytes,
                φ ↦ ξ.ρ.ρ.posix(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ ξ.other-as-bytes.slice(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      ),
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    ).eq(
                      α0 ↦ ξ.ρ.separator
                    ).if(
                      α0 ↦ ξ.other-as-bytes,
                      α1 ↦ ξ.ρ.uri.concat(
                        α0 ↦ ξ.ρ.separator
                      ).concat(
                        α0 ↦ ξ.other-as-bytes
                      )
                    )
                  )
                ).normalized
              ⟧,
              basename ↦ ⟦
                pth ↦ Φ.org.eolang.dataized(
                  α0 ↦ ξ.ρ.uri
                ).as-bytes,
                txt ↦ Φ.org.eolang.txt.text(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ ξ.pth
                  )
                ),
                slice-start-idx ↦ Φ.org.eolang.dataized(
                  α0 ↦ ξ.txt.last-index-of(
                    α0 ↦ ξ.ρ.separator
                  ).plus(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 3F-F0-00-00-00-00-00-00
                      )
                    )
                  )
                ).as-bytes,
                φ ↦ Φ.org.eolang.string(
                  α0 ↦ ξ.pth.size.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  ).or(
                    α0 ↦ ξ.slice-start-idx.eq(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                    )
                  ).if(
                    α0 ↦ ξ.pth,
                    α1 ↦ ξ.txt.slice(
                      α0 ↦ ξ.slice-start-idx,
                      α1 ↦ ξ.txt.length.minus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ ξ.slice-start-idx
                        )
                      )
                    ).as-bytes
                  )
                )
              ⟧,
              extname ↦ ⟦
                base ↦ Φ.org.eolang.dataized(
                  α0 ↦ ξ.ρ.basename
                ).as-bytes,
                txt ↦ Φ.org.eolang.txt.text(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ ξ.base
                  )
                ),
                slice-start-idx ↦ Φ.org.eolang.dataized(
                  α0 ↦ ξ.txt.last-index-of(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 2E-
                      )
                    )
                  )
                ).as-bytes,
                φ ↦ ξ.base.size.eq(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                ).or(
                  α0 ↦ ξ.slice-start-idx.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  )
                ).if(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ --
                    )
                  ),
                  α1 ↦ Φ.org.eolang.string(
                    α0 ↦ ξ.txt.slice(
                      α0 ↦ ξ.slice-start-idx,
                      α1 ↦ ξ.txt.length.minus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ ξ.slice-start-idx
                        )
                      )
                    ).as-bytes
                  )
                )
              ⟧,
              dirname ↦ ⟦
                pth ↦ Φ.org.eolang.dataized(
                  α0 ↦ ξ.ρ.uri
                ).as-bytes,
                txt ↦ Φ.org.eolang.txt.text(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ ξ.pth
                  )
                ),
                len ↦ Φ.org.eolang.dataized(
                  α0 ↦ ξ.txt.last-index-of(
                    α0 ↦ ξ.ρ.separator
                  )
                ).as-bytes,
                φ ↦ Φ.org.eolang.string(
                  α0 ↦ ξ.pth.size.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  ).or(
                    α0 ↦ ξ.len.eq(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ BF-F0-00-00-00-00-00-00
                        )
                      )
                    )
                  ).if(
                    α0 ↦ ξ.pth,
                    α1 ↦ ξ.txt.slice(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      ),
                      α1 ↦ ξ.len
                    ).as-bytes
                  )
                )
              ⟧
            ⟧,
            win32 ↦ ⟦
              uri ↦ ∅,
              separator ↦ Φ.org.eolang.string(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 5C-
                )
              ),
              φ ↦ ξ.validated(
                α0 ↦ ξ.validated.separated-correctly(
                  α0 ↦ ξ.uri
                ).as-bytes.as-string
              ).determined,
              validated ↦ ⟦
                uri ↦ ∅,
                determined ↦ ξ,
                separator ↦ ξ.ρ.separator,
                as-file ↦ Φ.org.eolang.fs.file(
                  α0 ↦ ξ.uri
                ).size.ρ,
                as-dir ↦ Φ.org.eolang.fs.dir(
                  α0 ↦ Φ.org.eolang.fs.file(
                    α0 ↦ ξ.uri
                  )
                ).made.ρ,
                φ ↦ ξ.uri,
                is-drive-relative ↦ ⟦
                  φ ↦ Φ.org.eolang.txt.regex(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 2F-5E-5B-61-2D-7A-41-2D-5A-5D-3A-2F
                      )
                    )
                  ).matches(
                    α0 ↦ ξ.uri
                  ).as-bool,
                  uri ↦ ∅
                ⟧,
                is-root-relative ↦ ⟦
                  uri ↦ ∅,
                  uri-as-bytes ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.uri
                  ).as-bytes,
                  φ ↦ ξ.uri-as-bytes.size.gt(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  ).and(
                    α0 ↦ ξ.uri-as-bytes.slice(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      ),
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    ).eq(
                      α0 ↦ ξ.ρ.separator
                    )
                  )
                ⟧,
                separated-correctly ↦ ⟦
                  uri ↦ ∅,
                  uri-as-bytes ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.uri
                  ).as-bytes,
                  pth ↦ Φ.org.eolang.txt.text(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ ξ.uri-as-bytes
                    )
                  ),
                  replaced ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.pth.replaced(
                      α0 ↦ Φ.org.eolang.txt.regex(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 2F-5C-2F-2F
                          )
                        )
                      ),
                      α1 ↦ ξ.ρ.separator
                    )
                  ).as-bytes,
                  φ ↦ ξ.pth.index-of(
                    α0 ↦ ξ.ρ.ρ.ρ.ρ.path.posix.separator
                  ).eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ ξ.uri-as-bytes
                    ),
                    α1 ↦ Φ.org.eolang.string(
                      α0 ↦ ξ.replaced
                    )
                  )
                ⟧,
                is-absolute ↦ ⟦
                  uri-as-bytes ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.ρ.uri
                  ).as-bytes,
                  φ ↦ ξ.uri-as-bytes.size.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.false,
                    α1 ↦ ξ.ρ.is-root-relative(
                      α0 ↦ ξ.uri-as-bytes
                    ).or(
                      α0 ↦ ξ.uri-as-bytes.size.gt(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 3F-F0-00-00-00-00-00-00
                          )
                        )
                      ).and(
                        α0 ↦ ξ.ρ.is-drive-relative(
                          α0 ↦ ξ.uri-as-bytes
                        )
                      )
                    )
                  )
                ⟧,
                normalized ↦ ⟦
                  uri-as-bytes ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.ρ.uri
                  ).as-bytes,
                  is-drive-relative ↦ ξ.ρ.is-drive-relative(
                    α0 ↦ ξ.uri-as-bytes
                  ).as-bool,
                  is-root-relative ↦ ξ.ρ.is-root-relative(
                    α0 ↦ ξ.uri-as-bytes
                  ).as-bool,
                  driveless ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.is-drive-relative.if(
                      α0 ↦ ξ.uri-as-bytes.slice(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 40-00-00-00-00-00-00-00
                          )
                        ),
                        α1 ↦ ξ.uri-as-bytes.size.plus(
                          α0 ↦ Φ.org.eolang.number(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ C0-00-00-00-00-00-00-00
                            )
                          )
                        )
                      ),
                      α1 ↦ ξ.uri-as-bytes
                    )
                  ).as-bytes,
                  has-trailing-slash ↦ ξ.uri-as-bytes.size.gt(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  ).and(
                    α0 ↦ ξ.uri-as-bytes.slice(
                      α0 ↦ ξ.uri-as-bytes.size.plus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ BF-F0-00-00-00-00-00-00
                          )
                        )
                      ),
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    ).eq(
                      α0 ↦ ξ.ρ.separator
                    )
                  ),
                  path ↦ Φ.org.eolang.dataized(
                    α0 ↦ Φ.org.eolang.txt.text(
                      α0 ↦ ξ.ρ.separator
                    ).joined(
                      α0 ↦ Φ.org.eolang.structs.list(
                        α0 ↦ Φ.org.eolang.txt.text(
                          α0 ↦ ξ.driveless
                        ).split(
                          α0 ↦ ξ.ρ.separator
                        )
                      ).reduced(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.auto-named-attr-at-359-27
                      )
                    )
                  ).as-bytes,
                  normalized ↦ ξ.driveless.size.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 2E-
                      )
                    ),
                    α1 ↦ ξ.is-drive-relative.if(
                      α0 ↦ ξ.driveless.slice(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 00-00-00-00-00-00-00-00
                          )
                        ),
                        α1 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 3F-F0-00-00-00-00-00-00
                          )
                        )
                      ).eq(
                        α0 ↦ ξ.ρ.separator
                      ).if(
                        α0 ↦ ξ.ρ.uri.slice(
                          α0 ↦ Φ.org.eolang.number(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ 00-00-00-00-00-00-00-00
                            )
                          ),
                          α1 ↦ Φ.org.eolang.number(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ 40-08-00-00-00-00-00-00
                            )
                          )
                        ),
                        α1 ↦ ξ.ρ.uri.slice(
                          α0 ↦ Φ.org.eolang.number(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ 00-00-00-00-00-00-00-00
                            )
                          ),
                          α1 ↦ Φ.org.eolang.number(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ 40-00-00-00-00-00-00-00
                            )
                          )
                        )
                      ),
                      α1 ↦ ξ.is-root-relative.if(
                        α0 ↦ ξ.ρ.separator,
                        α1 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ --
                        )
                      )
                    ).concat(
                      α0 ↦ ξ.path
                    ).concat(
                      α0 ↦ ξ.has-trailing-slash.if(
                        α0 ↦ ξ.ρ.separator,
                        α1 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ --
                        )
                      )
                    )
                  ).as-bytes,
                  φ ↦ ξ.ρ.ρ.validated(
                    α0 ↦ ξ.normalized.eq(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 5C-5C
                        )
                      )
                    ).if(
                      α0 ↦ ξ.ρ.separator,
                      α1 ↦ ξ.normalized
                    ).as-string
                  ).determined,
                  auto-named-attr-at-359-27 ↦ ⟦
                    accum ↦ ∅,
                    segment ↦ ∅,
                    φ ↦ ξ.segment.eq(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 2E-2E
                        )
                      )
                    ).if(
                      α0 ↦ ξ.accum.length.gt(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 00-00-00-00-00-00-00-00
                          )
                        )
                      ).and(
                        α0 ↦ ξ.accum.tail.eq(
                          α0 ↦ Φ.org.eolang.string(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ 2E-2E
                            )
                          )
                        ).not
                      ).if(
                        α0 ↦ ξ.accum.head,
                        α1 ↦ ξ.ρ.is-root-relative.not.and(
                          α0 ↦ ξ.ρ.is-drive-relative.not
                        ).if(
                          α0 ↦ ξ.accum.with(
                            α0 ↦ ξ.segment
                          ),
                          α1 ↦ ξ.accum
                        )
                      ),
                      α1 ↦ ξ.segment.eq(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 2E-
                          )
                        )
                      ).or(
                        α0 ↦ ξ.segment.eq(
                          α0 ↦ Φ.org.eolang.string(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ --
                            )
                          )
                        )
                      ).if(
                        α0 ↦ ξ.accum,
                        α1 ↦ ξ.accum.with(
                          α0 ↦ ξ.segment
                        )
                      )
                    )
                  ⟧
                ⟧,
                resolved ↦ ⟦
                  other ↦ ∅,
                  uri-as-bytes ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.ρ.uri
                  ).as-bytes,
                  valid-other ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.ρ.separated-correctly(
                      α0 ↦ ξ.other
                    )
                  ).as-bytes,
                  other-is-drive-relative ↦ ξ.ρ.is-drive-relative(
                    α0 ↦ ξ.valid-other
                  ).as-bool,
                  other-is-root-relative ↦ ξ.ρ.is-root-relative(
                    α0 ↦ ξ.valid-other
                  ).as-bool,
                  φ ↦ ξ.ρ.ρ.validated(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ ξ.other-is-drive-relative.if(
                        α0 ↦ ξ.valid-other,
                        α1 ↦ ξ.other-is-root-relative.if(
                          α0 ↦ ξ.ρ.is-drive-relative(
                            α0 ↦ ξ.uri-as-bytes
                          ).if(
                            α0 ↦ ξ.uri-as-bytes.slice(
                              α0 ↦ Φ.org.eolang.number(
                                α0 ↦ Φ.org.eolang.bytes(
                                  Δ ⤍ 00-00-00-00-00-00-00-00
                                )
                              ),
                              α1 ↦ Φ.org.eolang.number(
                                α0 ↦ Φ.org.eolang.bytes(
                                  Δ ⤍ 40-00-00-00-00-00-00-00
                                )
                              )
                            ).concat(
                              α0 ↦ ξ.valid-other
                            ),
                            α1 ↦ ξ.valid-other
                          ),
                          α1 ↦ ξ.uri-as-bytes.concat(
                            α0 ↦ ξ.ρ.separator
                          ).concat(
                            α0 ↦ ξ.valid-other
                          )
                        )
                      )
                    )
                  ).normalized
                ⟧,
                basename ↦ ⟦
                  pth ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.ρ.uri
                  ).as-bytes,
                  txt ↦ Φ.org.eolang.txt.text(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ ξ.pth
                    )
                  ),
                  slice-start-idx ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.txt.last-index-of(
                      α0 ↦ ξ.ρ.separator
                    ).plus(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    )
                  ).as-bytes,
                  φ ↦ Φ.org.eolang.string(
                    α0 ↦ ξ.pth.size.eq(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                    ).or(
                      α0 ↦ ξ.slice-start-idx.eq(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 00-00-00-00-00-00-00-00
                          )
                        )
                      )
                    ).if(
                      α0 ↦ ξ.pth,
                      α1 ↦ ξ.txt.slice(
                        α0 ↦ ξ.slice-start-idx,
                        α1 ↦ ξ.txt.length.minus(
                          α0 ↦ Φ.org.eolang.number(
                            α0 ↦ ξ.slice-start-idx
                          )
                        )
                      ).as-bytes
                    )
                  )
                ⟧,
                extname ↦ ⟦
                  base ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.ρ.basename
                  ).as-bytes,
                  txt ↦ Φ.org.eolang.txt.text(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ ξ.base
                    )
                  ),
                  slice-start-idx ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.txt.last-index-of(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 2E-
                        )
                      )
                    )
                  ).as-bytes,
                  φ ↦ ξ.base.size.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  ).or(
                    α0 ↦ ξ.slice-start-idx.eq(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ BF-F0-00-00-00-00-00-00
                        )
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ --
                      )
                    ),
                    α1 ↦ Φ.org.eolang.string(
                      α0 ↦ ξ.txt.slice(
                        α0 ↦ ξ.slice-start-idx,
                        α1 ↦ ξ.txt.length.minus(
                          α0 ↦ Φ.org.eolang.number(
                            α0 ↦ ξ.slice-start-idx
                          )
                        )
                      ).as-bytes
                    )
                  )
                ⟧,
                dirname ↦ ⟦
                  pth ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.ρ.uri
                  ).as-bytes,
                  txt ↦ Φ.org.eolang.txt.text(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ ξ.pth
                    )
                  ),
                  len ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.txt.last-index-of(
                      α0 ↦ ξ.ρ.separator
                    )
                  ).as-bytes,
                  φ ↦ Φ.org.eolang.string(
                    α0 ↦ ξ.pth.size.eq(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                    ).or(
                      α0 ↦ ξ.len.eq(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ BF-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    ).if(
                      α0 ↦ ξ.pth,
                      α1 ↦ ξ.txt.slice(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 00-00-00-00-00-00-00-00
                          )
                        ),
                        α1 ↦ ξ.len
                      ).as-bytes
                    )
                  )
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
  ⟧
}
```

## [org/eolang/fs/tmpdir.phi](./org/eolang/fs/tmpdir.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        fs ↦ ⟦
          tmpdir ↦ ⟦
            φ ↦ Φ.org.eolang.fs.dir(
              α0 ↦ Φ.org.eolang.fs.file(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ ξ.os-tmp-dir
                )
              )
            ),
            os-tmp-dir ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.os-tmp-dir-1
            ).as-bytes,
            os-tmp-dir-1 ↦ ⟦
              tmpdir ↦ Φ.org.eolang.dataized(
                α0 ↦ Φ.org.eolang.sys.getenv(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 54-4D-50-44-49-52
                    )
                  )
                )
              ).as-bytes,
              tmp ↦ Φ.org.eolang.dataized(
                α0 ↦ Φ.org.eolang.sys.getenv(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 54-4D-50
                    )
                  )
                )
              ).as-bytes,
              temp ↦ Φ.org.eolang.dataized(
                α0 ↦ Φ.org.eolang.sys.getenv(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 54-45-4D-50
                    )
                  )
                )
              ).as-bytes,
              tempdir ↦ Φ.org.eolang.dataized(
                α0 ↦ Φ.org.eolang.sys.getenv(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 54-45-4D-50-44-49-52
                    )
                  )
                )
              ).as-bytes,
              userprofile ↦ Φ.org.eolang.dataized(
                α0 ↦ Φ.org.eolang.sys.getenv(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 55-53-45-52-50-52-4F-46-49-4C-45
                    )
                  )
                )
              ).as-bytes,
              φ ↦ Φ.org.eolang.sys.os.is-windows.if(
                α0 ↦ ξ.tmp.eq(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ --
                    )
                  )
                ).if(
                  α0 ↦ ξ.temp.eq(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ --
                      )
                    )
                  ).if(
                    α0 ↦ ξ.userprofile.eq(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ --
                        )
                      )
                    ).if(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 43-3A-5C-57-69-6E-64-6F-77-73
                        )
                      ),
                      α1 ↦ ξ.userprofile
                    ),
                    α1 ↦ ξ.temp
                  ),
                  α1 ↦ ξ.tmp
                ),
                α1 ↦ ξ.tmpdir.eq(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ --
                    )
                  )
                ).if(
                  α0 ↦ ξ.tmp.eq(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ --
                      )
                    )
                  ).if(
                    α0 ↦ ξ.temp.eq(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ --
                        )
                      )
                    ).if(
                      α0 ↦ ξ.tempdir.eq(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ --
                          )
                        )
                      ).if(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 2F-74-6D-70
                          )
                        ),
                        α1 ↦ ξ.tempdir
                      ),
                      α1 ↦ ξ.temp
                    ),
                    α1 ↦ ξ.tmp
                  ),
                  α1 ↦ ξ.tmpdir
                )
              )
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

## [org/eolang/go.phi](./org/eolang/go.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        go ↦ ⟦
          id ↦ Φ.org.eolang.dataized(
            α0 ↦ Φ.org.eolang.malloc.of(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-20-00-00-00-00-00-00
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
              α1 ↦ ξ.auto-named-attr-at-65-9,
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
            auto-named-attr-at-65-9 ↦ ⟦
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

## [org/eolang/i16.phi](./org/eolang/i16.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        i16 ↦ ⟦
          as-bytes ↦ ∅,
          φ ↦ ξ.as-bytes,
          as-i16 ↦ ξ,
          neg ↦ ξ.times(
            α0 ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ BF-F0-00-00-00-00-00-00
              )
            ).as-i64.as-i32.as-i16
          ),
          as-i64 ↦ ξ.as-i32.as-i64,
          as-number ↦ ξ.as-i64.as-number,
          as-i32 ↦ ⟦
            λ ⤍ Lorg_eolang_i16_as_i32
          ⟧,
          lt ↦ ⟦
            φ ↦ ξ.ρ.as-i32.lt(
              α0 ↦ ξ.x.as-i16.as-i32
            ),
            x ↦ ∅
          ⟧,
          lte ↦ ⟦
            φ ↦ ξ.ρ.as-i32.lte(
              α0 ↦ ξ.x.as-i16.as-i32
            ),
            x ↦ ∅
          ⟧,
          gt ↦ ⟦
            φ ↦ ξ.ρ.as-i32.gt(
              α0 ↦ ξ.x.as-i16.as-i32
            ),
            x ↦ ∅
          ⟧,
          gte ↦ ⟦
            φ ↦ ξ.ρ.as-i32.gte(
              α0 ↦ ξ.x.as-i16.as-i32
            ),
            x ↦ ∅
          ⟧,
          times ↦ ⟦
            x ↦ ∅,
            bts ↦ ξ.ρ.as-i32.times(
              α0 ↦ ξ.x.as-i16.as-i32
            ).as-bytes,
            left ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-00-00-00-00-00-00-00
                )
              )
            ),
            right ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-00-00-00-00-00-00-00
                )
              )
            ),
            φ ↦ ξ.left.eq(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00
              )
            ).or(
              α0 ↦ ξ.left.eq(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ FF-FF
                )
              )
            ).if(
              α0 ↦ ξ.ρ.ρ.i16(
                α0 ↦ ξ.right
              ),
              α1 ↦ ξ.ρ.ρ.i16(
                α0 ↦ ξ.left
              ).plus(
                α0 ↦ ξ.ρ.ρ.i16(
                  α0 ↦ ξ.right
                )
              )
            )
          ⟧,
          plus ↦ ⟦
            x ↦ ∅,
            bts ↦ ξ.ρ.as-i32.plus(
              α0 ↦ ξ.x.as-i16.as-i32
            ).as-bytes,
            left ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-00-00-00-00-00-00-00
                )
              )
            ),
            right ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-00-00-00-00-00-00-00
                )
              )
            ),
            φ ↦ ξ.left.eq(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00
              )
            ).or(
              α0 ↦ ξ.left.eq(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ FF-FF
                )
              )
            ).if(
              α0 ↦ ξ.ρ.ρ.i16(
                α0 ↦ ξ.right
              ),
              α1 ↦ ξ.ρ.ρ.i16(
                α0 ↦ ξ.left
              ).plus(
                α0 ↦ ξ.ρ.ρ.i16(
                  α0 ↦ ξ.right
                )
              )
            )
          ⟧,
          minus ↦ ⟦
            φ ↦ ξ.ρ.plus(
              α0 ↦ ξ.x.as-i16.neg
            ),
            x ↦ ∅
          ⟧,
          div ↦ ⟦
            x ↦ ∅,
            x-as-i16 ↦ ξ.x.as-i16,
            bts ↦ ξ.ρ.as-i32.div(
              α0 ↦ ξ.x-as-i16.as-i32
            ).as-bytes,
            left ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-00-00-00-00-00-00-00
                )
              )
            ),
            right ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-00-00-00-00-00-00-00
                )
              )
            ),
            zero ↦ Φ.org.eolang.bytes(
              Δ ⤍ 00-00
            ),
            φ ↦ ξ.x-as-i16.eq(
              α0 ↦ ξ.zero
            ).if(
              α0 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.txt.sprintf(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 43-61-6E-27-74-20-64-69-76-69-64-65-20-25-64-20-62-79-20-69-31-36-20-7A-65-72-6F
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.ρ.as-i32.as-i64.as-number
                  )
                )
              ),
              α1 ↦ ξ.left.eq(
                α0 ↦ ξ.zero
              ).or(
                α0 ↦ ξ.left.eq(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ FF-FF
                  )
                )
              ).if(
                α0 ↦ ξ.ρ.ρ.i16(
                  α0 ↦ ξ.right
                ),
                α1 ↦ ξ.ρ.ρ.i16(
                  α0 ↦ ξ.left
                ).plus(
                  α0 ↦ ξ.ρ.ρ.i16(
                    α0 ↦ ξ.right
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

## [org/eolang/i32.phi](./org/eolang/i32.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        i32 ↦ ⟦
          as-bytes ↦ ∅,
          φ ↦ ξ.as-bytes,
          as-i32 ↦ ξ,
          neg ↦ ξ.times(
            α0 ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ BF-F0-00-00-00-00-00-00
              )
            ).as-i64.as-i32
          ),
          as-number ↦ ξ.as-i64.as-number,
          as-i64 ↦ ⟦
            λ ⤍ Lorg_eolang_i32_as_i64
          ⟧,
          as-i16 ↦ ⟦
            left ↦ ξ.ρ.as-bytes.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-00-00-00-00-00-00-00
                )
              )
            ).as-bytes,
            φ ↦ ξ.left.eq(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00
              )
            ).or(
              α0 ↦ ξ.left.eq(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ FF-FF
                )
              )
            ).if(
              α0 ↦ Φ.org.eolang.i16(
                α0 ↦ ξ.ρ.as-bytes.slice(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 40-00-00-00-00-00-00-00
                    )
                  ),
                  α1 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 40-00-00-00-00-00-00-00
                    )
                  )
                )
              ),
              α1 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.txt.sprintf(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-69-33-32-20-6E-75-6D-62-65-72-20-25-64-20-74-6F-20-69-31-36-20-62-65-63-61-75-73-65-20-69-74-27-73-20-6F-75-74-20-6F-66-20-69-31-36-20-62-6F-75-6E-64-73
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.ρ.as-i64.as-number
                  )
                )
              )
            )
          ⟧,
          lt ↦ ⟦
            φ ↦ ξ.ρ.as-i64.lt(
              α0 ↦ ξ.x.as-i32.as-i64
            ),
            x ↦ ∅
          ⟧,
          lte ↦ ⟦
            φ ↦ ξ.ρ.as-i64.lte(
              α0 ↦ ξ.x.as-i32.as-i64
            ),
            x ↦ ∅
          ⟧,
          gt ↦ ⟦
            φ ↦ ξ.ρ.as-i64.gt(
              α0 ↦ ξ.x.as-i32.as-i64
            ),
            x ↦ ∅
          ⟧,
          gte ↦ ⟦
            φ ↦ ξ.ρ.as-i64.gte(
              α0 ↦ ξ.x.as-i32.as-i64
            ),
            x ↦ ∅
          ⟧,
          times ↦ ⟦
            x ↦ ∅,
            bts ↦ ξ.ρ.as-i64.times(
              α0 ↦ ξ.x.as-i32.as-i64
            ).as-bytes,
            left ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-10-00-00-00-00-00-00
                )
              )
            ),
            right ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-10-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-10-00-00-00-00-00-00
                )
              )
            ),
            φ ↦ ξ.left.eq(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00
              )
            ).or(
              α0 ↦ ξ.left.eq(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ FF-FF-FF-FF
                )
              )
            ).if(
              α0 ↦ ξ.ρ.ρ.i32(
                α0 ↦ ξ.right
              ),
              α1 ↦ ξ.ρ.ρ.i32(
                α0 ↦ ξ.left
              ).plus(
                α0 ↦ ξ.ρ.ρ.i32(
                  α0 ↦ ξ.right
                )
              )
            )
          ⟧,
          plus ↦ ⟦
            x ↦ ∅,
            bts ↦ ξ.ρ.as-i64.plus(
              α0 ↦ ξ.x.as-i32.as-i64
            ).as-bytes,
            left ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-10-00-00-00-00-00-00
                )
              )
            ),
            right ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-10-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-10-00-00-00-00-00-00
                )
              )
            ),
            φ ↦ ξ.left.eq(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00
              )
            ).or(
              α0 ↦ ξ.left.eq(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ FF-FF-FF-FF
                )
              )
            ).if(
              α0 ↦ ξ.ρ.ρ.i32(
                α0 ↦ ξ.right
              ),
              α1 ↦ ξ.ρ.ρ.i32(
                α0 ↦ ξ.left
              ).plus(
                α0 ↦ ξ.ρ.ρ.i32(
                  α0 ↦ ξ.right
                )
              )
            )
          ⟧,
          minus ↦ ⟦
            φ ↦ ξ.ρ.plus(
              α0 ↦ ξ.x.as-i32.neg
            ),
            x ↦ ∅
          ⟧,
          div ↦ ⟦
            x ↦ ∅,
            x-as-i32 ↦ ξ.x.as-i32,
            bts ↦ ξ.ρ.as-i64.div(
              α0 ↦ ξ.x-as-i32.as-i64
            ).as-bytes,
            left ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-10-00-00-00-00-00-00
                )
              )
            ),
            right ↦ ξ.bts.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-10-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-10-00-00-00-00-00-00
                )
              )
            ),
            zero ↦ Φ.org.eolang.bytes(
              Δ ⤍ 00-00-00-00
            ),
            φ ↦ ξ.x-as-i32.eq(
              α0 ↦ ξ.zero
            ).if(
              α0 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.txt.sprintf(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 43-61-6E-27-74-20-64-69-76-69-64-65-20-25-64-20-62-79-20-69-33-32-20-7A-65-72-6F
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.ρ.as-i64.as-number
                  )
                )
              ),
              α1 ↦ ξ.left.eq(
                α0 ↦ ξ.zero
              ).or(
                α0 ↦ ξ.left.eq(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ FF-FF-FF-FF
                  )
                )
              ).if(
                α0 ↦ ξ.ρ.ρ.i32(
                  α0 ↦ ξ.right
                ),
                α1 ↦ ξ.ρ.ρ.i32(
                  α0 ↦ ξ.left
                ).plus(
                  α0 ↦ ξ.ρ.ρ.i32(
                    α0 ↦ ξ.right
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

## [org/eolang/i64.phi](./org/eolang/i64.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        i64 ↦ ⟦
          as-bytes ↦ ∅,
          φ ↦ ξ.as-bytes,
          as-i64 ↦ ξ,
          neg ↦ ξ.times(
            α0 ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ BF-F0-00-00-00-00-00-00
              )
            ).as-i64
          ),
          as-i16 ↦ ξ.as-i32.as-i16,
          as-i32 ↦ ⟦
            left ↦ ξ.ρ.as-bytes.slice(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ),
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-10-00-00-00-00-00-00
                )
              )
            ).as-bytes,
            φ ↦ ξ.left.eq(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00
              )
            ).or(
              α0 ↦ ξ.left.eq(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ FF-FF-FF-FF
                )
              )
            ).if(
              α0 ↦ Φ.org.eolang.i32(
                α0 ↦ ξ.ρ.as-bytes.slice(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 40-10-00-00-00-00-00-00
                    )
                  ),
                  α1 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 40-10-00-00-00-00-00-00
                    )
                  )
                )
              ),
              α1 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.txt.sprintf(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-69-36-34-20-6E-75-6D-62-65-72-20-25-64-20-74-6F-20-69-33-32-20-62-65-63-61-75-73-65-20-69-74-27-73-20-6F-75-74-20-6F-66-20-69-33-32-20-62-6F-75-6E-64-73
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.ρ.as-number
                  )
                )
              )
            )
          ⟧,
          as-number ↦ ⟦
            λ ⤍ Lorg_eolang_i64_as_number
          ⟧,
          lt ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ).as-i64.gt(
              α0 ↦ ξ.ρ.minus(
                α0 ↦ ξ.ρ.ρ.i64(
                  α0 ↦ ξ.value
                )
              )
            )
          ⟧,
          lte ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ ξ.ρ.lt(
              α0 ↦ ξ.value
            ).or(
              α0 ↦ ξ.ρ.eq(
                α0 ↦ ξ.value
              )
            )
          ⟧,
          gt ↦ ⟦
            λ ⤍ Lorg_eolang_i64_gt,
            x ↦ ∅
          ⟧,
          gte ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ ξ.ρ.gt(
              α0 ↦ ξ.value
            ).or(
              α0 ↦ ξ.ρ.eq(
                α0 ↦ ξ.value
              )
            )
          ⟧,
          times ↦ ⟦
            λ ⤍ Lorg_eolang_i64_times,
            x ↦ ∅
          ⟧,
          plus ↦ ⟦
            λ ⤍ Lorg_eolang_i64_plus,
            x ↦ ∅
          ⟧,
          minus ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ ξ.ρ.plus(
              α0 ↦ ξ.ρ.ρ.i64(
                α0 ↦ ξ.value
              ).neg
            )
          ⟧,
          div ↦ ⟦
            λ ⤍ Lorg_eolang_i64_div,
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

## [org/eolang/io/bytes-as-input.phi](./org/eolang/io/bytes-as-input.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        io ↦ ⟦
          bytes-as-input ↦ ⟦
            bts ↦ ∅,
            read ↦ ⟦
              size ↦ ∅,
              φ ↦ ξ.input-block(
                α0 ↦ ξ.ρ.bts,
                α1 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ --
                )
              ).read(
                α0 ↦ ξ.size
              ).self,
              input-block ↦ ⟦
                data ↦ ∅,
                buffer ↦ ∅,
                self ↦ ξ,
                φ ↦ ξ.buffer,
                read ↦ ⟦
                  size ↦ ∅,
                  to-read ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.size
                  ).as-bytes,
                  available ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.ρ.data.size
                  ).as-bytes,
                  next ↦ Φ.org.eolang.dataized(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ ξ.available
                    ).gt(
                      α0 ↦ ξ.to-read
                    ).if(
                      α0 ↦ ξ.to-read,
                      α1 ↦ ξ.available
                    )
                  ).as-bytes,
                  φ ↦ ξ.available.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ ξ.ρ.ρ.input-block(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ --
                      ),
                      α1 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ --
                      )
                    ),
                    α1 ↦ ξ.ρ.ρ.input-block(
                      α0 ↦ ξ.ρ.data.slice(
                        α0 ↦ ξ.next,
                        α1 ↦ Φ.org.eolang.number(
                          α0 ↦ ξ.available
                        ).minus(
                          α0 ↦ Φ.org.eolang.number(
                            α0 ↦ ξ.next
                          )
                        )
                      ).as-bytes,
                      α1 ↦ ξ.ρ.data.slice(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 00-00-00-00-00-00-00-00
                          )
                        ),
                        α1 ↦ ξ.next
                      ).as-bytes
                    )
                  ).self
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
  ⟧
}
```

## [org/eolang/io/console.phi](./org/eolang/io/console.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        io ↦ ⟦
          console ↦ ⟦
            φ ↦ Φ.org.eolang.sys.os.is-windows.if(
              α0 ↦ ξ.windows-console,
              α1 ↦ ξ.posix-console
            ).platform,
            posix-console ↦ ⟦
              platform ↦ ξ,
              read ↦ ⟦
                size ↦ ∅,
                φ ↦ ξ.input-block(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ --
                  )
                ).read(
                  α0 ↦ ξ.size
                ).self,
                input-block ↦ ⟦
                  buffer ↦ ∅,
                  self ↦ ξ,
                  φ ↦ ξ.buffer,
                  read ↦ ⟦
                    size ↦ ∅,
                    read-bytes ↦ Φ.org.eolang.dataized(
                      α0 ↦ Φ.org.eolang.sys.posix(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 72-65-61-64
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ Φ.org.eolang.sys.posix.stdin-fileno
                          ),
                          α1 ↦ ξ.size
                        )
                      ).output
                    ).as-bytes,
                    φ ↦ Φ.org.eolang.seq(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.read-bytes
                        ),
                        α1 ↦ ξ.ρ.ρ.input-block(
                          α0 ↦ ξ.read-bytes
                        )
                      )
                    ).self
                  ⟧
                ⟧
              ⟧,
              write ↦ ⟦
                buffer ↦ ∅,
                φ ↦ ξ.output-block.write(
                  α0 ↦ ξ.buffer
                ).self,
                output-block ↦ ⟦
                  self ↦ ξ,
                  φ ↦ Φ.org.eolang.true,
                  write ↦ ⟦
                    buffer ↦ ∅,
                    φ ↦ Φ.org.eolang.seq(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ Φ.org.eolang.sys.posix(
                            α0 ↦ Φ.org.eolang.string(
                              α0 ↦ Φ.org.eolang.bytes(
                                Δ ⤍ 77-72-69-74-65
                              )
                            ),
                            α1 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple(
                                α0 ↦ Φ.org.eolang.tuple(
                                  α0 ↦ Φ.org.eolang.tuple.empty,
                                  α1 ↦ Φ.org.eolang.sys.posix.stdout-fileno
                                ),
                                α1 ↦ ξ.buffer
                              ),
                              α1 ↦ ξ.buffer.size
                            )
                          ).code
                        ),
                        α1 ↦ ξ.ρ.ρ.output-block
                      )
                    ).self
                  ⟧
                ⟧
              ⟧
            ⟧,
            windows-console ↦ ⟦
              platform ↦ ξ,
              read ↦ ⟦
                size ↦ ∅,
                φ ↦ ξ.input-block(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ --
                  )
                ).read(
                  α0 ↦ ξ.size
                ).self,
                input-block ↦ ⟦
                  buffer ↦ ∅,
                  self ↦ ξ,
                  φ ↦ ξ.buffer,
                  read ↦ ⟦
                    size ↦ ∅,
                    read-bytes ↦ Φ.org.eolang.dataized(
                      α0 ↦ Φ.org.eolang.sys.win32(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 52-65-61-64-46-69-6C-65
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ Φ.org.eolang.sys.win32.std-input-handle
                          ),
                          α1 ↦ ξ.size
                        )
                      ).output
                    ).as-bytes,
                    φ ↦ Φ.org.eolang.seq(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.read-bytes
                        ),
                        α1 ↦ ξ.ρ.ρ.input-block(
                          α0 ↦ ξ.read-bytes
                        )
                      )
                    ).self
                  ⟧
                ⟧
              ⟧,
              write ↦ ⟦
                buffer ↦ ∅,
                φ ↦ ξ.output-block.write(
                  α0 ↦ ξ.buffer
                ).self,
                output-block ↦ ⟦
                  self ↦ ξ,
                  φ ↦ Φ.org.eolang.true,
                  write ↦ ⟦
                    buffer ↦ ∅,
                    φ ↦ Φ.org.eolang.seq(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ Φ.org.eolang.sys.win32(
                            α0 ↦ Φ.org.eolang.string(
                              α0 ↦ Φ.org.eolang.bytes(
                                Δ ⤍ 57-72-69-74-65-46-69-6C-65
                              )
                            ),
                            α1 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple(
                                α0 ↦ Φ.org.eolang.tuple(
                                  α0 ↦ Φ.org.eolang.tuple.empty,
                                  α1 ↦ Φ.org.eolang.sys.win32.std-output-handle
                                ),
                                α1 ↦ ξ.buffer
                              ),
                              α1 ↦ ξ.buffer.size
                            )
                          ).code
                        ),
                        α1 ↦ ξ.ρ.ρ.output-block
                      )
                    ).self
                  ⟧
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
  ⟧
}
```

## [org/eolang/io/dead-input.phi](./org/eolang/io/dead-input.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        io ↦ ⟦
          dead-input ↦ ⟦
            read ↦ ⟦
              size ↦ ∅,
              φ ↦ ξ.input-block,
              input-block ↦ ⟦
                φ ↦ Φ.org.eolang.bytes(
                  Δ ⤍ --
                ),
                read ↦ ⟦
                  φ ↦ ξ.ρ.ρ.input-block,
                  size ↦ ∅
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
  ⟧
}
```

## [org/eolang/io/dead-output.phi](./org/eolang/io/dead-output.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        io ↦ ⟦
          dead-output ↦ ⟦
            write ↦ ⟦
              buffer ↦ ∅,
              φ ↦ ξ.output-block,
              output-block ↦ ⟦
                φ ↦ Φ.org.eolang.true,
                write ↦ ⟦
                  φ ↦ ξ.ρ.ρ.output-block,
                  buffer ↦ ∅
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
  ⟧
}
```

## [org/eolang/io/input-length.phi](./org/eolang/io/input-length.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        io ↦ ⟦
          input-length ↦ ⟦
            input ↦ ∅,
            chunk ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 40-B0-00-00-00-00-00-00
              )
            ),
            φ ↦ ξ.rec-read(
              α0 ↦ ξ.input,
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
            ),
            rec-read ↦ ⟦
              input ↦ ∅,
              length ↦ ∅,
              read-bytes ↦ ξ.input.read(
                α0 ↦ ξ.ρ.chunk
              ).read.ρ,
              φ ↦ ξ.read-bytes.size.eq(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ ξ.length,
                α1 ↦ ξ.ρ.rec-read(
                  α0 ↦ ξ.read-bytes,
                  α1 ↦ ξ.length.plus(
                    α0 ↦ ξ.read-bytes.size
                  )
                )
              )
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

## [org/eolang/io/malloc-as-output.phi](./org/eolang/io/malloc-as-output.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        io ↦ ⟦
          malloc-as-output ↦ ⟦
            allocated ↦ ∅,
            write ↦ ⟦
              buffer ↦ ∅,
              φ ↦ ξ.output-block(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).write(
                α0 ↦ ξ.buffer
              ).self,
              output-block ↦ ⟦
                offset ↦ ∅,
                self ↦ ξ,
                φ ↦ Φ.org.eolang.true,
                write ↦ ⟦
                  buffer ↦ ∅,
                  φ ↦ Φ.org.eolang.seq(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.ρ.ρ.ρ.allocated.write(
                          α0 ↦ ξ.ρ.offset,
                          α1 ↦ ξ.buffer
                        )
                      ),
                      α1 ↦ ξ.ρ.ρ.output-block(
                        α0 ↦ ξ.ρ.offset.plus(
                          α0 ↦ ξ.buffer.size
                        )
                      )
                    )
                  ).self
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
            φ ↦ ξ.all-lines,
            all-lines ↦ ⟦
              φ ↦ ξ.rec-read(
                α0 ↦ ξ.ρ.next-line,
                α1 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ --
                ),
                α2 ↦ Φ.org.eolang.true
              ),
              separator ↦ Φ.org.eolang.dataized(
                α0 ↦ Φ.org.eolang.sys.line-separator
              ).as-bytes,
              rec-read ↦ ⟦
                line ↦ ∅,
                buffer ↦ ∅,
                first ↦ ∅,
                φ ↦ ξ.line.length.eq(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                ).if(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ ξ.buffer
                  ),
                  α1 ↦ ξ.ρ.rec-read(
                    α0 ↦ ξ.ρ.ρ.next-line,
                    α1 ↦ ξ.first.if(
                      α0 ↦ ξ.buffer.concat(
                        α0 ↦ ξ.line
                      ),
                      α1 ↦ ξ.buffer.concat(
                        α0 ↦ ξ.ρ.separator
                      ).concat(
                        α0 ↦ ξ.line
                      )
                    ),
                    α2 ↦ Φ.org.eolang.false
                  )
                )
              ⟧
            ⟧,
            next-line ↦ ⟦
              first ↦ Φ.org.eolang.io.console.read(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 3F-F0-00-00-00-00-00-00
                  )
                )
              ).self,
              φ ↦ ξ.first.as-bytes.size.eq(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ --
                  )
                ),
                α1 ↦ ξ.rec-read(
                  α0 ↦ ξ.first,
                  α1 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ --
                  )
                )
              ),
              rec-read ↦ ⟦
                input ↦ ∅,
                buffer ↦ ∅,
                char ↦ ξ.input.as-bytes,
                next ↦ ξ.input.read(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 3F-F0-00-00-00-00-00-00
                    )
                  )
                ).self,
                φ ↦ ξ.char.eq(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ --
                  )
                ).or(
                  α0 ↦ ξ.char.eq(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 0D-
                      )
                    )
                  ).and(
                    α0 ↦ ξ.next.as-bytes.eq(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 0A-
                        )
                      )
                    )
                  ).or(
                    α0 ↦ ξ.char.eq(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 0A-
                        )
                      )
                    )
                  )
                ).if(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ ξ.buffer
                  ),
                  α1 ↦ ξ.ρ.rec-read(
                    α0 ↦ ξ.next,
                    α1 ↦ ξ.buffer.concat(
                      α0 ↦ ξ.char
                    )
                  )
                )
              ⟧
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
            text ↦ ∅,
            φ ↦ Φ.org.eolang.seq(
              α0 ↦ Φ.org.eolang.tuple(
                α0 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ Φ.org.eolang.io.console.write(
                    α0 ↦ ξ.text
                  )
                ),
                α1 ↦ Φ.org.eolang.true
              )
            )
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

## [org/eolang/io/tee-input.phi](./org/eolang/io/tee-input.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        io ↦ ⟦
          tee-input ↦ ⟦
            input ↦ ∅,
            output ↦ ∅,
            read ↦ ⟦
              size ↦ ∅,
              φ ↦ ξ.input-block(
                α0 ↦ ξ.ρ.input,
                α1 ↦ ξ.ρ.output,
                α2 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ --
                )
              ).read(
                α0 ↦ ξ.size
              ).self,
              input-block ↦ ⟦
                input ↦ ∅,
                output ↦ ∅,
                buffer ↦ ∅,
                self ↦ ξ,
                φ ↦ ξ.buffer,
                read ↦ ⟦
                  size ↦ ∅,
                  read-bytes ↦ ξ.ρ.input.read(
                    α0 ↦ ξ.size
                  ).read.ρ,
                  written-bytes ↦ ξ.ρ.output.write(
                    α0 ↦ ξ.read-bytes
                  ).write.ρ,
                  φ ↦ Φ.org.eolang.seq(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.written-bytes
                      ),
                      α1 ↦ ξ.ρ.ρ.input-block(
                        α0 ↦ ξ.read-bytes,
                        α1 ↦ ξ.written-bytes,
                        α2 ↦ ξ.read-bytes.as-bytes
                      )
                    )
                  ).self
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
              α1 ↦ ξ.auto-named-attr-at-90-9
            ),
            auto-named-attr-at-90-9 ↦ ⟦
              m ↦ ∅,
              φ ↦ Φ.org.eolang.seq(
                α0 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.m.write(
                      α0 ↦ Φ.org.eolang.number(
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
              φ ↦ ξ.get,
              get ↦ ξ.read(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ),
                α1 ↦ ξ.size
              ),
              size ↦ ⟦
                λ ⤍ Lorg_eolang_malloc_of_allocated_size
              ⟧,
              resize ↦ ⟦
                λ ⤍ Lorg_eolang_malloc_of_allocated_resize,
                size ↦ ∅
              ⟧,
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
              put ↦ ⟦
                object ↦ ∅,
                φ ↦ Φ.org.eolang.seq(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ ξ.ρ.write(
                        α0 ↦ Φ.org.eolang.number(
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

## [org/eolang/math/angle.phi](./org/eolang/math/angle.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        math ↦ ⟦
          angle ↦ ⟦
            value ↦ ∅,
            φ ↦ ξ.value,
            in-degrees ↦ ξ.ρ.angle(
              α0 ↦ ξ.ρ.times(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 40-66-80-00-00-00-00-00
                  )
                )
              ).div(
                α0 ↦ Φ.org.eolang.math.pi
              )
            ),
            in-radians ↦ ξ.ρ.angle(
              α0 ↦ ξ.times(
                α0 ↦ Φ.org.eolang.math.pi
              ).div(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 40-66-80-00-00-00-00-00
                  )
                )
              )
            ),
            sin ↦ ⟦
              λ ⤍ Lorg_eolang_math_angle_sin
            ⟧,
            cos ↦ ⟦
              λ ⤍ Lorg_eolang_math_angle_cos
            ⟧,
            tan ↦ ⟦
              cosine ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.cos
              ).as-bytes,
              φ ↦ ξ.cosine.eq(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ Φ.org.eolang.nan,
                α1 ↦ ξ.ρ.sin.div(
                  α0 ↦ ξ.cosine
                )
              )
            ⟧,
            ctan ↦ ⟦
              sine ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.sin
              ).as-bytes,
              φ ↦ ξ.sine.eq(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ Φ.org.eolang.nan,
                α1 ↦ ξ.ρ.cos.div(
                  α0 ↦ ξ.sine
                )
              )
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

## [org/eolang/math/e.phi](./org/eolang/math/e.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        math ↦ ⟦
          e ↦ Φ.org.eolang.number(
            α0 ↦ Φ.org.eolang.bytes(
              Δ ⤍ 40-05-BF-0A-8B-14-57-69
            )
          ),
          λ ⤍ Package
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/math/integral.phi](./org/eolang/math/integral.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        math ↦ ⟦
          integral ↦ ⟦
            fun ↦ ∅,
            a ↦ ∅,
            b ↦ ∅,
            n ↦ ∅,
            subsection ↦ ⟦
              a ↦ ∅,
              b ↦ ∅,
              φ ↦ ξ.b.minus(
                α0 ↦ ξ.a
              ).div(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 40-18-00-00-00-00-00-00
                  )
                )
              ).times(
                α0 ↦ ξ.ρ.fun(
                  α0 ↦ ξ.a
                ).plus(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 40-10-00-00-00-00-00-00
                    )
                  ).times(
                    α0 ↦ ξ.ρ.fun(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-E0-00-00-00-00-00-00
                        )
                      ).times(
                        α0 ↦ ξ.a.plus(
                          α0 ↦ ξ.b
                        )
                      )
                    )
                  ).plus(
                    α0 ↦ ξ.ρ.fun(
                      α0 ↦ ξ.b
                    )
                  )
                )
              )
            ⟧,
            φ ↦ Φ.org.eolang.malloc.of(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-20-00-00-00-00-00-00
                )
              ),
              α1 ↦ ξ.auto-named-attr-at-52-11
            ).as-number,
            auto-named-attr-at-52-11 ↦ ⟦
              sum ↦ ∅,
              φ ↦ Φ.org.eolang.malloc.for(
                α0 ↦ ξ.ρ.a,
                α1 ↦ ξ.auto-named-attr-at-55-16
              ),
              auto-named-attr-at-55-16 ↦ ⟦
                left ↦ ∅,
                right ↦ ξ.ρ.ρ.b,
                step ↦ ξ.right.minus(
                  α0 ↦ ξ.left
                ).div(
                  α0 ↦ ξ.ρ.ρ.n
                ).as-number,
                φ ↦ Φ.org.eolang.while(
                  α0 ↦ ξ.auto-named-attr-at-59-17,
                  α1 ↦ ⟦
                    φ ↦ Φ.org.eolang.true,
                    i ↦ ∅
                  ⟧
                ),
                auto-named-attr-at-59-17 ↦ ⟦
                  i ↦ ∅,
                  φ ↦ ξ.ρ.left.as-number.plus(
                    α0 ↦ ξ.ρ.step
                  ).lt(
                    α0 ↦ ξ.ρ.right
                  ).if(
                    α0 ↦ Φ.org.eolang.seq(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ ξ.ρ.ρ.sum.put(
                              α0 ↦ ξ.ρ.ρ.sum.as-number.plus(
                                α0 ↦ ξ.ρ.ρ.ρ.subsection(
                                  α0 ↦ ξ.ρ.left.as-number,
                                  α1 ↦ ξ.ρ.left.as-number.plus(
                                    α0 ↦ ξ.ρ.step
                                  )
                                )
                              )
                            )
                          ),
                          α1 ↦ ξ.ρ.left.put(
                            α0 ↦ ξ.ρ.left.as-number.plus(
                              α0 ↦ ξ.ρ.step
                            )
                          )
                        ),
                        α1 ↦ Φ.org.eolang.true
                      )
                    ),
                    α1 ↦ Φ.org.eolang.false
                  )
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
  ⟧
}
```

## [org/eolang/math/numbers.phi](./org/eolang/math/numbers.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        math ↦ ⟦
          numbers ↦ ⟦
            sequence ↦ ∅,
            φ ↦ ξ.sequence,
            max ↦ ⟦
              lst ↦ Φ.org.eolang.structs.list(
                α0 ↦ ξ.ρ.sequence
              ),
              φ ↦ ξ.lst.is-empty.if(
                α0 ↦ Φ.org.eolang.error(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 43-61-6E-27-74-20-67-65-74-20-6D-61-78-20-6E-75-6D-62-65-72-20-66-72-6F-6D-20-65-6D-70-74-79-20-73-65-71-75-65-6E-63-65
                    )
                  )
                ),
                α1 ↦ ξ.lst.reduced(
                  α0 ↦ Φ.org.eolang.negative-infinity,
                  α1 ↦ ⟦
                    max ↦ ∅,
                    item ↦ ∅,
                    φ ↦ ξ.item.as-number.gt(
                      α0 ↦ ξ.max
                    ).if(
                      α0 ↦ ξ.item,
                      α1 ↦ ξ.max
                    )
                  ⟧
                )
              )
            ⟧,
            min ↦ ⟦
              lst ↦ Φ.org.eolang.structs.list(
                α0 ↦ ξ.ρ.sequence
              ),
              φ ↦ ξ.lst.is-empty.if(
                α0 ↦ Φ.org.eolang.error(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 43-61-6E-27-74-20-67-65-74-20-6D-69-6E-20-6E-75-6D-62-65-72-20-66-72-6F-6D-20-65-6D-70-74-79-20-73-65-71-75-65-6E-63-65
                    )
                  )
                ),
                α1 ↦ ξ.lst.reduced(
                  α0 ↦ Φ.org.eolang.positive-infinity,
                  α1 ↦ ⟦
                    min ↦ ∅,
                    item ↦ ∅,
                    φ ↦ ξ.min.gt(
                      α0 ↦ ξ.item.as-number
                    ).if(
                      α0 ↦ ξ.item,
                      α1 ↦ ξ.min
                    )
                  ⟧
                )
              )
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

## [org/eolang/math/pi.phi](./org/eolang/math/pi.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        math ↦ ⟦
          pi ↦ Φ.org.eolang.number(
            α0 ↦ Φ.org.eolang.bytes(
              Δ ⤍ 40-09-21-FB-54-44-2D-18
            )
          ),
          λ ⤍ Package
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/math/random.phi](./org/eolang/math/random.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        math ↦ ⟦
          random ↦ ⟦
            seed ↦ ∅,
            fixed ↦ ξ,
            φ ↦ ξ.seed.as-number.div(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-20-00-00-00-00-00-00
              ).as-i64.as-number
            ),
            next ↦ ξ.ρ.random(
              α0 ↦ ξ.seed.times(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 42-17-7B-B3-99-B4-00-00
                  )
                )
              ).plus(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 40-26-00-00-00-00-00-00
                  )
                )
              ).as-i64.and(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-0F-FF-FF-FF-FF-FF-FF
                )
              ).as-i64.as-number
            ).fixed,
            pseudo ↦ ⟦
              const-1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-41-80-00-00-00-00-00
                )
              ),
              const-2 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-4A-80-00-00-00-00-00
                )
              ),
              const-3 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-31-00-00-00-00-00-00
                )
              ),
              one ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-01
              ),
              φ ↦ ξ.ρ.ρ.random(
                α0 ↦ ξ.time-seed
              ),
              time-seed ↦ ξ.time-bytes.left(
                α0 ↦ ξ.const-1
              ).and(
                α0 ↦ ξ.one.left(
                  α0 ↦ ξ.const-2
                ).as-i64.minus(
                  α0 ↦ ξ.one
                ).as-bytes
              ).as-i64.plus(
                α0 ↦ ξ.time-bytes.left(
                  α0 ↦ ξ.const-3
                ).and(
                  α0 ↦ ξ.one.left(
                    α0 ↦ ξ.const-1
                  ).as-i64.minus(
                    α0 ↦ ξ.one
                  ).as-bytes
                ).as-i64.plus(
                  α0 ↦ ξ.time-bytes.and(
                    α0 ↦ ξ.one.left(
                      α0 ↦ ξ.const-3
                    ).as-i64.minus(
                      α0 ↦ ξ.one
                    ).as-bytes
                  ).as-i64
                )
              ).as-number,
              time-bytes ↦ Φ.org.eolang.sys.os.is-windows.if(
                α0 ↦ Φ.org.eolang.sys.win32(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 47-65-74-53-79-73-74-65-6D-54-69-6D-65
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ Φ.org.eolang.sys.win32.system-time
                  )
                ).milliseconds,
                α1 ↦ ⟦
                  timeval ↦ Φ.org.eolang.sys.posix(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 67-65-74-74-69-6D-65-6F-66-64-61-79
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ Φ.org.eolang.sys.posix.timeval
                    )
                  ).output,
                  φ ↦ ξ.timeval.tv-sec.times(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 40-8F-40-00-00-00-00-00
                      )
                    )
                  ).plus(
                    α0 ↦ ξ.timeval.tv-usec.as-i64.div(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 40-8F-40-00-00-00-00-00
                        )
                      ).as-i64
                    ).as-number
                  )
                ⟧
              ).as-i64.as-bytes
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

## [org/eolang/math/real.phi](./org/eolang/math/real.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        math ↦ ⟦
          real ↦ ⟦
            num ↦ ∅,
            φ ↦ ξ.num,
            exp ↦ Φ.org.eolang.math.real(
              α0 ↦ Φ.org.eolang.math.e
            ).pow(
              α0 ↦ ξ.num
            ),
            mod ↦ ⟦
              x ↦ ∅,
              dividend ↦ Φ.org.eolang.number(
                α0 ↦ ξ.ρ.num.as-bytes
              ),
              divisor ↦ Φ.org.eolang.number(
                α0 ↦ ξ.x.as-bytes
              ),
              φ ↦ ξ.divisor.eq(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ Φ.org.eolang.error(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 43-61-6E-27-74-20-63-61-6C-63-75-6C-61-74-65-20-6D-6F-64-20-62-79-20-7A-65-72-6F
                    )
                  )
                ),
                α1 ↦ ξ.dividend.gt(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                ).if(
                  α0 ↦ ξ.abs-mod,
                  α1 ↦ ξ.abs-mod.neg
                )
              ),
              abs-mod ↦ ⟦
                dividend-abs ↦ Φ.org.eolang.math.real(
                  α0 ↦ ξ.ρ.dividend
                ).abs,
                divisor-abs ↦ Φ.org.eolang.math.real(
                  α0 ↦ ξ.ρ.divisor
                ).abs,
                φ ↦ ξ.dividend-abs.minus(
                  α0 ↦ ξ.divisor-abs.times(
                    α0 ↦ ξ.dividend-abs.div(
                      α0 ↦ ξ.divisor-abs
                    ).floor
                  )
                )
              ⟧
            ⟧,
            abs ↦ ⟦
              value ↦ Φ.org.eolang.number(
                α0 ↦ ξ.ρ.num.as-bytes
              ),
              φ ↦ ξ.value.gte(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ ξ.value,
                α1 ↦ ξ.value.neg
              )
            ⟧,
            pow ↦ ⟦
              λ ⤍ Lorg_eolang_math_real_pow,
              x ↦ ∅
            ⟧,
            sqrt ↦ ⟦
              λ ⤍ Lorg_eolang_math_real_sqrt
            ⟧,
            ln ↦ ⟦
              λ ⤍ Lorg_eolang_math_real_ln
            ⟧,
            acos ↦ ⟦
              λ ⤍ Lorg_eolang_math_real_acos
            ⟧,
            asin ↦ ⟦
              λ ⤍ Lorg_eolang_math_real_asin
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

## [org/eolang/nan.phi](./org/eolang/nan.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        nan ↦ ⟦
          φ ↦ Φ.org.eolang.number(
            α0 ↦ Φ.org.eolang.bytes(
              Δ ⤍ 7F-F8-00-00-00-00-00-00
            )
          ),
          floor ↦ ξ,
          neg ↦ ξ,
          is-nan ↦ Φ.org.eolang.true,
          is-finite ↦ Φ.org.eolang.false,
          is-integer ↦ Φ.org.eolang.false,
          as-i64 ↦ Φ.org.eolang.error(
            α0 ↦ Φ.org.eolang.string(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-4E-61-4E-20-74-6F-20-69-36-34
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
          φ ↦ Φ.org.eolang.number(
            α0 ↦ Φ.org.eolang.bytes(
              Δ ⤍ FF-F0-00-00-00-00-00-00
            )
          ),
          floor ↦ ξ,
          neg ↦ Φ.org.eolang.positive-infinity,
          is-nan ↦ Φ.org.eolang.false,
          is-finite ↦ Φ.org.eolang.false,
          is-integer ↦ Φ.org.eolang.false,
          as-i64 ↦ Φ.org.eolang.error(
            α0 ↦ Φ.org.eolang.string(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-6E-65-67-61-74-69-76-65-20-69-6E-66-69-6E-69-74-79-20-74-6F-20-69-36-34
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
            φ ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ).is-nan.or(
              α0 ↦ ξ.ρ.eq(
                α0 ↦ ξ.value
              )
            ).not
          ⟧,
          lte ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ).is-nan.not
          ⟧,
          gt ↦ ⟦
            φ ↦ Φ.org.eolang.false,
            x ↦ ∅
          ⟧,
          gte ↦ ⟦
            φ ↦ ξ.ρ.eq(
              α0 ↦ ξ.x
            ),
            x ↦ ∅
          ⟧,
          times ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            num ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ),
            φ ↦ ξ.num.is-nan.or(
              α0 ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              )
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.num.gt(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ ξ.ρ,
                α1 ↦ Φ.org.eolang.positive-infinity
              )
            )
          ⟧,
          plus ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ).is-nan.or(
              α0 ↦ ξ.value.eq(
                α0 ↦ Φ.org.eolang.positive-infinity
              )
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.ρ
            )
          ⟧,
          minus ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ).is-nan.or(
              α0 ↦ ξ.value.eq(
                α0 ↦ ξ.ρ
              )
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.ρ
            )
          ⟧,
          div ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            num ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ),
            φ ↦ ξ.num.is-nan.or(
              α0 ↦ ξ.num.is-finite.not
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.value.eq(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 80-00-00-00-00-00-00-00
                  )
                ).as-bytes
              ).or(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).gt(
                  α0 ↦ ξ.value
                )
              ).if(
                α0 ↦ Φ.org.eolang.positive-infinity,
                α1 ↦ ξ.ρ
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

## [org/eolang/net/socket.phi](./org/eolang/net/socket.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        net ↦ ⟦
          socket ↦ ⟦
            address ↦ ∅,
            port ↦ ∅,
            φ ↦ Φ.org.eolang.sys.os.is-windows.if(
              α0 ↦ ξ.win-socket(
                α0 ↦ ξ.address,
                α1 ↦ ξ.port
              ),
              α1 ↦ ξ.posix-socket(
                α0 ↦ ξ.address,
                α1 ↦ ξ.port
              )
            ),
            htons ↦ ⟦
              port ↦ ∅,
              bts ↦ ξ.port.as-i16.as-bytes,
              φ ↦ ξ.bts.and(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ FF-
                )
              ).left(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 40-20-00-00-00-00-00-00
                  )
                )
              ).or(
                α0 ↦ ξ.bts.right(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 40-20-00-00-00-00-00-00
                    )
                  )
                ).and(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ FF-
                  )
                )
              ).as-i16
            ⟧,
            as-input ↦ ⟦
              recv ↦ ∅,
              read ↦ ⟦
                size ↦ ∅,
                φ ↦ ξ.input-block(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ --
                  )
                ).read(
                  α0 ↦ ξ.size
                ).self,
                input-block ↦ ⟦
                  buffer ↦ ∅,
                  self ↦ ξ,
                  φ ↦ ξ.buffer,
                  read ↦ ⟦
                    size ↦ ∅,
                    read-bytes ↦ ξ.ρ.ρ.ρ.recv(
                      α0 ↦ ξ.size
                    ).as-bytes,
                    φ ↦ Φ.org.eolang.seq(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.read-bytes
                        ),
                        α1 ↦ ξ.ρ.ρ.input-block(
                          α0 ↦ ξ.read-bytes
                        )
                      )
                    ).self
                  ⟧
                ⟧
              ⟧
            ⟧,
            as-output ↦ ⟦
              send ↦ ∅,
              write ↦ ⟦
                buffer ↦ ∅,
                φ ↦ ξ.output-block.write(
                  α0 ↦ ξ.buffer
                ).self,
                output-block ↦ ⟦
                  self ↦ ξ,
                  φ ↦ Φ.org.eolang.true,
                  write ↦ ⟦
                    buffer ↦ ∅,
                    φ ↦ Φ.org.eolang.seq(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.ρ.ρ.ρ.send(
                            α0 ↦ ξ.buffer
                          )
                        ),
                        α1 ↦ ξ.ρ.ρ.output-block
                      )
                    ).self
                  ⟧
                ⟧
              ⟧
            ⟧,
            posix-socket ↦ ⟦
              address ↦ ∅,
              port ↦ ∅,
              sd ↦ Φ.org.eolang.sys.posix(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 73-6F-63-6B-65-74
                  )
                ),
                α1 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ Φ.org.eolang.sys.posix.af-inet
                    ),
                    α1 ↦ Φ.org.eolang.sys.posix.sock-stream
                  ),
                  α1 ↦ Φ.org.eolang.sys.posix.ipproto-tcp
                )
              ).code,
              inet-addr ↦ Φ.org.eolang.sys.posix(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 69-6E-65-74-5F-61-64-64-72
                  )
                ),
                α1 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ ξ.address
                )
              ).code,
              inet-addr-as-int ↦ ξ.inet-addr.eq(
                α0 ↦ Φ.org.eolang.sys.posix.inaddr-none
              ).if(
                α0 ↦ Φ.org.eolang.error(
                  α0 ↦ Φ.org.eolang.txt.sprintf(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-63-6F-6E-76-65-72-74-20-61-6E-20-49-50-76-34-20-61-64-64-72-65-73-73-20-27-25-73-27-20-69-6E-74-6F-20-61-20-33-32-2D-62-69-74-20-69-6E-74-65-67-65-72-20-76-69-61-20-27-69-6E-65-74-5F-61-64-64-72-27-20-70-6F-73-69-78-20-73-79-73-63-61-6C-6C-2C-20-72-65-61-73-6F-6E-3A-20-27-25-73-27
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.address
                      ),
                      α1 ↦ ξ.strerror.code
                    )
                  )
                ),
                α1 ↦ ξ.inet-addr.as-i32
              ),
              sockaddr ↦ Φ.org.eolang.sys.posix.sockaddr-in(
                α0 ↦ Φ.org.eolang.sys.posix.af-inet.as-i16,
                α1 ↦ ξ.ρ.htons(
                  α0 ↦ ξ.port
                ),
                α2 ↦ ξ.inet-addr-as-int
              ),
              scoped-socket ↦ ⟦
                sockfd ↦ ∅,
                as-input ↦ ξ.ρ.ρ.ρ.as-input(
                  α0 ↦ ξ.recv
                ),
                as-output ↦ ξ.ρ.ρ.ρ.as-output(
                  α0 ↦ ξ.send
                ),
                send ↦ ⟦
                  buffer ↦ ∅,
                  buff ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.buffer
                  ).as-bytes,
                  sent ↦ Φ.org.eolang.sys.posix(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 73-65-6E-64
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ ξ.ρ.sockfd
                          ),
                          α1 ↦ ξ.buff
                        ),
                        α1 ↦ ξ.buff.size
                      ),
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                    )
                  ).code,
                  φ ↦ ξ.sent.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.txt.sprintf(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 46-61-69-6C-65-64-20-74-6F-20-73-65-6E-64-20-6D-65-73-73-61-67-65-20-74-68-72-6F-75-67-68-20-74-68-65-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-72-65-61-73-6F-6E-3A-20-25-73
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ ξ.ρ.sockfd
                          ),
                          α1 ↦ ξ.ρ.ρ.strerror.code
                        )
                      )
                    ),
                    α1 ↦ ξ.sent
                  )
                ⟧,
                recv ↦ ⟦
                  size ↦ ∅,
                  received ↦ Φ.org.eolang.sys.posix(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 72-65-63-76
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.ρ.sockfd
                        ),
                        α1 ↦ ξ.size
                      ),
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                    )
                  ).called,
                  φ ↦ ξ.received.code.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.txt.sprintf(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 46-61-69-6C-65-64-20-74-6F-20-72-65-63-65-69-76-65-20-64-61-74-61-20-66-72-6F-6D-20-74-68-65-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-72-65-61-73-6F-6E-3A-20-25-73
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ ξ.ρ.sockfd
                          ),
                          α1 ↦ ξ.ρ.ρ.strerror.code
                        )
                      )
                    ),
                    α1 ↦ ξ.received.output
                  )
                ⟧
              ⟧,
              strerror ↦ ⟦
                φ ↦ Φ.org.eolang.sys.posix(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 73-74-72-65-72-72-6F-72
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ Φ.org.eolang.sys.posix(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 65-72-72-6E-6F
                        )
                      ),
                      α1 ↦ Φ.org.eolang.tuple.empty
                    ).code
                  )
                )
              ⟧,
              closed-socket ↦ ⟦
                sockfd ↦ ∅,
                closed ↦ Φ.org.eolang.sys.posix(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 63-6C-6F-73-65
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.sockfd
                  )
                ).code,
                φ ↦ ξ.closed.eq(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ BF-F0-00-00-00-00-00-00
                    )
                  )
                ).if(
                  α0 ↦ Φ.org.eolang.error(
                    α0 ↦ Φ.org.eolang.txt.sprintf(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-63-6C-6F-73-65-20-61-20-70-6F-73-69-78-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-72-65-61-73-6F-6E-3A-20-27-25-73-27
                        )
                      ),
                      α1 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.sockfd
                        ),
                        α1 ↦ ξ.ρ.strerror.code
                      )
                    )
                  ),
                  α1 ↦ Φ.org.eolang.true
                )
              ⟧,
              safe-socket ↦ ⟦
                scope ↦ ∅,
                φ ↦ ξ.ρ.sd.eq(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ BF-F0-00-00-00-00-00-00
                    )
                  )
                ).if(
                  α0 ↦ Φ.org.eolang.error(
                    α0 ↦ Φ.org.eolang.txt.sprintf(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-63-72-65-61-74-65-20-61-20-70-6F-73-69-78-20-73-6F-63-6B-65-74-2C-20-72-65-61-73-6F-6E-3A-20-27-25-73-27
                        )
                      ),
                      α1 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.ρ.strerror.code
                      )
                    )
                  ),
                  α1 ↦ Φ.org.eolang.try(
                    α0 ↦ ξ.scope,
                    α1 ↦ ⟦
                      φ ↦ Φ.org.eolang.error(
                        α0 ↦ ξ.ex
                      ),
                      ex ↦ ∅
                    ⟧,
                    α2 ↦ ξ.ρ.closed-socket(
                      α0 ↦ ξ.ρ.sd
                    )
                  )
                )
              ⟧,
              connect ↦ ⟦
                scope ↦ ∅,
                φ ↦ ξ.ρ.safe-socket(
                  α0 ↦ ξ.auto-named-attr-at-278-10
                ),
                auto-named-attr-at-278-10 ↦ ⟦
                  sock ↦ ξ.ρ.ρ,
                  connected ↦ Φ.org.eolang.sys.posix(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 63-6F-6E-6E-65-63-74
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.sock.sd
                        ),
                        α1 ↦ ξ.sock.sockaddr
                      ),
                      α1 ↦ ξ.sock.sockaddr.size
                    )
                  ).code,
                  φ ↦ ξ.connected.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.txt.sprintf(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-63-6F-6E-6E-65-63-74-20-74-6F-20-27-25-73-3A-25-64-27-20-6F-6E-20-70-6F-73-69-78-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-72-65-61-73-6F-6E-3A-20-27-25-73-27
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple(
                                α0 ↦ Φ.org.eolang.tuple.empty,
                                α1 ↦ ξ.sock.address
                              ),
                              α1 ↦ ξ.sock.port
                            ),
                            α1 ↦ ξ.sock.sd
                          ),
                          α1 ↦ ξ.sock.strerror.code
                        )
                      )
                    ),
                    α1 ↦ Φ.org.eolang.dataized(
                      α0 ↦ ξ.ρ.scope(
                        α0 ↦ ξ.sock.scoped-socket(
                          α0 ↦ ξ.sock.sd
                        )
                      )
                    ).as-bytes
                  )
                ⟧
              ⟧,
              listen ↦ ⟦
                scope ↦ ∅,
                φ ↦ ξ.ρ.safe-socket(
                  α0 ↦ ξ.auto-named-attr-at-302-10
                ),
                auto-named-attr-at-302-10 ↦ ⟦
                  sock ↦ ξ.ρ.ρ,
                  bound ↦ Φ.org.eolang.sys.posix(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 62-69-6E-64
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.sock.sd
                        ),
                        α1 ↦ ξ.sock.sockaddr
                      ),
                      α1 ↦ ξ.sock.sockaddr.size
                    )
                  ).code,
                  listened ↦ Φ.org.eolang.sys.posix(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 6C-69-73-74-65-6E
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.sock.sd
                      ),
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 40-A0-00-00-00-00-00-00
                        )
                      )
                    )
                  ).code,
                  φ ↦ ξ.bound.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.txt.sprintf(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-62-69-6E-64-20-70-6F-73-69-78-20-73-6F-63-6B-65-74-20-27-25-64-27-20-74-6F-20-27-25-73-3A-25-64-27-2C-20-72-65-61-73-6F-6E-3A-20-27-25-73-27
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple(
                                α0 ↦ Φ.org.eolang.tuple.empty,
                                α1 ↦ ξ.sock.sd
                              ),
                              α1 ↦ ξ.sock.address
                            ),
                            α1 ↦ ξ.sock.port
                          ),
                          α1 ↦ ξ.sock.strerror.code
                        )
                      )
                    ),
                    α1 ↦ ξ.listened.eq(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ BF-F0-00-00-00-00-00-00
                        )
                      )
                    ).if(
                      α0 ↦ Φ.org.eolang.error(
                        α0 ↦ Φ.org.eolang.txt.sprintf(
                          α0 ↦ Φ.org.eolang.string(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ 46-61-69-6C-65-64-20-74-6F-20-6C-69-73-74-65-6E-20-66-6F-72-20-63-6F-6E-6E-65-63-74-69-6F-6E-73-20-74-6F-20-27-25-73-3A-25-64-27-20-6F-6E-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-72-65-61-73-6F-6E-3A-20-27-25-73-27
                            )
                          ),
                          α1 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple(
                                α0 ↦ Φ.org.eolang.tuple(
                                  α0 ↦ Φ.org.eolang.tuple.empty,
                                  α1 ↦ ξ.sock.address
                                ),
                                α1 ↦ ξ.sock.port
                              ),
                              α1 ↦ ξ.sock.sd
                            ),
                            α1 ↦ ξ.sock.strerror.code
                          )
                        )
                      ),
                      α1 ↦ Φ.org.eolang.dataized(
                        α0 ↦ ξ.ρ.scope(
                          α0 ↦ ξ.auto-named-attr-at-327-22
                        )
                      ).as-bytes
                    )
                  ),
                  auto-named-attr-at-327-22 ↦ ⟦
                    φ ↦ ξ.ρ.sock.scoped-socket(
                      α0 ↦ ξ.ρ.sock.sd
                    ),
                    accept ↦ ⟦
                      scope ↦ ∅,
                      sock ↦ ξ.ρ.ρ.sock,
                      client-sockfd ↦ Φ.org.eolang.sys.posix(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 61-63-63-65-70-74
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple.empty,
                              α1 ↦ ξ.sock.sd
                            ),
                            α1 ↦ ξ.sock.sockaddr
                          ),
                          α1 ↦ ξ.sock.sockaddr.size
                        )
                      ).code,
                      φ ↦ Φ.org.eolang.try(
                        α0 ↦ ξ.client-sockfd.eq(
                          α0 ↦ Φ.org.eolang.number(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ BF-F0-00-00-00-00-00-00
                            )
                          )
                        ).if(
                          α0 ↦ Φ.org.eolang.error(
                            α0 ↦ Φ.org.eolang.txt.sprintf(
                              α0 ↦ Φ.org.eolang.string(
                                α0 ↦ Φ.org.eolang.bytes(
                                  Δ ⤍ 46-61-69-6C-65-64-20-74-6F-20-61-63-63-65-70-74-20-61-20-63-6F-6E-6E-65-63-74-69-6F-6E-20-6F-6E-20-70-6F-73-69-78-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-72-65-61-73-6F-6E-3A-20-25-73
                                )
                              ),
                              α1 ↦ Φ.org.eolang.tuple(
                                α0 ↦ Φ.org.eolang.tuple(
                                  α0 ↦ Φ.org.eolang.tuple.empty,
                                  α1 ↦ ξ.sock.sd
                                ),
                                α1 ↦ ξ.sock.strerror.code
                              )
                            )
                          ),
                          α1 ↦ Φ.org.eolang.dataized(
                            α0 ↦ ξ.scope(
                              α0 ↦ ξ.sock.scoped-socket(
                                α0 ↦ ξ.client-sockfd
                              )
                            )
                          ).as-bytes
                        ),
                        α1 ↦ ⟦
                          φ ↦ Φ.org.eolang.error(
                            α0 ↦ ξ.ex
                          ),
                          ex ↦ ∅
                        ⟧,
                        α2 ↦ ξ.sock.closed-socket(
                          α0 ↦ ξ.client-sockfd
                        )
                      )
                    ⟧
                  ⟧
                ⟧
              ⟧
            ⟧,
            win-socket ↦ ⟦
              address ↦ ∅,
              port ↦ ∅,
              sd ↦ Φ.org.eolang.sys.win32(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 73-6F-63-6B-65-74
                  )
                ),
                α1 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ Φ.org.eolang.sys.win32.af-inet
                    ),
                    α1 ↦ Φ.org.eolang.sys.win32.sock-stream
                  ),
                  α1 ↦ Φ.org.eolang.sys.win32.ipproto-tcp
                )
              ).code,
              inet-addr ↦ Φ.org.eolang.sys.win32(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 69-6E-65-74-5F-61-64-64-72
                  )
                ),
                α1 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ ξ.address
                )
              ).code,
              inet-addr-as-int ↦ ξ.inet-addr.eq(
                α0 ↦ Φ.org.eolang.sys.win32.inaddr-none
              ).if(
                α0 ↦ Φ.org.eolang.error(
                  α0 ↦ Φ.org.eolang.txt.sprintf(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-63-6F-6E-76-65-72-74-20-61-6E-20-49-50-76-34-20-61-64-64-72-65-73-73-20-27-25-73-27-20-69-6E-74-6F-20-61-20-33-32-2D-62-69-74-20-69-6E-74-65-67-65-72-20-76-69-61-20-27-69-6E-65-74-5F-61-64-64-72-27-20-77-69-6E-33-32-20-66-75-6E-63-74-69-6F-6E-20-63-61-6C-6C-2C-20-57-53-41-20-65-72-72-6F-72-20-63-6F-64-65-3A-20-25-64
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.address
                      ),
                      α1 ↦ ξ.last-error.code
                    )
                  )
                ),
                α1 ↦ ξ.inet-addr.as-i32
              ),
              sockaddr ↦ Φ.org.eolang.sys.win32.sockaddr-in(
                α0 ↦ Φ.org.eolang.sys.win32.af-inet.as-i16,
                α1 ↦ ξ.ρ.htons(
                  α0 ↦ ξ.port
                ),
                α2 ↦ ξ.inet-addr-as-int
              ),
              scoped-socket ↦ ⟦
                sockfd ↦ ∅,
                as-input ↦ ξ.ρ.ρ.ρ.as-input(
                  α0 ↦ ξ.recv
                ),
                as-output ↦ ξ.ρ.ρ.ρ.as-output(
                  α0 ↦ ξ.send
                ),
                send ↦ ⟦
                  buffer ↦ ∅,
                  buff ↦ Φ.org.eolang.dataized(
                    α0 ↦ ξ.buffer
                  ).as-bytes,
                  sent ↦ Φ.org.eolang.sys.win32(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 73-65-6E-64
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ ξ.ρ.sockfd
                          ),
                          α1 ↦ ξ.buff
                        ),
                        α1 ↦ ξ.buff.size
                      ),
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                    )
                  ).code,
                  φ ↦ ξ.sent.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.txt.sprintf(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 46-61-69-6C-65-64-20-74-6F-20-73-65-6E-64-20-6D-65-73-73-61-67-65-20-74-68-72-6F-75-67-68-20-74-68-65-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-57-53-41-20-65-72-72-6F-72-20-63-6F-64-65-3A-20-25-64
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ ξ.ρ.sockfd
                          ),
                          α1 ↦ ξ.ρ.ρ.last-error.code
                        )
                      )
                    ),
                    α1 ↦ ξ.sent
                  )
                ⟧,
                recv ↦ ⟦
                  size ↦ ∅,
                  received ↦ Φ.org.eolang.sys.win32(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 72-65-63-76
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.ρ.sockfd
                        ),
                        α1 ↦ ξ.size
                      ),
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                    )
                  ).called,
                  φ ↦ ξ.received.code.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.txt.sprintf(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 46-61-69-6C-65-64-20-74-6F-20-72-65-63-65-69-76-65-20-64-61-74-61-20-66-72-6F-6D-20-74-68-65-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-57-53-41-20-65-72-72-6F-72-20-63-6F-64-65-3A-20-25-64
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ ξ.ρ.sockfd
                          ),
                          α1 ↦ ξ.ρ.ρ.last-error.code
                        )
                      )
                    ),
                    α1 ↦ ξ.received.output
                  )
                ⟧
              ⟧,
              last-error ↦ ⟦
                φ ↦ Φ.org.eolang.sys.win32(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 57-53-41-47-65-74-4C-61-73-74-45-72-72-6F-72
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple.empty
                )
              ⟧,
              closed-socket ↦ ⟦
                sockfd ↦ ∅,
                closed ↦ Φ.org.eolang.sys.win32(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 63-6C-6F-73-65-73-6F-63-6B-65-74
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.sockfd
                  )
                ).code,
                φ ↦ ξ.closed.eq(
                  α0 ↦ Φ.org.eolang.sys.win32.socket-error
                ).if(
                  α0 ↦ Φ.org.eolang.error(
                    α0 ↦ Φ.org.eolang.txt.sprintf(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-63-6C-6F-73-65-20-61-20-77-69-6E-33-32-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-57-53-41-20-65-72-72-6F-72-20-63-6F-64-65-3A-20-25-64
                        )
                      ),
                      α1 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.sockfd
                        ),
                        α1 ↦ ξ.ρ.last-error.code
                      )
                    )
                  ),
                  α1 ↦ Φ.org.eolang.true
                )
              ⟧,
              safe-socket ↦ ⟦
                scope ↦ ∅,
                started-up ↦ Φ.org.eolang.sys.win32(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 57-53-41-53-74-61-72-74-75-70
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ Φ.org.eolang.sys.win32.winsock-version-2-2
                  )
                ).code,
                cleaned-up ↦ Φ.org.eolang.sys.win32(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 57-53-41-43-6C-65-61-6E-75-70
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple.empty
                ).code,
                φ ↦ ξ.started-up.eq(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                ).not.if(
                  α0 ↦ Φ.org.eolang.error(
                    α0 ↦ Φ.org.eolang.txt.sprintf(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-69-6E-69-74-69-61-6C-69-7A-65-20-57-69-6E-73-6F-63-6B-20-76-69-61-20-27-57-53-41-53-74-61-72-74-75-70-27-20-63-61-6C-6C-2C-20-57-53-41-20-65-72-72-6F-72-20-63-6F-64-65-3A-20-25-64
                        )
                      ),
                      α1 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.started-up
                      )
                    )
                  ),
                  α1 ↦ Φ.org.eolang.try(
                    α0 ↦ ξ.ρ.sd.eq(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ BF-F0-00-00-00-00-00-00
                        )
                      )
                    ).if(
                      α0 ↦ Φ.org.eolang.error(
                        α0 ↦ Φ.org.eolang.txt.sprintf(
                          α0 ↦ Φ.org.eolang.string(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-63-72-65-61-74-65-20-61-20-77-69-6E-33-32-20-73-6F-63-6B-65-74-2C-20-57-53-41-20-65-72-72-6F-72-20-63-6F-64-65-3A-20-25-64
                            )
                          ),
                          α1 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ ξ.ρ.last-error.code
                          )
                        )
                      ),
                      α1 ↦ Φ.org.eolang.try(
                        α0 ↦ ξ.scope,
                        α1 ↦ ⟦
                          φ ↦ Φ.org.eolang.error(
                            α0 ↦ ξ.ex
                          ),
                          ex ↦ ∅
                        ⟧,
                        α2 ↦ ξ.ρ.closed-socket(
                          α0 ↦ ξ.ρ.sd
                        )
                      )
                    ),
                    α1 ↦ ⟦
                      φ ↦ Φ.org.eolang.error(
                        α0 ↦ ξ.ex
                      ),
                      ex ↦ ∅
                    ⟧,
                    α2 ↦ ξ.cleaned-up.eq(
                      α0 ↦ Φ.org.eolang.sys.win32.socket-error
                    ).if(
                      α0 ↦ Φ.org.eolang.error(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-63-6C-65-61-6E-75-70-20-57-69-6E-73-6F-63-6B-20-72-65-73-6F-75-72-63-65-73
                          )
                        )
                      ),
                      α1 ↦ Φ.org.eolang.true
                    )
                  )
                )
              ⟧,
              connect ↦ ⟦
                scope ↦ ∅,
                φ ↦ ξ.ρ.safe-socket(
                  α0 ↦ ξ.auto-named-attr-at-487-10
                ),
                auto-named-attr-at-487-10 ↦ ⟦
                  sock ↦ ξ.ρ.ρ,
                  connected ↦ Φ.org.eolang.sys.win32(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 63-6F-6E-6E-65-63-74
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.sock.sd
                        ),
                        α1 ↦ ξ.sock.sockaddr
                      ),
                      α1 ↦ ξ.sock.sockaddr.size
                    )
                  ).code,
                  φ ↦ ξ.connected.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.txt.sprintf(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-63-6F-6E-6E-65-63-74-20-74-6F-20-27-25-73-3A-25-64-27-20-6F-6E-20-77-69-6E-33-32-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-57-53-41-20-65-72-72-6F-72-20-63-6F-64-65-3A-20-25-64
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple(
                                α0 ↦ Φ.org.eolang.tuple.empty,
                                α1 ↦ ξ.sock.address
                              ),
                              α1 ↦ ξ.sock.port
                            ),
                            α1 ↦ ξ.sock.sd
                          ),
                          α1 ↦ ξ.sock.last-error.code
                        )
                      )
                    ),
                    α1 ↦ Φ.org.eolang.dataized(
                      α0 ↦ ξ.ρ.scope(
                        α0 ↦ ξ.sock.scoped-socket(
                          α0 ↦ ξ.sock.sd
                        )
                      )
                    ).as-bytes
                  )
                ⟧
              ⟧,
              listen ↦ ⟦
                scope ↦ ∅,
                φ ↦ ξ.ρ.safe-socket(
                  α0 ↦ ξ.auto-named-attr-at-511-10
                ),
                auto-named-attr-at-511-10 ↦ ⟦
                  sock ↦ ξ.ρ.ρ,
                  bound ↦ Φ.org.eolang.sys.win32(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 62-69-6E-64
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ ξ.sock.sd
                        ),
                        α1 ↦ ξ.sock.sockaddr
                      ),
                      α1 ↦ ξ.sock.sockaddr.size
                    )
                  ).code,
                  listened ↦ Φ.org.eolang.sys.win32(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 6C-69-73-74-65-6E
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.sock.sd
                      ),
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 40-A0-00-00-00-00-00-00
                        )
                      )
                    )
                  ).code,
                  φ ↦ ξ.bound.eq(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  ).if(
                    α0 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.txt.sprintf(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 43-6F-75-6C-64-6E-27-74-20-62-69-6E-64-20-77-69-6E-33-32-20-73-6F-63-6B-65-74-20-27-25-64-27-20-74-6F-20-27-25-73-3A-25-64-27-2C-20-57-53-41-20-65-72-72-6F-72-20-63-6F-64-65-3A-20-25-64
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple(
                                α0 ↦ Φ.org.eolang.tuple.empty,
                                α1 ↦ ξ.sock.sd
                              ),
                              α1 ↦ ξ.sock.address
                            ),
                            α1 ↦ ξ.sock.port
                          ),
                          α1 ↦ ξ.sock.last-error.code
                        )
                      )
                    ),
                    α1 ↦ ξ.listened.eq(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ BF-F0-00-00-00-00-00-00
                        )
                      )
                    ).if(
                      α0 ↦ Φ.org.eolang.error(
                        α0 ↦ Φ.org.eolang.txt.sprintf(
                          α0 ↦ Φ.org.eolang.string(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ 46-61-69-6C-65-64-20-74-6F-20-6C-69-73-74-65-6E-20-66-6F-72-20-63-6F-6E-6E-65-63-74-69-6F-6E-73-20-74-6F-20-27-25-73-3A-25-64-27-20-6F-6E-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-57-53-41-20-65-72-72-6F-72-20-63-6F-64-65-3A-20-25-64
                            )
                          ),
                          α1 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple(
                                α0 ↦ Φ.org.eolang.tuple(
                                  α0 ↦ Φ.org.eolang.tuple.empty,
                                  α1 ↦ ξ.sock.address
                                ),
                                α1 ↦ ξ.sock.port
                              ),
                              α1 ↦ ξ.sock.sd
                            ),
                            α1 ↦ ξ.sock.last-error.code
                          )
                        )
                      ),
                      α1 ↦ Φ.org.eolang.dataized(
                        α0 ↦ ξ.ρ.scope(
                          α0 ↦ ξ.auto-named-attr-at-536-22
                        )
                      ).as-bytes
                    )
                  ),
                  auto-named-attr-at-536-22 ↦ ⟦
                    φ ↦ ξ.ρ.sock.scoped-socket(
                      α0 ↦ ξ.ρ.sock.sd
                    ),
                    accept ↦ ⟦
                      scope ↦ ∅,
                      sock ↦ ξ.ρ.ρ.sock,
                      client-sockfd ↦ Φ.org.eolang.sys.win32(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 61-63-63-65-70-74
                          )
                        ),
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple(
                              α0 ↦ Φ.org.eolang.tuple.empty,
                              α1 ↦ ξ.sock.sd
                            ),
                            α1 ↦ ξ.sock.sockaddr
                          ),
                          α1 ↦ ξ.sock.sockaddr.size
                        )
                      ).code,
                      φ ↦ Φ.org.eolang.try(
                        α0 ↦ ξ.client-sockfd.eq(
                          α0 ↦ Φ.org.eolang.number(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ BF-F0-00-00-00-00-00-00
                            )
                          )
                        ).if(
                          α0 ↦ Φ.org.eolang.error(
                            α0 ↦ Φ.org.eolang.txt.sprintf(
                              α0 ↦ Φ.org.eolang.string(
                                α0 ↦ Φ.org.eolang.bytes(
                                  Δ ⤍ 46-61-69-6C-65-64-20-74-6F-20-61-63-63-65-70-74-20-61-20-63-6F-6E-6E-65-63-74-69-6F-6E-20-6F-6E-20-77-69-6E-33-32-20-73-6F-63-6B-65-74-20-27-25-64-27-2C-20-57-53-41-20-65-72-72-6F-72-20-63-6F-64-65-3A-20-25-64
                                )
                              ),
                              α1 ↦ Φ.org.eolang.tuple(
                                α0 ↦ Φ.org.eolang.tuple(
                                  α0 ↦ Φ.org.eolang.tuple.empty,
                                  α1 ↦ ξ.sock.sd
                                ),
                                α1 ↦ ξ.sock.last-error.code
                              )
                            )
                          ),
                          α1 ↦ Φ.org.eolang.dataized(
                            α0 ↦ ξ.scope(
                              α0 ↦ ξ.sock.scoped-socket(
                                α0 ↦ ξ.client-sockfd
                              )
                            )
                          ).as-bytes
                        ),
                        α1 ↦ ⟦
                          φ ↦ Φ.org.eolang.error(
                            α0 ↦ ξ.ex
                          ),
                          ex ↦ ∅
                        ⟧,
                        α2 ↦ ξ.sock.closed-socket(
                          α0 ↦ ξ.client-sockfd
                        )
                      )
                    ⟧
                  ⟧
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
  ⟧
}
```

## [org/eolang/number.phi](./org/eolang/number.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        number ↦ ⟦
          as-bytes ↦ ∅,
          φ ↦ ξ.as-bytes,
          as-number ↦ ξ,
          neg ↦ ξ.times(
            α0 ↦ ξ.ρ.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ BF-F0-00-00-00-00-00-00
              )
            )
          ),
          as-i32 ↦ ξ.as-i64.as-i32,
          as-i16 ↦ ξ.as-i32.as-i16,
          as-i64 ↦ ⟦
            λ ⤍ Lorg_eolang_number_as_i64
          ⟧,
          eq ↦ ⟦
            x ↦ ∅,
            x-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            self-as-bytes ↦ ξ.ρ.as-bytes,
            pos-zero-as-bytes ↦ ξ.ρ.ρ.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ).as-bytes,
            neg-zero-as-bytes ↦ ξ.ρ.ρ.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 80-00-00-00-00-00-00-00
              )
            ).as-bytes,
            φ ↦ ξ.ρ.is-nan.or(
              α0 ↦ ξ.ρ.ρ.number(
                α0 ↦ ξ.x-as-bytes
              ).is-nan
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
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ ξ.ρ.ρ.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ).gt(
              α0 ↦ ξ.ρ.minus(
                α0 ↦ ξ.ρ.ρ.number(
                  α0 ↦ ξ.value
                )
              )
            )
          ⟧,
          lte ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ ξ.ρ.lt(
              α0 ↦ ξ.value
            ).or(
              α0 ↦ ξ.ρ.eq(
                α0 ↦ ξ.value
              )
            )
          ⟧,
          gt ↦ ⟦
            λ ⤍ Lorg_eolang_number_gt,
            x ↦ ∅
          ⟧,
          gte ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ ξ.ρ.gt(
              α0 ↦ ξ.value
            ).or(
              α0 ↦ ξ.ρ.eq(
                α0 ↦ ξ.value
              )
            )
          ⟧,
          times ↦ ⟦
            λ ⤍ Lorg_eolang_number_times,
            x ↦ ∅
          ⟧,
          plus ↦ ⟦
            λ ⤍ Lorg_eolang_number_plus,
            x ↦ ∅
          ⟧,
          minus ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ ξ.ρ.plus(
              α0 ↦ ξ.ρ.ρ.number(
                α0 ↦ ξ.value
              ).neg
            )
          ⟧,
          div ↦ ⟦
            λ ⤍ Lorg_eolang_number_div,
            x ↦ ∅
          ⟧,
          floor ↦ ⟦
            λ ⤍ Lorg_eolang_number_floor
          ⟧,
          is-integer ↦ ⟦
            φ ↦ ξ.ρ.is-finite.and(
              α0 ↦ ξ.ρ.eq(
                α0 ↦ ξ.ρ.floor
              )
            )
          ⟧,
          is-finite ↦ ⟦
            φ ↦ ξ.ρ.is-nan.not.and(
              α0 ↦ ξ.ρ.eq(
                α0 ↦ Φ.org.eolang.positive-infinity
              ).or(
                α0 ↦ ξ.ρ.eq(
                  α0 ↦ Φ.org.eolang.negative-infinity
                )
              ).not
            )
          ⟧,
          is-nan ↦ ⟦
            φ ↦ ξ.ρ.as-bytes.eq(
              α0 ↦ Φ.org.eolang.nan.as-bytes
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
          φ ↦ Φ.org.eolang.number(
            α0 ↦ Φ.org.eolang.bytes(
              Δ ⤍ 7F-F0-00-00-00-00-00-00
            )
          ),
          floor ↦ ξ,
          neg ↦ Φ.org.eolang.negative-infinity,
          is-nan ↦ Φ.org.eolang.false,
          is-finite ↦ Φ.org.eolang.false,
          is-integer ↦ Φ.org.eolang.false,
          as-i64 ↦ Φ.org.eolang.error(
            α0 ↦ Φ.org.eolang.string(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-70-6F-73-69-74-69-76-65-20-69-6E-66-69-6E-69-74-79-20-74-6F-20-69-36-34
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
            φ ↦ Φ.org.eolang.false,
            x ↦ ∅
          ⟧,
          lte ↦ ⟦
            φ ↦ ξ.ρ.eq(
              α0 ↦ ξ.x
            ),
            x ↦ ∅
          ⟧,
          gt ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ).is-nan.or(
              α0 ↦ ξ.ρ.eq(
                α0 ↦ ξ.value
              )
            ).not
          ⟧,
          gte ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ).is-nan.not
          ⟧,
          times ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            num ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ),
            φ ↦ ξ.num.is-nan.or(
              α0 ↦ ξ.num.eq(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              )
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.num.gt(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ ξ.ρ,
                α1 ↦ Φ.org.eolang.negative-infinity
              )
            )
          ⟧,
          plus ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ).is-nan.or(
              α0 ↦ ξ.value.eq(
                α0 ↦ Φ.org.eolang.negative-infinity
              )
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.ρ
            )
          ⟧,
          minus ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            φ ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ).is-nan.or(
              α0 ↦ ξ.value.eq(
                α0 ↦ ξ.ρ
              )
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.ρ
            )
          ⟧,
          div ↦ ⟦
            x ↦ ∅,
            value ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.x
            ).as-bytes,
            num ↦ Φ.org.eolang.number(
              α0 ↦ ξ.value
            ),
            φ ↦ ξ.num.is-nan.or(
              α0 ↦ ξ.num.is-finite.not
            ).if(
              α0 ↦ Φ.org.eolang.nan,
              α1 ↦ ξ.value.eq(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 80-00-00-00-00-00-00-00
                  )
                ).as-bytes
              ).or(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).gt(
                  α0 ↦ ξ.value
                )
              ).if(
                α0 ↦ Φ.org.eolang.negative-infinity,
                α1 ↦ ξ.ρ
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
          steps ↦ ∅,
          φ ↦ ξ.steps.length.eq(
            α0 ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            )
          ).if(
            α0 ↦ Φ.org.eolang.true,
            α1 ↦ ξ.loop(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
            )
          ),
          max-len ↦ Φ.org.eolang.dataized(
            α0 ↦ ξ.steps.length.minus(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 3F-F0-00-00-00-00-00-00
                )
              )
            )
          ).as-bytes,
          loop ↦ ⟦
            index ↦ ∅,
            φ ↦ ξ.index.lt(
              α0 ↦ ξ.ρ.max-len
            ).and(
              α0 ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.steps.at(
                  α0 ↦ ξ.index
                )
              ).as-bool.or(
                α0 ↦ Φ.org.eolang.true
              )
            ).if(
              α0 ↦ ξ.ρ.loop(
                α0 ↦ ξ.index.plus(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 3F-F0-00-00-00-00-00-00
                    )
                  )
                )
              ),
              α1 ↦ ξ.ρ.steps.at(
                α0 ↦ ξ.index
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

## [org/eolang/string.phi](./org/eolang/string.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        string ↦ ⟦
          as-bytes ↦ ∅,
          φ ↦ ξ.as-bytes,
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

## [org/eolang/structs/bytes-as-array.phi](./org/eolang/structs/bytes-as-array.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        structs ↦ ⟦
          bytes-as-array ↦ ⟦
            bts ↦ ∅,
            bytes-size ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.bts.size
            ).as-bytes,
            φ ↦ ξ.slice-byte(
              α0 ↦ Φ.org.eolang.tuple.empty,
              α1 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
            ),
            slice-byte ↦ ⟦
              tup ↦ ∅,
              index ↦ ∅,
              φ ↦ ξ.index.lt(
                α0 ↦ ξ.ρ.bytes-size
              ).if(
                α0 ↦ ξ.ρ.slice-byte(
                  α0 ↦ ξ.tup.with(
                    α0 ↦ ξ.ρ.bts.slice(
                      α0 ↦ ξ.index,
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    )
                  ),
                  α1 ↦ ξ.index.plus(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 3F-F0-00-00-00-00-00-00
                      )
                    )
                  )
                ),
                α1 ↦ ξ.tup
              )
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

## [org/eolang/structs/hash-code-of.phi](./org/eolang/structs/hash-code-of.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        structs ↦ ⟦
          hash-code-of ↦ ⟦
            input ↦ ∅,
            input-as-bytes ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.input.as-bytes
            ).as-bytes,
            size ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.input-as-bytes.size
            ).as-bytes,
            φ ↦ ⟦
              magic-number ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 40-3F-00-00-00-00-00-00
                )
              ).as-i64,
              φ ↦ ξ.rec-hash-code(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ),
                α1 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ),
              rec-hash-code ↦ ⟦
                acc ↦ ∅,
                index ↦ ∅,
                φ ↦ ξ.index.eq(
                  α0 ↦ ξ.ρ.ρ.size
                ).if(
                  α0 ↦ ξ.acc.as-number,
                  α1 ↦ ξ.ρ.rec-hash-code(
                    α0 ↦ ξ.ρ.magic-number.times(
                      α0 ↦ ξ.acc
                    ).plus(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00
                      ).concat(
                        α0 ↦ ξ.ρ.ρ.input-as-bytes.slice(
                          α0 ↦ ξ.index,
                          α1 ↦ Φ.org.eolang.number(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ 3F-F0-00-00-00-00-00-00
                            )
                          )
                        )
                      ).as-i64
                    ),
                    α1 ↦ ξ.index.plus(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
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
      ⟧,
      λ ⤍ Package
    ⟧
  ⟧
}
```

## [org/eolang/structs/list.phi](./org/eolang/structs/list.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        structs ↦ ⟦
          list ↦ ⟦
            origin ↦ ∅,
            φ ↦ ξ.origin,
            is-empty ↦ ⟦
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).eq(
                α0 ↦ ξ.ρ.origin.length
              )
            ⟧,
            with ↦ ⟦
              x ↦ ∅,
              φ ↦ ξ.ρ.ρ.list(
                α0 ↦ ξ.ρ.origin.with(
                  α0 ↦ ξ.x
                )
              )
            ⟧,
            withi ↦ ⟦
              index ↦ ∅,
              item ↦ ∅,
              φ ↦ ξ.ρ.head(
                α0 ↦ ξ.index
              ).with(
                α0 ↦ ξ.item
              ).concat(
                α0 ↦ ξ.ρ.tail(
                  α0 ↦ ξ.ρ.origin.length.minus(
                    α0 ↦ ξ.index
                  )
                )
              )
            ⟧,
            reducedi ↦ ⟦
              start ↦ ∅,
              func ↦ ∅,
              origin-len ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.origin.length
              ).as-bytes,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).eq(
                α0 ↦ ξ.origin-len
              ).if(
                α0 ↦ ξ.start,
                α1 ↦ ξ.rec-reduced(
                  α0 ↦ ξ.start,
                  α1 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).as-bytes
                )
              ),
              rec-reduced ↦ ⟦
                accum ↦ ∅,
                index ↦ ∅,
                idx-as-number ↦ ξ.index.as-number,
                next-index ↦ Φ.org.eolang.dataized(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 3F-F0-00-00-00-00-00-00
                    )
                  ).plus(
                    α0 ↦ ξ.idx-as-number
                  )
                ).as-bytes,
                φ ↦ ξ.next-index.eq(
                  α0 ↦ ξ.ρ.origin-len
                ).if(
                  α0 ↦ ξ.accumulated,
                  α1 ↦ ξ.ρ.rec-reduced(
                    α0 ↦ ξ.accumulated,
                    α1 ↦ ξ.next-index
                  )
                ),
                accumulated ↦ ξ.ρ.func(
                  α0 ↦ ξ.accum,
                  α1 ↦ ξ.ρ.ρ.origin.at(
                    α0 ↦ ξ.idx-as-number
                  ),
                  α2 ↦ ξ.idx-as-number
                )
              ⟧
            ⟧,
            reduced ↦ ⟦
              start ↦ ∅,
              func ↦ ∅,
              φ ↦ ξ.ρ.reducedi(
                α0 ↦ ξ.start,
                α1 ↦ ξ.auto-named-attr-at-85-42
              ),
              auto-named-attr-at-85-42 ↦ ⟦
                φ ↦ ξ.ρ.func(
                  α0 ↦ ξ.accum,
                  α1 ↦ ξ.item
                ),
                accum ↦ ∅,
                item ↦ ∅,
                idx ↦ ∅
              ⟧
            ⟧,
            mappedi ↦ ⟦
              func ↦ ∅,
              φ ↦ ξ.ρ.ρ.list(
                α0 ↦ ξ.ρ.reducedi(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ ξ.auto-named-attr-at-95-24
                )
              ),
              auto-named-attr-at-95-24 ↦ ⟦
                accum ↦ ∅,
                item ↦ ∅,
                idx ↦ ∅,
                φ ↦ ξ.accum.with(
                  α0 ↦ ξ.ρ.func(
                    α0 ↦ ξ.item,
                    α1 ↦ ξ.idx
                  )
                )
              ⟧
            ⟧,
            mapped ↦ ⟦
              func ↦ ∅,
              φ ↦ ξ.ρ.mappedi(
                α0 ↦ ξ.auto-named-attr-at-105-30
              ),
              auto-named-attr-at-105-30 ↦ ⟦
                φ ↦ ξ.ρ.func(
                  α0 ↦ ξ.item
                ),
                item ↦ ∅,
                idx ↦ ∅
              ⟧
            ⟧,
            eachi ↦ ⟦
              func ↦ ∅,
              φ ↦ ξ.ρ.reducedi(
                α0 ↦ Φ.org.eolang.true,
                α1 ↦ ξ.auto-named-attr-at-115-22
              ),
              auto-named-attr-at-115-22 ↦ ⟦
                acc ↦ ∅,
                item ↦ ∅,
                index ↦ ∅,
                φ ↦ Φ.org.eolang.seq(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ ξ.acc
                    ),
                    α1 ↦ ξ.ρ.func(
                      α0 ↦ ξ.item,
                      α1 ↦ ξ.index
                    )
                  )
                )
              ⟧
            ⟧,
            each ↦ ⟦
              func ↦ ∅,
              φ ↦ ξ.ρ.eachi(
                α0 ↦ ξ.auto-named-attr-at-126-32
              ),
              auto-named-attr-at-126-32 ↦ ⟦
                φ ↦ ξ.ρ.func(
                  α0 ↦ ξ.item
                ),
                item ↦ ∅,
                index ↦ ∅
              ⟧
            ⟧,
            withouti ↦ ⟦
              i ↦ ∅,
              φ ↦ ξ.ρ.ρ.list(
                α0 ↦ ξ.ρ.reducedi(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ ξ.auto-named-attr-at-133-24
                )
              ),
              auto-named-attr-at-133-24 ↦ ⟦
                accum ↦ ∅,
                item ↦ ∅,
                idx ↦ ∅,
                φ ↦ ξ.ρ.i.eq(
                  α0 ↦ ξ.idx
                ).if(
                  α0 ↦ ξ.accum,
                  α1 ↦ ξ.accum.with(
                    α0 ↦ ξ.item
                  )
                )
              ⟧
            ⟧,
            without ↦ ⟦
              element ↦ ∅,
              φ ↦ ξ.ρ.ρ.list(
                α0 ↦ ξ.ρ.reduced(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ ξ.auto-named-attr-at-144-20
                )
              ),
              auto-named-attr-at-144-20 ↦ ⟦
                accum ↦ ∅,
                item ↦ ∅,
                φ ↦ ξ.ρ.element.eq(
                  α0 ↦ ξ.item
                ).if(
                  α0 ↦ ξ.accum,
                  α1 ↦ ξ.accum.with(
                    α0 ↦ ξ.item
                  )
                )
              ⟧
            ⟧,
            eq ↦ ⟦
              other ↦ ∅,
              φ ↦ ξ.ρ.origin.length.eq(
                α0 ↦ ξ.other.length
              ).and(
                α0 ↦ ξ.ρ.reducedi(
                  α0 ↦ Φ.org.eolang.true,
                  α1 ↦ ξ.auto-named-attr-at-159-24
                )
              ),
              auto-named-attr-at-159-24 ↦ ⟦
                accum ↦ ∅,
                item ↦ ∅,
                idx ↦ ∅,
                φ ↦ ξ.accum.and(
                  α0 ↦ ξ.item.eq(
                    α0 ↦ ξ.ρ.other.at(
                      α0 ↦ ξ.idx
                    )
                  )
                )
              ⟧
            ⟧,
            concat ↦ ⟦
              passed ↦ ∅,
              φ ↦ ξ.ρ.ρ.list(
                α0 ↦ ξ.passed
              ).reduced(
                α0 ↦ ξ.ρ,
                α1 ↦ ⟦
                  φ ↦ ξ.accum.with(
                    α0 ↦ ξ.item
                  ),
                  accum ↦ ∅,
                  item ↦ ∅
                ⟧
              )
            ⟧,
            index-of ↦ ⟦
              wanted ↦ ∅,
              φ ↦ ξ.ρ.reducedi(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ BF-F0-00-00-00-00-00-00
                  )
                ),
                α1 ↦ ξ.auto-named-attr-at-179-24
              ),
              auto-named-attr-at-179-24 ↦ ⟦
                accum ↦ ∅,
                item ↦ ∅,
                index ↦ ∅,
                φ ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ BF-F0-00-00-00-00-00-00
                  )
                ).eq(
                  α0 ↦ ξ.accum
                ).and(
                  α0 ↦ ξ.item.eq(
                    α0 ↦ ξ.ρ.wanted
                  )
                ).if(
                  α0 ↦ ξ.index,
                  α1 ↦ ξ.accum
                )
              ⟧
            ⟧,
            last-index-of ↦ ⟦
              wanted ↦ ∅,
              φ ↦ ξ.ρ.reducedi(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ BF-F0-00-00-00-00-00-00
                  )
                ),
                α1 ↦ ξ.auto-named-attr-at-192-24
              ),
              auto-named-attr-at-192-24 ↦ ⟦
                accum ↦ ∅,
                item ↦ ∅,
                index ↦ ∅,
                φ ↦ ξ.item.eq(
                  α0 ↦ ξ.ρ.wanted
                ).if(
                  α0 ↦ ξ.index,
                  α1 ↦ ξ.accum
                )
              ⟧
            ⟧,
            contains ↦ ⟦
              element ↦ ∅,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ BF-F0-00-00-00-00-00-00
                )
              ).eq(
                α0 ↦ ξ.ρ.index-of(
                  α0 ↦ ξ.element
                )
              ).not
            ⟧,
            sorted ↦ ⟦
              φ ↦ ξ.ρ
            ⟧,
            filteredi ↦ ⟦
              func ↦ ∅,
              origin-length ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.origin.length
              ).as-bytes,
              φ ↦ ξ.ρ.ρ.list(
                α0 ↦ ξ.rec-filtered(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).as-bytes,
                  α1 ↦ Φ.org.eolang.tuple.empty
                )
              ),
              rec-filtered ↦ ⟦
                idx-as-bytes ↦ ∅,
                accum ↦ ∅,
                original ↦ ξ.ρ.ρ.origin,
                index ↦ ξ.idx-as-bytes.as-number,
                item ↦ ξ.ρ.ρ.origin.at(
                  α0 ↦ ξ.index
                ),
                φ ↦ ξ.idx-as-bytes.eq(
                  α0 ↦ ξ.ρ.origin-length
                ).if(
                  α0 ↦ ξ.accum,
                  α1 ↦ ξ.ρ.rec-filtered(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 3F-F0-00-00-00-00-00-00
                      )
                    ).plus(
                      α0 ↦ ξ.index
                    ).as-bytes,
                    α1 ↦ ξ.ρ.func(
                      α0 ↦ ξ.item,
                      α1 ↦ ξ.index
                    ).if(
                      α0 ↦ ξ.accum.with(
                        α0 ↦ ξ.item
                      ),
                      α1 ↦ ξ.accum
                    )
                  )
                )
              ⟧
            ⟧,
            filtered ↦ ⟦
              func ↦ ∅,
              φ ↦ ξ.ρ.filteredi(
                α0 ↦ ξ.auto-named-attr-at-245-32
              ),
              auto-named-attr-at-245-32 ↦ ⟦
                φ ↦ ξ.ρ.func(
                  α0 ↦ ξ.item
                ),
                item ↦ ∅,
                index ↦ ∅
              ⟧
            ⟧,
            head ↦ ⟦
              index ↦ ∅,
              idx ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.index
              ).as-bytes,
              φ ↦ Φ.org.eolang.switch(
                α0 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple(
                            α0 ↦ Φ.org.eolang.tuple.empty,
                            α1 ↦ Φ.org.eolang.number(
                              α0 ↦ Φ.org.eolang.bytes(
                                Δ ⤍ 00-00-00-00-00-00-00-00
                              )
                            ).eq(
                              α0 ↦ ξ.idx
                            )
                          ),
                          α1 ↦ ξ.ρ.ρ.list(
                            α0 ↦ Φ.org.eolang.tuple.empty
                          )
                        )
                      ),
                      α1 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple(
                          α0 ↦ Φ.org.eolang.tuple.empty,
                          α1 ↦ Φ.org.eolang.number(
                            α0 ↦ Φ.org.eolang.bytes(
                              Δ ⤍ 00-00-00-00-00-00-00-00
                            )
                          ).gt(
                            α0 ↦ ξ.idx
                          )
                        ),
                        α1 ↦ ξ.ρ.tail(
                          α0 ↦ ξ.index.as-number.neg
                        )
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.ρ.origin.length.lte(
                          α0 ↦ ξ.idx
                        )
                      ),
                      α1 ↦ ξ.ρ
                    )
                  ),
                  α1 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ Φ.org.eolang.true
                    ),
                    α1 ↦ ξ.ρ.ρ.list(
                      α0 ↦ ξ.ρ.reducedi(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.auto-named-attr-at-266-32
                      )
                    )
                  )
                )
              ),
              auto-named-attr-at-266-32 ↦ ⟦
                accum ↦ ∅,
                item ↦ ∅,
                index ↦ ∅,
                φ ↦ ξ.index.gte(
                  α0 ↦ ξ.ρ.idx
                ).if(
                  α0 ↦ ξ.accum,
                  α1 ↦ ξ.accum.with(
                    α0 ↦ ξ.item
                  )
                )
              ⟧
            ⟧,
            tail ↦ ⟦
              index ↦ ∅,
              idx ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.index
              ).as-bytes,
              start ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.origin.length.minus(
                  α0 ↦ ξ.idx.as-number
                )
              ).as-bytes,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).gt(
                α0 ↦ ξ.start
              ).if(
                α0 ↦ ξ.ρ,
                α1 ↦ ξ.ρ.ρ.list(
                  α0 ↦ ξ.ρ.reducedi(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.auto-named-attr-at-282-26
                  )
                )
              ),
              auto-named-attr-at-282-26 ↦ ⟦
                accum ↦ ∅,
                item ↦ ∅,
                idx ↦ ∅,
                φ ↦ ξ.idx.gte(
                  α0 ↦ ξ.ρ.start
                ).if(
                  α0 ↦ ξ.accum.with(
                    α0 ↦ ξ.item
                  ),
                  α1 ↦ ξ.accum
                )
              ⟧
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

## [org/eolang/structs/map.phi](./org/eolang/structs/map.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        structs ↦ ⟦
          map ↦ ⟦
            pairs ↦ ∅,
            φ ↦ ξ.auto-named-attr-at-39-6.initialized,
            entry ↦ ⟦
              key ↦ ∅,
              value ↦ ∅
            ⟧,
            initialized ↦ ⟦
              entries ↦ ∅,
              initialized ↦ ξ,
              size ↦ ξ.entries.length,
              keys ↦ ⟦
                φ ↦ Φ.org.eolang.structs.list(
                  α0 ↦ ξ.ρ.entries
                ).mapped(
                  α0 ↦ ⟦
                    φ ↦ ξ.entry.key,
                    entry ↦ ∅
                  ⟧
                )
              ⟧,
              values ↦ ⟦
                φ ↦ Φ.org.eolang.structs.list(
                  α0 ↦ ξ.ρ.entries
                ).mapped(
                  α0 ↦ ⟦
                    φ ↦ ξ.entry.value,
                    entry ↦ ∅
                  ⟧
                )
              ⟧,
              has ↦ ⟦
                φ ↦ ξ.ρ.found(
                  α0 ↦ ξ.key
                ).exists,
                key ↦ ∅
              ⟧,
              found ↦ ⟦
                key ↦ ∅,
                hash ↦ Φ.org.eolang.dataized(
                  α0 ↦ Φ.org.eolang.structs.hash-code-of(
                    α0 ↦ ξ.key
                  )
                ).as-bytes,
                φ ↦ ξ.ρ.size.eq(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                ).if(
                  α0 ↦ ξ.not-found,
                  α1 ↦ ξ.rec-key-search(
                    α0 ↦ ξ.not-found,
                    α1 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  )
                ),
                rec-key-search ↦ ⟦
                  found ↦ ∅,
                  index ↦ ∅,
                  entry ↦ ξ.ρ.ρ.entries.at(
                    α0 ↦ ξ.index
                  ),
                  φ ↦ ξ.found.exists.or(
                    α0 ↦ ξ.ρ.ρ.size.eq(
                      α0 ↦ ξ.index
                    )
                  ).if(
                    α0 ↦ ξ.found,
                    α1 ↦ ξ.ρ.rec-key-search(
                      α0 ↦ ξ.ρ.hash.eq(
                        α0 ↦ ξ.entry.hash
                      ).if(
                        α0 ↦ ξ.auto-named-attr-at-135-54,
                        α1 ↦ ξ.found
                      ),
                      α1 ↦ ξ.index.plus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 3F-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    )
                  ),
                  auto-named-attr-at-135-54 ↦ ⟦
                    exists ↦ Φ.org.eolang.true,
                    get ↦ ξ.ρ.entry.value
                  ⟧
                ⟧,
                not-found ↦ ⟦
                  exists ↦ Φ.org.eolang.false,
                  get ↦ Φ.org.eolang.error(
                    α0 ↦ Φ.org.eolang.txt.sprintf(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 4F-62-6A-65-63-74-20-62-79-20-68-61-73-68-20-63-6F-64-65-20-25-64-20-66-72-6F-6D-20-67-69-76-65-6E-20-6B-65-79-20-64-6F-65-73-20-6E-6F-74-20-65-78-69-73-74-73
                        )
                      ),
                      α1 ↦ Φ.org.eolang.tuple(
                        α0 ↦ Φ.org.eolang.tuple.empty,
                        α1 ↦ ξ.ρ.hash
                      )
                    )
                  )
                ⟧
              ⟧,
              with ↦ ⟦
                key ↦ ∅,
                value ↦ ∅,
                hash ↦ Φ.org.eolang.dataized(
                  α0 ↦ Φ.org.eolang.structs.hash-code-of(
                    α0 ↦ ξ.key
                  )
                ).as-bytes,
                φ ↦ ξ.ρ.ρ.ρ.map.initialized(
                  α0 ↦ Φ.org.eolang.structs.list(
                    α0 ↦ ξ.ρ.entries
                  ).filtered(
                    α0 ↦ ξ.auto-named-attr-at-155-50
                  ).origin.with(
                    α0 ↦ ξ.auto-named-attr-at-156-12
                  )
                ),
                auto-named-attr-at-155-50 ↦ ⟦
                  φ ↦ ξ.ρ.hash.eq(
                    α0 ↦ ξ.entry.hash
                  ).not,
                  entry ↦ ∅
                ⟧,
                auto-named-attr-at-156-12 ↦ ⟦
                  key ↦ ξ.ρ.key,
                  value ↦ ξ.ρ.value,
                  hash ↦ ξ.ρ.hash
                ⟧
              ⟧,
              without ↦ ⟦
                key ↦ ∅,
                hash ↦ Φ.org.eolang.dataized(
                  α0 ↦ Φ.org.eolang.structs.hash-code-of(
                    α0 ↦ ξ.key
                  )
                ).as-bytes,
                φ ↦ ξ.ρ.ρ.ρ.map.initialized(
                  α0 ↦ Φ.org.eolang.structs.list(
                    α0 ↦ ξ.ρ.entries
                  ).filtered(
                    α0 ↦ ξ.auto-named-attr-at-169-48
                  ).origin
                ),
                auto-named-attr-at-169-48 ↦ ⟦
                  φ ↦ ξ.ρ.hash.eq(
                    α0 ↦ ξ.entry.hash
                  ).not,
                  entry ↦ ∅
                ⟧
              ⟧
            ⟧,
            auto-named-attr-at-39-6 ↦ ⟦
              pairs-size ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.pairs.length
              ).as-bytes,
              φ ↦ ξ.ρ.initialized(
                α0 ↦ ξ.pairs-size.eq(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                ).if(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ ξ.rec-rebuild(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    ),
                    α2 ↦ Φ.org.eolang.structs.list(
                      α0 ↦ Φ.org.eolang.tuple.empty
                    )
                  )
                )
              ),
              rec-rebuild ↦ ⟦
                accum ↦ ∅,
                index ↦ ∅,
                hashes ↦ ∅,
                entry ↦ ξ.ρ.ρ.pairs.at(
                  α0 ↦ ξ.index
                ),
                hash ↦ Φ.org.eolang.dataized(
                  α0 ↦ Φ.org.eolang.structs.hash-code-of(
                    α0 ↦ ξ.entry.key
                  )
                ).as-bytes,
                φ ↦ ξ.ρ.pairs-size.eq(
                  α0 ↦ ξ.index
                ).if(
                  α0 ↦ ξ.accum,
                  α1 ↦ ξ.ρ.rec-rebuild(
                    α0 ↦ ξ.hashes.contains(
                      α0 ↦ ξ.hash
                    ).if(
                      α0 ↦ ξ.accum,
                      α1 ↦ ξ.accum.with(
                        α0 ↦ ξ.auto-named-attr-at-61-18
                      )
                    ),
                    α1 ↦ ξ.index.plus(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    ),
                    α2 ↦ ξ.hashes.with(
                      α0 ↦ ξ.hash
                    )
                  )
                ),
                auto-named-attr-at-61-18 ↦ ⟦
                  key ↦ ξ.ρ.entry.key,
                  value ↦ ξ.ρ.entry.value,
                  hash ↦ ξ.ρ.hash
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
  ⟧
}
```

## [org/eolang/structs/range-of-ints.phi](./org/eolang/structs/range-of-ints.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        structs ↦ ⟦
          range-of-ints ↦ ⟦
            start ↦ ∅,
            end ↦ ∅,
            φ ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ).eq(
              α0 ↦ ξ.start
            ).or(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 3F-F0-00-00-00-00-00-00
                )
              ).eq(
                α0 ↦ ξ.start.div(
                  α0 ↦ ξ.start
                )
              )
            ).and(
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).eq(
                α0 ↦ ξ.end
              ).or(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 3F-F0-00-00-00-00-00-00
                  )
                ).eq(
                  α0 ↦ ξ.end.div(
                    α0 ↦ ξ.end
                  )
                )
              )
            ).if(
              α0 ↦ Φ.org.eolang.structs.range(
                α0 ↦ ξ.auto-named-attr-at-44-8,
                α1 ↦ ξ.end
              ),
              α1 ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 53-6F-6D-65-20-6F-66-20-74-68-65-20-61-72-67-75-6D-65-6E-74-73-20-61-72-65-20-6E-6F-74-20-69-6E-74-65-67-65-72-73
                  )
                )
              )
            ),
            auto-named-attr-at-44-8 ↦ ⟦
              build ↦ ⟦
                num ↦ ∅,
                φ ↦ ξ.num,
                next ↦ ξ.ρ.build(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 3F-F0-00-00-00-00-00-00
                    )
                  ).plus(
                    α0 ↦ ξ.φ
                  )
                )
              ⟧,
              φ ↦ ξ.build(
                α0 ↦ ξ.ρ.start
              )
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

## [org/eolang/structs/range.phi](./org/eolang/structs/range.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        structs ↦ ⟦
          range ↦ ⟦
            start ↦ ∅,
            end ↦ ∅,
            φ ↦ Φ.org.eolang.structs.list(
              α0 ↦ ξ.start.lt(
                α0 ↦ ξ.end
              ).if(
                α0 ↦ ξ.appended(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.start
                  ),
                  α1 ↦ ξ.start.next
                ),
                α1 ↦ Φ.org.eolang.tuple.empty
              )
            ),
            appended ↦ ⟦
              acc ↦ ∅,
              current ↦ ∅,
              φ ↦ ξ.current.lt(
                α0 ↦ ξ.ρ.end
              ).if(
                α0 ↦ ξ.ρ.appended(
                  α0 ↦ ξ.acc.with(
                    α0 ↦ ξ.current
                  ),
                  α1 ↦ ξ.current.next
                ),
                α1 ↦ ξ.acc
              )
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

## [org/eolang/structs/set.phi](./org/eolang/structs/set.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        structs ↦ ⟦
          set ↦ ⟦
            lst ↦ ∅,
            φ ↦ ξ.initialized(
              α0 ↦ Φ.org.eolang.structs.map(
                α0 ↦ Φ.org.eolang.structs.list(
                  α0 ↦ ξ.lst
                ).mapped(
                  α0 ↦ ⟦
                    φ ↦ Φ.org.eolang.structs.map.entry(
                      α0 ↦ ξ.item,
                      α1 ↦ Φ.org.eolang.true
                    ),
                    item ↦ ∅
                  ⟧
                ).origin
              )
            ).initialized,
            initialized ↦ ⟦
              map ↦ ∅,
              initialized ↦ ξ,
              φ ↦ ξ.map.keys,
              size ↦ ξ.map.size,
              with ↦ ⟦
                item ↦ ∅,
                φ ↦ ξ.ρ.ρ.ρ.set.initialized(
                  α0 ↦ ξ.ρ.map.with(
                    α0 ↦ ξ.item,
                    α1 ↦ Φ.org.eolang.true
                  )
                )
              ⟧,
              without ↦ ⟦
                item ↦ ∅,
                φ ↦ ξ.ρ.ρ.ρ.set.initialized(
                  α0 ↦ ξ.ρ.map.without(
                    α0 ↦ ξ.item
                  )
                )
              ⟧,
              has ↦ ⟦
                item ↦ ∅,
                φ ↦ ξ.ρ.map.has(
                  α0 ↦ ξ.item
                )
              ⟧
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
          φ ↦ ξ.len.eq(
            α0 ↦ Φ.org.eolang.number(
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
              α0 ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              )
            )
          ),
          case-at ↦ ⟦
            index ↦ ∅,
            case ↦ ξ.ρ.cases.at(
              α0 ↦ ξ.index
            ),
            φ ↦ ξ.index.eq(
              α0 ↦ ξ.ρ.len
            ).if(
              α0 ↦ Φ.org.eolang.true,
              α1 ↦ ξ.case.at(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ ξ.case.at(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 3F-F0-00-00-00-00-00-00
                    )
                  )
                ),
                α1 ↦ ξ.ρ.case-at(
                  α0 ↦ ξ.index.plus(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 3F-F0-00-00-00-00-00-00
                      )
                    )
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

## [org/eolang/sys/getenv.phi](./org/eolang/sys/getenv.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        sys ↦ ⟦
          getenv ↦ ⟦
            name ↦ ∅,
            φ ↦ Φ.org.eolang.sys.os.is-windows.if(
              α0 ↦ Φ.org.eolang.sys.win32(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 47-65-74-45-6E-76-69-72-6F-6E-6D-65-6E-74-56-61-72-69-61-62-6C-65
                  )
                ),
                α1 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple(
                    α0 ↦ Φ.org.eolang.tuple.empty,
                    α1 ↦ ξ.name
                  ),
                  α1 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 40-80-00-00-00-00-00-00
                    )
                  )
                )
              ),
              α1 ↦ Φ.org.eolang.sys.posix(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 67-65-74-65-6E-76
                  )
                ),
                α1 ↦ Φ.org.eolang.tuple(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ ξ.name
                )
              )
            ).output
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

## [org/eolang/sys/line-separator.phi](./org/eolang/sys/line-separator.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        sys ↦ ⟦
          line-separator ↦ ⟦
            φ ↦ Φ.org.eolang.string(
              α0 ↦ Φ.org.eolang.sys.os.is-windows.if(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 0D-0A
                  )
                ),
                α1 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 0A-
                  )
                )
              ).as-bytes
            )
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

## [org/eolang/sys/os.phi](./org/eolang/sys/os.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        sys ↦ ⟦
          os ↦ ⟦
            φ ↦ ξ.name,
            is-windows ↦ ⟦
              os-name ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.name
              ).as-bytes,
              φ ↦ ξ.os-name.size.gt(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 40-18-00-00-00-00-00-00
                  )
                )
              ).and(
                α0 ↦ ξ.os-name.slice(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ),
                  α1 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 40-1C-00-00-00-00-00-00
                    )
                  )
                ).eq(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 57-69-6E-64-6F-77-73
                    )
                  )
                )
              )
            ⟧,
            is-linux ↦ Φ.org.eolang.txt.regex(
              α0 ↦ Φ.org.eolang.string(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 2F-6C-69-6E-75-78-2F-69
                )
              )
            ).matches(
              α0 ↦ ξ.name
            ).as-bool,
            is-macos ↦ Φ.org.eolang.txt.regex(
              α0 ↦ Φ.org.eolang.string(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 2F-6D-61-63-2F-69
                )
              )
            ).matches(
              α0 ↦ ξ.name
            ).as-bool,
            name ↦ ⟦
              λ ⤍ Lorg_eolang_sys_os_name
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

## [org/eolang/sys/posix.phi](./org/eolang/sys/posix.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        sys ↦ ⟦
          posix ↦ ⟦
            name ↦ ∅,
            args ↦ ∅,
            stdin-fileno ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ),
            stdout-fileno ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 3F-F0-00-00-00-00-00-00
              )
            ),
            af-inet ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 40-00-00-00-00-00-00-00
              )
            ),
            sock-stream ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 3F-F0-00-00-00-00-00-00
              )
            ),
            ipproto-tcp ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 40-18-00-00-00-00-00-00
              )
            ),
            inaddr-none ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ BF-F0-00-00-00-00-00-00
              )
            ),
            φ ↦ ⟦
              λ ⤍ Lorg_eolang_sys_posix_φ
            ⟧,
            return ↦ ⟦
              code ↦ ∅,
              output ↦ ∅,
              called ↦ ξ,
              φ ↦ ξ.output
            ⟧,
            timeval ↦ ⟦
              tv-sec ↦ ∅,
              tv-usec ↦ ∅,
              self ↦ ξ
            ⟧,
            sockaddr-in ↦ ⟦
              sin-family ↦ ∅,
              sin-port ↦ ∅,
              sin-addr ↦ ∅,
              sin-zero ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              ),
              size ↦ ξ.sin-family.size.plus(
                α0 ↦ ξ.sin-port.size
              ).plus(
                α0 ↦ ξ.sin-addr.size
              ).plus(
                α0 ↦ ξ.sin-zero.size
              )
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

## [org/eolang/sys/win32.phi](./org/eolang/sys/win32.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        sys ↦ ⟦
          win32 ↦ ⟦
            name ↦ ∅,
            args ↦ ∅,
            std-input-handle ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ C0-24-00-00-00-00-00-00
              )
            ),
            std-output-handle ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ C0-26-00-00-00-00-00-00
              )
            ),
            af-inet ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 40-00-00-00-00-00-00-00
              )
            ),
            sock-stream ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 3F-F0-00-00-00-00-00-00
              )
            ),
            ipproto-tcp ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 40-18-00-00-00-00-00-00
              )
            ),
            invalid-socket ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ BF-F0-00-00-00-00-00-00
              )
            ),
            socket-error ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ BF-F0-00-00-00-00-00-00
              )
            ),
            inaddr-none ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ BF-F0-00-00-00-00-00-00
              )
            ),
            winsock-version-2-2 ↦ Φ.org.eolang.bytes(
              Δ ⤍ 02-02
            ),
            φ ↦ ⟦
              λ ⤍ Lorg_eolang_sys_win32_φ
            ⟧,
            return ↦ ⟦
              code ↦ ∅,
              output ↦ ∅,
              called ↦ ξ,
              φ ↦ ξ.output
            ⟧,
            system-time ↦ ⟦
              year ↦ ∅,
              month ↦ ∅,
              day ↦ ∅,
              day-of-week ↦ ∅,
              hour ↦ ∅,
              minute ↦ ∅,
              second ↦ ∅,
              milliseconds ↦ ∅,
              self ↦ ξ
            ⟧,
            sockaddr-in ↦ ⟦
              sin-family ↦ ∅,
              sin-port ↦ ∅,
              sin-addr ↦ ∅,
              sin-zero ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              ),
              size ↦ ξ.sin-family.size.plus(
                α0 ↦ ξ.sin-port.size
              ).plus(
                α0 ↦ ξ.sin-addr.size
              ).plus(
                α0 ↦ ξ.sin-zero.size
              )
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
            length ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            ),
            at ↦ ⟦
              φ ↦ Φ.org.eolang.error(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 43-61-6E-27-74-20-67-65-74-20-61-6E-20-6F-62-6A-65-63-74-20-66-72-6F-6D-20-74-68-65-20-65-6D-70-74-79-20-74-75-70-6C-65
                  )
                )
              ),
              i ↦ ∅
            ⟧,
            with ↦ ⟦
              φ ↦ ξ.ρ.ρ.ρ.tuple(
                α0 ↦ ξ.ρ,
                α1 ↦ ξ.x
              ),
              x ↦ ∅
            ⟧
          ⟧,
          length ↦ ⟦
            len ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.ρ.head.length.plus(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 3F-F0-00-00-00-00-00-00
                  )
                )
              )
            ).as-bytes,
            φ ↦ Φ.org.eolang.number(
              α0 ↦ ξ.len
            )
          ⟧,
          at ↦ ⟦
            i ↦ ∅,
            len ↦ ξ.ρ.length,
            idx ↦ Φ.org.eolang.dataized(
              α0 ↦ ξ.i
            ).as-bytes,
            index ↦ Φ.org.eolang.dataized(
              α0 ↦ Φ.org.eolang.number(
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
            φ ↦ Φ.org.eolang.number(
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
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ BF-F0-00-00-00-00-00-00
                  )
                )
              ).gt(
                α0 ↦ ξ.ρ.index
              ).if(
                α0 ↦ ξ.ρ.at-fast(
                  α0 ↦ ξ.tup.head,
                  α1 ↦ ξ.len.plus(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  )
                ),
                α1 ↦ ξ.tup.tail
              )
            ⟧
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

## [org/eolang/txt/regex.phi](./org/eolang/txt/regex.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        txt ↦ ⟦
          regex ↦ ⟦
            expression ↦ ∅,
            φ ↦ ξ.compiled,
            compiled ↦ ⟦
              λ ⤍ Lorg_eolang_txt_regex_compiled
            ⟧,
            pattern ↦ ⟦
              serialized ↦ ∅,
              matches ↦ ⟦
                φ ↦ ξ.ρ.match(
                  α0 ↦ ξ.txt
                ).next.exists,
                txt ↦ ∅
              ⟧,
              match ↦ ⟦
                txt ↦ ∅,
                next ↦ ξ.matched-from-index(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 3F-F0-00-00-00-00-00-00
                    )
                  ),
                  α1 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                ).matched,
                matched-from-index ↦ ⟦
                  λ ⤍ Lorg_eolang_txt_regex_pattern_match_matched_from_index,
                  position ↦ ∅,
                  start ↦ ∅
                ⟧,
                matched ↦ ⟦
                  position ↦ ∅,
                  start ↦ ∅,
                  from ↦ ∅,
                  to ↦ ∅,
                  groups ↦ ∅,
                  matched ↦ ξ,
                  groups-count ↦ ξ.groups.length,
                  exists ↦ ξ.start.gte(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 00-00-00-00-00-00-00-00
                      )
                    )
                  ),
                  next ↦ ξ.exists.if(
                    α0 ↦ ξ.ρ.matched-from-index(
                      α0 ↦ ξ.position.plus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 3F-F0-00-00-00-00-00-00
                          )
                        )
                      ),
                      α1 ↦ ξ.to
                    ).matched,
                    α1 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 4D-61-74-63-68-65-64-20-62-6C-6F-63-6B-20-64-6F-65-73-20-6E-6F-74-20-65-78-69-73-74-2C-20-63-61-6E-27-74-20-67-65-74-20-6E-65-78-74
                        )
                      )
                    )
                  ),
                  text ↦ ξ.exists.if(
                    α0 ↦ ξ.group(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 00-00-00-00-00-00-00-00
                        )
                      )
                    ),
                    α1 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 4D-61-74-63-68-65-64-20-62-6C-6F-63-6B-20-64-6F-65-73-20-6E-6F-74-20-65-78-69-73-74-2C-20-63-61-6E-27-74-20-67-65-74-20-74-65-78-74
                        )
                      )
                    )
                  ),
                  group ↦ ⟦
                    φ ↦ ξ.ρ.groups.at(
                      α0 ↦ ξ.index
                    ),
                    index ↦ ∅
                  ⟧
                ⟧,
                not-matched ↦ ⟦
                  position ↦ ∅,
                  φ ↦ ξ.ρ.matched(
                    α0 ↦ ξ.position,
                    α1 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    ),
                    α2 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 4D-61-74-63-68-65-64-20-62-6C-6F-63-6B-20-64-6F-65-73-20-6E-6F-74-20-65-78-69-73-74-2C-20-63-61-6E-27-74-20-67-65-74-20-27-66-72-6F-6D-27-20-70-6F-73-69-74-69-6F-6E
                        )
                      )
                    ),
                    α3 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 4D-61-74-63-68-65-64-20-62-6C-6F-63-6B-20-64-6F-65-73-20-6E-6F-74-20-65-78-69-73-74-2C-20-63-61-6E-27-74-20-67-65-74-20-27-74-6F-27-20-70-6F-73-69-74-69-6F-6E
                        )
                      )
                    ),
                    α4 ↦ Φ.org.eolang.error(
                      α0 ↦ Φ.org.eolang.string(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 4D-61-74-63-68-65-64-20-62-6C-6F-63-6B-20-64-6F-65-73-20-6E-6F-74-20-65-78-69-73-74-2C-20-63-61-6E-27-74-20-67-65-74-20-67-72-6F-75-70-73
                        )
                      )
                    )
                  )
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
  ⟧
}
```

## [org/eolang/txt/sprintf.phi](./org/eolang/txt/sprintf.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        txt ↦ ⟦
          sprintf ↦ ⟦
            λ ⤍ Lorg_eolang_txt_sprintf,
            format ↦ ∅,
            args ↦ ∅
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

## [org/eolang/txt/sscanf.phi](./org/eolang/txt/sscanf.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        txt ↦ ⟦
          sscanf ↦ ⟦
            λ ⤍ Lorg_eolang_txt_sscanf,
            format ↦ ∅,
            read ↦ ∅
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

## [org/eolang/txt/text.phi](./org/eolang/txt/text.phi)

```console
{
  ⟦
    org ↦ ⟦
      eolang ↦ ⟦
        txt ↦ ⟦
          text ↦ ⟦
            origin ↦ ∅,
            φ ↦ ξ.origin,
            is-alphanumeric ↦ ⟦
              φ ↦ Φ.org.eolang.txt.regex(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 2F-5E-5B-41-2D-5A-61-2D-7A-30-2D-39-5D-2B-24-2F
                  )
                )
              ).matches(
                α0 ↦ ξ.ρ.origin
              )
            ⟧,
            is-alpha ↦ ⟦
              φ ↦ Φ.org.eolang.txt.regex(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 2F-5E-5B-61-2D-7A-41-2D-5A-5D-2B-24-2F
                  )
                )
              ).matches(
                α0 ↦ ξ.ρ.origin
              )
            ⟧,
            is-ascii ↦ ⟦
              φ ↦ Φ.org.eolang.txt.regex(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 2F-5E-5B-5C-78-30-30-2D-5C-78-37-46-5D-2A-24-2F
                  )
                )
              ).matches(
                α0 ↦ ξ.ρ.origin
              )
            ⟧,
            slice ↦ ⟦
              start ↦ ∅,
              len ↦ ∅,
              φ ↦ ξ.ρ.ρ.text(
                α0 ↦ ξ.ρ.origin.slice(
                  α0 ↦ ξ.start,
                  α1 ↦ ξ.len
                )
              )
            ⟧,
            trimmed-left ↦ ⟦
              len ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.origin.length
              ).as-bytes,
              idx ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.first-non-space-index(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                )
              ).as-bytes,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).eq(
                α0 ↦ ξ.len
              ).if(
                α0 ↦ ξ.ρ,
                α1 ↦ ξ.ρ.slice(
                  α0 ↦ ξ.idx,
                  α1 ↦ Φ.org.eolang.number(
                    α0 ↦ ξ.len
                  ).minus(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ ξ.idx
                    )
                  )
                )
              ),
              first-non-space-index ↦ ⟦
                index ↦ ∅,
                char ↦ Φ.org.eolang.dataized(
                  α0 ↦ ξ.ρ.ρ.origin.slice(
                    α0 ↦ ξ.index,
                    α1 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 3F-F0-00-00-00-00-00-00
                      )
                    )
                  )
                ).as-bytes,
                φ ↦ ξ.ρ.len.eq(
                  α0 ↦ ξ.index
                ).if(
                  α0 ↦ ξ.index,
                  α1 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 20-
                    )
                  ).eq(
                    α0 ↦ ξ.char
                  ).if(
                    α0 ↦ ξ.ρ.first-non-space-index(
                      α0 ↦ ξ.index.plus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 3F-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    ),
                    α1 ↦ ξ.index
                  )
                )
              ⟧
            ⟧,
            trimmed-right ↦ ⟦
              len ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.origin.length
              ).as-bytes,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).eq(
                α0 ↦ ξ.len
              ).if(
                α0 ↦ ξ.ρ,
                α1 ↦ ξ.ρ.slice(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ),
                  α1 ↦ ξ.first-non-space-index(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ ξ.len
                    ).plus(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ BF-F0-00-00-00-00-00-00
                        )
                      )
                    )
                  )
                )
              ),
              first-non-space-index ↦ ⟦
                index ↦ ∅,
                char ↦ Φ.org.eolang.dataized(
                  α0 ↦ ξ.ρ.ρ.origin.slice(
                    α0 ↦ ξ.index,
                    α1 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 3F-F0-00-00-00-00-00-00
                      )
                    )
                  )
                ).as-bytes,
                φ ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ BF-F0-00-00-00-00-00-00
                  )
                ).eq(
                  α0 ↦ ξ.index
                ).if(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ),
                  α1 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 20-
                    )
                  ).eq(
                    α0 ↦ ξ.char
                  ).if(
                    α0 ↦ ξ.ρ.first-non-space-index(
                      α0 ↦ ξ.index.plus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ BF-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    ),
                    α1 ↦ ξ.index.plus(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    )
                  )
                )
              ⟧
            ⟧,
            trimmed ↦ ⟦
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).eq(
                α0 ↦ ξ.ρ.length
              ).if(
                α0 ↦ ξ.ρ,
                α1 ↦ ξ.ρ.trimmed-left.trimmed-right
              )
            ⟧,
            joined ↦ ⟦
              items ↦ ∅,
              delimiter ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.origin
              ).as-bytes,
              first ↦ ξ.items.at(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ),
              len ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.items.length
              ).as-bytes,
              φ ↦ ξ.ρ.ρ.text(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).eq(
                  α0 ↦ ξ.len
                ).if(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ --
                    )
                  ),
                  α1 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 3F-F0-00-00-00-00-00-00
                    )
                  ).eq(
                    α0 ↦ ξ.len
                  ).if(
                    α0 ↦ ξ.first,
                    α1 ↦ ξ.first.as-bytes.concat(
                      α0 ↦ ξ.with-delimiter(
                        α0 ↦ Φ.org.eolang.string(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ --
                          )
                        ).as-bytes,
                        α1 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 3F-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    ).as-string
                  )
                )
              ),
              with-delimiter ↦ ⟦
                acc ↦ ∅,
                index ↦ ∅,
                φ ↦ ξ.ρ.len.eq(
                  α0 ↦ ξ.index
                ).if(
                  α0 ↦ ξ.acc,
                  α1 ↦ ξ.ρ.with-delimiter(
                    α0 ↦ ξ.acc.concat(
                      α0 ↦ ξ.ρ.delimiter.concat(
                        α0 ↦ ξ.ρ.items.at(
                          α0 ↦ ξ.index
                        )
                      )
                    ),
                    α1 ↦ ξ.index.plus(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    )
                  )
                )
              ⟧
            ⟧,
            repeated ↦ ⟦
              times ↦ ∅,
              bts ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.origin.as-bytes
              ).as-bytes,
              amount ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.times
              ).as-bytes,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).gt(
                α0 ↦ ξ.amount
              ).if(
                α0 ↦ Φ.org.eolang.error(
                  α0 ↦ Φ.org.eolang.txt.sprintf(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 43-61-6E-27-74-20-72-65-70-65-61-74-20-74-65-78-74-20-25-64-20-74-69-6D-65-73
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ ξ.amount
                    )
                  )
                ),
                α1 ↦ ξ.ρ.ρ.text(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ).eq(
                    α0 ↦ ξ.amount
                  ).if(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ --
                      )
                    ),
                    α1 ↦ ξ.rec-repeated(
                      α0 ↦ ξ.bts,
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    ).as-string
                  )
                )
              ),
              rec-repeated ↦ ⟦
                accum ↦ ∅,
                index ↦ ∅,
                φ ↦ ξ.ρ.amount.eq(
                  α0 ↦ ξ.index
                ).if(
                  α0 ↦ ξ.accum,
                  α1 ↦ ξ.ρ.rec-repeated(
                    α0 ↦ ξ.accum.concat(
                      α0 ↦ ξ.ρ.bts
                    ),
                    α1 ↦ ξ.index.plus(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    )
                  )
                )
              ⟧
            ⟧,
            contains ↦ ⟦
              substring ↦ ∅,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ BF-F0-00-00-00-00-00-00
                )
              ).eq(
                α0 ↦ ξ.ρ.index-of(
                  α0 ↦ ξ.substring
                )
              ).not
            ⟧,
            ends-with ↦ ⟦
              substring ↦ ∅,
              substr ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.substring
              ).as-bytes,
              φ ↦ ξ.ρ.index-of(
                α0 ↦ ξ.substr
              ).eq(
                α0 ↦ ξ.ρ.length.minus(
                  α0 ↦ ξ.substr.size
                )
              )
            ⟧,
            starts-with ↦ ⟦
              substring ↦ ∅,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).eq(
                α0 ↦ ξ.ρ.index-of(
                  α0 ↦ ξ.substring
                )
              )
            ⟧,
            index-of ↦ ⟦
              substring ↦ ∅,
              self-len ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.origin.as-bytes.size
              ).as-bytes,
              substr ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.substring
              ).as-bytes,
              sub-len ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.substr.size
              ).as-bytes,
              end ↦ Φ.org.eolang.dataized(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ ξ.self-len
                ).minus(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ ξ.sub-len
                  )
                )
              ).as-bytes,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ ξ.sub-len
              ).gt(
                α0 ↦ ξ.self-len
              ).or(
                α0 ↦ ξ.sub-len.eq(
                  α0 ↦ ξ.self-len
                ).and(
                  α0 ↦ ξ.substr.eq(
                    α0 ↦ ξ.ρ.origin
                  ).not
                )
              ).if(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ BF-F0-00-00-00-00-00-00
                  )
                ),
                α1 ↦ ξ.rec-index-of-substr(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                )
              ),
              rec-index-of-substr ↦ ⟦
                idx ↦ ∅,
                φ ↦ ξ.ρ.end.eq(
                  α0 ↦ ξ.idx
                ).if(
                  α0 ↦ ξ.contains.if(
                    α0 ↦ ξ.idx,
                    α1 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  ),
                  α1 ↦ ξ.contains.if(
                    α0 ↦ ξ.idx,
                    α1 ↦ ξ.ρ.rec-index-of-substr(
                      α0 ↦ ξ.idx.plus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 3F-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    )
                  )
                ),
                contains ↦ ξ.ρ.substr.eq(
                  α0 ↦ ξ.ρ.ρ.slice(
                    α0 ↦ ξ.idx,
                    α1 ↦ ξ.ρ.sub-len
                  )
                )
              ⟧
            ⟧,
            last-index-of ↦ ⟦
              substring ↦ ∅,
              self-len ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.origin.as-bytes.size
              ).as-bytes,
              substr ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.substring
              ).as-bytes,
              sub-len ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.substring.size
              ).as-bytes,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ ξ.sub-len
              ).gt(
                α0 ↦ ξ.self-len
              ).or(
                α0 ↦ ξ.sub-len.eq(
                  α0 ↦ ξ.self-len
                ).and(
                  α0 ↦ ξ.substr.eq(
                    α0 ↦ ξ.ρ.origin
                  ).not
                )
              ).if(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ BF-F0-00-00-00-00-00-00
                  )
                ),
                α1 ↦ ξ.rec-index-of-substr(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ ξ.self-len
                  ).minus(
                    α0 ↦ Φ.org.eolang.number(
                      α0 ↦ ξ.sub-len
                    )
                  )
                )
              ),
              rec-index-of-substr ↦ ⟦
                idx ↦ ∅,
                φ ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).eq(
                  α0 ↦ ξ.idx
                ).if(
                  α0 ↦ ξ.contains.if(
                    α0 ↦ ξ.idx,
                    α1 ↦ Φ.org.eolang.number(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ BF-F0-00-00-00-00-00-00
                      )
                    )
                  ),
                  α1 ↦ ξ.contains.if(
                    α0 ↦ ξ.idx,
                    α1 ↦ ξ.ρ.rec-index-of-substr(
                      α0 ↦ ξ.idx.plus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ BF-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    )
                  )
                ),
                contains ↦ ξ.ρ.substr.eq(
                  α0 ↦ ξ.ρ.ρ.slice(
                    α0 ↦ ξ.idx,
                    α1 ↦ ξ.ρ.sub-len
                  )
                )
              ⟧
            ⟧,
            up-cased ↦ ⟦
              ascii-z ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ascii(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 7A-
                    )
                  )
                )
              ).as-bytes,
              ascii-a ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ascii(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 61-
                    )
                  )
                )
              ).as-bytes,
              distance ↦ Φ.org.eolang.number(
                α0 ↦ ξ.ascii-a
              ).minus(
                α0 ↦ ξ.ascii(
                  α0 ↦ Φ.org.eolang.string(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 41-
                    )
                  )
                )
              ),
              φ ↦ ξ.ρ.ρ.text(
                α0 ↦ Φ.org.eolang.structs.list(
                  α0 ↦ Φ.org.eolang.structs.bytes-as-array(
                    α0 ↦ ξ.ρ.origin.as-bytes
                  )
                ).reduced(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ --
                  ),
                  α1 ↦ ξ.auto-named-attr-at-261-22
                ).as-string
              ),
              ascii ↦ ⟦
                char ↦ ∅,
                φ ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00
                ).concat(
                  α0 ↦ ξ.char.as-bytes
                ).as-i64.as-number
              ⟧,
              auto-named-attr-at-261-22 ↦ ⟦
                accum ↦ ∅,
                byte ↦ ∅,
                ascii-bte ↦ ξ.ρ.ascii(
                  α0 ↦ ξ.byte
                ),
                φ ↦ ξ.accum.concat(
                  α0 ↦ ξ.ascii-bte.lte(
                    α0 ↦ ξ.ρ.ascii-z
                  ).and(
                    α0 ↦ ξ.ascii-bte.gte(
                      α0 ↦ ξ.ρ.ascii-a
                    )
                  ).if(
                    α0 ↦ ξ.ascii-bte.minus(
                      α0 ↦ ξ.ρ.distance
                    ).as-i64.as-bytes.slice(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 40-1C-00-00-00-00-00-00
                        )
                      ),
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    ),
                    α1 ↦ ξ.byte
                  )
                )
              ⟧
            ⟧,
            low-cased ↦ ⟦
              ascii-z ↦ ξ.ρ.up-cased.ascii(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 5A-
                  )
                )
              ),
              ascii-a ↦ ξ.ρ.up-cased.ascii(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 41-
                  )
                )
              ),
              φ ↦ ξ.ρ.ρ.text(
                α0 ↦ Φ.org.eolang.structs.list(
                  α0 ↦ Φ.org.eolang.structs.bytes-as-array(
                    α0 ↦ ξ.ρ.origin.as-bytes
                  )
                ).reduced(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ --
                  ),
                  α1 ↦ ξ.auto-named-attr-at-294-22
                ).as-string
              ),
              auto-named-attr-at-294-22 ↦ ⟦
                accum ↦ ∅,
                byte ↦ ∅,
                ascii-bte ↦ ξ.ρ.ρ.up-cased.ascii(
                  α0 ↦ ξ.byte
                ),
                φ ↦ ξ.accum.concat(
                  α0 ↦ ξ.ascii-bte.lte(
                    α0 ↦ ξ.ρ.ascii-z
                  ).and(
                    α0 ↦ ξ.ascii-bte.gte(
                      α0 ↦ ξ.ρ.ascii-a
                    )
                  ).if(
                    α0 ↦ ξ.ascii-bte.plus(
                      α0 ↦ ξ.ρ.ρ.up-cased.distance
                    ).as-i64.as-bytes.slice(
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 40-1C-00-00-00-00-00-00
                        )
                      ),
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    ),
                    α1 ↦ ξ.byte
                  )
                )
              ⟧
            ⟧,
            at ↦ ⟦
              i ↦ ∅,
              len ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.length
              ).as-bytes,
              idx ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.i
              ).as-bytes,
              index ↦ Φ.org.eolang.dataized(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                ).gt(
                  α0 ↦ ξ.idx
                ).if(
                  α0 ↦ Φ.org.eolang.number(
                    α0 ↦ ξ.len
                  ).plus(
                    α0 ↦ ξ.idx
                  ),
                  α1 ↦ ξ.idx
                )
              ).as-bytes,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).gt(
                α0 ↦ ξ.index
              ).or(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ ξ.index
                ).gte(
                  α0 ↦ ξ.len
                )
              ).if(
                α0 ↦ Φ.org.eolang.error(
                  α0 ↦ Φ.org.eolang.txt.sprintf(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 47-69-76-65-6E-20-69-6E-64-65-78-20-25-64-20-69-73-20-6F-75-74-20-6F-66-20-74-65-78-74-20-62-6F-75-6E-64-73
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ ξ.index
                    )
                  )
                ),
                α1 ↦ ξ.ρ.slice(
                  α0 ↦ ξ.index,
                  α1 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 3F-F0-00-00-00-00-00-00
                    )
                  )
                )
              )
            ⟧,
            replaced ↦ ⟦
              target ↦ ∅,
              replacement ↦ ∅,
              self-as-bytes ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.ρ.origin
              ).as-bytes,
              reinit ↦ Φ.org.eolang.string(
                α0 ↦ ξ.self-as-bytes
              ),
              matched ↦ ξ.target.match(
                α0 ↦ ξ.reinit
              ).next,
              φ ↦ ξ.matched.exists.not.if(
                α0 ↦ Φ.org.eolang.txt.text(
                  α0 ↦ ξ.reinit
                ),
                α1 ↦ Φ.org.eolang.txt.text(
                  α0 ↦ ξ.rec-replaced(
                    α0 ↦ ξ.matched,
                    α1 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ --
                      )
                    ),
                    α2 ↦ ξ.matched.start
                  )
                )
              ),
              rec-replaced ↦ ⟦
                block ↦ ∅,
                accum ↦ ∅,
                start ↦ ∅,
                φ ↦ ξ.block.exists.if(
                  α0 ↦ ξ.ρ.rec-replaced(
                    α0 ↦ ξ.block.next,
                    α1 ↦ ξ.accum.concat(
                      α0 ↦ ξ.ρ.reinit.slice(
                        α0 ↦ ξ.start,
                        α1 ↦ ξ.block.from.minus(
                          α0 ↦ ξ.start
                        )
                      )
                    ).concat(
                      α0 ↦ ξ.ρ.replacement
                    ),
                    α2 ↦ ξ.block.to
                  ),
                  α1 ↦ ξ.accum.concat(
                    α0 ↦ ξ.ρ.reinit.slice(
                      α0 ↦ ξ.start,
                      α1 ↦ ξ.ρ.reinit.length.minus(
                        α0 ↦ ξ.start
                      )
                    )
                  ).as-string
                )
              ⟧
            ⟧,
            as-number ↦ ⟦
              scanned ↦ Φ.org.eolang.txt.sscanf(
                α0 ↦ Φ.org.eolang.string(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 25-66
                  )
                ),
                α1 ↦ ξ.ρ.origin
              ),
              φ ↦ ξ.scanned.length.eq(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ Φ.org.eolang.error(
                  α0 ↦ Φ.org.eolang.txt.sprintf(
                    α0 ↦ Φ.org.eolang.string(
                      α0 ↦ Φ.org.eolang.bytes(
                        Δ ⤍ 43-61-6E-27-74-20-63-6F-6E-76-65-72-74-20-74-65-78-74-20-25-73-20-74-6F-20-6E-75-6D-62-65-72
                      )
                    ),
                    α1 ↦ Φ.org.eolang.tuple(
                      α0 ↦ Φ.org.eolang.tuple.empty,
                      α1 ↦ ξ.ρ.origin
                    )
                  )
                ),
                α1 ↦ ξ.scanned.tail
              )
            ⟧,
            split ↦ ⟦
              delimiter ↦ ∅,
              delim ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.delimiter
              ).as-bytes,
              self-as-bytes ↦ ξ.ρ.origin.as-bytes,
              len ↦ Φ.org.eolang.dataized(
                α0 ↦ ξ.self-as-bytes.size
              ).as-bytes,
              φ ↦ ξ.len.eq(
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 00-00-00-00-00-00-00-00
                  )
                )
              ).if(
                α0 ↦ Φ.org.eolang.tuple.empty,
                α1 ↦ ξ.rec-split(
                  α0 ↦ Φ.org.eolang.tuple.empty,
                  α1 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  ),
                  α2 ↦ Φ.org.eolang.number(
                    α0 ↦ Φ.org.eolang.bytes(
                      Δ ⤍ 00-00-00-00-00-00-00-00
                    )
                  )
                )
              ),
              rec-split ↦ ⟦
                accum ↦ ∅,
                start ↦ ∅,
                current ↦ ∅,
                φ ↦ ξ.ρ.len.eq(
                  α0 ↦ ξ.current
                ).if(
                  α0 ↦ ξ.with-substr,
                  α1 ↦ ξ.ρ.delim.eq(
                    α0 ↦ ξ.ρ.self-as-bytes.slice(
                      α0 ↦ ξ.current,
                      α1 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
                        )
                      )
                    )
                  ).if(
                    α0 ↦ ξ.ρ.rec-split(
                      α0 ↦ ξ.with-substr,
                      α1 ↦ ξ.current.plus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 3F-F0-00-00-00-00-00-00
                          )
                        )
                      ),
                      α2 ↦ ξ.current.plus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 3F-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    ),
                    α1 ↦ ξ.ρ.rec-split(
                      α0 ↦ ξ.accum,
                      α1 ↦ ξ.start,
                      α2 ↦ ξ.current.plus(
                        α0 ↦ Φ.org.eolang.number(
                          α0 ↦ Φ.org.eolang.bytes(
                            Δ ⤍ 3F-F0-00-00-00-00-00-00
                          )
                        )
                      )
                    )
                  )
                ),
                with-substr ↦ ξ.accum.with(
                  α0 ↦ ξ.ρ.self-as-bytes.slice(
                    α0 ↦ ξ.start,
                    α1 ↦ ξ.current.minus(
                      α0 ↦ ξ.start
                    )
                  )
                )
              ⟧
            ⟧,
            chained ↦ ⟦
              others ↦ ∅,
              φ ↦ Φ.org.eolang.number(
                α0 ↦ Φ.org.eolang.bytes(
                  Δ ⤍ 00-00-00-00-00-00-00-00
                )
              ).eq(
                α0 ↦ ξ.others.length
              ).if(
                α0 ↦ ξ.ρ,
                α1 ↦ ξ.ρ.ρ.text(
                  α0 ↦ Φ.org.eolang.structs.list(
                    α0 ↦ ξ.others
                  ).reduced(
                    α0 ↦ ξ.ρ.origin.as-bytes,
                    α1 ↦ ⟦
                      φ ↦ ξ.accum.concat(
                        α0 ↦ ξ.str.as-bytes
                      ),
                      accum ↦ ∅,
                      str ↦ ∅
                    ⟧
                  ).as-string
                )
              )
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
            α0 ↦ Φ.org.eolang.number(
              α0 ↦ Φ.org.eolang.bytes(
                Δ ⤍ 00-00-00-00-00-00-00-00
              )
            )
          ).as-bool.if(
            α0 ↦ ξ.loop(
              α0 ↦ Φ.org.eolang.number(
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
                α0 ↦ Φ.org.eolang.number(
                  α0 ↦ Φ.org.eolang.bytes(
                    Δ ⤍ 3F-F0-00-00-00-00-00-00
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
                      α0 ↦ Φ.org.eolang.number(
                        α0 ↦ Φ.org.eolang.bytes(
                          Δ ⤍ 3F-F0-00-00-00-00-00-00
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
