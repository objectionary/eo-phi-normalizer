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

## [org/eolang/cti.phi](./org/eolang/cti.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      cti(delegate, level, message) ↦ ⟦
        φ ↦ ξ.delegate
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

## [org/eolang/fs/dir.phi](./org/eolang/fs/dir.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      fs() ↦ ⟦
        dir(file) ↦ ⟦
          φ ↦ ξ.file,
          is-directory ↦ Φ̇.true,
          made() ↦ ⟦
            φ ↦ ξ.ρ.exists.if(
              ξ.ρ, Φ̇.seq(Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.mkdir), ξ.ρ))
            ),
            mkdir() ↦ ⟦
              λ ⤍ Lorg_eolang_fs_dir_made_mkdir
            ⟧
          ⟧,
          walk(glob) ↦ ⟦
            λ ⤍ Lorg_eolang_fs_dir_walk
          ⟧,
          deleted() ↦ ⟦
            walked ↦ ξ.ρ.walk("**").at.ρ,
            len ↦ Φ̇.dataized(ξ.walked.length).as-bytes,
            φ ↦ ξ.ρ.exists.if(
              Φ̇.seq(
                Φ̇.tuple(
                  Φ̇.tuple(Φ̇.tuple.empty, ξ.rec-delete(ξ.walked, 0)), ξ.ρ
                )
              ),
              ξ.ρ
            ),
            rec-delete(tup, index) ↦ ⟦
              φ ↦ ξ.ρ.len.eq(ξ.index).if(
                Φ̇.true,
                Φ̇.seq(
                  Φ̇.tuple(
                    Φ̇.tuple(Φ̇.tuple.empty, ξ.tup.tail.deleted.exists),
                    ξ.ρ.rec-delete(ξ.tup.head, ξ.index.plus(1))
                  )
                )
              )
            ⟧
          ⟧,
          tmpfile() ↦ ⟦
            φ ↦ ξ.ρ.exists.if(
              Φ̇.fs.file(Φ̇.string(ξ.touch.as-bytes)),
              Φ̇.error(
                Φ̇.txt.sprintf(
                  "Directory %s does not exist, can't create temporary file",
                  Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.path)
                )
              )
            ),
            touch() ↦ ⟦
              λ ⤍ Lorg_eolang_fs_dir_tmpfile_touch
            ⟧
          ⟧,
          open(mode, scope) ↦ ⟦
            φ ↦ Φ̇.error(
              Φ̇.txt.sprintf(
                "The file %s is a directory, can't open for I/O operations",
                Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.path)
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
⟧}
```

## [org/eolang/fs/file.phi](./org/eolang/fs/file.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      fs() ↦ ⟦
        file(path) ↦ ⟦
          φ ↦ ξ.path,
          is-directory() ↦ ⟦
            λ ⤍ Lorg_eolang_fs_file_is_directory
          ⟧,
          exists() ↦ ⟦
            λ ⤍ Lorg_eolang_fs_file_exists
          ⟧,
          touched() ↦ ⟦
            φ ↦ ξ.ρ.exists.if(
              ξ.ρ, Φ̇.seq(Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.touch), ξ.ρ))
            ),
            touch() ↦ ⟦
              λ ⤍ Lorg_eolang_fs_file_touched_touch
            ⟧
          ⟧,
          deleted() ↦ ⟦
            φ ↦ ξ.ρ.exists.if(
              Φ̇.seq(Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.delete), ξ.ρ)), ξ.ρ
            ),
            delete() ↦ ⟦
              λ ⤍ Lorg_eolang_fs_file_deleted_delete
            ⟧
          ⟧,
          size() ↦ ⟦
            λ ⤍ Lorg_eolang_fs_file_size
          ⟧,
          moved(target) ↦ ⟦
            φ ↦ Φ̇.fs.file(Φ̇.string(ξ.move.as-bytes)),
            move() ↦ ⟦
              λ ⤍ Lorg_eolang_fs_file_moved_move
            ⟧
          ⟧,
          as-path() ↦ ⟦
            φ ↦ Φ̇.fs.path(ξ.ρ.path).determined
          ⟧,
          open(mode, scope) ↦ ⟦
            access ↦ Φ̇.dataized(ξ.mode).as-bytes,
            read ↦ ξ.access.eq("r"),
            write ↦ ξ.access.eq("w"),
            append ↦ ξ.access.eq("a"),
            read-write ↦ ξ.access.eq("r+"),
            write-read ↦ ξ.access.eq("w+"),
            read-append ↦ ξ.access.eq("a+"),
            can-read ↦ ξ.read.or(ξ.read-write).or(ξ.write-read.or(ξ.read-append)).as-bool,
            can-write ↦ ξ.write.or(ξ.read-write).or(ξ.write-read.or(ξ.read-append)).or(
              ξ.append
            ).as-bool,
            must-exists ↦ ξ.read.or(ξ.read-write).as-bool,
            truncate ↦ ξ.write.or(ξ.write-read).as-bool,
            φ ↦ ξ.can-read.not.and(ξ.can-write.not).if(
              Φ̇.error(
                "Wrong access mod. Only next modes are available: 'r', 'w', 'a', 'r+', 'w+', 'a+'"
              ),
              ξ.ρ.exists.not.if(
                ξ.must-exists.if(
                  Φ̇.error(
                    Φ̇.txt.sprintf(
                      "File must exist for given access mod: '%s'",
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.access)
                    )
                  ),
                  Φ̇.seq(
                    Φ̇.tuple(
                      Φ̇.tuple(
                        Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.touched.touch), ξ.process-file
                      ),
                      ξ.ρ
                    )
                  )
                ),
                ξ.truncate.if(
                  Φ̇.seq(
                    Φ̇.tuple(
                      Φ̇.tuple(
                        Φ̇.tuple(
                          Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.deleted.delete), ξ.ρ.touched.touch
                        ),
                        ξ.process-file
                      ),
                      ξ.ρ
                    )
                  ),
                  Φ̇.seq(Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.process-file), ξ.ρ))
                )
              )
            ),
            process-file() ↦ ⟦
              λ ⤍ Lorg_eolang_fs_file_open_process_file
            ⟧,
            file-stream() ↦ ⟦
              read(size) ↦ ⟦
                φ ↦ ξ.input-block(Φ̇.bytes(⟦ Δ ⤍ -- ⟧)).read(ξ.size).self,
                input-block(buffer) ↦ ⟦
                  self ↦ ξ,
                  φ ↦ ξ.buffer,
                  read(size) ↦ ⟦
                    read-bytes ↦ Φ̇.dataized(ξ.ρ.ρ.read-bytes(ξ.size)).as-bytes,
                    φ ↦ ξ.ρ.ρ.ρ.ρ.can-read.not.if(
                      ξ.auto-named-attr-at-211-18,
                      Φ̇.seq(
                        Φ̇.tuple(
                          Φ̇.tuple(Φ̇.tuple.empty, ξ.read-bytes), ξ.ρ.ρ.input-block(ξ.read-bytes)
                        )
                      )
                    ).self,
                    auto-named-attr-at-211-18() ↦ ⟦
                      self ↦ ξ,
                      φ ↦ Φ̇.error(
                        Φ̇.txt.sprintf(
                          "Can't read from file with provided access mode '%s'",
                          Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.ρ.ρ.ρ.ρ.access)
                        )
                      )
                    ⟧
                  ⟧
                ⟧,
                read-bytes(size) ↦ ⟦
                  λ ⤍ Lorg_eolang_fs_file_open_file_stream_read_read_bytes
                ⟧
              ⟧,
              write(buffer) ↦ ⟦
                φ ↦ ξ.output-block.write(ξ.buffer).self,
                output-block() ↦ ⟦
                  self ↦ ξ,
                  φ ↦ Φ̇.true,
                  write(buffer) ↦ ⟦
                    φ ↦ ξ.ρ.ρ.ρ.ρ.can-write.not.if(
                      ξ.auto-named-attr-at-252-18,
                      Φ̇.seq(
                        Φ̇.tuple(
                          Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.ρ.written-bytes(ξ.buffer)), ξ.ρ.ρ.output-block
                        )
                      )
                    ).self,
                    auto-named-attr-at-252-18() ↦ ⟦
                      self ↦ ξ,
                      φ ↦ Φ̇.error(
                        Φ̇.txt.sprintf(
                          "Can't write to file with provided access mode '%s'",
                          Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.ρ.ρ.ρ.ρ.access)
                        )
                      )
                    ⟧
                  ⟧
                ⟧,
                written-bytes(buffer) ↦ ⟦
                  λ ⤍ Lorg_eolang_fs_file_open_file_stream_write_written_bytes
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
⟧}
```

## [org/eolang/fs/path.phi](./org/eolang/fs/path.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      fs() ↦ ⟦
        path(uri) ↦ ⟦
          φ ↦ Φ̇.sys.os.is-windows.if(
            ξ.win32(Φ̇.string(ξ.uri.as-bytes)), ξ.posix(Φ̇.string(ξ.uri.as-bytes))
          ).determined,
          joined(paths) ↦ ⟦
            joined-path ↦ Φ̇.string(
              Φ̇.txt.text(ξ.ρ.separator).joined(ξ.paths).as-bytes
            ),
            φ ↦ Φ̇.sys.os.is-windows.if(
              ξ.ρ.win32(ξ.joined-path), ξ.ρ.posix(ξ.joined-path)
            ).normalized
          ⟧,
          separator() ↦ ⟦
            φ ↦ Φ̇.sys.os.is-windows.if(ξ.ρ.win32.separator, ξ.ρ.posix.separator)
          ⟧,
          posix(uri) ↦ ⟦
            determined ↦ ξ,
            separator ↦ "/",
            as-file ↦ Φ̇.fs.file(ξ.uri).size.ρ,
            as-dir ↦ Φ̇.fs.dir(Φ̇.fs.file(ξ.uri)).made.ρ,
            φ ↦ ξ.uri,
            is-absolute() ↦ ⟦
              φ ↦ ξ.ρ.uri.length.gt(0).and(
                ξ.ρ.uri.as-bytes.slice(0, 1).eq(ξ.ρ.separator)
              )
            ⟧,
            normalized() ↦ ⟦
              uri-as-bytes ↦ ξ.ρ.uri.as-bytes,
              is-absolute ↦ ξ.ρ.is-absolute.as-bool,
              has-trailing-slash ↦ ξ.uri-as-bytes.size.gt(0).and(
                ξ.uri-as-bytes.slice(ξ.uri-as-bytes.size.plus(-1), 1).eq(
                  ξ.ρ.separator
                )
              ),
              path ↦ Φ̇.txt.text(ξ.ρ.separator).joined(
                Φ̇.structs.list(Φ̇.txt.text(ξ.ρ.uri).split(ξ.ρ.separator)).reduced(
                  Φ̇.tuple.empty, ξ.auto-named-attr-at-102-25
                )
              ),
              normalized ↦ ξ.ρ.uri.length.eq(0).if(
                ".",
                ξ.is-absolute.if(ξ.ρ.separator.concat(ξ.path), ξ.path).concat(
                  ξ.has-trailing-slash.if(ξ.ρ.separator, Φ̇.bytes(⟦ Δ ⤍ -- ⟧))
                )
              ).as-bytes,
              φ ↦ ξ.ρ.ρ.posix(
                ξ.normalized.eq("//").if("/", Φ̇.string(ξ.normalized))
              ).determined,
              auto-named-attr-at-102-25(accum, segment) ↦ ⟦
                φ ↦ ξ.segment.eq("..").if(
                  ξ.accum.length.gt(0).and(ξ.accum.tail.eq("..").not).if(
                    ξ.accum.head, ξ.ρ.is-absolute.not.if(ξ.accum.with(ξ.segment), ξ.accum)
                  ),
                  ξ.segment.eq(".").or(ξ.segment.eq("")).if(
                    ξ.accum, ξ.accum.with(ξ.segment)
                  )
                )
              ⟧
            ⟧,
            resolved(other) ↦ ⟦
              other-as-bytes ↦ ξ.other.as-bytes,
              φ ↦ ξ.ρ.ρ.posix(
                Φ̇.string(
                  ξ.other-as-bytes.slice(0, 1).eq(ξ.ρ.separator).if(
                    ξ.other-as-bytes, ξ.ρ.uri.concat(ξ.ρ.separator).concat(ξ.other-as-bytes)
                  )
                )
              ).normalized
            ⟧,
            basename() ↦ ⟦
              pth ↦ Φ̇.dataized(ξ.ρ.uri).as-bytes,
              txt ↦ Φ̇.txt.text(Φ̇.string(ξ.pth)),
              slice-start-idx ↦ Φ̇.dataized(
                ξ.txt.last-index-of(ξ.ρ.separator).plus(1)
              ).as-bytes,
              φ ↦ Φ̇.string(
                ξ.pth.size.eq(0).or(ξ.slice-start-idx.eq(0)).if(
                  ξ.pth,
                  ξ.txt.slice(
                    ξ.slice-start-idx, ξ.txt.length.minus(Φ̇.number(ξ.slice-start-idx))
                  ).as-bytes
                )
              )
            ⟧,
            extname() ↦ ⟦
              base ↦ Φ̇.dataized(ξ.ρ.basename).as-bytes,
              txt ↦ Φ̇.txt.text(Φ̇.string(ξ.base)),
              slice-start-idx ↦ Φ̇.dataized(ξ.txt.last-index-of(".")).as-bytes,
              φ ↦ ξ.base.size.eq(0).or(ξ.slice-start-idx.eq(-1)).if(
                "",
                Φ̇.string(
                  ξ.txt.slice(
                    ξ.slice-start-idx, ξ.txt.length.minus(Φ̇.number(ξ.slice-start-idx))
                  ).as-bytes
                )
              )
            ⟧,
            dirname() ↦ ⟦
              pth ↦ Φ̇.dataized(ξ.ρ.uri).as-bytes,
              txt ↦ Φ̇.txt.text(Φ̇.string(ξ.pth)),
              len ↦ Φ̇.dataized(ξ.txt.last-index-of(ξ.ρ.separator)).as-bytes,
              φ ↦ Φ̇.string(
                ξ.pth.size.eq(0).or(ξ.len.eq(-1)).if(
                  ξ.pth, ξ.txt.slice(0, ξ.len).as-bytes
                )
              )
            ⟧
          ⟧,
          win32(uri) ↦ ⟦
            separator ↦ "\\",
            φ ↦ ξ.validated(
              Φ̇.string(ξ.validated.separated-correctly(ξ.uri).as-bytes)
            ).determined,
            validated(uri) ↦ ⟦
              determined ↦ ξ,
              separator ↦ ξ.ρ.separator,
              as-file ↦ Φ̇.fs.file(ξ.uri).size.ρ,
              as-dir ↦ Φ̇.fs.dir(Φ̇.fs.file(ξ.uri)).made.ρ,
              φ ↦ ξ.uri,
              is-drive-relative(uri) ↦ ⟦
                φ ↦ Φ̇.txt.regex("/^[a-zA-Z]:/").matches(ξ.uri).as-bool
              ⟧,
              is-root-relative(uri) ↦ ⟦
                uri-as-bytes ↦ Φ̇.dataized(ξ.uri).as-bytes,
                φ ↦ ξ.uri-as-bytes.size.gt(0).and(
                  ξ.uri-as-bytes.slice(0, 1).eq(ξ.ρ.separator)
                )
              ⟧,
              separated-correctly(uri) ↦ ⟦
                uri-as-bytes ↦ Φ̇.dataized(ξ.uri).as-bytes,
                pth ↦ Φ̇.txt.text(Φ̇.string(ξ.uri-as-bytes)),
                replaced ↦ Φ̇.dataized(
                  ξ.pth.replaced(Φ̇.txt.regex("/\\//"), ξ.ρ.separator)
                ).as-bytes,
                φ ↦ ξ.pth.index-of(ξ.ρ.ρ.ρ.ρ.path.posix.separator).eq(-1).if(
                  Φ̇.string(ξ.uri-as-bytes), Φ̇.string(ξ.replaced)
                )
              ⟧,
              is-absolute() ↦ ⟦
                uri-as-bytes ↦ Φ̇.dataized(ξ.ρ.uri).as-bytes,
                φ ↦ ξ.uri-as-bytes.size.eq(0).if(
                  Φ̇.false,
                  ξ.ρ.is-root-relative(ξ.uri-as-bytes).or(
                    ξ.uri-as-bytes.size.gt(1).and(
                      ξ.ρ.is-drive-relative(ξ.uri-as-bytes)
                    )
                  )
                )
              ⟧,
              normalized() ↦ ⟦
                uri-as-bytes ↦ Φ̇.dataized(ξ.ρ.uri).as-bytes,
                is-drive-relative ↦ ξ.ρ.is-drive-relative(ξ.uri-as-bytes).as-bool,
                is-root-relative ↦ ξ.ρ.is-root-relative(ξ.uri-as-bytes).as-bool,
                driveless ↦ Φ̇.dataized(
                  ξ.is-drive-relative.if(
                    ξ.uri-as-bytes.slice(2, ξ.uri-as-bytes.size.plus(-2)), ξ.uri-as-bytes
                  )
                ).as-bytes,
                has-trailing-slash ↦ ξ.uri-as-bytes.size.gt(0).and(
                  ξ.uri-as-bytes.slice(ξ.uri-as-bytes.size.plus(-1), 1).eq(
                    ξ.ρ.separator
                  )
                ),
                path ↦ Φ̇.dataized(
                  Φ̇.txt.text(ξ.ρ.separator).joined(
                    Φ̇.structs.list(
                      Φ̇.txt.text(ξ.driveless).split(ξ.ρ.separator)
                    ).reduced(Φ̇.tuple.empty, ξ.auto-named-attr-at-357-27)
                  )
                ).as-bytes,
                normalized ↦ ξ.driveless.size.eq(0).if(
                  ".",
                  ξ.is-drive-relative.if(
                    ξ.driveless.slice(0, 1).eq(ξ.ρ.separator).if(
                      ξ.ρ.uri.slice(0, 3), ξ.ρ.uri.slice(0, 2)
                    ),
                    ξ.is-root-relative.if(ξ.ρ.separator, Φ̇.bytes(⟦ Δ ⤍ -- ⟧))
                  ).concat(ξ.path).concat(
                    ξ.has-trailing-slash.if(ξ.ρ.separator, Φ̇.bytes(⟦ Δ ⤍ -- ⟧))
                  )
                ).as-bytes,
                φ ↦ ξ.ρ.ρ.validated(
                  ξ.normalized.eq("\\\\").if(
                    ξ.ρ.separator, Φ̇.string(ξ.normalized)
                  )
                ).determined,
                auto-named-attr-at-357-27(accum, segment) ↦ ⟦
                  φ ↦ ξ.segment.eq("..").if(
                    ξ.accum.length.gt(0).and(ξ.accum.tail.eq("..").not).if(
                      ξ.accum.head,
                      ξ.ρ.is-root-relative.not.and(ξ.ρ.is-drive-relative.not).if(
                        ξ.accum.with(ξ.segment), ξ.accum
                      )
                    ),
                    ξ.segment.eq(".").or(ξ.segment.eq("")).if(
                      ξ.accum, ξ.accum.with(ξ.segment)
                    )
                  )
                ⟧
              ⟧,
              resolved(other) ↦ ⟦
                uri-as-bytes ↦ Φ̇.dataized(ξ.ρ.uri).as-bytes,
                valid-other ↦ Φ̇.dataized(ξ.ρ.separated-correctly(ξ.other)).as-bytes,
                other-is-drive-relative ↦ ξ.ρ.is-drive-relative(ξ.valid-other).as-bool,
                other-is-root-relative ↦ ξ.ρ.is-root-relative(ξ.valid-other).as-bool,
                φ ↦ ξ.ρ.ρ.validated(
                  Φ̇.string(
                    ξ.other-is-drive-relative.if(
                      ξ.valid-other,
                      ξ.other-is-root-relative.if(
                        ξ.ρ.is-drive-relative(ξ.uri-as-bytes).if(
                          ξ.uri-as-bytes.slice(0, 2).concat(ξ.valid-other), ξ.valid-other
                        ),
                        ξ.uri-as-bytes.concat(ξ.ρ.separator).concat(
                          ξ.valid-other
                        )
                      )
                    )
                  )
                ).normalized
              ⟧,
              basename() ↦ ⟦
                pth ↦ Φ̇.dataized(ξ.ρ.uri).as-bytes,
                txt ↦ Φ̇.txt.text(Φ̇.string(ξ.pth)),
                slice-start-idx ↦ Φ̇.dataized(
                  ξ.txt.last-index-of(ξ.ρ.separator).plus(1)
                ).as-bytes,
                φ ↦ Φ̇.string(
                  ξ.pth.size.eq(0).or(ξ.slice-start-idx.eq(0)).if(
                    ξ.pth,
                    ξ.txt.slice(
                      ξ.slice-start-idx, ξ.txt.length.minus(Φ̇.number(ξ.slice-start-idx))
                    ).as-bytes
                  )
                )
              ⟧,
              extname() ↦ ⟦
                base ↦ Φ̇.dataized(ξ.ρ.basename).as-bytes,
                txt ↦ Φ̇.txt.text(Φ̇.string(ξ.base)),
                slice-start-idx ↦ Φ̇.dataized(ξ.txt.last-index-of(".")).as-bytes,
                φ ↦ ξ.base.size.eq(0).or(ξ.slice-start-idx.eq(-1)).if(
                  "",
                  Φ̇.string(
                    ξ.txt.slice(
                      ξ.slice-start-idx, ξ.txt.length.minus(Φ̇.number(ξ.slice-start-idx))
                    ).as-bytes
                  )
                )
              ⟧,
              dirname() ↦ ⟦
                pth ↦ Φ̇.dataized(ξ.ρ.uri).as-bytes,
                txt ↦ Φ̇.txt.text(Φ̇.string(ξ.pth)),
                len ↦ Φ̇.dataized(ξ.txt.last-index-of(ξ.ρ.separator)).as-bytes,
                φ ↦ Φ̇.string(
                  ξ.pth.size.eq(0).or(ξ.len.eq(-1)).if(
                    ξ.pth, ξ.txt.slice(0, ξ.len).as-bytes
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
⟧}
```

## [org/eolang/fs/tmpdir.phi](./org/eolang/fs/tmpdir.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      fs() ↦ ⟦
        tmpdir() ↦ ⟦
          φ ↦ Φ̇.fs.dir(Φ̇.fs.file(Φ̇.string(ξ.os-tmp-dir))),
          os-tmp-dir ↦ Φ̇.dataized(ξ.os-tmp-dir-1).as-bytes,
          os-tmp-dir-1() ↦ ⟦
            tmpdir ↦ Φ̇.dataized(Φ̇.sys.getenv("TMPDIR")).as-bytes,
            tmp ↦ Φ̇.dataized(Φ̇.sys.getenv("TMP")).as-bytes,
            temp ↦ Φ̇.dataized(Φ̇.sys.getenv("TEMP")).as-bytes,
            tempdir ↦ Φ̇.dataized(Φ̇.sys.getenv("TEMPDIR")).as-bytes,
            userprofile ↦ Φ̇.dataized(Φ̇.sys.getenv("USERPROFILE")).as-bytes,
            φ ↦ Φ̇.sys.os.is-windows.if(
              ξ.tmp.eq("").if(
                ξ.temp.eq("").if(
                  ξ.userprofile.eq("").if("C:\\Windows", ξ.userprofile), ξ.temp
                ),
                ξ.tmp
              ),
              ξ.tmpdir.eq("").if(
                ξ.tmp.eq("").if(
                  ξ.temp.eq("").if(
                    ξ.tempdir.eq("").if("/tmp", ξ.tempdir), ξ.temp
                  ),
                  ξ.tmp
                ),
                ξ.tmpdir
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
⟧}
```

## [org/eolang/go.phi](./org/eolang/go.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      go() ↦ ⟦
        id ↦ Φ̇.dataized(
          Φ̇.malloc.of(
            8,
            ⟦
              m ↦ ∅,
              φ ↦ ξ.m.put(ξ.m.id)
            ⟧
          )
        ).as-bytes,
        to(body) ↦ ⟦
          φ ↦ Φ̇.try(ξ.body(ξ.token), ξ.auto-named-attr-at-63-9, Φ̇.true),
          token() ↦ ⟦
            backward ↦ Φ̇.error(ξ.jump(ξ.ρ.ρ.to(ξ.ρ.body))),
            jump(value) ↦ ⟦
              id ↦ ξ.ρ.ρ.ρ.id
            ⟧,
            forward(res) ↦ ⟦
              φ ↦ Φ̇.error(ξ.ρ.jump(ξ.res))
            ⟧
          ⟧,
          auto-named-attr-at-63-9(e) ↦ ⟦
            φ ↦ ξ.ρ.ρ.id.eq(ξ.e.id).if(ξ.e.value, Φ̇.error(ξ.e))
          ⟧
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

## [org/eolang/io/bytes-as-input.phi](./org/eolang/io/bytes-as-input.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      io() ↦ ⟦
        bytes-as-input(bts) ↦ ⟦
          read(size) ↦ ⟦
            φ ↦ ξ.input-block(ξ.ρ.bts, Φ̇.bytes(⟦ Δ ⤍ -- ⟧)).read(ξ.size).self,
            input-block(data, buffer) ↦ ⟦
              self ↦ ξ,
              φ ↦ ξ.buffer,
              read(size) ↦ ⟦
                to-read ↦ Φ̇.dataized(ξ.size).as-bytes,
                available ↦ Φ̇.dataized(ξ.ρ.data.size).as-bytes,
                next ↦ Φ̇.dataized(
                  Φ̇.number(ξ.available).gt(ξ.to-read).if(ξ.to-read, ξ.available)
                ).as-bytes,
                φ ↦ ξ.available.eq(0).if(
                  ξ.ρ.ρ.input-block(Φ̇.bytes(⟦ Δ ⤍ -- ⟧), Φ̇.bytes(⟦ Δ ⤍ -- ⟧)),
                  ξ.ρ.ρ.input-block(
                    ξ.ρ.data.slice(
                      ξ.next, Φ̇.number(ξ.available).minus(Φ̇.number(ξ.next))
                    ).as-bytes,
                    ξ.ρ.data.slice(0, ξ.next).as-bytes
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
⟧}
```

## [org/eolang/io/console.phi](./org/eolang/io/console.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      io() ↦ ⟦
        console() ↦ ⟦
          φ ↦ Φ̇.sys.os.is-windows.if(ξ.windows-console, ξ.posix-console).platform,
          posix-console() ↦ ⟦
            platform ↦ ξ,
            read(size) ↦ ⟦
              φ ↦ ξ.input-block(Φ̇.bytes(⟦ Δ ⤍ -- ⟧)).read(ξ.size).self,
              input-block(buffer) ↦ ⟦
                self ↦ ξ,
                φ ↦ ξ.buffer,
                read(size) ↦ ⟦
                  read-bytes ↦ Φ̇.dataized(
                    Φ̇.sys.posix(
                      "read",
                      Φ̇.tuple(
                        Φ̇.tuple(Φ̇.tuple.empty, Φ̇.sys.posix.stdin-fileno), ξ.size
                      )
                    ).output
                  ).as-bytes,
                  φ ↦ Φ̇.seq(
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.read-bytes), ξ.ρ.ρ.input-block(ξ.read-bytes)
                    )
                  ).self
                ⟧
              ⟧
            ⟧,
            write(buffer) ↦ ⟦
              φ ↦ ξ.output-block.write(ξ.buffer).self,
              output-block() ↦ ⟦
                self ↦ ξ,
                φ ↦ Φ̇.true,
                write(buffer) ↦ ⟦
                  φ ↦ Φ̇.seq(
                    Φ̇.tuple(
                      Φ̇.tuple(
                        Φ̇.tuple.empty,
                        Φ̇.sys.posix(
                          "write",
                          Φ̇.tuple(
                            Φ̇.tuple(
                              Φ̇.tuple(
                                Φ̇.tuple.empty, Φ̇.sys.posix.stdout-fileno
                              ),
                              ξ.buffer
                            ),
                            ξ.buffer.size
                          )
                        ).code
                      ),
                      ξ.ρ.ρ.output-block
                    )
                  ).self
                ⟧
              ⟧
            ⟧
          ⟧,
          windows-console() ↦ ⟦
            platform ↦ ξ,
            read(size) ↦ ⟦
              φ ↦ ξ.input-block(Φ̇.bytes(⟦ Δ ⤍ -- ⟧)).read(ξ.size).self,
              input-block(buffer) ↦ ⟦
                self ↦ ξ,
                φ ↦ ξ.buffer,
                read(size) ↦ ⟦
                  read-bytes ↦ Φ̇.dataized(
                    Φ̇.sys.win32(
                      "ReadFile",
                      Φ̇.tuple(
                        Φ̇.tuple(Φ̇.tuple.empty, Φ̇.sys.win32.std-input-handle), ξ.size
                      )
                    ).output
                  ).as-bytes,
                  φ ↦ Φ̇.seq(
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.read-bytes), ξ.ρ.ρ.input-block(ξ.read-bytes)
                    )
                  ).self
                ⟧
              ⟧
            ⟧,
            write(buffer) ↦ ⟦
              φ ↦ ξ.output-block.write(ξ.buffer).self,
              output-block() ↦ ⟦
                self ↦ ξ,
                φ ↦ Φ̇.true,
                write(buffer) ↦ ⟦
                  φ ↦ Φ̇.seq(
                    Φ̇.tuple(
                      Φ̇.tuple(
                        Φ̇.tuple.empty,
                        Φ̇.sys.win32(
                          "WriteFile",
                          Φ̇.tuple(
                            Φ̇.tuple(
                              Φ̇.tuple(
                                Φ̇.tuple.empty, Φ̇.sys.win32.std-output-handle
                              ),
                              ξ.buffer
                            ),
                            ξ.buffer.size
                          )
                        ).code
                      ),
                      ξ.ρ.ρ.output-block
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

## [org/eolang/io/input-length.phi](./org/eolang/io/input-length.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      io() ↦ ⟦
        input-length(input) ↦ ⟦
          chunk ↦ 4096,
          φ ↦ ξ.rec-read(ξ.input, 0),
          rec-read(input, length) ↦ ⟦
            read-bytes ↦ ξ.input.read(ξ.ρ.chunk).read.ρ,
            φ ↦ ξ.read-bytes.size.eq(0).if(
              ξ.length, ξ.ρ.rec-read(ξ.read-bytes, ξ.length.plus(ξ.read-bytes.size))
            )
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

## [org/eolang/io/malloc-as-output.phi](./org/eolang/io/malloc-as-output.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      io() ↦ ⟦
        malloc-as-output(allocated) ↦ ⟦
          write(buffer) ↦ ⟦
            φ ↦ ξ.output-block(0).write(ξ.buffer).self,
            output-block(offset) ↦ ⟦
              self ↦ ξ,
              φ ↦ Φ̇.true,
              write(buffer) ↦ ⟦
                φ ↦ Φ̇.seq(
                  Φ̇.tuple(
                    Φ̇.tuple(
                      Φ̇.tuple.empty, ξ.ρ.ρ.ρ.allocated.write(ξ.ρ.offset, ξ.buffer)
                    ),
                    ξ.ρ.ρ.output-block(ξ.ρ.offset.plus(ξ.buffer.size))
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
⟧}
```

## [org/eolang/io/stdin.phi](./org/eolang/io/stdin.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      io() ↦ ⟦
        stdin() ↦ ⟦
          φ ↦ ξ.all-lines,
          all-lines() ↦ ⟦
            φ ↦ ξ.rec-read(ξ.ρ.next-line, Φ̇.bytes(⟦ Δ ⤍ -- ⟧), Φ̇.true),
            separator ↦ Φ̇.dataized(Φ̇.sys.line-separator).as-bytes,
            rec-read(line, buffer, first) ↦ ⟦
              φ ↦ ξ.line.length.eq(0).if(
                Φ̇.string(ξ.buffer),
                ξ.ρ.rec-read(
                  ξ.ρ.ρ.next-line,
                  ξ.first.if(
                    ξ.buffer.concat(ξ.line), ξ.buffer.concat(ξ.ρ.separator).concat(ξ.line)
                  ),
                  Φ̇.false
                )
              )
            ⟧
          ⟧,
          next-line() ↦ ⟦
            first ↦ Φ̇.io.console.read(1).self,
            φ ↦ ξ.first.as-bytes.size.eq(0).if(
              "", ξ.rec-read(ξ.first, Φ̇.bytes(⟦ Δ ⤍ -- ⟧))
            ),
            rec-read(input, buffer) ↦ ⟦
              char ↦ ξ.input.as-bytes,
              next ↦ ξ.input.read(1).self,
              φ ↦ ξ.char.eq(Φ̇.bytes(⟦ Δ ⤍ -- ⟧)).or(
                ξ.char.eq("\r").and(ξ.next.as-bytes.eq("\n")).or(ξ.char.eq("\n"))
              ).if(
                Φ̇.string(ξ.buffer), ξ.ρ.rec-read(ξ.next, ξ.buffer.concat(ξ.char))
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
⟧}
```

## [org/eolang/io/stdout.phi](./org/eolang/io/stdout.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      io() ↦ ⟦
        stdout(text) ↦ ⟦
          φ ↦ Φ̇.seq(
            Φ̇.tuple(
              Φ̇.tuple(Φ̇.tuple.empty, Φ̇.io.console.write(ξ.text)), Φ̇.true
            )
          )
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/io/tee-input.phi](./org/eolang/io/tee-input.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      io() ↦ ⟦
        tee-input(input, output) ↦ ⟦
          read(size) ↦ ⟦
            φ ↦ ξ.input-block(ξ.ρ.input, ξ.ρ.output, Φ̇.bytes(⟦ Δ ⤍ -- ⟧)).read(
              ξ.size
            ).self,
            input-block(input, output, buffer) ↦ ⟦
              self ↦ ξ,
              φ ↦ ξ.buffer,
              read(size) ↦ ⟦
                read-bytes ↦ ξ.ρ.input.read(ξ.size).read.ρ,
                written-bytes ↦ ξ.ρ.output.write(ξ.read-bytes).write.ρ,
                φ ↦ Φ̇.seq(
                  Φ̇.tuple(
                    Φ̇.tuple(Φ̇.tuple.empty, ξ.written-bytes),
                    ξ.ρ.ρ.input-block(
                      ξ.read-bytes, ξ.written-bytes, ξ.read-bytes.as-bytes
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
⟧}
```

## [org/eolang/malloc.phi](./org/eolang/malloc.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      malloc() ↦ ⟦
        empty(scope) ↦ ⟦
          φ ↦ ξ.ρ.ρ.malloc.of(0, ξ.scope)
        ⟧,
        for(object, scope) ↦ ⟦
          bts ↦ Φ̇.dataized(ξ.object).as-bytes,
          φ ↦ ξ.ρ.ρ.malloc.of(ξ.bts.size, ξ.auto-named-attr-at-96-9),
          auto-named-attr-at-96-9(m) ↦ ⟦
            φ ↦ Φ̇.seq(
              Φ̇.tuple(
                Φ̇.tuple(Φ̇.tuple.empty, ξ.m.write(0, ξ.ρ.bts)), ξ.ρ.scope(ξ.m)
              )
            )
          ⟧
        ⟧,
        of(size, scope) ↦ ⟦
          φ() ↦ ⟦
            λ ⤍ Lorg_eolang_malloc_of_φ
          ⟧,
          allocated(id) ↦ ⟦
            φ ↦ ξ.get,
            get ↦ ξ.read(0, ξ.size),
            size() ↦ ⟦
              λ ⤍ Lorg_eolang_malloc_of_allocated_size
            ⟧,
            resized(new-size) ↦ ⟦
              λ ⤍ Lorg_eolang_malloc_of_allocated_resized
            ⟧,
            copy(source, target, length) ↦ ⟦
              φ ↦ ξ.ρ.write(ξ.target, ξ.ρ.read(ξ.source, ξ.length))
            ⟧,
            read(offset, length) ↦ ⟦
              λ ⤍ Lorg_eolang_malloc_of_allocated_read
            ⟧,
            write(offset, data) ↦ ⟦
              λ ⤍ Lorg_eolang_malloc_of_allocated_write
            ⟧,
            put(object) ↦ ⟦
              φ ↦ Φ̇.seq(
                Φ̇.tuple(
                  Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.write(0, ξ.object)), ξ.ρ.get
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
⟧}
```

## [org/eolang/math/angle.phi](./org/eolang/math/angle.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      math() ↦ ⟦
        angle(value) ↦ ⟦
          φ ↦ ξ.value,
          in-degrees ↦ ξ.ρ.angle(ξ.ρ.times(180).div(Φ̇.math.pi)),
          in-radians ↦ ξ.ρ.angle(ξ.times(Φ̇.math.pi).div(180)),
          sin() ↦ ⟦
            λ ⤍ Lorg_eolang_math_angle_sin
          ⟧,
          cos() ↦ ⟦
            λ ⤍ Lorg_eolang_math_angle_cos
          ⟧,
          tan() ↦ ⟦
            cosine ↦ Φ̇.dataized(ξ.ρ.cos).as-bytes,
            φ ↦ ξ.cosine.eq(0).if(Φ̇.nan, ξ.ρ.sin.div(ξ.cosine))
          ⟧,
          ctan() ↦ ⟦
            sine ↦ Φ̇.dataized(ξ.ρ.sin).as-bytes,
            φ ↦ ξ.sine.eq(0).if(Φ̇.nan, ξ.ρ.cos.div(ξ.sine))
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

## [org/eolang/math/e.phi](./org/eolang/math/e.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      math() ↦ ⟦
        e ↦ 2.718281828459045,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/math/integral.phi](./org/eolang/math/integral.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      math() ↦ ⟦
        integral(fun, a, b, n) ↦ ⟦
          subsection(a, b) ↦ ⟦
            φ ↦ ξ.b.minus(ξ.a).div(6).times(
              ξ.ρ.fun(ξ.a).plus(
                4.times(ξ.ρ.fun(0.5.times(ξ.a.plus(ξ.b)))).plus(ξ.ρ.fun(ξ.b))
              )
            )
          ⟧,
          φ ↦ Φ̇.malloc.of(8, ξ.auto-named-attr-at-50-11).as-number,
          auto-named-attr-at-50-11(sum) ↦ ⟦
            φ ↦ Φ̇.malloc.for(ξ.ρ.a, ξ.auto-named-attr-at-53-16),
            auto-named-attr-at-53-16(left) ↦ ⟦
              right ↦ ξ.ρ.ρ.b,
              step ↦ ξ.right.minus(ξ.left).div(ξ.ρ.ρ.n).as-number,
              φ ↦ Φ̇.seq(
                Φ̇.tuple(
                  Φ̇.tuple(
                    Φ̇.tuple.empty,
                    Φ̇.while(
                      ξ.auto-named-attr-at-58-19,
                      ⟦
                        i ↦ ∅,
                        φ ↦ Φ̇.true
                      ⟧
                    )
                  ),
                  ξ.ρ.sum
                )
              ),
              auto-named-attr-at-58-19(i) ↦ ⟦
                φ ↦ ξ.ρ.left.as-number.plus(ξ.ρ.step).lt(ξ.ρ.right).if(
                  Φ̇.seq(
                    Φ̇.tuple(
                      Φ̇.tuple(
                        Φ̇.tuple(
                          Φ̇.tuple.empty,
                          ξ.ρ.ρ.sum.put(
                            ξ.ρ.ρ.sum.as-number.plus(
                              ξ.ρ.ρ.ρ.subsection(
                                ξ.ρ.left.as-number, ξ.ρ.left.as-number.plus(ξ.ρ.step)
                              )
                            )
                          )
                        ),
                        ξ.ρ.left.put(ξ.ρ.left.as-number.plus(ξ.ρ.step))
                      ),
                      Φ̇.true
                    )
                  ),
                  Φ̇.false
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
⟧}
```

## [org/eolang/math/numbers.phi](./org/eolang/math/numbers.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      math() ↦ ⟦
        numbers(sequence) ↦ ⟦
          φ ↦ ξ.sequence,
          max() ↦ ⟦
            lst ↦ Φ̇.structs.list(ξ.ρ.sequence),
            φ ↦ ξ.lst.is-empty.if(
              Φ̇.error("Can't get max number from empty sequence"),
              ξ.lst.reduced(
                Φ̇.negative-infinity,
                ⟦
                  max ↦ ∅,
                  item ↦ ∅,
                  φ ↦ ξ.item.as-number.gt(ξ.max).if(ξ.item, ξ.max)
                ⟧
              )
            )
          ⟧,
          min() ↦ ⟦
            lst ↦ Φ̇.structs.list(ξ.ρ.sequence),
            φ ↦ ξ.lst.is-empty.if(
              Φ̇.error("Can't get min number from empty sequence"),
              ξ.lst.reduced(
                Φ̇.positive-infinity,
                ⟦
                  min ↦ ∅,
                  item ↦ ∅,
                  φ ↦ ξ.min.gt(ξ.item.as-number).if(ξ.item, ξ.min)
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
⟧}
```

## [org/eolang/math/pi.phi](./org/eolang/math/pi.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      math() ↦ ⟦
        pi ↦ 3.141592653589793,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/math/random.phi](./org/eolang/math/random.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      math() ↦ ⟦
        random(seed) ↦ ⟦
          fixed ↦ ξ,
          φ ↦ ξ.seed.as-number.div(
            Φ̇.bytes(⟦ Δ ⤍ 00-20-00-00-00-00-00-00 ⟧).as-i64.as-number
          ),
          next ↦ ξ.ρ.random(
            ξ.seed.times(25214903917).plus(11).as-i64.and(
              Φ̇.bytes(⟦ Δ ⤍ 00-0F-FF-FF-FF-FF-FF-FF ⟧)
            ).as-i64.as-number
          ).fixed,
          pseudo() ↦ ⟦
            const-1 ↦ 35,
            const-2 ↦ 53,
            const-3 ↦ 17,
            one ↦ Φ̇.bytes(⟦ Δ ⤍ 00-00-00-00-00-00-00-01 ⟧),
            φ ↦ ξ.ρ.ρ.random(ξ.time-seed),
            time-seed ↦ ξ.time-bytes.left(ξ.const-1).and(
              ξ.one.left(ξ.const-2).as-i64.minus(ξ.one).as-bytes
            ).as-i64.plus(
              ξ.time-bytes.left(ξ.const-3).and(
                ξ.one.left(ξ.const-1).as-i64.minus(ξ.one).as-bytes
              ).as-i64.plus(
                ξ.time-bytes.and(
                  ξ.one.left(ξ.const-3).as-i64.minus(ξ.one).as-bytes
                ).as-i64
              )
            ).as-number,
            time-bytes ↦ Φ̇.sys.os.is-windows.if(
              Φ̇.sys.win32(
                "GetSystemTime", Φ̇.tuple(Φ̇.tuple.empty, Φ̇.sys.win32.system-time)
              ).milliseconds,
              ⟦
                timeval ↦ Φ̇.sys.posix(
                  "gettimeofday", Φ̇.tuple(Φ̇.tuple.empty, Φ̇.sys.posix.timeval)
                ).output,
                φ ↦ ξ.timeval.tv-sec.times(1000).plus(
                  ξ.timeval.tv-usec.as-i64.div(1000.as-i64).as-number
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
⟧}
```

## [org/eolang/math/real.phi](./org/eolang/math/real.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      math() ↦ ⟦
        real(num) ↦ ⟦
          φ ↦ ξ.num,
          exp ↦ Φ̇.math.real(Φ̇.math.e).pow(ξ.num),
          mod(x) ↦ ⟦
            dividend ↦ Φ̇.number(ξ.ρ.num.as-bytes),
            divisor ↦ Φ̇.number(ξ.x.as-bytes),
            φ ↦ ξ.divisor.eq(0).if(
              Φ̇.error("Can't calculate mod by zero"),
              ξ.dividend.gt(0).if(ξ.abs-mod, ξ.abs-mod.neg)
            ),
            abs-mod() ↦ ⟦
              dividend-abs ↦ Φ̇.math.real(ξ.ρ.dividend).abs,
              divisor-abs ↦ Φ̇.math.real(ξ.ρ.divisor).abs,
              φ ↦ ξ.dividend-abs.minus(
                ξ.divisor-abs.times(ξ.dividend-abs.div(ξ.divisor-abs).floor)
              )
            ⟧
          ⟧,
          abs() ↦ ⟦
            value ↦ Φ̇.number(ξ.ρ.num.as-bytes),
            φ ↦ ξ.value.gte(0).if(ξ.value, ξ.value.neg)
          ⟧,
          pow(x) ↦ ⟦
            λ ⤍ Lorg_eolang_math_real_pow
          ⟧,
          sqrt() ↦ ⟦
            λ ⤍ Lorg_eolang_math_real_sqrt
          ⟧,
          ln() ↦ ⟦
            λ ⤍ Lorg_eolang_math_real_ln
          ⟧,
          acos() ↦ ⟦
            λ ⤍ Lorg_eolang_math_real_acos
          ⟧,
          asin() ↦ ⟦
            λ ⤍ Lorg_eolang_math_real_asin
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

## [org/eolang/net/socket.phi](./org/eolang/net/socket.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      net() ↦ ⟦
        socket(address, port) ↦ ⟦
          φ ↦ Φ̇.sys.os.is-windows.if(
            ξ.win-socket(ξ.address, ξ.port), ξ.posix-socket(ξ.address, ξ.port)
          ),
          htons(port) ↦ ⟦
            bts ↦ ξ.port.as-i16.as-bytes,
            φ ↦ ξ.bts.and(Φ̇.bytes(⟦ Δ ⤍ FF- ⟧)).left(8).or(
              ξ.bts.right(8).and(Φ̇.bytes(⟦ Δ ⤍ FF- ⟧))
            ).as-i16
          ⟧,
          as-input(recv) ↦ ⟦
            read(size) ↦ ⟦
              φ ↦ ξ.input-block(Φ̇.bytes(⟦ Δ ⤍ -- ⟧)).read(ξ.size).self,
              input-block(buffer) ↦ ⟦
                self ↦ ξ,
                φ ↦ ξ.buffer,
                read(size) ↦ ⟦
                  read-bytes ↦ ξ.ρ.ρ.ρ.recv(ξ.size).as-bytes,
                  φ ↦ Φ̇.seq(
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.read-bytes), ξ.ρ.ρ.input-block(ξ.read-bytes)
                    )
                  ).self
                ⟧
              ⟧
            ⟧
          ⟧,
          as-output(send) ↦ ⟦
            write(buffer) ↦ ⟦
              φ ↦ ξ.output-block.write(ξ.buffer).self,
              output-block() ↦ ⟦
                self ↦ ξ,
                φ ↦ Φ̇.true,
                write(buffer) ↦ ⟦
                  φ ↦ Φ̇.seq(
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.ρ.ρ.send(ξ.buffer)), ξ.ρ.ρ.output-block
                    )
                  ).self
                ⟧
              ⟧
            ⟧
          ⟧,
          posix-socket(address, port) ↦ ⟦
            sd ↦ Φ̇.sys.posix(
              "socket",
              Φ̇.tuple(
                Φ̇.tuple(
                  Φ̇.tuple(Φ̇.tuple.empty, Φ̇.sys.posix.af-inet), Φ̇.sys.posix.sock-stream
                ),
                Φ̇.sys.posix.ipproto-tcp
              )
            ).code,
            inet-addr ↦ Φ̇.sys.posix(
              "inet_addr", Φ̇.tuple(Φ̇.tuple.empty, ξ.address)
            ).code,
            inet-addr-as-int ↦ ξ.inet-addr.eq(Φ̇.sys.posix.inaddr-none).if(
              Φ̇.error(
                Φ̇.txt.sprintf(
                  "Couldn't convert an IPv4 address '%s' into a 32-bit integer via 'inet_addr' posix syscall, reason: '%s'",
                  Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.address), ξ.strerror.code)
                )
              ),
              ξ.inet-addr.as-i32
            ),
            sockaddr ↦ Φ̇.sys.posix.sockaddr-in(
              Φ̇.sys.posix.af-inet.as-i16, ξ.ρ.htons(ξ.port), ξ.inet-addr-as-int
            ),
            scoped-socket(sockfd) ↦ ⟦
              as-input ↦ ξ.ρ.ρ.ρ.as-input(ξ.recv),
              as-output ↦ ξ.ρ.ρ.ρ.as-output(ξ.send),
              send(buffer) ↦ ⟦
                buff ↦ Φ̇.dataized(ξ.buffer).as-bytes,
                sent ↦ Φ̇.sys.posix(
                  "send",
                  Φ̇.tuple(
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.sockfd), ξ.buff), ξ.buff.size
                    ),
                    0
                  )
                ).code,
                φ ↦ ξ.sent.eq(-1).if(
                  Φ̇.error(
                    Φ̇.txt.sprintf(
                      "Failed to send message through the socket '%d', reason: %s",
                      Φ̇.tuple(
                        Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.sockfd), ξ.ρ.ρ.strerror.code
                      )
                    )
                  ),
                  ξ.sent
                )
              ⟧,
              recv(size) ↦ ⟦
                received ↦ Φ̇.sys.posix(
                  "recv",
                  Φ̇.tuple(
                    Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.sockfd), ξ.size), 0
                  )
                ).called,
                φ ↦ ξ.received.code.eq(-1).if(
                  Φ̇.error(
                    Φ̇.txt.sprintf(
                      "Failed to receive data from the socket '%d', reason: %s",
                      Φ̇.tuple(
                        Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.sockfd), ξ.ρ.ρ.strerror.code
                      )
                    )
                  ),
                  ξ.received.output
                )
              ⟧
            ⟧,
            strerror() ↦ ⟦
              φ ↦ Φ̇.sys.posix(
                "strerror",
                Φ̇.tuple(
                  Φ̇.tuple.empty, Φ̇.sys.posix("errno", Φ̇.tuple.empty).code
                )
              )
            ⟧,
            closed-socket(sockfd) ↦ ⟦
              closed ↦ Φ̇.sys.posix("close", Φ̇.tuple(Φ̇.tuple.empty, ξ.sockfd)).code,
              φ ↦ ξ.closed.eq(-1).if(
                Φ̇.error(
                  Φ̇.txt.sprintf(
                    "Couldn't close a posix socket '%d', reason: '%s'",
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.sockfd), ξ.ρ.strerror.code
                    )
                  )
                ),
                Φ̇.true
              )
            ⟧,
            safe-socket(scope) ↦ ⟦
              φ ↦ ξ.ρ.sd.eq(-1).if(
                Φ̇.error(
                  Φ̇.txt.sprintf(
                    "Couldn't create a posix socket, reason: '%s'",
                    Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.strerror.code)
                  )
                ),
                Φ̇.try(
                  ξ.scope,
                  ⟦
                    ex ↦ ∅,
                    φ ↦ Φ̇.error(ξ.ex)
                  ⟧,
                  ξ.ρ.closed-socket(ξ.ρ.sd)
                )
              )
            ⟧,
            connect(scope) ↦ ⟦
              φ ↦ ξ.ρ.safe-socket(ξ.auto-named-attr-at-274-10),
              auto-named-attr-at-274-10() ↦ ⟦
                sock ↦ ξ.ρ.ρ,
                connected ↦ Φ̇.sys.posix(
                  "connect",
                  Φ̇.tuple(
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), ξ.sock.sockaddr
                    ),
                    ξ.sock.sockaddr.size
                  )
                ).code,
                φ ↦ ξ.connected.eq(-1).if(
                  Φ̇.error(
                    Φ̇.txt.sprintf(
                      "Couldn't connect to '%s:%d' on posix socket '%d', reason: '%s'",
                      Φ̇.tuple(
                        Φ̇.tuple(
                          Φ̇.tuple(
                            Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.address), ξ.sock.port
                          ),
                          ξ.sock.sd
                        ),
                        ξ.sock.strerror.code
                      )
                    )
                  ),
                  Φ̇.dataized(ξ.ρ.scope(ξ.sock.scoped-socket(ξ.sock.sd))).as-bytes
                )
              ⟧
            ⟧,
            listen(scope) ↦ ⟦
              φ ↦ ξ.ρ.safe-socket(ξ.auto-named-attr-at-298-10),
              auto-named-attr-at-298-10() ↦ ⟦
                sock ↦ ξ.ρ.ρ,
                bound ↦ Φ̇.sys.posix(
                  "bind",
                  Φ̇.tuple(
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), ξ.sock.sockaddr
                    ),
                    ξ.sock.sockaddr.size
                  )
                ).code,
                listened ↦ Φ̇.sys.posix(
                  "listen", Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), 2048)
                ).code,
                φ ↦ ξ.bound.eq(-1).if(
                  Φ̇.error(
                    Φ̇.txt.sprintf(
                      "Couldn't bind posix socket '%d' to '%s:%d', reason: '%s'",
                      Φ̇.tuple(
                        Φ̇.tuple(
                          Φ̇.tuple(
                            Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), ξ.sock.address
                          ),
                          ξ.sock.port
                        ),
                        ξ.sock.strerror.code
                      )
                    )
                  ),
                  ξ.listened.eq(-1).if(
                    Φ̇.error(
                      Φ̇.txt.sprintf(
                        "Failed to listen for connections to '%s:%d' on socket '%d', reason: '%s'",
                        Φ̇.tuple(
                          Φ̇.tuple(
                            Φ̇.tuple(
                              Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.address), ξ.sock.port
                            ),
                            ξ.sock.sd
                          ),
                          ξ.sock.strerror.code
                        )
                      )
                    ),
                    Φ̇.dataized(ξ.ρ.scope(ξ.auto-named-attr-at-323-22)).as-bytes
                  )
                ),
                auto-named-attr-at-323-22() ↦ ⟦
                  φ ↦ ξ.ρ.sock.scoped-socket(ξ.ρ.sock.sd),
                  accept(scope) ↦ ⟦
                    sock ↦ ξ.ρ.ρ.sock,
                    client-sockfd ↦ Φ̇.sys.posix(
                      "accept",
                      Φ̇.tuple(
                        Φ̇.tuple(
                          Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), ξ.sock.sockaddr
                        ),
                        ξ.sock.sockaddr.size
                      )
                    ).code,
                    φ ↦ Φ̇.try(
                      ξ.client-sockfd.eq(-1).if(
                        Φ̇.error(
                          Φ̇.txt.sprintf(
                            "Failed to accept a connection on posix socket '%d', reason: %s",
                            Φ̇.tuple(
                              Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), ξ.sock.strerror.code
                            )
                          )
                        ),
                        Φ̇.dataized(
                          ξ.scope(ξ.sock.scoped-socket(ξ.client-sockfd))
                        ).as-bytes
                      ),
                      ⟦
                        ex ↦ ∅,
                        φ ↦ Φ̇.error(ξ.ex)
                      ⟧,
                      ξ.sock.closed-socket(ξ.client-sockfd)
                    )
                  ⟧
                ⟧
              ⟧
            ⟧
          ⟧,
          win-socket(address, port) ↦ ⟦
            sd ↦ Φ̇.sys.win32(
              "socket",
              Φ̇.tuple(
                Φ̇.tuple(
                  Φ̇.tuple(Φ̇.tuple.empty, Φ̇.sys.win32.af-inet), Φ̇.sys.win32.sock-stream
                ),
                Φ̇.sys.win32.ipproto-tcp
              )
            ).code,
            inet-addr ↦ Φ̇.sys.win32(
              "inet_addr", Φ̇.tuple(Φ̇.tuple.empty, ξ.address)
            ).code,
            inet-addr-as-int ↦ ξ.inet-addr.eq(Φ̇.sys.win32.inaddr-none).if(
              Φ̇.error(
                Φ̇.txt.sprintf(
                  "Couldn't convert an IPv4 address '%s' into a 32-bit integer via 'inet_addr' win32 function call, WSA error code: %d",
                  Φ̇.tuple(
                    Φ̇.tuple(Φ̇.tuple.empty, ξ.address), ξ.last-error.code
                  )
                )
              ),
              ξ.inet-addr.as-i32
            ),
            sockaddr ↦ Φ̇.sys.win32.sockaddr-in(
              Φ̇.sys.win32.af-inet.as-i16, ξ.ρ.htons(ξ.port), ξ.inet-addr-as-int
            ),
            scoped-socket(sockfd) ↦ ⟦
              as-input ↦ ξ.ρ.ρ.ρ.as-input(ξ.recv),
              as-output ↦ ξ.ρ.ρ.ρ.as-output(ξ.send),
              send(buffer) ↦ ⟦
                buff ↦ Φ̇.dataized(ξ.buffer).as-bytes,
                sent ↦ Φ̇.sys.win32(
                  "send",
                  Φ̇.tuple(
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.sockfd), ξ.buff), ξ.buff.size
                    ),
                    0
                  )
                ).code,
                φ ↦ ξ.sent.eq(-1).if(
                  Φ̇.error(
                    Φ̇.txt.sprintf(
                      "Failed to send message through the socket '%d', WSA error code: %d",
                      Φ̇.tuple(
                        Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.sockfd), ξ.ρ.ρ.last-error.code
                      )
                    )
                  ),
                  ξ.sent
                )
              ⟧,
              recv(size) ↦ ⟦
                received ↦ Φ̇.sys.win32(
                  "recv",
                  Φ̇.tuple(
                    Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.sockfd), ξ.size), 0
                  )
                ).called,
                φ ↦ ξ.received.code.eq(-1).if(
                  Φ̇.error(
                    Φ̇.txt.sprintf(
                      "Failed to receive data from the socket '%d', WSA error code: %d",
                      Φ̇.tuple(
                        Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.sockfd), ξ.ρ.ρ.last-error.code
                      )
                    )
                  ),
                  ξ.received.output
                )
              ⟧
            ⟧,
            last-error() ↦ ⟦
              φ ↦ Φ̇.sys.win32("WSAGetLastError", Φ̇.tuple.empty)
            ⟧,
            closed-socket(sockfd) ↦ ⟦
              closed ↦ Φ̇.sys.win32(
                "closesocket", Φ̇.tuple(Φ̇.tuple.empty, ξ.sockfd)
              ).code,
              φ ↦ ξ.closed.eq(Φ̇.sys.win32.socket-error).if(
                Φ̇.error(
                  Φ̇.txt.sprintf(
                    "Couldn't close a win32 socket '%d', WSA error code: %d",
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.sockfd), ξ.ρ.last-error.code
                    )
                  )
                ),
                Φ̇.true
              )
            ⟧,
            safe-socket(scope) ↦ ⟦
              started-up ↦ Φ̇.sys.win32(
                "WSAStartup", Φ̇.tuple(Φ̇.tuple.empty, Φ̇.sys.win32.winsock-version-2-2)
              ).code,
              cleaned-up ↦ Φ̇.sys.win32("WSACleanup", Φ̇.tuple.empty).code,
              φ ↦ ξ.started-up.eq(0).not.if(
                Φ̇.error(
                  Φ̇.txt.sprintf(
                    "Couldn't initialize Winsock via 'WSAStartup' call, WSA error code: %d",
                    Φ̇.tuple(Φ̇.tuple.empty, ξ.started-up)
                  )
                ),
                Φ̇.try(
                  ξ.ρ.sd.eq(-1).if(
                    Φ̇.error(
                      Φ̇.txt.sprintf(
                        "Couldn't create a win32 socket, WSA error code: %d",
                        Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.last-error.code)
                      )
                    ),
                    Φ̇.try(
                      ξ.scope,
                      ⟦
                        ex ↦ ∅,
                        φ ↦ Φ̇.error(ξ.ex)
                      ⟧,
                      ξ.ρ.closed-socket(ξ.ρ.sd)
                    )
                  ),
                  ⟦
                    ex ↦ ∅,
                    φ ↦ Φ̇.error(ξ.ex)
                  ⟧,
                  ξ.cleaned-up.eq(Φ̇.sys.win32.socket-error).if(
                    Φ̇.error("Couldn't cleanup Winsock resources"), Φ̇.true
                  )
                )
              )
            ⟧,
            connect(scope) ↦ ⟦
              φ ↦ ξ.ρ.safe-socket(ξ.auto-named-attr-at-483-10),
              auto-named-attr-at-483-10() ↦ ⟦
                sock ↦ ξ.ρ.ρ,
                connected ↦ Φ̇.sys.win32(
                  "connect",
                  Φ̇.tuple(
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), ξ.sock.sockaddr
                    ),
                    ξ.sock.sockaddr.size
                  )
                ).code,
                φ ↦ ξ.connected.eq(-1).if(
                  Φ̇.error(
                    Φ̇.txt.sprintf(
                      "Couldn't connect to '%s:%d' on win32 socket '%d', WSA error code: %d",
                      Φ̇.tuple(
                        Φ̇.tuple(
                          Φ̇.tuple(
                            Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.address), ξ.sock.port
                          ),
                          ξ.sock.sd
                        ),
                        ξ.sock.last-error.code
                      )
                    )
                  ),
                  Φ̇.dataized(ξ.ρ.scope(ξ.sock.scoped-socket(ξ.sock.sd))).as-bytes
                )
              ⟧
            ⟧,
            listen(scope) ↦ ⟦
              φ ↦ ξ.ρ.safe-socket(ξ.auto-named-attr-at-507-10),
              auto-named-attr-at-507-10() ↦ ⟦
                sock ↦ ξ.ρ.ρ,
                bound ↦ Φ̇.sys.win32(
                  "bind",
                  Φ̇.tuple(
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), ξ.sock.sockaddr
                    ),
                    ξ.sock.sockaddr.size
                  )
                ).code,
                listened ↦ Φ̇.sys.win32(
                  "listen", Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), 2048)
                ).code,
                φ ↦ ξ.bound.eq(-1).if(
                  Φ̇.error(
                    Φ̇.txt.sprintf(
                      "Couldn't bind win32 socket '%d' to '%s:%d', WSA error code: %d",
                      Φ̇.tuple(
                        Φ̇.tuple(
                          Φ̇.tuple(
                            Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), ξ.sock.address
                          ),
                          ξ.sock.port
                        ),
                        ξ.sock.last-error.code
                      )
                    )
                  ),
                  ξ.listened.eq(-1).if(
                    Φ̇.error(
                      Φ̇.txt.sprintf(
                        "Failed to listen for connections to '%s:%d' on socket '%d', WSA error code: %d",
                        Φ̇.tuple(
                          Φ̇.tuple(
                            Φ̇.tuple(
                              Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.address), ξ.sock.port
                            ),
                            ξ.sock.sd
                          ),
                          ξ.sock.last-error.code
                        )
                      )
                    ),
                    Φ̇.dataized(ξ.ρ.scope(ξ.auto-named-attr-at-532-22)).as-bytes
                  )
                ),
                auto-named-attr-at-532-22() ↦ ⟦
                  φ ↦ ξ.ρ.sock.scoped-socket(ξ.ρ.sock.sd),
                  accept(scope) ↦ ⟦
                    sock ↦ ξ.ρ.ρ.sock,
                    client-sockfd ↦ Φ̇.sys.win32(
                      "accept",
                      Φ̇.tuple(
                        Φ̇.tuple(
                          Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), ξ.sock.sockaddr
                        ),
                        ξ.sock.sockaddr.size
                      )
                    ).code,
                    φ ↦ Φ̇.try(
                      ξ.client-sockfd.eq(-1).if(
                        Φ̇.error(
                          Φ̇.txt.sprintf(
                            "Failed to accept a connection on win32 socket '%d', WSA error code: %d",
                            Φ̇.tuple(
                              Φ̇.tuple(Φ̇.tuple.empty, ξ.sock.sd), ξ.sock.last-error.code
                            )
                          )
                        ),
                        Φ̇.dataized(
                          ξ.scope(ξ.sock.scoped-socket(ξ.client-sockfd))
                        ).as-bytes
                      ),
                      ⟦
                        ex ↦ ∅,
                        φ ↦ Φ̇.error(ξ.ex)
                      ⟧,
                      ξ.sock.closed-socket(ξ.client-sockfd)
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

## [org/eolang/structs/bytes-as-array.phi](./org/eolang/structs/bytes-as-array.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      structs() ↦ ⟦
        bytes-as-array(bts) ↦ ⟦
          bytes-size ↦ Φ̇.dataized(ξ.bts.size).as-bytes,
          φ ↦ ξ.slice-byte(Φ̇.tuple.empty, 0),
          slice-byte(tup, index) ↦ ⟦
            φ ↦ ξ.index.lt(ξ.ρ.bytes-size).if(
              ξ.ρ.slice-byte(
                ξ.tup.with(ξ.ρ.bts.slice(ξ.index, 1)), ξ.index.plus(1)
              ),
              ξ.tup
            )
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

## [org/eolang/structs/hash-code-of.phi](./org/eolang/structs/hash-code-of.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      structs() ↦ ⟦
        hash-code-of(input) ↦ ⟦
          input-as-bytes ↦ Φ̇.dataized(ξ.input.as-bytes).as-bytes,
          size ↦ Φ̇.dataized(ξ.input-as-bytes.size).as-bytes,
          magic-number ↦ 31.as-i64,
          φ ↦ ξ.rec-hash-code(0, 0),
          rec-hash-code(acc, index) ↦ ⟦
            φ ↦ ξ.index.eq(ξ.ρ.size).if(
              ξ.acc.as-number,
              ξ.ρ.rec-hash-code(
                ξ.ρ.magic-number.times(ξ.acc).plus(
                  Φ̇.bytes(⟦ Δ ⤍ 00-00-00-00-00-00-00 ⟧).concat(
                    ξ.ρ.input-as-bytes.slice(ξ.index, 1)
                  ).as-i64
                ),
                ξ.index.plus(1)
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
⟧}
```

## [org/eolang/structs/list.phi](./org/eolang/structs/list.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      structs() ↦ ⟦
        list(origin) ↦ ⟦
          φ ↦ ξ.origin,
          is-empty() ↦ ⟦
            φ ↦ 0.eq(ξ.ρ.origin.length)
          ⟧,
          with(x) ↦ ⟦
            φ ↦ ξ.ρ.ρ.list(ξ.ρ.origin.with(ξ.x))
          ⟧,
          withi(index, item) ↦ ⟦
            φ ↦ ξ.ρ.head(ξ.index).with(ξ.item).concat(
              ξ.ρ.tail(ξ.ρ.origin.length.minus(ξ.index))
            )
          ⟧,
          reducedi(start, func) ↦ ⟦
            origin-len ↦ Φ̇.dataized(ξ.ρ.origin.length).as-bytes,
            φ ↦ 0.eq(ξ.origin-len).if(ξ.start, ξ.rec-reduced(ξ.start, 0.as-bytes)),
            rec-reduced(accum, index) ↦ ⟦
              idx-as-number ↦ ξ.index.as-number,
              next-index ↦ Φ̇.dataized(1.plus(ξ.idx-as-number)).as-bytes,
              φ ↦ ξ.next-index.eq(ξ.ρ.origin-len).if(
                ξ.accumulated, ξ.ρ.rec-reduced(ξ.accumulated, ξ.next-index)
              ),
              accumulated ↦ ξ.ρ.func(
                ξ.accum, ξ.ρ.ρ.origin.at(ξ.idx-as-number), ξ.idx-as-number
              )
            ⟧
          ⟧,
          reduced(start, func) ↦ ⟦
            φ ↦ ξ.ρ.reducedi(ξ.start, ξ.auto-named-attr-at-83-42),
            auto-named-attr-at-83-42(accum, item, idx) ↦ ⟦
              φ ↦ ξ.ρ.func(ξ.accum, ξ.item)
            ⟧
          ⟧,
          mappedi(func) ↦ ⟦
            φ ↦ ξ.ρ.ρ.list(
              ξ.ρ.reducedi(Φ̇.tuple.empty, ξ.auto-named-attr-at-93-24)
            ),
            auto-named-attr-at-93-24(accum, item, idx) ↦ ⟦
              φ ↦ ξ.accum.with(ξ.ρ.func(ξ.item, ξ.idx))
            ⟧
          ⟧,
          mapped(func) ↦ ⟦
            φ ↦ ξ.ρ.mappedi(ξ.auto-named-attr-at-103-30),
            auto-named-attr-at-103-30(item, idx) ↦ ⟦
              φ ↦ ξ.ρ.func(ξ.item)
            ⟧
          ⟧,
          eachi(func) ↦ ⟦
            φ ↦ ξ.ρ.reducedi(Φ̇.true, ξ.auto-named-attr-at-113-22),
            auto-named-attr-at-113-22(acc, item, index) ↦ ⟦
              φ ↦ Φ̇.seq(
                Φ̇.tuple(
                  Φ̇.tuple(Φ̇.tuple.empty, ξ.acc), ξ.ρ.func(ξ.item, ξ.index)
                )
              )
            ⟧
          ⟧,
          each(func) ↦ ⟦
            φ ↦ ξ.ρ.eachi(ξ.auto-named-attr-at-123-32),
            auto-named-attr-at-123-32(item, index) ↦ ⟦
              φ ↦ ξ.ρ.func(ξ.item)
            ⟧
          ⟧,
          withouti(i) ↦ ⟦
            φ ↦ ξ.ρ.ρ.list(
              ξ.ρ.reducedi(Φ̇.tuple.empty, ξ.auto-named-attr-at-130-24)
            ),
            auto-named-attr-at-130-24(accum, item, idx) ↦ ⟦
              φ ↦ ξ.ρ.i.eq(ξ.idx).if(ξ.accum, ξ.accum.with(ξ.item))
            ⟧
          ⟧,
          without(element) ↦ ⟦
            φ ↦ ξ.ρ.ρ.list(
              ξ.ρ.reduced(Φ̇.tuple.empty, ξ.auto-named-attr-at-141-20)
            ),
            auto-named-attr-at-141-20(accum, item) ↦ ⟦
              φ ↦ ξ.ρ.element.eq(ξ.item).if(ξ.accum, ξ.accum.with(ξ.item))
            ⟧
          ⟧,
          eq(other) ↦ ⟦
            φ ↦ ξ.ρ.origin.length.eq(ξ.other.length).and(
              ξ.ρ.reducedi(Φ̇.true, ξ.auto-named-attr-at-156-24)
            ),
            auto-named-attr-at-156-24(accum, item, idx) ↦ ⟦
              φ ↦ ξ.accum.and(ξ.item.eq(ξ.ρ.other.at(ξ.idx)))
            ⟧
          ⟧,
          concat(passed) ↦ ⟦
            φ ↦ ξ.ρ.ρ.list(ξ.passed).reduced(
              ξ.ρ,
              ⟦
                accum ↦ ∅,
                item ↦ ∅,
                φ ↦ ξ.accum.with(ξ.item)
              ⟧
            )
          ⟧,
          index-of(wanted) ↦ ⟦
            φ ↦ ξ.ρ.reducedi(-1, ξ.auto-named-attr-at-176-24),
            auto-named-attr-at-176-24(accum, item, index) ↦ ⟦
              φ ↦ -1.eq(ξ.accum).and(ξ.item.eq(ξ.ρ.wanted)).if(ξ.index, ξ.accum)
            ⟧
          ⟧,
          last-index-of(wanted) ↦ ⟦
            φ ↦ ξ.ρ.reducedi(-1, ξ.auto-named-attr-at-189-24),
            auto-named-attr-at-189-24(accum, item, index) ↦ ⟦
              φ ↦ ξ.item.eq(ξ.ρ.wanted).if(ξ.index, ξ.accum)
            ⟧
          ⟧,
          contains(element) ↦ ⟦
            φ ↦ -1.eq(ξ.ρ.index-of(ξ.element)).not
          ⟧,
          sorted() ↦ ⟦
            φ ↦ ξ.ρ
          ⟧,
          filteredi(func) ↦ ⟦
            origin-length ↦ Φ̇.dataized(ξ.ρ.origin.length).as-bytes,
            φ ↦ ξ.ρ.ρ.list(ξ.rec-filtered(0.as-bytes, Φ̇.tuple.empty)),
            rec-filtered(idx-as-bytes, accum) ↦ ⟦
              original ↦ ξ.ρ.ρ.origin,
              index ↦ ξ.idx-as-bytes.as-number,
              item ↦ ξ.ρ.ρ.origin.at(ξ.index),
              φ ↦ ξ.idx-as-bytes.eq(ξ.ρ.origin-length).if(
                ξ.accum,
                ξ.ρ.rec-filtered(
                  1.plus(ξ.index).as-bytes,
                  ξ.ρ.func(ξ.item, ξ.index).if(ξ.accum.with(ξ.item), ξ.accum)
                )
              )
            ⟧
          ⟧,
          filtered(func) ↦ ⟦
            φ ↦ ξ.ρ.filteredi(ξ.auto-named-attr-at-238-32),
            auto-named-attr-at-238-32(item, index) ↦ ⟦
              φ ↦ ξ.ρ.func(ξ.item)
            ⟧
          ⟧,
          head(index) ↦ ⟦
            idx ↦ Φ̇.dataized(ξ.index).as-bytes,
            φ ↦ Φ̇.switch(
              Φ̇.tuple(
                Φ̇.tuple(
                  Φ̇.tuple(
                    Φ̇.tuple(
                      Φ̇.tuple.empty,
                      Φ̇.tuple(
                        Φ̇.tuple(Φ̇.tuple.empty, 0.eq(ξ.idx)), ξ.ρ.ρ.list(Φ̇.tuple.empty)
                      )
                    ),
                    Φ̇.tuple(
                      Φ̇.tuple(Φ̇.tuple.empty, 0.gt(ξ.idx)), ξ.ρ.tail(ξ.index.as-number.neg)
                    )
                  ),
                  Φ̇.tuple(
                    Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.origin.length.lte(ξ.idx)), ξ.ρ
                  )
                ),
                Φ̇.tuple(
                  Φ̇.tuple(Φ̇.tuple.empty, Φ̇.true),
                  ξ.ρ.ρ.list(
                    ξ.ρ.reducedi(Φ̇.tuple.empty, ξ.auto-named-attr-at-259-32)
                  )
                )
              )
            ),
            auto-named-attr-at-259-32(accum, item, index) ↦ ⟦
              φ ↦ ξ.index.gte(ξ.ρ.idx).if(ξ.accum, ξ.accum.with(ξ.item))
            ⟧
          ⟧,
          tail(index) ↦ ⟦
            idx ↦ Φ̇.dataized(ξ.index).as-bytes,
            start ↦ Φ̇.dataized(ξ.ρ.origin.length.minus(ξ.idx.as-number)).as-bytes,
            φ ↦ 0.gt(ξ.start).if(
              ξ.ρ,
              ξ.ρ.ρ.list(
                ξ.ρ.reducedi(Φ̇.tuple.empty, ξ.auto-named-attr-at-275-26)
              )
            ),
            auto-named-attr-at-275-26(accum, item, idx) ↦ ⟦
              φ ↦ ξ.idx.gte(ξ.ρ.start).if(ξ.accum.with(ξ.item), ξ.accum)
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

## [org/eolang/structs/map.phi](./org/eolang/structs/map.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      structs() ↦ ⟦
        map(pairs) ↦ ⟦
          φ ↦ ξ.auto-named-attr-at-37-6.initialized,
          entry(key, value) ↦ ⟦⟧,
          initialized(entries) ↦ ⟦
            initialized ↦ ξ,
            size ↦ ξ.entries.length,
            keys() ↦ ⟦
              φ ↦ Φ̇.structs.list(ξ.ρ.entries).mapped(
                ⟦
                  entry ↦ ∅,
                  φ ↦ ξ.entry.key
                ⟧
              )
            ⟧,
            values() ↦ ⟦
              φ ↦ Φ̇.structs.list(ξ.ρ.entries).mapped(
                ⟦
                  entry ↦ ∅,
                  φ ↦ ξ.entry.value
                ⟧
              )
            ⟧,
            has(key) ↦ ⟦
              φ ↦ ξ.ρ.found(ξ.key).exists
            ⟧,
            found(key) ↦ ⟦
              hash ↦ Φ̇.dataized(Φ̇.structs.hash-code-of(ξ.key)).as-bytes,
              φ ↦ ξ.ρ.size.eq(0).if(ξ.not-found, ξ.rec-key-search(ξ.not-found, 0)),
              rec-key-search(found, index) ↦ ⟦
                entry ↦ ξ.ρ.ρ.entries.at(ξ.index),
                φ ↦ ξ.found.exists.or(ξ.ρ.ρ.size.eq(ξ.index)).if(
                  ξ.found,
                  ξ.ρ.rec-key-search(
                    ξ.ρ.hash.eq(ξ.entry.hash).if(
                      ξ.auto-named-attr-at-125-54, ξ.found
                    ),
                    ξ.index.plus(1)
                  )
                ),
                auto-named-attr-at-125-54() ↦ ⟦
                  exists ↦ Φ̇.true,
                  get ↦ ξ.ρ.entry.value
                ⟧
              ⟧,
              not-found() ↦ ⟦
                exists ↦ Φ̇.false,
                get ↦ Φ̇.error(
                  Φ̇.txt.sprintf(
                    "Object by hash code %d from given key does not exists",
                    Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.hash)
                  )
                )
              ⟧
            ⟧,
            with(key, value) ↦ ⟦
              hash ↦ Φ̇.dataized(Φ̇.structs.hash-code-of(ξ.key)).as-bytes,
              φ ↦ ξ.ρ.ρ.ρ.map.initialized(
                Φ̇.structs.list(ξ.ρ.entries).filtered(
                  ξ.auto-named-attr-at-145-50
                ).origin.with(ξ.auto-named-attr-at-146-12)
              ),
              auto-named-attr-at-145-50(entry) ↦ ⟦
                φ ↦ ξ.ρ.hash.eq(ξ.entry.hash).not
              ⟧,
              auto-named-attr-at-146-12() ↦ ⟦
                key ↦ ξ.ρ.key,
                value ↦ ξ.ρ.value,
                hash ↦ ξ.ρ.hash
              ⟧
            ⟧,
            without(key) ↦ ⟦
              hash ↦ Φ̇.dataized(Φ̇.structs.hash-code-of(ξ.key)).as-bytes,
              φ ↦ ξ.ρ.ρ.ρ.map.initialized(
                Φ̇.structs.list(ξ.ρ.entries).filtered(
                  ξ.auto-named-attr-at-159-48
                ).origin
              ),
              auto-named-attr-at-159-48(entry) ↦ ⟦
                φ ↦ ξ.ρ.hash.eq(ξ.entry.hash).not
              ⟧
            ⟧
          ⟧,
          auto-named-attr-at-37-6() ↦ ⟦
            pairs-size ↦ Φ̇.dataized(ξ.ρ.pairs.length).as-bytes,
            φ ↦ ξ.ρ.initialized(
              ξ.pairs-size.eq(0).if(
                Φ̇.tuple.empty,
                ξ.rec-rebuild(Φ̇.tuple.empty, 0, Φ̇.structs.list(Φ̇.tuple.empty))
              )
            ),
            rec-rebuild(accum, index, hashes) ↦ ⟦
              entry ↦ ξ.ρ.ρ.pairs.at(ξ.index),
              hash ↦ Φ̇.dataized(Φ̇.structs.hash-code-of(ξ.entry.key)).as-bytes,
              φ ↦ ξ.ρ.pairs-size.eq(ξ.index).if(
                ξ.accum,
                ξ.ρ.rec-rebuild(
                  ξ.hashes.contains(ξ.hash).if(
                    ξ.accum, ξ.accum.with(ξ.auto-named-attr-at-59-18)
                  ),
                  ξ.index.plus(1),
                  ξ.hashes.with(ξ.hash)
                )
              ),
              auto-named-attr-at-59-18() ↦ ⟦
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
⟧}
```

## [org/eolang/structs/range-of-ints.phi](./org/eolang/structs/range-of-ints.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      structs() ↦ ⟦
        range-of-ints(start, end) ↦ ⟦
          φ ↦ 0.eq(ξ.start).or(1.eq(ξ.start.div(ξ.start))).and(
            0.eq(ξ.end).or(1.eq(ξ.end.div(ξ.end)))
          ).if(
            Φ̇.structs.range(ξ.auto-named-attr-at-42-8, ξ.end),
            Φ̇.error("Some of the arguments are not integers")
          ),
          auto-named-attr-at-42-8() ↦ ⟦
            build(num) ↦ ⟦
              φ ↦ ξ.num,
              next ↦ ξ.ρ.build(1.plus(ξ.φ))
            ⟧,
            φ ↦ ξ.build(ξ.ρ.start)
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

## [org/eolang/structs/range.phi](./org/eolang/structs/range.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      structs() ↦ ⟦
        range(start, end) ↦ ⟦
          φ ↦ Φ̇.structs.list(
            ξ.start.lt(ξ.end).if(
              ξ.appended(Φ̇.tuple(Φ̇.tuple.empty, ξ.start), ξ.start.next), Φ̇.tuple.empty
            )
          ),
          appended(acc, current) ↦ ⟦
            φ ↦ ξ.current.lt(ξ.ρ.end).if(
              ξ.ρ.appended(ξ.acc.with(ξ.current), ξ.current.next), ξ.acc
            )
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

## [org/eolang/structs/set.phi](./org/eolang/structs/set.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      structs() ↦ ⟦
        set(lst) ↦ ⟦
          φ ↦ ξ.initialized(
            Φ̇.structs.map(
              Φ̇.structs.list(ξ.lst).mapped(
                ⟦
                  item ↦ ∅,
                  φ ↦ Φ̇.structs.map.entry(ξ.item, Φ̇.true)
                ⟧
              ).origin
            )
          ).initialized,
          initialized(map) ↦ ⟦
            initialized ↦ ξ,
            φ ↦ ξ.map.keys,
            size ↦ ξ.map.size,
            with(item) ↦ ⟦
              φ ↦ ξ.ρ.ρ.ρ.set.initialized(ξ.ρ.map.with(ξ.item, Φ̇.true))
            ⟧,
            without(item) ↦ ⟦
              φ ↦ ξ.ρ.ρ.ρ.set.initialized(ξ.ρ.map.without(ξ.item))
            ⟧,
            has(item) ↦ ⟦
              φ ↦ ξ.ρ.map.has(ξ.item)
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

## [org/eolang/switch.phi](./org/eolang/switch.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      switch(cases) ↦ ⟦
        len ↦ Φ̇.dataized(ξ.cases.length).as-bytes,
        φ ↦ ξ.len.eq(0).if(Φ̇.error("switch cases are empty"), ξ.case-at(0)),
        case-at(index) ↦ ⟦
          case ↦ ξ.ρ.cases.at(ξ.index),
          φ ↦ ξ.index.eq(ξ.ρ.len).if(
            Φ̇.true, ξ.case.at(0).if(ξ.case.at(1), ξ.ρ.case-at(ξ.index.plus(1)))
          )
        ⟧
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/sys/getenv.phi](./org/eolang/sys/getenv.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      sys() ↦ ⟦
        getenv(name) ↦ ⟦
          φ ↦ Φ̇.sys.os.is-windows.if(
            Φ̇.sys.win32(
              "GetEnvironmentVariable", Φ̇.tuple(Φ̇.tuple(Φ̇.tuple.empty, ξ.name), 512)
            ),
            Φ̇.sys.posix("getenv", Φ̇.tuple(Φ̇.tuple.empty, ξ.name))
          ).output
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/sys/line-separator.phi](./org/eolang/sys/line-separator.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      sys() ↦ ⟦
        line-separator() ↦ ⟦
          φ ↦ Φ̇.string(Φ̇.sys.os.is-windows.if("\r\n", "\n").as-bytes)
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/sys/os.phi](./org/eolang/sys/os.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      sys() ↦ ⟦
        os() ↦ ⟦
          φ ↦ ξ.name,
          is-windows() ↦ ⟦
            os-name ↦ Φ̇.dataized(ξ.ρ.name).as-bytes,
            φ ↦ ξ.os-name.size.gt(6).and(ξ.os-name.slice(0, 7).eq("Windows"))
          ⟧,
          is-linux ↦ Φ̇.txt.regex("/linux/i").matches(ξ.name).as-bool,
          is-macos ↦ Φ̇.txt.regex("/mac/i").matches(ξ.name).as-bool,
          name() ↦ ⟦
            λ ⤍ Lorg_eolang_sys_os_name
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

## [org/eolang/sys/posix.phi](./org/eolang/sys/posix.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      sys() ↦ ⟦
        posix(name, args) ↦ ⟦
          stdin-fileno ↦ 0,
          stdout-fileno ↦ 1,
          af-inet ↦ 2,
          sock-stream ↦ 1,
          ipproto-tcp ↦ 6,
          inaddr-none ↦ -1,
          φ() ↦ ⟦
            λ ⤍ Lorg_eolang_sys_posix_φ
          ⟧,
          return(code, output) ↦ ⟦
            called ↦ ξ,
            φ ↦ ξ.output
          ⟧,
          timeval(tv-sec, tv-usec) ↦ ⟦
            self ↦ ξ
          ⟧,
          sockaddr-in(sin-family, sin-port, sin-addr) ↦ ⟦
            sin-zero ↦ Φ̇.bytes(⟦ Δ ⤍ 00-00-00-00-00-00-00-00 ⟧),
            size ↦ ξ.sin-family.size.plus(ξ.sin-port.size).plus(ξ.sin-addr.size).plus(
              ξ.sin-zero.size
            )
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

## [org/eolang/sys/win32.phi](./org/eolang/sys/win32.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      sys() ↦ ⟦
        win32(name, args) ↦ ⟦
          std-input-handle ↦ -10,
          std-output-handle ↦ -11,
          af-inet ↦ 2,
          sock-stream ↦ 1,
          ipproto-tcp ↦ 6,
          invalid-socket ↦ -1,
          socket-error ↦ -1,
          inaddr-none ↦ -1,
          winsock-version-2-2 ↦ Φ̇.bytes(⟦ Δ ⤍ 02-02 ⟧),
          φ() ↦ ⟦
            λ ⤍ Lorg_eolang_sys_win32_φ
          ⟧,
          return(code, output) ↦ ⟦
            called ↦ ξ,
            φ ↦ ξ.output
          ⟧,
          system-time(year, month, day, day-of-week, hour, minute, second, milliseconds) ↦ ⟦
            self ↦ ξ
          ⟧,
          sockaddr-in(sin-family, sin-port, sin-addr) ↦ ⟦
            sin-zero ↦ Φ̇.bytes(⟦ Δ ⤍ 00-00-00-00-00-00-00-00 ⟧),
            size ↦ ξ.sin-family.size.plus(ξ.sin-port.size).plus(ξ.sin-addr.size).plus(
              ξ.sin-zero.size
            )
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

## [org/eolang/txt/regex.phi](./org/eolang/txt/regex.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      txt() ↦ ⟦
        regex(expression) ↦ ⟦
          φ ↦ ξ.compiled,
          compiled() ↦ ⟦
            λ ⤍ Lorg_eolang_txt_regex_compiled
          ⟧,
          pattern(serialized) ↦ ⟦
            matches(txt) ↦ ⟦
              φ ↦ ξ.ρ.match(ξ.txt).next.exists
            ⟧,
            match(txt) ↦ ⟦
              next ↦ ξ.matched-from-index(1, 0).matched,
              matched-from-index(position, start) ↦ ⟦
                λ ⤍ Lorg_eolang_txt_regex_pattern_match_matched_from_index
              ⟧,
              matched(position, start, from, to, groups) ↦ ⟦
                matched ↦ ξ,
                groups-count ↦ ξ.groups.length,
                exists ↦ ξ.start.gte(0),
                next ↦ ξ.exists.if(
                  ξ.ρ.matched-from-index(ξ.position.plus(1), ξ.to).matched,
                  Φ̇.error("Matched block does not exist, can't get next")
                ),
                text ↦ ξ.exists.if(
                  ξ.group(0), Φ̇.error("Matched block does not exist, can't get text")
                ),
                group(index) ↦ ⟦
                  φ ↦ ξ.ρ.groups.at(ξ.index)
                ⟧
              ⟧,
              not-matched(position) ↦ ⟦
                φ ↦ ξ.ρ.matched(
                  ξ.position,
                  -1,
                  Φ̇.error(
                    "Matched block does not exist, can't get 'from' position"
                  ),
                  Φ̇.error(
                    "Matched block does not exist, can't get 'to' position"
                  ),
                  Φ̇.error("Matched block does not exist, can't get groups")
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

## [org/eolang/txt/sscanf.phi](./org/eolang/txt/sscanf.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      txt() ↦ ⟦
        sscanf(format, read) ↦ ⟦
          λ ⤍ Lorg_eolang_txt_sscanf
        ⟧,
        λ ⤍ Package
      ⟧,
      λ ⤍ Package
    ⟧,
    λ ⤍ Package
  ⟧
⟧}
```

## [org/eolang/txt/text.phi](./org/eolang/txt/text.phi)

```console
{⟦
  org() ↦ ⟦
    eolang() ↦ ⟦
      txt() ↦ ⟦
        text(origin) ↦ ⟦
          φ ↦ ξ.origin,
          is-alphanumeric ↦ Φ̇.txt.regex("/^[A-Za-z0-9]+$/").matches(ξ.origin),
          is-alpha ↦ Φ̇.txt.regex("/^[a-zA-Z]+$/").matches(ξ.origin),
          is-ascii ↦ Φ̇.txt.regex("/^[\\x00-\\x7F]*$/").matches(ξ.origin),
          slice(start, len) ↦ ⟦
            φ ↦ ξ.ρ.ρ.text(ξ.ρ.origin.slice(ξ.start, ξ.len))
          ⟧,
          trimmed-left() ↦ ⟦
            len ↦ Φ̇.dataized(ξ.ρ.origin.length).as-bytes,
            idx ↦ Φ̇.dataized(ξ.first-non-space-index(0)).as-bytes,
            φ ↦ 0.eq(ξ.len).if(
              ξ.ρ, ξ.ρ.slice(ξ.idx, Φ̇.number(ξ.len).minus(Φ̇.number(ξ.idx)))
            ),
            first-non-space-index(index) ↦ ⟦
              char ↦ Φ̇.dataized(ξ.ρ.ρ.origin.slice(ξ.index, 1)).as-bytes,
              φ ↦ ξ.ρ.len.eq(ξ.index).if(
                ξ.index,
                " ".eq(ξ.char).if(
                  ξ.ρ.first-non-space-index(ξ.index.plus(1)), ξ.index
                )
              )
            ⟧
          ⟧,
          trimmed-right() ↦ ⟦
            len ↦ Φ̇.dataized(ξ.ρ.origin.length).as-bytes,
            φ ↦ 0.eq(ξ.len).if(
              ξ.ρ, ξ.ρ.slice(0, ξ.first-non-space-index(Φ̇.number(ξ.len).plus(-1)))
            ),
            first-non-space-index(index) ↦ ⟦
              char ↦ Φ̇.dataized(ξ.ρ.ρ.origin.slice(ξ.index, 1)).as-bytes,
              φ ↦ -1.eq(ξ.index).if(
                0,
                " ".eq(ξ.char).if(
                  ξ.ρ.first-non-space-index(ξ.index.plus(-1)), ξ.index.plus(1)
                )
              )
            ⟧
          ⟧,
          trimmed() ↦ ⟦
            φ ↦ 0.eq(ξ.ρ.length).if(ξ.ρ, ξ.ρ.trimmed-left.trimmed-right)
          ⟧,
          joined(items) ↦ ⟦
            delimiter ↦ Φ̇.dataized(ξ.ρ.origin).as-bytes,
            first ↦ ξ.items.at(0),
            len ↦ Φ̇.dataized(ξ.items.length).as-bytes,
            not-empty ↦ Φ̇.dataized(
              1.eq(ξ.len).if(
                ξ.first, ξ.first.as-bytes.concat(ξ.with-delimiter("".as-bytes, 1))
              )
            ).as-bytes,
            φ ↦ ξ.ρ.ρ.text(0.eq(ξ.len).if("", Φ̇.string(ξ.not-empty))),
            with-delimiter(acc, index) ↦ ⟦
              φ ↦ ξ.ρ.len.eq(ξ.index).if(
                ξ.acc,
                ξ.ρ.with-delimiter(
                  ξ.acc.concat(ξ.ρ.delimiter.concat(ξ.ρ.items.at(ξ.index))), ξ.index.plus(1)
                )
              )
            ⟧
          ⟧,
          repeated(times) ↦ ⟦
            bts ↦ Φ̇.dataized(ξ.ρ.origin.as-bytes).as-bytes,
            amount ↦ Φ̇.dataized(ξ.times).as-bytes,
            φ ↦ 0.gt(ξ.amount).if(
              Φ̇.error(
                Φ̇.txt.sprintf(
                  "Can't repeat text %d times", Φ̇.tuple(Φ̇.tuple.empty, ξ.amount)
                )
              ),
              ξ.ρ.ρ.text(
                0.eq(ξ.amount).if("", Φ̇.string(ξ.rec-repeated(ξ.bts, 1)))
              )
            ),
            rec-repeated(accum, index) ↦ ⟦
              φ ↦ ξ.ρ.amount.eq(ξ.index).if(
                ξ.accum, ξ.ρ.rec-repeated(ξ.accum.concat(ξ.ρ.bts), ξ.index.plus(1))
              )
            ⟧
          ⟧,
          contains(substring) ↦ ⟦
            φ ↦ -1.eq(ξ.ρ.index-of(ξ.substring)).not
          ⟧,
          ends-with(substring) ↦ ⟦
            substr ↦ Φ̇.dataized(ξ.substring).as-bytes,
            φ ↦ ξ.ρ.index-of(ξ.substr).eq(ξ.ρ.length.minus(ξ.substr.size))
          ⟧,
          starts-with(substring) ↦ ⟦
            φ ↦ 0.eq(ξ.ρ.index-of(ξ.substring))
          ⟧,
          index-of(substring) ↦ ⟦
            self-len ↦ Φ̇.dataized(Φ̇.string(ξ.ρ.origin.as-bytes).length).as-bytes,
            substr ↦ Φ̇.string(ξ.substr-as-bytes),
            sub-len ↦ Φ̇.dataized(ξ.substr.length).as-bytes,
            end ↦ Φ̇.dataized(Φ̇.number(ξ.self-len).minus(Φ̇.number(ξ.sub-len))).as-bytes,
            φ ↦ Φ̇.number(ξ.sub-len).gt(ξ.self-len).or(
              ξ.sub-len.eq(ξ.self-len).and(ξ.substr.eq(ξ.ρ.origin).not)
            ).if(-1, ξ.rec-index-of-substr(0)),
            rec-index-of-substr(idx) ↦ ⟦
              φ ↦ ξ.ρ.end.eq(ξ.idx).if(
                ξ.contains.if(ξ.idx, -1),
                ξ.contains.if(ξ.idx, ξ.ρ.rec-index-of-substr(ξ.idx.plus(1)))
              ),
              contains ↦ ξ.ρ.substr.eq(ξ.ρ.ρ.slice(ξ.idx, ξ.ρ.sub-len))
            ⟧,
            substr-as-bytes ↦ Φ̇.dataized(ξ.substring).as-bytes
          ⟧,
          last-index-of(substring) ↦ ⟦
            self-len ↦ Φ̇.dataized(Φ̇.string(ξ.ρ.origin.as-bytes).length).as-bytes,
            substr ↦ Φ̇.string(ξ.substr-as-bytes),
            sub-len ↦ Φ̇.dataized(ξ.substr.length).as-bytes,
            φ ↦ Φ̇.number(ξ.sub-len).gt(ξ.self-len).or(
              ξ.sub-len.eq(ξ.self-len).and(ξ.substr.eq(ξ.ρ.origin).not)
            ).if(
              -1,
              ξ.rec-index-of-substr(
                Φ̇.number(ξ.self-len).minus(Φ̇.number(ξ.sub-len))
              )
            ),
            rec-index-of-substr(idx) ↦ ⟦
              φ ↦ 0.eq(ξ.idx).if(
                ξ.contains.if(ξ.idx, -1),
                ξ.contains.if(ξ.idx, ξ.ρ.rec-index-of-substr(ξ.idx.plus(-1)))
              ),
              contains ↦ ξ.ρ.substr.eq(ξ.ρ.ρ.slice(ξ.idx, ξ.ρ.sub-len))
            ⟧,
            substr-as-bytes ↦ Φ̇.dataized(ξ.substring).as-bytes
          ⟧,
          up-cased() ↦ ⟦
            ascii-z ↦ Φ̇.dataized(ξ.ascii("z")).as-bytes,
            ascii-a ↦ Φ̇.dataized(ξ.ascii("a")).as-bytes,
            distance ↦ Φ̇.number(ξ.ascii-a).minus(ξ.ascii("A")),
            φ ↦ ξ.ρ.ρ.text(
              Φ̇.string(
                Φ̇.structs.list(Φ̇.structs.bytes-as-array(ξ.ρ.origin.as-bytes)).reduced(
                  Φ̇.bytes(⟦ Δ ⤍ -- ⟧), ξ.auto-named-attr-at-258-22
                )
              )
            ),
            ascii(char) ↦ ⟦
              φ ↦ Φ̇.bytes(⟦ Δ ⤍ 00-00-00-00-00-00-00 ⟧).concat(ξ.char.as-bytes).as-i64.as-number
            ⟧,
            auto-named-attr-at-258-22(accum, byte) ↦ ⟦
              ascii-bte ↦ ξ.ρ.ascii(ξ.byte),
              φ ↦ ξ.accum.concat(
                ξ.ascii-bte.lte(ξ.ρ.ascii-z).and(ξ.ascii-bte.gte(ξ.ρ.ascii-a)).if(
                  ξ.ascii-bte.minus(ξ.ρ.distance).as-i64.as-bytes.slice(7, 1), ξ.byte
                )
              )
            ⟧
          ⟧,
          low-cased() ↦ ⟦
            ascii-z ↦ ξ.ρ.up-cased.ascii("Z"),
            ascii-a ↦ ξ.ρ.up-cased.ascii("A"),
            φ ↦ ξ.ρ.ρ.text(
              Φ̇.string(
                Φ̇.structs.list(Φ̇.structs.bytes-as-array(ξ.ρ.origin.as-bytes)).reduced(
                  Φ̇.bytes(⟦ Δ ⤍ -- ⟧), ξ.auto-named-attr-at-291-22
                )
              )
            ),
            auto-named-attr-at-291-22(accum, byte) ↦ ⟦
              ascii-bte ↦ ξ.ρ.ρ.up-cased.ascii(ξ.byte),
              φ ↦ ξ.accum.concat(
                ξ.ascii-bte.lte(ξ.ρ.ascii-z).and(ξ.ascii-bte.gte(ξ.ρ.ascii-a)).if(
                  ξ.ascii-bte.plus(ξ.ρ.ρ.up-cased.distance).as-i64.as-bytes.slice(
                    7, 1
                  ),
                  ξ.byte
                )
              )
            ⟧
          ⟧,
          at(i) ↦ ⟦
            len ↦ Φ̇.dataized(ξ.ρ.length).as-bytes,
            idx ↦ Φ̇.dataized(ξ.i).as-bytes,
            index ↦ Φ̇.dataized(
              0.gt(ξ.idx).if(Φ̇.number(ξ.len).plus(ξ.idx), ξ.idx)
            ).as-bytes,
            φ ↦ 0.gt(ξ.index).or(Φ̇.number(ξ.index).gte(ξ.len)).if(
              Φ̇.error(
                Φ̇.txt.sprintf(
                  "Given index %d is out of text bounds", Φ̇.tuple(Φ̇.tuple.empty, ξ.index)
                )
              ),
              ξ.ρ.slice(ξ.index, 1)
            )
          ⟧,
          replaced(target, replacement) ↦ ⟦
            self-as-bytes ↦ Φ̇.dataized(ξ.ρ.origin).as-bytes,
            reinit ↦ Φ̇.string(ξ.self-as-bytes),
            matched ↦ ξ.target.match(ξ.reinit).next,
            φ ↦ ξ.matched.exists.not.if(
              Φ̇.txt.text(ξ.reinit),
              Φ̇.txt.text(ξ.rec-replaced(ξ.matched, "", ξ.matched.start))
            ),
            rec-replaced(block, accum, start) ↦ ⟦
              φ ↦ ξ.block.exists.if(
                ξ.ρ.rec-replaced(
                  ξ.block.next,
                  ξ.accum.concat(
                    ξ.ρ.reinit.slice(ξ.start, ξ.block.from.minus(ξ.start))
                  ).concat(ξ.ρ.replacement),
                  ξ.block.to
                ),
                Φ̇.string(
                  ξ.accum.concat(
                    ξ.ρ.reinit.slice(ξ.start, ξ.ρ.reinit.length.minus(ξ.start))
                  )
                )
              )
            ⟧
          ⟧,
          as-number() ↦ ⟦
            scanned ↦ Φ̇.txt.sscanf("%f", ξ.ρ.origin),
            φ ↦ ξ.scanned.length.eq(0).if(
              Φ̇.error(
                Φ̇.txt.sprintf(
                  "Can't convert text %s to number", Φ̇.tuple(Φ̇.tuple.empty, ξ.ρ.origin)
                )
              ),
              ξ.scanned.tail
            )
          ⟧,
          split(delimiter) ↦ ⟦
            delim ↦ Φ̇.dataized(ξ.delimiter).as-bytes,
            self-as-bytes ↦ ξ.ρ.origin.as-bytes,
            len ↦ Φ̇.dataized(ξ.self-as-bytes.size).as-bytes,
            φ ↦ ξ.len.eq(0).if(Φ̇.tuple.empty, ξ.rec-split(Φ̇.tuple.empty, 0, 0)),
            rec-split(accum, start, current) ↦ ⟦
              φ ↦ ξ.ρ.len.eq(ξ.current).if(
                ξ.with-substr,
                ξ.ρ.delim.eq(ξ.ρ.self-as-bytes.slice(ξ.current, 1)).if(
                  ξ.ρ.rec-split(
                    ξ.with-substr, ξ.current.plus(1), ξ.current.plus(1)
                  ),
                  ξ.ρ.rec-split(ξ.accum, ξ.start, ξ.current.plus(1))
                )
              ),
              with-substr ↦ ξ.accum.with(
                Φ̇.string(
                  ξ.ρ.self-as-bytes.slice(ξ.start, ξ.current.minus(ξ.start))
                )
              )
            ⟧
          ⟧,
          chained(others) ↦ ⟦
            φ ↦ 0.eq(ξ.others.length).if(
              ξ.ρ,
              ξ.ρ.ρ.text(
                Φ̇.string(
                  Φ̇.structs.list(ξ.others).reduced(
                    ξ.ρ.origin.as-bytes,
                    ⟦
                      accum ↦ ∅,
                      str ↦ ∅,
                      φ ↦ ξ.accum.concat(ξ.str.as-bytes)
                    ⟧
                  )
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

