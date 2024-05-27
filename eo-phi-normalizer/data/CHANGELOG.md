# Changelog for $\varphi$ standard library

Only changes in objects' interfaces (not implementation) are documented

## v0.38.0
### Modified
* simplify `as-bytes` attribute in `bytes.phi`

### Added
* `auto-named-attr-at-64-9` to:
    * `go.phi`
    * `malloc.phi`
    * `negative-infinity.phi` to `is-num-gte-zero` attribute
    * `positive-infinity.phi` to `is-num-gte-zero` and `is-num-gt-zero` attributes

### Removed
* `start` attribute in `while.phi`


## v0.37.0

### Modified
* `bytes.phi`
    * add `Δ` as a void attribute
    * change `x` param name in `eq` attribute into `b`
    * replace with it's eo `φ` equivalent
        * `λ ⤍ Lorg_eolang_bytes_as_string` in `as-string` attribute
        * `λ ⤍ Lorg_eolang_bytes_as_int` in `as-int` attribute
        * `λ ⤍ Lorg_eolang_bytes_as_float` in `as-float` attribute

* `malloc.phi`
    * `size` and `pointer` attributes removed
    * `for` and `of` attributes added
    * `φ` is moved into `of`
    * `free` and `pointer` in `memory-block-pointer` is removed
    * `memory-block-pointer` is renamed to `allocated` and moved inside `of`
    * `φ`, `get`, and `put` added in `allocated` (previously `memory-block-pointer`)

### Added
* `false.phi`
* `true.phi`

### Removed
* `bool.phi`
* `goto.phi`
* `heap.phi`
* `if.phi`
* `memory.phi`
* `nop.phi`
* `ram.phi`



## v0.36.0
### Initial version
