# Changelog for eo atoms' signature

## v0.38.0
    - change `σ` to `ρ`

    - `bytes.phi`
        - simplifies `as-bytes` attribute
    - `go.phi`
        - add `auto-named-attr-at-64-9`
    - `malloc.phi`
        - add `auto-named-attr-at-64-9`
    - `negative-infinity.phi`
        - add auto-named-attr-at-64-9 to `is-num-gte-zero` attribute
    - `positive-infinity.phi`
        - add auto-named-attr-at-64-9 to `is-num-gte-zero` and `is-num-gt-zero` attributes
    - `while.phi`
        - remove `start` atrribute

## v0.37.0
    - `bool.phi` file in v0.36.0 change into two separate files `false.phi` and `true.phi`
    - `bytes.phi`
        - add delta
        - change `x` param name in `eq` attribute into `b`
    - `goto.phi`, `heap.phi`, `if.phi`, `memory.phi`, `nop.phi`, and `ram.phi` files are deleted
    - `malloc.phi`
        - `size` and `pointer` attributes removed
        - `for` and `of` attributes added
        - `memory-block-pointer` is renamed to `allocated` and added to `of` as attribute
        - `free` in `memory-block-pointer` is removed
        - `get` and `put` added in `allocated`


## v0.36.0
    Initial version
