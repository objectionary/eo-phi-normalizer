# User-defined rules

## `MetaPHI`

You can define [rewrite rules](https://en.wikipedia.org/wiki/Rewriting#Term_rewriting_systems) for the `PHI` language (the $\varphi$-calculus language) using `YAML` and the `MetaPHI` language that is a superset of `PHI`.

See the `MetaPHI` [Labelled BNF](https://bnfc.readthedocs.io/en/latest/lbnf.html) in [Syntax.cf](https://github.com/objectionary/normalizer/blob/master/eo-phi-normalizer/grammar/EO/Phi/Syntax.cf).

## phi-paper rules

Currently, the `PHI` normalizer supports rules defined in an unpublished paper by Yegor Bugayenko.

![Rules](media/rules.jpg)

### yegor.yaml

These rules translated to `MetaPHI` are in [yegor.yaml](https://github.com/objectionary/normalizer/blob/master/eo-phi-normalizer/test/eo/phi/rules/yegor.yaml).

Each rule has the following structure:

- `name` - Rule name.
- `description` - Rule description.
- `context` - (optional) Rule context. A contex may contain:
  - `global-object` - (optional) Global object `MetaId`.
  - `current-object` - (optional) Current object `MetaId`.
- `pattern` - Term pattern.
  - When this term pattern matches a subterm of a `PHI` term, `MetaId`s from the term pattern become associated with matching subexpressions of that subterm.
- `result` - Substitution result.
  - `MetaId`s in the subterm pattern get replaced by their associated subexpressions.
- `when` - A list of conditions for pattern matching.
  - `nf` - A list of `MetaId`s associated with subexpressions that shoud be in [normal form](#normal-form).
  - `present_attrs` - A list of attributes that must be present in subexpression bindings.
    - `attrs` - A list of attributes. Can include `MetaId`s.
    - `bindings` - A list of bindings that must contain these attributes.
  - `absent_attrs` - A list of attributes that must not be present in subexpression bindings.
    - `attrs` - A list of attributes. Can include `MetaId`s.
    - `bindings` - A list of bindings that must not contain these attributes.
- `tests` - A list of unit tests for this rule.
  - `name` - Test name.
  - `input` - An initial `PHI` term.
  - `output` - The initial `PHI` term after this rule was applied.
  - `matches` - Whether the term pattern should match any subterm.

### Normal form

<!-- TODO #119:30m  -->

## Use rules

{{#include ./common/normalize-phi-options.md}}

### Sample program

{{#include ./common/sample-program.md}}

### Prepare environment

The commands in the following sections access files that are available in the project repository.
Clone and enter the repository directory.

```sh
git clone https://github.com/objectionary/normalizer
cd normalizer
```

#### `--ruleset-yaml`

Normalize a 𝜑-expression from `program.phi` using the [yegor.yaml](#yegoryaml) rules.

There can be multiple numbered results that correspond to multiple rule application sequences.

```sh
normalize-phi --rules-yaml ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml program.phi
```

```console
Rule set based on Yegor's draft
Input:
{ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b (c ↦ ⟦ ⟧).d ⟧.e }
====================================================
Result 1 out of 1:
⟦ a ↦ ξ.b (c ↦ ⟦ ⟧).d (ρ ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧ ⟧) ⟧
----------------------------------------------------
```

#### `--chain`

Use `--chain` to see numbered normalization steps for each normalization result.

```sh
normalize-phi --chain --rules-yaml ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml program.phi
```

```console
Rule set based on Yegor's draft
Input:
{ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b (c ↦ ⟦ ⟧).d ⟧.e }
====================================================
Result 1 out of 1:
[ 1 / 2 ]⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b (c ↦ ⟦ ⟧).d ⟧.e ⟧
[ 2 / 2 ]⟦ a ↦ ξ.b (c ↦ ⟦ ⟧).d (ρ ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧ ⟧) ⟧
----------------------------------------------------
```

#### `--single`

Use `--single` to print a single normalized program.

```sh
normalize-phi --single --rules-yaml ./eo-phi-normalizer/test/eo/phi/rules/yegor.yaml program.phi
```

```console
⟦ a ↦ ξ.b (c ↦ ⟦ ⟧).d (ρ ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧ ⟧) ⟧
```
