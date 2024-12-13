# Normalizer for 𝜑-calculus

[![`eo-phi-normalizer` on Hackage](https://img.shields.io/hackage/v/eo-phi-normalizer)](http://hackage.haskell.org/package/eo-phi-normalizer)
[![Haddock](<https://shields.io/badge/Haddock%20(master)-Code%20documentation-informational>)](https://www.objectionary.com/eo-phi-normalizer/haddock/)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE.txt)
[![Hits-of-Code](https://hitsofcode.com/github/objectionary/eo-phi-normalizer?branch=master&label=Hits-of-Code)](https://hitsofcode.com/github/objectionary/eo-phi-normalizer/view?branch=master&label=Hits-of-Code)
![Lines of code](https://sloc.xyz/github/objectionary/eo-phi-normalizer)

This command line tool helps you deal with 𝜑-calculus expressions, usually
produced by the [EO compiler][eo].

First, install it (you should install [Stack][stack] first):

```bash
stack update
stack install eo-phi-normalizer
```

Then, normalize a simple 𝜑-expression:

```bash
$ cat > foo.phi
{⟦ m ↦ ⟦ x ↦ ⟦ ρ ↦ ∅ ⟧.ρ.k, k ↦ ⟦ Δ ⤍ 42- ⟧ ⟧.x ⟧}
$ eo-phi-normalizer rewrite --chain --tex foo.phi
```

The output will contain a ready-to-use LaTeX document, where all
rewritting steps are explained.

More detailed documentation is [here][site].

## How to use custom rules?

By default, the rules of normalization of 𝜑-calculus are used. They are
defined in the [rules.yaml][rules] file. You can use your own rules, with the
help of our custom YAML format, for example in `forty-three.yml`:

```yaml
title: "forty-three"
rules:
  - name: forty-three
    description: 'change 33 double to 42 double'
    pattern: |
      Φ.org.eolang.bytes ( α0 ↦ ⟦ Δ ⤍ 40-40-80-00-00-00-00-00 ⟧ )
    result: |
      Φ.org.eolang.bytes ( α0 ↦ ⟦ Δ ⤍ 40-45-00-00-00-00-00-00 ⟧ )
    tests: [ ]
```

Then, use this file:

```bash
eo-phi-normalizer rewrite --rules=forty-three.yml foo.phi
```

[site]: https://www.objectionary.com/eo-phi-normalizer/
[stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[eo]: https://github.com/objectionary/eo
[rules]: https://github.com/objectionary/eo-phi-normalizer/blob/master/eo-phi-normalizer/test/eo/phi/rules/new.yaml
