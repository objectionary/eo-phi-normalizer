# Patent (draft)

## Background

## Problem

Applying rewrite rules matching higher-level patterns in the code,
while supporting partial evaluation, including (possibly platform-specific?) primitives.

## Example

1. ?

## Existing Solutions

1. I find the approach similar to the design of abstract machines, like STG,
   which also embed primitives into the lower-level intermediate representation.
   The difference of our approach (as I see it) is that we combine (partial) evaluation with
   rewriting techniques and use it in a different phase of the compilation pipeline.

## Our Solution

### Introduction

### Evaluation of Example Program

## Distinguishing Features (Novelty)

1. Two-phase computation (normalize/rewrite + dataize/eval)
2.

## Detectability

??? Not sure what to use here, it seems we would benefit from _specific_ rewrite rules!

## Advantageous Effect

1. Separation of concerns, flexibility?
2. Easy support of platform-specific low-level primitives (atoms)?
3. Ability to rely on higher-level patterns in the code?

## Related Patents

## Related Papers

1. Conditional Rewriting modulo a Built-in Algebra <https://publikationen.sulb.uni-saarland.de/handle/20.500.11880/37693>
2. Term rewriting with built-in numbers and collection data structures <https://digitalrepository.unm.edu/cgi/viewcontent.cgi?article=1004&context=cs_etds>
3. Preserving confluence for rewrite systems with built-in operations <https://link.springer.com/chapter/10.1007/3-540-60381-6_5>
4.
