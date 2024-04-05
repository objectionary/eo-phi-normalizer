# [Title]

[N.B: Replace text in square brackets with your text]

| author             | date-accepted                         | pr-url                                          | implemented                                              |
| ------------------ | ------------------------------------- | ----------------------------------------------- | -------------------------------------------------------- |
| [Full author name] | [Date when the proposal was accepted] | [URL of a PR where this proposal was discussed] | [normalizer version where this proposal was implemented] |

## Motivation

[
Give a strong reason for why you need this change.
Describe the use case as clearly as possible and give an example.
Explain how the status quo is insufficient or not ideal.

A good Motivation section is often driven by examples and real-world scenarios.
]

## Proposed Change Specification

[
Specify the change in precise, comprehensive yet concise language.
Avoid words like "should" or "could".
Strive for a complete definition.
Your specification may include,

- LBNF grammar and semantics of any new syntactic constructs (see [LBNF reference](https://bnfc.readthedocs.io/en/latest/lbnf.html)).
- how the proposed change interacts with existing normalizer features, in case that is otherwise ambiguous

Strive for *precision*.

Think about corner cases. Write down
general rules and invariants.

Note, however, that this section should focus on a precise
*specification*; it need not (and should not) devote space to
*implementation* details -- the "Implementation Plan" section can be used for that.

The specification can, and almost always should, be illustrated with
*examples* that illustrate corner cases. But it is not sufficient to
give a couple of examples and regard that as the specification! The
examples should illustrate and elucidate a clearly-articulated
specification that covers the general case.
]

```yaml
rules:
  - pattern: |
      FunctionDot. Function ::= "$dot" "(" <prefix :: (Expression)> "," <suffix :: (Expression)> ")"
    description: |
      Return the dispatch <prefix>.<suffix>
    examples:
      - applications:
          - application: $dot(ξ, $dot(ρ, $dot($name(?b_a), write(α0 ↦ ?t))))
            # Note the result isn't a value because the result contains MetaId-s
            result: ξ.ρ.?b_a.write(α0 ↦ ?t)
  - pattern: |
      FunctionName. Function ::= "$name" "(" <binding :: (MetaId)> ")"
    description: |
      Return the name of <binding>.
    examples:
      - pattern: ⟦ \?Name ?a ↦ ⟦ ?A ⟧ ⟧
        input: ⟦ name ↦ ⟦ a ↦ Φ.a.c, b ↦ Φ.a.c ⟧ ⟧
        matches:
          - metavar: "?Name"
            value: name ↦ ⟦ a ↦ Φ.a.c, b ↦ Φ.a.c ⟧
          - metavar: "?a"
            value: name
        applications:
          - application: $name(?Name)
            value: name
  - pattern: |
      FunctionNames. Function ::= "$names" "(" <bindings :: (MetaId | Function)> ")"
    description: |
      Return the names of <bindings>.
    examples:
      - pattern: "⟦ ?B ⟧"
        input: "⟦ a ↦ Φ.b, c ↦ Φ.c ⟧"
        matches:
          - metavar: "?B"
            value: a ↦ Φ.b, c ↦ Φ.c
        applications:
          - application: $names(?B)
            value: a, c
  - pattern: |
      FunctionReplace. Function ::= "$replace" "(" <e1 :: (MetaId | Expression)> "," <e2 :: (MetaId | Expression)> "," <e3 :: (MetaId | Expression)> ")"
    description: |
      Return <e1> with all occurencies of <e2> replaced with <e3>.
    examples:
      - pattern: ⟦ a ↦ ?B ⟧
        input: ⟦ a ↦ ⟦ a ↦ Φ.a, b ↦ Φ.b, c ↦ Φ.c ⟧ ⟧
        matches:
          - metavar: "?B"
            value: ⟦ a ↦ Φ.a, b ↦ Φ.b, c ↦ Φ.c ⟧
        application: $replace(?B, Φ.b, Φ.baz)
  # FIXME what's the semantics ???
  - pattern: |
      FunctionCombine. Function ::= "$combine" "(" <b1 :: (MetaId)> "," <b2 :: (MetaId)> ")"
    # FIXME order of elements depends on order of bindings in code?
    description: |
      Return new bindings obtained by combining <b1> and <b2>.

      ?B1: [a1 -> x, a2 -> y, a3 -> z]
      ?B2: [b1 -> c, b2 -> d, b3 -> e]
      $combine(?B1, ?B2) => [a1 -> c, a2 -> d, a3 -> e]
    Inspiration 1: array_combine - https://www.php.net/manual/ru/function.array-combine.php
    Inspiration 2: func foo1(int x, string y)
      func foo2(bool z, char a)

      foo1(1, “s”)
      foo2(true, ‘x’)

      func foo_combo(int x, string y, bool z, char a)

      foo_combo(1, “s”, true, ‘x’)
    examples:
      - pattern: ⟦ a ↦ ⟦ ?A ⟧, b ↦ ⟦ ?B ⟧ ⟧
        input: ⟦ a ↦ ⟦ a ↦ Φ.a, b ↦ Φ.b, c ↦ Φ.c ⟧, b ↦ ⟦ d ↦ Φ.d, e ↦ Φ.e ⟧ ⟧
        matches:
          - metavar: "?A"
            value: a ↦ Φ.a, b ↦ Φ.b, c ↦ Φ.c
          - metavar: "?B"
            value: a ↦ Φ.a, d ↦ Φ.d, e ↦ Φ.e
        applications:
          - application: $combine(?A, ?B)
            value: a ↦ Φ.d, b ↦ Φ.e
  # FIXME what's the semantics ???
  - pattern: |
      FunctionUnique. Function ::= "$unique" "(" <bindings :: (MetaId)> ")"
    description: |
      Replace all names in <bindings> with unique names.
    examples:
      - $unique(?B)
  - pattern: |
      FunctionPrefixed. Function ::= $prefixed "(" <name1 :: (Expression)> "," <name2 :: (Expression)> ")"
predicates:
  - pattern: 'PredicateForAll. Predicate ::= "$for-all" "(" <bindings :: (MetaId)> "," <arg :: (MetaId)> "," <predicate :: (Predicate)> ")"'
    description: |
      Check that the <predicate> is TRUE for all <bindings>.
      You can use <arg> inside the <predicate>.
    examples: See below
  - pattern: 'PredicateBoundTo. Predicate ::= "$bound-to" "(" <name :: (MetaId)> "," <value :: (Expression)> ")"'
    description: |
      Check that <name> is bound to <value>.
    examples:
      - pattern: "⟦ ?name ↦ Φ.?a.b, ?B ⟧"
        input: ⟦ name ↦ Φ.a.b, a ↦ Φ.a.c, b ↦ Φ.a.c ⟧
        matches:
          - metavar: "?name"
            value: name
          - metavar: "?a"
            value: a
          - metavar: "?B"
            value: a ↦ Φ.a.c, b ↦ Φ.a.c
        where:
          - condition: $bound-to(?name, ∅)
            value: "FALSE"
          - condition: $for-all(?B, ?binding, $bound-to(?binding, Φ.a.c))
            value: "TRUE"
  - pattern: 'PredicateStartsWith. Predicate ::= "$starts-with" "(" <name :: (Expression)> "," <prefix :: (String)> ")"'
    description: |
      Check that <name> starts with <prefix>.
    examples:
      - pattern: '⟦ \?Binding Φ.?a.b, ?Bindings ⟧'
        input: "⟦ name ↦ ⟦ k ↦ Φ.a.b ⟧, a_foo ↦ Φ.a.c, a_bar ↦ Φ.a.c ⟧"
        matches:
          - metavar: "?Binding"
            value: name ↦ ⟦ k ↦ Φ.a.b ⟧
          - metavar: "?Bindings"
            value: a_foo ↦ Φ.a.c, a_bar ↦ Φ.a.c
        where:
          - condition: $for-all(?Bindings, ?binding, $starts-with($name(?binding), "a_"))
            value: TRUE
  - pattern: 'PredicateNot. Predicate ::= "$not" "(" <predicate :: (Predicate)> ")"'
    description: |
      Negate the <predicate>.
    examples:
      - pattern: "⟦ ?name ↦ Φ.?a.b, ?B ⟧"
        input: ⟦ name ↦ Φ.foo.b ⟧
        matches:
          - metavar: "?name"
            value: name
          - metavar: "?a"
            value: foo
          - metavar: "?B"
            value: []
        where:
          - condition: $not($starts-with(?a), "foo")
            value: FALSE
          - condition: $not($bound-to(?name, Φ))
            value: TRUE
```

## Examples

[
This section illustrates the specification through the use of examples of the
language change proposed. It is best to exemplify each point made in the
specification, though perhaps one example can cover several points. Contrived
examples are OK here. If the Motivation section describes something that is
hard to do without this proposal, this is a good place to show how easy that
thing is to do with the proposal.
]

## Effect and Interactions

[
Your proposed change addresses the issues raised in the
motivation. Explain how.

Also, discuss possibly contentious interactions with existing language or normalizer
features. Complete this section with potential interactions raised
during the PR discussion.
]

## Alternatives

[
List alternative designs to your proposed change. Both existing
workarounds, or alternative choices for the changes. Explain
the reasons for choosing the proposed change over these alternative:
*e.g.* they can be cheaper but insufficient, or better but too
expensive. Or something else.

The PR discussion often raises other potential designs, and they should be
added to this section. Similarly, if the proposed change
specification changes significantly, the old one should be listed in
this section.
]

## Unresolved Questions

[
Explicitly list any remaining issues that remain in the conceptual design and
specification. Please do not list *implementation* issues.
]

## Implementation Plan

[
(Optional) If accepted who will implement the change?
Which other resources and prerequisites are required for implementation?
]
