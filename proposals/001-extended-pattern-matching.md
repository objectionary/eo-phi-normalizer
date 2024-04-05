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
# Approach:
# - Try to express patterns in terms of BNFC (https://bnfc.digitalgrammars.com/) (with slight modifications)
# - Use <label :: (Type)> expressions to label non-terminals

patterns:
  - comment: |
      MetaBinding combines three syntaxes:

      (1) \?B1 ?x.?y... ↦ ?a.b ===> Binding B1 that matches with ?x.?y... ↦ ?a.b, can be used futher as ?B1

      (2) ?bar => \?b_bar ξ.ρ.?b_a.?foo ===>
        in the current formation,
        find a binding with name ?bar where body contains somewhere ξ.ρ.?b_a.?foo,
        and save its whole body to variable ?b_bar

      (3) ?p1..?main ↦ ...     ===> some path to ?main
      (3.1) if used in formation - means enclosure, for ex: ⟦ org ↦ ⟦ eolang ↦ ⟦ main ↦ ... ⟧ ⟧ ⟧
      (3.2) if used in application - means dot notation, for ex (x ↦ org.eolang.main)

      I believe (3) cannot be used without a right side (after ↦) so that I know when to stop searching.

      I think (1) should include (3) as follows.

      (1.1) \?A ?a.?b..c.?d..?e.f ↦ ⟦ ?B ⟧
      (1.2) \?C ?g..h.?i ↦ ?j.k(D).?l..?m.n

      In other words, MetaBinding should allow:
      - optionally saving the full binding to a given metavariable (?A, ?C)
      - matching individual path elements (?a, c, ?e, f, h, ?i)
      - matching a segment of the path (?b.., ?d.., ?g..)
      - matching an object on the right side (⟦ ?C ⟧, ?j.k(D).?l..?m.n)
      - matching a segment of a path within a sequence of dispatches ?l..

      Then
      - (2) is expressible via (1) and $name
      - (3) is expressible via (1) without a metavariable for the full binding
    pattern: |
      MetaBinding. Binding ::= ("\\" <binding :: (MetaId)>)? ((<pathElement :: (Attribute)> ".")* (<segment :: (MetaId)> "..")? (<name :: (Attribute)>) ".")* (<finalName :: (Attribute)>) "↦" <object :: (Object)>
    description: |
      Find a <binding> that has a path to a value after the "↦" that matches a given <object>.
      When <binding> is given, the <binding> equals to the full binding.

      The path can include:
      - individual <pathElement>-s
      - <segment>-s of unknown length

      The path must include:
      - the <finalName> of the last binding
      - the <object> to match after the "↦"
    examples:
      - pattern: '⟦ \?FullBinding ?Path..?Name ↦ ?Object.world(?Bindings) ⟧'
        input: "⟦ org ↦ ⟦ eolang ↦ ⟦ main ↦ hello.world(and ↦ you) ⟧ ⟧ ⟧"
        matches:
          - metavar: "?Path"
            value: ["org", "eolang"]
          - metavar: "?Name"
            value: "main"
          - metavar: "?Object"
            value: "hello"
          - metavar: "?Bindings"
            value: [and ↦ you]
          - metavar: "?PathHead"
            value: ["org"]
          - metavar: "?FullBinding"
            value: [org ↦ ⟦ eolang ↦ ⟦ main ↦ hello.world(and ↦ you) ⟧ ⟧]
        applications:
          - application: $name(?FullBinding)
            value: "org"
        constructions:
          - construction: "⟦ ?FullBinding ⟧"
            value: "⟦ org ↦ ⟦ eolang ↦ ⟦ main ↦ hello.world(and ↦ you) ⟧ ⟧ ⟧"
      - pattern: '(\?FullBinding ?Name ↦ ?DispatchPath..?DispatchName.world)'
        input: "(x ↦ Φ.org.eolang.world, y ↦ Φ.org.eolang.work)"
        matches:
          - metavar: "?FullBinding"
            value: "x ↦ Φ.org.eolang.world"
          - metavar: "?Name"
            value: "x"
          - metavar: "?DispatchPath"
            value: ["Φ", "org"]
          - metavar: "?DispatchName"
            value: "eolang"
        constructions:
          - construction: "⟦ ?FullBinding ⟧"
            value: "⟦ x ↦ Φ.org.eolang.world ⟧"
  - pattern: |
      ExpressionObject. Expression ::= Object
    examples:
      - ξ
      - ⟦ a ↦ ?B ⟧
  - pattern: |
      ExpressionFunction. Expression ::= Function
    examples:
      - $name(?a)
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
