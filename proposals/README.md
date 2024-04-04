# Proposal

A Proposal is a document describing a proposed change to the normalizer.

## Proposal format

- Template for proposals should resemble the [ghc-proposals template](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0000-template.md).
- Proposal file names should consist of an index, a title, and the `.md` extension
  - Example: `001-pattern-matching-on-paths.md`.
- PR names should follow the format `[proposal] proposal_title`
  - Example: `[proposal] Pattern matching on paths`.

## Proposal process

- (No label.) The author drafts a proposal.
- (No label.) The author submits the proposal for discussion, as a pull request against this repository.
- Label: [proposal under review](https://github.com/objectionary/normalizer/labels/proposal%20under%20review). The normalizer team discusses the proposal in the commit section of the pull request, while the author refines the proposal. This phase lasts as long as necessary.
- Eventually, the normalizer team rejects a proposal (label: [proposal rejected](https://github.com/objectionary/normalizer/labels/proposal%20rejected)), or passes it back to the author for review (label: [proposal needs revision](https://github.com/objectionary/normalizer/labels/Proposal%20needs%20revision)), or accepts it (label: [proposal  accepted](https://github.com/objectionary/normalizer/labels/proposal%20accepted)).
- Label: [proposal implemented](https://github.com/objectionary/normalizer/labels/proposal%20implemented). Once a proposal is accepted, it still has to be implemented. The author may do that, or someone else. We mark the proposal as “implemented” once it hits normalizer master branch (and we are happy to be nudged to do so by email, GitHub issue, or a comment on the relevant pull request) and the corresponding documentation on the site is updated.
