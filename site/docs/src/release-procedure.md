# Release procedure

## Prerequisites

1. [Create an account](https://hackage.haskell.org/accounts) on [Hackage](https://hackage.haskell.org/).
1. Ask one of the [package maintainers](https://hackage.haskell.org/package/eo-phi-normalizer/maintainers/) to make you a new package maintainer.

## In the [repository](https://github.com/objectionary/eo-phi-normalizer)

1. Draft a [new release](https://github.com/objectionary/eo-phi-normalizer/releases/new).
1. Create a new tag `v<version>`. The `<version>` must conform to [SemVer](https://semver.org/).
1. Click `Generate release notes`.
1. Create an issue named `Release v<version>` ([example](https://github.com/objectionary/eo-phi-normalizer/issues/526)).
1. List there in the `## Subtasks` section what needs to be done for the release.
1. On the issue page, find `Development` and click `Create a branch ...`.
1. Checkout this branch locally using `git`.
1. Open `CHANGELOG.md`
1. Add a new section at the top of the file before sections for previous versions.
1. In the section title, specify the version (`v<version>`) and the release date.
1. Copy the generated release notes into that section.
1. Format the notes similar to the sections that go below this one.
1. Commit the changes with a message `feat(changelog): add notes for v<version>` (replace `<version>` with the actual version).
1. In `eo-phi-normalizer/package.yaml` and `eo-phi-normalizer/eo-phi-normalizer.cabal`, update the `version`.
1. Commit the changes with a message `chore(eo-phi-normalizer): bump version <previous-version> <version>` (replace the `<previous-version>` and `<version>` with the actual versions).
1. Push changes.
1. Wait until CI succeeds (see [Actions](https://github.com/objectionary/eo-phi-normalizer/actions)).
1. Merge changes into `master`.
1. Wait until CI succeeds (see [Actions](https://github.com/objectionary/eo-phi-normalizer/actions)).
1. Return to the page where you drafted the release.
1. Copy the text from the section in `CHANGELOG.md` into the release description.
1. Select `Set as the latest release`.
1. Click `Publish release`.

## On [Hackage](https://hackage.haskell.org/)

1. If the release was successful, go to the [package page](https://hackage.haskell.org/package/eo-phi-normalizer).
1. Find there `Candidates`.
1. Click on the relevant release candidate.
1. Publish it.
