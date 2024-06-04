# Deliverables

## Stage 1

1. A software (“Normalizer”) ([link](https://github.com/objectionary/normalizer) to the GitHub repository).
1. A translator from XMIR to φ-calculus expressions:
    - The translator was implemented by the team of Yegor Bugayenko ([link](https://github.com/objectionary/eo/blob/c4fe24fc2359957261b1e56b8cac9113d0b77db2/eo-maven-plugin/src/main/java/org/eolang/maven/PhiMojo.java) to the implementation).
1. Tests using existing EOLANG code examples:
    - We used existing EOLANG examples ([link](https://github.com/objectionary/eo/tree/c4fe24fc2359957261b1e56b8cac9113d0b77db2/eo-runtime/src/test/eo/org/eolang) to existing test files).
    - We implemented the full pipeline that tests the Normalizer on existing EOLANG code examples ([link](https://github.com/objectionary/normalizer/blob/bbe60bcbcaea7332515485da0d4e5e31fc1e163d/.github/workflows/ghc.yml#L103-L308) to the pipeline configuration).
    - We explained how the pipeline works ([link](https://www.objectionary.com/normalizer/docs/pipeline.html) to the pipeline docs).
    - We run the pipeline on each push to the repository and generate reports for each run ([link](https://www.objectionary.com/normalizer/report/) to the report for the last pipeline run).
1. Draft of an academic paper explaining the details of normalization (a link will be provided later).
