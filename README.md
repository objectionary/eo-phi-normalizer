# Deliverables

## Stage 1

1. A software (“Normalizer”) ([link](https://github.com/objectionary/eo-phi-normalizer) to the GitHub repository).
1. A translator from XMIR to φ-calculus expressions:
    - The translator was implemented by the team of Yegor Bugayenko ([link](https://github.com/objectionary/eo/blob/c4fe24fc2359957261b1e56b8cac9113d0b77db2/eo-maven-plugin/src/main/java/org/eolang/maven/PhiMojo.java) to the implementation).
1. Tests using existing EOLANG code examples:
    - We used existing EOLANG examples ([link](https://github.com/objectionary/eo/tree/c4fe24fc2359957261b1e56b8cac9113d0b77db2/eo-runtime/src/test/eo/org/eolang) to existing test files).
    - We implemented the full pipeline that tests the Normalizer on existing EOLANG code examples ([link](https://github.com/objectionary/eo-phi-normalizer/blob/bbe60bcbcaea7332515485da0d4e5e31fc1e163d/.github/workflows/ghc.yml#L103-L308) to the pipeline configuration).
    - We explained how the pipeline works ([link](https://www.objectionary.com/eo-phi-normalizer/docs/pipeline.html) to the pipeline docs).
    - We run the pipeline on each push to the repository and generate reports for each run ([link](https://www.objectionary.com/eo-phi-normalizer/report/) to the report for the last pipeline run).
1. Draft of an academic paper explaining the details of normalization ([link](./papers/phi-confluence.pdf) to the paper).

## Stage 2

1. A final version of the academic paper ([link](./papers/phi-confluence.pdf) to the paper).
    - The paper was submitted to ITP 2024, but was rejected ([link](./papers/phi-confluence-ITP-2024-reviews.txt) to the reviews).
1. Experimental results of its application.
    - The [normalizer](https://github.com/objectionary/eo-phi-normalizer) tool was built assuming confluence proved in the paper.
    - We generate reports for each pipeline run ([link](https://www.objectionary.com/eo-phi-normalizer/report/) to the report for the last pipeline run).
1. A proof of confluence of φ-calculus.
    - A proof of confluence of φ-calculus is implemented in the [proof](https://github.com/objectionary/proof) repository.
    - The proof documentation is available [here](https://www.objectionary.com/proof/docs/).

## Stage 3

1. A software (“Normalizer”) ([link](https://github.com/objectionary/eo-phi-normalizer) to the GitHub repository).
1. Tests using existing EOLANG code examples:
    - We used existing EOLANG examples ([link](https://github.com/objectionary/eo/tree/4f291e6d4c28c2aacd8c01c0c025f488eeaf2053/eo-runtime/src/test/eo/org/eolang) to existing test files).
    - We implemented the full pipeline that tests the Normalizer on existing EOLANG code examples ([link](https://github.com/objectionary/eo-phi-normalizer/blob/bbe60bcbcaea7332515485da0d4e5e31fc1e163d/.github/workflows/ghc.yml#L103-L308) to the pipeline configuration).
    - We explained how the pipeline works ([link](https://www.objectionary.com/eo-phi-normalizer/docs/pipeline.html) to the pipeline docs).
    - We run the pipeline on each push to the repository and generate reports for each run ([link](https://www.objectionary.com/eo-phi-normalizer/report/) to the report for the last pipeline run).
1. A proof of confluence of φ-calculus.
   - A proof of confluence of φ-calculus is implemented in the [proof](https://github.com/objectionary/proof) repository.
   - The proof documentation is available [here](https://www.objectionary.com/proof/docs/Minimal/Term.html).
1. 10 tests for the proof ([link](https://github.com/objectionary/proof/blob/master/Minimal/Examples.lean)).
1. The approved patent application ([link](https://www.overleaf.com/project/66912652c058ec418f5b3e50)).
