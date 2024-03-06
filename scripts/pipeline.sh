set -euo pipefail

if ! [ -d node_modules ]; then npm i; fi

export LC_ALL=C.UTF-8

shopt -s expand_aliases
EO="0.35.6"
alias eo="npx eoc --parser=${EO}"

shopt -s extglob

function prepare_directory {
    printf "\nClean the pipeline directory\n\n"

    rm -rf pipeline/*/

    printf "\nGenerate EO test files\n\n"

    mkdir -p pipeline/eo
    stack run transform-eo-tests
}

function enter_directory {
    printf "\nEnter the pipeline directory\n\n"

    cd pipeline
}

function tests_without_normalization {
    printf "\nConvert EO to PHI\n\n"

    mkdir -p phi
    cd eo
    eo clean
    eo phi
    cp -r .eoc/phi/!(org) ../phi
    cd ..


    printf "\nConvert PHI to EO without normalization\n\n"

    mkdir -p eo-not-normalized
    cd phi
    cp -r ../eo/.eoc .
    eo unphi --tests
    cp -r .eoc/unphi/!(org) .eoc/2-optimize
    eo print
    cp -r .eoc/print/!(org) ../eo-not-normalized
    cd ..

    printf "\nTest EO without normalization\n\n"

    cd eo-not-normalized
    eo test
    cd ..
}


function tests_with_normalization {
    printf "\nNormalize PHI\n\n"

    mkdir -p phi-normalized
    cd phi
    PHI_FILES="$(find -name '*.phi' -not -path '.eoc/**')"
    for f in $PHI_FILES; do
        destination="../phi-normalized/$f"
        mkdir -p $(dirname $destination)

        stack run -- \
            transform \
            --single \
            --rules \
            ../../eo-phi-normalizer/test/eo/phi/rules/yegor.yaml \
            "$f" \
            > $destination
    done
    cd ..


    printf "\nConvert normalized PHI to EO\n\n"

    cd phi-normalized
    cp -r ../eo/.eoc .
    eo unphi --tests
    cp -r .eoc/unphi/!(org) .eoc/2-optimize
    eo print
    cd ..


    printf "\nTest EO\n\n"

    mkdir -p eo-normalized
    cd eo-normalized
    cp -r ../phi-normalized/.eoc/print/!(org)  .
    eo test
    cd ..
}

prepare_directory
enter_directory
tests_without_normalization
tests_with_normalization
