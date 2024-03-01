set -euo pipefail

if ! [ -d node_modules ]; then npm i; fi

shopt -s expand_aliases
EO="0.35.5"
alias eo="npx eoc --parser=${EO}"

function prepare_directory {
    printf "\nClean the pipeline directory\n\n"

    rm -rf pipeline/*/

    printf "\nGenerate EO test files\n\n"

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
    rsync -r .eoc/phi/* --exclude org ../phi
    cd ..


    printf "\nConvert PHI to EO without normalization\n\n"

    mkdir -p eo-not-normalized
    cd phi
    rsync -r ../eo/.eoc .
    eo unphi
    rsync -r .eoc/unphi/* --exclude org .eoc/2-optimize
    eo print
    rsync -r .eoc/print/* --exclude org ../eo-not-normalized
    cd ..


    function add_metas {
        EO_FILES="$(find -name '*.eo' -not -path '.eoc/**')"
        for f in $EO_FILES;
        do
            cat $f > $f.bk
            printf "+tests\n" > $f
            cat $f.bk >> $f
            rm $f.bk
        done
    }

    printf "\nTest EO without normalization\n\n"

    cd eo-not-normalized
    add_metas
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
    rsync -r ../eo/.eoc .
    eo unphi
    rsync -r .eoc/unphi/* --exclude org .eoc/2-optimize
    eo print
    cd ..


    printf "\nTest EO\n\n"

    mkdir -p eo-normalized
    cd eo-normalized
    rsync -r ../phi-normalized/.eoc/print/* --exclude org  .
    add_metas
    eo test
    cd ..
}

prepare_directory
enter_directory
tests_without_normalization
tests_with_normalization
