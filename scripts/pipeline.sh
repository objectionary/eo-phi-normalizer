set -euo pipefail

if ! [ -d node_modules ]; then npm i; fi

export LC_ALL=C.UTF-8

shopt -s expand_aliases
EO="0.35.8"
alias eo="npx eoc --parser=${EO}"

shopt -s extglob

function print_message {
    printf "\n\n\n[[[%s]]]\n\n\n" "$1"
}

function prepare_directory {
    print_message "Clean the pipeline directory"

    rm -rf pipeline/*/

    print_message "Generate EO test files"

    mkdir -p pipeline/eo
    stack run transform-eo-tests
}

function enter_directory {
    print_message "Enter the pipeline directory"

    cd pipeline
}

function tests_without_normalization {
    print_message "Convert EO to PHI"

    mkdir -p phi
    cd eo
    eo clean
    eo phi
    cp -r .eoc/phi/!(org) ../phi
    cd ..


    print_message "Convert PHI to EO without normalization"

    mkdir -p eo-non-normalized
    cd phi
    cp -r ../eo/.eoc .
    eo unphi --tests
    cp -r .eoc/unphi/!(org) .eoc/2-optimize
    eo print
    cp -r .eoc/print/!(org) ../eo-non-normalized
    cd ..

    print_message "Test EO without normalization"

    cd eo-non-normalized
    eo test
    cd ..
}


function tests_with_normalization {
    print_message "Normalize PHI"

    mkdir -p phi-normalized
    cd phi
    PHI_FILES="$(find . -name '*.phi' -not -path '.eoc/**')"
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


    print_message "Convert normalized PHI to EO"

    cd phi-normalized
    cp -r ../eo/.eoc .
    eo unphi --tests
    cp -r .eoc/unphi/!(org) .eoc/2-optimize
    eo print
    cd ..


    print_message "Test EO with normalization"

    mkdir -p eo-normalized
    cd eo-normalized
    cp -r ../phi-normalized/.eoc/print/!(org)  .
    eo test
    cd ..
}

function generate_report () {
    print_message "Generate a report"

    stack run --cwd .. -- report --config report/config.yaml
}

prepare_directory
enter_directory
tests_without_normalization
tests_with_normalization
generate_report
