set -euo pipefail

if ! [ -d node_modules ]; then npm i; fi

export LC_ALL=C.UTF-8

shopt -s expand_aliases
EO="0.36.0"
alias eo="npx eoc --parser=${EO}"

shopt -s extglob

function print_message {
    printf "\n\n\n[[[%s]]]\n\n\n" "$1"
}

function check_configs {
    # TODO #263:1h Check all fields of configs in a Haskell script

    pipeline_config=pipeline/config.yaml
    report_config=report/config.yaml
    eo_tests=eo/eo-runtime/src/test/eo/org/eolang

    eo_files="$(mktemp)"
    (
        cd "$eo_tests"
        find * -type f \
            | sort \
            | uniq \
            > "$eo_files"
    )

    print_message "Check diff between $pipeline_config and EO tests in $eo_tests"

    pipeline_eo_files="$(mktemp)"
    grep source "$pipeline_config" \
        | sed -r 's|.*/eolang/(.*\.eo)|\1|g' \
        > "$pipeline_eo_files"

    diff -U 10 "$pipeline_eo_files" "$eo_files" \
        && print_message "No difference"

    print_message "Check diff between $report_config and EO tests in $eo_tests"

    report_eo_files="$(mktemp)"
    grep '\- phi:' "$report_config" \
        | sed -r 's|.*/phi/(.*).phi|\1|g' \
        | xargs -I {} printf "%s.eo\n" {} \
        > "$report_eo_files"

    diff -U 10 "$report_eo_files" "$eo_files" \
        && print_message "No difference"
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

    mkdir -p eo-not-normalized
    cd phi
    cp -r ../eo/.eoc .
    eo unphi --tests
    cp -r .eoc/unphi/!(org) .eoc/2-optimize
    eo print
    cp -r .eoc/print/!(org) ../eo-not-normalized
    cd ..

    print_message "Test EO without normalization"

    cd eo-not-normalized
    eo test
    cd ..
}


function tests_with_normalization {
    print_message "Normalize PHI"

    mkdir -p phi-normalized
    cd phi
    PHI_FILES="$(find . -name '*.phi' -not -path '.eoc/**')"
    dependencies_files="$(find .eoc/phi/org/eolang -type f)"

    function normalize {
        set -x
        local f="$1"
        destination="../phi-normalized/$f"
        mkdir -p "$(dirname "$destination")"

        stack run -- \
            dataize \
            --rules \
            ../../eo-phi-normalizer/test/eo/phi/rules/yegor.yaml \
            $(printf "%s" "$(find .eoc/phi/org/eolang -type f)" | xargs -I {} printf "--dependency-file %s " {}) \
            "$f" \
            > "$destination" || set +x
        set +x
    }

    export -f normalize

    time printf "%s" "$PHI_FILES" \
        | xargs -I {} bash -c 'normalize {}'
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

check_configs
# prepare_directory
# enter_directory
# tests_without_normalization
# tests_with_normalization
# generate_report
