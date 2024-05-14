# shellcheck disable=SC2148

set -euo pipefail

if ! [ -d node_modules ]; then npm i; fi

export LC_ALL=C.UTF-8
shopt -s extglob

# import scripts - https://stackoverflow.com/a/12694189
DIR="${BASH_SOURCE%/*}"
if [[ ! -d "$DIR" ]]; then DIR="$PWD/pipeline"; fi
source "$DIR/lib.sh"

EO="$(get_eo_version)"

print_message "EO version: $EO"

function eo {
    npx eoc --parser="$EO" "$@"
}

function check_configs {
    # TODO #263:1h Check all fields of configs in a Haskell script

    pipeline_config="pipeline/config.yaml"
    report_config="report/config.yaml"
    eo_tests="eo/eo-runtime/src/test/eo/org/eolang"

    eo_files="$(mktemp)"
    (
        cd "$eo_tests"
        find -- * -type f \
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
        && print_message "Result: no difference"
}

function generate_eo_tests {
    print_message "Generate EO test files"

    stack run transform-eo-tests
}

function enter_pipeline_directory {

    print_message "Enter the pipeline directory"

    cd pipeline
}

function convert_eo_to_phi {

    print_message "Convert EO to PHI"

    mkdir_clean phi

    cd eo
    eo clean
    eo phi
    cp -r .eoc/phi/!(org) ../phi
    cd ..
}

function update_normalizer_phi_files {

    print_message "Update .phi data files in eo-phi-normalizer"

    cd eo
    data_directory="../../eo-phi-normalizer/data/$EO"
    mkdir_clean "$data_directory"
    cp -r .eoc/phi/org "$data_directory"
    cd ..
}

function convert_phi_to_eo {

    print_message "Convert PHI to EO without normalization"

    mkdir_clean eo-non-normalized

    cd phi
    cp -r ../eo/.eoc .
    eo unphi --tests
    cp -r .eoc/unphi/!(org) .eoc/2-optimize
    eo print
    cp -r .eoc/print/!(org) ../eo-non-normalized
    cd ..

}

function test_without_normalization {

    print_message "Test EO without normalization"

    cd eo-non-normalized
    eo test
    cd ..
}

function normalize {

    print_message "Normalize PHI"

    mkdir_clean phi-normalized

    cd phi
    phi_files="$(find -- * -type f)"
    dependency_files="$(find ../eo/.eoc/phi/org/eolang -type f)"
    export dependency_files

    stack install --ghc-options -O2

    function normalize_file {
        local f="$1"
        destination="../phi-normalized/$f"
        mkdir -p "$(dirname "$destination")"

        dependency_file_options="$(printf "%s" "$dependency_files" | xargs -I {} printf "%s" " --dependency-file {} ")"

        set -x
        # shellcheck disable=SC2086
        normalizer dataize \
            --recursive \
            --rules \
            ../../eo-phi-normalizer/test/eo/phi/rules/yegor.yaml \
            $dependency_file_options \
            "$f" \
            > "$destination" \
            || set +x
        set +x
    }

    export -f normalize_file

    time printf "%s" "$phi_files" \
        | xargs -I {} bash -c 'normalize_file {}'
    cd ..
}

function generate_report {

    print_message "Generate a report"

    stack run --cwd .. -- report --config report/config.yaml
}

function convert_normalized_phi_to_eo {
    print_message "Convert normalized PHI to EO"

    cd phi-normalized
    cp -r ../eo/.eoc .
    eo unphi --tests
    cp -r .eoc/unphi/!(org) .eoc/2-optimize
    eo print
    cd ..
}

function test_with_normalization {

    print_message "Test EO with normalization"

    mkdir_clean eo-normalized

    cd eo-normalized
    cp -r ../phi-normalized/.eoc/print/!(org)  .
    eo test
    cd ..
}

check_configs
update_pipeline_lock
generate_eo_tests
enter_pipeline_directory

if [[ "$pipeline_lock_changed" = true ]]; then
    convert_eo_to_phi
    update_normalizer_phi_files
    convert_phi_to_eo
    test_without_normalization
fi

normalize
generate_report
convert_normalized_phi_to_eo
test_with_normalization
