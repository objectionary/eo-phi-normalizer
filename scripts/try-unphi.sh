# shellcheck disable=SC2148

set -euo pipefail

if ! [ -d node_modules ]; then npm i; fi

shopt -s extglob

IMPORT_DIR="$PWD/scripts"
source "$IMPORT_DIR/lib.sh"

EO="$(get_eo_version)"

DIR="$PWD/try-unphi"

function prepare_directory {
    print_message "Prepare the $DIR directory"

    INPUT_DIR="$DIR/input-phi-programs"
    OUTPUT_DIR="$DIR/output-eo-programs"
    TMP_DIR="$DIR/tmp"
    INIT_DIR="$DIR/init"

    mkdir -p "$INPUT_DIR"
    mkdir -p "$INIT_DIR"

    mkdir_clean "$TMP_DIR"
    mkdir_clean "$OUTPUT_DIR"
}

function enter_directory {
    print_message "Enter the $DIR directory"

    cd "$DIR"
}

function init_eoc {
    print_message "Generate an initial .eoc directory"

    cd "$INIT_DIR"

    if [ ! -d .eoc ]; then
        cat <<EOM > test.eo
+alias org.eolang.io.stdout
+architect yegor256@gmail.com
+home https://github.com/objectionary/eo
+tests
+package org.eolang
+version 0.0.0

# Test.
[] > prints-itself
  gt. > @
    length.
      as-phi $
    0
EOM
        eo phi
    fi

    rm -f .eoc/phi/test.phi
    rm -f .eoc/2-optimize/test.xmir

    cd "$DIR"
}

function unphi {
    print_message "Unphi"

    cd "$TMP_DIR"

    cp -r "$INIT_DIR/.eoc" .
    cp -r "$INPUT_DIR/*" .eoc/phi

    eo unphi

    cp -r .eoc/unphi/!(org) .eoc/2-optimize

    eo print

    cp -r .eoc/print/!(org) "$OUTPUT_DIR"

    cd "$DIR"
}

prepare_directory
enter_directory
init_eoc
unphi
