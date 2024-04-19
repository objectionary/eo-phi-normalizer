# shellcheck disable=SC2148

set -euo pipefail

if ! [ -d node_modules ]; then npm i; fi

shopt -s extglob
EO="${EO:-$(yq '.project.parent.version' -p xml < eo/eo-runtime/pom.xml)}"
DIR=try-unphi

function print_message {
    printf "\n\n\n[[[%s]]]\n\n\n" "$1"
}

function eo {
    npx eoc --parser="${EO}"
}

function prepare_directory {
    print_message "Prepare the $DIR directory"

    mkdir -p $DIR/phi
    mkdir -p $DIR/init

    rm -rf $DIR/tmp
    mkdir -p $DIR/tmp

    rm -rf $DIR/unphi
    mkdir -p $DIR/unphi
}

function enter_directory {
    print_message "Enter the $DIR directory"

    cd $DIR
}

function init_eoc {
    print_message "Generate an initial .eoc directory"

    cd init

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

    cd ..
}

function unphi {
    print_message "Unphi"

    cd tmp

    cp -r ../init/.eoc .
    cp -r ../phi/* .eoc/phi

    eo unphi

    cp -r .eoc/unphi/!(org) .eoc/2-optimize

    eo print

    cp -r .eoc/print/!(org) ../unphi

    cd ..
}

prepare_directory
enter_directory
init_eoc
unphi
