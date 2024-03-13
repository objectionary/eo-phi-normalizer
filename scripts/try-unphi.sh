set -euo pipefail

if ! [ -d node_modules ]; then npm i; fi

shopt -s expand_aliases
EO="0.35.5"
alias eo="npx eoc --parser=${EO}"

DIR=try-unphi

function prepare_directory {
    printf "\nClean the $DIR directory\n\n"

    mkdir -p $DIR/init
    mkdir -p $DIR/phi
    mkdir -p $DIR/tmp
    mkdir -p $DIR/unphi
}

function enter_directory {
    printf "\nEnter the $DIR directory\n\n"

    cd $DIR
}

function init_eoc {
    printf "\nGenerate an initial .eoc directory\n\n"

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

    cd ..
}

function unphi {
    printf "\nUnphi\n\n"

    cd tmp
    rsync -r ../phi/ .
    rsync -r ../init/.eoc .
    eo unphi
    rsync -r .eoc/unphi/ --exclude org .eoc/2-optimize
    eo print
    rsync -r .eoc/print/ --exclude org ../unphi
    cd ..
}

prepare_directory
enter_directory
init_eoc
unphi
