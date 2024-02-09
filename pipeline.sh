set -euo pipefail

if ! [ -d node_modules ]; then npm i; fi

shopt -s expand_aliases
EO="0.34.1"
alias eo="npx eoc --parser=${EO}"

printf "\nGenerate EO test files\n\n"

stack run transform-eo-tests

cd pipeline

printf "\nConvert EO to PHI\n\n"

mkdir -p phi
cd eo
eo clean
eo phi
cp .eoc/phi/*.phi ../phi
cd ..

printf "\nNormalize PHI\n\n"

mkdir -p phi-normalized
cd phi
for f in $(ls); do
    stack run -- \
        -s \
        --rules-yaml \
        ../../eo-phi-normalizer/test/eo/phi/rules/yegor.yaml \
        "$f" \
        > "../phi-normalized/$f"
done
cd ..

printf "\nConvert normalized PHI to EO\n\n"

cd phi-normalized
cp -r ../eo/.eoc .
eo unphi
cp .eoc/unphi/*.xmir ".eoc/2-optimize"
cd ..

printf "\nTest EO\n\n"

mkdir -p eo-normalized
cd eo-normalized
cp ../phi-normalized/.eoc/print/*.eo .
for f in $(ls *.eo); do
    if ! [ -s "${f}" ]; then
        rm $f
    fi
done
eo test
cd ..
