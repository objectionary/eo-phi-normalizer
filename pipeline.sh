set -euo pipefail

if ! [ -d node_modules ]; then npm i; fi

shopt -s expand_aliases
EO="0.35.2"
alias eo="npx eoc --parser=${EO}"

printf "\nClean the pipeline directory\n\n"

rm -rf pipeline/*/


printf "\nGenerate EO test files\n\n"

stack run transform-eo-tests


printf "\nEnter the pipeline directory\n\n"

cd pipeline


printf "\nConvert EO to PHI\n\n"

mkdir -p phi
cd eo
eo clean
eo phi
cp .eoc/phi/*.phi ../phi
cd ..


printf "\nConvert PHI to EO without normalization\n\n"

mkdir -p eo-not-normalized
cd phi
cp -r ../eo/.eoc .
eo unphi
cp .eoc/unphi/*.xmir ".eoc/2-optimize"
eo print
cp .eoc/print/*.eo ../eo-not-normalized
cd ..


printf "\nTest EO without normalization\n\n"

cd eo-not-normalized
eo test
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
eo print
cd ..


printf "\nTest EO\n\n"

mkdir -p eo-normalized
cd eo-normalized
cp ../phi-normalized/.eoc/print/*.eo .
eo test
cd ..
