set -euo pipefail

shopt -s expand_aliases
EO="0.34.1"
alias eo="npx eoc --parser=${EO}"

# generate EO test files
stack run transform-eo-tests

# convert EO to PHI
cd pipeline/eo
eo clean
eo phi
cd ..

mkdir -p phi
cp eo/.eoc/phi/*.phi phi

cd phi
# TODO #100:15min run normalizer
# normalizer should create phi-normalized
cd ..

# TODO #100:15min run unphi
# cd phi-normalized
# eo unphi
# cd ..
# mkdir -p eo-normalized
# copy normalized files to eo-normalized

# TODO #100:15min run tests
# cd eo-normalized
# eo test



# TODO #100:15min remove old code
# eo phi

# IO=".eoc/phi/app.phi"
# I=".eoc/phi/app.bk.phi"
# mv "$IO" "$I"

# stack run normalize-phi < "$I" > "$IO" \
#   || {
#     cat <<EOF
# Normalizer failed!


# * EO expression:

# $(cat app.eo)


# * Phi expression:

# $(cat "$I")


# * Error:

# $(cat "$IO")
# EOF
#     mv "$I" "$IO"
#     exit 1
#   }

# {
#   export LC_ALL="C"
#   perl -i -pe 'chomp if eof' "$IO"
# }

# cat <<EOF
# Normalizer succeeded!


# * EO expression:

# $(cat app.eo)


# * Phi expression:

# $(cat "$I")


# * Normalized Phi expression:

# $(cat "$IO")


# * Diff:

# $(diff "$I" "$IO" || true)
# EOF

# eo unphi

# cp .eoc/unphi/app.xmir .eoc/2-optimize/app.xmir

# eo print

# cp .eoc/print/app.eo app.eo

# eo clean
# eo link
# eo --alone dataize app > after.txt

# # Check dataization with and without the normalizer
# # produces the same results

# if [ "$(cat before.txt)" == "$(cat after.txt)" ]; then
#     echo 'SUCCESS'
# else
#     echo 'FAILURE'
#     exit 1
# fi
