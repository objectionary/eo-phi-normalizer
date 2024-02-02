set -euo pipefail

ROOT_DIR="$PWD"

mkdir -p ".pipeline/$PROGRAM"
cd ".pipeline/$PROGRAM"

shopt -s expand_aliases

EO="0.34.3"
alias eo="eoc --parser=${EO}"

cp "$ROOT_DIR"/pipeline/programs/"$PROGRAM"/app.eo .

# Without normalizer

eo clean
eo link
eo --alone dataize app > before.txt
cp before.txt "$ROOT_DIR"/pipeline/programs/"$PROGRAM"/before.txt

# With normalizer

eo phi

IO=".eoc/phi/app.phi"
I=".eoc/phi/app.bk.phi"
mv "$IO" "$I"

stack run normalize-phi < "$I" > "$IO" \
  || {
    cat <<EOF
Normalizer failed!


* EO expression:

$(cat app.eo)


* Phi expression:

$(cat "$I")


* Error:

$(cat "$IO")
EOF
    mv "$I" "$IO"
    exit 1
  }

{
  export LC_ALL="C"
  perl -i -pe 'chomp if eof' "$IO"
}

cat <<EOF
Normalizer succeeded!


* EO expression:

$(cat app.eo)


* Phi expression:

$(cat "$I")


* Normalized Phi expression:

$(cat "$IO")


* Diff:

$(diff "$I" "$IO" || true)
EOF

eo unphi

cp .eoc/unphi/app.xmir .eoc/2-optimize/app.xmir

eo print

cp .eoc/print/app.eo app.eo

eo clean
eo link
eo --alone dataize app > after.txt

# Check dataization with and without the normalizer
# produces the same results

if [ "$(cat before.txt)" == "$(cat after.txt)" ]; then
    echo 'SUCCESS'
else
    echo 'FAILURE'
    exit 1
fi
