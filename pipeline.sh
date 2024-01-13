set -euo pipefail

mkdir -p pipeline
cd pipeline

shopt -s expand_aliases

EO="0.34.3"
alias eo="eoc --parser=${EO}"

cat > app.eo <<EOT
[args] > app
  QQ.io.stdout > @
    "Hello, world!\n"
EOT

eo clean
eo link
eo --alone dataize app > before.txt

eo phi

# Now, you modify/normalize this file:
IO=".eoc/phi/app.phi"
I=".eoc/phi/app.bk.phi"
mv "$IO" "$I"
stack run normalize-phi < "$I" > "$IO" \
  || {
    printf "\n\nNormalizer failed!"
    printf "\n\n* EO expression:\n\n"
    cat app.eo
    printf "\n\n* Phi expression:\n\n"
    cat "$I"
    printf "\n\n* Error:\n\n"
    cat "$IO"
    exit 1
  }
perl -i -pe 'chomp if eof' "$IO"

printf "\n\nNormalizer succeeded!"
printf "\n\n* EO expression:\n\n"
cat app.eo
printf "\n\n* Phi expression:\n\n"
cat "$I"
printf "\n\n* Normalized Phi expression:\n\n"
cat "$IO"
printf "\n\n* Diff:\n\n"
diff "$I" "$IO" || true

eo unphi

cp .eoc/unphi/app.xmir .eoc/2-optimize/app.xmir

eo print

cp .eoc/print/app.eo app.eo

eo clean
eo link
eo --alone dataize app > after.txt

if [ "$(cat before.txt)" == "$(cat after.txt)" ]; then
    echo 'SUCCESS'
else
    echo 'FAILURE'
    exit 1
fi