mkdir -p pipeline
cd pipeline

shopt -s expand_aliases

EO=0.34.2
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
# .eoc/phi/app.phi

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