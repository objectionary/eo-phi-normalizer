set -euo pipefail
shopt -s extglob

cd pipeline

function collect_metrics {
    printf "\nCollecting metrics for PHI$3\n\n"

    METRICS_DIR="$1"

    mkdir -p "$METRICS_DIR"

    cd "$2"

    PHI_FILES="$(find . -wholename '*.phi' -not -path './.eoc/**')"

    for phi_file in $PHI_FILES; do
        phi_metrics_file_name="${phi_file%.phi}.json"
        destination="../$METRICS_DIR/$phi_metrics_file_name"
        stack run -- metrics "$phi_file" --program-path "org.eolang" > "$destination"
    done

    cd ..
}

collect_metrics "metrics/phi" "phi" ""
collect_metrics "metrics/phi-normalized" "phi-normalized" " normalized"
