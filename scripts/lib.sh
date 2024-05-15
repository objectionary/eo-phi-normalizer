# shellcheck disable=SC2148

set -euo pipefail

PWD_DIR="$PWD"
PIPELINE_DIR_RELATIVE="pipeline"
PIPELINE_DIR="$PWD/$PIPELINE_DIR_RELATIVE"
PIPELINE_PHI_DIR="$PIPELINE_DIR/phi"
PIPELINE_EO_DIR="$PIPELINE_DIR/eo"
PIPELINE_EO_NON_NORMALIZED_DIR="$PIPELINE_DIR/eo-non-normalized"
PIPELINE_EO_NORMALIZED_DIR="$PIPELINE_DIR/eo-normalized"
PIPELINE_PHI_NORMALIZED_DIR="$PIPELINE_DIR/phi-normalized"
PIPELINE_NORMALIZER_DIR="$PWD/eo-phi-normalizer"
PIPELINE_REPORT_DIR="$PWD/report"

PIPELINE_CONFIG_FILE="$PIPELINE_DIR/config.yaml"

PIPELINE_LOCK_FILE_NAME="pipeline.lock"
PIPELINE_LOCK_FILE="$PIPELINE_DIR/$PIPELINE_LOCK_FILE_NAME"

PIPELINE_LOCK_FILE_NEW_NAME="pipeline_new.lock"
PIPELINE_LOCK_FILE_NEW="$PIPELINE_DIR/$PIPELINE_LOCK_FILE_NEW_NAME"

PIPELINE_LOCK_FILE_RELATIVE="$PIPELINE_DIR_RELATIVE/$PIPELINE_LOCK_FILE_NAME"
PIPELINE_LOCK_FILE_NEW_RELATIVE="$PIPELINE_DIR_RELATIVE/$PIPELINE_LOCK_FILE_NEW_NAME"

NORMALIZER_INSTALLED="${NORMALIZER_INSTALLED:-false}"

INSTALLATION_PATH="$(dirname "$PWD")/installation"
mkdir -p "$INSTALLATION_PATH"
export PATH="$INSTALLATION_PATH:$PATH"

function print_message {
    printf "\n\n\n[[[%s]]]\n\n\n" "$1"
}

export -f print_message

function write_pipeline_lock {
        cat > "$PIPELINE_LOCK_FILE_NEW" <<EOF
EO_HEAD_HASH="$(git rev-parse HEAD:eo)"
PIPELINE_CONFIG_HASH="$(git hash-object "$PIPELINE_CONFIG_FILE")"
EOF

    PIPELINE_LOCK_CHANGED="${PIPELINE_LOCK_CHANGED:-false}"

    if ! cmp "$PIPELINE_LOCK_FILE" "$PIPELINE_LOCK_FILE_NEW"; then
        PIPELINE_LOCK_CHANGED=true
    fi
}

function update_pipeline_lock {
    print_message "Update pipeline lock in $PIPELINE_LOCK_FILE"

    write_pipeline_lock

    if [[ "$PIPELINE_LOCK_CHANGED" = "true" ]]; then
        print_message "Result: pipeline lock updated"
        mv "$PIPELINE_LOCK_FILE_NEW" "$PIPELINE_LOCK_FILE"
    else
        print_message "Result: pipeline lock didn't change"
    fi
}

export -f write_pipeline_lock

function mkdir_clean {
    rm -rf "$1"
    mkdir -p "$1"
}

export -f mkdir_clean

function get_eo_version {
    printf "%s" "${EO:-$(yq '.project.parent.version' -p xml < eo/eo-runtime/pom.xml)}"
}

export -f get_eo_version

function commit_and_push_if_changed {
    files="$1"
    updated_message="$2"
    if [ -n "$(git status --porcelain "$files")" ]; then
        git add "$files"
        git commit -m "Update $updated_message"
        git push
    else
        echo "Nothing to commit.";
    fi
}

export -f commit_and_push_if_changed

EO="$(get_eo_version)"

function eo {
    npx eoc --parser="$EO" "$@"
}

export -f eo
