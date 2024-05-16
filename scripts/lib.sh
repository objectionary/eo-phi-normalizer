# shellcheck disable=SC2148

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

function print_message {
    printf "\n\n\n[[[%s]]]\n\n\n" "$1"
}

export -f print_message

function set_is_windows {
    # check whether the current platform is Windows
    # https://stackoverflow.com/a/3466183
    local unameOut
    unameOut="$(uname -s)"

    IS_WINDOWS=false

    case "${unameOut}" in
        Linux*)     IS_WINDOWS=false;;
        Darwin*)    IS_WINDOWS=false;;
        CYGWIN*)    IS_WINDOWS=true;;
        MINGW*)     IS_WINDOWS=true;;
        MSYS_NT*)   IS_WINDOWS=true;;
        *)          IS_WINDOWS=false
    esac
}

set_is_windows

function set_installation_path {
    INSTALLATION_PATH="$(stack path --local-bin)"

    if [[ "$IS_WINDOWS" = "true" ]]; then
        INSTALLATION_PATH="$(cygpath.exe "$INSTALLATION_PATH")"
    fi

    print_message "Normalizer installation path: $INSTALLATION_PATH"
}

export -f set_installation_path

function add_installation_path_to_path {
    if ! [[ ":$PATH:" == *":$INSTALLATION_PATH:"* ]]; then
        export PATH="$INSTALLATION_PATH:$PATH"
        print_message "Added Normalizer installation path to PATH"
    else
        print_message "PATH already contains the Normalizer installation path"
    fi
}

export -f add_installation_path_to_path

function write_pipeline_lock {
        cat > "$PIPELINE_LOCK_FILE_NEW" <<EOF
EO_HEAD_HASH="$(git rev-parse HEAD:eo)"
PIPELINE_CONFIG_HASH="$(git hash-object "$PIPELINE_CONFIG_FILE")"
EOF

    PIPELINE_LOCK_CHANGED="${PIPELINE_LOCK_CHANGED:-false}"

    if ! cmp "$PIPELINE_LOCK_FILE" "$PIPELINE_LOCK_FILE_NEW"; then
        PIPELINE_LOCK_CHANGED="true"
    fi
}

export -f write_pipeline_lock

function update_pipeline_lock {
    print_message "Check the pipeline lock in $PIPELINE_LOCK_FILE"

    write_pipeline_lock

    if [[ "$PIPELINE_LOCK_CHANGED" = "true" ]]; then
        print_message "Pipeline lock updated"
        mv "$PIPELINE_LOCK_FILE_NEW" "$PIPELINE_LOCK_FILE"
    else
        print_message "Pipeline lock didn't change"
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
    local files="$1"
    local updated_message="$2"
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

function install_normalizer {
    set_installation_path
    add_installation_path_to_path

    print_message "Install the Normalizer"

    if [[ "$NORMALIZER_INSTALLED" = "true" && "$IS_WINDOWS" = "true" ]]; then
        mv "$INSTALLATION_PATH/normalizer.exe" "$INSTALLATION_PATH/normalizer"
    else
        stack install eo-phi-normalizer:exe:normalizer --ghc-options -O2
    fi

    print_message "The Normalizer is installed"
}

export -f install_normalizer
