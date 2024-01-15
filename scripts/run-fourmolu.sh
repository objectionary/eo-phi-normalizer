#!/usr/bin/env bash
#
# Run fourmolu, assuming it's already been built.

# adopted from https://github.com/fourmolu/fourmolu/blob/f3b49896d0e00fb33192c43bc40496b39f62dddb/.pre-commit-config.yaml

# Copyright © 2018–2020 Tweag I/O, 2020-present Matt Parsons

# All rights reserved.

# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

#     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

#     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

#     Neither the name Tweag I/O nor the names of contributors may be used to endorse or promote products derived from this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

set -eu -o pipefail

if [[ -z "${BUILD_TYPE:-}" ]]; then
    # naive detection of stack vs cabal
    if [[ -d .stack-work ]]; then
        BUILD_TYPE=stack
    else
        BUILD_TYPE=cabal
    fi
fi

case "${BUILD_TYPE}" in
    (stack) FOURMOLU='stack exec -- fourmolu' ;;
    (cabal) FOURMOLU='cabal run -- fourmolu' ;;
    (*)
        echo "Unknown build type: ${BUILD_TYPE}" >&2
        exit 1
    ;;
esac

exec $FOURMOLU "$@"