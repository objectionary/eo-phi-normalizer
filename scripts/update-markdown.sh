#!/bin/bash

# The MIT License (MIT)
#
# SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
# SPDX-License-Identifier: MIT

# shellcheck disable=SC2148

# This file was generated automatically.
# You can edit the script in 'flake.nix'
set -ex

mdsh -i site/docs/src/common/celsius.md --work_dir .
mdsh -i site/docs/src/eo-phi-normalizer.md --work_dir .
mdsh -i site/docs/src/installation.md --work_dir .
mdsh -i site/docs/src/pipeline.md --work_dir .
mdsh -i site/docs/src/quick-start.md --work_dir .
mdsh -i site/docs/src/eo-phi-normalizer/dataize.md --work_dir .
mdsh -i site/docs/src/eo-phi-normalizer/metrics.md --work_dir .
mdsh -i site/docs/src/eo-phi-normalizer/rewrite.md --work_dir .
mdsh -i site/docs/src/eo-phi-normalizer/print-rules.md --work_dir .
mdsh -i site/docs/src/eo-phi-normalizer/test.md --work_dir .
mdsh -i site/docs/src/contributing.md --work_dir .

cp site/docs/src/contributing.md CONTRIBUTING.md

rm celsius.phi bar.phi

npm i
npx prettier -w "**/*.md"
