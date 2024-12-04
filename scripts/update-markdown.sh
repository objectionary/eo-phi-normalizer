# The MIT License (MIT)
# 
# Copyright (c) 2016-2024 Objectionary.com
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# 

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
