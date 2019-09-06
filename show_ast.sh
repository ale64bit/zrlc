#!/bin/bash
# Copyright 2019 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -e
rm -f /tmp/output.png
rm -f /tmp/output.dot
OCAMLRUNPARAM=b dune exec ./bin/zrlc.exe -- -i $1 -ast_file /tmp/output.dot -v 
echo 'Converting dot to png...'
dot -Tpng /tmp/output.dot > /tmp/output.png
echo 'Showing...'
eog /tmp/output.png
