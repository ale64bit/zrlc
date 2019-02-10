#!/bin/bash
set -e
dune exec ./bin/rpdlc.exe -- -i $1 -ast_file $2
echo 'Converting dot to png...'
dot -Tpng $2 > /tmp/output.png
echo 'Showing...'
eog /tmp/output.png
