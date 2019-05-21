#!/bin/bash
set -e
rm -f /tmp/output.png
rm -f $2
OCAMLRUNPARAM=b dune exec ./bin/rpdlc.bc -- -i $1 -ast_file $2 -v ${@:3}
echo 'Converting dot to png...'
dot -Tpng $2 > /tmp/output.png
echo 'Showing...'
eog /tmp/output.png
