#!/bin/bash
set -e
rm -f /tmp/output.png
rm -f /tmp/output.dot
OCAMLRUNPARAM=b dune exec ./bin/zrlc.exe -- -i $1 -ast_file /tmp/output.dot -v 
echo 'Converting dot to png...'
dot -Tpng /tmp/output.dot > /tmp/output.png
echo 'Showing...'
eog /tmp/output.png
