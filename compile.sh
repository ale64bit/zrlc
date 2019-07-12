#!/bin/bash
set -e
OCAMLRUNPARAM=b dune exec ./bin/zrlc.exe -- -i $1 -o /tmp/ -v
