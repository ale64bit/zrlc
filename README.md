# The ZRL Programming Language

This repository contains the source code for ZRL, including the compiler, documentation and examples.

*Disclaimer: ZRL is a hobby project developed as a learning exercise combining several topics I enjoy (graphics and compilers). It's a work in progress, barely tested and probably full of bugs. I focused mostly on having a working prototype but there's plenty of room for optimization, better testing and cleaner design.*

## Quick Start

Check the [tutorial](docs/tutorial/README.md).

## Building the compiler

The compiler is written in [OCaml](https://en.wikipedia.org/wiki/OCaml) and uses [dune](https://dune.build/) as build system. The easiest way to have a working setup is first installing [opam](https://opam.ocaml.org/), which is the package manager for OCaml and then runnning:

```
opam install dune menhir pcre yojson ocamlgraph oUnit
```

This will install dune and the compiler dependencies. After this, you can clone the repo and build the compiler as follows:

```
git clone https://github.com/ale64bit/zrlc.git
cd zrlc
dune build ./bin/zrlc.exe
```

Note that `dune` uses the `.exe` file extension for binaries regardless of the actual OS. You should have the compiler binary built at `zrlc/_build/default/bin/zrlc.exe`. Feel free to move it somewhere else, rename it and/or add it to `$PATH`. We will refer to this binary as `zrlc` elsewhere in the documentation.

## Exporting the AST

TODO

## Contributing

TODO

## License

ZRL is distributed under the Apache License Version 2.0.

