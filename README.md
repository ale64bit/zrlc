# The ZRL Programming Language

This repository contains the source code for ZRL, including the compiler, documentation and examples.

*Disclaimer: ZRL is a hobby project developed as a learning exercise combining several topics I enjoy (graphics and compilers). It's a work in progress, barely tested and probably full of bugs. I focused mostly on having a working prototype but there's plenty of room for optimization, better testing and cleaner design.*

## Quick Start

Check the [examples](docs/examples/README.md).

## Building the compiler

The compiler is written in [OCaml](https://en.wikipedia.org/wiki/OCaml) and uses [dune](https://dune.build/) as build system. The easiest way to have a working setup is first installing [opam](https://opam.ocaml.org/), which is the package manager for OCaml and then runnning:

```
opam install dune menhir pcre yojson ocamlgraph oUnit ppx_deriving_yojson
```

This will install dune and the compiler dependencies. After this, you can clone the repo and build the compiler as follows:

```
git clone https://github.com/ale64bit/zrlc.git
cd zrlc
dune build ./bin/zrlc.exe
```

Note that `dune` uses the `.exe` file extension for binaries regardless of the actual OS. You should have the compiler binary built at `zrlc/_build/default/bin/zrlc.exe`. Feel free to move it somewhere else, rename it and/or add it to `$PATH`. We will refer to this binary as `zrlc` elsewhere in the documentation.

## Exploring the Generated Code

In order to explore the generated code, you can run the compiler standalone and specify an output directory. For example:

```
$ mkdir /tmp/output
$ zrlc.exe -i samples/tutorial/simple.zrl -o /tmp/output
$ tree /tmp/output
/tmp/output
├── include
│   ├── Simple.h
│   └── Types.h
├── shader
│   ├── Forward.frag.glsl
│   └── Forward.vert.glsl
└── src
    └── Simple.cc

3 directories, 5 files
```

In the output directory, the compile always creates 3 directories:
* shader: generated GLSL shaders.
* include: generated C++ header files.
* src: generated C++ source files.

## Visualizing the AST

If you omit the output directory, the compiler won't call the backend (i.e. it won't generate target code). This still can be useful if you want to verify if a ZRL program compiles correctly. Additionally, you can specify `-ast_file` in order to get a [dot](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) file containing a diagram of the AST. For example:

```
$ zrlc.exe -i input.zrl -ast_file /tmp/ast.dot
```

If you are on Ubuntu (and probably other Linux distros) you can use the provided script `show_ast.sh` to also convert the dot file to an image and display it. For example:

```
$ ./show_ast.sh input.zrl
```

(this will open the AST image in a new window)

## Limitations and Improvements

* the compiler reports only the first error it encounters.
* very few tests, mostly for the analysis phase.
* only a single backend for Vulkan is implemented.
* only graphics pipelines are currently supported by the backend.
* stencil buffer not supported.
* multisampling not supported.
* blending not supported.
* generated shader code is GLSL.
* render target sizes different from swapchain size not supported.
* only a single input file is allowed. `module` not really used as of yet.
* generated code is single-threaded.
* staging and uniform internal buffers have fixed sizes. This should be configurable.
* no optimizations. I would like to explore a more systematic way of implementing and combining the phases, in particular for optimizations ([this article](http://okmij.org/ftp/tagless-final/course/optimizations.html) looks very attractive but I need to research more):
  - merging or exploding shader stages.
  - descriptor set assignment according to bind frequency.
  - reaching definitions to avoid hashing pipelines and render passes.
  - using derivative pipelines.
  - optimal barriers and image layout transitions.

## Contributing

See the [Contributing](CONTRIBUTING.md) section.

## License

ZRL is distributed under the Apache License Version 2.0.

