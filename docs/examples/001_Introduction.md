[Table of Contents](README.md) | Next: [Development Environment](002_Development_Environment.md)

# Introduction

Welcome to ZRL!

ZRL stands for **Z** **R**endering **L**anguage. In a nutshell, ZRL allows you to generate a 3D graphics renderer from a high-level description.

The main motivation for ZRL was while following Vulkan tutorials and reading about real-time rendering. The tutorials and examples are not hard to follow, however, trying to modify them or trying new ideas required large amounts of boilerplate and details that slow down the overall creative process. On the other hand, I wasn't willing to use a full-blown engine (like UE4 or Unity) since the target was to learn, after all. The idea of a DSL for a rendering engine was attractive for the following reasons:

* It allows me to write the same sort of things I would write when using Vulkan and other low-level APIs directly, but without having to deal with the boilerplate and unnecessary details.
* It allows me to decouple the problem of writing rendering pipelines (the creative part) from the problem of optimizing such pipelines (the technical part). I enjoy both sides, but perhaps not concurrently.

So how does it look like? Before diving in, let's setup our development environment.

[Table of Contents](README.md) | Next: [Development Environment](002_Development_Environment.md)
