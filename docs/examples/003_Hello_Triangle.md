Previous: [Development Environment](002_Development_Environment.md) | [Table of Contents](README.md) | Next: [Textured Cube](004_Textured_Cube.md)

# Hello Triangle

## TL;DR

**Description**: Draws a colored triangle.

**Code**: https://github.com/ale64bit/zrl/tree/master/examples/hellotriangle

**Run it**: `bazel run //examples/hellotriangle:main`.

**Screenshots**:
<p align="center">
  <img src="https://github.com/ale64bit/zrlc/blob/master/docs/examples/003_Hello_Triangle.png" width="400">
</p>

## Introduction

In this example we explore the canonical hello world application for graphics API: drawing a triangle.

The main idea around ZRL is to stay as low-level as possible (think, shader languages such as [GLSL](https://en.wikipedia.org/wiki/OpenGL_Shading_Language) and [HLSL](https://en.wikipedia.org/wiki/High-Level_Shading_Language)) while removing certain tedious elements such as specifying inputs and uniforms' binding locations, and elevating a few common concepts as first-class citizens of the language. When using a shader language directly, each shader normally describes a single stage of the graphics pipeline and the interaction between all these stages and passes is normally delegated to the host language (commonly, C++). ZRL attempts to unify writing the rendering pipelines and also their relationships to produce a blackbox library that encapsulates the whole rendering engine while remaining as agnostic as possible to the format of the inputs.

The whole ZRL program for this example is:
```
module hellotriangle

pipeline Draw(): fvec4 {

  def vertex(pos, col: fvec3): fvec3 {
    builtin.position = fvec4(pos, 1.0)
    return col
  }

  def fragment(col: fvec3): fvec4 {
    return fvec4(col, 1.0)
  }
}

renderer Main() {

  def main(triangle: atom) {
    builtin.screen = fvec4(0.0, 0.0, 0.0, 1.0)
    builtin.screen += Draw(pos=triangle, col=triangle)
  }
}
```

Let's walk through it. 

### Pipeline

First, it's the mandatory `module` statement that must appear as the first element of each ZRL program and identifies it:
```
module hellotriangle
```

Then, we have a pipeline:
```
pipeline Draw(): fvec4 {

  def vertex(pos, col: fvec3): fvec3 {
    builtin.position = fvec4(pos, 1.0)
    return col
  }

  def fragment(col: fvec3): fvec4 {
    return fvec4(col, 1.0)
  }
}
```
A **pipeline** is one of the top-level elements in ZRL programs. It's conceptually equivalent to a [pipeline in Vulkan](https://www.khronos.org/registry/vulkan/specs/1.1/html/vkspec.html#pipelines), if you are familiar with it. As such, there are pipelines of different types: graphics, compute, etc. In ZRL a pipeline is declared with the `pipeline` keyword and looks exactly like a function definition: the arguments to the pipeline become uniforms and the return values become the values written to the render targets. In our example above, our `Draw` pipeline has no arguments (and thus, no uniforms or global state) and returns a single value of type `fvec4` (a vector of 4 32-bit floating point components, equivalent to `vec4` in GLSL and `float4` in HLSL). ZRL supports some types commonly found in other shading languages, such as vector and matrix types.

The pipeline itself contains two functions: `vertex` and `fragment`. To the reader familiar with graphics it should be obvious that these correspond to the vertex and fragment shaders, respectively. The vertex stage has two arguments of type `fvec3`: the position and the color of the vertex. The `builtin` keyword refers to a context-specific object that contains a number of fields that are only available in such context. In this case, it's used to set the `position` attribute of the vertex. This is equivalent to the `gl_Position` builtin in GLSL. Other values are available in different contexts, as we will see later. The `vertex` function returns the provided color unchanged. This color is in turn received by the `fragment` function, which simply returns it after expanding it to a `fvec4`. In summary, `Draw` is a graphics pipeline that takes a position and color of each vertex and outputs a color.

### Renderer

In our example there's a single pipeline, but normally you would probably have more than one (actually, many more). We need a way to specify how these pipelines are assembled together and what are the actual inputs to our renderer. That's the purpose of a **renderer**, another of the top-level elements in ZRL programs:
```
renderer Main() {

  def main(triangle: atom) {
    builtin.screen = fvec4(0.0, 0.0, 0.0, 1.0)
    builtin.screen += Draw(pos=triangle, col=triangle)
  }
}
```

A renderer is declared with the `renderer` keyword and also looks like a function definition. However, note that renderers do not return anything. A renderer also must contain a function called `main`, which is equivalent to the entry point of our program. The catch is that we will not call our program once, but instead, we will call it for every frame. Generally speaking, ZRL does not have a concept of frame and it just outputs a library that exports a `Render` method which is traditionally called once per frame, but you are not limited to do so. The `main` function in our example takes a single argument `triangle` with a strange type: `atom`. We will come back to this later. The code of the renderer itself should be pretty straightforward: we see again the `builtin` object, this time using a field called `screen` which is available only in the renderer and corresponds to the render target that represents the screen. When we assign a `fvec4` to the render target, we are essentially clearing it with a color. Thus, the first line simply clears the screen black. The second line actually uses the pipeline `Draw` and applies the result to the screen.

Note that we are passing two named arguments to the pipeline call: `pos` and `col`, both equal to `triangle`. Even though a pipeline "looks" like a function, it is in fact a bit more broad than that: a pipeline takes as parameters all the pipeline parameters plus all the parameters of the entry-point function, if any. Our pipeline does not have any actual parameters, but the entry-point function, which is `vertex`, takes two parameters: `pos` and `col`. Internally, the compiler backend infers from the function names that this is a graphics pipeline and thus selects `vertex` as the entry-point function. However, we still don't really know what `triangle`'s type `atom` looks like.

### Atoms

The `atom` type represents an abstract type in ZRL. More concretely, it is an arbitrary type that serves as interface between the renderer generated code and the actual user application. For the default compiler backend, what this means in practice is that certain C++ templates are generated and need to be specialized for whatever concrete type we are passing here. Think of it as when you want to use a [std::unordered_map](https://en.cppreference.com/w/cpp/container/unordered_map) with a user-provided type: first you must provide a [std::hash](https://en.cppreference.com/w/cpp/utility/hash) specialization for your custom type.

For example, in our example, the compiler will generate two templates of the form:
```cpp
template <class T> struct Draw_pos;
template <class T> struct Draw_col;
```

and we will have to supply specializations for whatever concrete type we actually use when rendering. The generated templates are always named after the pipeline and parameter they relate to, i.e. `<PIPELINE>_<PARAMETER>`.

### Specializations

Let's take a look at [main.cc](https://github.com/ale64bit/zrl/blob/master/examples/hellotriangle/main.cc) for our example.

First, we have the concrete type we will use to represent our triangle:
```cpp
struct Triangle {
  const std::array<glm::fvec3, 3> position;
  const std::array<glm::fvec3, 3> color;
} triangle = {
    {
        glm::fvec3(0.0, -1.0, 0.0),
        glm::fvec3(1.0, 1.0, 0.0),
        glm::fvec3(-1.0, 1.0, 0.0),
    },
    {
        glm::fvec3(1.0, 0.0, 0.0),
        glm::fvec3(0.0, 1.0, 0.0),
        glm::fvec3(0.0, 0.0, 1.0),
    },
};
```

This is pretty straighforward. We have the positions and colors for each vertex of the triangle stored in arrays.

Now let's check the actual specializations (for now, ignore the `Draw_indices_` specialization):

```cpp
template <> struct Draw_pos<Triangle> {
  void operator()(const Triangle &t, uint32_t &uid, void const **src,
                  VkDeviceSize &size) const noexcept {
    uid = 0;
    size = 3 * sizeof(glm::fvec3);
    if (src != nullptr) {
      *src = t.position.data();
    }
  }
};

template <> struct Draw_col<Triangle> {
  void operator()(const Triangle &t, uint32_t &uid, void const **src,
                  VkDeviceSize &size) const noexcept {
    uid = 0;
    size = 3 * sizeof(glm::fvec3);
    if (src != nullptr) {
      *src = t.color.data();
    }
  }
};
```

Each specialization provides a call operator `operator()` implementation, similar to `std::hash` and other templates in the STL. Each implementations takes 4 parameters:
1. A `const` reference to an instance of the concrete type that is bound.
2. A unique identifier for this instance or `0` if the value is transient and the same instance can result in different values.
3. A `const` pointer-to-pointer where we will provide the framework with our data.
4. The `size` of our data.

Note that we always check if `src` is not `nullptr`. This is because the rendering code could ask for the `size` only, without having to provide the data.

This is all we need to provide to the generated code for our example. Generally speaking, whenever an `atom` is passed as a pipeline argument, a template must be specialized to provide the underlying data from the atom to the renderer generated code. This example does not attempt to abstract away any detail, so that the actual process becomes explicit and the reader becomes familiar with it. This abstraction is very important since it allows the usage of essentially any asset format as long as a specialization is provided to extract the required information from it, and it allows using the same generated code in multiple applications with completely different concrete types of asset formats.

Finally, note that we skipped the `Draw_indices_` specialization. For each graphics pipeline, an `_indices_` specialization is always generated since the index values are not an explicit input to the pipeline. By setting the `index_count` to `0`, we are indicating that we don't really want to use indices, and rather do an non-indexed draw. Also note that the call operator signature for the index specialization is slightly different that the input specializations we saw above: in particular, it takes an additional parameter of type [VkIndexType](https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkIndexType.html) indicating the type of the index elements.

### Putting It All Together

The rest of the example code is very similar to the previous example:
```cpp
int main() {
  const zrl::Config config{/* app_name */ "hellotriangle",
                           /* engine_name */ "zrl",
                           /* width */ 800,
                           /* height */ 600,
                           /* fullscreen*/ false,
                           /* debug*/ true};
  zrl::Core core(config);
  Main renderer(core);

  while (!glfwWindowShouldClose(core.GetWindow())) {
    glfwPollEvents();
    renderer.Render(triangle);
  }

  return 0;
}
```

The only difference is that now we are actually creating a renderer named after our ZRL renderer:
```
  Main renderer(core);
```
and we are calling the `Render` function in a render loop:
```
    renderer.Render(triangle);
```
using our global `triangle` instance. This is only possible because we provided the specializations above; try removing one of them and you will get a compilation error indicating that the required template has not been specialized.

## Wrapping Up

Hopefully, this example gave you a rough idea of what ZRL looks like. Here are some simple ideas you can try, building upon the current example:

* draw more than one triangle.
* make the colors change gradually.

In the next example, we will see how to use uniforms and textures in our pipeline.

## Reference

### ZRL Types Used in this Section

- `fvec3`: 3-component vector of single-precision floating-point numbers.
- `fvec4`: 4-component vector of single-precision floating-point numbers.
- `atom`: abtract interface type that can be bound to pipeline parameters.

### Specialization Signatures

Signature for pipeline stage parameters:

```cpp
void operator()(const T &t, uint32_t &uid, void const **src, VkDeviceSize &size) const noexcept;
```

Signature for graphics pipeline indices (optional):

```cpp
void operator()(const T &t, uint32_t &uid, void const **src, VkIndexType &index_type, uint32_t &index_count) const noexcept;
```

Previous: [Development Environment](002_Development_Environment.md) | [Table of Contents](README.md) | Next: [Textured Cube](004_Textured_Cube.md)
