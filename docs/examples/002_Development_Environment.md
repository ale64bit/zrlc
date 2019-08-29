Previous: [Introduction](001_Introduction.md) | [Table of Contents](README.md) | Next: [Hello Triangle](003_Hello_Triangle.md)

# Development Environment

The source code for all examples can be found in the [zrl](https://github.com/ale64bit/zrl) repository. This repo also contains the ZRL support library. Let's clone it:

```
git clone https://github.com/ale64bit/zrl.git
cd zrl
```

The package `core` is the ZRL support library: every package produced by ZRL will depend on `//core`. There's nothing special about it; it's just a bunch of wrappers around Vulkan basic API.

## Dependencies

Before you can run any of the examples, you must install the needed dependencies:

### Windows

First you need to download and install the [Vulkan SDK](https://vulkan.lunarg.com/sdk/home#windows). Then, you need to set the `VULKAN_SDK_PATH` environment variable to point to the directory where the SDK was installed. For example, using PowerShell:

```
$Env:VULKAN_SDK_PATH = "C:\VulkanSDK\1.1.114.0"
```

Also, you need to download and extract [GLFW](https://github.com/glfw/glfw/releases/download/3.3/glfw-3.3.bin.WIN64.zip) and set the `GLFW_PATH` environment variable to point to the directory where the archive was uncompressed. For example, using PowerShell:

```
$Env:GLFW_PATH = "C:\Path\To\glfw-3.3.bin.WIN64"
```

### Linux

First you need to download and extract the [Vulkan SDK](https://vulkan.lunarg.com/sdk/home#linux). Then, you need to set the `VULKAN_SDK_PATH` environment variable to point to the directory where the SDK was extracted. For example:

```
export VULKAN_SDK_PATH=/home/username/vulkan-sdk/1.1.114.0
```

Also, you need to install [GLFW](https://www.glfw.org/download.html) or [build it from source](https://www.glfw.org/docs/latest/compile.html) if your package manager doesn't provide a pre-compiled package. Then, you need to set the `GLFW_PATH` environment variable to point to the directory where GLFW was installed. For example:

```
export GLFW_PATH=/usr/local
```

If you are on Ubuntu, you can probably install it with:

```
sudo apt-get install libglfw3 libglfw3-dev
```

## Minimal Example

Now, let's walk over the "empty" example in [examples/empty](https://github.com/ale4bit/zrl/blob/master/examples/empty):

[BUILD](https://github.com/ale64bit/zrl/blob/master/examples/empty/BULID):
```
load("//core:builddefs.bzl", "COPTS", "DEFINES")
load("//core:zrl_library.bzl", "zrl_library")

zrl_library(
    name = "empty",
    src = "empty.zrl",
)

cc_binary(
    name = "main",
    srcs = ["main.cc"],
    copts = COPTS,
    defines = DEFINES,
    deps = [
        ":empty",
        "//core",
        "@glm",
        "@vulkan_repo//:sdk",
    ],
)
```

The `BUILD` file contains two targets: `empty` and `main`. The first one is generated using [zrl_library](https://github.com/ale64bit/zrl/blob/master/core/zrl_library.bzl); this is a rule provided by the ZRL support library that produces a C++ library from a source ZRL file. The second one is a standard `cc_binary` rule that depends on the generated code. This is the common pattern we will use across most examples: one rule to produce our renderer from a ZRL file and one rule that uses the renderer in a traditional C++ application.

Note that `main` also depends on `//core` (the ZRL support library), `@glm` (the math library) and `@vulkan_repo//:sdk`. Additionally, we import `COPTS` and `DEFINES`, which are the compilation options and `#define`s required for ZRL applications.

[empty.zrl](https://github.com/ale64bit/zrl/blob/master/examples/empty/empty.zrl):
```
module empty
```

The actual ZRL program. In this case, an empty program. Even empty programs must contain a `module` declaration, though. We will see an actual program in the next example.

[main.cc](https://github.com/ale64bit/zrl/blob/master/examples/empty/main.cc):
```cpp
#include "core/Core.h"
#include "core/Log.h"

int main() {
  const zrl::Config config{/* app_name */ "empty",
                           /* engine_name */ "zrl",
                           /* width */ 800,
                           /* height */ 600,
                           /* fullscreen*/ false,
                           /* debug*/ true};
  zrl::Core core(config);
  LOG(INFO) << "Ready!" << std::endl;
  return 0;
}
```

This example doesn't really use ZRL at all, but it uses the ZRL support library and thus it's a good way to verify your development environment is correctly setup.

Here we first include the headers for the ZRL support library. Then we create a `zrl::Config` object with the desired configuration, including the app and engine name, screen resolution and fullscreen mode. Finally, we create the actual `zrl::Core` object using that config and we log a message to verify that everything is working correctly; this object encapsulates common behavior for all Vulkan applications, such as selecting the physical device and creating the Vulkan instance, but it doesn't go much far from that.

Try to run the example:

```
bazel run //examples/empty:main
```

If you can see the `Ready!` log message, you are all set. 

Previous: [Introduction](001_Introduction.md) | [Table of Contents](README.md) | Next: [Hello Triangle](003_Hello_Triangle.md)

