Previous: [Introduction](001_Introduction.md) | [Table of Contents](README.md) | Next: [Hello Triangle](003_Hello_Triangle.md)

# Development Environment

First, let's create a basic bazel project with the minimal configuration we need to walk through the tutorial:

```
git clone https://github.com/ale64bit/zrl.git
cd zrl
ls -l
```

The package `core` is the ZRL support library: every package produced by ZRL will depend on `//core`. There's nothing special about it; it's just a bunch of wrappers around Vulkan basic API. Note that the `core` package depends on [GLFW](https://www.glfw.org/) so you will need to build or install that in your system before you can run the example. If you are running Ubuntu, you can do:

```
sudo apt-get install libglfw3 libglfw3-dev
```

Now, let's walk over the basic example code in [tutorial/part2/main.cc](https://github.com/ale64bit/zrl/blob/master/tutorial/part2/main.cc):

```
#include "core/Core.h"
#include "core/Log.h"

int main() {
  const zrl::Config config{/* app_name */ "part2",
                           /* engine_name */ "zrl",
                           /* width */ 1920,
                           /* height */ 1080,
                           /* fullscreen*/ false,
                           /* debug*/ true};
  zrl::Core core(config);
  LOG(INFO) << "Ready!" << std::endl;
  return 0;
}
```

Here we first include the headers for the ZRL support library. Then we create a `zrl::Config` object with the desired configuration, including the app and engine name, screen resolution and fullscreen mode. Finally, we create the actual `zrl::Core` object using that config and we log a message to verify that everything is working correctly; this object encapsulates common behavior for all Vulkan applications, such as selecting the physical device and creating the Vulkan instance, but it doesn't go much far from that.

Try to run the example:

```
bazel run //tutorial/part2:main
```

If you can see the `Ready!` log message, you are all set.

Previous: [Introduction](001_Introduction.md) | [Table of Contents](README.md) | Next: [Hello Triangle](003_Hello_Triangle.md)

