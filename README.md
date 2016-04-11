Ada Vulkan
==========

This is an Ada 2012 binding to the new [Vulkan API](https://www.khronos.org/registry/vulkan).

Building
========

There are a number of variables which can be set to control the compilation:

VULKAN_PLATFORM = Can be set to one of the following values: linux, windows, macosx, ios or android. Defaults to linux.
VULKAN_MODE     = Can be one of: debug or release. Defaults to debug.
VULKAN_BUILD    = Can be one of: static or shared. Defaults to a statically built library.
VULKAN_BITS     = Can be one of: 32 or 64. Defaults to 64.
VULKAN_WINAPI   = Can be one of: none, real. Defaults to none, this will include local packages to provide extra types
                  for the windowing API such that the programmer can access window handles on various OSes. If real is
                  selected, then there must already be bindings to the various windowing API's available on the
				  development machine.

```
cd build/gnat
make VULKAN_PLATFORM=linux VULKAN_MODE=release VULKAN_BUILD=static VULKAN_WINAPI=real VULKAN_BITS=64
```

[Current version](http://www.semver.org)
========================================

0.0.1

Installation
============

```
make VULKAN_PLATFORM=linux VULKAN_MODE=release VULKAN_BUILD=static VULKAN_WINAPI=real VULKAN_BITS=64 DESTDIR=/myprefix install
```

Copyright
=========

Copyright (C) 2016 by Luke A. Guest

Roadmap
=======

1) Quick binding generation using g++ -fdump-ada-spec-slim
  * Massage API by hand.
2) Experiment with the API to work out how best to generate a full thick binding.
  * given that Vulkan was designed for multithreaded use, it is a natural fit for Ada's tasking abilities.
3) Extend Vulkan's registry python scripts to dump out the finalised binding.
  * Given that Vulkan will be extended over time, we don't want to be in the same situation we have been in
    with OpenGL bindings, i.e. stuck on 1.5-2.0 for years whilst everyone else is using 3-4.x. So we extend their
    generator such that new Ada bindings can be generated when a new vk.xml file is released.
4) Provide an Ada specific Vulkan SDK, possibly porting some of the C/C++ demos and showing framerates and other
   statistics to show Ada is a good fit for this area of software development.
5) Ada 2012 based shader language -> [SPIR-V](https://github.com/Lucretia/adaspir-v)

XCB bindings
============

[Jacob Sparre Andersen](https://bitbucket.org/sparre/multithreading-made-easy/src/4bf747ac5941cd4cc668d77794bfc5075e680984/xcb/?at=default)
