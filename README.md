Ada Vulkan
==========

This is an Ada 2012 binding to the new [Vulkan API](https://www.khronos.org/registry/vulkan).

Building
========

There are a number of variables which can be set to control the compilation:

VULKAN_PLATFORM = Can be set to one of the following values: linux, windows, macosx, ios or android
VULKAN_MODE     = Can be one of: debug or release. Defaults to debug.
VULKAN_BUILD    = Can be one of: static or shared. Defaults to a statically built library.

```
cd build/gnat
make VULKAN_PLATFORM=linux VULKAN_BUILD=static VULKAN_MODE=release
```

[Current version](http://www.semver.org)
========================================

0.0.1

Installation
============

```
make VULKAN_PLATFORM=linux VULKAN_BUILD=static VULKAN_MODE=release DESTDIR=/myprefix install
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
