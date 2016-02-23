--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 201 Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
--  Vulkan
--
--  Ada 2012 bindings to the Vulkan library.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C; use Interfaces.C;
with stdint_h;
with System;
with stddef_h;
with Interfaces.C.Strings;

package Vulkan is
   type Versions is
      record
         Major : Int;
         Minor : Int;
         Patch : Int;
      end record;

   for Versions use
      record
         Major at 0 range 31 .. 22;
         Minor at 0 range 21 .. 12;
         Patch at 0 range 11 ..  0;
      end record;

   API_Version : constant Version := (Major => 1, Minor => 0, Patch => 3);

   --  unsupported macro: VK_VERSION_1_0 1
   --  arg-macro: function VK_MAKE_VERSION (((major) << 22) or ((minor) << 12) or (patch)
   --    return ((major) << 22) or ((minor) << 12) or (patch);
   --  unsupported macro: VK_API_VERSION VK_MAKE_VERSION(1, 0, 3)
   --  arg-macro: function VK_VERSION_MAJOR ((uint32_t)(version) >> 22
   --    return (uint32_t)(version) >> 22;
   --  arg-macro: function VK_VERSION_MINOR (((uint32_t)(version) >> 12) and 0x3ff
   --    return ((uint32_t)(version) >> 12) and 0x3ff;
   --  arg-macro: function VK_VERSION_PATCH ((uint32_t)(version) and 0xfff
   --    return (uint32_t)(version) and 0xfff;
   --  unsupported macro: VK_NULL_HANDLE 0
   --  unsupported macro: VK_DEFINE_HANDLE(object) typedef struct object ##_T* object;
   --  unsupported macro: VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef struct object ##_T *object;
   --  unsupported macro: VK_LOD_CLAMP_NONE 1000.0f
   --  unsupported macro: VK_REMAINING_MIP_LEVELS (~0U)
   --  unsupported macro: VK_REMAINING_ARRAY_LAYERS (~0U)
   --  unsupported macro: VK_WHOLE_SIZE (~0ULL)
   --  unsupported macro: VK_ATTACHMENT_UNUSED (~0U)
   --  unsupported macro: VK_TRUE 1
   --  unsupported macro: VK_FALSE 0
   --  unsupported macro: VK_QUEUE_FAMILY_IGNORED (~0U)
   --  unsupported macro: VK_SUBPASS_EXTERNAL (~0U)
   --  unsupported macro: VK_MAX_PHYSICAL_DEVICE_NAME_SIZE 256
   --  unsupported macro: VK_UUID_SIZE 16
   --  unsupported macro: VK_MAX_MEMORY_TYPES 32
   --  unsupported macro: VK_MAX_MEMORY_HEAPS 16
   --  unsupported macro: VK_MAX_EXTENSION_NAME_SIZE 256
   --  unsupported macro: VK_MAX_DESCRIPTION_SIZE 256
   --  unsupported macro: VK_KHR_surface 1
   --  unsupported macro: VK_KHR_SURFACE_SPEC_VERSION 25
   --  unsupported macro: VK_KHR_SURFACE_EXTENSION_NAME "VK_KHR_surface"
   --  unsupported macro: VK_KHR_swapchain 1
   --  unsupported macro: VK_KHR_SWAPCHAIN_SPEC_VERSION 67
   --  unsupported macro: VK_KHR_SWAPCHAIN_EXTENSION_NAME "VK_KHR_swapchain"
   --  unsupported macro: VK_KHR_display 1
   --  unsupported macro: VK_KHR_DISPLAY_SPEC_VERSION 21
   --  unsupported macro: VK_KHR_DISPLAY_EXTENSION_NAME "VK_KHR_display"
   --  unsupported macro: VK_KHR_display_swapchain 1
   --  unsupported macro: VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION 9
   --  unsupported macro: VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME "VK_KHR_display_swapchain"
   --  unsupported macro: VK_EXT_debug_report 1
   --  unsupported macro: VK_EXT_DEBUG_REPORT_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_DEBUG_REPORT_EXTENSION_NAME "VK_EXT_debug_report"
  --** Copyright (c) 2015-2016 The Khronos Group Inc.
  --**
  --** Permission is hereby granted, free of charge, to any person obtaining a
  --** copy of this software and/or associated documentation files (the
  --** "Materials"), to deal in the Materials without restriction, including
  --** without limitation the rights to use, copy, modify, merge, publish,
  --** distribute, sublicense, and/or sell copies of the Materials, and to
  --** permit persons to whom the Materials are furnished to do so, subject to
  --** the following conditions:
  --**
  --** The above copyright notice and this permission notice shall be included
  --** in all copies or substantial portions of the Materials.
  --**
  --** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  --** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  --** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  --** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  --** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  --** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  --** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
  --

  --** This header is generated from the Khronos Vulkan XML API Registry.
  --**
  --

  -- Vulkan API version supported by this file
   subtype VkFlags is stdint_h.uint32_t;  -- src/vulkan/vulkan.h:65

   subtype VkBool32 is stdint_h.uint32_t;  -- src/vulkan/vulkan.h:66

   subtype VkDeviceSize is stdint_h.uint64_t;  -- src/vulkan/vulkan.h:67

   subtype VkSampleMask is stdint_h.uint32_t;  -- src/vulkan/vulkan.h:68

   type VkInstance is new System.Address;  -- src/vulkan/vulkan.h:70

   --  skipped empty struct VkInstance_T

   type VkPhysicalDevice is new System.Address;  -- src/vulkan/vulkan.h:71

   --  skipped empty struct VkPhysicalDevice_T

   type VkDevice is new System.Address;  -- src/vulkan/vulkan.h:72

   --  skipped empty struct VkDevice_T

   type VkQueue is new System.Address;  -- src/vulkan/vulkan.h:73

   --  skipped empty struct VkQueue_T

   type VkSemaphore is new System.Address;  -- src/vulkan/vulkan.h:74

   --  skipped empty struct VkSemaphore_T

   type VkCommandBuffer is new System.Address;  -- src/vulkan/vulkan.h:75

   --  skipped empty struct VkCommandBuffer_T

   type VkFence is new System.Address;  -- src/vulkan/vulkan.h:76

   --  skipped empty struct VkFence_T

   type VkDeviceMemory is new System.Address;  -- src/vulkan/vulkan.h:77

   --  skipped empty struct VkDeviceMemory_T

   type VkBuffer is new System.Address;  -- src/vulkan/vulkan.h:78

   --  skipped empty struct VkBuffer_T

   type VkImage is new System.Address;  -- src/vulkan/vulkan.h:79

   --  skipped empty struct VkImage_T

   type VkEvent is new System.Address;  -- src/vulkan/vulkan.h:80

   --  skipped empty struct VkEvent_T

   type VkQueryPool is new System.Address;  -- src/vulkan/vulkan.h:81

   --  skipped empty struct VkQueryPool_T

   type VkBufferView is new System.Address;  -- src/vulkan/vulkan.h:82

   --  skipped empty struct VkBufferView_T

   type VkImageView is new System.Address;  -- src/vulkan/vulkan.h:83

   --  skipped empty struct VkImageView_T

   type VkShaderModule is new System.Address;  -- src/vulkan/vulkan.h:84

   --  skipped empty struct VkShaderModule_T

   type VkPipelineCache is new System.Address;  -- src/vulkan/vulkan.h:85

   --  skipped empty struct VkPipelineCache_T

   type VkPipelineLayout is new System.Address;  -- src/vulkan/vulkan.h:86

   --  skipped empty struct VkPipelineLayout_T

   type VkRenderPass is new System.Address;  -- src/vulkan/vulkan.h:87

   --  skipped empty struct VkRenderPass_T

   type VkPipeline is new System.Address;  -- src/vulkan/vulkan.h:88

   --  skipped empty struct VkPipeline_T

   type VkDescriptorSetLayout is new System.Address;  -- src/vulkan/vulkan.h:89

   --  skipped empty struct VkDescriptorSetLayout_T

   type VkSampler is new System.Address;  -- src/vulkan/vulkan.h:90

   --  skipped empty struct VkSampler_T

   type VkDescriptorPool is new System.Address;  -- src/vulkan/vulkan.h:91

   --  skipped empty struct VkDescriptorPool_T

   type VkDescriptorSet is new System.Address;  -- src/vulkan/vulkan.h:92

   --  skipped empty struct VkDescriptorSet_T

   type VkFramebuffer is new System.Address;  -- src/vulkan/vulkan.h:93

   --  skipped empty struct VkFramebuffer_T

   type VkCommandPool is new System.Address;  -- src/vulkan/vulkan.h:94

   --  skipped empty struct VkCommandPool_T

   subtype VkPipelineCacheHeaderVersion is unsigned;
   VK_PIPELINE_CACHE_HEADER_VERSION_ONE : constant VkPipelineCacheHeaderVersion := 1;
   VK_PIPELINE_CACHE_HEADER_VERSION_BEGIN_RANGE : constant VkPipelineCacheHeaderVersion := 1;
   VK_PIPELINE_CACHE_HEADER_VERSION_END_RANGE : constant VkPipelineCacheHeaderVersion := 1;
   VK_PIPELINE_CACHE_HEADER_VERSION_RANGE_SIZE : constant VkPipelineCacheHeaderVersion := 1;
   VK_PIPELINE_CACHE_HEADER_VERSION_MAX_ENUM : constant VkPipelineCacheHeaderVersion := 2147483647;  -- src/vulkan/vulkan.h:113

   subtype VkResult is unsigned;
   VK_SUCCESS : constant VkResult := 0;
   VK_NOT_READY : constant VkResult := 1;
   VK_TIMEOUT : constant VkResult := 2;
   VK_EVENT_SET : constant VkResult := 3;
   VK_EVENT_RESET : constant VkResult := 4;
   VK_INCOMPLETE : constant VkResult := 5;
   VK_ERROR_OUT_OF_HOST_MEMORY : constant VkResult := -1;
   VK_ERROR_OUT_OF_DEVICE_MEMORY : constant VkResult := -2;
   VK_ERROR_INITIALIZATION_FAILED : constant VkResult := -3;
   VK_ERROR_DEVICE_LOST : constant VkResult := -4;
   VK_ERROR_MEMORY_MAP_FAILED : constant VkResult := -5;
   VK_ERROR_LAYER_NOT_PRESENT : constant VkResult := -6;
   VK_ERROR_EXTENSION_NOT_PRESENT : constant VkResult := -7;
   VK_ERROR_FEATURE_NOT_PRESENT : constant VkResult := -8;
   VK_ERROR_INCOMPATIBLE_DRIVER : constant VkResult := -9;
   VK_ERROR_TOO_MANY_OBJECTS : constant VkResult := -10;
   VK_ERROR_FORMAT_NOT_SUPPORTED : constant VkResult := -11;
   VK_ERROR_SURFACE_LOST_KHR : constant VkResult := -1000000000;
   VK_ERROR_NATIVE_WINDOW_IN_USE_KHR : constant VkResult := -1000000001;
   VK_SUBOPTIMAL_KHR : constant VkResult := 1000001003;
   VK_ERROR_OUT_OF_DATE_KHR : constant VkResult := -1000001004;
   VK_ERROR_INCOMPATIBLE_DISPLAY_KHR : constant VkResult := -1000003001;
   VK_ERROR_VALIDATION_FAILED_EXT : constant VkResult := -1000011001;
   VK_RESULT_BEGIN_RANGE : constant VkResult := -11;
   VK_RESULT_END_RANGE : constant VkResult := 5;
   VK_RESULT_RANGE_SIZE : constant VkResult := 17;
   VK_RESULT_MAX_ENUM : constant VkResult := 2147483647;  -- src/vulkan/vulkan.h:121

   subtype VkStructureType is unsigned;
   VK_STRUCTURE_TYPE_APPLICATION_INFO : constant VkStructureType := 0;
   VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO : constant VkStructureType := 1;
   VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO : constant VkStructureType := 2;
   VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO : constant VkStructureType := 3;
   VK_STRUCTURE_TYPE_SUBMIT_INFO : constant VkStructureType := 4;
   VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO : constant VkStructureType := 5;
   VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE : constant VkStructureType := 6;
   VK_STRUCTURE_TYPE_BIND_SPARSE_INFO : constant VkStructureType := 7;
   VK_STRUCTURE_TYPE_FENCE_CREATE_INFO : constant VkStructureType := 8;
   VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO : constant VkStructureType := 9;
   VK_STRUCTURE_TYPE_EVENT_CREATE_INFO : constant VkStructureType := 10;
   VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO : constant VkStructureType := 11;
   VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO : constant VkStructureType := 12;
   VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO : constant VkStructureType := 13;
   VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO : constant VkStructureType := 14;
   VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO : constant VkStructureType := 15;
   VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO : constant VkStructureType := 16;
   VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO : constant VkStructureType := 17;
   VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO : constant VkStructureType := 18;
   VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO : constant VkStructureType := 19;
   VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO : constant VkStructureType := 20;
   VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO : constant VkStructureType := 21;
   VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO : constant VkStructureType := 22;
   VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO : constant VkStructureType := 23;
   VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO : constant VkStructureType := 24;
   VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO : constant VkStructureType := 25;
   VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO : constant VkStructureType := 26;
   VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO : constant VkStructureType := 27;
   VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO : constant VkStructureType := 28;
   VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO : constant VkStructureType := 29;
   VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO : constant VkStructureType := 30;
   VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO : constant VkStructureType := 31;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO : constant VkStructureType := 32;
   VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO : constant VkStructureType := 33;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO : constant VkStructureType := 34;
   VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET : constant VkStructureType := 35;
   VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET : constant VkStructureType := 36;
   VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO : constant VkStructureType := 37;
   VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO : constant VkStructureType := 38;
   VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO : constant VkStructureType := 39;
   VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO : constant VkStructureType := 40;
   VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO : constant VkStructureType := 41;
   VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO : constant VkStructureType := 42;
   VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO : constant VkStructureType := 43;
   VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER : constant VkStructureType := 44;
   VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER : constant VkStructureType := 45;
   VK_STRUCTURE_TYPE_MEMORY_BARRIER : constant VkStructureType := 46;
   VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO : constant VkStructureType := 47;
   VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO : constant VkStructureType := 48;
   VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR : constant VkStructureType := 1000001000;
   VK_STRUCTURE_TYPE_PRESENT_INFO_KHR : constant VkStructureType := 1000001001;
   VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR : constant VkStructureType := 1000002000;
   VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000002001;
   VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR : constant VkStructureType := 1000003000;
   VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000004000;
   VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000005000;
   VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000006000;
   VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000007000;
   VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000008000;
   VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000009000;
   VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT : constant VkStructureType := 1000011000;
   VK_STRUCTURE_TYPE_BEGIN_RANGE : constant VkStructureType := 0;
   VK_STRUCTURE_TYPE_END_RANGE : constant VkStructureType := 48;
   VK_STRUCTURE_TYPE_RANGE_SIZE : constant VkStructureType := 49;
   VK_STRUCTURE_TYPE_MAX_ENUM : constant VkStructureType := 2147483647;  -- src/vulkan/vulkan.h:151

   subtype VkSystemAllocationScope is unsigned;
   VK_SYSTEM_ALLOCATION_SCOPE_COMMAND : constant VkSystemAllocationScope := 0;
   VK_SYSTEM_ALLOCATION_SCOPE_OBJECT : constant VkSystemAllocationScope := 1;
   VK_SYSTEM_ALLOCATION_SCOPE_CACHE : constant VkSystemAllocationScope := 2;
   VK_SYSTEM_ALLOCATION_SCOPE_DEVICE : constant VkSystemAllocationScope := 3;
   VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE : constant VkSystemAllocationScope := 4;
   VK_SYSTEM_ALLOCATION_SCOPE_BEGIN_RANGE : constant VkSystemAllocationScope := 0;
   VK_SYSTEM_ALLOCATION_SCOPE_END_RANGE : constant VkSystemAllocationScope := 4;
   VK_SYSTEM_ALLOCATION_SCOPE_RANGE_SIZE : constant VkSystemAllocationScope := 5;
   VK_SYSTEM_ALLOCATION_SCOPE_MAX_ENUM : constant VkSystemAllocationScope := 2147483647;  -- src/vulkan/vulkan.h:219

   subtype VkInternalAllocationType is unsigned;
   VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE : constant VkInternalAllocationType := 0;
   VK_INTERNAL_ALLOCATION_TYPE_BEGIN_RANGE : constant VkInternalAllocationType := 0;
   VK_INTERNAL_ALLOCATION_TYPE_END_RANGE : constant VkInternalAllocationType := 0;
   VK_INTERNAL_ALLOCATION_TYPE_RANGE_SIZE : constant VkInternalAllocationType := 1;
   VK_INTERNAL_ALLOCATION_TYPE_MAX_ENUM : constant VkInternalAllocationType := 2147483647;  -- src/vulkan/vulkan.h:231

   subtype VkFormat is unsigned;
   VK_FORMAT_UNDEFINED : constant VkFormat := 0;
   VK_FORMAT_R4G4_UNORM_PACK8 : constant VkFormat := 1;
   VK_FORMAT_R4G4B4A4_UNORM_PACK16 : constant VkFormat := 2;
   VK_FORMAT_B4G4R4A4_UNORM_PACK16 : constant VkFormat := 3;
   VK_FORMAT_R5G6B5_UNORM_PACK16 : constant VkFormat := 4;
   VK_FORMAT_B5G6R5_UNORM_PACK16 : constant VkFormat := 5;
   VK_FORMAT_R5G5B5A1_UNORM_PACK16 : constant VkFormat := 6;
   VK_FORMAT_B5G5R5A1_UNORM_PACK16 : constant VkFormat := 7;
   VK_FORMAT_A1R5G5B5_UNORM_PACK16 : constant VkFormat := 8;
   VK_FORMAT_R8_UNORM : constant VkFormat := 9;
   VK_FORMAT_R8_SNORM : constant VkFormat := 10;
   VK_FORMAT_R8_USCALED : constant VkFormat := 11;
   VK_FORMAT_R8_SSCALED : constant VkFormat := 12;
   VK_FORMAT_R8_UINT : constant VkFormat := 13;
   VK_FORMAT_R8_SINT : constant VkFormat := 14;
   VK_FORMAT_R8_SRGB : constant VkFormat := 15;
   VK_FORMAT_R8G8_UNORM : constant VkFormat := 16;
   VK_FORMAT_R8G8_SNORM : constant VkFormat := 17;
   VK_FORMAT_R8G8_USCALED : constant VkFormat := 18;
   VK_FORMAT_R8G8_SSCALED : constant VkFormat := 19;
   VK_FORMAT_R8G8_UINT : constant VkFormat := 20;
   VK_FORMAT_R8G8_SINT : constant VkFormat := 21;
   VK_FORMAT_R8G8_SRGB : constant VkFormat := 22;
   VK_FORMAT_R8G8B8_UNORM : constant VkFormat := 23;
   VK_FORMAT_R8G8B8_SNORM : constant VkFormat := 24;
   VK_FORMAT_R8G8B8_USCALED : constant VkFormat := 25;
   VK_FORMAT_R8G8B8_SSCALED : constant VkFormat := 26;
   VK_FORMAT_R8G8B8_UINT : constant VkFormat := 27;
   VK_FORMAT_R8G8B8_SINT : constant VkFormat := 28;
   VK_FORMAT_R8G8B8_SRGB : constant VkFormat := 29;
   VK_FORMAT_B8G8R8_UNORM : constant VkFormat := 30;
   VK_FORMAT_B8G8R8_SNORM : constant VkFormat := 31;
   VK_FORMAT_B8G8R8_USCALED : constant VkFormat := 32;
   VK_FORMAT_B8G8R8_SSCALED : constant VkFormat := 33;
   VK_FORMAT_B8G8R8_UINT : constant VkFormat := 34;
   VK_FORMAT_B8G8R8_SINT : constant VkFormat := 35;
   VK_FORMAT_B8G8R8_SRGB : constant VkFormat := 36;
   VK_FORMAT_R8G8B8A8_UNORM : constant VkFormat := 37;
   VK_FORMAT_R8G8B8A8_SNORM : constant VkFormat := 38;
   VK_FORMAT_R8G8B8A8_USCALED : constant VkFormat := 39;
   VK_FORMAT_R8G8B8A8_SSCALED : constant VkFormat := 40;
   VK_FORMAT_R8G8B8A8_UINT : constant VkFormat := 41;
   VK_FORMAT_R8G8B8A8_SINT : constant VkFormat := 42;
   VK_FORMAT_R8G8B8A8_SRGB : constant VkFormat := 43;
   VK_FORMAT_B8G8R8A8_UNORM : constant VkFormat := 44;
   VK_FORMAT_B8G8R8A8_SNORM : constant VkFormat := 45;
   VK_FORMAT_B8G8R8A8_USCALED : constant VkFormat := 46;
   VK_FORMAT_B8G8R8A8_SSCALED : constant VkFormat := 47;
   VK_FORMAT_B8G8R8A8_UINT : constant VkFormat := 48;
   VK_FORMAT_B8G8R8A8_SINT : constant VkFormat := 49;
   VK_FORMAT_B8G8R8A8_SRGB : constant VkFormat := 50;
   VK_FORMAT_A8B8G8R8_UNORM_PACK32 : constant VkFormat := 51;
   VK_FORMAT_A8B8G8R8_SNORM_PACK32 : constant VkFormat := 52;
   VK_FORMAT_A8B8G8R8_USCALED_PACK32 : constant VkFormat := 53;
   VK_FORMAT_A8B8G8R8_SSCALED_PACK32 : constant VkFormat := 54;
   VK_FORMAT_A8B8G8R8_UINT_PACK32 : constant VkFormat := 55;
   VK_FORMAT_A8B8G8R8_SINT_PACK32 : constant VkFormat := 56;
   VK_FORMAT_A8B8G8R8_SRGB_PACK32 : constant VkFormat := 57;
   VK_FORMAT_A2R10G10B10_UNORM_PACK32 : constant VkFormat := 58;
   VK_FORMAT_A2R10G10B10_SNORM_PACK32 : constant VkFormat := 59;
   VK_FORMAT_A2R10G10B10_USCALED_PACK32 : constant VkFormat := 60;
   VK_FORMAT_A2R10G10B10_SSCALED_PACK32 : constant VkFormat := 61;
   VK_FORMAT_A2R10G10B10_UINT_PACK32 : constant VkFormat := 62;
   VK_FORMAT_A2R10G10B10_SINT_PACK32 : constant VkFormat := 63;
   VK_FORMAT_A2B10G10R10_UNORM_PACK32 : constant VkFormat := 64;
   VK_FORMAT_A2B10G10R10_SNORM_PACK32 : constant VkFormat := 65;
   VK_FORMAT_A2B10G10R10_USCALED_PACK32 : constant VkFormat := 66;
   VK_FORMAT_A2B10G10R10_SSCALED_PACK32 : constant VkFormat := 67;
   VK_FORMAT_A2B10G10R10_UINT_PACK32 : constant VkFormat := 68;
   VK_FORMAT_A2B10G10R10_SINT_PACK32 : constant VkFormat := 69;
   VK_FORMAT_R16_UNORM : constant VkFormat := 70;
   VK_FORMAT_R16_SNORM : constant VkFormat := 71;
   VK_FORMAT_R16_USCALED : constant VkFormat := 72;
   VK_FORMAT_R16_SSCALED : constant VkFormat := 73;
   VK_FORMAT_R16_UINT : constant VkFormat := 74;
   VK_FORMAT_R16_SINT : constant VkFormat := 75;
   VK_FORMAT_R16_SFLOAT : constant VkFormat := 76;
   VK_FORMAT_R16G16_UNORM : constant VkFormat := 77;
   VK_FORMAT_R16G16_SNORM : constant VkFormat := 78;
   VK_FORMAT_R16G16_USCALED : constant VkFormat := 79;
   VK_FORMAT_R16G16_SSCALED : constant VkFormat := 80;
   VK_FORMAT_R16G16_UINT : constant VkFormat := 81;
   VK_FORMAT_R16G16_SINT : constant VkFormat := 82;
   VK_FORMAT_R16G16_SFLOAT : constant VkFormat := 83;
   VK_FORMAT_R16G16B16_UNORM : constant VkFormat := 84;
   VK_FORMAT_R16G16B16_SNORM : constant VkFormat := 85;
   VK_FORMAT_R16G16B16_USCALED : constant VkFormat := 86;
   VK_FORMAT_R16G16B16_SSCALED : constant VkFormat := 87;
   VK_FORMAT_R16G16B16_UINT : constant VkFormat := 88;
   VK_FORMAT_R16G16B16_SINT : constant VkFormat := 89;
   VK_FORMAT_R16G16B16_SFLOAT : constant VkFormat := 90;
   VK_FORMAT_R16G16B16A16_UNORM : constant VkFormat := 91;
   VK_FORMAT_R16G16B16A16_SNORM : constant VkFormat := 92;
   VK_FORMAT_R16G16B16A16_USCALED : constant VkFormat := 93;
   VK_FORMAT_R16G16B16A16_SSCALED : constant VkFormat := 94;
   VK_FORMAT_R16G16B16A16_UINT : constant VkFormat := 95;
   VK_FORMAT_R16G16B16A16_SINT : constant VkFormat := 96;
   VK_FORMAT_R16G16B16A16_SFLOAT : constant VkFormat := 97;
   VK_FORMAT_R32_UINT : constant VkFormat := 98;
   VK_FORMAT_R32_SINT : constant VkFormat := 99;
   VK_FORMAT_R32_SFLOAT : constant VkFormat := 100;
   VK_FORMAT_R32G32_UINT : constant VkFormat := 101;
   VK_FORMAT_R32G32_SINT : constant VkFormat := 102;
   VK_FORMAT_R32G32_SFLOAT : constant VkFormat := 103;
   VK_FORMAT_R32G32B32_UINT : constant VkFormat := 104;
   VK_FORMAT_R32G32B32_SINT : constant VkFormat := 105;
   VK_FORMAT_R32G32B32_SFLOAT : constant VkFormat := 106;
   VK_FORMAT_R32G32B32A32_UINT : constant VkFormat := 107;
   VK_FORMAT_R32G32B32A32_SINT : constant VkFormat := 108;
   VK_FORMAT_R32G32B32A32_SFLOAT : constant VkFormat := 109;
   VK_FORMAT_R64_UINT : constant VkFormat := 110;
   VK_FORMAT_R64_SINT : constant VkFormat := 111;
   VK_FORMAT_R64_SFLOAT : constant VkFormat := 112;
   VK_FORMAT_R64G64_UINT : constant VkFormat := 113;
   VK_FORMAT_R64G64_SINT : constant VkFormat := 114;
   VK_FORMAT_R64G64_SFLOAT : constant VkFormat := 115;
   VK_FORMAT_R64G64B64_UINT : constant VkFormat := 116;
   VK_FORMAT_R64G64B64_SINT : constant VkFormat := 117;
   VK_FORMAT_R64G64B64_SFLOAT : constant VkFormat := 118;
   VK_FORMAT_R64G64B64A64_UINT : constant VkFormat := 119;
   VK_FORMAT_R64G64B64A64_SINT : constant VkFormat := 120;
   VK_FORMAT_R64G64B64A64_SFLOAT : constant VkFormat := 121;
   VK_FORMAT_B10G11R11_UFLOAT_PACK32 : constant VkFormat := 122;
   VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 : constant VkFormat := 123;
   VK_FORMAT_D16_UNORM : constant VkFormat := 124;
   VK_FORMAT_X8_D24_UNORM_PACK32 : constant VkFormat := 125;
   VK_FORMAT_D32_SFLOAT : constant VkFormat := 126;
   VK_FORMAT_S8_UINT : constant VkFormat := 127;
   VK_FORMAT_D16_UNORM_S8_UINT : constant VkFormat := 128;
   VK_FORMAT_D24_UNORM_S8_UINT : constant VkFormat := 129;
   VK_FORMAT_D32_SFLOAT_S8_UINT : constant VkFormat := 130;
   VK_FORMAT_BC1_RGB_UNORM_BLOCK : constant VkFormat := 131;
   VK_FORMAT_BC1_RGB_SRGB_BLOCK : constant VkFormat := 132;
   VK_FORMAT_BC1_RGBA_UNORM_BLOCK : constant VkFormat := 133;
   VK_FORMAT_BC1_RGBA_SRGB_BLOCK : constant VkFormat := 134;
   VK_FORMAT_BC2_UNORM_BLOCK : constant VkFormat := 135;
   VK_FORMAT_BC2_SRGB_BLOCK : constant VkFormat := 136;
   VK_FORMAT_BC3_UNORM_BLOCK : constant VkFormat := 137;
   VK_FORMAT_BC3_SRGB_BLOCK : constant VkFormat := 138;
   VK_FORMAT_BC4_UNORM_BLOCK : constant VkFormat := 139;
   VK_FORMAT_BC4_SNORM_BLOCK : constant VkFormat := 140;
   VK_FORMAT_BC5_UNORM_BLOCK : constant VkFormat := 141;
   VK_FORMAT_BC5_SNORM_BLOCK : constant VkFormat := 142;
   VK_FORMAT_BC6H_UFLOAT_BLOCK : constant VkFormat := 143;
   VK_FORMAT_BC6H_SFLOAT_BLOCK : constant VkFormat := 144;
   VK_FORMAT_BC7_UNORM_BLOCK : constant VkFormat := 145;
   VK_FORMAT_BC7_SRGB_BLOCK : constant VkFormat := 146;
   VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK : constant VkFormat := 147;
   VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK : constant VkFormat := 148;
   VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK : constant VkFormat := 149;
   VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK : constant VkFormat := 150;
   VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK : constant VkFormat := 151;
   VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK : constant VkFormat := 152;
   VK_FORMAT_EAC_R11_UNORM_BLOCK : constant VkFormat := 153;
   VK_FORMAT_EAC_R11_SNORM_BLOCK : constant VkFormat := 154;
   VK_FORMAT_EAC_R11G11_UNORM_BLOCK : constant VkFormat := 155;
   VK_FORMAT_EAC_R11G11_SNORM_BLOCK : constant VkFormat := 156;
   VK_FORMAT_ASTC_4x4_UNORM_BLOCK : constant VkFormat := 157;
   VK_FORMAT_ASTC_4x4_SRGB_BLOCK : constant VkFormat := 158;
   VK_FORMAT_ASTC_5x4_UNORM_BLOCK : constant VkFormat := 159;
   VK_FORMAT_ASTC_5x4_SRGB_BLOCK : constant VkFormat := 160;
   VK_FORMAT_ASTC_5x5_UNORM_BLOCK : constant VkFormat := 161;
   VK_FORMAT_ASTC_5x5_SRGB_BLOCK : constant VkFormat := 162;
   VK_FORMAT_ASTC_6x5_UNORM_BLOCK : constant VkFormat := 163;
   VK_FORMAT_ASTC_6x5_SRGB_BLOCK : constant VkFormat := 164;
   VK_FORMAT_ASTC_6x6_UNORM_BLOCK : constant VkFormat := 165;
   VK_FORMAT_ASTC_6x6_SRGB_BLOCK : constant VkFormat := 166;
   VK_FORMAT_ASTC_8x5_UNORM_BLOCK : constant VkFormat := 167;
   VK_FORMAT_ASTC_8x5_SRGB_BLOCK : constant VkFormat := 168;
   VK_FORMAT_ASTC_8x6_UNORM_BLOCK : constant VkFormat := 169;
   VK_FORMAT_ASTC_8x6_SRGB_BLOCK : constant VkFormat := 170;
   VK_FORMAT_ASTC_8x8_UNORM_BLOCK : constant VkFormat := 171;
   VK_FORMAT_ASTC_8x8_SRGB_BLOCK : constant VkFormat := 172;
   VK_FORMAT_ASTC_10x5_UNORM_BLOCK : constant VkFormat := 173;
   VK_FORMAT_ASTC_10x5_SRGB_BLOCK : constant VkFormat := 174;
   VK_FORMAT_ASTC_10x6_UNORM_BLOCK : constant VkFormat := 175;
   VK_FORMAT_ASTC_10x6_SRGB_BLOCK : constant VkFormat := 176;
   VK_FORMAT_ASTC_10x8_UNORM_BLOCK : constant VkFormat := 177;
   VK_FORMAT_ASTC_10x8_SRGB_BLOCK : constant VkFormat := 178;
   VK_FORMAT_ASTC_10x10_UNORM_BLOCK : constant VkFormat := 179;
   VK_FORMAT_ASTC_10x10_SRGB_BLOCK : constant VkFormat := 180;
   VK_FORMAT_ASTC_12x10_UNORM_BLOCK : constant VkFormat := 181;
   VK_FORMAT_ASTC_12x10_SRGB_BLOCK : constant VkFormat := 182;
   VK_FORMAT_ASTC_12x12_UNORM_BLOCK : constant VkFormat := 183;
   VK_FORMAT_ASTC_12x12_SRGB_BLOCK : constant VkFormat := 184;
   VK_FORMAT_BEGIN_RANGE : constant VkFormat := 0;
   VK_FORMAT_END_RANGE : constant VkFormat := 184;
   VK_FORMAT_RANGE_SIZE : constant VkFormat := 185;
   VK_FORMAT_MAX_ENUM : constant VkFormat := 2147483647;  -- src/vulkan/vulkan.h:239

   subtype VkImageType is unsigned;
   VK_IMAGE_TYPE_1D : constant VkImageType := 0;
   VK_IMAGE_TYPE_2D : constant VkImageType := 1;
   VK_IMAGE_TYPE_3D : constant VkImageType := 2;
   VK_IMAGE_TYPE_BEGIN_RANGE : constant VkImageType := 0;
   VK_IMAGE_TYPE_END_RANGE : constant VkImageType := 2;
   VK_IMAGE_TYPE_RANGE_SIZE : constant VkImageType := 3;
   VK_IMAGE_TYPE_MAX_ENUM : constant VkImageType := 2147483647;  -- src/vulkan/vulkan.h:431

   subtype VkImageTiling is unsigned;
   VK_IMAGE_TILING_OPTIMAL : constant VkImageTiling := 0;
   VK_IMAGE_TILING_LINEAR : constant VkImageTiling := 1;
   VK_IMAGE_TILING_BEGIN_RANGE : constant VkImageTiling := 0;
   VK_IMAGE_TILING_END_RANGE : constant VkImageTiling := 1;
   VK_IMAGE_TILING_RANGE_SIZE : constant VkImageTiling := 2;
   VK_IMAGE_TILING_MAX_ENUM : constant VkImageTiling := 2147483647;  -- src/vulkan/vulkan.h:441

   subtype VkPhysicalDeviceType is unsigned;
   VK_PHYSICAL_DEVICE_TYPE_OTHER : constant VkPhysicalDeviceType := 0;
   VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU : constant VkPhysicalDeviceType := 1;
   VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU : constant VkPhysicalDeviceType := 2;
   VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU : constant VkPhysicalDeviceType := 3;
   VK_PHYSICAL_DEVICE_TYPE_CPU : constant VkPhysicalDeviceType := 4;
   VK_PHYSICAL_DEVICE_TYPE_BEGIN_RANGE : constant VkPhysicalDeviceType := 0;
   VK_PHYSICAL_DEVICE_TYPE_END_RANGE : constant VkPhysicalDeviceType := 4;
   VK_PHYSICAL_DEVICE_TYPE_RANGE_SIZE : constant VkPhysicalDeviceType := 5;
   VK_PHYSICAL_DEVICE_TYPE_MAX_ENUM : constant VkPhysicalDeviceType := 2147483647;  -- src/vulkan/vulkan.h:450

   subtype VkQueryType is unsigned;
   VK_QUERY_TYPE_OCCLUSION : constant VkQueryType := 0;
   VK_QUERY_TYPE_PIPELINE_STATISTICS : constant VkQueryType := 1;
   VK_QUERY_TYPE_TIMESTAMP : constant VkQueryType := 2;
   VK_QUERY_TYPE_BEGIN_RANGE : constant VkQueryType := 0;
   VK_QUERY_TYPE_END_RANGE : constant VkQueryType := 2;
   VK_QUERY_TYPE_RANGE_SIZE : constant VkQueryType := 3;
   VK_QUERY_TYPE_MAX_ENUM : constant VkQueryType := 2147483647;  -- src/vulkan/vulkan.h:462

   subtype VkSharingMode is unsigned;
   VK_SHARING_MODE_EXCLUSIVE : constant VkSharingMode := 0;
   VK_SHARING_MODE_CONCURRENT : constant VkSharingMode := 1;
   VK_SHARING_MODE_BEGIN_RANGE : constant VkSharingMode := 0;
   VK_SHARING_MODE_END_RANGE : constant VkSharingMode := 1;
   VK_SHARING_MODE_RANGE_SIZE : constant VkSharingMode := 2;
   VK_SHARING_MODE_MAX_ENUM : constant VkSharingMode := 2147483647;  -- src/vulkan/vulkan.h:472

   subtype VkImageLayout is unsigned;
   VK_IMAGE_LAYOUT_UNDEFINED : constant VkImageLayout := 0;
   VK_IMAGE_LAYOUT_GENERAL : constant VkImageLayout := 1;
   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL : constant VkImageLayout := 2;
   VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL : constant VkImageLayout := 3;
   VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL : constant VkImageLayout := 4;
   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL : constant VkImageLayout := 5;
   VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL : constant VkImageLayout := 6;
   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL : constant VkImageLayout := 7;
   VK_IMAGE_LAYOUT_PREINITIALIZED : constant VkImageLayout := 8;
   VK_IMAGE_LAYOUT_PRESENT_SRC_KHR : constant VkImageLayout := 1000001002;
   VK_IMAGE_LAYOUT_BEGIN_RANGE : constant VkImageLayout := 0;
   VK_IMAGE_LAYOUT_END_RANGE : constant VkImageLayout := 8;
   VK_IMAGE_LAYOUT_RANGE_SIZE : constant VkImageLayout := 9;
   VK_IMAGE_LAYOUT_MAX_ENUM : constant VkImageLayout := 2147483647;  -- src/vulkan/vulkan.h:481

   subtype VkImageViewType is unsigned;
   VK_IMAGE_VIEW_TYPE_1D : constant VkImageViewType := 0;
   VK_IMAGE_VIEW_TYPE_2D : constant VkImageViewType := 1;
   VK_IMAGE_VIEW_TYPE_3D : constant VkImageViewType := 2;
   VK_IMAGE_VIEW_TYPE_CUBE : constant VkImageViewType := 3;
   VK_IMAGE_VIEW_TYPE_1D_ARRAY : constant VkImageViewType := 4;
   VK_IMAGE_VIEW_TYPE_2D_ARRAY : constant VkImageViewType := 5;
   VK_IMAGE_VIEW_TYPE_CUBE_ARRAY : constant VkImageViewType := 6;
   VK_IMAGE_VIEW_TYPE_BEGIN_RANGE : constant VkImageViewType := 0;
   VK_IMAGE_VIEW_TYPE_END_RANGE : constant VkImageViewType := 6;
   VK_IMAGE_VIEW_TYPE_RANGE_SIZE : constant VkImageViewType := 7;
   VK_IMAGE_VIEW_TYPE_MAX_ENUM : constant VkImageViewType := 2147483647;  -- src/vulkan/vulkan.h:498

   subtype VkComponentSwizzle is unsigned;
   VK_COMPONENT_SWIZZLE_IDENTITY : constant VkComponentSwizzle := 0;
   VK_COMPONENT_SWIZZLE_ZERO : constant VkComponentSwizzle := 1;
   VK_COMPONENT_SWIZZLE_ONE : constant VkComponentSwizzle := 2;
   VK_COMPONENT_SWIZZLE_R : constant VkComponentSwizzle := 3;
   VK_COMPONENT_SWIZZLE_G : constant VkComponentSwizzle := 4;
   VK_COMPONENT_SWIZZLE_B : constant VkComponentSwizzle := 5;
   VK_COMPONENT_SWIZZLE_A : constant VkComponentSwizzle := 6;
   VK_COMPONENT_SWIZZLE_BEGIN_RANGE : constant VkComponentSwizzle := 0;
   VK_COMPONENT_SWIZZLE_END_RANGE : constant VkComponentSwizzle := 6;
   VK_COMPONENT_SWIZZLE_RANGE_SIZE : constant VkComponentSwizzle := 7;
   VK_COMPONENT_SWIZZLE_MAX_ENUM : constant VkComponentSwizzle := 2147483647;  -- src/vulkan/vulkan.h:512

   subtype VkVertexInputRate is unsigned;
   VK_VERTEX_INPUT_RATE_VERTEX : constant VkVertexInputRate := 0;
   VK_VERTEX_INPUT_RATE_INSTANCE : constant VkVertexInputRate := 1;
   VK_VERTEX_INPUT_RATE_BEGIN_RANGE : constant VkVertexInputRate := 0;
   VK_VERTEX_INPUT_RATE_END_RANGE : constant VkVertexInputRate := 1;
   VK_VERTEX_INPUT_RATE_RANGE_SIZE : constant VkVertexInputRate := 2;
   VK_VERTEX_INPUT_RATE_MAX_ENUM : constant VkVertexInputRate := 2147483647;  -- src/vulkan/vulkan.h:526

   subtype VkPrimitiveTopology is unsigned;
   VK_PRIMITIVE_TOPOLOGY_POINT_LIST : constant VkPrimitiveTopology := 0;
   VK_PRIMITIVE_TOPOLOGY_LINE_LIST : constant VkPrimitiveTopology := 1;
   VK_PRIMITIVE_TOPOLOGY_LINE_STRIP : constant VkPrimitiveTopology := 2;
   VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST : constant VkPrimitiveTopology := 3;
   VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP : constant VkPrimitiveTopology := 4;
   VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN : constant VkPrimitiveTopology := 5;
   VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY : constant VkPrimitiveTopology := 6;
   VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY : constant VkPrimitiveTopology := 7;
   VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY : constant VkPrimitiveTopology := 8;
   VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY : constant VkPrimitiveTopology := 9;
   VK_PRIMITIVE_TOPOLOGY_PATCH_LIST : constant VkPrimitiveTopology := 10;
   VK_PRIMITIVE_TOPOLOGY_BEGIN_RANGE : constant VkPrimitiveTopology := 0;
   VK_PRIMITIVE_TOPOLOGY_END_RANGE : constant VkPrimitiveTopology := 10;
   VK_PRIMITIVE_TOPOLOGY_RANGE_SIZE : constant VkPrimitiveTopology := 11;
   VK_PRIMITIVE_TOPOLOGY_MAX_ENUM : constant VkPrimitiveTopology := 2147483647;  -- src/vulkan/vulkan.h:535

   subtype VkPolygonMode is unsigned;
   VK_POLYGON_MODE_FILL : constant VkPolygonMode := 0;
   VK_POLYGON_MODE_LINE : constant VkPolygonMode := 1;
   VK_POLYGON_MODE_POINT : constant VkPolygonMode := 2;
   VK_POLYGON_MODE_BEGIN_RANGE : constant VkPolygonMode := 0;
   VK_POLYGON_MODE_END_RANGE : constant VkPolygonMode := 2;
   VK_POLYGON_MODE_RANGE_SIZE : constant VkPolygonMode := 3;
   VK_POLYGON_MODE_MAX_ENUM : constant VkPolygonMode := 2147483647;  -- src/vulkan/vulkan.h:553

   subtype VkFrontFace is unsigned;
   VK_FRONT_FACE_COUNTER_CLOCKWISE : constant VkFrontFace := 0;
   VK_FRONT_FACE_CLOCKWISE : constant VkFrontFace := 1;
   VK_FRONT_FACE_BEGIN_RANGE : constant VkFrontFace := 0;
   VK_FRONT_FACE_END_RANGE : constant VkFrontFace := 1;
   VK_FRONT_FACE_RANGE_SIZE : constant VkFrontFace := 2;
   VK_FRONT_FACE_MAX_ENUM : constant VkFrontFace := 2147483647;  -- src/vulkan/vulkan.h:563

   subtype VkCompareOp is unsigned;
   VK_COMPARE_OP_NEVER : constant VkCompareOp := 0;
   VK_COMPARE_OP_LESS : constant VkCompareOp := 1;
   VK_COMPARE_OP_EQUAL : constant VkCompareOp := 2;
   VK_COMPARE_OP_LESS_OR_EQUAL : constant VkCompareOp := 3;
   VK_COMPARE_OP_GREATER : constant VkCompareOp := 4;
   VK_COMPARE_OP_NOT_EQUAL : constant VkCompareOp := 5;
   VK_COMPARE_OP_GREATER_OR_EQUAL : constant VkCompareOp := 6;
   VK_COMPARE_OP_ALWAYS : constant VkCompareOp := 7;
   VK_COMPARE_OP_BEGIN_RANGE : constant VkCompareOp := 0;
   VK_COMPARE_OP_END_RANGE : constant VkCompareOp := 7;
   VK_COMPARE_OP_RANGE_SIZE : constant VkCompareOp := 8;
   VK_COMPARE_OP_MAX_ENUM : constant VkCompareOp := 2147483647;  -- src/vulkan/vulkan.h:572

   subtype VkStencilOp is unsigned;
   VK_STENCIL_OP_KEEP : constant VkStencilOp := 0;
   VK_STENCIL_OP_ZERO : constant VkStencilOp := 1;
   VK_STENCIL_OP_REPLACE : constant VkStencilOp := 2;
   VK_STENCIL_OP_INCREMENT_AND_CLAMP : constant VkStencilOp := 3;
   VK_STENCIL_OP_DECREMENT_AND_CLAMP : constant VkStencilOp := 4;
   VK_STENCIL_OP_INVERT : constant VkStencilOp := 5;
   VK_STENCIL_OP_INCREMENT_AND_WRAP : constant VkStencilOp := 6;
   VK_STENCIL_OP_DECREMENT_AND_WRAP : constant VkStencilOp := 7;
   VK_STENCIL_OP_BEGIN_RANGE : constant VkStencilOp := 0;
   VK_STENCIL_OP_END_RANGE : constant VkStencilOp := 7;
   VK_STENCIL_OP_RANGE_SIZE : constant VkStencilOp := 8;
   VK_STENCIL_OP_MAX_ENUM : constant VkStencilOp := 2147483647;  -- src/vulkan/vulkan.h:587

   subtype VkLogicOp is unsigned;
   VK_LOGIC_OP_CLEAR : constant VkLogicOp := 0;
   VK_LOGIC_OP_AND : constant VkLogicOp := 1;
   VK_LOGIC_OP_AND_REVERSE : constant VkLogicOp := 2;
   VK_LOGIC_OP_COPY : constant VkLogicOp := 3;
   VK_LOGIC_OP_AND_INVERTED : constant VkLogicOp := 4;
   VK_LOGIC_OP_NO_OP : constant VkLogicOp := 5;
   VK_LOGIC_OP_XOR : constant VkLogicOp := 6;
   VK_LOGIC_OP_OR : constant VkLogicOp := 7;
   VK_LOGIC_OP_NOR : constant VkLogicOp := 8;
   VK_LOGIC_OP_EQUIVALENT : constant VkLogicOp := 9;
   VK_LOGIC_OP_INVERT : constant VkLogicOp := 10;
   VK_LOGIC_OP_OR_REVERSE : constant VkLogicOp := 11;
   VK_LOGIC_OP_COPY_INVERTED : constant VkLogicOp := 12;
   VK_LOGIC_OP_OR_INVERTED : constant VkLogicOp := 13;
   VK_LOGIC_OP_NAND : constant VkLogicOp := 14;
   VK_LOGIC_OP_SET : constant VkLogicOp := 15;
   VK_LOGIC_OP_BEGIN_RANGE : constant VkLogicOp := 0;
   VK_LOGIC_OP_END_RANGE : constant VkLogicOp := 15;
   VK_LOGIC_OP_RANGE_SIZE : constant VkLogicOp := 16;
   VK_LOGIC_OP_MAX_ENUM : constant VkLogicOp := 2147483647;  -- src/vulkan/vulkan.h:602

   subtype VkBlendFactor is unsigned;
   VK_BLEND_FACTOR_ZERO : constant VkBlendFactor := 0;
   VK_BLEND_FACTOR_ONE : constant VkBlendFactor := 1;
   VK_BLEND_FACTOR_SRC_COLOR : constant VkBlendFactor := 2;
   VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR : constant VkBlendFactor := 3;
   VK_BLEND_FACTOR_DST_COLOR : constant VkBlendFactor := 4;
   VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR : constant VkBlendFactor := 5;
   VK_BLEND_FACTOR_SRC_ALPHA : constant VkBlendFactor := 6;
   VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA : constant VkBlendFactor := 7;
   VK_BLEND_FACTOR_DST_ALPHA : constant VkBlendFactor := 8;
   VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA : constant VkBlendFactor := 9;
   VK_BLEND_FACTOR_CONSTANT_COLOR : constant VkBlendFactor := 10;
   VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR : constant VkBlendFactor := 11;
   VK_BLEND_FACTOR_CONSTANT_ALPHA : constant VkBlendFactor := 12;
   VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA : constant VkBlendFactor := 13;
   VK_BLEND_FACTOR_SRC_ALPHA_SATURATE : constant VkBlendFactor := 14;
   VK_BLEND_FACTOR_SRC1_COLOR : constant VkBlendFactor := 15;
   VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR : constant VkBlendFactor := 16;
   VK_BLEND_FACTOR_SRC1_ALPHA : constant VkBlendFactor := 17;
   VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA : constant VkBlendFactor := 18;
   VK_BLEND_FACTOR_BEGIN_RANGE : constant VkBlendFactor := 0;
   VK_BLEND_FACTOR_END_RANGE : constant VkBlendFactor := 18;
   VK_BLEND_FACTOR_RANGE_SIZE : constant VkBlendFactor := 19;
   VK_BLEND_FACTOR_MAX_ENUM : constant VkBlendFactor := 2147483647;  -- src/vulkan/vulkan.h:625

   subtype VkBlendOp is unsigned;
   VK_BLEND_OP_ADD : constant VkBlendOp := 0;
   VK_BLEND_OP_SUBTRACT : constant VkBlendOp := 1;
   VK_BLEND_OP_REVERSE_SUBTRACT : constant VkBlendOp := 2;
   VK_BLEND_OP_MIN : constant VkBlendOp := 3;
   VK_BLEND_OP_MAX : constant VkBlendOp := 4;
   VK_BLEND_OP_BEGIN_RANGE : constant VkBlendOp := 0;
   VK_BLEND_OP_END_RANGE : constant VkBlendOp := 4;
   VK_BLEND_OP_RANGE_SIZE : constant VkBlendOp := 5;
   VK_BLEND_OP_MAX_ENUM : constant VkBlendOp := 2147483647;  -- src/vulkan/vulkan.h:651

   subtype VkDynamicState is unsigned;
   VK_DYNAMIC_STATE_VIEWPORT : constant VkDynamicState := 0;
   VK_DYNAMIC_STATE_SCISSOR : constant VkDynamicState := 1;
   VK_DYNAMIC_STATE_LINE_WIDTH : constant VkDynamicState := 2;
   VK_DYNAMIC_STATE_DEPTH_BIAS : constant VkDynamicState := 3;
   VK_DYNAMIC_STATE_BLEND_CONSTANTS : constant VkDynamicState := 4;
   VK_DYNAMIC_STATE_DEPTH_BOUNDS : constant VkDynamicState := 5;
   VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK : constant VkDynamicState := 6;
   VK_DYNAMIC_STATE_STENCIL_WRITE_MASK : constant VkDynamicState := 7;
   VK_DYNAMIC_STATE_STENCIL_REFERENCE : constant VkDynamicState := 8;
   VK_DYNAMIC_STATE_BEGIN_RANGE : constant VkDynamicState := 0;
   VK_DYNAMIC_STATE_END_RANGE : constant VkDynamicState := 8;
   VK_DYNAMIC_STATE_RANGE_SIZE : constant VkDynamicState := 9;
   VK_DYNAMIC_STATE_MAX_ENUM : constant VkDynamicState := 2147483647;  -- src/vulkan/vulkan.h:663

   subtype VkFilter is unsigned;
   VK_FILTER_NEAREST : constant VkFilter := 0;
   VK_FILTER_LINEAR : constant VkFilter := 1;
   VK_FILTER_BEGIN_RANGE : constant VkFilter := 0;
   VK_FILTER_END_RANGE : constant VkFilter := 1;
   VK_FILTER_RANGE_SIZE : constant VkFilter := 2;
   VK_FILTER_MAX_ENUM : constant VkFilter := 2147483647;  -- src/vulkan/vulkan.h:679

   subtype VkSamplerMipmapMode is unsigned;
   VK_SAMPLER_MIPMAP_MODE_NEAREST : constant VkSamplerMipmapMode := 0;
   VK_SAMPLER_MIPMAP_MODE_LINEAR : constant VkSamplerMipmapMode := 1;
   VK_SAMPLER_MIPMAP_MODE_BEGIN_RANGE : constant VkSamplerMipmapMode := 0;
   VK_SAMPLER_MIPMAP_MODE_END_RANGE : constant VkSamplerMipmapMode := 1;
   VK_SAMPLER_MIPMAP_MODE_RANGE_SIZE : constant VkSamplerMipmapMode := 2;
   VK_SAMPLER_MIPMAP_MODE_MAX_ENUM : constant VkSamplerMipmapMode := 2147483647;  -- src/vulkan/vulkan.h:688

   subtype VkSamplerAddressMode is unsigned;
   VK_SAMPLER_ADDRESS_MODE_REPEAT : constant VkSamplerAddressMode := 0;
   VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT : constant VkSamplerAddressMode := 1;
   VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE : constant VkSamplerAddressMode := 2;
   VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER : constant VkSamplerAddressMode := 3;
   VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE : constant VkSamplerAddressMode := 4;
   VK_SAMPLER_ADDRESS_MODE_BEGIN_RANGE : constant VkSamplerAddressMode := 0;
   VK_SAMPLER_ADDRESS_MODE_END_RANGE : constant VkSamplerAddressMode := 4;
   VK_SAMPLER_ADDRESS_MODE_RANGE_SIZE : constant VkSamplerAddressMode := 5;
   VK_SAMPLER_ADDRESS_MODE_MAX_ENUM : constant VkSamplerAddressMode := 2147483647;  -- src/vulkan/vulkan.h:697

   subtype VkBorderColor is unsigned;
   VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK : constant VkBorderColor := 0;
   VK_BORDER_COLOR_INT_TRANSPARENT_BLACK : constant VkBorderColor := 1;
   VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK : constant VkBorderColor := 2;
   VK_BORDER_COLOR_INT_OPAQUE_BLACK : constant VkBorderColor := 3;
   VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE : constant VkBorderColor := 4;
   VK_BORDER_COLOR_INT_OPAQUE_WHITE : constant VkBorderColor := 5;
   VK_BORDER_COLOR_BEGIN_RANGE : constant VkBorderColor := 0;
   VK_BORDER_COLOR_END_RANGE : constant VkBorderColor := 5;
   VK_BORDER_COLOR_RANGE_SIZE : constant VkBorderColor := 6;
   VK_BORDER_COLOR_MAX_ENUM : constant VkBorderColor := 2147483647;  -- src/vulkan/vulkan.h:709

   subtype VkDescriptorType is unsigned;
   VK_DESCRIPTOR_TYPE_SAMPLER : constant VkDescriptorType := 0;
   VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER : constant VkDescriptorType := 1;
   VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE : constant VkDescriptorType := 2;
   VK_DESCRIPTOR_TYPE_STORAGE_IMAGE : constant VkDescriptorType := 3;
   VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER : constant VkDescriptorType := 4;
   VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER : constant VkDescriptorType := 5;
   VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER : constant VkDescriptorType := 6;
   VK_DESCRIPTOR_TYPE_STORAGE_BUFFER : constant VkDescriptorType := 7;
   VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC : constant VkDescriptorType := 8;
   VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC : constant VkDescriptorType := 9;
   VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT : constant VkDescriptorType := 10;
   VK_DESCRIPTOR_TYPE_BEGIN_RANGE : constant VkDescriptorType := 0;
   VK_DESCRIPTOR_TYPE_END_RANGE : constant VkDescriptorType := 10;
   VK_DESCRIPTOR_TYPE_RANGE_SIZE : constant VkDescriptorType := 11;
   VK_DESCRIPTOR_TYPE_MAX_ENUM : constant VkDescriptorType := 2147483647;  -- src/vulkan/vulkan.h:722

   subtype VkAttachmentLoadOp is unsigned;
   VK_ATTACHMENT_LOAD_OP_LOAD : constant VkAttachmentLoadOp := 0;
   VK_ATTACHMENT_LOAD_OP_CLEAR : constant VkAttachmentLoadOp := 1;
   VK_ATTACHMENT_LOAD_OP_DONT_CARE : constant VkAttachmentLoadOp := 2;
   VK_ATTACHMENT_LOAD_OP_BEGIN_RANGE : constant VkAttachmentLoadOp := 0;
   VK_ATTACHMENT_LOAD_OP_END_RANGE : constant VkAttachmentLoadOp := 2;
   VK_ATTACHMENT_LOAD_OP_RANGE_SIZE : constant VkAttachmentLoadOp := 3;
   VK_ATTACHMENT_LOAD_OP_MAX_ENUM : constant VkAttachmentLoadOp := 2147483647;  -- src/vulkan/vulkan.h:740

   subtype VkAttachmentStoreOp is unsigned;
   VK_ATTACHMENT_STORE_OP_STORE : constant VkAttachmentStoreOp := 0;
   VK_ATTACHMENT_STORE_OP_DONT_CARE : constant VkAttachmentStoreOp := 1;
   VK_ATTACHMENT_STORE_OP_BEGIN_RANGE : constant VkAttachmentStoreOp := 0;
   VK_ATTACHMENT_STORE_OP_END_RANGE : constant VkAttachmentStoreOp := 1;
   VK_ATTACHMENT_STORE_OP_RANGE_SIZE : constant VkAttachmentStoreOp := 2;
   VK_ATTACHMENT_STORE_OP_MAX_ENUM : constant VkAttachmentStoreOp := 2147483647;  -- src/vulkan/vulkan.h:750

   subtype VkPipelineBindPoint is unsigned;
   VK_PIPELINE_BIND_POINT_GRAPHICS : constant VkPipelineBindPoint := 0;
   VK_PIPELINE_BIND_POINT_COMPUTE : constant VkPipelineBindPoint := 1;
   VK_PIPELINE_BIND_POINT_BEGIN_RANGE : constant VkPipelineBindPoint := 0;
   VK_PIPELINE_BIND_POINT_END_RANGE : constant VkPipelineBindPoint := 1;
   VK_PIPELINE_BIND_POINT_RANGE_SIZE : constant VkPipelineBindPoint := 2;
   VK_PIPELINE_BIND_POINT_MAX_ENUM : constant VkPipelineBindPoint := 2147483647;  -- src/vulkan/vulkan.h:759

   subtype VkCommandBufferLevel is unsigned;
   VK_COMMAND_BUFFER_LEVEL_PRIMARY : constant VkCommandBufferLevel := 0;
   VK_COMMAND_BUFFER_LEVEL_SECONDARY : constant VkCommandBufferLevel := 1;
   VK_COMMAND_BUFFER_LEVEL_BEGIN_RANGE : constant VkCommandBufferLevel := 0;
   VK_COMMAND_BUFFER_LEVEL_END_RANGE : constant VkCommandBufferLevel := 1;
   VK_COMMAND_BUFFER_LEVEL_RANGE_SIZE : constant VkCommandBufferLevel := 2;
   VK_COMMAND_BUFFER_LEVEL_MAX_ENUM : constant VkCommandBufferLevel := 2147483647;  -- src/vulkan/vulkan.h:768

   subtype VkIndexType is unsigned;
   VK_INDEX_TYPE_UINT16 : constant VkIndexType := 0;
   VK_INDEX_TYPE_UINT32 : constant VkIndexType := 1;
   VK_INDEX_TYPE_BEGIN_RANGE : constant VkIndexType := 0;
   VK_INDEX_TYPE_END_RANGE : constant VkIndexType := 1;
   VK_INDEX_TYPE_RANGE_SIZE : constant VkIndexType := 2;
   VK_INDEX_TYPE_MAX_ENUM : constant VkIndexType := 2147483647;  -- src/vulkan/vulkan.h:777

   subtype VkSubpassContents is unsigned;
   VK_SUBPASS_CONTENTS_INLINE : constant VkSubpassContents := 0;
   VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS : constant VkSubpassContents := 1;
   VK_SUBPASS_CONTENTS_BEGIN_RANGE : constant VkSubpassContents := 0;
   VK_SUBPASS_CONTENTS_END_RANGE : constant VkSubpassContents := 1;
   VK_SUBPASS_CONTENTS_RANGE_SIZE : constant VkSubpassContents := 2;
   VK_SUBPASS_CONTENTS_MAX_ENUM : constant VkSubpassContents := 2147483647;  -- src/vulkan/vulkan.h:786

   subtype VkInstanceCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:795

   subtype VkFormatFeatureFlagBits is unsigned;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT : constant VkFormatFeatureFlagBits := 1;
   VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT : constant VkFormatFeatureFlagBits := 2;
   VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT : constant VkFormatFeatureFlagBits := 4;
   VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT : constant VkFormatFeatureFlagBits := 8;
   VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT : constant VkFormatFeatureFlagBits := 16;
   VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT : constant VkFormatFeatureFlagBits := 32;
   VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT : constant VkFormatFeatureFlagBits := 64;
   VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT : constant VkFormatFeatureFlagBits := 128;
   VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT : constant VkFormatFeatureFlagBits := 256;
   VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT : constant VkFormatFeatureFlagBits := 512;
   VK_FORMAT_FEATURE_BLIT_SRC_BIT : constant VkFormatFeatureFlagBits := 1024;
   VK_FORMAT_FEATURE_BLIT_DST_BIT : constant VkFormatFeatureFlagBits := 2048;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT : constant VkFormatFeatureFlagBits := 4096;  -- src/vulkan/vulkan.h:797

   subtype VkFormatFeatureFlags is VkFlags;  -- src/vulkan/vulkan.h:812

   subtype VkImageUsageFlagBits is unsigned;
   VK_IMAGE_USAGE_TRANSFER_SRC_BIT : constant VkImageUsageFlagBits := 1;
   VK_IMAGE_USAGE_TRANSFER_DST_BIT : constant VkImageUsageFlagBits := 2;
   VK_IMAGE_USAGE_SAMPLED_BIT : constant VkImageUsageFlagBits := 4;
   VK_IMAGE_USAGE_STORAGE_BIT : constant VkImageUsageFlagBits := 8;
   VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT : constant VkImageUsageFlagBits := 16;
   VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT : constant VkImageUsageFlagBits := 32;
   VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT : constant VkImageUsageFlagBits := 64;
   VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT : constant VkImageUsageFlagBits := 128;  -- src/vulkan/vulkan.h:814

   subtype VkImageUsageFlags is VkFlags;  -- src/vulkan/vulkan.h:824

   subtype VkImageCreateFlagBits is unsigned;
   VK_IMAGE_CREATE_SPARSE_BINDING_BIT : constant VkImageCreateFlagBits := 1;
   VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT : constant VkImageCreateFlagBits := 2;
   VK_IMAGE_CREATE_SPARSE_ALIASED_BIT : constant VkImageCreateFlagBits := 4;
   VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT : constant VkImageCreateFlagBits := 8;
   VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT : constant VkImageCreateFlagBits := 16;  -- src/vulkan/vulkan.h:826

   subtype VkImageCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:833

   subtype VkSampleCountFlagBits is unsigned;
   VK_SAMPLE_COUNT_1_BIT : constant VkSampleCountFlagBits := 1;
   VK_SAMPLE_COUNT_2_BIT : constant VkSampleCountFlagBits := 2;
   VK_SAMPLE_COUNT_4_BIT : constant VkSampleCountFlagBits := 4;
   VK_SAMPLE_COUNT_8_BIT : constant VkSampleCountFlagBits := 8;
   VK_SAMPLE_COUNT_16_BIT : constant VkSampleCountFlagBits := 16;
   VK_SAMPLE_COUNT_32_BIT : constant VkSampleCountFlagBits := 32;
   VK_SAMPLE_COUNT_64_BIT : constant VkSampleCountFlagBits := 64;  -- src/vulkan/vulkan.h:835

   subtype VkSampleCountFlags is VkFlags;  -- src/vulkan/vulkan.h:844

   subtype VkQueueFlagBits is unsigned;
   VK_QUEUE_GRAPHICS_BIT : constant VkQueueFlagBits := 1;
   VK_QUEUE_COMPUTE_BIT : constant VkQueueFlagBits := 2;
   VK_QUEUE_TRANSFER_BIT : constant VkQueueFlagBits := 4;
   VK_QUEUE_SPARSE_BINDING_BIT : constant VkQueueFlagBits := 8;  -- src/vulkan/vulkan.h:846

   subtype VkQueueFlags is VkFlags;  -- src/vulkan/vulkan.h:852

   subtype VkMemoryPropertyFlagBits is unsigned;
   VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT : constant VkMemoryPropertyFlagBits := 1;
   VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT : constant VkMemoryPropertyFlagBits := 2;
   VK_MEMORY_PROPERTY_HOST_COHERENT_BIT : constant VkMemoryPropertyFlagBits := 4;
   VK_MEMORY_PROPERTY_HOST_CACHED_BIT : constant VkMemoryPropertyFlagBits := 8;
   VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT : constant VkMemoryPropertyFlagBits := 16;  -- src/vulkan/vulkan.h:854

   subtype VkMemoryPropertyFlags is VkFlags;  -- src/vulkan/vulkan.h:861

   subtype VkMemoryHeapFlagBits is unsigned;
   VK_MEMORY_HEAP_DEVICE_LOCAL_BIT : constant VkMemoryHeapFlagBits := 1;  -- src/vulkan/vulkan.h:863

   subtype VkMemoryHeapFlags is VkFlags;  -- src/vulkan/vulkan.h:866

   subtype VkDeviceCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:867

   subtype VkDeviceQueueCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:868

   subtype VkPipelineStageFlagBits is unsigned;
   VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT : constant VkPipelineStageFlagBits := 1;
   VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT : constant VkPipelineStageFlagBits := 2;
   VK_PIPELINE_STAGE_VERTEX_INPUT_BIT : constant VkPipelineStageFlagBits := 4;
   VK_PIPELINE_STAGE_VERTEX_SHADER_BIT : constant VkPipelineStageFlagBits := 8;
   VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT : constant VkPipelineStageFlagBits := 16;
   VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT : constant VkPipelineStageFlagBits := 32;
   VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT : constant VkPipelineStageFlagBits := 64;
   VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT : constant VkPipelineStageFlagBits := 128;
   VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT : constant VkPipelineStageFlagBits := 256;
   VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT : constant VkPipelineStageFlagBits := 512;
   VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT : constant VkPipelineStageFlagBits := 1024;
   VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT : constant VkPipelineStageFlagBits := 2048;
   VK_PIPELINE_STAGE_TRANSFER_BIT : constant VkPipelineStageFlagBits := 4096;
   VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT : constant VkPipelineStageFlagBits := 8192;
   VK_PIPELINE_STAGE_HOST_BIT : constant VkPipelineStageFlagBits := 16384;
   VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT : constant VkPipelineStageFlagBits := 32768;
   VK_PIPELINE_STAGE_ALL_COMMANDS_BIT : constant VkPipelineStageFlagBits := 65536;  -- src/vulkan/vulkan.h:870

   subtype VkPipelineStageFlags is VkFlags;  -- src/vulkan/vulkan.h:889

   subtype VkMemoryMapFlags is VkFlags;  -- src/vulkan/vulkan.h:890

   subtype VkImageAspectFlagBits is unsigned;
   VK_IMAGE_ASPECT_COLOR_BIT : constant VkImageAspectFlagBits := 1;
   VK_IMAGE_ASPECT_DEPTH_BIT : constant VkImageAspectFlagBits := 2;
   VK_IMAGE_ASPECT_STENCIL_BIT : constant VkImageAspectFlagBits := 4;
   VK_IMAGE_ASPECT_METADATA_BIT : constant VkImageAspectFlagBits := 8;  -- src/vulkan/vulkan.h:892

   subtype VkImageAspectFlags is VkFlags;  -- src/vulkan/vulkan.h:898

   subtype VkSparseImageFormatFlagBits is unsigned;
   VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT : constant VkSparseImageFormatFlagBits := 1;
   VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT : constant VkSparseImageFormatFlagBits := 2;
   VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT : constant VkSparseImageFormatFlagBits := 4;  -- src/vulkan/vulkan.h:900

   subtype VkSparseImageFormatFlags is VkFlags;  -- src/vulkan/vulkan.h:905

   subtype VkSparseMemoryBindFlagBits is unsigned;
   VK_SPARSE_MEMORY_BIND_METADATA_BIT : constant VkSparseMemoryBindFlagBits := 1;  -- src/vulkan/vulkan.h:907

   subtype VkSparseMemoryBindFlags is VkFlags;  -- src/vulkan/vulkan.h:910

   subtype VkFenceCreateFlagBits is unsigned;
   VK_FENCE_CREATE_SIGNALED_BIT : constant VkFenceCreateFlagBits := 1;  -- src/vulkan/vulkan.h:912

   subtype VkFenceCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:915

   subtype VkSemaphoreCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:916

   subtype VkEventCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:917

   subtype VkQueryPoolCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:918

   subtype VkQueryPipelineStatisticFlagBits is unsigned;
   VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT : constant VkQueryPipelineStatisticFlagBits := 1;
   VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT : constant VkQueryPipelineStatisticFlagBits := 2;
   VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 4;
   VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 8;
   VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT : constant VkQueryPipelineStatisticFlagBits := 16;
   VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 32;
   VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT : constant VkQueryPipelineStatisticFlagBits := 64;
   VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 128;
   VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT : constant VkQueryPipelineStatisticFlagBits := 256;
   VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 512;
   VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 1024;  -- src/vulkan/vulkan.h:920

   subtype VkQueryPipelineStatisticFlags is VkFlags;  -- src/vulkan/vulkan.h:933

   subtype VkQueryResultFlagBits is unsigned;
   VK_QUERY_RESULT_64_BIT : constant VkQueryResultFlagBits := 1;
   VK_QUERY_RESULT_WAIT_BIT : constant VkQueryResultFlagBits := 2;
   VK_QUERY_RESULT_WITH_AVAILABILITY_BIT : constant VkQueryResultFlagBits := 4;
   VK_QUERY_RESULT_PARTIAL_BIT : constant VkQueryResultFlagBits := 8;  -- src/vulkan/vulkan.h:935

   subtype VkQueryResultFlags is VkFlags;  -- src/vulkan/vulkan.h:941

   subtype VkBufferCreateFlagBits is unsigned;
   VK_BUFFER_CREATE_SPARSE_BINDING_BIT : constant VkBufferCreateFlagBits := 1;
   VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT : constant VkBufferCreateFlagBits := 2;
   VK_BUFFER_CREATE_SPARSE_ALIASED_BIT : constant VkBufferCreateFlagBits := 4;  -- src/vulkan/vulkan.h:943

   subtype VkBufferCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:948

   subtype VkBufferUsageFlagBits is unsigned;
   VK_BUFFER_USAGE_TRANSFER_SRC_BIT : constant VkBufferUsageFlagBits := 1;
   VK_BUFFER_USAGE_TRANSFER_DST_BIT : constant VkBufferUsageFlagBits := 2;
   VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT : constant VkBufferUsageFlagBits := 4;
   VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT : constant VkBufferUsageFlagBits := 8;
   VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT : constant VkBufferUsageFlagBits := 16;
   VK_BUFFER_USAGE_STORAGE_BUFFER_BIT : constant VkBufferUsageFlagBits := 32;
   VK_BUFFER_USAGE_INDEX_BUFFER_BIT : constant VkBufferUsageFlagBits := 64;
   VK_BUFFER_USAGE_VERTEX_BUFFER_BIT : constant VkBufferUsageFlagBits := 128;
   VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT : constant VkBufferUsageFlagBits := 256;  -- src/vulkan/vulkan.h:950

   subtype VkBufferUsageFlags is VkFlags;  -- src/vulkan/vulkan.h:961

   subtype VkBufferViewCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:962

   subtype VkImageViewCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:963

   subtype VkShaderModuleCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:964

   subtype VkPipelineCacheCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:965

   subtype VkPipelineCreateFlagBits is unsigned;
   VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT : constant VkPipelineCreateFlagBits := 1;
   VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT : constant VkPipelineCreateFlagBits := 2;
   VK_PIPELINE_CREATE_DERIVATIVE_BIT : constant VkPipelineCreateFlagBits := 4;  -- src/vulkan/vulkan.h:967

   subtype VkPipelineCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:972

   subtype VkPipelineShaderStageCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:973

   subtype VkShaderStageFlagBits is unsigned;
   VK_SHADER_STAGE_VERTEX_BIT : constant VkShaderStageFlagBits := 1;
   VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT : constant VkShaderStageFlagBits := 2;
   VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT : constant VkShaderStageFlagBits := 4;
   VK_SHADER_STAGE_GEOMETRY_BIT : constant VkShaderStageFlagBits := 8;
   VK_SHADER_STAGE_FRAGMENT_BIT : constant VkShaderStageFlagBits := 16;
   VK_SHADER_STAGE_COMPUTE_BIT : constant VkShaderStageFlagBits := 32;
   VK_SHADER_STAGE_ALL_GRAPHICS : constant VkShaderStageFlagBits := 31;
   VK_SHADER_STAGE_ALL : constant VkShaderStageFlagBits := 2147483647;  -- src/vulkan/vulkan.h:975

   subtype VkPipelineVertexInputStateCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:985

   subtype VkPipelineInputAssemblyStateCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:986

   subtype VkPipelineTessellationStateCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:987

   subtype VkPipelineViewportStateCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:988

   subtype VkPipelineRasterizationStateCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:989

   type VkCullModeFlagBits is
     (VK_CULL_MODE_NONE,
      VK_CULL_MODE_FRONT_BIT,
      VK_CULL_MODE_BACK_BIT,
      VK_CULL_MODE_FRONT_AND_BACK);
   pragma Convention (C, VkCullModeFlagBits);  -- src/vulkan/vulkan.h:991

   subtype VkCullModeFlags is VkFlags;  -- src/vulkan/vulkan.h:997

   subtype VkPipelineMultisampleStateCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:998

   subtype VkPipelineDepthStencilStateCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:999

   subtype VkPipelineColorBlendStateCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:1000

   subtype VkColorComponentFlagBits is unsigned;
   VK_COLOR_COMPONENT_R_BIT : constant VkColorComponentFlagBits := 1;
   VK_COLOR_COMPONENT_G_BIT : constant VkColorComponentFlagBits := 2;
   VK_COLOR_COMPONENT_B_BIT : constant VkColorComponentFlagBits := 4;
   VK_COLOR_COMPONENT_A_BIT : constant VkColorComponentFlagBits := 8;  -- src/vulkan/vulkan.h:1002

   subtype VkColorComponentFlags is VkFlags;  -- src/vulkan/vulkan.h:1008

   subtype VkPipelineDynamicStateCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:1009

   subtype VkPipelineLayoutCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:1010

   subtype VkShaderStageFlags is VkFlags;  -- src/vulkan/vulkan.h:1011

   subtype VkSamplerCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:1012

   subtype VkDescriptorSetLayoutCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:1013

   subtype VkDescriptorPoolCreateFlagBits is unsigned;
   VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT : constant VkDescriptorPoolCreateFlagBits := 1;  -- src/vulkan/vulkan.h:1015

   subtype VkDescriptorPoolCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:1018

   subtype VkDescriptorPoolResetFlags is VkFlags;  -- src/vulkan/vulkan.h:1019

   subtype VkFramebufferCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:1020

   subtype VkRenderPassCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:1021

   subtype VkAttachmentDescriptionFlagBits is unsigned;
   VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT : constant VkAttachmentDescriptionFlagBits := 1;  -- src/vulkan/vulkan.h:1023

   subtype VkAttachmentDescriptionFlags is VkFlags;  -- src/vulkan/vulkan.h:1026

   subtype VkSubpassDescriptionFlags is VkFlags;  -- src/vulkan/vulkan.h:1027

   subtype VkAccessFlagBits is unsigned;
   VK_ACCESS_INDIRECT_COMMAND_READ_BIT : constant VkAccessFlagBits := 1;
   VK_ACCESS_INDEX_READ_BIT : constant VkAccessFlagBits := 2;
   VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT : constant VkAccessFlagBits := 4;
   VK_ACCESS_UNIFORM_READ_BIT : constant VkAccessFlagBits := 8;
   VK_ACCESS_INPUT_ATTACHMENT_READ_BIT : constant VkAccessFlagBits := 16;
   VK_ACCESS_SHADER_READ_BIT : constant VkAccessFlagBits := 32;
   VK_ACCESS_SHADER_WRITE_BIT : constant VkAccessFlagBits := 64;
   VK_ACCESS_COLOR_ATTACHMENT_READ_BIT : constant VkAccessFlagBits := 128;
   VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT : constant VkAccessFlagBits := 256;
   VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT : constant VkAccessFlagBits := 512;
   VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT : constant VkAccessFlagBits := 1024;
   VK_ACCESS_TRANSFER_READ_BIT : constant VkAccessFlagBits := 2048;
   VK_ACCESS_TRANSFER_WRITE_BIT : constant VkAccessFlagBits := 4096;
   VK_ACCESS_HOST_READ_BIT : constant VkAccessFlagBits := 8192;
   VK_ACCESS_HOST_WRITE_BIT : constant VkAccessFlagBits := 16384;
   VK_ACCESS_MEMORY_READ_BIT : constant VkAccessFlagBits := 32768;
   VK_ACCESS_MEMORY_WRITE_BIT : constant VkAccessFlagBits := 65536;  -- src/vulkan/vulkan.h:1029

   subtype VkAccessFlags is VkFlags;  -- src/vulkan/vulkan.h:1048

   subtype VkDependencyFlagBits is unsigned;
   VK_DEPENDENCY_BY_REGION_BIT : constant VkDependencyFlagBits := 1;  -- src/vulkan/vulkan.h:1050

   subtype VkDependencyFlags is VkFlags;  -- src/vulkan/vulkan.h:1053

   subtype VkCommandPoolCreateFlagBits is unsigned;
   VK_COMMAND_POOL_CREATE_TRANSIENT_BIT : constant VkCommandPoolCreateFlagBits := 1;
   VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT : constant VkCommandPoolCreateFlagBits := 2;  -- src/vulkan/vulkan.h:1055

   subtype VkCommandPoolCreateFlags is VkFlags;  -- src/vulkan/vulkan.h:1059

   subtype VkCommandPoolResetFlagBits is unsigned;
   VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT : constant VkCommandPoolResetFlagBits := 1;  -- src/vulkan/vulkan.h:1061

   subtype VkCommandPoolResetFlags is VkFlags;  -- src/vulkan/vulkan.h:1064

   subtype VkCommandBufferUsageFlagBits is unsigned;
   VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT : constant VkCommandBufferUsageFlagBits := 1;
   VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT : constant VkCommandBufferUsageFlagBits := 2;
   VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT : constant VkCommandBufferUsageFlagBits := 4;  -- src/vulkan/vulkan.h:1066

   subtype VkCommandBufferUsageFlags is VkFlags;  -- src/vulkan/vulkan.h:1071

   subtype VkQueryControlFlagBits is unsigned;
   VK_QUERY_CONTROL_PRECISE_BIT : constant VkQueryControlFlagBits := 1;  -- src/vulkan/vulkan.h:1073

   subtype VkQueryControlFlags is VkFlags;  -- src/vulkan/vulkan.h:1076

   subtype VkCommandBufferResetFlagBits is unsigned;
   VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT : constant VkCommandBufferResetFlagBits := 1;  -- src/vulkan/vulkan.h:1078

   subtype VkCommandBufferResetFlags is VkFlags;  -- src/vulkan/vulkan.h:1081

   subtype VkStencilFaceFlagBits is unsigned;
   VK_STENCIL_FACE_FRONT_BIT : constant VkStencilFaceFlagBits := 1;
   VK_STENCIL_FACE_BACK_BIT : constant VkStencilFaceFlagBits := 2;
   VK_STENCIL_FRONT_AND_BACK : constant VkStencilFaceFlagBits := 3;  -- src/vulkan/vulkan.h:1083

   subtype VkStencilFaceFlags is VkFlags;  -- src/vulkan/vulkan.h:1088

   type PFN_vkAllocationFunction is access function
        (arg1 : System.Address;
         arg2 : stddef_h.size_t;
         arg3 : stddef_h.size_t;
         arg4 : VkSystemAllocationScope) return System.Address;
   pragma Convention (C, PFN_vkAllocationFunction);  -- src/vulkan/vulkan.h:1090

   type PFN_vkReallocationFunction is access function
        (arg1 : System.Address;
         arg2 : System.Address;
         arg3 : stddef_h.size_t;
         arg4 : stddef_h.size_t;
         arg5 : VkSystemAllocationScope) return System.Address;
   pragma Convention (C, PFN_vkReallocationFunction);  -- src/vulkan/vulkan.h:1096

   type PFN_vkFreeFunction is access procedure (arg1 : System.Address; arg2 : System.Address);
   pragma Convention (C, PFN_vkFreeFunction);  -- src/vulkan/vulkan.h:1103

   type PFN_vkInternalAllocationNotification is access procedure
        (arg1 : System.Address;
         arg2 : stddef_h.size_t;
         arg3 : VkInternalAllocationType;
         arg4 : VkSystemAllocationScope);
   pragma Convention (C, PFN_vkInternalAllocationNotification);  -- src/vulkan/vulkan.h:1107

   type PFN_vkInternalFreeNotification is access procedure
        (arg1 : System.Address;
         arg2 : stddef_h.size_t;
         arg3 : VkInternalAllocationType;
         arg4 : VkSystemAllocationScope);
   pragma Convention (C, PFN_vkInternalFreeNotification);  -- src/vulkan/vulkan.h:1113

   type PFN_vkVoidFunction is access procedure;
   pragma Convention (C, PFN_vkVoidFunction);  -- src/vulkan/vulkan.h:1119

   type VkApplicationInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1122
      pNext : System.Address;  -- src/vulkan/vulkan.h:1123
      pApplicationName : Interfaces.C.Strings.chars_ptr;  -- src/vulkan/vulkan.h:1124
      applicationVersion : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1125
      pEngineName : Interfaces.C.Strings.chars_ptr;  -- src/vulkan/vulkan.h:1126
      engineVersion : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1127
      apiVersion : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1128
   end record;
   pragma Convention (C_Pass_By_Copy, VkApplicationInfo);  -- src/vulkan/vulkan.h:1121

   type VkInstanceCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1132
      pNext : System.Address;  -- src/vulkan/vulkan.h:1133
      flags : aliased VkInstanceCreateFlags;  -- src/vulkan/vulkan.h:1134
      pApplicationInfo : System.Address;  -- src/vulkan/vulkan.h:1135
      enabledLayerCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1136
      ppEnabledLayerNames : System.Address;  -- src/vulkan/vulkan.h:1137
      enabledExtensionCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1138
      ppEnabledExtensionNames : System.Address;  -- src/vulkan/vulkan.h:1139
   end record;
   pragma Convention (C_Pass_By_Copy, VkInstanceCreateInfo);  -- src/vulkan/vulkan.h:1131

   type VkAllocationCallbacks is record
      pUserData : System.Address;  -- src/vulkan/vulkan.h:1143
      pfnAllocation : PFN_vkAllocationFunction;  -- src/vulkan/vulkan.h:1144
      pfnReallocation : PFN_vkReallocationFunction;  -- src/vulkan/vulkan.h:1145
      pfnFree : PFN_vkFreeFunction;  -- src/vulkan/vulkan.h:1146
      pfnInternalAllocation : PFN_vkInternalAllocationNotification;  -- src/vulkan/vulkan.h:1147
      pfnInternalFree : PFN_vkInternalFreeNotification;  -- src/vulkan/vulkan.h:1148
   end record;
   pragma Convention (C_Pass_By_Copy, VkAllocationCallbacks);  -- src/vulkan/vulkan.h:1142

   type VkPhysicalDeviceFeatures is record
      robustBufferAccess : aliased VkBool32;  -- src/vulkan/vulkan.h:1152
      fullDrawIndexUint32 : aliased VkBool32;  -- src/vulkan/vulkan.h:1153
      imageCubeArray : aliased VkBool32;  -- src/vulkan/vulkan.h:1154
      independentBlend : aliased VkBool32;  -- src/vulkan/vulkan.h:1155
      geometryShader : aliased VkBool32;  -- src/vulkan/vulkan.h:1156
      tessellationShader : aliased VkBool32;  -- src/vulkan/vulkan.h:1157
      sampleRateShading : aliased VkBool32;  -- src/vulkan/vulkan.h:1158
      dualSrcBlend : aliased VkBool32;  -- src/vulkan/vulkan.h:1159
      logicOp : aliased VkBool32;  -- src/vulkan/vulkan.h:1160
      multiDrawIndirect : aliased VkBool32;  -- src/vulkan/vulkan.h:1161
      drawIndirectFirstInstance : aliased VkBool32;  -- src/vulkan/vulkan.h:1162
      depthClamp : aliased VkBool32;  -- src/vulkan/vulkan.h:1163
      depthBiasClamp : aliased VkBool32;  -- src/vulkan/vulkan.h:1164
      fillModeNonSolid : aliased VkBool32;  -- src/vulkan/vulkan.h:1165
      depthBounds : aliased VkBool32;  -- src/vulkan/vulkan.h:1166
      wideLines : aliased VkBool32;  -- src/vulkan/vulkan.h:1167
      largePoints : aliased VkBool32;  -- src/vulkan/vulkan.h:1168
      alphaToOne : aliased VkBool32;  -- src/vulkan/vulkan.h:1169
      multiViewport : aliased VkBool32;  -- src/vulkan/vulkan.h:1170
      samplerAnisotropy : aliased VkBool32;  -- src/vulkan/vulkan.h:1171
      textureCompressionETC2 : aliased VkBool32;  -- src/vulkan/vulkan.h:1172
      textureCompressionASTC_LDR : aliased VkBool32;  -- src/vulkan/vulkan.h:1173
      textureCompressionBC : aliased VkBool32;  -- src/vulkan/vulkan.h:1174
      occlusionQueryPrecise : aliased VkBool32;  -- src/vulkan/vulkan.h:1175
      pipelineStatisticsQuery : aliased VkBool32;  -- src/vulkan/vulkan.h:1176
      vertexPipelineStoresAndAtomics : aliased VkBool32;  -- src/vulkan/vulkan.h:1177
      fragmentStoresAndAtomics : aliased VkBool32;  -- src/vulkan/vulkan.h:1178
      shaderTessellationAndGeometryPointSize : aliased VkBool32;  -- src/vulkan/vulkan.h:1179
      shaderImageGatherExtended : aliased VkBool32;  -- src/vulkan/vulkan.h:1180
      shaderStorageImageExtendedFormats : aliased VkBool32;  -- src/vulkan/vulkan.h:1181
      shaderStorageImageMultisample : aliased VkBool32;  -- src/vulkan/vulkan.h:1182
      shaderStorageImageReadWithoutFormat : aliased VkBool32;  -- src/vulkan/vulkan.h:1183
      shaderStorageImageWriteWithoutFormat : aliased VkBool32;  -- src/vulkan/vulkan.h:1184
      shaderUniformBufferArrayDynamicIndexing : aliased VkBool32;  -- src/vulkan/vulkan.h:1185
      shaderSampledImageArrayDynamicIndexing : aliased VkBool32;  -- src/vulkan/vulkan.h:1186
      shaderStorageBufferArrayDynamicIndexing : aliased VkBool32;  -- src/vulkan/vulkan.h:1187
      shaderStorageImageArrayDynamicIndexing : aliased VkBool32;  -- src/vulkan/vulkan.h:1188
      shaderClipDistance : aliased VkBool32;  -- src/vulkan/vulkan.h:1189
      shaderCullDistance : aliased VkBool32;  -- src/vulkan/vulkan.h:1190
      shaderFloat64 : aliased VkBool32;  -- src/vulkan/vulkan.h:1191
      shaderInt64 : aliased VkBool32;  -- src/vulkan/vulkan.h:1192
      shaderInt16 : aliased VkBool32;  -- src/vulkan/vulkan.h:1193
      shaderResourceResidency : aliased VkBool32;  -- src/vulkan/vulkan.h:1194
      shaderResourceMinLod : aliased VkBool32;  -- src/vulkan/vulkan.h:1195
      sparseBinding : aliased VkBool32;  -- src/vulkan/vulkan.h:1196
      sparseResidencyBuffer : aliased VkBool32;  -- src/vulkan/vulkan.h:1197
      sparseResidencyImage2D : aliased VkBool32;  -- src/vulkan/vulkan.h:1198
      sparseResidencyImage3D : aliased VkBool32;  -- src/vulkan/vulkan.h:1199
      sparseResidency2Samples : aliased VkBool32;  -- src/vulkan/vulkan.h:1200
      sparseResidency4Samples : aliased VkBool32;  -- src/vulkan/vulkan.h:1201
      sparseResidency8Samples : aliased VkBool32;  -- src/vulkan/vulkan.h:1202
      sparseResidency16Samples : aliased VkBool32;  -- src/vulkan/vulkan.h:1203
      sparseResidencyAliased : aliased VkBool32;  -- src/vulkan/vulkan.h:1204
      variableMultisampleRate : aliased VkBool32;  -- src/vulkan/vulkan.h:1205
      inheritedQueries : aliased VkBool32;  -- src/vulkan/vulkan.h:1206
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFeatures);  -- src/vulkan/vulkan.h:1151

   type VkFormatProperties is record
      linearTilingFeatures : aliased VkFormatFeatureFlags;  -- src/vulkan/vulkan.h:1210
      optimalTilingFeatures : aliased VkFormatFeatureFlags;  -- src/vulkan/vulkan.h:1211
      bufferFeatures : aliased VkFormatFeatureFlags;  -- src/vulkan/vulkan.h:1212
   end record;
   pragma Convention (C_Pass_By_Copy, VkFormatProperties);  -- src/vulkan/vulkan.h:1209

   type VkExtent3D is record
      width : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1216
      height : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1217
      depth : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1218
   end record;
   pragma Convention (C_Pass_By_Copy, VkExtent3D);  -- src/vulkan/vulkan.h:1215

   type VkImageFormatProperties is record
      maxExtent : aliased VkExtent3D;  -- src/vulkan/vulkan.h:1222
      maxMipLevels : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1223
      maxArrayLayers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1224
      sampleCounts : aliased VkSampleCountFlags;  -- src/vulkan/vulkan.h:1225
      maxResourceSize : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1226
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageFormatProperties);  -- src/vulkan/vulkan.h:1221

   type VkPhysicalDeviceLimits_maxComputeWorkGroupCount_array is array (0 .. 2) of aliased stdint_h.uint32_t;
   type VkPhysicalDeviceLimits_maxComputeWorkGroupSize_array is array (0 .. 2) of aliased stdint_h.uint32_t;
   type VkPhysicalDeviceLimits_maxViewportDimensions_array is array (0 .. 1) of aliased stdint_h.uint32_t;
   type VkPhysicalDeviceLimits_viewportBoundsRange_array is array (0 .. 1) of aliased float;
   type VkPhysicalDeviceLimits_pointSizeRange_array is array (0 .. 1) of aliased float;
   type VkPhysicalDeviceLimits_lineWidthRange_array is array (0 .. 1) of aliased float;
   type VkPhysicalDeviceLimits is record
      maxImageDimension1D : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1230
      maxImageDimension2D : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1231
      maxImageDimension3D : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1232
      maxImageDimensionCube : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1233
      maxImageArrayLayers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1234
      maxTexelBufferElements : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1235
      maxUniformBufferRange : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1236
      maxStorageBufferRange : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1237
      maxPushConstantsSize : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1238
      maxMemoryAllocationCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1239
      maxSamplerAllocationCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1240
      bufferImageGranularity : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1241
      sparseAddressSpaceSize : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1242
      maxBoundDescriptorSets : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1243
      maxPerStageDescriptorSamplers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1244
      maxPerStageDescriptorUniformBuffers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1245
      maxPerStageDescriptorStorageBuffers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1246
      maxPerStageDescriptorSampledImages : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1247
      maxPerStageDescriptorStorageImages : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1248
      maxPerStageDescriptorInputAttachments : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1249
      maxPerStageResources : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1250
      maxDescriptorSetSamplers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1251
      maxDescriptorSetUniformBuffers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1252
      maxDescriptorSetUniformBuffersDynamic : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1253
      maxDescriptorSetStorageBuffers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1254
      maxDescriptorSetStorageBuffersDynamic : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1255
      maxDescriptorSetSampledImages : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1256
      maxDescriptorSetStorageImages : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1257
      maxDescriptorSetInputAttachments : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1258
      maxVertexInputAttributes : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1259
      maxVertexInputBindings : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1260
      maxVertexInputAttributeOffset : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1261
      maxVertexInputBindingStride : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1262
      maxVertexOutputComponents : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1263
      maxTessellationGenerationLevel : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1264
      maxTessellationPatchSize : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1265
      maxTessellationControlPerVertexInputComponents : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1266
      maxTessellationControlPerVertexOutputComponents : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1267
      maxTessellationControlPerPatchOutputComponents : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1268
      maxTessellationControlTotalOutputComponents : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1269
      maxTessellationEvaluationInputComponents : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1270
      maxTessellationEvaluationOutputComponents : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1271
      maxGeometryShaderInvocations : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1272
      maxGeometryInputComponents : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1273
      maxGeometryOutputComponents : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1274
      maxGeometryOutputVertices : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1275
      maxGeometryTotalOutputComponents : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1276
      maxFragmentInputComponents : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1277
      maxFragmentOutputAttachments : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1278
      maxFragmentDualSrcAttachments : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1279
      maxFragmentCombinedOutputResources : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1280
      maxComputeSharedMemorySize : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1281
      maxComputeWorkGroupCount : aliased VkPhysicalDeviceLimits_maxComputeWorkGroupCount_array;  -- src/vulkan/vulkan.h:1282
      maxComputeWorkGroupInvocations : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1283
      maxComputeWorkGroupSize : aliased VkPhysicalDeviceLimits_maxComputeWorkGroupSize_array;  -- src/vulkan/vulkan.h:1284
      subPixelPrecisionBits : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1285
      subTexelPrecisionBits : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1286
      mipmapPrecisionBits : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1287
      maxDrawIndexedIndexValue : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1288
      maxDrawIndirectCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1289
      maxSamplerLodBias : aliased float;  -- src/vulkan/vulkan.h:1290
      maxSamplerAnisotropy : aliased float;  -- src/vulkan/vulkan.h:1291
      maxViewports : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1292
      maxViewportDimensions : aliased VkPhysicalDeviceLimits_maxViewportDimensions_array;  -- src/vulkan/vulkan.h:1293
      viewportBoundsRange : aliased VkPhysicalDeviceLimits_viewportBoundsRange_array;  -- src/vulkan/vulkan.h:1294
      viewportSubPixelBits : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1295
      minMemoryMapAlignment : aliased stddef_h.size_t;  -- src/vulkan/vulkan.h:1296
      minTexelBufferOffsetAlignment : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1297
      minUniformBufferOffsetAlignment : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1298
      minStorageBufferOffsetAlignment : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1299
      minTexelOffset : aliased stdint_h.int32_t;  -- src/vulkan/vulkan.h:1300
      maxTexelOffset : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1301
      minTexelGatherOffset : aliased stdint_h.int32_t;  -- src/vulkan/vulkan.h:1302
      maxTexelGatherOffset : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1303
      minInterpolationOffset : aliased float;  -- src/vulkan/vulkan.h:1304
      maxInterpolationOffset : aliased float;  -- src/vulkan/vulkan.h:1305
      subPixelInterpolationOffsetBits : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1306
      maxFramebufferWidth : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1307
      maxFramebufferHeight : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1308
      maxFramebufferLayers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1309
      framebufferColorSampleCounts : aliased VkSampleCountFlags;  -- src/vulkan/vulkan.h:1310
      framebufferDepthSampleCounts : aliased VkSampleCountFlags;  -- src/vulkan/vulkan.h:1311
      framebufferStencilSampleCounts : aliased VkSampleCountFlags;  -- src/vulkan/vulkan.h:1312
      framebufferNoAttachmentsSampleCounts : aliased VkSampleCountFlags;  -- src/vulkan/vulkan.h:1313
      maxColorAttachments : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1314
      sampledImageColorSampleCounts : aliased VkSampleCountFlags;  -- src/vulkan/vulkan.h:1315
      sampledImageIntegerSampleCounts : aliased VkSampleCountFlags;  -- src/vulkan/vulkan.h:1316
      sampledImageDepthSampleCounts : aliased VkSampleCountFlags;  -- src/vulkan/vulkan.h:1317
      sampledImageStencilSampleCounts : aliased VkSampleCountFlags;  -- src/vulkan/vulkan.h:1318
      storageImageSampleCounts : aliased VkSampleCountFlags;  -- src/vulkan/vulkan.h:1319
      maxSampleMaskWords : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1320
      timestampComputeAndGraphics : aliased VkBool32;  -- src/vulkan/vulkan.h:1321
      timestampPeriod : aliased float;  -- src/vulkan/vulkan.h:1322
      maxClipDistances : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1323
      maxCullDistances : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1324
      maxCombinedClipAndCullDistances : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1325
      discreteQueuePriorities : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1326
      pointSizeRange : aliased VkPhysicalDeviceLimits_pointSizeRange_array;  -- src/vulkan/vulkan.h:1327
      lineWidthRange : aliased VkPhysicalDeviceLimits_lineWidthRange_array;  -- src/vulkan/vulkan.h:1328
      pointSizeGranularity : aliased float;  -- src/vulkan/vulkan.h:1329
      lineWidthGranularity : aliased float;  -- src/vulkan/vulkan.h:1330
      strictLines : aliased VkBool32;  -- src/vulkan/vulkan.h:1331
      standardSampleLocations : aliased VkBool32;  -- src/vulkan/vulkan.h:1332
      optimalBufferCopyOffsetAlignment : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1333
      optimalBufferCopyRowPitchAlignment : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1334
      nonCoherentAtomSize : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1335
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceLimits);  -- src/vulkan/vulkan.h:1229

   type VkPhysicalDeviceSparseProperties is record
      residencyStandard2DBlockShape : aliased VkBool32;  -- src/vulkan/vulkan.h:1339
      residencyStandard2DMultisampleBlockShape : aliased VkBool32;  -- src/vulkan/vulkan.h:1340
      residencyStandard3DBlockShape : aliased VkBool32;  -- src/vulkan/vulkan.h:1341
      residencyAlignedMipSize : aliased VkBool32;  -- src/vulkan/vulkan.h:1342
      residencyNonResidentStrict : aliased VkBool32;  -- src/vulkan/vulkan.h:1343
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSparseProperties);  -- src/vulkan/vulkan.h:1338

   subtype VkPhysicalDeviceProperties_deviceName_array is Interfaces.C.char_array (0 .. 255);
   type VkPhysicalDeviceProperties_pipelineCacheUUID_array is array (0 .. 15) of aliased stdint_h.uint8_t;
   type VkPhysicalDeviceProperties is record
      apiVersion : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1347
      driverVersion : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1348
      vendorID : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1349
      deviceID : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1350
      deviceType : aliased VkPhysicalDeviceType;  -- src/vulkan/vulkan.h:1351
      deviceName : aliased VkPhysicalDeviceProperties_deviceName_array;  -- src/vulkan/vulkan.h:1352
      pipelineCacheUUID : aliased VkPhysicalDeviceProperties_pipelineCacheUUID_array;  -- src/vulkan/vulkan.h:1353
      limits : aliased VkPhysicalDeviceLimits;  -- src/vulkan/vulkan.h:1354
      sparseProperties : aliased VkPhysicalDeviceSparseProperties;  -- src/vulkan/vulkan.h:1355
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceProperties);  -- src/vulkan/vulkan.h:1346

   type VkQueueFamilyProperties is record
      queueFlags : aliased VkQueueFlags;  -- src/vulkan/vulkan.h:1359
      queueCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1360
      timestampValidBits : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1361
      minImageTransferGranularity : aliased VkExtent3D;  -- src/vulkan/vulkan.h:1362
   end record;
   pragma Convention (C_Pass_By_Copy, VkQueueFamilyProperties);  -- src/vulkan/vulkan.h:1358

   type VkMemoryType is record
      propertyFlags : aliased VkMemoryPropertyFlags;  -- src/vulkan/vulkan.h:1366
      heapIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1367
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryType);  -- src/vulkan/vulkan.h:1365

   type VkMemoryHeap is record
      size : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1371
      flags : aliased VkMemoryHeapFlags;  -- src/vulkan/vulkan.h:1372
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryHeap);  -- src/vulkan/vulkan.h:1370

   type VkPhysicalDeviceMemoryProperties_memoryTypes_array is array (0 .. 31) of aliased VkMemoryType;
   type VkPhysicalDeviceMemoryProperties_memoryHeaps_array is array (0 .. 15) of aliased VkMemoryHeap;
   type VkPhysicalDeviceMemoryProperties is record
      memoryTypeCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1376
      memoryTypes : aliased VkPhysicalDeviceMemoryProperties_memoryTypes_array;  -- src/vulkan/vulkan.h:1377
      memoryHeapCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1378
      memoryHeaps : aliased VkPhysicalDeviceMemoryProperties_memoryHeaps_array;  -- src/vulkan/vulkan.h:1379
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMemoryProperties);  -- src/vulkan/vulkan.h:1375

   type VkDeviceQueueCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1383
      pNext : System.Address;  -- src/vulkan/vulkan.h:1384
      flags : aliased VkDeviceQueueCreateFlags;  -- src/vulkan/vulkan.h:1385
      queueFamilyIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1386
      queueCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1387
      pQueuePriorities : access float;  -- src/vulkan/vulkan.h:1388
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceQueueCreateInfo);  -- src/vulkan/vulkan.h:1382

   type VkDeviceCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1392
      pNext : System.Address;  -- src/vulkan/vulkan.h:1393
      flags : aliased VkDeviceCreateFlags;  -- src/vulkan/vulkan.h:1394
      queueCreateInfoCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1395
      pQueueCreateInfos : System.Address;  -- src/vulkan/vulkan.h:1396
      enabledLayerCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1397
      ppEnabledLayerNames : System.Address;  -- src/vulkan/vulkan.h:1398
      enabledExtensionCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1399
      ppEnabledExtensionNames : System.Address;  -- src/vulkan/vulkan.h:1400
      pEnabledFeatures : System.Address;  -- src/vulkan/vulkan.h:1401
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceCreateInfo);  -- src/vulkan/vulkan.h:1391

   subtype VkExtensionProperties_extensionName_array is Interfaces.C.char_array (0 .. 255);
   type VkExtensionProperties is record
      extensionName : aliased VkExtensionProperties_extensionName_array;  -- src/vulkan/vulkan.h:1405
      specVersion : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1406
   end record;
   pragma Convention (C_Pass_By_Copy, VkExtensionProperties);  -- src/vulkan/vulkan.h:1404

   subtype VkLayerProperties_layerName_array is Interfaces.C.char_array (0 .. 255);
   subtype VkLayerProperties_description_array is Interfaces.C.char_array (0 .. 255);
   type VkLayerProperties is record
      layerName : aliased VkLayerProperties_layerName_array;  -- src/vulkan/vulkan.h:1410
      specVersion : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1411
      implementationVersion : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1412
      description : aliased VkLayerProperties_description_array;  -- src/vulkan/vulkan.h:1413
   end record;
   pragma Convention (C_Pass_By_Copy, VkLayerProperties);  -- src/vulkan/vulkan.h:1409

   type VkSubmitInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1417
      pNext : System.Address;  -- src/vulkan/vulkan.h:1418
      waitSemaphoreCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1419
      pWaitSemaphores : System.Address;  -- src/vulkan/vulkan.h:1420
      pWaitDstStageMask : access VkPipelineStageFlags;  -- src/vulkan/vulkan.h:1421
      commandBufferCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1422
      pCommandBuffers : System.Address;  -- src/vulkan/vulkan.h:1423
      signalSemaphoreCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1424
      pSignalSemaphores : System.Address;  -- src/vulkan/vulkan.h:1425
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubmitInfo);  -- src/vulkan/vulkan.h:1416

   type VkMemoryAllocateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1429
      pNext : System.Address;  -- src/vulkan/vulkan.h:1430
      allocationSize : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1431
      memoryTypeIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1432
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryAllocateInfo);  -- src/vulkan/vulkan.h:1428

   type VkMappedMemoryRange is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1436
      pNext : System.Address;  -- src/vulkan/vulkan.h:1437
      memory : VkDeviceMemory;  -- src/vulkan/vulkan.h:1438
      offset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1439
      size : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1440
   end record;
   pragma Convention (C_Pass_By_Copy, VkMappedMemoryRange);  -- src/vulkan/vulkan.h:1435

   type VkMemoryRequirements is record
      size : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1444
      alignment : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1445
      memoryTypeBits : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1446
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryRequirements);  -- src/vulkan/vulkan.h:1443

   type VkSparseImageFormatProperties is record
      aspectMask : aliased VkImageAspectFlags;  -- src/vulkan/vulkan.h:1450
      imageGranularity : aliased VkExtent3D;  -- src/vulkan/vulkan.h:1451
      flags : aliased VkSparseImageFormatFlags;  -- src/vulkan/vulkan.h:1452
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageFormatProperties);  -- src/vulkan/vulkan.h:1449

   type VkSparseImageMemoryRequirements is record
      formatProperties : aliased VkSparseImageFormatProperties;  -- src/vulkan/vulkan.h:1456
      imageMipTailFirstLod : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1457
      imageMipTailSize : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1458
      imageMipTailOffset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1459
      imageMipTailStride : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1460
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageMemoryRequirements);  -- src/vulkan/vulkan.h:1455

   type VkSparseMemoryBind is record
      resourceOffset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1464
      size : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1465
      memory : VkDeviceMemory;  -- src/vulkan/vulkan.h:1466
      memoryOffset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1467
      flags : aliased VkSparseMemoryBindFlags;  -- src/vulkan/vulkan.h:1468
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseMemoryBind);  -- src/vulkan/vulkan.h:1463

   type VkSparseBufferMemoryBindInfo is record
      buffer : VkBuffer;  -- src/vulkan/vulkan.h:1472
      bindCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1473
      pBinds : System.Address;  -- src/vulkan/vulkan.h:1474
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseBufferMemoryBindInfo);  -- src/vulkan/vulkan.h:1471

   type VkSparseImageOpaqueMemoryBindInfo is record
      image : VkImage;  -- src/vulkan/vulkan.h:1478
      bindCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1479
      pBinds : System.Address;  -- src/vulkan/vulkan.h:1480
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageOpaqueMemoryBindInfo);  -- src/vulkan/vulkan.h:1477

   type VkImageSubresource is record
      aspectMask : aliased VkImageAspectFlags;  -- src/vulkan/vulkan.h:1484
      mipLevel : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1485
      arrayLayer : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1486
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageSubresource);  -- src/vulkan/vulkan.h:1483

   type VkOffset3D is record
      x : aliased stdint_h.int32_t;  -- src/vulkan/vulkan.h:1490
      y : aliased stdint_h.int32_t;  -- src/vulkan/vulkan.h:1491
      z : aliased stdint_h.int32_t;  -- src/vulkan/vulkan.h:1492
   end record;
   pragma Convention (C_Pass_By_Copy, VkOffset3D);  -- src/vulkan/vulkan.h:1489

   type VkSparseImageMemoryBind is record
      subresource : aliased VkImageSubresource;  -- src/vulkan/vulkan.h:1496
      offset : aliased VkOffset3D;  -- src/vulkan/vulkan.h:1497
      extent : aliased VkExtent3D;  -- src/vulkan/vulkan.h:1498
      memory : VkDeviceMemory;  -- src/vulkan/vulkan.h:1499
      memoryOffset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1500
      flags : aliased VkSparseMemoryBindFlags;  -- src/vulkan/vulkan.h:1501
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageMemoryBind);  -- src/vulkan/vulkan.h:1495

   type VkSparseImageMemoryBindInfo is record
      image : VkImage;  -- src/vulkan/vulkan.h:1505
      bindCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1506
      pBinds : System.Address;  -- src/vulkan/vulkan.h:1507
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageMemoryBindInfo);  -- src/vulkan/vulkan.h:1504

   type VkBindSparseInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1511
      pNext : System.Address;  -- src/vulkan/vulkan.h:1512
      waitSemaphoreCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1513
      pWaitSemaphores : System.Address;  -- src/vulkan/vulkan.h:1514
      bufferBindCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1515
      pBufferBinds : System.Address;  -- src/vulkan/vulkan.h:1516
      imageOpaqueBindCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1517
      pImageOpaqueBinds : System.Address;  -- src/vulkan/vulkan.h:1518
      imageBindCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1519
      pImageBinds : System.Address;  -- src/vulkan/vulkan.h:1520
      signalSemaphoreCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1521
      pSignalSemaphores : System.Address;  -- src/vulkan/vulkan.h:1522
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindSparseInfo);  -- src/vulkan/vulkan.h:1510

   type VkFenceCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1526
      pNext : System.Address;  -- src/vulkan/vulkan.h:1527
      flags : aliased VkFenceCreateFlags;  -- src/vulkan/vulkan.h:1528
   end record;
   pragma Convention (C_Pass_By_Copy, VkFenceCreateInfo);  -- src/vulkan/vulkan.h:1525

   type VkSemaphoreCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1532
      pNext : System.Address;  -- src/vulkan/vulkan.h:1533
      flags : aliased VkSemaphoreCreateFlags;  -- src/vulkan/vulkan.h:1534
   end record;
   pragma Convention (C_Pass_By_Copy, VkSemaphoreCreateInfo);  -- src/vulkan/vulkan.h:1531

   type VkEventCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1538
      pNext : System.Address;  -- src/vulkan/vulkan.h:1539
      flags : aliased VkEventCreateFlags;  -- src/vulkan/vulkan.h:1540
   end record;
   pragma Convention (C_Pass_By_Copy, VkEventCreateInfo);  -- src/vulkan/vulkan.h:1537

   type VkQueryPoolCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1544
      pNext : System.Address;  -- src/vulkan/vulkan.h:1545
      flags : aliased VkQueryPoolCreateFlags;  -- src/vulkan/vulkan.h:1546
      queryType : aliased VkQueryType;  -- src/vulkan/vulkan.h:1547
      queryCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1548
      pipelineStatistics : aliased VkQueryPipelineStatisticFlags;  -- src/vulkan/vulkan.h:1549
   end record;
   pragma Convention (C_Pass_By_Copy, VkQueryPoolCreateInfo);  -- src/vulkan/vulkan.h:1543

   type VkBufferCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1553
      pNext : System.Address;  -- src/vulkan/vulkan.h:1554
      flags : aliased VkBufferCreateFlags;  -- src/vulkan/vulkan.h:1555
      size : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1556
      usage : aliased VkBufferUsageFlags;  -- src/vulkan/vulkan.h:1557
      sharingMode : aliased VkSharingMode;  -- src/vulkan/vulkan.h:1558
      queueFamilyIndexCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1559
      pQueueFamilyIndices : access stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1560
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferCreateInfo);  -- src/vulkan/vulkan.h:1552

   type VkBufferViewCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1564
      pNext : System.Address;  -- src/vulkan/vulkan.h:1565
      flags : aliased VkBufferViewCreateFlags;  -- src/vulkan/vulkan.h:1566
      buffer : VkBuffer;  -- src/vulkan/vulkan.h:1567
      format : aliased VkFormat;  -- src/vulkan/vulkan.h:1568
      offset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1569
      c_range : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1570
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferViewCreateInfo);  -- src/vulkan/vulkan.h:1563

   type VkImageCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1574
      pNext : System.Address;  -- src/vulkan/vulkan.h:1575
      flags : aliased VkImageCreateFlags;  -- src/vulkan/vulkan.h:1576
      imageType : aliased VkImageType;  -- src/vulkan/vulkan.h:1577
      format : aliased VkFormat;  -- src/vulkan/vulkan.h:1578
      extent : aliased VkExtent3D;  -- src/vulkan/vulkan.h:1579
      mipLevels : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1580
      arrayLayers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1581
      samples : aliased VkSampleCountFlagBits;  -- src/vulkan/vulkan.h:1582
      tiling : aliased VkImageTiling;  -- src/vulkan/vulkan.h:1583
      usage : aliased VkImageUsageFlags;  -- src/vulkan/vulkan.h:1584
      sharingMode : aliased VkSharingMode;  -- src/vulkan/vulkan.h:1585
      queueFamilyIndexCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1586
      pQueueFamilyIndices : access stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1587
      initialLayout : aliased VkImageLayout;  -- src/vulkan/vulkan.h:1588
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageCreateInfo);  -- src/vulkan/vulkan.h:1573

   type VkSubresourceLayout is record
      offset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1592
      size : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1593
      rowPitch : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1594
      arrayPitch : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1595
      depthPitch : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1596
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubresourceLayout);  -- src/vulkan/vulkan.h:1591

   type VkComponentMapping is record
      r : aliased VkComponentSwizzle;  -- src/vulkan/vulkan.h:1600
      g : aliased VkComponentSwizzle;  -- src/vulkan/vulkan.h:1601
      b : aliased VkComponentSwizzle;  -- src/vulkan/vulkan.h:1602
      a : aliased VkComponentSwizzle;  -- src/vulkan/vulkan.h:1603
   end record;
   pragma Convention (C_Pass_By_Copy, VkComponentMapping);  -- src/vulkan/vulkan.h:1599

   type VkImageSubresourceRange is record
      aspectMask : aliased VkImageAspectFlags;  -- src/vulkan/vulkan.h:1607
      baseMipLevel : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1608
      levelCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1609
      baseArrayLayer : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1610
      layerCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1611
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageSubresourceRange);  -- src/vulkan/vulkan.h:1606

   type VkImageViewCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1615
      pNext : System.Address;  -- src/vulkan/vulkan.h:1616
      flags : aliased VkImageViewCreateFlags;  -- src/vulkan/vulkan.h:1617
      image : VkImage;  -- src/vulkan/vulkan.h:1618
      viewType : aliased VkImageViewType;  -- src/vulkan/vulkan.h:1619
      format : aliased VkFormat;  -- src/vulkan/vulkan.h:1620
      components : aliased VkComponentMapping;  -- src/vulkan/vulkan.h:1621
      subresourceRange : aliased VkImageSubresourceRange;  -- src/vulkan/vulkan.h:1622
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageViewCreateInfo);  -- src/vulkan/vulkan.h:1614

   type VkShaderModuleCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1626
      pNext : System.Address;  -- src/vulkan/vulkan.h:1627
      flags : aliased VkShaderModuleCreateFlags;  -- src/vulkan/vulkan.h:1628
      codeSize : aliased stddef_h.size_t;  -- src/vulkan/vulkan.h:1629
      pCode : access stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1630
   end record;
   pragma Convention (C_Pass_By_Copy, VkShaderModuleCreateInfo);  -- src/vulkan/vulkan.h:1625

   type VkPipelineCacheCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1634
      pNext : System.Address;  -- src/vulkan/vulkan.h:1635
      flags : aliased VkPipelineCacheCreateFlags;  -- src/vulkan/vulkan.h:1636
      initialDataSize : aliased stddef_h.size_t;  -- src/vulkan/vulkan.h:1637
      pInitialData : System.Address;  -- src/vulkan/vulkan.h:1638
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineCacheCreateInfo);  -- src/vulkan/vulkan.h:1633

   type VkSpecializationMapEntry is record
      constantID : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1642
      offset : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1643
      size : aliased stddef_h.size_t;  -- src/vulkan/vulkan.h:1644
   end record;
   pragma Convention (C_Pass_By_Copy, VkSpecializationMapEntry);  -- src/vulkan/vulkan.h:1641

   type VkSpecializationInfo is record
      mapEntryCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1648
      pMapEntries : System.Address;  -- src/vulkan/vulkan.h:1649
      dataSize : aliased stddef_h.size_t;  -- src/vulkan/vulkan.h:1650
      pData : System.Address;  -- src/vulkan/vulkan.h:1651
   end record;
   pragma Convention (C_Pass_By_Copy, VkSpecializationInfo);  -- src/vulkan/vulkan.h:1647

   type VkPipelineShaderStageCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1655
      pNext : System.Address;  -- src/vulkan/vulkan.h:1656
      flags : aliased VkPipelineShaderStageCreateFlags;  -- src/vulkan/vulkan.h:1657
      stage : aliased VkShaderStageFlagBits;  -- src/vulkan/vulkan.h:1658
      module : VkShaderModule;  -- src/vulkan/vulkan.h:1659
      pName : Interfaces.C.Strings.chars_ptr;  -- src/vulkan/vulkan.h:1660
      pSpecializationInfo : System.Address;  -- src/vulkan/vulkan.h:1661
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineShaderStageCreateInfo);  -- src/vulkan/vulkan.h:1654

   type VkVertexInputBindingDescription is record
      binding : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1665
      stride : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1666
      inputRate : aliased VkVertexInputRate;  -- src/vulkan/vulkan.h:1667
   end record;
   pragma Convention (C_Pass_By_Copy, VkVertexInputBindingDescription);  -- src/vulkan/vulkan.h:1664

   type VkVertexInputAttributeDescription is record
      location : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1671
      binding : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1672
      format : aliased VkFormat;  -- src/vulkan/vulkan.h:1673
      offset : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1674
   end record;
   pragma Convention (C_Pass_By_Copy, VkVertexInputAttributeDescription);  -- src/vulkan/vulkan.h:1670

   type VkPipelineVertexInputStateCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1678
      pNext : System.Address;  -- src/vulkan/vulkan.h:1679
      flags : aliased VkPipelineVertexInputStateCreateFlags;  -- src/vulkan/vulkan.h:1680
      vertexBindingDescriptionCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1681
      pVertexBindingDescriptions : System.Address;  -- src/vulkan/vulkan.h:1682
      vertexAttributeDescriptionCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1683
      pVertexAttributeDescriptions : System.Address;  -- src/vulkan/vulkan.h:1684
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineVertexInputStateCreateInfo);  -- src/vulkan/vulkan.h:1677

   type VkPipelineInputAssemblyStateCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1688
      pNext : System.Address;  -- src/vulkan/vulkan.h:1689
      flags : aliased VkPipelineInputAssemblyStateCreateFlags;  -- src/vulkan/vulkan.h:1690
      topology : aliased VkPrimitiveTopology;  -- src/vulkan/vulkan.h:1691
      primitiveRestartEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1692
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineInputAssemblyStateCreateInfo);  -- src/vulkan/vulkan.h:1687

   type VkPipelineTessellationStateCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1696
      pNext : System.Address;  -- src/vulkan/vulkan.h:1697
      flags : aliased VkPipelineTessellationStateCreateFlags;  -- src/vulkan/vulkan.h:1698
      patchControlPoints : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1699
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineTessellationStateCreateInfo);  -- src/vulkan/vulkan.h:1695

   type VkViewport is record
      x : aliased float;  -- src/vulkan/vulkan.h:1703
      y : aliased float;  -- src/vulkan/vulkan.h:1704
      width : aliased float;  -- src/vulkan/vulkan.h:1705
      height : aliased float;  -- src/vulkan/vulkan.h:1706
      minDepth : aliased float;  -- src/vulkan/vulkan.h:1707
      maxDepth : aliased float;  -- src/vulkan/vulkan.h:1708
   end record;
   pragma Convention (C_Pass_By_Copy, VkViewport);  -- src/vulkan/vulkan.h:1702

   type VkOffset2D is record
      x : aliased stdint_h.int32_t;  -- src/vulkan/vulkan.h:1712
      y : aliased stdint_h.int32_t;  -- src/vulkan/vulkan.h:1713
   end record;
   pragma Convention (C_Pass_By_Copy, VkOffset2D);  -- src/vulkan/vulkan.h:1711

   type VkExtent2D is record
      width : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1717
      height : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1718
   end record;
   pragma Convention (C_Pass_By_Copy, VkExtent2D);  -- src/vulkan/vulkan.h:1716

   type VkRect2D is record
      offset : aliased VkOffset2D;  -- src/vulkan/vulkan.h:1722
      extent : aliased VkExtent2D;  -- src/vulkan/vulkan.h:1723
   end record;
   pragma Convention (C_Pass_By_Copy, VkRect2D);  -- src/vulkan/vulkan.h:1721

   type VkPipelineViewportStateCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1727
      pNext : System.Address;  -- src/vulkan/vulkan.h:1728
      flags : aliased VkPipelineViewportStateCreateFlags;  -- src/vulkan/vulkan.h:1729
      viewportCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1730
      pViewports : System.Address;  -- src/vulkan/vulkan.h:1731
      scissorCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1732
      pScissors : System.Address;  -- src/vulkan/vulkan.h:1733
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineViewportStateCreateInfo);  -- src/vulkan/vulkan.h:1726

   type VkPipelineRasterizationStateCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1737
      pNext : System.Address;  -- src/vulkan/vulkan.h:1738
      flags : aliased VkPipelineRasterizationStateCreateFlags;  -- src/vulkan/vulkan.h:1739
      depthClampEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1740
      rasterizerDiscardEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1741
      polygonMode : aliased VkPolygonMode;  -- src/vulkan/vulkan.h:1742
      cullMode : aliased VkCullModeFlags;  -- src/vulkan/vulkan.h:1743
      frontFace : aliased VkFrontFace;  -- src/vulkan/vulkan.h:1744
      depthBiasEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1745
      depthBiasConstantFactor : aliased float;  -- src/vulkan/vulkan.h:1746
      depthBiasClamp : aliased float;  -- src/vulkan/vulkan.h:1747
      depthBiasSlopeFactor : aliased float;  -- src/vulkan/vulkan.h:1748
      lineWidth : aliased float;  -- src/vulkan/vulkan.h:1749
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineRasterizationStateCreateInfo);  -- src/vulkan/vulkan.h:1736

   type VkPipelineMultisampleStateCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1753
      pNext : System.Address;  -- src/vulkan/vulkan.h:1754
      flags : aliased VkPipelineMultisampleStateCreateFlags;  -- src/vulkan/vulkan.h:1755
      rasterizationSamples : aliased VkSampleCountFlagBits;  -- src/vulkan/vulkan.h:1756
      sampleShadingEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1757
      minSampleShading : aliased float;  -- src/vulkan/vulkan.h:1758
      pSampleMask : access VkSampleMask;  -- src/vulkan/vulkan.h:1759
      alphaToCoverageEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1760
      alphaToOneEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1761
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineMultisampleStateCreateInfo);  -- src/vulkan/vulkan.h:1752

   type VkStencilOpState is record
      failOp : aliased VkStencilOp;  -- src/vulkan/vulkan.h:1765
      passOp : aliased VkStencilOp;  -- src/vulkan/vulkan.h:1766
      depthFailOp : aliased VkStencilOp;  -- src/vulkan/vulkan.h:1767
      compareOp : aliased VkCompareOp;  -- src/vulkan/vulkan.h:1768
      compareMask : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1769
      writeMask : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1770
      reference : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1771
   end record;
   pragma Convention (C_Pass_By_Copy, VkStencilOpState);  -- src/vulkan/vulkan.h:1764

   type VkPipelineDepthStencilStateCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1775
      pNext : System.Address;  -- src/vulkan/vulkan.h:1776
      flags : aliased VkPipelineDepthStencilStateCreateFlags;  -- src/vulkan/vulkan.h:1777
      depthTestEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1778
      depthWriteEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1779
      depthCompareOp : aliased VkCompareOp;  -- src/vulkan/vulkan.h:1780
      depthBoundsTestEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1781
      stencilTestEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1782
      front : aliased VkStencilOpState;  -- src/vulkan/vulkan.h:1783
      back : aliased VkStencilOpState;  -- src/vulkan/vulkan.h:1784
      minDepthBounds : aliased float;  -- src/vulkan/vulkan.h:1785
      maxDepthBounds : aliased float;  -- src/vulkan/vulkan.h:1786
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineDepthStencilStateCreateInfo);  -- src/vulkan/vulkan.h:1774

   type VkPipelineColorBlendAttachmentState is record
      blendEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1790
      srcColorBlendFactor : aliased VkBlendFactor;  -- src/vulkan/vulkan.h:1791
      dstColorBlendFactor : aliased VkBlendFactor;  -- src/vulkan/vulkan.h:1792
      colorBlendOp : aliased VkBlendOp;  -- src/vulkan/vulkan.h:1793
      srcAlphaBlendFactor : aliased VkBlendFactor;  -- src/vulkan/vulkan.h:1794
      dstAlphaBlendFactor : aliased VkBlendFactor;  -- src/vulkan/vulkan.h:1795
      alphaBlendOp : aliased VkBlendOp;  -- src/vulkan/vulkan.h:1796
      colorWriteMask : aliased VkColorComponentFlags;  -- src/vulkan/vulkan.h:1797
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineColorBlendAttachmentState);  -- src/vulkan/vulkan.h:1789

   type VkPipelineColorBlendStateCreateInfo_blendConstants_array is array (0 .. 3) of aliased float;
   type VkPipelineColorBlendStateCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1801
      pNext : System.Address;  -- src/vulkan/vulkan.h:1802
      flags : aliased VkPipelineColorBlendStateCreateFlags;  -- src/vulkan/vulkan.h:1803
      logicOpEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1804
      logicOp : aliased VkLogicOp;  -- src/vulkan/vulkan.h:1805
      attachmentCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1806
      pAttachments : System.Address;  -- src/vulkan/vulkan.h:1807
      blendConstants : aliased VkPipelineColorBlendStateCreateInfo_blendConstants_array;  -- src/vulkan/vulkan.h:1808
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineColorBlendStateCreateInfo);  -- src/vulkan/vulkan.h:1800

   type VkPipelineDynamicStateCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1812
      pNext : System.Address;  -- src/vulkan/vulkan.h:1813
      flags : aliased VkPipelineDynamicStateCreateFlags;  -- src/vulkan/vulkan.h:1814
      dynamicStateCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1815
      pDynamicStates : System.Address;  -- src/vulkan/vulkan.h:1816
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineDynamicStateCreateInfo);  -- src/vulkan/vulkan.h:1811

   type VkGraphicsPipelineCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1820
      pNext : System.Address;  -- src/vulkan/vulkan.h:1821
      flags : aliased VkPipelineCreateFlags;  -- src/vulkan/vulkan.h:1822
      stageCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1823
      pStages : System.Address;  -- src/vulkan/vulkan.h:1824
      pVertexInputState : System.Address;  -- src/vulkan/vulkan.h:1825
      pInputAssemblyState : System.Address;  -- src/vulkan/vulkan.h:1826
      pTessellationState : System.Address;  -- src/vulkan/vulkan.h:1827
      pViewportState : System.Address;  -- src/vulkan/vulkan.h:1828
      pRasterizationState : System.Address;  -- src/vulkan/vulkan.h:1829
      pMultisampleState : System.Address;  -- src/vulkan/vulkan.h:1830
      pDepthStencilState : System.Address;  -- src/vulkan/vulkan.h:1831
      pColorBlendState : System.Address;  -- src/vulkan/vulkan.h:1832
      pDynamicState : System.Address;  -- src/vulkan/vulkan.h:1833
      layout : VkPipelineLayout;  -- src/vulkan/vulkan.h:1834
      renderPass : VkRenderPass;  -- src/vulkan/vulkan.h:1835
      subpass : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1836
      basePipelineHandle : VkPipeline;  -- src/vulkan/vulkan.h:1837
      basePipelineIndex : aliased stdint_h.int32_t;  -- src/vulkan/vulkan.h:1838
   end record;
   pragma Convention (C_Pass_By_Copy, VkGraphicsPipelineCreateInfo);  -- src/vulkan/vulkan.h:1819

   type VkComputePipelineCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1842
      pNext : System.Address;  -- src/vulkan/vulkan.h:1843
      flags : aliased VkPipelineCreateFlags;  -- src/vulkan/vulkan.h:1844
      stage : aliased VkPipelineShaderStageCreateInfo;  -- src/vulkan/vulkan.h:1845
      layout : VkPipelineLayout;  -- src/vulkan/vulkan.h:1846
      basePipelineHandle : VkPipeline;  -- src/vulkan/vulkan.h:1847
      basePipelineIndex : aliased stdint_h.int32_t;  -- src/vulkan/vulkan.h:1848
   end record;
   pragma Convention (C_Pass_By_Copy, VkComputePipelineCreateInfo);  -- src/vulkan/vulkan.h:1841

   type VkPushConstantRange is record
      stageFlags : aliased VkShaderStageFlags;  -- src/vulkan/vulkan.h:1852
      offset : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1853
      size : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1854
   end record;
   pragma Convention (C_Pass_By_Copy, VkPushConstantRange);  -- src/vulkan/vulkan.h:1851

   type VkPipelineLayoutCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1858
      pNext : System.Address;  -- src/vulkan/vulkan.h:1859
      flags : aliased VkPipelineLayoutCreateFlags;  -- src/vulkan/vulkan.h:1860
      setLayoutCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1861
      pSetLayouts : System.Address;  -- src/vulkan/vulkan.h:1862
      pushConstantRangeCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1863
      pPushConstantRanges : System.Address;  -- src/vulkan/vulkan.h:1864
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineLayoutCreateInfo);  -- src/vulkan/vulkan.h:1857

   type VkSamplerCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1868
      pNext : System.Address;  -- src/vulkan/vulkan.h:1869
      flags : aliased VkSamplerCreateFlags;  -- src/vulkan/vulkan.h:1870
      magFilter : aliased VkFilter;  -- src/vulkan/vulkan.h:1871
      minFilter : aliased VkFilter;  -- src/vulkan/vulkan.h:1872
      mipmapMode : aliased VkSamplerMipmapMode;  -- src/vulkan/vulkan.h:1873
      addressModeU : aliased VkSamplerAddressMode;  -- src/vulkan/vulkan.h:1874
      addressModeV : aliased VkSamplerAddressMode;  -- src/vulkan/vulkan.h:1875
      addressModeW : aliased VkSamplerAddressMode;  -- src/vulkan/vulkan.h:1876
      mipLodBias : aliased float;  -- src/vulkan/vulkan.h:1877
      anisotropyEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1878
      maxAnisotropy : aliased float;  -- src/vulkan/vulkan.h:1879
      compareEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:1880
      compareOp : aliased VkCompareOp;  -- src/vulkan/vulkan.h:1881
      minLod : aliased float;  -- src/vulkan/vulkan.h:1882
      maxLod : aliased float;  -- src/vulkan/vulkan.h:1883
      borderColor : aliased VkBorderColor;  -- src/vulkan/vulkan.h:1884
      unnormalizedCoordinates : aliased VkBool32;  -- src/vulkan/vulkan.h:1885
   end record;
   pragma Convention (C_Pass_By_Copy, VkSamplerCreateInfo);  -- src/vulkan/vulkan.h:1867

   type VkDescriptorSetLayoutBinding is record
      binding : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1889
      descriptorType : aliased VkDescriptorType;  -- src/vulkan/vulkan.h:1890
      descriptorCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1891
      stageFlags : aliased VkShaderStageFlags;  -- src/vulkan/vulkan.h:1892
      pImmutableSamplers : System.Address;  -- src/vulkan/vulkan.h:1893
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorSetLayoutBinding);  -- src/vulkan/vulkan.h:1888

   type VkDescriptorSetLayoutCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1897
      pNext : System.Address;  -- src/vulkan/vulkan.h:1898
      flags : aliased VkDescriptorSetLayoutCreateFlags;  -- src/vulkan/vulkan.h:1899
      bindingCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1900
      pBindings : System.Address;  -- src/vulkan/vulkan.h:1901
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorSetLayoutCreateInfo);  -- src/vulkan/vulkan.h:1896

   type VkDescriptorPoolSize is record
      c_type : aliased VkDescriptorType;  -- src/vulkan/vulkan.h:1905
      descriptorCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1906
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorPoolSize);  -- src/vulkan/vulkan.h:1904

   type VkDescriptorPoolCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1910
      pNext : System.Address;  -- src/vulkan/vulkan.h:1911
      flags : aliased VkDescriptorPoolCreateFlags;  -- src/vulkan/vulkan.h:1912
      maxSets : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1913
      poolSizeCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1914
      pPoolSizes : System.Address;  -- src/vulkan/vulkan.h:1915
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorPoolCreateInfo);  -- src/vulkan/vulkan.h:1909

   type VkDescriptorSetAllocateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1919
      pNext : System.Address;  -- src/vulkan/vulkan.h:1920
      descriptorPool : VkDescriptorPool;  -- src/vulkan/vulkan.h:1921
      descriptorSetCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1922
      pSetLayouts : System.Address;  -- src/vulkan/vulkan.h:1923
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorSetAllocateInfo);  -- src/vulkan/vulkan.h:1918

   type VkDescriptorImageInfo is record
      sampler : VkSampler;  -- src/vulkan/vulkan.h:1927
      imageView : VkImageView;  -- src/vulkan/vulkan.h:1928
      imageLayout : aliased VkImageLayout;  -- src/vulkan/vulkan.h:1929
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorImageInfo);  -- src/vulkan/vulkan.h:1926

   type VkDescriptorBufferInfo is record
      buffer : VkBuffer;  -- src/vulkan/vulkan.h:1933
      offset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1934
      c_range : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:1935
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorBufferInfo);  -- src/vulkan/vulkan.h:1932

   type VkWriteDescriptorSet is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1939
      pNext : System.Address;  -- src/vulkan/vulkan.h:1940
      dstSet : VkDescriptorSet;  -- src/vulkan/vulkan.h:1941
      dstBinding : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1942
      dstArrayElement : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1943
      descriptorCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1944
      descriptorType : aliased VkDescriptorType;  -- src/vulkan/vulkan.h:1945
      pImageInfo : System.Address;  -- src/vulkan/vulkan.h:1946
      pBufferInfo : System.Address;  -- src/vulkan/vulkan.h:1947
      pTexelBufferView : System.Address;  -- src/vulkan/vulkan.h:1948
   end record;
   pragma Convention (C_Pass_By_Copy, VkWriteDescriptorSet);  -- src/vulkan/vulkan.h:1938

   type VkCopyDescriptorSet is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1952
      pNext : System.Address;  -- src/vulkan/vulkan.h:1953
      srcSet : VkDescriptorSet;  -- src/vulkan/vulkan.h:1954
      srcBinding : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1955
      srcArrayElement : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1956
      dstSet : VkDescriptorSet;  -- src/vulkan/vulkan.h:1957
      dstBinding : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1958
      dstArrayElement : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1959
      descriptorCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1960
   end record;
   pragma Convention (C_Pass_By_Copy, VkCopyDescriptorSet);  -- src/vulkan/vulkan.h:1951

   type VkFramebufferCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:1964
      pNext : System.Address;  -- src/vulkan/vulkan.h:1965
      flags : aliased VkFramebufferCreateFlags;  -- src/vulkan/vulkan.h:1966
      renderPass : VkRenderPass;  -- src/vulkan/vulkan.h:1967
      attachmentCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1968
      pAttachments : System.Address;  -- src/vulkan/vulkan.h:1969
      width : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1970
      height : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1971
      layers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1972
   end record;
   pragma Convention (C_Pass_By_Copy, VkFramebufferCreateInfo);  -- src/vulkan/vulkan.h:1963

   type VkAttachmentDescription is record
      flags : aliased VkAttachmentDescriptionFlags;  -- src/vulkan/vulkan.h:1976
      format : aliased VkFormat;  -- src/vulkan/vulkan.h:1977
      samples : aliased VkSampleCountFlagBits;  -- src/vulkan/vulkan.h:1978
      loadOp : aliased VkAttachmentLoadOp;  -- src/vulkan/vulkan.h:1979
      storeOp : aliased VkAttachmentStoreOp;  -- src/vulkan/vulkan.h:1980
      stencilLoadOp : aliased VkAttachmentLoadOp;  -- src/vulkan/vulkan.h:1981
      stencilStoreOp : aliased VkAttachmentStoreOp;  -- src/vulkan/vulkan.h:1982
      initialLayout : aliased VkImageLayout;  -- src/vulkan/vulkan.h:1983
      finalLayout : aliased VkImageLayout;  -- src/vulkan/vulkan.h:1984
   end record;
   pragma Convention (C_Pass_By_Copy, VkAttachmentDescription);  -- src/vulkan/vulkan.h:1975

   type VkAttachmentReference is record
      attachment : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1988
      layout : aliased VkImageLayout;  -- src/vulkan/vulkan.h:1989
   end record;
   pragma Convention (C_Pass_By_Copy, VkAttachmentReference);  -- src/vulkan/vulkan.h:1987

   type VkSubpassDescription is record
      flags : aliased VkSubpassDescriptionFlags;  -- src/vulkan/vulkan.h:1993
      pipelineBindPoint : aliased VkPipelineBindPoint;  -- src/vulkan/vulkan.h:1994
      inputAttachmentCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1995
      pInputAttachments : System.Address;  -- src/vulkan/vulkan.h:1996
      colorAttachmentCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:1997
      pColorAttachments : System.Address;  -- src/vulkan/vulkan.h:1998
      pResolveAttachments : System.Address;  -- src/vulkan/vulkan.h:1999
      pDepthStencilAttachment : System.Address;  -- src/vulkan/vulkan.h:2000
      preserveAttachmentCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2001
      pPreserveAttachments : access stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2002
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubpassDescription);  -- src/vulkan/vulkan.h:1992

   type VkSubpassDependency is record
      srcSubpass : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2006
      dstSubpass : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2007
      srcStageMask : aliased VkPipelineStageFlags;  -- src/vulkan/vulkan.h:2008
      dstStageMask : aliased VkPipelineStageFlags;  -- src/vulkan/vulkan.h:2009
      srcAccessMask : aliased VkAccessFlags;  -- src/vulkan/vulkan.h:2010
      dstAccessMask : aliased VkAccessFlags;  -- src/vulkan/vulkan.h:2011
      dependencyFlags : aliased VkDependencyFlags;  -- src/vulkan/vulkan.h:2012
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubpassDependency);  -- src/vulkan/vulkan.h:2005

   type VkRenderPassCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:2016
      pNext : System.Address;  -- src/vulkan/vulkan.h:2017
      flags : aliased VkRenderPassCreateFlags;  -- src/vulkan/vulkan.h:2018
      attachmentCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2019
      pAttachments : System.Address;  -- src/vulkan/vulkan.h:2020
      subpassCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2021
      pSubpasses : System.Address;  -- src/vulkan/vulkan.h:2022
      dependencyCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2023
      pDependencies : System.Address;  -- src/vulkan/vulkan.h:2024
   end record;
   pragma Convention (C_Pass_By_Copy, VkRenderPassCreateInfo);  -- src/vulkan/vulkan.h:2015

   type VkCommandPoolCreateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:2028
      pNext : System.Address;  -- src/vulkan/vulkan.h:2029
      flags : aliased VkCommandPoolCreateFlags;  -- src/vulkan/vulkan.h:2030
      queueFamilyIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2031
   end record;
   pragma Convention (C_Pass_By_Copy, VkCommandPoolCreateInfo);  -- src/vulkan/vulkan.h:2027

   type VkCommandBufferAllocateInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:2035
      pNext : System.Address;  -- src/vulkan/vulkan.h:2036
      commandPool : VkCommandPool;  -- src/vulkan/vulkan.h:2037
      level : aliased VkCommandBufferLevel;  -- src/vulkan/vulkan.h:2038
      commandBufferCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2039
   end record;
   pragma Convention (C_Pass_By_Copy, VkCommandBufferAllocateInfo);  -- src/vulkan/vulkan.h:2034

   type VkCommandBufferInheritanceInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:2043
      pNext : System.Address;  -- src/vulkan/vulkan.h:2044
      renderPass : VkRenderPass;  -- src/vulkan/vulkan.h:2045
      subpass : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2046
      framebuffer : VkFramebuffer;  -- src/vulkan/vulkan.h:2047
      occlusionQueryEnable : aliased VkBool32;  -- src/vulkan/vulkan.h:2048
      queryFlags : aliased VkQueryControlFlags;  -- src/vulkan/vulkan.h:2049
      pipelineStatistics : aliased VkQueryPipelineStatisticFlags;  -- src/vulkan/vulkan.h:2050
   end record;
   pragma Convention (C_Pass_By_Copy, VkCommandBufferInheritanceInfo);  -- src/vulkan/vulkan.h:2042

   type VkCommandBufferBeginInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:2054
      pNext : System.Address;  -- src/vulkan/vulkan.h:2055
      flags : aliased VkCommandBufferUsageFlags;  -- src/vulkan/vulkan.h:2056
      pInheritanceInfo : System.Address;  -- src/vulkan/vulkan.h:2057
   end record;
   pragma Convention (C_Pass_By_Copy, VkCommandBufferBeginInfo);  -- src/vulkan/vulkan.h:2053

   type VkBufferCopy is record
      srcOffset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:2061
      dstOffset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:2062
      size : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:2063
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferCopy);  -- src/vulkan/vulkan.h:2060

   type VkImageSubresourceLayers is record
      aspectMask : aliased VkImageAspectFlags;  -- src/vulkan/vulkan.h:2067
      mipLevel : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2068
      baseArrayLayer : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2069
      layerCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2070
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageSubresourceLayers);  -- src/vulkan/vulkan.h:2066

   type VkImageCopy is record
      srcSubresource : aliased VkImageSubresourceLayers;  -- src/vulkan/vulkan.h:2074
      srcOffset : aliased VkOffset3D;  -- src/vulkan/vulkan.h:2075
      dstSubresource : aliased VkImageSubresourceLayers;  -- src/vulkan/vulkan.h:2076
      dstOffset : aliased VkOffset3D;  -- src/vulkan/vulkan.h:2077
      extent : aliased VkExtent3D;  -- src/vulkan/vulkan.h:2078
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageCopy);  -- src/vulkan/vulkan.h:2073

   type VkImageBlit_srcOffsets_array is array (0 .. 1) of aliased VkOffset3D;
   type VkImageBlit_dstOffsets_array is array (0 .. 1) of aliased VkOffset3D;
   type VkImageBlit is record
      srcSubresource : aliased VkImageSubresourceLayers;  -- src/vulkan/vulkan.h:2082
      srcOffsets : aliased VkImageBlit_srcOffsets_array;  -- src/vulkan/vulkan.h:2083
      dstSubresource : aliased VkImageSubresourceLayers;  -- src/vulkan/vulkan.h:2084
      dstOffsets : aliased VkImageBlit_dstOffsets_array;  -- src/vulkan/vulkan.h:2085
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageBlit);  -- src/vulkan/vulkan.h:2081

   type VkBufferImageCopy is record
      bufferOffset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:2089
      bufferRowLength : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2090
      bufferImageHeight : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2091
      imageSubresource : aliased VkImageSubresourceLayers;  -- src/vulkan/vulkan.h:2092
      imageOffset : aliased VkOffset3D;  -- src/vulkan/vulkan.h:2093
      imageExtent : aliased VkExtent3D;  -- src/vulkan/vulkan.h:2094
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferImageCopy);  -- src/vulkan/vulkan.h:2088

   type VkClearColorValue_float32_array is array (0 .. 3) of aliased float;
   type VkClearColorValue_int32_array is array (0 .. 3) of aliased stdint_h.int32_t;
   type VkClearColorValue_uint32_array is array (0 .. 3) of aliased stdint_h.uint32_t;
   type VkClearColorValue (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            float32 : aliased VkClearColorValue_float32_array;  -- src/vulkan/vulkan.h:2098
         when 1 =>
            int32 : aliased VkClearColorValue_int32_array;  -- src/vulkan/vulkan.h:2099
         when others =>
            uint32 : aliased VkClearColorValue_uint32_array;  -- src/vulkan/vulkan.h:2100
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, VkClearColorValue);
   pragma Unchecked_Union (VkClearColorValue);  -- src/vulkan/vulkan.h:2097

   type VkClearDepthStencilValue is record
      depth : aliased float;  -- src/vulkan/vulkan.h:2104
      stencil : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2105
   end record;
   pragma Convention (C_Pass_By_Copy, VkClearDepthStencilValue);  -- src/vulkan/vulkan.h:2103

   type VkClearValue (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            color : VkClearColorValue;  -- src/vulkan/vulkan.h:2109
         when others =>
            depthStencil : aliased VkClearDepthStencilValue;  -- src/vulkan/vulkan.h:2110
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, VkClearValue);
   pragma Unchecked_Union (VkClearValue);  -- src/vulkan/vulkan.h:2108

   type VkClearAttachment is record
      aspectMask : aliased VkImageAspectFlags;  -- src/vulkan/vulkan.h:2114
      colorAttachment : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2115
      clearValue : VkClearValue;  -- src/vulkan/vulkan.h:2116
   end record;
   pragma Convention (C_Pass_By_Copy, VkClearAttachment);  -- src/vulkan/vulkan.h:2113

   type VkClearRect is record
      rect : aliased VkRect2D;  -- src/vulkan/vulkan.h:2120
      baseArrayLayer : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2121
      layerCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2122
   end record;
   pragma Convention (C_Pass_By_Copy, VkClearRect);  -- src/vulkan/vulkan.h:2119

   type VkImageResolve is record
      srcSubresource : aliased VkImageSubresourceLayers;  -- src/vulkan/vulkan.h:2126
      srcOffset : aliased VkOffset3D;  -- src/vulkan/vulkan.h:2127
      dstSubresource : aliased VkImageSubresourceLayers;  -- src/vulkan/vulkan.h:2128
      dstOffset : aliased VkOffset3D;  -- src/vulkan/vulkan.h:2129
      extent : aliased VkExtent3D;  -- src/vulkan/vulkan.h:2130
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageResolve);  -- src/vulkan/vulkan.h:2125

   type VkMemoryBarrier is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:2134
      pNext : System.Address;  -- src/vulkan/vulkan.h:2135
      srcAccessMask : aliased VkAccessFlags;  -- src/vulkan/vulkan.h:2136
      dstAccessMask : aliased VkAccessFlags;  -- src/vulkan/vulkan.h:2137
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryBarrier);  -- src/vulkan/vulkan.h:2133

   type VkBufferMemoryBarrier is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:2141
      pNext : System.Address;  -- src/vulkan/vulkan.h:2142
      srcAccessMask : aliased VkAccessFlags;  -- src/vulkan/vulkan.h:2143
      dstAccessMask : aliased VkAccessFlags;  -- src/vulkan/vulkan.h:2144
      srcQueueFamilyIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2145
      dstQueueFamilyIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2146
      buffer : VkBuffer;  -- src/vulkan/vulkan.h:2147
      offset : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:2148
      size : aliased VkDeviceSize;  -- src/vulkan/vulkan.h:2149
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferMemoryBarrier);  -- src/vulkan/vulkan.h:2140

   type VkImageMemoryBarrier is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:2153
      pNext : System.Address;  -- src/vulkan/vulkan.h:2154
      srcAccessMask : aliased VkAccessFlags;  -- src/vulkan/vulkan.h:2155
      dstAccessMask : aliased VkAccessFlags;  -- src/vulkan/vulkan.h:2156
      oldLayout : aliased VkImageLayout;  -- src/vulkan/vulkan.h:2157
      newLayout : aliased VkImageLayout;  -- src/vulkan/vulkan.h:2158
      srcQueueFamilyIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2159
      dstQueueFamilyIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2160
      image : VkImage;  -- src/vulkan/vulkan.h:2161
      subresourceRange : aliased VkImageSubresourceRange;  -- src/vulkan/vulkan.h:2162
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageMemoryBarrier);  -- src/vulkan/vulkan.h:2152

   type VkRenderPassBeginInfo is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:2166
      pNext : System.Address;  -- src/vulkan/vulkan.h:2167
      renderPass : VkRenderPass;  -- src/vulkan/vulkan.h:2168
      framebuffer : VkFramebuffer;  -- src/vulkan/vulkan.h:2169
      renderArea : aliased VkRect2D;  -- src/vulkan/vulkan.h:2170
      clearValueCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2171
      pClearValues : System.Address;  -- src/vulkan/vulkan.h:2172
   end record;
   pragma Convention (C_Pass_By_Copy, VkRenderPassBeginInfo);  -- src/vulkan/vulkan.h:2165

   type VkDispatchIndirectCommand is record
      x : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2176
      y : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2177
      z : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2178
   end record;
   pragma Convention (C_Pass_By_Copy, VkDispatchIndirectCommand);  -- src/vulkan/vulkan.h:2175

   type VkDrawIndexedIndirectCommand is record
      indexCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2182
      instanceCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2183
      firstIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2184
      vertexOffset : aliased stdint_h.int32_t;  -- src/vulkan/vulkan.h:2185
      firstInstance : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2186
   end record;
   pragma Convention (C_Pass_By_Copy, VkDrawIndexedIndirectCommand);  -- src/vulkan/vulkan.h:2181

   type VkDrawIndirectCommand is record
      vertexCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2190
      instanceCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2191
      firstVertex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2192
      firstInstance : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:2193
   end record;
   pragma Convention (C_Pass_By_Copy, VkDrawIndirectCommand);  -- src/vulkan/vulkan.h:2189

   type PFN_vkCreateInstance is access function
        (arg1 : System.Address;
         arg2 : System.Address;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateInstance);  -- src/vulkan/vulkan.h:2197

   type PFN_vkDestroyInstance is access procedure (arg1 : VkInstance; arg2 : System.Address);
   pragma Convention (C, PFN_vkDestroyInstance);  -- src/vulkan/vulkan.h:2198

   type PFN_vkEnumeratePhysicalDevices is access function
        (arg1 : VkInstance;
         arg2 : access stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkEnumeratePhysicalDevices);  -- src/vulkan/vulkan.h:2199

   type PFN_vkGetPhysicalDeviceFeatures is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceFeatures);
   pragma Convention (C, PFN_vkGetPhysicalDeviceFeatures);  -- src/vulkan/vulkan.h:2200

   type PFN_vkGetPhysicalDeviceFormatProperties is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : VkFormat;
         arg3 : access VkFormatProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceFormatProperties);  -- src/vulkan/vulkan.h:2201

   type PFN_vkGetPhysicalDeviceImageFormatProperties is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkFormat;
         arg3 : VkImageType;
         arg4 : VkImageTiling;
         arg5 : VkImageUsageFlags;
         arg6 : VkImageCreateFlags;
         arg7 : access VkImageFormatProperties) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceImageFormatProperties);  -- src/vulkan/vulkan.h:2202

   type PFN_vkGetPhysicalDeviceProperties is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceProperties);  -- src/vulkan/vulkan.h:2203

   type PFN_vkGetPhysicalDeviceQueueFamilyProperties is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkQueueFamilyProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceQueueFamilyProperties);  -- src/vulkan/vulkan.h:2204

   type PFN_vkGetPhysicalDeviceMemoryProperties is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceMemoryProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceMemoryProperties);  -- src/vulkan/vulkan.h:2205

   type PFN_vkGetInstanceProcAddr is access function (arg1 : VkInstance; arg2 : Interfaces.C.Strings.chars_ptr) return PFN_vkVoidFunction;
   pragma Convention (C, PFN_vkGetInstanceProcAddr);  -- src/vulkan/vulkan.h:2206

   type PFN_vkGetDeviceProcAddr is access function (arg1 : VkDevice; arg2 : Interfaces.C.Strings.chars_ptr) return PFN_vkVoidFunction;
   pragma Convention (C, PFN_vkGetDeviceProcAddr);  -- src/vulkan/vulkan.h:2207

   type PFN_vkCreateDevice is access function
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDevice);  -- src/vulkan/vulkan.h:2208

   type PFN_vkDestroyDevice is access procedure (arg1 : VkDevice; arg2 : System.Address);
   pragma Convention (C, PFN_vkDestroyDevice);  -- src/vulkan/vulkan.h:2209

   type PFN_vkEnumerateInstanceExtensionProperties is access function
        (arg1 : Interfaces.C.Strings.chars_ptr;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkExtensionProperties) return VkResult;
   pragma Convention (C, PFN_vkEnumerateInstanceExtensionProperties);  -- src/vulkan/vulkan.h:2210

   type PFN_vkEnumerateDeviceExtensionProperties is access function
        (arg1 : VkPhysicalDevice;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkExtensionProperties) return VkResult;
   pragma Convention (C, PFN_vkEnumerateDeviceExtensionProperties);  -- src/vulkan/vulkan.h:2211

   type PFN_vkEnumerateInstanceLayerProperties is access function (arg1 : access stdint_h.uint32_t; arg2 : access VkLayerProperties) return VkResult;
   pragma Convention (C, PFN_vkEnumerateInstanceLayerProperties);  -- src/vulkan/vulkan.h:2212

   type PFN_vkEnumerateDeviceLayerProperties is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkLayerProperties) return VkResult;
   pragma Convention (C, PFN_vkEnumerateDeviceLayerProperties);  -- src/vulkan/vulkan.h:2213

   type PFN_vkGetDeviceQueue is access procedure
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkGetDeviceQueue);  -- src/vulkan/vulkan.h:2214

   type PFN_vkQueueSubmit is access function
        (arg1 : VkQueue;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkFence) return VkResult;
   pragma Convention (C, PFN_vkQueueSubmit);  -- src/vulkan/vulkan.h:2215

   type PFN_vkQueueWaitIdle is access function (arg1 : VkQueue) return VkResult;
   pragma Convention (C, PFN_vkQueueWaitIdle);  -- src/vulkan/vulkan.h:2216

   type PFN_vkDeviceWaitIdle is access function (arg1 : VkDevice) return VkResult;
   pragma Convention (C, PFN_vkDeviceWaitIdle);  -- src/vulkan/vulkan.h:2217

   type PFN_vkAllocateMemory is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkAllocateMemory);  -- src/vulkan/vulkan.h:2218

   type PFN_vkFreeMemory is access procedure
        (arg1 : VkDevice;
         arg2 : VkDeviceMemory;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkFreeMemory);  -- src/vulkan/vulkan.h:2219

   type PFN_vkMapMemory is access function
        (arg1 : VkDevice;
         arg2 : VkDeviceMemory;
         arg3 : VkDeviceSize;
         arg4 : VkDeviceSize;
         arg5 : VkMemoryMapFlags;
         arg6 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkMapMemory);  -- src/vulkan/vulkan.h:2220

   type PFN_vkUnmapMemory is access procedure (arg1 : VkDevice; arg2 : VkDeviceMemory);
   pragma Convention (C, PFN_vkUnmapMemory);  -- src/vulkan/vulkan.h:2221

   type PFN_vkFlushMappedMemoryRanges is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkFlushMappedMemoryRanges);  -- src/vulkan/vulkan.h:2222

   type PFN_vkInvalidateMappedMemoryRanges is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkInvalidateMappedMemoryRanges);  -- src/vulkan/vulkan.h:2223

   type PFN_vkGetDeviceMemoryCommitment is access procedure
        (arg1 : VkDevice;
         arg2 : VkDeviceMemory;
         arg3 : access VkDeviceSize);
   pragma Convention (C, PFN_vkGetDeviceMemoryCommitment);  -- src/vulkan/vulkan.h:2224

   type PFN_vkBindBufferMemory is access function
        (arg1 : VkDevice;
         arg2 : VkBuffer;
         arg3 : VkDeviceMemory;
         arg4 : VkDeviceSize) return VkResult;
   pragma Convention (C, PFN_vkBindBufferMemory);  -- src/vulkan/vulkan.h:2225

   type PFN_vkBindImageMemory is access function
        (arg1 : VkDevice;
         arg2 : VkImage;
         arg3 : VkDeviceMemory;
         arg4 : VkDeviceSize) return VkResult;
   pragma Convention (C, PFN_vkBindImageMemory);  -- src/vulkan/vulkan.h:2226

   type PFN_vkGetBufferMemoryRequirements is access procedure
        (arg1 : VkDevice;
         arg2 : VkBuffer;
         arg3 : access VkMemoryRequirements);
   pragma Convention (C, PFN_vkGetBufferMemoryRequirements);  -- src/vulkan/vulkan.h:2227

   type PFN_vkGetImageMemoryRequirements is access procedure
        (arg1 : VkDevice;
         arg2 : VkImage;
         arg3 : access VkMemoryRequirements);
   pragma Convention (C, PFN_vkGetImageMemoryRequirements);  -- src/vulkan/vulkan.h:2228

   type PFN_vkGetImageSparseMemoryRequirements is access procedure
        (arg1 : VkDevice;
         arg2 : VkImage;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkSparseImageMemoryRequirements);
   pragma Convention (C, PFN_vkGetImageSparseMemoryRequirements);  -- src/vulkan/vulkan.h:2229

   type PFN_vkGetPhysicalDeviceSparseImageFormatProperties is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : VkFormat;
         arg3 : VkImageType;
         arg4 : VkSampleCountFlagBits;
         arg5 : VkImageUsageFlags;
         arg6 : VkImageTiling;
         arg7 : access stdint_h.uint32_t;
         arg8 : access VkSparseImageFormatProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceSparseImageFormatProperties);  -- src/vulkan/vulkan.h:2230

   type PFN_vkQueueBindSparse is access function
        (arg1 : VkQueue;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkFence) return VkResult;
   pragma Convention (C, PFN_vkQueueBindSparse);  -- src/vulkan/vulkan.h:2231

   type PFN_vkCreateFence is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateFence);  -- src/vulkan/vulkan.h:2232

   type PFN_vkDestroyFence is access procedure
        (arg1 : VkDevice;
         arg2 : VkFence;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyFence);  -- src/vulkan/vulkan.h:2233

   type PFN_vkResetFences is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkResetFences);  -- src/vulkan/vulkan.h:2234

   type PFN_vkGetFenceStatus is access function (arg1 : VkDevice; arg2 : VkFence) return VkResult;
   pragma Convention (C, PFN_vkGetFenceStatus);  -- src/vulkan/vulkan.h:2235

   type PFN_vkWaitForFences is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkBool32;
         arg5 : stdint_h.uint64_t) return VkResult;
   pragma Convention (C, PFN_vkWaitForFences);  -- src/vulkan/vulkan.h:2236

   type PFN_vkCreateSemaphore is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateSemaphore);  -- src/vulkan/vulkan.h:2237

   type PFN_vkDestroySemaphore is access procedure
        (arg1 : VkDevice;
         arg2 : VkSemaphore;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroySemaphore);  -- src/vulkan/vulkan.h:2238

   type PFN_vkCreateEvent is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateEvent);  -- src/vulkan/vulkan.h:2239

   type PFN_vkDestroyEvent is access procedure
        (arg1 : VkDevice;
         arg2 : VkEvent;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyEvent);  -- src/vulkan/vulkan.h:2240

   type PFN_vkGetEventStatus is access function (arg1 : VkDevice; arg2 : VkEvent) return VkResult;
   pragma Convention (C, PFN_vkGetEventStatus);  -- src/vulkan/vulkan.h:2241

   type PFN_vkSetEvent is access function (arg1 : VkDevice; arg2 : VkEvent) return VkResult;
   pragma Convention (C, PFN_vkSetEvent);  -- src/vulkan/vulkan.h:2242

   type PFN_vkResetEvent is access function (arg1 : VkDevice; arg2 : VkEvent) return VkResult;
   pragma Convention (C, PFN_vkResetEvent);  -- src/vulkan/vulkan.h:2243

   type PFN_vkCreateQueryPool is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateQueryPool);  -- src/vulkan/vulkan.h:2244

   type PFN_vkDestroyQueryPool is access procedure
        (arg1 : VkDevice;
         arg2 : VkQueryPool;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyQueryPool);  -- src/vulkan/vulkan.h:2245

   type PFN_vkGetQueryPoolResults is access function
        (arg1 : VkDevice;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : stddef_h.size_t;
         arg6 : System.Address;
         arg7 : VkDeviceSize;
         arg8 : VkQueryResultFlags) return VkResult;
   pragma Convention (C, PFN_vkGetQueryPoolResults);  -- src/vulkan/vulkan.h:2246

   type PFN_vkCreateBuffer is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateBuffer);  -- src/vulkan/vulkan.h:2247

   type PFN_vkDestroyBuffer is access procedure
        (arg1 : VkDevice;
         arg2 : VkBuffer;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyBuffer);  -- src/vulkan/vulkan.h:2248

   type PFN_vkCreateBufferView is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateBufferView);  -- src/vulkan/vulkan.h:2249

   type PFN_vkDestroyBufferView is access procedure
        (arg1 : VkDevice;
         arg2 : VkBufferView;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyBufferView);  -- src/vulkan/vulkan.h:2250

   type PFN_vkCreateImage is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateImage);  -- src/vulkan/vulkan.h:2251

   type PFN_vkDestroyImage is access procedure
        (arg1 : VkDevice;
         arg2 : VkImage;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyImage);  -- src/vulkan/vulkan.h:2252

   type PFN_vkGetImageSubresourceLayout is access procedure
        (arg1 : VkDevice;
         arg2 : VkImage;
         arg3 : System.Address;
         arg4 : access VkSubresourceLayout);
   pragma Convention (C, PFN_vkGetImageSubresourceLayout);  -- src/vulkan/vulkan.h:2253

   type PFN_vkCreateImageView is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateImageView);  -- src/vulkan/vulkan.h:2254

   type PFN_vkDestroyImageView is access procedure
        (arg1 : VkDevice;
         arg2 : VkImageView;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyImageView);  -- src/vulkan/vulkan.h:2255

   type PFN_vkCreateShaderModule is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateShaderModule);  -- src/vulkan/vulkan.h:2256

   type PFN_vkDestroyShaderModule is access procedure
        (arg1 : VkDevice;
         arg2 : VkShaderModule;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyShaderModule);  -- src/vulkan/vulkan.h:2257

   type PFN_vkCreatePipelineCache is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreatePipelineCache);  -- src/vulkan/vulkan.h:2258

   type PFN_vkDestroyPipelineCache is access procedure
        (arg1 : VkDevice;
         arg2 : VkPipelineCache;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyPipelineCache);  -- src/vulkan/vulkan.h:2259

   type PFN_vkGetPipelineCacheData is access function
        (arg1 : VkDevice;
         arg2 : VkPipelineCache;
         arg3 : access stddef_h.size_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetPipelineCacheData);  -- src/vulkan/vulkan.h:2260

   type PFN_vkMergePipelineCaches is access function
        (arg1 : VkDevice;
         arg2 : VkPipelineCache;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkMergePipelineCaches);  -- src/vulkan/vulkan.h:2261

   type PFN_vkCreateGraphicsPipelines is access function
        (arg1 : VkDevice;
         arg2 : VkPipelineCache;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : System.Address;
         arg6 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateGraphicsPipelines);  -- src/vulkan/vulkan.h:2262

   type PFN_vkCreateComputePipelines is access function
        (arg1 : VkDevice;
         arg2 : VkPipelineCache;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : System.Address;
         arg6 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateComputePipelines);  -- src/vulkan/vulkan.h:2263

   type PFN_vkDestroyPipeline is access procedure
        (arg1 : VkDevice;
         arg2 : VkPipeline;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyPipeline);  -- src/vulkan/vulkan.h:2264

   type PFN_vkCreatePipelineLayout is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreatePipelineLayout);  -- src/vulkan/vulkan.h:2265

   type PFN_vkDestroyPipelineLayout is access procedure
        (arg1 : VkDevice;
         arg2 : VkPipelineLayout;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyPipelineLayout);  -- src/vulkan/vulkan.h:2266

   type PFN_vkCreateSampler is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateSampler);  -- src/vulkan/vulkan.h:2267

   type PFN_vkDestroySampler is access procedure
        (arg1 : VkDevice;
         arg2 : VkSampler;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroySampler);  -- src/vulkan/vulkan.h:2268

   type PFN_vkCreateDescriptorSetLayout is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDescriptorSetLayout);  -- src/vulkan/vulkan.h:2269

   type PFN_vkDestroyDescriptorSetLayout is access procedure
        (arg1 : VkDevice;
         arg2 : VkDescriptorSetLayout;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyDescriptorSetLayout);  -- src/vulkan/vulkan.h:2270

   type PFN_vkCreateDescriptorPool is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDescriptorPool);  -- src/vulkan/vulkan.h:2271

   type PFN_vkDestroyDescriptorPool is access procedure
        (arg1 : VkDevice;
         arg2 : VkDescriptorPool;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyDescriptorPool);  -- src/vulkan/vulkan.h:2272

   type PFN_vkResetDescriptorPool is access function
        (arg1 : VkDevice;
         arg2 : VkDescriptorPool;
         arg3 : VkDescriptorPoolResetFlags) return VkResult;
   pragma Convention (C, PFN_vkResetDescriptorPool);  -- src/vulkan/vulkan.h:2273

   type PFN_vkAllocateDescriptorSets is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkAllocateDescriptorSets);  -- src/vulkan/vulkan.h:2274

   type PFN_vkFreeDescriptorSets is access function
        (arg1 : VkDevice;
         arg2 : VkDescriptorPool;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkFreeDescriptorSets);  -- src/vulkan/vulkan.h:2275

   type PFN_vkUpdateDescriptorSets is access procedure
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : stdint_h.uint32_t;
         arg5 : System.Address);
   pragma Convention (C, PFN_vkUpdateDescriptorSets);  -- src/vulkan/vulkan.h:2276

   type PFN_vkCreateFramebuffer is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateFramebuffer);  -- src/vulkan/vulkan.h:2277

   type PFN_vkDestroyFramebuffer is access procedure
        (arg1 : VkDevice;
         arg2 : VkFramebuffer;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyFramebuffer);  -- src/vulkan/vulkan.h:2278

   type PFN_vkCreateRenderPass is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateRenderPass);  -- src/vulkan/vulkan.h:2279

   type PFN_vkDestroyRenderPass is access procedure
        (arg1 : VkDevice;
         arg2 : VkRenderPass;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyRenderPass);  -- src/vulkan/vulkan.h:2280

   type PFN_vkGetRenderAreaGranularity is access procedure
        (arg1 : VkDevice;
         arg2 : VkRenderPass;
         arg3 : access VkExtent2D);
   pragma Convention (C, PFN_vkGetRenderAreaGranularity);  -- src/vulkan/vulkan.h:2281

   type PFN_vkCreateCommandPool is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateCommandPool);  -- src/vulkan/vulkan.h:2282

   type PFN_vkDestroyCommandPool is access procedure
        (arg1 : VkDevice;
         arg2 : VkCommandPool;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyCommandPool);  -- src/vulkan/vulkan.h:2283

   type PFN_vkResetCommandPool is access function
        (arg1 : VkDevice;
         arg2 : VkCommandPool;
         arg3 : VkCommandPoolResetFlags) return VkResult;
   pragma Convention (C, PFN_vkResetCommandPool);  -- src/vulkan/vulkan.h:2284

   type PFN_vkAllocateCommandBuffers is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkAllocateCommandBuffers);  -- src/vulkan/vulkan.h:2285

   type PFN_vkFreeCommandBuffers is access procedure
        (arg1 : VkDevice;
         arg2 : VkCommandPool;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkFreeCommandBuffers);  -- src/vulkan/vulkan.h:2286

   type PFN_vkBeginCommandBuffer is access function (arg1 : VkCommandBuffer; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkBeginCommandBuffer);  -- src/vulkan/vulkan.h:2287

   type PFN_vkEndCommandBuffer is access function (arg1 : VkCommandBuffer) return VkResult;
   pragma Convention (C, PFN_vkEndCommandBuffer);  -- src/vulkan/vulkan.h:2288

   type PFN_vkResetCommandBuffer is access function (arg1 : VkCommandBuffer; arg2 : VkCommandBufferResetFlags) return VkResult;
   pragma Convention (C, PFN_vkResetCommandBuffer);  -- src/vulkan/vulkan.h:2289

   type PFN_vkCmdBindPipeline is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineBindPoint;
         arg3 : VkPipeline);
   pragma Convention (C, PFN_vkCmdBindPipeline);  -- src/vulkan/vulkan.h:2290

   type PFN_vkCmdSetViewport is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkCmdSetViewport);  -- src/vulkan/vulkan.h:2291

   type PFN_vkCmdSetScissor is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkCmdSetScissor);  -- src/vulkan/vulkan.h:2292

   type PFN_vkCmdSetLineWidth is access procedure (arg1 : VkCommandBuffer; arg2 : float);
   pragma Convention (C, PFN_vkCmdSetLineWidth);  -- src/vulkan/vulkan.h:2293

   type PFN_vkCmdSetDepthBias is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : float;
         arg3 : float;
         arg4 : float);
   pragma Convention (C, PFN_vkCmdSetDepthBias);  -- src/vulkan/vulkan.h:2294

   type PFN_vkCmdSetBlendConstants is access procedure (arg1 : VkCommandBuffer; arg2 : access float);
   pragma Convention (C, PFN_vkCmdSetBlendConstants);  -- src/vulkan/vulkan.h:2295

   type PFN_vkCmdSetDepthBounds is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : float;
         arg3 : float);
   pragma Convention (C, PFN_vkCmdSetDepthBounds);  -- src/vulkan/vulkan.h:2296

   type PFN_vkCmdSetStencilCompareMask is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkStencilFaceFlags;
         arg3 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdSetStencilCompareMask);  -- src/vulkan/vulkan.h:2297

   type PFN_vkCmdSetStencilWriteMask is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkStencilFaceFlags;
         arg3 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdSetStencilWriteMask);  -- src/vulkan/vulkan.h:2298

   type PFN_vkCmdSetStencilReference is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkStencilFaceFlags;
         arg3 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdSetStencilReference);  -- src/vulkan/vulkan.h:2299

   type PFN_vkCmdBindDescriptorSets is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineBindPoint;
         arg3 : VkPipelineLayout;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address;
         arg7 : stdint_h.uint32_t;
         arg8 : access stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdBindDescriptorSets);  -- src/vulkan/vulkan.h:2300

   type PFN_vkCmdBindIndexBuffer is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkIndexType);
   pragma Convention (C, PFN_vkCmdBindIndexBuffer);  -- src/vulkan/vulkan.h:2301

   type PFN_vkCmdBindVertexBuffers is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : access VkDeviceSize);
   pragma Convention (C, PFN_vkCmdBindVertexBuffers);  -- src/vulkan/vulkan.h:2302

   type PFN_vkCmdDraw is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDraw);  -- src/vulkan/vulkan.h:2303

   type PFN_vkCmdDrawIndexed is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.int32_t;
         arg6 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndexed);  -- src/vulkan/vulkan.h:2304

   type PFN_vkCmdDrawIndirect is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndirect);  -- src/vulkan/vulkan.h:2305

   type PFN_vkCmdDrawIndexedIndirect is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndexedIndirect);  -- src/vulkan/vulkan.h:2306

   type PFN_vkCmdDispatch is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDispatch);  -- src/vulkan/vulkan.h:2307

   type PFN_vkCmdDispatchIndirect is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize);
   pragma Convention (C, PFN_vkCmdDispatchIndirect);  -- src/vulkan/vulkan.h:2308

   type PFN_vkCmdCopyBuffer is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkBuffer;
         arg4 : stdint_h.uint32_t;
         arg5 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyBuffer);  -- src/vulkan/vulkan.h:2309

   type PFN_vkCmdCopyImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : VkImage;
         arg5 : VkImageLayout;
         arg6 : stdint_h.uint32_t;
         arg7 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyImage);  -- src/vulkan/vulkan.h:2310

   type PFN_vkCmdBlitImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : VkImage;
         arg5 : VkImageLayout;
         arg6 : stdint_h.uint32_t;
         arg7 : System.Address;
         arg8 : VkFilter);
   pragma Convention (C, PFN_vkCmdBlitImage);  -- src/vulkan/vulkan.h:2311

   type PFN_vkCmdCopyBufferToImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkImage;
         arg4 : VkImageLayout;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyBufferToImage);  -- src/vulkan/vulkan.h:2312

   type PFN_vkCmdCopyImageToBuffer is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : VkBuffer;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyImageToBuffer);  -- src/vulkan/vulkan.h:2313

   type PFN_vkCmdUpdateBuffer is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkDeviceSize;
         arg5 : access stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdUpdateBuffer);  -- src/vulkan/vulkan.h:2314

   type PFN_vkCmdFillBuffer is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkDeviceSize;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdFillBuffer);  -- src/vulkan/vulkan.h:2315

   type PFN_vkCmdClearColorImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : System.Address;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdClearColorImage);  -- src/vulkan/vulkan.h:2316

   type PFN_vkCmdClearDepthStencilImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : System.Address;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdClearDepthStencilImage);  -- src/vulkan/vulkan.h:2317

   type PFN_vkCmdClearAttachments is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : stdint_h.uint32_t;
         arg5 : System.Address);
   pragma Convention (C, PFN_vkCmdClearAttachments);  -- src/vulkan/vulkan.h:2318

   type PFN_vkCmdResolveImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : VkImage;
         arg5 : VkImageLayout;
         arg6 : stdint_h.uint32_t;
         arg7 : System.Address);
   pragma Convention (C, PFN_vkCmdResolveImage);  -- src/vulkan/vulkan.h:2319

   type PFN_vkCmdSetEvent is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkEvent;
         arg3 : VkPipelineStageFlags);
   pragma Convention (C, PFN_vkCmdSetEvent);  -- src/vulkan/vulkan.h:2320

   type PFN_vkCmdResetEvent is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkEvent;
         arg3 : VkPipelineStageFlags);
   pragma Convention (C, PFN_vkCmdResetEvent);  -- src/vulkan/vulkan.h:2321

   type PFN_vkCmdWaitEvents is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkPipelineStageFlags;
         arg5 : VkPipelineStageFlags;
         arg6 : stdint_h.uint32_t;
         arg7 : System.Address;
         arg8 : stdint_h.uint32_t;
         arg9 : System.Address;
         arg10 : stdint_h.uint32_t;
         arg11 : System.Address);
   pragma Convention (C, PFN_vkCmdWaitEvents);  -- src/vulkan/vulkan.h:2322

   type PFN_vkCmdPipelineBarrier is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineStageFlags;
         arg3 : VkPipelineStageFlags;
         arg4 : VkDependencyFlags;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address;
         arg7 : stdint_h.uint32_t;
         arg8 : System.Address;
         arg9 : stdint_h.uint32_t;
         arg10 : System.Address);
   pragma Convention (C, PFN_vkCmdPipelineBarrier);  -- src/vulkan/vulkan.h:2323

   type PFN_vkCmdBeginQuery is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : VkQueryControlFlags);
   pragma Convention (C, PFN_vkCmdBeginQuery);  -- src/vulkan/vulkan.h:2324

   type PFN_vkCmdEndQuery is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdEndQuery);  -- src/vulkan/vulkan.h:2325

   type PFN_vkCmdResetQueryPool is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdResetQueryPool);  -- src/vulkan/vulkan.h:2326

   type PFN_vkCmdWriteTimestamp is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineStageFlagBits;
         arg3 : VkQueryPool;
         arg4 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdWriteTimestamp);  -- src/vulkan/vulkan.h:2327

   type PFN_vkCmdCopyQueryPoolResults is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : VkBuffer;
         arg6 : VkDeviceSize;
         arg7 : VkDeviceSize;
         arg8 : VkQueryResultFlags);
   pragma Convention (C, PFN_vkCmdCopyQueryPoolResults);  -- src/vulkan/vulkan.h:2328

   type PFN_vkCmdPushConstants is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineLayout;
         arg3 : VkShaderStageFlags;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdPushConstants);  -- src/vulkan/vulkan.h:2329

   type PFN_vkCmdBeginRenderPass is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : System.Address;
         arg3 : VkSubpassContents);
   pragma Convention (C, PFN_vkCmdBeginRenderPass);  -- src/vulkan/vulkan.h:2330

   type PFN_vkCmdNextSubpass is access procedure (arg1 : VkCommandBuffer; arg2 : VkSubpassContents);
   pragma Convention (C, PFN_vkCmdNextSubpass);  -- src/vulkan/vulkan.h:2331

   type PFN_vkCmdEndRenderPass is access procedure (arg1 : VkCommandBuffer);
   pragma Convention (C, PFN_vkCmdEndRenderPass);  -- src/vulkan/vulkan.h:2332

   type PFN_vkCmdExecuteCommands is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdExecuteCommands);  -- src/vulkan/vulkan.h:2333

   function vkCreateInstance
     (pCreateInfo : System.Address;
      pAllocator : System.Address;
      pInstance : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2336
   pragma Import (C, vkCreateInstance, "vkCreateInstance");

   procedure vkDestroyInstance (instance : VkInstance; pAllocator : System.Address);  -- src/vulkan/vulkan.h:2341
   pragma Import (C, vkDestroyInstance, "vkDestroyInstance");

   function vkEnumeratePhysicalDevices
     (instance : VkInstance;
      pPhysicalDeviceCount : access stdint_h.uint32_t;
      pPhysicalDevices : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2345
   pragma Import (C, vkEnumeratePhysicalDevices, "vkEnumeratePhysicalDevices");

   procedure vkGetPhysicalDeviceFeatures (physicalDevice : VkPhysicalDevice; pFeatures : access VkPhysicalDeviceFeatures);  -- src/vulkan/vulkan.h:2350
   pragma Import (C, vkGetPhysicalDeviceFeatures, "vkGetPhysicalDeviceFeatures");

   procedure vkGetPhysicalDeviceFormatProperties
     (physicalDevice : VkPhysicalDevice;
      format : VkFormat;
      pFormatProperties : access VkFormatProperties);  -- src/vulkan/vulkan.h:2354
   pragma Import (C, vkGetPhysicalDeviceFormatProperties, "vkGetPhysicalDeviceFormatProperties");

   function vkGetPhysicalDeviceImageFormatProperties
     (physicalDevice : VkPhysicalDevice;
      format : VkFormat;
      c_type : VkImageType;
      tiling : VkImageTiling;
      usage : VkImageUsageFlags;
      flags : VkImageCreateFlags;
      pImageFormatProperties : access VkImageFormatProperties) return VkResult;  -- src/vulkan/vulkan.h:2359
   pragma Import (C, vkGetPhysicalDeviceImageFormatProperties, "vkGetPhysicalDeviceImageFormatProperties");

   procedure vkGetPhysicalDeviceProperties (physicalDevice : VkPhysicalDevice; pProperties : access VkPhysicalDeviceProperties);  -- src/vulkan/vulkan.h:2368
   pragma Import (C, vkGetPhysicalDeviceProperties, "vkGetPhysicalDeviceProperties");

   procedure vkGetPhysicalDeviceQueueFamilyProperties
     (physicalDevice : VkPhysicalDevice;
      pQueueFamilyPropertyCount : access stdint_h.uint32_t;
      pQueueFamilyProperties : access VkQueueFamilyProperties);  -- src/vulkan/vulkan.h:2372
   pragma Import (C, vkGetPhysicalDeviceQueueFamilyProperties, "vkGetPhysicalDeviceQueueFamilyProperties");

   procedure vkGetPhysicalDeviceMemoryProperties (physicalDevice : VkPhysicalDevice; pMemoryProperties : access VkPhysicalDeviceMemoryProperties);  -- src/vulkan/vulkan.h:2377
   pragma Import (C, vkGetPhysicalDeviceMemoryProperties, "vkGetPhysicalDeviceMemoryProperties");

   function vkGetInstanceProcAddr (instance : VkInstance; pName : Interfaces.C.Strings.chars_ptr) return PFN_vkVoidFunction;  -- src/vulkan/vulkan.h:2381
   pragma Import (C, vkGetInstanceProcAddr, "vkGetInstanceProcAddr");

   function vkGetDeviceProcAddr (device : VkDevice; pName : Interfaces.C.Strings.chars_ptr) return PFN_vkVoidFunction;  -- src/vulkan/vulkan.h:2385
   pragma Import (C, vkGetDeviceProcAddr, "vkGetDeviceProcAddr");

   function vkCreateDevice
     (physicalDevice : VkPhysicalDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pDevice : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2389
   pragma Import (C, vkCreateDevice, "vkCreateDevice");

   procedure vkDestroyDevice (device : VkDevice; pAllocator : System.Address);  -- src/vulkan/vulkan.h:2395
   pragma Import (C, vkDestroyDevice, "vkDestroyDevice");

   function vkEnumerateInstanceExtensionProperties
     (pLayerName : Interfaces.C.Strings.chars_ptr;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkExtensionProperties) return VkResult;  -- src/vulkan/vulkan.h:2399
   pragma Import (C, vkEnumerateInstanceExtensionProperties, "vkEnumerateInstanceExtensionProperties");

   function vkEnumerateDeviceExtensionProperties
     (physicalDevice : VkPhysicalDevice;
      pLayerName : Interfaces.C.Strings.chars_ptr;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkExtensionProperties) return VkResult;  -- src/vulkan/vulkan.h:2404
   pragma Import (C, vkEnumerateDeviceExtensionProperties, "vkEnumerateDeviceExtensionProperties");

   function vkEnumerateInstanceLayerProperties (pPropertyCount : access stdint_h.uint32_t; pProperties : access VkLayerProperties) return VkResult;  -- src/vulkan/vulkan.h:2410
   pragma Import (C, vkEnumerateInstanceLayerProperties, "vkEnumerateInstanceLayerProperties");

   function vkEnumerateDeviceLayerProperties
     (physicalDevice : VkPhysicalDevice;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkLayerProperties) return VkResult;  -- src/vulkan/vulkan.h:2414
   pragma Import (C, vkEnumerateDeviceLayerProperties, "vkEnumerateDeviceLayerProperties");

   procedure vkGetDeviceQueue
     (device : VkDevice;
      queueFamilyIndex : stdint_h.uint32_t;
      queueIndex : stdint_h.uint32_t;
      pQueue : System.Address);  -- src/vulkan/vulkan.h:2419
   pragma Import (C, vkGetDeviceQueue, "vkGetDeviceQueue");

   function vkQueueSubmit
     (queue : VkQueue;
      submitCount : stdint_h.uint32_t;
      pSubmits : System.Address;
      fence : VkFence) return VkResult;  -- src/vulkan/vulkan.h:2425
   pragma Import (C, vkQueueSubmit, "vkQueueSubmit");

   function vkQueueWaitIdle (queue : VkQueue) return VkResult;  -- src/vulkan/vulkan.h:2431
   pragma Import (C, vkQueueWaitIdle, "vkQueueWaitIdle");

   function vkDeviceWaitIdle (device : VkDevice) return VkResult;  -- src/vulkan/vulkan.h:2434
   pragma Import (C, vkDeviceWaitIdle, "vkDeviceWaitIdle");

   function vkAllocateMemory
     (device : VkDevice;
      pAllocateInfo : System.Address;
      pAllocator : System.Address;
      pMemory : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2437
   pragma Import (C, vkAllocateMemory, "vkAllocateMemory");

   procedure vkFreeMemory
     (device : VkDevice;
      memory : VkDeviceMemory;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2443
   pragma Import (C, vkFreeMemory, "vkFreeMemory");

   function vkMapMemory
     (device : VkDevice;
      memory : VkDeviceMemory;
      offset : VkDeviceSize;
      size : VkDeviceSize;
      flags : VkMemoryMapFlags;
      ppData : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2448
   pragma Import (C, vkMapMemory, "vkMapMemory");

   procedure vkUnmapMemory (device : VkDevice; memory : VkDeviceMemory);  -- src/vulkan/vulkan.h:2456
   pragma Import (C, vkUnmapMemory, "vkUnmapMemory");

   function vkFlushMappedMemoryRanges
     (device : VkDevice;
      memoryRangeCount : stdint_h.uint32_t;
      pMemoryRanges : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2460
   pragma Import (C, vkFlushMappedMemoryRanges, "vkFlushMappedMemoryRanges");

   function vkInvalidateMappedMemoryRanges
     (device : VkDevice;
      memoryRangeCount : stdint_h.uint32_t;
      pMemoryRanges : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2465
   pragma Import (C, vkInvalidateMappedMemoryRanges, "vkInvalidateMappedMemoryRanges");

   procedure vkGetDeviceMemoryCommitment
     (device : VkDevice;
      memory : VkDeviceMemory;
      pCommittedMemoryInBytes : access VkDeviceSize);  -- src/vulkan/vulkan.h:2470
   pragma Import (C, vkGetDeviceMemoryCommitment, "vkGetDeviceMemoryCommitment");

   function vkBindBufferMemory
     (device : VkDevice;
      buffer : VkBuffer;
      memory : VkDeviceMemory;
      memoryOffset : VkDeviceSize) return VkResult;  -- src/vulkan/vulkan.h:2475
   pragma Import (C, vkBindBufferMemory, "vkBindBufferMemory");

   function vkBindImageMemory
     (device : VkDevice;
      image : VkImage;
      memory : VkDeviceMemory;
      memoryOffset : VkDeviceSize) return VkResult;  -- src/vulkan/vulkan.h:2481
   pragma Import (C, vkBindImageMemory, "vkBindImageMemory");

   procedure vkGetBufferMemoryRequirements
     (device : VkDevice;
      buffer : VkBuffer;
      pMemoryRequirements : access VkMemoryRequirements);  -- src/vulkan/vulkan.h:2487
   pragma Import (C, vkGetBufferMemoryRequirements, "vkGetBufferMemoryRequirements");

   procedure vkGetImageMemoryRequirements
     (device : VkDevice;
      image : VkImage;
      pMemoryRequirements : access VkMemoryRequirements);  -- src/vulkan/vulkan.h:2492
   pragma Import (C, vkGetImageMemoryRequirements, "vkGetImageMemoryRequirements");

   procedure vkGetImageSparseMemoryRequirements
     (device : VkDevice;
      image : VkImage;
      pSparseMemoryRequirementCount : access stdint_h.uint32_t;
      pSparseMemoryRequirements : access VkSparseImageMemoryRequirements);  -- src/vulkan/vulkan.h:2497
   pragma Import (C, vkGetImageSparseMemoryRequirements, "vkGetImageSparseMemoryRequirements");

   procedure vkGetPhysicalDeviceSparseImageFormatProperties
     (physicalDevice : VkPhysicalDevice;
      format : VkFormat;
      c_type : VkImageType;
      samples : VkSampleCountFlagBits;
      usage : VkImageUsageFlags;
      tiling : VkImageTiling;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkSparseImageFormatProperties);  -- src/vulkan/vulkan.h:2503
   pragma Import (C, vkGetPhysicalDeviceSparseImageFormatProperties, "vkGetPhysicalDeviceSparseImageFormatProperties");

   function vkQueueBindSparse
     (queue : VkQueue;
      bindInfoCount : stdint_h.uint32_t;
      pBindInfo : System.Address;
      fence : VkFence) return VkResult;  -- src/vulkan/vulkan.h:2513
   pragma Import (C, vkQueueBindSparse, "vkQueueBindSparse");

   function vkCreateFence
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pFence : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2519
   pragma Import (C, vkCreateFence, "vkCreateFence");

   procedure vkDestroyFence
     (device : VkDevice;
      fence : VkFence;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2525
   pragma Import (C, vkDestroyFence, "vkDestroyFence");

   function vkResetFences
     (device : VkDevice;
      fenceCount : stdint_h.uint32_t;
      pFences : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2530
   pragma Import (C, vkResetFences, "vkResetFences");

   function vkGetFenceStatus (device : VkDevice; fence : VkFence) return VkResult;  -- src/vulkan/vulkan.h:2535
   pragma Import (C, vkGetFenceStatus, "vkGetFenceStatus");

   function vkWaitForFences
     (device : VkDevice;
      fenceCount : stdint_h.uint32_t;
      pFences : System.Address;
      waitAll : VkBool32;
      timeout : stdint_h.uint64_t) return VkResult;  -- src/vulkan/vulkan.h:2539
   pragma Import (C, vkWaitForFences, "vkWaitForFences");

   function vkCreateSemaphore
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pSemaphore : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2546
   pragma Import (C, vkCreateSemaphore, "vkCreateSemaphore");

   procedure vkDestroySemaphore
     (device : VkDevice;
      semaphore : VkSemaphore;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2552
   pragma Import (C, vkDestroySemaphore, "vkDestroySemaphore");

   function vkCreateEvent
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pEvent : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2557
   pragma Import (C, vkCreateEvent, "vkCreateEvent");

   procedure vkDestroyEvent
     (device : VkDevice;
      event : VkEvent;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2563
   pragma Import (C, vkDestroyEvent, "vkDestroyEvent");

   function vkGetEventStatus (device : VkDevice; event : VkEvent) return VkResult;  -- src/vulkan/vulkan.h:2568
   pragma Import (C, vkGetEventStatus, "vkGetEventStatus");

   function vkSetEvent (device : VkDevice; event : VkEvent) return VkResult;  -- src/vulkan/vulkan.h:2572
   pragma Import (C, vkSetEvent, "vkSetEvent");

   function vkResetEvent (device : VkDevice; event : VkEvent) return VkResult;  -- src/vulkan/vulkan.h:2576
   pragma Import (C, vkResetEvent, "vkResetEvent");

   function vkCreateQueryPool
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pQueryPool : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2580
   pragma Import (C, vkCreateQueryPool, "vkCreateQueryPool");

   procedure vkDestroyQueryPool
     (device : VkDevice;
      queryPool : VkQueryPool;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2586
   pragma Import (C, vkDestroyQueryPool, "vkDestroyQueryPool");

   function vkGetQueryPoolResults
     (device : VkDevice;
      queryPool : VkQueryPool;
      firstQuery : stdint_h.uint32_t;
      queryCount : stdint_h.uint32_t;
      dataSize : stddef_h.size_t;
      pData : System.Address;
      stride : VkDeviceSize;
      flags : VkQueryResultFlags) return VkResult;  -- src/vulkan/vulkan.h:2591
   pragma Import (C, vkGetQueryPoolResults, "vkGetQueryPoolResults");

   function vkCreateBuffer
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pBuffer : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2601
   pragma Import (C, vkCreateBuffer, "vkCreateBuffer");

   procedure vkDestroyBuffer
     (device : VkDevice;
      buffer : VkBuffer;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2607
   pragma Import (C, vkDestroyBuffer, "vkDestroyBuffer");

   function vkCreateBufferView
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pView : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2612
   pragma Import (C, vkCreateBufferView, "vkCreateBufferView");

   procedure vkDestroyBufferView
     (device : VkDevice;
      bufferView : VkBufferView;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2618
   pragma Import (C, vkDestroyBufferView, "vkDestroyBufferView");

   function vkCreateImage
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pImage : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2623
   pragma Import (C, vkCreateImage, "vkCreateImage");

   procedure vkDestroyImage
     (device : VkDevice;
      image : VkImage;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2629
   pragma Import (C, vkDestroyImage, "vkDestroyImage");

   procedure vkGetImageSubresourceLayout
     (device : VkDevice;
      image : VkImage;
      pSubresource : System.Address;
      pLayout : access VkSubresourceLayout);  -- src/vulkan/vulkan.h:2634
   pragma Import (C, vkGetImageSubresourceLayout, "vkGetImageSubresourceLayout");

   function vkCreateImageView
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pView : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2640
   pragma Import (C, vkCreateImageView, "vkCreateImageView");

   procedure vkDestroyImageView
     (device : VkDevice;
      imageView : VkImageView;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2646
   pragma Import (C, vkDestroyImageView, "vkDestroyImageView");

   function vkCreateShaderModule
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pShaderModule : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2651
   pragma Import (C, vkCreateShaderModule, "vkCreateShaderModule");

   procedure vkDestroyShaderModule
     (device : VkDevice;
      shaderModule : VkShaderModule;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2657
   pragma Import (C, vkDestroyShaderModule, "vkDestroyShaderModule");

   function vkCreatePipelineCache
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pPipelineCache : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2662
   pragma Import (C, vkCreatePipelineCache, "vkCreatePipelineCache");

   procedure vkDestroyPipelineCache
     (device : VkDevice;
      pipelineCache : VkPipelineCache;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2668
   pragma Import (C, vkDestroyPipelineCache, "vkDestroyPipelineCache");

   function vkGetPipelineCacheData
     (device : VkDevice;
      pipelineCache : VkPipelineCache;
      pDataSize : access stddef_h.size_t;
      pData : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2673
   pragma Import (C, vkGetPipelineCacheData, "vkGetPipelineCacheData");

   function vkMergePipelineCaches
     (device : VkDevice;
      dstCache : VkPipelineCache;
      srcCacheCount : stdint_h.uint32_t;
      pSrcCaches : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2679
   pragma Import (C, vkMergePipelineCaches, "vkMergePipelineCaches");

   function vkCreateGraphicsPipelines
     (device : VkDevice;
      pipelineCache : VkPipelineCache;
      createInfoCount : stdint_h.uint32_t;
      pCreateInfos : System.Address;
      pAllocator : System.Address;
      pPipelines : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2685
   pragma Import (C, vkCreateGraphicsPipelines, "vkCreateGraphicsPipelines");

   function vkCreateComputePipelines
     (device : VkDevice;
      pipelineCache : VkPipelineCache;
      createInfoCount : stdint_h.uint32_t;
      pCreateInfos : System.Address;
      pAllocator : System.Address;
      pPipelines : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2693
   pragma Import (C, vkCreateComputePipelines, "vkCreateComputePipelines");

   procedure vkDestroyPipeline
     (device : VkDevice;
      pipeline : VkPipeline;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2701
   pragma Import (C, vkDestroyPipeline, "vkDestroyPipeline");

   function vkCreatePipelineLayout
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pPipelineLayout : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2706
   pragma Import (C, vkCreatePipelineLayout, "vkCreatePipelineLayout");

   procedure vkDestroyPipelineLayout
     (device : VkDevice;
      pipelineLayout : VkPipelineLayout;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2712
   pragma Import (C, vkDestroyPipelineLayout, "vkDestroyPipelineLayout");

   function vkCreateSampler
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pSampler : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2717
   pragma Import (C, vkCreateSampler, "vkCreateSampler");

   procedure vkDestroySampler
     (device : VkDevice;
      sampler : VkSampler;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2723
   pragma Import (C, vkDestroySampler, "vkDestroySampler");

   function vkCreateDescriptorSetLayout
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pSetLayout : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2728
   pragma Import (C, vkCreateDescriptorSetLayout, "vkCreateDescriptorSetLayout");

   procedure vkDestroyDescriptorSetLayout
     (device : VkDevice;
      descriptorSetLayout : VkDescriptorSetLayout;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2734
   pragma Import (C, vkDestroyDescriptorSetLayout, "vkDestroyDescriptorSetLayout");

   function vkCreateDescriptorPool
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pDescriptorPool : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2739
   pragma Import (C, vkCreateDescriptorPool, "vkCreateDescriptorPool");

   procedure vkDestroyDescriptorPool
     (device : VkDevice;
      descriptorPool : VkDescriptorPool;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2745
   pragma Import (C, vkDestroyDescriptorPool, "vkDestroyDescriptorPool");

   function vkResetDescriptorPool
     (device : VkDevice;
      descriptorPool : VkDescriptorPool;
      flags : VkDescriptorPoolResetFlags) return VkResult;  -- src/vulkan/vulkan.h:2750
   pragma Import (C, vkResetDescriptorPool, "vkResetDescriptorPool");

   function vkAllocateDescriptorSets
     (device : VkDevice;
      pAllocateInfo : System.Address;
      pDescriptorSets : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2755
   pragma Import (C, vkAllocateDescriptorSets, "vkAllocateDescriptorSets");

   function vkFreeDescriptorSets
     (device : VkDevice;
      descriptorPool : VkDescriptorPool;
      descriptorSetCount : stdint_h.uint32_t;
      pDescriptorSets : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2760
   pragma Import (C, vkFreeDescriptorSets, "vkFreeDescriptorSets");

   procedure vkUpdateDescriptorSets
     (device : VkDevice;
      descriptorWriteCount : stdint_h.uint32_t;
      pDescriptorWrites : System.Address;
      descriptorCopyCount : stdint_h.uint32_t;
      pDescriptorCopies : System.Address);  -- src/vulkan/vulkan.h:2766
   pragma Import (C, vkUpdateDescriptorSets, "vkUpdateDescriptorSets");

   function vkCreateFramebuffer
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pFramebuffer : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2773
   pragma Import (C, vkCreateFramebuffer, "vkCreateFramebuffer");

   procedure vkDestroyFramebuffer
     (device : VkDevice;
      framebuffer : VkFramebuffer;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2779
   pragma Import (C, vkDestroyFramebuffer, "vkDestroyFramebuffer");

   function vkCreateRenderPass
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pRenderPass : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2784
   pragma Import (C, vkCreateRenderPass, "vkCreateRenderPass");

   procedure vkDestroyRenderPass
     (device : VkDevice;
      renderPass : VkRenderPass;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2790
   pragma Import (C, vkDestroyRenderPass, "vkDestroyRenderPass");

   procedure vkGetRenderAreaGranularity
     (device : VkDevice;
      renderPass : VkRenderPass;
      pGranularity : access VkExtent2D);  -- src/vulkan/vulkan.h:2795
   pragma Import (C, vkGetRenderAreaGranularity, "vkGetRenderAreaGranularity");

   function vkCreateCommandPool
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pCommandPool : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2800
   pragma Import (C, vkCreateCommandPool, "vkCreateCommandPool");

   procedure vkDestroyCommandPool
     (device : VkDevice;
      commandPool : VkCommandPool;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:2806
   pragma Import (C, vkDestroyCommandPool, "vkDestroyCommandPool");

   function vkResetCommandPool
     (device : VkDevice;
      commandPool : VkCommandPool;
      flags : VkCommandPoolResetFlags) return VkResult;  -- src/vulkan/vulkan.h:2811
   pragma Import (C, vkResetCommandPool, "vkResetCommandPool");

   function vkAllocateCommandBuffers
     (device : VkDevice;
      pAllocateInfo : System.Address;
      pCommandBuffers : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2816
   pragma Import (C, vkAllocateCommandBuffers, "vkAllocateCommandBuffers");

   procedure vkFreeCommandBuffers
     (device : VkDevice;
      commandPool : VkCommandPool;
      commandBufferCount : stdint_h.uint32_t;
      pCommandBuffers : System.Address);  -- src/vulkan/vulkan.h:2821
   pragma Import (C, vkFreeCommandBuffers, "vkFreeCommandBuffers");

   function vkBeginCommandBuffer (commandBuffer : VkCommandBuffer; pBeginInfo : System.Address) return VkResult;  -- src/vulkan/vulkan.h:2827
   pragma Import (C, vkBeginCommandBuffer, "vkBeginCommandBuffer");

   function vkEndCommandBuffer (commandBuffer : VkCommandBuffer) return VkResult;  -- src/vulkan/vulkan.h:2831
   pragma Import (C, vkEndCommandBuffer, "vkEndCommandBuffer");

   function vkResetCommandBuffer (commandBuffer : VkCommandBuffer; flags : VkCommandBufferResetFlags) return VkResult;  -- src/vulkan/vulkan.h:2834
   pragma Import (C, vkResetCommandBuffer, "vkResetCommandBuffer");

   procedure vkCmdBindPipeline
     (commandBuffer : VkCommandBuffer;
      pipelineBindPoint : VkPipelineBindPoint;
      pipeline : VkPipeline);  -- src/vulkan/vulkan.h:2838
   pragma Import (C, vkCmdBindPipeline, "vkCmdBindPipeline");

   procedure vkCmdSetViewport
     (commandBuffer : VkCommandBuffer;
      firstViewport : stdint_h.uint32_t;
      viewportCount : stdint_h.uint32_t;
      pViewports : System.Address);  -- src/vulkan/vulkan.h:2843
   pragma Import (C, vkCmdSetViewport, "vkCmdSetViewport");

   procedure vkCmdSetScissor
     (commandBuffer : VkCommandBuffer;
      firstScissor : stdint_h.uint32_t;
      scissorCount : stdint_h.uint32_t;
      pScissors : System.Address);  -- src/vulkan/vulkan.h:2849
   pragma Import (C, vkCmdSetScissor, "vkCmdSetScissor");

   procedure vkCmdSetLineWidth (commandBuffer : VkCommandBuffer; lineWidth : float);  -- src/vulkan/vulkan.h:2855
   pragma Import (C, vkCmdSetLineWidth, "vkCmdSetLineWidth");

   procedure vkCmdSetDepthBias
     (commandBuffer : VkCommandBuffer;
      depthBiasConstantFactor : float;
      depthBiasClamp : float;
      depthBiasSlopeFactor : float);  -- src/vulkan/vulkan.h:2859
   pragma Import (C, vkCmdSetDepthBias, "vkCmdSetDepthBias");

   procedure vkCmdSetBlendConstants (commandBuffer : VkCommandBuffer; blendConstants : access float);  -- src/vulkan/vulkan.h:2865
   pragma Import (C, vkCmdSetBlendConstants, "vkCmdSetBlendConstants");

   procedure vkCmdSetDepthBounds
     (commandBuffer : VkCommandBuffer;
      minDepthBounds : float;
      maxDepthBounds : float);  -- src/vulkan/vulkan.h:2869
   pragma Import (C, vkCmdSetDepthBounds, "vkCmdSetDepthBounds");

   procedure vkCmdSetStencilCompareMask
     (commandBuffer : VkCommandBuffer;
      faceMask : VkStencilFaceFlags;
      compareMask : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:2874
   pragma Import (C, vkCmdSetStencilCompareMask, "vkCmdSetStencilCompareMask");

   procedure vkCmdSetStencilWriteMask
     (commandBuffer : VkCommandBuffer;
      faceMask : VkStencilFaceFlags;
      writeMask : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:2879
   pragma Import (C, vkCmdSetStencilWriteMask, "vkCmdSetStencilWriteMask");

   procedure vkCmdSetStencilReference
     (commandBuffer : VkCommandBuffer;
      faceMask : VkStencilFaceFlags;
      reference : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:2884
   pragma Import (C, vkCmdSetStencilReference, "vkCmdSetStencilReference");

   procedure vkCmdBindDescriptorSets
     (commandBuffer : VkCommandBuffer;
      pipelineBindPoint : VkPipelineBindPoint;
      layout : VkPipelineLayout;
      firstSet : stdint_h.uint32_t;
      descriptorSetCount : stdint_h.uint32_t;
      pDescriptorSets : System.Address;
      dynamicOffsetCount : stdint_h.uint32_t;
      pDynamicOffsets : access stdint_h.uint32_t);  -- src/vulkan/vulkan.h:2889
   pragma Import (C, vkCmdBindDescriptorSets, "vkCmdBindDescriptorSets");

   procedure vkCmdBindIndexBuffer
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      indexType : VkIndexType);  -- src/vulkan/vulkan.h:2899
   pragma Import (C, vkCmdBindIndexBuffer, "vkCmdBindIndexBuffer");

   procedure vkCmdBindVertexBuffers
     (commandBuffer : VkCommandBuffer;
      firstBinding : stdint_h.uint32_t;
      bindingCount : stdint_h.uint32_t;
      pBuffers : System.Address;
      pOffsets : access VkDeviceSize);  -- src/vulkan/vulkan.h:2905
   pragma Import (C, vkCmdBindVertexBuffers, "vkCmdBindVertexBuffers");

   procedure vkCmdDraw
     (commandBuffer : VkCommandBuffer;
      vertexCount : stdint_h.uint32_t;
      instanceCount : stdint_h.uint32_t;
      firstVertex : stdint_h.uint32_t;
      firstInstance : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:2912
   pragma Import (C, vkCmdDraw, "vkCmdDraw");

   procedure vkCmdDrawIndexed
     (commandBuffer : VkCommandBuffer;
      indexCount : stdint_h.uint32_t;
      instanceCount : stdint_h.uint32_t;
      firstIndex : stdint_h.uint32_t;
      vertexOffset : stdint_h.int32_t;
      firstInstance : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:2919
   pragma Import (C, vkCmdDrawIndexed, "vkCmdDrawIndexed");

   procedure vkCmdDrawIndirect
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      drawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:2927
   pragma Import (C, vkCmdDrawIndirect, "vkCmdDrawIndirect");

   procedure vkCmdDrawIndexedIndirect
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      drawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:2934
   pragma Import (C, vkCmdDrawIndexedIndirect, "vkCmdDrawIndexedIndirect");

   procedure vkCmdDispatch
     (commandBuffer : VkCommandBuffer;
      x : stdint_h.uint32_t;
      y : stdint_h.uint32_t;
      z : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:2941
   pragma Import (C, vkCmdDispatch, "vkCmdDispatch");

   procedure vkCmdDispatchIndirect
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize);  -- src/vulkan/vulkan.h:2947
   pragma Import (C, vkCmdDispatchIndirect, "vkCmdDispatchIndirect");

   procedure vkCmdCopyBuffer
     (commandBuffer : VkCommandBuffer;
      srcBuffer : VkBuffer;
      dstBuffer : VkBuffer;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address);  -- src/vulkan/vulkan.h:2952
   pragma Import (C, vkCmdCopyBuffer, "vkCmdCopyBuffer");

   procedure vkCmdCopyImage
     (commandBuffer : VkCommandBuffer;
      srcImage : VkImage;
      srcImageLayout : VkImageLayout;
      dstImage : VkImage;
      dstImageLayout : VkImageLayout;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address);  -- src/vulkan/vulkan.h:2959
   pragma Import (C, vkCmdCopyImage, "vkCmdCopyImage");

   procedure vkCmdBlitImage
     (commandBuffer : VkCommandBuffer;
      srcImage : VkImage;
      srcImageLayout : VkImageLayout;
      dstImage : VkImage;
      dstImageLayout : VkImageLayout;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address;
      filter : VkFilter);  -- src/vulkan/vulkan.h:2968
   pragma Import (C, vkCmdBlitImage, "vkCmdBlitImage");

   procedure vkCmdCopyBufferToImage
     (commandBuffer : VkCommandBuffer;
      srcBuffer : VkBuffer;
      dstImage : VkImage;
      dstImageLayout : VkImageLayout;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address);  -- src/vulkan/vulkan.h:2978
   pragma Import (C, vkCmdCopyBufferToImage, "vkCmdCopyBufferToImage");

   procedure vkCmdCopyImageToBuffer
     (commandBuffer : VkCommandBuffer;
      srcImage : VkImage;
      srcImageLayout : VkImageLayout;
      dstBuffer : VkBuffer;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address);  -- src/vulkan/vulkan.h:2986
   pragma Import (C, vkCmdCopyImageToBuffer, "vkCmdCopyImageToBuffer");

   procedure vkCmdUpdateBuffer
     (commandBuffer : VkCommandBuffer;
      dstBuffer : VkBuffer;
      dstOffset : VkDeviceSize;
      dataSize : VkDeviceSize;
      pData : access stdint_h.uint32_t);  -- src/vulkan/vulkan.h:2994
   pragma Import (C, vkCmdUpdateBuffer, "vkCmdUpdateBuffer");

   procedure vkCmdFillBuffer
     (commandBuffer : VkCommandBuffer;
      dstBuffer : VkBuffer;
      dstOffset : VkDeviceSize;
      size : VkDeviceSize;
      data : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:3001
   pragma Import (C, vkCmdFillBuffer, "vkCmdFillBuffer");

   procedure vkCmdClearColorImage
     (commandBuffer : VkCommandBuffer;
      image : VkImage;
      imageLayout : VkImageLayout;
      pColor : System.Address;
      rangeCount : stdint_h.uint32_t;
      pRanges : System.Address);  -- src/vulkan/vulkan.h:3008
   pragma Import (C, vkCmdClearColorImage, "vkCmdClearColorImage");

   procedure vkCmdClearDepthStencilImage
     (commandBuffer : VkCommandBuffer;
      image : VkImage;
      imageLayout : VkImageLayout;
      pDepthStencil : System.Address;
      rangeCount : stdint_h.uint32_t;
      pRanges : System.Address);  -- src/vulkan/vulkan.h:3016
   pragma Import (C, vkCmdClearDepthStencilImage, "vkCmdClearDepthStencilImage");

   procedure vkCmdClearAttachments
     (commandBuffer : VkCommandBuffer;
      attachmentCount : stdint_h.uint32_t;
      pAttachments : System.Address;
      rectCount : stdint_h.uint32_t;
      pRects : System.Address);  -- src/vulkan/vulkan.h:3024
   pragma Import (C, vkCmdClearAttachments, "vkCmdClearAttachments");

   procedure vkCmdResolveImage
     (commandBuffer : VkCommandBuffer;
      srcImage : VkImage;
      srcImageLayout : VkImageLayout;
      dstImage : VkImage;
      dstImageLayout : VkImageLayout;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address);  -- src/vulkan/vulkan.h:3031
   pragma Import (C, vkCmdResolveImage, "vkCmdResolveImage");

   procedure vkCmdSetEvent
     (commandBuffer : VkCommandBuffer;
      event : VkEvent;
      stageMask : VkPipelineStageFlags);  -- src/vulkan/vulkan.h:3040
   pragma Import (C, vkCmdSetEvent, "vkCmdSetEvent");

   procedure vkCmdResetEvent
     (commandBuffer : VkCommandBuffer;
      event : VkEvent;
      stageMask : VkPipelineStageFlags);  -- src/vulkan/vulkan.h:3045
   pragma Import (C, vkCmdResetEvent, "vkCmdResetEvent");

   procedure vkCmdWaitEvents
     (commandBuffer : VkCommandBuffer;
      eventCount : stdint_h.uint32_t;
      pEvents : System.Address;
      srcStageMask : VkPipelineStageFlags;
      dstStageMask : VkPipelineStageFlags;
      memoryBarrierCount : stdint_h.uint32_t;
      pMemoryBarriers : System.Address;
      bufferMemoryBarrierCount : stdint_h.uint32_t;
      pBufferMemoryBarriers : System.Address;
      imageMemoryBarrierCount : stdint_h.uint32_t;
      pImageMemoryBarriers : System.Address);  -- src/vulkan/vulkan.h:3050
   pragma Import (C, vkCmdWaitEvents, "vkCmdWaitEvents");

   procedure vkCmdPipelineBarrier
     (commandBuffer : VkCommandBuffer;
      srcStageMask : VkPipelineStageFlags;
      dstStageMask : VkPipelineStageFlags;
      dependencyFlags : VkDependencyFlags;
      memoryBarrierCount : stdint_h.uint32_t;
      pMemoryBarriers : System.Address;
      bufferMemoryBarrierCount : stdint_h.uint32_t;
      pBufferMemoryBarriers : System.Address;
      imageMemoryBarrierCount : stdint_h.uint32_t;
      pImageMemoryBarriers : System.Address);  -- src/vulkan/vulkan.h:3063
   pragma Import (C, vkCmdPipelineBarrier, "vkCmdPipelineBarrier");

   procedure vkCmdBeginQuery
     (commandBuffer : VkCommandBuffer;
      queryPool : VkQueryPool;
      query : stdint_h.uint32_t;
      flags : VkQueryControlFlags);  -- src/vulkan/vulkan.h:3075
   pragma Import (C, vkCmdBeginQuery, "vkCmdBeginQuery");

   procedure vkCmdEndQuery
     (commandBuffer : VkCommandBuffer;
      queryPool : VkQueryPool;
      query : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:3081
   pragma Import (C, vkCmdEndQuery, "vkCmdEndQuery");

   procedure vkCmdResetQueryPool
     (commandBuffer : VkCommandBuffer;
      queryPool : VkQueryPool;
      firstQuery : stdint_h.uint32_t;
      queryCount : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:3086
   pragma Import (C, vkCmdResetQueryPool, "vkCmdResetQueryPool");

   procedure vkCmdWriteTimestamp
     (commandBuffer : VkCommandBuffer;
      pipelineStage : VkPipelineStageFlagBits;
      queryPool : VkQueryPool;
      query : stdint_h.uint32_t);  -- src/vulkan/vulkan.h:3092
   pragma Import (C, vkCmdWriteTimestamp, "vkCmdWriteTimestamp");

   procedure vkCmdCopyQueryPoolResults
     (commandBuffer : VkCommandBuffer;
      queryPool : VkQueryPool;
      firstQuery : stdint_h.uint32_t;
      queryCount : stdint_h.uint32_t;
      dstBuffer : VkBuffer;
      dstOffset : VkDeviceSize;
      stride : VkDeviceSize;
      flags : VkQueryResultFlags);  -- src/vulkan/vulkan.h:3098
   pragma Import (C, vkCmdCopyQueryPoolResults, "vkCmdCopyQueryPoolResults");

   procedure vkCmdPushConstants
     (commandBuffer : VkCommandBuffer;
      layout : VkPipelineLayout;
      stageFlags : VkShaderStageFlags;
      offset : stdint_h.uint32_t;
      size : stdint_h.uint32_t;
      pValues : System.Address);  -- src/vulkan/vulkan.h:3108
   pragma Import (C, vkCmdPushConstants, "vkCmdPushConstants");

   procedure vkCmdBeginRenderPass
     (commandBuffer : VkCommandBuffer;
      pRenderPassBegin : System.Address;
      contents : VkSubpassContents);  -- src/vulkan/vulkan.h:3116
   pragma Import (C, vkCmdBeginRenderPass, "vkCmdBeginRenderPass");

   procedure vkCmdNextSubpass (commandBuffer : VkCommandBuffer; contents : VkSubpassContents);  -- src/vulkan/vulkan.h:3121
   pragma Import (C, vkCmdNextSubpass, "vkCmdNextSubpass");

   procedure vkCmdEndRenderPass (commandBuffer : VkCommandBuffer);  -- src/vulkan/vulkan.h:3125
   pragma Import (C, vkCmdEndRenderPass, "vkCmdEndRenderPass");

   procedure vkCmdExecuteCommands
     (commandBuffer : VkCommandBuffer;
      commandBufferCount : stdint_h.uint32_t;
      pCommandBuffers : System.Address);  -- src/vulkan/vulkan.h:3128
   pragma Import (C, vkCmdExecuteCommands, "vkCmdExecuteCommands");

   type VkSurfaceKHR is new System.Address;  -- src/vulkan/vulkan.h:3135

   --  skipped empty struct VkSurfaceKHR_T

   subtype VkColorSpaceKHR is unsigned;
   VK_COLORSPACE_SRGB_NONLINEAR_KHR : constant VkColorSpaceKHR := 0;
   VK_COLORSPACE_BEGIN_RANGE : constant VkColorSpaceKHR := 0;
   VK_COLORSPACE_END_RANGE : constant VkColorSpaceKHR := 0;
   VK_COLORSPACE_RANGE_SIZE : constant VkColorSpaceKHR := 1;
   VK_COLORSPACE_MAX_ENUM : constant VkColorSpaceKHR := 2147483647;  -- src/vulkan/vulkan.h:3141

   subtype VkPresentModeKHR is unsigned;
   VK_PRESENT_MODE_IMMEDIATE_KHR : constant VkPresentModeKHR := 0;
   VK_PRESENT_MODE_MAILBOX_KHR : constant VkPresentModeKHR := 1;
   VK_PRESENT_MODE_FIFO_KHR : constant VkPresentModeKHR := 2;
   VK_PRESENT_MODE_FIFO_RELAXED_KHR : constant VkPresentModeKHR := 3;
   VK_PRESENT_MODE_BEGIN_RANGE : constant VkPresentModeKHR := 0;
   VK_PRESENT_MODE_END_RANGE : constant VkPresentModeKHR := 3;
   VK_PRESENT_MODE_RANGE_SIZE : constant VkPresentModeKHR := 4;
   VK_PRESENT_MODE_MAX_ENUM : constant VkPresentModeKHR := 2147483647;  -- src/vulkan/vulkan.h:3149

   subtype VkSurfaceTransformFlagBitsKHR is unsigned;
   VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 1;
   VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 2;
   VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 4;
   VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 8;
   VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 16;
   VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 32;
   VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 64;
   VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 128;
   VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 256;  -- src/vulkan/vulkan.h:3161

   subtype VkSurfaceTransformFlagsKHR is VkFlags;  -- src/vulkan/vulkan.h:3172

   subtype VkCompositeAlphaFlagBitsKHR is unsigned;
   VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR : constant VkCompositeAlphaFlagBitsKHR := 1;
   VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR : constant VkCompositeAlphaFlagBitsKHR := 2;
   VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR : constant VkCompositeAlphaFlagBitsKHR := 4;
   VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR : constant VkCompositeAlphaFlagBitsKHR := 8;  -- src/vulkan/vulkan.h:3174

   subtype VkCompositeAlphaFlagsKHR is VkFlags;  -- src/vulkan/vulkan.h:3180

   type VkSurfaceCapabilitiesKHR is record
      minImageCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3183
      maxImageCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3184
      currentExtent : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3185
      minImageExtent : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3186
      maxImageExtent : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3187
      maxImageArrayLayers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3188
      supportedTransforms : aliased VkSurfaceTransformFlagsKHR;  -- src/vulkan/vulkan.h:3189
      currentTransform : aliased VkSurfaceTransformFlagBitsKHR;  -- src/vulkan/vulkan.h:3190
      supportedCompositeAlpha : aliased VkCompositeAlphaFlagsKHR;  -- src/vulkan/vulkan.h:3191
      supportedUsageFlags : aliased VkImageUsageFlags;  -- src/vulkan/vulkan.h:3192
   end record;
   pragma Convention (C_Pass_By_Copy, VkSurfaceCapabilitiesKHR);  -- src/vulkan/vulkan.h:3182

   type VkSurfaceFormatKHR is record
      format : aliased VkFormat;  -- src/vulkan/vulkan.h:3196
      colorSpace : aliased VkColorSpaceKHR;  -- src/vulkan/vulkan.h:3197
   end record;
   pragma Convention (C_Pass_By_Copy, VkSurfaceFormatKHR);  -- src/vulkan/vulkan.h:3195

   type PFN_vkDestroySurfaceKHR is access procedure
        (arg1 : VkInstance;
         arg2 : VkSurfaceKHR;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroySurfaceKHR);  -- src/vulkan/vulkan.h:3201

   type PFN_vkGetPhysicalDeviceSurfaceSupportKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : VkSurfaceKHR;
         arg4 : access VkBool32) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSurfaceSupportKHR);  -- src/vulkan/vulkan.h:3202

   type PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkSurfaceKHR;
         arg3 : access VkSurfaceCapabilitiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR);  -- src/vulkan/vulkan.h:3203

   type PFN_vkGetPhysicalDeviceSurfaceFormatsKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkSurfaceKHR;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkSurfaceFormatKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSurfaceFormatsKHR);  -- src/vulkan/vulkan.h:3204

   type PFN_vkGetPhysicalDeviceSurfacePresentModesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkSurfaceKHR;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkPresentModeKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSurfacePresentModesKHR);  -- src/vulkan/vulkan.h:3205

   procedure vkDestroySurfaceKHR
     (instance : VkInstance;
      surface : VkSurfaceKHR;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:3208
   pragma Import (C, vkDestroySurfaceKHR, "vkDestroySurfaceKHR");

   function vkGetPhysicalDeviceSurfaceSupportKHR
     (physicalDevice : VkPhysicalDevice;
      queueFamilyIndex : stdint_h.uint32_t;
      surface : VkSurfaceKHR;
      pSupported : access VkBool32) return VkResult;  -- src/vulkan/vulkan.h:3213
   pragma Import (C, vkGetPhysicalDeviceSurfaceSupportKHR, "vkGetPhysicalDeviceSurfaceSupportKHR");

   function vkGetPhysicalDeviceSurfaceCapabilitiesKHR
     (physicalDevice : VkPhysicalDevice;
      surface : VkSurfaceKHR;
      pSurfaceCapabilities : access VkSurfaceCapabilitiesKHR) return VkResult;  -- src/vulkan/vulkan.h:3219
   pragma Import (C, vkGetPhysicalDeviceSurfaceCapabilitiesKHR, "vkGetPhysicalDeviceSurfaceCapabilitiesKHR");

   function vkGetPhysicalDeviceSurfaceFormatsKHR
     (physicalDevice : VkPhysicalDevice;
      surface : VkSurfaceKHR;
      pSurfaceFormatCount : access stdint_h.uint32_t;
      pSurfaceFormats : access VkSurfaceFormatKHR) return VkResult;  -- src/vulkan/vulkan.h:3224
   pragma Import (C, vkGetPhysicalDeviceSurfaceFormatsKHR, "vkGetPhysicalDeviceSurfaceFormatsKHR");

   function vkGetPhysicalDeviceSurfacePresentModesKHR
     (physicalDevice : VkPhysicalDevice;
      surface : VkSurfaceKHR;
      pPresentModeCount : access stdint_h.uint32_t;
      pPresentModes : access VkPresentModeKHR) return VkResult;  -- src/vulkan/vulkan.h:3230
   pragma Import (C, vkGetPhysicalDeviceSurfacePresentModesKHR, "vkGetPhysicalDeviceSurfacePresentModesKHR");

   type VkSwapchainKHR is new System.Address;  -- src/vulkan/vulkan.h:3238

   --  skipped empty struct VkSwapchainKHR_T

   subtype VkSwapchainCreateFlagsKHR is VkFlags;  -- src/vulkan/vulkan.h:3243

   type VkSwapchainCreateInfoKHR is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:3246
      pNext : System.Address;  -- src/vulkan/vulkan.h:3247
      flags : aliased VkSwapchainCreateFlagsKHR;  -- src/vulkan/vulkan.h:3248
      surface : VkSurfaceKHR;  -- src/vulkan/vulkan.h:3249
      minImageCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3250
      imageFormat : aliased VkFormat;  -- src/vulkan/vulkan.h:3251
      imageColorSpace : aliased VkColorSpaceKHR;  -- src/vulkan/vulkan.h:3252
      imageExtent : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3253
      imageArrayLayers : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3254
      imageUsage : aliased VkImageUsageFlags;  -- src/vulkan/vulkan.h:3255
      imageSharingMode : aliased VkSharingMode;  -- src/vulkan/vulkan.h:3256
      queueFamilyIndexCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3257
      pQueueFamilyIndices : access stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3258
      preTransform : aliased VkSurfaceTransformFlagBitsKHR;  -- src/vulkan/vulkan.h:3259
      compositeAlpha : aliased VkCompositeAlphaFlagBitsKHR;  -- src/vulkan/vulkan.h:3260
      presentMode : aliased VkPresentModeKHR;  -- src/vulkan/vulkan.h:3261
      clipped : aliased VkBool32;  -- src/vulkan/vulkan.h:3262
      oldSwapchain : VkSwapchainKHR;  -- src/vulkan/vulkan.h:3263
   end record;
   pragma Convention (C_Pass_By_Copy, VkSwapchainCreateInfoKHR);  -- src/vulkan/vulkan.h:3245

   type VkPresentInfoKHR is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:3267
      pNext : System.Address;  -- src/vulkan/vulkan.h:3268
      waitSemaphoreCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3269
      pWaitSemaphores : System.Address;  -- src/vulkan/vulkan.h:3270
      swapchainCount : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3271
      pSwapchains : System.Address;  -- src/vulkan/vulkan.h:3272
      pImageIndices : access stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3273
      pResults : access VkResult;  -- src/vulkan/vulkan.h:3274
   end record;
   pragma Convention (C_Pass_By_Copy, VkPresentInfoKHR);  -- src/vulkan/vulkan.h:3266

   type PFN_vkCreateSwapchainKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateSwapchainKHR);  -- src/vulkan/vulkan.h:3278

   type PFN_vkDestroySwapchainKHR is access procedure
        (arg1 : VkDevice;
         arg2 : VkSwapchainKHR;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroySwapchainKHR);  -- src/vulkan/vulkan.h:3279

   type PFN_vkGetSwapchainImagesKHR is access function
        (arg1 : VkDevice;
         arg2 : VkSwapchainKHR;
         arg3 : access stdint_h.uint32_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetSwapchainImagesKHR);  -- src/vulkan/vulkan.h:3280

   type PFN_vkAcquireNextImageKHR is access function
        (arg1 : VkDevice;
         arg2 : VkSwapchainKHR;
         arg3 : stdint_h.uint64_t;
         arg4 : VkSemaphore;
         arg5 : VkFence;
         arg6 : access stdint_h.uint32_t) return VkResult;
   pragma Convention (C, PFN_vkAcquireNextImageKHR);  -- src/vulkan/vulkan.h:3281

   type PFN_vkQueuePresentKHR is access function (arg1 : VkQueue; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkQueuePresentKHR);  -- src/vulkan/vulkan.h:3282

   function vkCreateSwapchainKHR
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pSwapchain : System.Address) return VkResult;  -- src/vulkan/vulkan.h:3285
   pragma Import (C, vkCreateSwapchainKHR, "vkCreateSwapchainKHR");

   procedure vkDestroySwapchainKHR
     (device : VkDevice;
      swapchain : VkSwapchainKHR;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:3291
   pragma Import (C, vkDestroySwapchainKHR, "vkDestroySwapchainKHR");

   function vkGetSwapchainImagesKHR
     (device : VkDevice;
      swapchain : VkSwapchainKHR;
      pSwapchainImageCount : access stdint_h.uint32_t;
      pSwapchainImages : System.Address) return VkResult;  -- src/vulkan/vulkan.h:3296
   pragma Import (C, vkGetSwapchainImagesKHR, "vkGetSwapchainImagesKHR");

   function vkAcquireNextImageKHR
     (device : VkDevice;
      swapchain : VkSwapchainKHR;
      timeout : stdint_h.uint64_t;
      semaphore : VkSemaphore;
      fence : VkFence;
      pImageIndex : access stdint_h.uint32_t) return VkResult;  -- src/vulkan/vulkan.h:3302
   pragma Import (C, vkAcquireNextImageKHR, "vkAcquireNextImageKHR");

   function vkQueuePresentKHR (queue : VkQueue; pPresentInfo : System.Address) return VkResult;  -- src/vulkan/vulkan.h:3310
   pragma Import (C, vkQueuePresentKHR, "vkQueuePresentKHR");

   type VkDisplayKHR is new System.Address;  -- src/vulkan/vulkan.h:3316

   --  skipped empty struct VkDisplayKHR_T

   type VkDisplayModeKHR is new System.Address;  -- src/vulkan/vulkan.h:3317

   --  skipped empty struct VkDisplayModeKHR_T

   subtype VkDisplayPlaneAlphaFlagBitsKHR is unsigned;
   VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR : constant VkDisplayPlaneAlphaFlagBitsKHR := 1;
   VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR : constant VkDisplayPlaneAlphaFlagBitsKHR := 2;
   VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR : constant VkDisplayPlaneAlphaFlagBitsKHR := 4;
   VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR : constant VkDisplayPlaneAlphaFlagBitsKHR := 8;  -- src/vulkan/vulkan.h:3323

   subtype VkDisplayModeCreateFlagsKHR is VkFlags;  -- src/vulkan/vulkan.h:3329

   subtype VkDisplayPlaneAlphaFlagsKHR is VkFlags;  -- src/vulkan/vulkan.h:3330

   subtype VkDisplaySurfaceCreateFlagsKHR is VkFlags;  -- src/vulkan/vulkan.h:3331

   type VkDisplayPropertiesKHR is record
      display : VkDisplayKHR;  -- src/vulkan/vulkan.h:3334
      displayName : Interfaces.C.Strings.chars_ptr;  -- src/vulkan/vulkan.h:3335
      physicalDimensions : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3336
      physicalResolution : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3337
      supportedTransforms : aliased VkSurfaceTransformFlagsKHR;  -- src/vulkan/vulkan.h:3338
      planeReorderPossible : aliased VkBool32;  -- src/vulkan/vulkan.h:3339
      persistentContent : aliased VkBool32;  -- src/vulkan/vulkan.h:3340
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPropertiesKHR);  -- src/vulkan/vulkan.h:3333

   type VkDisplayModeParametersKHR is record
      visibleRegion : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3344
      refreshRate : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3345
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayModeParametersKHR);  -- src/vulkan/vulkan.h:3343

   type VkDisplayModePropertiesKHR is record
      displayMode : VkDisplayModeKHR;  -- src/vulkan/vulkan.h:3349
      parameters : aliased VkDisplayModeParametersKHR;  -- src/vulkan/vulkan.h:3350
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayModePropertiesKHR);  -- src/vulkan/vulkan.h:3348

   type VkDisplayModeCreateInfoKHR is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:3354
      pNext : System.Address;  -- src/vulkan/vulkan.h:3355
      flags : aliased VkDisplayModeCreateFlagsKHR;  -- src/vulkan/vulkan.h:3356
      parameters : aliased VkDisplayModeParametersKHR;  -- src/vulkan/vulkan.h:3357
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayModeCreateInfoKHR);  -- src/vulkan/vulkan.h:3353

   type VkDisplayPlaneCapabilitiesKHR is record
      supportedAlpha : aliased VkDisplayPlaneAlphaFlagsKHR;  -- src/vulkan/vulkan.h:3361
      minSrcPosition : aliased VkOffset2D;  -- src/vulkan/vulkan.h:3362
      maxSrcPosition : aliased VkOffset2D;  -- src/vulkan/vulkan.h:3363
      minSrcExtent : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3364
      maxSrcExtent : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3365
      minDstPosition : aliased VkOffset2D;  -- src/vulkan/vulkan.h:3366
      maxDstPosition : aliased VkOffset2D;  -- src/vulkan/vulkan.h:3367
      minDstExtent : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3368
      maxDstExtent : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3369
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPlaneCapabilitiesKHR);  -- src/vulkan/vulkan.h:3360

   type VkDisplayPlanePropertiesKHR is record
      currentDisplay : VkDisplayKHR;  -- src/vulkan/vulkan.h:3373
      currentStackIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3374
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPlanePropertiesKHR);  -- src/vulkan/vulkan.h:3372

   type VkDisplaySurfaceCreateInfoKHR is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:3378
      pNext : System.Address;  -- src/vulkan/vulkan.h:3379
      flags : aliased VkDisplaySurfaceCreateFlagsKHR;  -- src/vulkan/vulkan.h:3380
      displayMode : VkDisplayModeKHR;  -- src/vulkan/vulkan.h:3381
      planeIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3382
      planeStackIndex : aliased stdint_h.uint32_t;  -- src/vulkan/vulkan.h:3383
      transform : aliased VkSurfaceTransformFlagBitsKHR;  -- src/vulkan/vulkan.h:3384
      globalAlpha : aliased float;  -- src/vulkan/vulkan.h:3385
      alphaMode : aliased VkDisplayPlaneAlphaFlagBitsKHR;  -- src/vulkan/vulkan.h:3386
      imageExtent : aliased VkExtent2D;  -- src/vulkan/vulkan.h:3387
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplaySurfaceCreateInfoKHR);  -- src/vulkan/vulkan.h:3377

   type PFN_vkGetPhysicalDeviceDisplayPropertiesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkDisplayPropertiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceDisplayPropertiesKHR);  -- src/vulkan/vulkan.h:3391

   type PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkDisplayPlanePropertiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR);  -- src/vulkan/vulkan.h:3392

   type PFN_vkGetDisplayPlaneSupportedDisplaysKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : access stdint_h.uint32_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetDisplayPlaneSupportedDisplaysKHR);  -- src/vulkan/vulkan.h:3393

   type PFN_vkGetDisplayModePropertiesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkDisplayKHR;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkDisplayModePropertiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetDisplayModePropertiesKHR);  -- src/vulkan/vulkan.h:3394

   type PFN_vkCreateDisplayModeKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkDisplayKHR;
         arg3 : System.Address;
         arg4 : System.Address;
         arg5 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDisplayModeKHR);  -- src/vulkan/vulkan.h:3395

   type PFN_vkGetDisplayPlaneCapabilitiesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkDisplayModeKHR;
         arg3 : stdint_h.uint32_t;
         arg4 : access VkDisplayPlaneCapabilitiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetDisplayPlaneCapabilitiesKHR);  -- src/vulkan/vulkan.h:3396

   type PFN_vkCreateDisplayPlaneSurfaceKHR is access function
        (arg1 : VkInstance;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDisplayPlaneSurfaceKHR);  -- src/vulkan/vulkan.h:3397

   function vkGetPhysicalDeviceDisplayPropertiesKHR
     (physicalDevice : VkPhysicalDevice;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkDisplayPropertiesKHR) return VkResult;  -- src/vulkan/vulkan.h:3400
   pragma Import (C, vkGetPhysicalDeviceDisplayPropertiesKHR, "vkGetPhysicalDeviceDisplayPropertiesKHR");

   function vkGetPhysicalDeviceDisplayPlanePropertiesKHR
     (physicalDevice : VkPhysicalDevice;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkDisplayPlanePropertiesKHR) return VkResult;  -- src/vulkan/vulkan.h:3405
   pragma Import (C, vkGetPhysicalDeviceDisplayPlanePropertiesKHR, "vkGetPhysicalDeviceDisplayPlanePropertiesKHR");

   function vkGetDisplayPlaneSupportedDisplaysKHR
     (physicalDevice : VkPhysicalDevice;
      planeIndex : stdint_h.uint32_t;
      pDisplayCount : access stdint_h.uint32_t;
      pDisplays : System.Address) return VkResult;  -- src/vulkan/vulkan.h:3410
   pragma Import (C, vkGetDisplayPlaneSupportedDisplaysKHR, "vkGetDisplayPlaneSupportedDisplaysKHR");

   function vkGetDisplayModePropertiesKHR
     (physicalDevice : VkPhysicalDevice;
      display : VkDisplayKHR;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkDisplayModePropertiesKHR) return VkResult;  -- src/vulkan/vulkan.h:3416
   pragma Import (C, vkGetDisplayModePropertiesKHR, "vkGetDisplayModePropertiesKHR");

   function vkCreateDisplayModeKHR
     (physicalDevice : VkPhysicalDevice;
      display : VkDisplayKHR;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pMode : System.Address) return VkResult;  -- src/vulkan/vulkan.h:3422
   pragma Import (C, vkCreateDisplayModeKHR, "vkCreateDisplayModeKHR");

   function vkGetDisplayPlaneCapabilitiesKHR
     (physicalDevice : VkPhysicalDevice;
      mode : VkDisplayModeKHR;
      planeIndex : stdint_h.uint32_t;
      pCapabilities : access VkDisplayPlaneCapabilitiesKHR) return VkResult;  -- src/vulkan/vulkan.h:3429
   pragma Import (C, vkGetDisplayPlaneCapabilitiesKHR, "vkGetDisplayPlaneCapabilitiesKHR");

   function vkCreateDisplayPlaneSurfaceKHR
     (instance : VkInstance;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pSurface : System.Address) return VkResult;  -- src/vulkan/vulkan.h:3435
   pragma Import (C, vkCreateDisplayPlaneSurfaceKHR, "vkCreateDisplayPlaneSurfaceKHR");

   type VkDisplayPresentInfoKHR is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:3447
      pNext : System.Address;  -- src/vulkan/vulkan.h:3448
      srcRect : aliased VkRect2D;  -- src/vulkan/vulkan.h:3449
      dstRect : aliased VkRect2D;  -- src/vulkan/vulkan.h:3450
      persistent : aliased VkBool32;  -- src/vulkan/vulkan.h:3451
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPresentInfoKHR);  -- src/vulkan/vulkan.h:3446

   type PFN_vkCreateSharedSwapchainsKHR is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : System.Address;
         arg5 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateSharedSwapchainsKHR);  -- src/vulkan/vulkan.h:3455

   function vkCreateSharedSwapchainsKHR
     (device : VkDevice;
      swapchainCount : stdint_h.uint32_t;
      pCreateInfos : System.Address;
      pAllocator : System.Address;
      pSwapchains : System.Address) return VkResult;  -- src/vulkan/vulkan.h:3458
   pragma Import (C, vkCreateSharedSwapchainsKHR, "vkCreateSharedSwapchainsKHR");

   type VkDebugReportCallbackEXT is new System.Address;  -- src/vulkan/vulkan.h:3671

   --  skipped empty struct VkDebugReportCallbackEXT_T

   type VkDebugReportObjectTypeEXT is
     (VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT,
      VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT);
   pragma Convention (C, VkDebugReportObjectTypeEXT);  -- src/vulkan/vulkan.h:3677

   type VkDebugReportErrorEXT is
     (VK_DEBUG_REPORT_ERROR_NONE_EXT,
      VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT);
   pragma Convention (C, VkDebugReportErrorEXT);  -- src/vulkan/vulkan.h:3709

   subtype VkDebugReportFlagBitsEXT is unsigned;
   VK_DEBUG_REPORT_INFORMATION_BIT_EXT : constant VkDebugReportFlagBitsEXT := 1;
   VK_DEBUG_REPORT_WARNING_BIT_EXT : constant VkDebugReportFlagBitsEXT := 2;
   VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT : constant VkDebugReportFlagBitsEXT := 4;
   VK_DEBUG_REPORT_ERROR_BIT_EXT : constant VkDebugReportFlagBitsEXT := 8;
   VK_DEBUG_REPORT_DEBUG_BIT_EXT : constant VkDebugReportFlagBitsEXT := 16;  -- src/vulkan/vulkan.h:3715

   subtype VkDebugReportFlagsEXT is VkFlags;  -- src/vulkan/vulkan.h:3722

   type PFN_vkDebugReportCallbackEXT is access function
        (arg1 : VkDebugReportFlagsEXT;
         arg2 : VkDebugReportObjectTypeEXT;
         arg3 : stdint_h.uint64_t;
         arg4 : stddef_h.size_t;
         arg5 : stdint_h.int32_t;
         arg6 : Interfaces.C.Strings.chars_ptr;
         arg7 : Interfaces.C.Strings.chars_ptr;
         arg8 : System.Address) return VkBool32;
   pragma Convention (C, PFN_vkDebugReportCallbackEXT);  -- src/vulkan/vulkan.h:3724

   type VkDebugReportCallbackCreateInfoEXT is record
      sType : aliased VkStructureType;  -- src/vulkan/vulkan.h:3736
      pNext : System.Address;  -- src/vulkan/vulkan.h:3737
      flags : aliased VkDebugReportFlagsEXT;  -- src/vulkan/vulkan.h:3738
      pfnCallback : PFN_vkDebugReportCallbackEXT;  -- src/vulkan/vulkan.h:3739
      pUserData : System.Address;  -- src/vulkan/vulkan.h:3740
   end record;
   pragma Convention (C_Pass_By_Copy, VkDebugReportCallbackCreateInfoEXT);  -- src/vulkan/vulkan.h:3735

   type PFN_vkCreateDebugReportCallbackEXT is access function
        (arg1 : VkInstance;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDebugReportCallbackEXT);  -- src/vulkan/vulkan.h:3744

   type PFN_vkDestroyDebugReportCallbackEXT is access procedure
        (arg1 : VkInstance;
         arg2 : VkDebugReportCallbackEXT;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyDebugReportCallbackEXT);  -- src/vulkan/vulkan.h:3745

   type PFN_vkDebugReportMessageEXT is access procedure
        (arg1 : VkInstance;
         arg2 : VkDebugReportFlagsEXT;
         arg3 : VkDebugReportObjectTypeEXT;
         arg4 : stdint_h.uint64_t;
         arg5 : stddef_h.size_t;
         arg6 : stdint_h.int32_t;
         arg7 : Interfaces.C.Strings.chars_ptr;
         arg8 : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, PFN_vkDebugReportMessageEXT);  -- src/vulkan/vulkan.h:3746

   function vkCreateDebugReportCallbackEXT
     (instance : VkInstance;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pCallback : System.Address) return VkResult;  -- src/vulkan/vulkan.h:3749
   pragma Import (C, vkCreateDebugReportCallbackEXT, "vkCreateDebugReportCallbackEXT");

   procedure vkDestroyDebugReportCallbackEXT
     (instance : VkInstance;
      callback : VkDebugReportCallbackEXT;
      pAllocator : System.Address);  -- src/vulkan/vulkan.h:3755
   pragma Import (C, vkDestroyDebugReportCallbackEXT, "vkDestroyDebugReportCallbackEXT");

   procedure vkDebugReportMessageEXT
     (instance : VkInstance;
      flags : VkDebugReportFlagsEXT;
      objectType : VkDebugReportObjectTypeEXT;
      object : stdint_h.uint64_t;
      location : stddef_h.size_t;
      messageCode : stdint_h.int32_t;
      pLayerPrefix : Interfaces.C.Strings.chars_ptr;
      pMessage : Interfaces.C.Strings.chars_ptr);  -- src/vulkan/vulkan.h:3760
   pragma Import (C, vkDebugReportMessageEXT, "vkDebugReportMessageEXT");


   --  START: Package structure.
   --  Here lists the various child-packages:
   package Instances is
      generic
         type P is access procedure; -- TODO
      procedure Get_Procedure;
   end Instances;

   package Devices is
      generic
         type P is access procedure; -- TODO
      procedure Get_Procedure;
   end Devices;

   package Command_Buffers is
   end Command_Buffers;

   package Queues is
   end Queues;

   package Caches is
   end Caches;

   package Synchronisation is
   end Synchronisation;

   package Render_Passes is
   end Render_Passes;

   package Shaders is
   end Shaders;

   package Pipelines is
   end Pipelines;

   package Memories is
   end Memories;

   package Resources is
      package Sparse is
      end Sparse;
   end Resources;

   package Samplers is
   end Samplers;

   package Layers is
   end Layers;

   package WSI is
      --  Separate various WSI's into directories for inclusion based on command line options as they are platform
      --  specific.
   end WSI;

   --  TODO: Other subprograms in the other sections of the API should fit into one of the above packages.
   --  END: Package structure.
end Vulkan;
