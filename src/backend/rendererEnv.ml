open Cpp
open Zrl.Extensions
module MapString = Map.Make (String)

type render_target_type = Color | DepthStencil

type t = {
  name : string;
  render_targets : render_target_type MapString.t;
  rclass : Class.t;
  render : Function.t;
  ctor : Function.t;
  dtor : Function.t;
}

let get_or_create_render_pass =
  Function.(
    empty "GetOrCreateRenderPass"
    |> set_return_type "VkRenderPass"
    |> add_param ("const std::vector<RenderTargetReference*> &", "rts")
    |> append_code_section
         {|  std::vector<RenderTargetReference> key;
  for (auto rt : rts) {
    key.emplace_back(*rt);
  }
  auto res = render_pass_cache_.find(key);
  if (res == render_pass_cache_.end()) {
    DLOG << name_ << ": creating new render pass" << '\n';
    std::vector<VkAttachmentDescription> attachment_descriptions;
    std::vector<VkAttachmentReference> attachment_references;
    std::vector<VkAttachmentReference> color_attachments;
    VkAttachmentReference depth_stencil_attachment = {};
    depth_stencil_attachment.attachment = VK_ATTACHMENT_UNUSED;
    
    uint32_t attachment_number = 0;
    for (auto rt : rts) {
      VkAttachmentDescription description = {};
      description.format = rt->format;
      description.samples = VK_SAMPLE_COUNT_1_BIT;
      description.loadOp = rt->load_op;
      description.storeOp = VK_ATTACHMENT_STORE_OP_STORE;
      description.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
      description.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
      description.initialLayout = rt->current_layout;
      description.finalLayout = rt->target_layout;
    
      VkAttachmentReference reference = {};
      reference.attachment = attachment_number;
      reference.layout = (rt->format == core_.GetSwapchain().GetSurfaceFormat()
                          ? VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                          : VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL);
    
      if (rt->format == core_.GetSwapchain().GetSurfaceFormat()) {
        color_attachments.push_back(reference);
      } else {
        depth_stencil_attachment = reference;
      }
      attachment_descriptions.push_back(description);
      attachment_references.push_back(reference);
      ++attachment_number;
    }
    
    VkSubpassDescription subpass = {};
    subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
    subpass.colorAttachmentCount = static_cast<uint32_t>(color_attachments.size());
    subpass.pColorAttachments = color_attachments.data();
    subpass.pDepthStencilAttachment = &depth_stencil_attachment;
    std::array<VkSubpassDescription, 1> subpass_descriptions = {{subpass}};
    
    VkSubpassDependency final_to_initial = {};
    final_to_initial.srcSubpass = VK_SUBPASS_EXTERNAL;
    final_to_initial.dstSubpass = 0;
    final_to_initial.srcStageMask = VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT;
    final_to_initial.dstStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    final_to_initial.srcAccessMask = VK_ACCESS_MEMORY_READ_BIT;
    final_to_initial.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT |
                                     VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
    final_to_initial.dependencyFlags = VK_DEPENDENCY_BY_REGION_BIT;
    
    VkSubpassDependency initial_to_final = {};
    initial_to_final.srcSubpass = 0;
    initial_to_final.dstSubpass = VK_SUBPASS_EXTERNAL;
    initial_to_final.srcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    initial_to_final.dstStageMask = VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT;
    initial_to_final.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT |
                                     VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
    initial_to_final.dstAccessMask = VK_ACCESS_MEMORY_READ_BIT;
    initial_to_final.dependencyFlags = VK_DEPENDENCY_BY_REGION_BIT;
    
    std::array<VkSubpassDependency, 2> dependencies = {{
        final_to_initial,
        initial_to_final,
    }};
    
    VkRenderPass rp = VK_NULL_HANDLE;
    VkRenderPassCreateInfo create_info = {};
    create_info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
    create_info.pNext = nullptr;
    create_info.flags = 0;
    create_info.attachmentCount = static_cast<uint32_t>(attachment_descriptions.size());
    create_info.pAttachments = attachment_descriptions.data();
    create_info.subpassCount = static_cast<uint32_t>(subpass_descriptions.size());
    create_info.pSubpasses = subpass_descriptions.data();
    create_info.dependencyCount = static_cast<uint32_t>(dependencies.size());
    create_info.pDependencies = dependencies.data();
    CHECK_VK(vkCreateRenderPass(core_.GetLogicalDevice().GetHandle(),
                                &create_info, nullptr, &rp));
    
    render_pass_cache_[key] = rp;
    return rp;
  }
  return res->second;|})

let get_or_create_framebuffer =
  Function.(
    empty "GetOrCreateFramebuffer"
    |> set_return_type "VkFramebuffer"
    |> add_param ("const std::vector<RenderTargetReference*> &", "rts")
    |> add_param ("VkRenderPass", "render_pass")
    |> append_code_section
         {|  std::vector<RenderTargetReference> key;
  for (auto rt : rts) {
    key.emplace_back(*rt);
  }
  auto res = framebuffer_cache_.find(key);
  if (res == framebuffer_cache_.end()) {
    DLOG << name_ << ": creating new framebuffer" << '\n';
    std::vector<VkImageView> attachments;
    for (auto rt : rts) {
      attachments.push_back(rt->attachment);
    }

    VkFramebuffer fb = VK_NULL_HANDLE;
    VkFramebufferCreateInfo create_info = {};
    create_info.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
    create_info.renderPass = render_pass;
    create_info.attachmentCount = static_cast<uint32_t>(attachments.size());
    create_info.pAttachments = attachments.data();
    create_info.width = core_.GetSwapchain().GetExtent().width;
    create_info.height = core_.GetSwapchain().GetExtent().height;
    create_info.layers = 1;
    CHECK_VK(vkCreateFramebuffer(core_.GetLogicalDevice().GetHandle(),
                                 &create_info, nullptr, &fb));
    framebuffer_cache_[key] = fb;
    return fb;
  }
  return res->second;|})

let create_command_pool =
  Function.(
    empty "CreateCommandPool"
    |> set_return_type "VkCommandPool"
    |> add_param ("uint32_t", "family")
    |> append_code_section
         {|  const VkDevice device = core_.GetLogicalDevice().GetHandle();
  VkCommandPool cmd_pool = VK_NULL_HANDLE;
  VkCommandPoolCreateInfo create_info = {};
  create_info.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
  create_info.queueFamilyIndex = family;
  create_info.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
  CHECK_VK(vkCreateCommandPool(device, &create_info, nullptr, &cmd_pool));
  return cmd_pool;|})

let create_command_buffers =
  Function.(
    empty "CreateCommandBuffers"
    |> set_return_type "std::vector<VkCommandBuffer>"
    |> add_param ("VkCommandPool", "pool")
    |> add_param ("VkCommandBufferLevel", "level")
    |> add_param ("uint32_t", "count")
    |> append_code_section
         {|  const VkDevice device = core_.GetLogicalDevice().GetHandle();
  VkCommandBufferAllocateInfo alloc_info = {};
  alloc_info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  alloc_info.commandPool = pool;
  alloc_info.level = level;
  alloc_info.commandBufferCount = count;
  std::vector<VkCommandBuffer> cmd_buffers(count);
  CHECK_VK(vkAllocateCommandBuffers(device, &alloc_info, cmd_buffers.data()));
  return cmd_buffers;|})

let create_events =
  Function.(
    empty "CreateEvents"
    |> set_return_type "std::vector<VkEvent>"
    |> add_param ("uint32_t", "count")
    |> append_code_section
         {|  const VkDevice device = core_.GetLogicalDevice().GetHandle();
  VkEventCreateInfo event_create_info = {};
  event_create_info.sType = VK_STRUCTURE_TYPE_EVENT_CREATE_INFO;
  event_create_info.pNext = nullptr;
  event_create_info.flags = 0;
  std::vector<VkEvent> events(count);
  for (size_t i = 0; i < events.size(); ++i) {
    CHECK_VK(vkCreateEvent(device, &event_create_info, nullptr, &events[i]));
  }
  return events;|})

let create_fences =
  Function.(
    empty "CreateFences"
    |> set_return_type "std::vector<VkFence>"
    |> add_param ("uint32_t", "count")
    |> append_code_section
         {|  const VkDevice device = core_.GetLogicalDevice().GetHandle();
  VkFenceCreateInfo fence_create_info = {};
  fence_create_info.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
  fence_create_info.flags = VK_FENCE_CREATE_SIGNALED_BIT;
  std::vector<VkFence> fences(count);
  for (size_t i = 0; i < fences.size(); ++i) {
    CHECK_VK(vkCreateFence(device, &fence_create_info, nullptr, &fences[i]));
  }
  return fences;|})

let create_image_view =
  Function.(
    empty "CreateImageView"
    |> set_return_type "VkImageView"
    |> add_param ("VkImage", "image")
    |> add_param ("VkFormat", "format")
    |> append_code_section
         {|  const VkDevice device = core_.GetLogicalDevice().GetHandle();
  VkImageView image_view = VK_NULL_HANDLE;
  VkImageViewCreateInfo create_info = {};
  create_info.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
  create_info.image = image;
  create_info.viewType = VK_IMAGE_VIEW_TYPE_2D;
  create_info.format = format;
  create_info.components.r = VK_COMPONENT_SWIZZLE_IDENTITY;
  create_info.components.g = VK_COMPONENT_SWIZZLE_IDENTITY;
  create_info.components.b = VK_COMPONENT_SWIZZLE_IDENTITY;
  create_info.components.a = VK_COMPONENT_SWIZZLE_IDENTITY;
  create_info.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
  create_info.subresourceRange.baseMipLevel = 0;
  create_info.subresourceRange.levelCount = 1;
  create_info.subresourceRange.baseArrayLayer = 0;
  create_info.subresourceRange.layerCount = 1;
  CHECK_VK(vkCreateImageView(device, &create_info, nullptr, &image_view));
  return image_view;|})

let create_color_render_target =
  Function.(
    empty "CreateColorRenderTarget"
    |> set_return_type "std::unique_ptr<zrl::Image>"
    |> append_code_section
         {|  const VkFormat format = core_.GetSwapchain().GetSurfaceFormat();
  constexpr VkImageUsageFlags usage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | 
                                      VK_IMAGE_USAGE_SAMPLED_BIT;
  return zrl::Image::Image2D(core_, core_.GetSwapchain().GetExtent(), 1, 1,
                             format, usage);|})

let create_depth_render_target =
  Function.(
    empty "CreateDepthRenderTarget"
    |> set_return_type "std::unique_ptr<zrl::Image>"
    |> append_code_section
         {|  constexpr VkFormat format = VK_FORMAT_D16_UNORM;
  constexpr VkImageUsageFlags usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
  return zrl::Image::DepthBuffer(core_, core_.GetSwapchain().GetExtent(),
                                 format, usage);|})

let bind_sampled_image2D =
  Function.(
    empty "BindSampledImage2D" |> set_return_type "void"
    |> add_param ("VkDescriptorSet", "set")
    |> add_param ("uint32_t", "binding")
    |> add_param ("SampledImage2DReference&", "ref")
    |> add_param ("std::vector<std::unique_ptr<zrl::Image>>&", "images")
    |> append_code_section
         {|
      const VkExtent2D extent = {ref.width, ref.height};
      const bool is_empty  = extent.width * extent.height == 0;
      uint32_t layer_count = static_cast<uint32_t>(ref.image_data.size());
      uint32_t level_count = (layer_count == 0)
                             ? 1
                             : static_cast<uint32_t>(ref.image_data[0].size());

      // Create sampler if needed.
      VkSampler sampler = VK_NULL_HANDLE;
      if (is_empty) {
        sampler = dummy_sampler_;
      } else {
        ref.sampler_create_info.sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
        ref.sampler_create_info.pNext = nullptr;
        if (ref.build_mipmaps) {
          ref.sampler_create_info.maxLod = std::floor(
                                           std::log2(
                                           std::max(ref.width, ref.height))) + 1;
        }
        auto it = sampler_cache_.find(ref.sampler_create_info);
        if (it == sampler_cache_.end()) {
          DLOG << name_ << ": creating new sampler\n";
          CHECK_VK(vkCreateSampler(core_.GetLogicalDevice().GetHandle(),
                                   &ref.sampler_create_info, nullptr, &sampler));
          sampler_cache_[ref.sampler_create_info] = sampler;
        } else {
          sampler = it->second;
        }
      }

      VkImageView img_view = VK_NULL_HANDLE;
      VkImageLayout img_layout = VK_IMAGE_LAYOUT_UNDEFINED;
      if (ref.rt_ref != nullptr) { 
        // Load image from render target.
        img_view = ref.rt_ref->attachment;
        img_layout = VK_IMAGE_LAYOUT_GENERAL;
      } else { 
        // Create image resource.
        VkImageUsageFlags usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT |
                                  VK_IMAGE_USAGE_SAMPLED_BIT;
        img_layout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        for (size_t i = 1; i < ref.image_data.size(); ++i) {
          CHECK_PC(ref.image_data[0].size() == ref.image_data[i].size(), 
                   "all image layers must have the same level count");
        }

        std::unique_ptr<zrl::Image> img;
        if (is_empty) {
          CHECK_PC(!ref.build_mipmaps, "cannot build mipmaps for dummy texture");
          LOG(WARNING) << name_ << ": missing texture, will use dummy 4x4 image\n";
          layer_count = 1;
          img = zrl::Image::Image2D(core_, VkExtent2D{4, 4}, level_count, 
                                    layer_count, core_.GetSwapchain().GetSurfaceFormat(), 
                                    usage);
        } else {
          if (ref.build_mipmaps) {
            CHECK_PC(level_count == 1, 
                     "build_mipmaps requested but image data already has multiple levels");
            VkFormatProperties props = core_.GetLogicalDevice()
                                            .GetPhysicalDevice()
                                            .GetFormatProperties(ref.format);
            CHECK_PC(props.optimalTilingFeatures & VK_FORMAT_FEATURE_BLIT_SRC_BIT,
                     "BLIT_SRC is required for building mipmaps");
            CHECK_PC(props.optimalTilingFeatures & VK_FORMAT_FEATURE_BLIT_DST_BIT,
                     "BLIT_DST is required for building mipmaps");
            level_count = std::floor(std::log2(std::max(ref.width, ref.height))) + 1;
            usage ^= VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
          }
          img = zrl::Image::Image2D(core_, extent, level_count, layer_count, 
                                    ref.format, usage);
        }

        // Schedule copy and barriers.
        VkImageMemoryBarrier pre_copy_barrier = {};
        pre_copy_barrier.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
        pre_copy_barrier.pNext = nullptr;
        pre_copy_barrier.srcAccessMask = 0;
        pre_copy_barrier.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        pre_copy_barrier.oldLayout = VK_IMAGE_LAYOUT_UNDEFINED;
        pre_copy_barrier.newLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        pre_copy_barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
        pre_copy_barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
        pre_copy_barrier.image = img->GetHandle();
        pre_copy_barrier.subresourceRange.aspectMask = img->GetAspects();
        pre_copy_barrier.subresourceRange.baseMipLevel = 0;
        pre_copy_barrier.subresourceRange.levelCount = ref.build_mipmaps ? 1 : img->GetLevelCount();
        pre_copy_barrier.subresourceRange.baseArrayLayer = 0;
        pre_copy_barrier.subresourceRange.layerCount = img->GetLayerCount();
        pending_pre_copy_image_barriers_.push_back(pre_copy_barrier);

        if (!is_empty) {
          const uint32_t copy_levels = ref.build_mipmaps ? 1 : level_count;
          for (uint32_t layer = 0; layer < layer_count; ++layer) {
            for (uint32_t level = 0; level < copy_levels; ++level) {
              auto src_offset = staging_buffer_->PushData(ref.size >> level, 
                                                          ref.image_data[layer][level]);
              delete ref.image_data[layer][level];

              const VkExtent3D full_extent = img->GetExtent();
              const VkExtent3D copy_extent = {full_extent.width >> level, full_extent.height >> level, 1};
              VkBufferImageCopy buffer_image_copy = {};
              buffer_image_copy.bufferOffset = src_offset;
              buffer_image_copy.bufferRowLength = 0;
              buffer_image_copy.bufferImageHeight = 0;
              buffer_image_copy.imageSubresource.aspectMask = img->GetAspects();
              buffer_image_copy.imageSubresource.mipLevel = level;
              buffer_image_copy.imageSubresource.baseArrayLayer = layer;
              buffer_image_copy.imageSubresource.layerCount = 1;
              buffer_image_copy.imageOffset = {0, 0, 0};
              buffer_image_copy.imageExtent = copy_extent;
              pending_image_copies_.push_back(
                  std::make_tuple(img->GetHandle(), buffer_image_copy, ref.build_mipmaps));
            }
          }
        }

        if (!ref.build_mipmaps) {
          VkImageMemoryBarrier post_copy_barrier = {};
          post_copy_barrier.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
          post_copy_barrier.pNext = nullptr;
          post_copy_barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
          post_copy_barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
          post_copy_barrier.oldLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
          post_copy_barrier.newLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          post_copy_barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
          post_copy_barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
          post_copy_barrier.image = img->GetHandle();
          post_copy_barrier.subresourceRange.aspectMask = img->GetAspects();
          post_copy_barrier.subresourceRange.baseMipLevel = 0;
          post_copy_barrier.subresourceRange.levelCount = img->GetLevelCount();
          post_copy_barrier.subresourceRange.baseArrayLayer = 0;
          post_copy_barrier.subresourceRange.layerCount = img->GetLayerCount();
          pending_post_copy_image_barriers_.push_back(post_copy_barrier);
        }
        img_view = img->GetViewHandle();
        images.push_back(std::move(img));
      }

      // Update descriptor set.
      VkDescriptorImageInfo image_info = {};
      image_info.sampler = sampler;
      image_info.imageView = img_view; 
      image_info.imageLayout = img_layout;

      VkWriteDescriptorSet write = {};
      write.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
      write.pNext = nullptr;
      write.dstSet = set;
      write.dstBinding = binding;
      write.dstArrayElement = 0;
      write.descriptorCount = 1;
      write.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
      write.pImageInfo = &image_info;
      write.pBufferInfo = nullptr;
      write.pTexelBufferView = nullptr;

      vkUpdateDescriptorSets(core_.GetLogicalDevice().GetHandle(),
                             1, &write, 0, nullptr);
    |})

let bind_cube_map =
  Function.(
    empty "BindCubeMap" |> set_return_type "void"
    |> add_param ("VkDescriptorSet", "set")
    |> add_param ("uint32_t", "binding")
    |> add_param ("SampledCubeMapReference&", "ref")
    |> add_param ("std::vector<std::unique_ptr<zrl::Image>>&", "images")
    |> append_code_section
         {|
      const VkExtent2D extent = {ref.width, ref.height};
      const bool is_empty  = extent.width * extent.height == 0;
      constexpr uint32_t layer_count = 6;
      const uint32_t level_count = ref.image_data[0].size();
      for (size_t i = 1; i < ref.image_data.size(); ++i) {
        CHECK_PC(ref.image_data[0].size() == ref.image_data[i].size(), 
                 "all image layers must have the same level count");
      }

      // Create sampler if needed.
      VkSampler sampler = VK_NULL_HANDLE;
      if (is_empty) {
        sampler = dummy_sampler_;
      } else {
        ref.sampler_create_info.sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
        ref.sampler_create_info.pNext = nullptr;
        if (ref.sampler_create_info.maxLod == 0 && level_count > 1) {
          ref.sampler_create_info.maxLod = static_cast<float>(level_count);
        }
        auto it = sampler_cache_.find(ref.sampler_create_info);
        if (it == sampler_cache_.end()) {
          DLOG << name_ << ": creating new sampler\n";
          CHECK_VK(vkCreateSampler(core_.GetLogicalDevice().GetHandle(),
                                   &ref.sampler_create_info, nullptr, &sampler));
          sampler_cache_[ref.sampler_create_info] = sampler;
        } else {
          sampler = it->second;
        }
      }

      VkImageView img_view = VK_NULL_HANDLE;
      VkImageLayout img_layout = VK_IMAGE_LAYOUT_UNDEFINED;

      // Create image resource.
      VkImageUsageFlags usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT |
                                VK_IMAGE_USAGE_SAMPLED_BIT;
      img_layout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;

      std::unique_ptr<zrl::Image> img;
      if (is_empty) {
        LOG(WARNING) << name_ << ": missing cubemap, will use dummy 4x4 cubemap\n";
        img = zrl::Image::CubeMap(core_, VkExtent2D{4, 4}, level_count,
                                  core_.GetSwapchain().GetSurfaceFormat(), 
                                  usage);
      } else {
        img = zrl::Image::CubeMap(core_, extent, level_count, ref.format, usage);
      }

      // Schedule copy and barriers.
      VkImageMemoryBarrier pre_copy_barrier = {};
      pre_copy_barrier.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
      pre_copy_barrier.pNext = nullptr;
      pre_copy_barrier.srcAccessMask = 0;
      pre_copy_barrier.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
      pre_copy_barrier.oldLayout = VK_IMAGE_LAYOUT_UNDEFINED;
      pre_copy_barrier.newLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
      pre_copy_barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
      pre_copy_barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
      pre_copy_barrier.image = img->GetHandle();
      pre_copy_barrier.subresourceRange.aspectMask = img->GetAspects();
      pre_copy_barrier.subresourceRange.baseMipLevel = 0;
      pre_copy_barrier.subresourceRange.levelCount = img->GetLevelCount();
      pre_copy_barrier.subresourceRange.baseArrayLayer = 0;
      pre_copy_barrier.subresourceRange.layerCount = img->GetLayerCount();
      pending_pre_copy_image_barriers_.push_back(pre_copy_barrier);

      if (!is_empty) {
        for (uint32_t layer = 0; layer < layer_count; ++layer) {
          for (uint32_t level = 0; level < level_count; ++level) {
            auto src_offset = staging_buffer_->PushData(ref.size >> level, 
                                                        ref.image_data[layer][level]);
            delete ref.image_data[layer][level];

            const VkExtent3D full_extent = img->GetExtent();
            const VkExtent3D copy_extent = {full_extent.width >> level, full_extent.height >> level, 1};
            VkBufferImageCopy buffer_image_copy = {};
            buffer_image_copy.bufferOffset = src_offset;
            buffer_image_copy.bufferRowLength = 0;
            buffer_image_copy.bufferImageHeight = 0;
            buffer_image_copy.imageSubresource.aspectMask = img->GetAspects();
            buffer_image_copy.imageSubresource.mipLevel = level;
            buffer_image_copy.imageSubresource.baseArrayLayer = layer;
            buffer_image_copy.imageSubresource.layerCount = 1;
            buffer_image_copy.imageOffset = {0, 0, 0};
            buffer_image_copy.imageExtent = copy_extent;
            pending_image_copies_.push_back(
                std::make_tuple(img->GetHandle(), buffer_image_copy, false));
          }
        }
      }

      VkImageMemoryBarrier post_copy_barrier = {};
      post_copy_barrier.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
      post_copy_barrier.pNext = nullptr;
      post_copy_barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
      post_copy_barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
      post_copy_barrier.oldLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
      post_copy_barrier.newLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
      post_copy_barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
      post_copy_barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
      post_copy_barrier.image = img->GetHandle();
      post_copy_barrier.subresourceRange.aspectMask = img->GetAspects();
      post_copy_barrier.subresourceRange.baseMipLevel = 0;
      post_copy_barrier.subresourceRange.levelCount = img->GetLevelCount();
      post_copy_barrier.subresourceRange.baseArrayLayer = 0;
      post_copy_barrier.subresourceRange.layerCount = img->GetLayerCount();
      pending_post_copy_image_barriers_.push_back(post_copy_barrier);

      img_view = img->GetViewHandle();
      images.push_back(std::move(img));

      // Update descriptor set.
      VkDescriptorImageInfo image_info = {};
      image_info.sampler = sampler;
      image_info.imageView = img_view; 
      image_info.imageLayout = img_layout;

      VkWriteDescriptorSet write = {};
      write.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
      write.pNext = nullptr;
      write.dstSet = set;
      write.dstBinding = binding;
      write.dstArrayElement = 0;
      write.descriptorCount = 1;
      write.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
      write.pImageInfo = &image_info;
      write.pBufferInfo = nullptr;
      write.pTexelBufferView = nullptr;

      vkUpdateDescriptorSets(core_.GetLogicalDevice().GetHandle(),
                             1, &write, 0, nullptr);
    |})

let ctor_body =
  {|  const auto &device = core_.GetLogicalDevice();
  //======================================================================
  // Command Pools and Command Buffers and Sync Primitives
  DLOG << name_ << ": creating gct command pool\n";
  gct_cmd_pool_ = CreateCommandPool(device.GetGCTQueueFamily());
  DLOG << name_ << ": allocating graphics-compute command buffer\n";
  gc_cmd_buffer_ = CreateCommandBuffers(gct_cmd_pool_, 
                                        VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                                        core_.GetSwapchain().GetImageCount());
  DLOG << name_ << ": allocating transfer command buffer\n";
  t_cmd_buffer_ = CreateCommandBuffers(gct_cmd_pool_, 
                                       VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                                       core_.GetSwapchain().GetImageCount());
  gct_cmd_buffer_fence_ = CreateFences(core_.GetSwapchain().GetImageCount());
  if (device.IsSingleQueue()) {
    DLOG << name_ << ": reusing gct command pool as present command pool\n";
    present_cmd_pool_ = gct_cmd_pool_;
    present_cmd_buffer_ = gc_cmd_buffer_;
    present_cmd_buffer_fence_ = gct_cmd_buffer_fence_;
  } else {
    DLOG << name_ << ": creating present command pool\n";
    present_cmd_pool_ = CreateCommandPool(device.GetPresentQueueFamily());
    DLOG << name_ << ": allocating present command buffer\n";
    present_cmd_buffer_ = CreateCommandBuffers(present_cmd_pool_, 
                                               VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                                               core_.GetSwapchain().GetImageCount());
    present_cmd_buffer_fence_ = CreateFences(core_.GetSwapchain().GetImageCount());
  }
  DLOG << name_ << ": creating semaphores\n";
  VkSemaphoreCreateInfo semaphore_create_info = {};
  semaphore_create_info.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
  CHECK_VK(vkCreateSemaphore(device.GetHandle(), &semaphore_create_info, nullptr, 
                             &acquire_semaphore_));
  CHECK_VK(vkCreateSemaphore(device.GetHandle(), &semaphore_create_info, nullptr, 
                             &render_semaphore_));
  //======================================================================
  // Descriptor Pool and Descriptor Sets
  DLOG << name_ << ": creating descriptor pool\n";
  // TODO: make descriptor set counts configurable
  std::array<VkDescriptorPoolSize, 2> descriptor_pool_sizes = {
      VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 1<<10},
      VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1<<20}
  };

  // TODO: make maxSets configurable
  VkDescriptorPoolCreateInfo descriptor_pool_create_info = {};
  descriptor_pool_create_info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
  descriptor_pool_create_info.pNext = nullptr;
  descriptor_pool_create_info.flags = 0;
  descriptor_pool_create_info.maxSets = 1<<20;
  descriptor_pool_create_info.poolSizeCount = static_cast<uint32_t>(descriptor_pool_sizes.size());
  descriptor_pool_create_info.pPoolSizes = descriptor_pool_sizes.data();
  CHECK_VK(vkCreateDescriptorPool(device.GetHandle(), &descriptor_pool_create_info, 
                                  nullptr, &descriptor_pool_));
  discarded_descriptor_sets_.resize(core_.GetSwapchain().GetImageCount());
  //======================================================================
  // Staging Buffer
  DLOG << name_ << ": creating staging buffer\n";
  staging_buffer_ = std::make_unique<zrl::StagingBuffer>(core_, zrl::_512MB);
  //======================================================================
  // Vertex Buffer State
  DLOG << name_ << ": creating vertex buffer pool\n";
  vb_buffer_pool_ = std::make_unique<zrl::BufferPool>(
      core, "vb", zrl::_128MB, 
      VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
      VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT |
      VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
      zrl::_1KB, false);
  vb_discarded_blocks_.resize(core_.GetSwapchain().GetImageCount());
  dummy_block_ = vb_buffer_pool_->Alloc(16);
  //======================================================================
  // Uniform Buffer State
  DLOG << name_ << ": creating uniform buffer pool\n";
  VkDeviceSize minUBOBlockSize =
      std::min(std::max(core_.GetLogicalDevice()
                            .GetPhysicalDevice()
                            .GetProperties()
                            .limits.minUniformBufferOffsetAlignment,
                        64ul),
               256ul);
  ubo_buffer_pool_ = std::make_unique<zrl::BufferPool>(
      core, "ubo", zrl::_128MB, 
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
      VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
      minUBOBlockSize, true);
  ubo_discarded_blocks_.resize(core_.GetSwapchain().GetImageCount());
  //======================================================================
  // Sampled Images State
  discarded_images_.resize(core_.GetSwapchain().GetImageCount());
  {
    VkSamplerCreateInfo create_info = {};
    create_info.sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
    create_info.pNext = nullptr;
    create_info.flags = 0;
    CHECK_VK(vkCreateSampler(device.GetHandle(), &create_info, nullptr, &dummy_sampler_));
  }
  //======================================================================
  // Render Target State
  DLOG << name_ << ": initializing render targets\n";
  rt_builtin_screen_img_view_.resize(core_.GetSwapchain().GetImageCount());
  for (size_t i = 0; i < rt_builtin_screen_img_view_.size(); ++i) {
    VkImage img = core_.GetSwapchain().GetImages()[i];
    VkImageView view = CreateImageView(img,
                                       core_.GetSwapchain().GetSurfaceFormat());
    rt_builtin_screen_img_view_[i] = view;
    rt_builtin_screen_ref_.push_back({core_.GetSwapchain().GetSurfaceFormat(), view, img,
                                      VK_IMAGE_LAYOUT_UNDEFINED, 
                                      VK_IMAGE_LAYOUT_UNDEFINED, 
                                      VK_ATTACHMENT_LOAD_OP_DONT_CARE, 
                                      VkClearValue{}});
  }
  //======================================================================
|}

let dtor_body =
  {|  staging_buffer_.reset();
  ubo_buffer_pool_.reset();
  vb_buffer_pool_.reset();
  for (auto rt : rt_builtin_screen_ref_) {
    vkDestroyImageView(device, rt.attachment, nullptr);
  }
  DLOG << name_ << ": total samplers: " << sampler_cache_.size() << '\n';
  vkDestroySampler(device, dummy_sampler_, nullptr);
  for (auto p : sampler_cache_) {
    vkDestroySampler(device, p.second, nullptr);
  }
  DLOG << name_ << ": total graphics pipelines: " << pipeline_cache_.size() << '\n';
  for (auto p : pipeline_cache_) {
    vkDestroyPipeline(device, p.second, nullptr);
  }
  DLOG << name_ << ": total framebuffers: " << framebuffer_cache_.size() << '\n';
  for (auto p : framebuffer_cache_) {
    vkDestroyFramebuffer(device, p.second, nullptr);
  }
  DLOG << name_ << ": total render passes: " << render_pass_cache_.size() << '\n';
  for (auto p : render_pass_cache_) {
    vkDestroyRenderPass(device, p.second, nullptr);
  }
  vkDestroySemaphore(device, acquire_semaphore_, nullptr);
  vkDestroySemaphore(device, render_semaphore_, nullptr);
  vkDestroyCommandPool(device, gct_cmd_pool_, nullptr);
  vkDestroyDescriptorPool(device, descriptor_pool_, nullptr);
  for (auto fence : gct_cmd_buffer_fence_) {
    vkDestroyFence(device, fence, nullptr);
  }
  if (!core_.GetLogicalDevice().IsSingleQueue()) {
    vkDestroyCommandPool(device, present_cmd_pool_, nullptr);
    for (auto fence : present_cmd_buffer_fence_) {
      vkDestroyFence(device, fence, nullptr);
    }
  }|}

let begin_recording =
  Function.(
    empty "BeginRecording" |> set_return_type "bool"
    |> append_code_section
         {|  const VkDevice device = core_.GetLogicalDevice().GetHandle();
  // Acquire swapchain image.
  VkResult result = 
      vkAcquireNextImageKHR(device, core_.GetSwapchain().GetHandle(),
                            std::numeric_limits<uint64_t>::max(),
                            acquire_semaphore_, VK_NULL_HANDLE, &image_index_);
  if (result == VK_ERROR_OUT_OF_DATE_KHR) {
    LOG(WARNING) << name_ << ": swapchain out of date\n";
    core_.UpdateSwapchain();
    for (auto img_view : rt_builtin_screen_img_view_) {
      vkDestroyImageView(device, img_view, nullptr);
    }
    rt_builtin_screen_ref_.clear();
    rt_builtin_screen_img_view_.resize(core_.GetSwapchain().GetImageCount());
    for (size_t i = 0; i < rt_builtin_screen_img_view_.size(); ++i) {
      VkImage img = core_.GetSwapchain().GetImages()[i];
      VkImageView view =
          CreateImageView(img, core_.GetSwapchain().GetSurfaceFormat());
      rt_builtin_screen_img_view_[i] = view;
      rt_builtin_screen_ref_.push_back(
          {core_.GetSwapchain().GetSurfaceFormat(), view, img, VK_IMAGE_LAYOUT_UNDEFINED,
           VK_IMAGE_LAYOUT_UNDEFINED, VK_ATTACHMENT_LOAD_OP_DONT_CARE,
           VkClearValue{}});
    }
    return true;
  }
  CHECK_VK(result);

  // Wait for cmd buffer to be ready.
  CHECK_VK(vkWaitForFences(device, 1, &gct_cmd_buffer_fence_[image_index_], VK_TRUE,
                     std::numeric_limits<uint64_t>::max()));
  CHECK_VK(vkResetFences(device, 1, &gct_cmd_buffer_fence_[image_index_]));

  VkCommandBufferBeginInfo cmd_buffer_begin_info = {};
  cmd_buffer_begin_info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
  cmd_buffer_begin_info.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
  cmd_buffer_begin_info.pInheritanceInfo = nullptr;
  CHECK_VK(vkBeginCommandBuffer(t_cmd_buffer_[image_index_], &cmd_buffer_begin_info));
  CHECK_VK(vkBeginCommandBuffer(gc_cmd_buffer_[image_index_], &cmd_buffer_begin_info));
  return false;
|})

let clear_descriptor_set_register =
  Function.(
    empty "ClearDescriptorSetRegister"
    |> set_return_type "void"
    |> add_param ("int", "set_index")
    |> append_code_section
         {|
    CHECK_PC(set_index < kDescriptorSetRegisterCount, "invalid descriptor set register");
    auto &r = descriptor_set_registers_[set_index];
    if (!r.in_use) { return; }
    r.in_use = false;
    r.uid = 0;
|})

let recycle_or_allocate_descriptor_set =
  Function.(
    empty "RecycleOrAllocateDescriptorSet"
    |> set_return_type "VkDescriptorSet"
    |> add_param ("VkDescriptorSetLayout", "layout")
    |> append_code_section
         {|
  VkDescriptorSet set = VK_NULL_HANDLE;
  if (recycled_descriptor_sets_[layout].size() > 0) {
    set = recycled_descriptor_sets_[layout].back();
    recycled_descriptor_sets_[layout].pop_back();
  } else {
    VkDescriptorSetAllocateInfo alloc_info = {};
    alloc_info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
    alloc_info.pNext = nullptr;
    alloc_info.descriptorPool = descriptor_pool_;
    alloc_info.descriptorSetCount = 1;
    alloc_info.pSetLayouts = &layout;
    CHECK_VK(vkAllocateDescriptorSets(core_.
                                      GetLogicalDevice().GetHandle(), &alloc_info,
                                      &set));
  }
  return set;
|})

let stop_recording_body =
  {|  if (current_render_pass_ != VK_NULL_HANDLE) {
    vkCmdEndRenderPass(gc_cmd_buffer_[image_index_]);
  }

  // Extra render pass to flush pending screen clears and transition
  // swapchain image view into PRESENT layout.
  {
    builtin.screen->target_layout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
    const std::vector<RenderTargetReference*> desc = {builtin.screen};
    VkRenderPass render_pass = GetOrCreateRenderPass(desc);
    VkFramebuffer framebuffer = GetOrCreateFramebuffer(desc, render_pass);
    VkRenderPassBeginInfo begin_info = {};
    begin_info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
    begin_info.pNext = nullptr;
    begin_info.renderPass = render_pass;
    begin_info.framebuffer = framebuffer;
    begin_info.renderArea.offset = {0, 0};
    begin_info.renderArea.extent = core_.GetSwapchain().GetExtent();
    begin_info.clearValueCount = 1;
    begin_info.pClearValues = &builtin.screen->clear_value;
    vkCmdBeginRenderPass(gc_cmd_buffer_[image_index_], &begin_info,
                         VK_SUBPASS_CONTENTS_INLINE);
    vkCmdEndRenderPass(gc_cmd_buffer_[image_index_]);
    builtin.screen->current_layout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
  }

  staging_buffer_->Flush();

  // Schedule pending vertex buffer copies.
  if (vb_pending_copies_.size() > 0) {
    vkCmdCopyBuffer(t_cmd_buffer_[image_index_], 
                    staging_buffer_->GetHandle(),
                    vb_buffer_pool_->GetHandle(), 
                    static_cast<uint32_t>(vb_pending_copies_.size()),
                    vb_pending_copies_.data());
  }

  // Schedule image barriers and copies.
  if (pending_image_copies_.size() > 0) {
    vkCmdPipelineBarrier(t_cmd_buffer_[image_index_],
                         VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                         VK_PIPELINE_STAGE_TRANSFER_BIT,
                         VK_DEPENDENCY_BY_REGION_BIT,
                         0, nullptr, 0, nullptr,
                         static_cast<uint32_t>(pending_pre_copy_image_barriers_.size()),
                         pending_pre_copy_image_barriers_.data());
    for (const auto t : pending_image_copies_) {
      VkImage image = VK_NULL_HANDLE;
      VkBufferImageCopy buffer_image_copy;
      bool build_mipmaps = false;
      std::tie(image, buffer_image_copy, build_mipmaps) = t;
      vkCmdCopyBufferToImage(t_cmd_buffer_[image_index_],
                             staging_buffer_->GetHandle(),
                             image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                             1, &buffer_image_copy);
      if (build_mipmaps) { 
        const VkExtent3D extent = buffer_image_copy.imageExtent;
        const uint32_t layer = buffer_image_copy.imageSubresource.baseArrayLayer;
        const uint32_t mip_levels = std::floor(
                                    std::log2(
                                    std::max(extent.width, extent.height))) + 1;

        // Setup the common fields of a barrier struct.
        VkImageMemoryBarrier barrier = {};
        barrier.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
        barrier.pNext = nullptr;
        barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
        barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
        barrier.image = std::get<0>(t);
        barrier.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
        barrier.subresourceRange.baseArrayLayer = layer;
        barrier.subresourceRange.layerCount = 1;
        barrier.subresourceRange.levelCount = 1;

        // Transition level 0 to TRANSFER_SRC_OPTIMAL.
        barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        barrier.dstAccessMask = 0;
        barrier.oldLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        barrier.newLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        barrier.subresourceRange.baseMipLevel = 0;
        vkCmdPipelineBarrier(t_cmd_buffer_[image_index_],
                             VK_PIPELINE_STAGE_TRANSFER_BIT,
                             VK_PIPELINE_STAGE_HOST_BIT,
                             VK_DEPENDENCY_BY_REGION_BIT,
                             0, nullptr, 0, nullptr, 1, &barrier);

        // Generate levels.
        for (uint32_t i = 1; i < mip_levels; ++i) {
          // Prepare current level by transition to transfer dst.
          barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
          barrier.dstAccessMask = 0;
          barrier.oldLayout = VK_IMAGE_LAYOUT_UNDEFINED;
          barrier.newLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
          barrier.subresourceRange.baseMipLevel = i;
          vkCmdPipelineBarrier(t_cmd_buffer_[image_index_],
                               VK_PIPELINE_STAGE_TRANSFER_BIT,
                               VK_PIPELINE_STAGE_HOST_BIT,
                               VK_DEPENDENCY_BY_REGION_BIT,
                               0, nullptr, 0, nullptr, 1, &barrier);
          
          // Blit previous level.
          VkImageBlit blit = {};
          blit.srcSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
          blit.srcSubresource.baseArrayLayer = layer;
          blit.srcSubresource.layerCount = 1;
          blit.srcSubresource.mipLevel = i-1;
          blit.srcOffsets[1].x = static_cast<int32_t>(extent.width >> (i-1));
          blit.srcOffsets[1].y = static_cast<int32_t>(extent.height >> (i-1));
          blit.srcOffsets[1].z = 1;
          blit.dstSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
          blit.dstSubresource.baseArrayLayer = layer;
          blit.dstSubresource.layerCount = 1;
          blit.dstSubresource.mipLevel = i;
          blit.dstOffsets[1].x = static_cast<int32_t>(extent.width >> i);
          blit.dstOffsets[1].y = static_cast<int32_t>(extent.height >> i);
          blit.dstOffsets[1].z = 1;
          vkCmdBlitImage(t_cmd_buffer_[image_index_], 
                         barrier.image, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                         barrier.image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                         1, &blit, VK_FILTER_LINEAR);

          // Setup current level as source for the next level.
          barrier.srcAccessMask = 0;
          barrier.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
          barrier.oldLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
          barrier.newLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
          barrier.subresourceRange.baseMipLevel = i;
          vkCmdPipelineBarrier(t_cmd_buffer_[image_index_],
                               VK_PIPELINE_STAGE_HOST_BIT,
                               VK_PIPELINE_STAGE_TRANSFER_BIT,
                               VK_DEPENDENCY_BY_REGION_BIT,
                               0, nullptr, 0, nullptr, 1, &barrier);
        }

        // Transition all levels to SHADER_READ_ONLY_OPTIMAL.
        barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
        barrier.oldLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        barrier.newLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        barrier.subresourceRange.levelCount = mip_levels;
        barrier.subresourceRange.baseMipLevel = 0;
        vkCmdPipelineBarrier(t_cmd_buffer_[image_index_],
                             VK_PIPELINE_STAGE_TRANSFER_BIT,
                             VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
                             VK_DEPENDENCY_BY_REGION_BIT,
                             0, nullptr, 0, nullptr, 1, &barrier);
      }
    }
    vkCmdPipelineBarrier(t_cmd_buffer_[image_index_],
                         VK_PIPELINE_STAGE_TRANSFER_BIT,
                         VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
                         VK_DEPENDENCY_BY_REGION_BIT,
                         0, nullptr, 0, nullptr,
                         static_cast<uint32_t>(pending_post_copy_image_barriers_.size()),
                         pending_post_copy_image_barriers_.data());
  }

  // Stop recording.
  CHECK_VK(vkEndCommandBuffer(t_cmd_buffer_[image_index_]));
  CHECK_VK(vkEndCommandBuffer(gc_cmd_buffer_[image_index_]));

  std::array<VkCommandBuffer, 2> cmd_buffers = {
      t_cmd_buffer_[image_index_],
      gc_cmd_buffer_[image_index_],
  };

  // Submit render command buffer.
  constexpr std::array<VkPipelineStageFlags, 1> wait_stages = {
      VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
  VkSubmitInfo submit_info = {};
  submit_info.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
  submit_info.waitSemaphoreCount = 1;
  submit_info.pWaitSemaphores = &acquire_semaphore_;
  submit_info.pWaitDstStageMask = wait_stages.data();
  submit_info.commandBufferCount = static_cast<uint32_t>(cmd_buffers.size());
  submit_info.pCommandBuffers = cmd_buffers.data();
  submit_info.signalSemaphoreCount = 1;
  submit_info.pSignalSemaphores = &render_semaphore_;

  CHECK_VK(vkQueueSubmit(core_.GetLogicalDevice().GetGCTQueue(), 1, &submit_info,
                         gct_cmd_buffer_fence_[image_index_]));

  // TODO(ale64bit): if gct and present queues are not the same, an additional
  // barrier is needed here.
  CHECK_PC(core_.GetLogicalDevice().IsSingleQueue(), "Additional barrier needed");

  // Present the swapchain image.
  VkSwapchainKHR swapchains[] = {core_.GetSwapchain().GetHandle()};
  VkPresentInfoKHR present_info = {};
  present_info.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
  present_info.waitSemaphoreCount = 1;
  present_info.pWaitSemaphores = &render_semaphore_;
  present_info.swapchainCount = 1;
  present_info.pSwapchains = swapchains;
  present_info.pImageIndices = &image_index_;
  present_info.pResults = nullptr;

  CHECK_VK(vkQueuePresentKHR(core_.GetLogicalDevice().GetPresentQueue(), &present_info));
|}

let wait_device_and_queues_idle =
  {|  const VkDevice device = core_.GetLogicalDevice().GetHandle();
  LOG(INFO) << name_ << ": waiting for gct queue to be idle\n";
  CHECK_VK(vkQueueWaitIdle(core_.GetLogicalDevice().GetGCTQueue()));
  LOG(INFO) << name_ << ": waiting for present queue to be idle\n";
  CHECK_VK(vkQueueWaitIdle(core_.GetLogicalDevice().GetPresentQueue()));
  LOG(INFO) << name_ << ": waiting for device to be idle\n";
  CHECK_VK(vkDeviceWaitIdle(device));
|}

let empty rname =
  {
    name = rname;
    render_targets = MapString.(empty |> add "builtin_screen" Color);
    rclass =
      Class.(
        empty rname |> add_include "<algorithm>" |> add_include "<string>"
        |> add_include "<unordered_set>"
        |> add_include "<unordered_map>"
        |> add_include "<vector>" |> add_include "<deque>"
        |> add_include "\"vulkan/vulkan.h\""
        |> add_include "\"core/Core.h\""
        |> add_include "\"core/Image.h\""
        |> add_include "\"core/Log.h\""
        |> add_include "\"core/StagingBuffer.h\""
        |> add_include "\"core/BufferPool.h\""
        |> add_include "\"core/LRU.h\""
        |> add_include "\"glm/glm.hpp\""
        |> add_include "\"glm/gtc/type_ptr.hpp\""
        |> add_include "\"Types.h\""
        |> add_private_member ("using UID = uint32_t", "")
        |> add_private_member ("const std::string ", "name_")
        |> add_private_member ("zrl::Core&", "core_")
        |> add_private_member ("uint32_t", "image_index_")
        (* Command Pools and Command Buffers *)
        |> add_private_member ("VkCommandPool", "gct_cmd_pool_")
        |> add_private_member ("VkCommandPool", "present_cmd_pool_")
        |> add_private_member ("std::vector<VkCommandBuffer>", "gc_cmd_buffer_")
        |> add_private_member ("std::vector<VkCommandBuffer>", "t_cmd_buffer_")
        |> add_private_member
             ("std::vector<VkCommandBuffer>", "present_cmd_buffer_")
        (* Sync Primitives *)
        |> add_private_member ("std::vector<VkFence>", "gct_cmd_buffer_fence_")
        |> add_private_member
             ("std::vector<VkFence>", "present_cmd_buffer_fence_")
        |> add_private_member ("VkSemaphore", "acquire_semaphore_")
        |> add_private_member ("VkSemaphore", "render_semaphore_")
        (* RenderPass/Framebuffer Caches *)
        |> add_private_member
             ( "std::unordered_map<std::vector<RenderTargetReference>, \
                VkRenderPass, RenderPassHash, RenderPassEqualTo>",
               "render_pass_cache_" )
        |> add_private_member
             ( "std::unordered_map<std::vector<RenderTargetReference>, \
                VkFramebuffer, FramebufferHash, FramebufferEqualTo>",
               "framebuffer_cache_" )
        (* Pipeline Layout LCP and Pipeline Cache *)
        |> add_private_member
             ( "std::unordered_map<std::pair<VkPipelineLayout, \
                VkPipelineLayout>, int>",
               "pipeline_layout_lcp_" )
        |> add_private_member
             ("std::unordered_map<size_t, VkPipeline>", "pipeline_cache_")
        (* Descriptor Pool and Descriptor Sets *)
        |> add_private_member ("VkDescriptorPool", "descriptor_pool_")
        |> add_private_member
             ( "std::array<DescriptorSetRegister, kDescriptorSetRegisterCount>",
               "descriptor_set_registers_" )
        |> add_private_member
             ( "std::vector<std::unordered_map<VkDescriptorSetLayout, \
                std::vector<VkDescriptorSet>>>",
               "discarded_descriptor_sets_" )
        |> add_private_member
             ( "std::unordered_map<VkDescriptorSetLayout, \
                std::vector<VkDescriptorSet>>",
               "recycled_descriptor_sets_" )
        (* Staging Buffer *)
        |> add_private_member
             ("std::unique_ptr<zrl::StagingBuffer>", "staging_buffer_")
        (* Vertex Buffer State *)
        |> add_private_member
             ("std::unique_ptr<zrl::BufferPool>", "vb_buffer_pool_")
        |> add_private_member
             ("std::unordered_map<UID, zrl::Block>", "vb_cache_")
        |> add_private_member ("zrl::LRU<UID>", "vb_lru_")
        |> add_private_member
             ("std::vector<VkBufferCopy>", "vb_pending_copies_")
        |> add_private_member
             ("std::vector<std::vector<zrl::Block>>", "vb_discarded_blocks_")
        |> add_private_member ("zrl::Block", "dummy_block_")
        (* Uniform Buffer State *)
        |> add_private_member
             ("std::unique_ptr<zrl::BufferPool>", "ubo_buffer_pool_")
        |> add_private_member
             ("std::unordered_map<UID, UniformResource>", "ubo_cache_")
        |> add_private_member ("zrl::LRU<UID>", "ubo_lru_")
        |> add_private_member
             ("std::vector<std::vector<zrl::Block>>", "ubo_discarded_blocks_")
        (* Sampled Images State *)
        |> add_private_member ("VkSampler", "dummy_sampler_")
        |> add_private_member
             ( "std::unordered_map<VkSamplerCreateInfo, VkSampler>",
               "sampler_cache_" )
        |> add_private_member
             ( "std::vector<std::vector<std::unique_ptr<zrl::Image>>>",
               "discarded_images_" )
        |> add_private_member
             ( "std::vector<VkImageMemoryBarrier>",
               "pending_pre_copy_image_barriers_" )
        |> add_private_member
             ( "std::vector<std::tuple<VkImage, VkBufferImageCopy, bool>>",
               "pending_image_copies_" )
        |> add_private_member
             ( "std::vector<VkImageMemoryBarrier>",
               "pending_post_copy_image_barriers_" )
        (* Per-Frame State *)
        |> add_private_member ("VkRenderPass", "current_render_pass_")
        |> add_private_member ("VkPipeline", "current_pipeline_")
        |> add_private_member ("VkPipelineLayout", "current_pipeline_layout_")
        |> add_private_member ("Builtin", "builtin")
        (* Render Target State *)
        |> add_private_member
             ("std::vector<VkImageView>", "rt_builtin_screen_img_view_")
        |> add_private_member
             ("std::vector<RenderTargetReference>", "rt_builtin_screen_ref_")
        |> add_private_function create_command_pool
        |> add_private_function create_command_buffers
        |> add_private_function create_events
        |> add_private_function create_fences
        |> add_private_function create_image_view
        |> add_private_function create_color_render_target
        |> add_private_function create_depth_render_target
        |> add_private_function bind_sampled_image2D
        |> add_private_function bind_cube_map
        |> add_private_function get_or_create_render_pass
        |> add_private_function get_or_create_framebuffer
        |> add_private_function begin_recording
        |> add_private_function clear_descriptor_set_register
        |> add_private_function recycle_or_allocate_descriptor_set);
    render =
      Function.(
        empty "Render" |> set_return_type "void"
        |> append_code_section
             {|if (BeginRecording()) { return; };
                 // Reclaim discarded blocks.
                 for (auto block: vb_discarded_blocks_[image_index_]) { 
                   vb_buffer_pool_->Free(block);
                 }
                 vb_discarded_blocks_[image_index_].clear();
                 for (auto block: ubo_discarded_blocks_[image_index_]) {
                   ubo_buffer_pool_->Free(block);
                 }
                 ubo_discarded_blocks_[image_index_].clear();
                 // Reclaim discarded descriptor sets.
                 for (auto p: discarded_descriptor_sets_[image_index_]) {
                   recycled_descriptor_sets_[p.first].insert(
                       recycled_descriptor_sets_[p.first].end(),
                       p.second.begin(), 
                       p.second.end());
                 }
                 discarded_descriptor_sets_[image_index_].clear();
                 // Reclaim discarded images.
                 discarded_images_[image_index_].clear();
                 // Clear descriptor set register bank.
                 for (int i = 0; i < kDescriptorSetRegisterCount; ++i) { 
                   ClearDescriptorSetRegister(i);
                 }
                 // Clear pending vertex buffer copies.
                 vb_pending_copies_.clear();
                 // Clear pending image barriers.
                 pending_pre_copy_image_barriers_.clear();
                 pending_image_copies_.clear();
                 pending_post_copy_image_barriers_.clear();
                 // Clear per-frame state.
                 current_render_pass_ = VK_NULL_HANDLE;
                 current_pipeline_ = VK_NULL_HANDLE;
                 current_pipeline_layout_ = VK_NULL_HANDLE;
                 // Reset render target state
                 builtin.screen = &rt_builtin_screen_ref_[image_index_];
                 builtin.screen->current_layout = VK_IMAGE_LAYOUT_UNDEFINED;
                 builtin.screen->load_op = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
             |});
    ctor =
      Function.(
        empty rname
        |> add_param ("zrl::Core&", "core")
        |> add_member_initializer ("name_", "\"" ^ rname ^ "\"")
        |> add_member_initializer ("core_", "core")
        |> add_member_initializer ("gct_cmd_pool_", "VK_NULL_HANDLE")
        |> add_member_initializer ("present_cmd_pool_", "VK_NULL_HANDLE")
        |> add_member_initializer ("acquire_semaphore_", "VK_NULL_HANDLE")
        |> add_member_initializer ("render_semaphore_", "VK_NULL_HANDLE")
        |> add_member_initializer ("descriptor_pool_", "VK_NULL_HANDLE")
        |> add_member_initializer ("dummy_sampler_", "VK_NULL_HANDLE")
        |> add_member_initializer ("current_render_pass_", "VK_NULL_HANDLE")
        |> add_member_initializer ("current_pipeline_", "VK_NULL_HANDLE")
        |> add_member_initializer ("current_pipeline_layout_", "VK_NULL_HANDLE")
        |> append_code_section ctor_body);
    dtor = Function.(empty ("~" ^ rname));
  }

let export t =
  let open Cpp in
  let dtor =
    Function.(
      t.dtor
      |> prepend_code_section wait_device_and_queues_idle
      |> append_code_section dtor_body)
  in
  let main =
    List.find
      (fun f -> Function.name f = "main")
      (Class.private_functions t.rclass)
  in
  let render =
    List.fold_left
      (flip Function.add_template_param)
      t.render
      (Function.template_params main)
  in
  let render =
    List.fold_left (flip Function.add_param) render (Function.params main)
  in
  let arg_names =
    String.concat ", "
      (List.map (fun (_, name) -> name) (Function.params main))
  in
  let render =
    Function.(
      render
      |> set_return_type (Function.return_type main)
      |> append_code_section (Printf.sprintf "  main(%s);" arg_names)
      |> append_code_section stop_recording_body)
  in
  Class.(
    t.rclass |> add_public_function t.ctor |> add_public_function dtor
    |> add_public_function render)
