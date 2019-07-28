open Cpp
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
      reference.layout = (rt->format == VK_FORMAT_B8G8R8A8_UNORM
                          ? VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                          : VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL);
    
      if (rt->format == VK_FORMAT_B8G8R8A8_UNORM) {
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
         {|  constexpr VkFormat format = VK_FORMAT_B8G8R8A8_UNORM;
  constexpr VkImageUsageFlags usage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | 
                                      VK_IMAGE_USAGE_SAMPLED_BIT;
  constexpr VkImageAspectFlags aspects = VK_IMAGE_ASPECT_COLOR_BIT;
  return std::make_unique<zrl::Image>(
      core_, core_.GetSwapchain().GetExtent(), 1, format,
      VK_IMAGE_TILING_OPTIMAL, usage,
      VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
      aspects);|})

let create_depth_render_target =
  Function.(
    empty "CreateDepthRenderTarget"
    |> set_return_type "std::unique_ptr<zrl::Image>"
    |> append_code_section
         {|  constexpr VkFormat format = VK_FORMAT_D32_SFLOAT;
  constexpr VkImageUsageFlags usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
  constexpr VkImageAspectFlags aspects = VK_IMAGE_ASPECT_DEPTH_BIT;
  return std::make_unique<zrl::Image>(
      core_, core_.GetSwapchain().GetExtent(), 1, format, 
      VK_IMAGE_TILING_OPTIMAL, 
      usage, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, aspects);|})

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
  staging_buffer_ = std::make_unique<zrl::StagingBuffer>(core_, zrl::_128MB);
  //======================================================================
  // Vertex Buffer State
  DLOG << name_ << ": creating vertex buffer pool\n";
  vb_buffer_pool_ = std::make_unique<zrl::BufferPool>(
      core, zrl::_128MB, 
      VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
      VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT |
      VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
      zrl::_1KB);
  vb_discarded_blocks_.resize(core_.GetSwapchain().GetImageCount());
  //======================================================================
  // Uniform Buffer State
  DLOG << name_ << ": creating uniform buffer pool\n";
  ubo_buffer_pool_ = std::make_unique<zrl::BufferPool>(
      core, zrl::_128MB, 
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
      VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
      256);
  ubo_discarded_blocks_.resize(core_.GetSwapchain().GetImageCount());
  //======================================================================
  // Render Target State
  DLOG << name_ << ": initializing render targets\n";
  rt_builtin_screen_img_view_.resize(core_.GetSwapchain().GetImageCount());
  for (size_t i = 0; i < rt_builtin_screen_img_view_.size(); ++i) {
    VkImageView view = CreateImageView(core_.GetSwapchain().GetImages()[i],
                                       core_.GetSwapchain().GetSurfaceFormat());
    rt_builtin_screen_img_view_[i] = view;
    rt_builtin_screen_ref_.push_back({VK_FORMAT_B8G8R8A8_UNORM, view, 
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
      VkImageView view =
          CreateImageView(core_.GetSwapchain().GetImages()[i],
                          core_.GetSwapchain().GetSurfaceFormat());
      rt_builtin_screen_img_view_[i] = view;
      rt_builtin_screen_ref_.push_back(
          {VK_FORMAT_B8G8R8A8_UNORM, view, VK_IMAGE_LAYOUT_UNDEFINED,
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
    r.uid = -1;
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

  // Schedule all pending vertex buffer copies.
  if (vb_pending_copies_.size() > 0) {
    vkCmdCopyBuffer(t_cmd_buffer_[image_index_], 
                    staging_buffer_->GetHandle(),
                    vb_buffer_pool_->GetHandle(), 
                    static_cast<uint32_t>(vb_pending_copies_.size()),
                    vb_pending_copies_.data());
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

let empty rname pkg =
  let pkg_path = String.concat "/" pkg in
  {
    name = rname;
    render_targets = MapString.(empty |> add "builtin_screen" Color);
    rclass =
      Class.(
        empty rname |> add_include "<algorithm>" |> add_include "<string>"
        |> add_include "<unordered_set>"
        |> add_include "<unordered_map>"
        |> add_include "<vector>"
        |> add_include "\"vulkan/vulkan.h\""
        |> add_include "\"core/Core.h\""
        |> add_include "\"core/Image.h\""
        |> add_include "\"core/Log.h\""
        |> add_include "\"core/StagingBuffer.h\""
        |> add_include "\"core/BufferPool.h\""
        |> add_include "\"core/LRU.h\""
        |> add_include "\"glm/glm.hpp\""
        |> add_include "\"glm/gtc/type_ptr.hpp\""
        |> add_include (Printf.sprintf {|"%s/Types.h"|} pkg_path)
        |> add_private_member ("using UID = int32_t", "")
        |> add_private_member
             ("using Block = std::pair<VkDeviceSize, VkDeviceSize>", "")
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
        |> add_private_member ("std::unordered_map<UID, Block>", "vb_cache_")
        |> add_private_member ("zrl::LRU<UID>", "vb_lru_")
        |> add_private_member
             ("std::vector<VkBufferCopy>", "vb_pending_copies_")
        |> add_private_member
             ("std::vector<std::vector<Block>>", "vb_discarded_blocks_")
        (* Uniform Buffer State *)
        |> add_private_member
             ("std::unique_ptr<zrl::BufferPool>", "ubo_buffer_pool_")
        |> add_private_member
             ("std::unordered_map<UID, UniformResource>", "ubo_cache_")
        |> add_private_member ("zrl::LRU<UID>", "ubo_lru_")
        |> add_private_member
             ("std::vector<std::vector<Block>>", "ubo_discarded_blocks_")
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
        |> add_private_function get_or_create_render_pass
        |> add_private_function get_or_create_framebuffer
        |> add_private_function begin_recording
        |> add_private_function clear_descriptor_set_register);
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
                 // Clear descriptor set register bank.
                 for (int i = 0; i < kDescriptorSetRegisterCount; ++i) { 
                   ClearDescriptorSetRegister(i);
                 }
                 // Clear pending vertex buffer copies.
                 vb_pending_copies_.clear();
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
        |> add_member_initializer ("current_render_pass_", "VK_NULL_HANDLE")
        |> add_member_initializer ("current_pipeline_", "VK_NULL_HANDLE")
        |> add_member_initializer ("current_pipeline_layout_", "VK_NULL_HANDLE")
        |> append_code_section ctor_body);
    dtor = Function.(empty ("~" ^ rname));
  }

let flip f x y = f y x

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
