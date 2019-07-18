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
  before_recording : string list;
  outside_render_passes : string list;
  inside_render_passes : string list;
  after_recording : string list;
}

let get_or_create_render_pass =
  Function.(
    empty "GetOrCreateRenderPass"
    |> set_return_type "VkRenderPass"
    |> add_param ("const RenderPassDescriptor&", "desc")
    |> append_code_section
         {|  auto res = render_pass_cache_.find(desc);
  if (res == render_pass_cache_.end()) {
    DLOG << name_ << ": creating new render pass" << '\n';
    std::vector<VkAttachmentDescription> attachment_descriptions;
    std::vector<VkAttachmentReference> attachment_references;
    std::vector<VkAttachmentReference> color_attachments;
    VkAttachmentReference depth_stencil_attachment = {};
    depth_stencil_attachment.attachment = VK_ATTACHMENT_UNUSED;
    
    uint32_t attachment_number = 0;
    for (const auto &p : desc.attachments) {
      VkAttachmentDescription description = {};
      description.format = std::get<0>(p);
      description.samples = VK_SAMPLE_COUNT_1_BIT;
      description.loadOp = std::get<1>(p);
      description.storeOp = VK_ATTACHMENT_STORE_OP_STORE;
      description.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
      description.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
      description.initialLayout = std::get<2>(p);
      description.finalLayout = std::get<3>(p);
    
      VkAttachmentReference reference = {};
      reference.attachment = attachment_number;
      reference.layout = (std::get<0>(p)== VK_FORMAT_B8G8R8A8_UNORM
                          ? VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                          : VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL);
    
      if (std::get<0>(p) == VK_FORMAT_B8G8R8A8_UNORM) {
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
    
    render_pass_cache_[desc] = rp;
    return rp;
  }
  return res->second;|})

let get_or_create_framebuffer =
  Function.(
    empty "GetOrCreateFramebuffer"
    |> set_return_type "VkFramebuffer"
    |> add_param ("const FramebufferDescriptor&", "desc")
    |> add_param ("VkRenderPass", "render_pass")
    |> append_code_section
         {|  auto res = framebuffer_cache_.find(desc);
  if (res == framebuffer_cache_.end()) {
    DLOG << name_ << ": creating new framebuffer" << '\n';
    VkFramebuffer fb = VK_NULL_HANDLE;
    VkFramebufferCreateInfo create_info = {};
    create_info.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
    create_info.renderPass = render_pass;
    create_info.attachmentCount = static_cast<uint32_t>(desc.attachments.size());
    create_info.pAttachments = desc.attachments.data();
    create_info.width = core_.GetSwapchain().GetExtent().width;
    create_info.height = core_.GetSwapchain().GetExtent().height;
    create_info.layers = 1;
    CHECK_VK(vkCreateFramebuffer(core_.GetLogicalDevice().GetHandle(),
                                 &create_info, nullptr, &fb));
    framebuffer_cache_[desc] = fb;
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
    |> add_param ("uint32_t", "count")
    |> append_code_section
         {|  const VkDevice device = core_.GetLogicalDevice().GetHandle();
  VkCommandBufferAllocateInfo alloc_info = {};
  alloc_info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  alloc_info.commandPool = pool;
  alloc_info.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
  alloc_info.commandBufferCount = count;
  std::vector<VkCommandBuffer> cmd_buffers(count);
  CHECK_VK(vkAllocateCommandBuffers(device, &alloc_info, cmd_buffers.data()));
  return cmd_buffers;|})

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
      core_.GetLogicalDevice().GetHandle(),
      core_.GetSwapchain().GetExtent(), 1, format,
      VK_IMAGE_TILING_OPTIMAL, usage,
      VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
      aspects,
      core_.GetLogicalDevice().GetPhysicalDevice().GetMemoryProperties());|})

let create_depth_render_target =
  Function.(
    empty "CreateDepthRenderTarget"
    |> set_return_type "std::unique_ptr<zrl::Image>"
    |> append_code_section
         {|  constexpr VkFormat format = VK_FORMAT_D32_SFLOAT;
  constexpr VkImageUsageFlags usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
  constexpr VkImageAspectFlags aspects = VK_IMAGE_ASPECT_DEPTH_BIT;
  return std::make_unique<zrl::Image>(
      core_.GetLogicalDevice().GetHandle(), 
      core_.GetSwapchain().GetExtent(), 1, format, 
      VK_IMAGE_TILING_OPTIMAL, 
      usage, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, aspects,
      core_.GetLogicalDevice().GetPhysicalDevice().GetMemoryProperties());|})

let ctor_body =
  {|  const auto &device = core_.GetLogicalDevice();
  DLOG << name_ << ": creating gct command pool\n";
  gct_cmd_pool_ = CreateCommandPool(device.GetGCTQueueFamily());
  gct_cmd_buffer_ = CreateCommandBuffers(gct_cmd_pool_,
                                         core_.GetSwapchain().GetImageCount());
  gct_cmd_buffer_fence_ = CreateFences(core_.GetSwapchain().GetImageCount());
  if (device.IsSingleQueue()) {
    DLOG << name_ << ": reusing gct command pool as present command pool\n";
    present_cmd_pool_ = gct_cmd_pool_;
    present_cmd_buffer_ = gct_cmd_buffer_;
    present_cmd_buffer_fence_ = gct_cmd_buffer_fence_;
  } else {
    DLOG << name_ << ": creating present command pool\n";
    present_cmd_pool_ = CreateCommandPool(device.GetPresentQueueFamily());
    present_cmd_buffer_ = CreateCommandBuffers(present_cmd_pool_,
                                               core_.GetSwapchain().GetImageCount());
    present_cmd_buffer_fence_ = CreateFences(core_.GetSwapchain().GetImageCount());
  }

  VkSemaphoreCreateInfo semaphore_create_info = {};
  semaphore_create_info.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
  CHECK_VK(vkCreateSemaphore(device.GetHandle(), &semaphore_create_info, nullptr, 
                             &acquire_semaphore_));
  CHECK_VK(vkCreateSemaphore(device.GetHandle(), &semaphore_create_info, nullptr, 
                             &render_semaphore_));

  rt_builtin_screen_.resize(core_.GetSwapchain().GetImageCount());
  for (size_t i = 0; i < rt_builtin_screen_.size(); ++i) {
    rt_builtin_screen_[i] = CreateImageView(core_.GetSwapchain().GetImages()[i],
                                            core_.GetSwapchain().GetSurfaceFormat());
  }
|}

let dtor_body =
  {|  for (auto image_view : rt_builtin_screen_) {
    vkDestroyImageView(device, image_view, nullptr);
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
    empty "BeginRecording" |> set_return_type "uint32_t"
    |> append_code_section
         {|  const VkDevice device = core_.GetLogicalDevice().GetHandle();
  // Acquire swapchain image.
  uint32_t image_index = 0;
  CHECK_VK(vkAcquireNextImageKHR(device, core_.GetSwapchain().GetHandle(),
                                 std::numeric_limits<uint64_t>::max(),
                                 acquire_semaphore_, VK_NULL_HANDLE, &image_index));
  // Wait for cmd buffer to be ready.
  CHECK_VK(vkWaitForFences(device, 1, &gct_cmd_buffer_fence_[image_index], VK_TRUE,
                     std::numeric_limits<uint64_t>::max()));
  CHECK_VK(vkResetFences(device, 1, &gct_cmd_buffer_fence_[image_index]));

  VkCommandBufferBeginInfo cmd_buffer_begin_info = {};
  cmd_buffer_begin_info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
  cmd_buffer_begin_info.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
  cmd_buffer_begin_info.pInheritanceInfo = nullptr;
  CHECK_VK(vkBeginCommandBuffer(gct_cmd_buffer_[image_index],
                                &cmd_buffer_begin_info));
  return image_index;
|})

let stop_recording_body =
  {|  if (current_render_pass != VK_NULL_HANDLE) {
    vkCmdEndRenderPass(gct_cmd_buffer_[image_index]);
  }

  // Extra render pass to flush pending screen clears and transition
  // swapchain image view into PRESENT layout.
  {
    RenderPassDescriptor rp_desc{{std::make_tuple(
        rt_builtin_screen_format, rt_builtin_screen_load_op, 
        rt_builtin_screen_layout, VK_IMAGE_LAYOUT_PRESENT_SRC_KHR)}};
    FramebufferDescriptor fb_desc{{rt_builtin_screen_[image_index]}};
    VkRenderPass render_pass = GetOrCreateRenderPass(rp_desc);
    VkFramebuffer framebuffer = GetOrCreateFramebuffer(fb_desc, render_pass);
    VkClearValue clear_value = {};
    if (rt_builtin_screen_load_op == VK_ATTACHMENT_LOAD_OP_CLEAR) {
      std::memcpy(clear_value.color.float32,
                  glm::value_ptr(rt_builtin_screen_clear_value),
                  sizeof clear_value.color);
      rt_builtin_screen_load_op = VK_ATTACHMENT_LOAD_OP_LOAD;
    }
    VkRenderPassBeginInfo begin_info = {};
    begin_info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
    begin_info.pNext = nullptr;
    begin_info.renderPass = render_pass;
    begin_info.framebuffer = framebuffer;
    begin_info.renderArea.offset = {0, 0};
    begin_info.renderArea.extent = core_.GetSwapchain().GetExtent();
    begin_info.clearValueCount = 1;
    begin_info.pClearValues = &clear_value;
    vkCmdBeginRenderPass(gct_cmd_buffer_[image_index], &begin_info,
                         VK_SUBPASS_CONTENTS_INLINE);
    vkCmdEndRenderPass(gct_cmd_buffer_[image_index]);
  }
  
  // Stop recording.
  CHECK_VK(vkEndCommandBuffer(gct_cmd_buffer_[image_index]));

  // Submit render command buffer.
  constexpr std::array<VkPipelineStageFlags, 1> wait_stages = {
      VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
  VkSubmitInfo submit_info = {};
  submit_info.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
  submit_info.waitSemaphoreCount = 1;
  submit_info.pWaitSemaphores = &acquire_semaphore_;
  submit_info.pWaitDstStageMask = wait_stages.data();
  submit_info.commandBufferCount = 1;
  submit_info.pCommandBuffers = &gct_cmd_buffer_[image_index];
  submit_info.signalSemaphoreCount = 1;
  submit_info.pSignalSemaphores = &render_semaphore_;

  CHECK_VK(vkQueueSubmit(core_.GetLogicalDevice().GetGCTQueue(), 1, &submit_info,
                         gct_cmd_buffer_fence_[image_index]));

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
  present_info.pImageIndices = &image_index;
  present_info.pResults = nullptr;

  CHECK_VK(vkQueuePresentKHR(core_.GetLogicalDevice().GetPresentQueue(), &present_info));
|}

let wait_device_and_queues_idle =
  {|  const VkDevice device = core_.GetLogicalDevice().GetHandle();
  LOG(INFO) << name_ << ": waiting for gct queue to be idle\n";
  vkQueueWaitIdle(core_.GetLogicalDevice().GetGCTQueue());
  LOG(INFO) << name_ << ": waiting for present queue to be idle\n";
  vkQueueWaitIdle(core_.GetLogicalDevice().GetPresentQueue());
  LOG(INFO) << name_ << ": waiting for device to be idle\n";
  vkDeviceWaitIdle(device);
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
        |> add_include "\"glm/glm.hpp\""
        |> add_include "\"glm/gtc/type_ptr.hpp\""
        |> add_include (Printf.sprintf {|"%s/Types.h"|} pkg_path)
        |> add_private_member ("const std::string ", "name_")
        |> add_private_member ("zrl::Core&", "core_")
        |> add_private_member ("VkCommandPool", "gct_cmd_pool_")
        |> add_private_member ("VkCommandPool", "present_cmd_pool_")
        |> add_private_member
             ("std::vector<VkCommandBuffer>", "gct_cmd_buffer_")
        |> add_private_member
             ("std::vector<VkCommandBuffer>", "present_cmd_buffer_")
        |> add_private_member ("std::vector<VkFence>", "gct_cmd_buffer_fence_")
        |> add_private_member
             ("std::vector<VkFence>", "present_cmd_buffer_fence_")
        |> add_private_member ("VkSemaphore", "acquire_semaphore_")
        |> add_private_member ("VkSemaphore", "render_semaphore_")
        |> add_private_member
             ( "std::unordered_map<RenderPassDescriptor, VkRenderPass>",
               "render_pass_cache_" )
        |> add_private_member
             ( "std::unordered_map<FramebufferDescriptor, VkFramebuffer>",
               "framebuffer_cache_" )
        |> add_private_member
             ( "std::unordered_map<VkGraphicsPipelineCreateInfo, VkPipeline>",
               "pipeline_cache_" )
        |> add_private_member ("std::vector<VkImageView>", "rt_builtin_screen_")
        |> add_private_member
             ("VkAttachmentLoadOp", "rt_builtin_screen_load_op")
        |> add_private_member ("glm::fvec4", "rt_builtin_screen_clear_value")
        |> add_private_function create_command_pool
        |> add_private_function create_command_buffers
        |> add_private_function create_fences
        |> add_private_function create_image_view
        |> add_private_function create_color_render_target
        |> add_private_function create_depth_render_target
        |> add_private_function get_or_create_render_pass
        |> add_private_function get_or_create_framebuffer
        |> add_private_function begin_recording);
    render = Function.(empty "Render" |> set_return_type "void");
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
        |> add_member_initializer
             ("rt_builtin_screen_load_op", "VK_ATTACHMENT_LOAD_OP_DONT_CARE")
        |> append_code_section ctor_body);
    dtor = Function.(empty ("~" ^ rname));
    before_recording =
      List.rev
        [ "VkRenderPass current_render_pass = VK_NULL_HANDLE;";
          "VkPipeline current_pipeline = VK_NULL_HANDLE;";
          "constexpr VkFormat rt_builtin_screen_format = \
           VK_FORMAT_B8G8R8A8_UNORM;";
          "VkImageLayout rt_builtin_screen_layout = VK_IMAGE_LAYOUT_UNDEFINED;"
        ];
    outside_render_passes = [];
    inside_render_passes = [];
    after_recording = [];
  }

let add_stmt_before_recording stmt t =
  { t with before_recording = stmt :: t.before_recording }

let add_stmt_outside_render_passes stmt t =
  { t with outside_render_passes = stmt :: t.outside_render_passes }

let add_stmt_inside_render_passes stmt t =
  { t with inside_render_passes = stmt :: t.inside_render_passes }

let add_stmt_after_recording stmt t =
  { t with after_recording = stmt :: t.after_recording }

let export t =
  let dtor =
    Function.(
      t.dtor
      |> prepend_code_section wait_device_and_queues_idle
      |> append_code_section dtor_body)
  in
  let render =
    Function.(
      t.render
      |> append_code_section "// ====== Before recording ======"
      |> append_code_sections (List.rev t.before_recording)
      |> append_code_section "const uint32_t image_index = BeginRecording();"
      |> append_code_section "// ====== Outside render passes ======"
      |> append_code_sections (List.rev t.outside_render_passes)
      |> append_code_section "// ====== Inside render passes ======"
      |> append_code_sections (List.rev t.inside_render_passes)
      |> append_code_section stop_recording_body
      |> append_code_section "// ====== After recording ======"
      |> append_code_sections (List.rev t.after_recording))
  in
  Class.(
    t.rclass |> add_public_function t.ctor |> add_public_function dtor
    |> add_public_function render)
