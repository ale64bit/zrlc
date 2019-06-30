open Zrl
open Monad.Result
open Cpp
module L = Located
module MapString = Map.Make (String)

module Error = struct
  type t = [`Unsupported of string | `MissingRendererEntryPoint of string]

  let string_of_error L.{loc; value} =
    let pos = L.string_of_start_position loc in
    let prefix = Printf.sprintf "%s: vkdummy error" pos in
    match value with
    | `Unsupported msg ->
        Printf.sprintf "%s: unsupported: %s" prefix msg
    | `MissingRendererEntryPoint r ->
        Printf.sprintf "%s: renderer %s is missing the entry point" prefix r
end

type render_target_type = Color | DepthStencil

module RenderFunction = struct
  type t =
    { render_targets: render_target_type MapString.t
    ; before_recording: string list
    ; outside_render_pass: string list
    ; inside_render_pass: string list
    ; after_recording: string list }

  let empty rt =
    { render_targets= rt
    ; before_recording= []
    ; outside_render_pass= []
    ; inside_render_pass= []
    ; after_recording= [] }

  let add_before_recording op t =
    {t with before_recording= op :: t.before_recording}

  (*
  let add_outside_render_pass op t =
    {t with outside_render_pass= op :: t.outside_render_pass}

  let add_inside_render_pass op t =
    {t with inside_render_pass= op :: t.inside_render_pass}
*)

  let add_after_recording op t =
    {t with after_recording= op :: t.after_recording}

  let all_sections t =
    ["// Before recording command buffer"]
    @ List.rev t.before_recording
    @ ["// Outside render pass"]
    @ List.rev t.outside_render_pass
    @ ["// Inside render pass"]
    @ List.rev t.inside_render_pass
    @ ["// After recording"] @ List.rev t.after_recording
end

type renderer =
  { r_class: Class.t
  ; r_ctor: Function.t
  ; r_dtor: Function.t
  ; r_render_function: Function.t
  ; r_render_targets: render_target_type MapString.t }

(*

type graphics_pipeline =
  { gp_name: string
  ; gp_declaration: TypedAst.pipeline_declaration
  ; gp_uniforms: (string * Type.t) list
  ; gp_inputs: (string * Type.t) list
  ; gp_outputs: Type.t list
  ; gp_vertex_stage: TypedAst.function_declaration
  ; gp_geometry_stage: TypedAst.function_declaration option
  ; gp_fragment_stage: TypedAst.function_declaration option
  ; gp_helper_functions: TypedAst.function_declaration list }

*)

let error loc e = Error L.{loc; value= e}

let write_file fname contents =
  let out = open_out fname in
  let () = Printf.fprintf out "%s" contents in
  close_out out

let build libraries =
  let tmpl =
    Mustache.of_string
      {|load("//core:builddefs.bzl", "COPTS", "DEFINES")

package(default_visibility = ["//visibility:public"])

{{#libraries}}
{{& library}}
{{/libraries}}
|}
  in
  let o =
    `O
      [ ( "libraries"
        , `A (List.map (fun l -> `O [("library", `String l)]) libraries) ) ]
  in
  Mustache.render tmpl o

let render_pass_descriptor_struct =
  {|struct RenderPassDescriptor {
  std::vector<std::tuple<VkFormat, VkAttachmentLoadOp, VkImageLayout, VkImageLayout>> attachments;

  bool operator==(const RenderPassDescriptor &that) const {
    return attachments == that.attachments;
  }
}|}

let render_pass_descriptor_hash =
  {|struct RenderPassDescriptorHash { 
  size_t operator()(const RenderPassDescriptor &desc) const noexcept {
    size_t h = 0;
    for (const auto &t : desc.attachments) {
      h = h*31 + std::get<0>(t);
      h = h*31 + std::get<1>(t);
      h = h*31 + std::get<2>(t);
      h = h*31 + std::get<3>(t);
    }
    return h;
  }
}|}

let framebuffer_descriptor_struct =
  {|struct FramebufferDescriptor {
  std::vector<VkImageView> attachments;

  bool operator==(const FramebufferDescriptor &that) const {
    return attachments == that.attachments;
  }
}|}

let framebuffer_descriptor_hash =
  {|struct FramebufferDescriptorHash {
  size_t operator()(const FramebufferDescriptor &desc) const noexcept {
    size_t h = 0;
    for (const auto &a : desc.attachments) {
      h = h*31 + reinterpret_cast<size_t>(a);
    }
    return h;
  }
}|}

let create_command_pool =
  {|static VkCommandPool CreateCommandPool(VkDevice device, uint32_t family) {
  VkCommandPool cmd_pool = VK_NULL_HANDLE;
  VkCommandPoolCreateInfo create_info = {};
  create_info.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
  create_info.queueFamilyIndex = family;
  create_info.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
  CHECK_VK(vkCreateCommandPool(device, &create_info, nullptr, &cmd_pool));
  return cmd_pool;
}|}

let create_command_buffers =
  {|static std::vector<VkCommandBuffer> CreateCommandBuffers(VkDevice device, 
    VkCommandPool pool, uint32_t count) {
  VkCommandBufferAllocateInfo alloc_info = {};
  alloc_info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  alloc_info.commandPool = pool;
  alloc_info.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
  alloc_info.commandBufferCount = count;

  std::vector<VkCommandBuffer> cmd_buffers(count);
  CHECK_VK(vkAllocateCommandBuffers(device, &alloc_info,
                                    cmd_buffers.data()));
  return cmd_buffers;
}|}

let create_command_buffer_fences =
  {|static std::vector<VkFence> CreateCommandBufferFences(VkDevice device, 
    uint32_t count) {
  VkFenceCreateInfo fence_create_info = {};
  fence_create_info.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
  fence_create_info.flags = VK_FENCE_CREATE_SIGNALED_BIT;
  std::vector<VkFence> fences(count);
  for (size_t i = 0; i < fences.size(); ++i) {
    CHECK_VK(vkCreateFence(device, &fence_create_info, nullptr, &fences[i]));
  }
  return fences;
}|}

let create_image_view =
  {|static VkImageView CreateImageView(VkDevice device, VkImage image, VkFormat format) {
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
  return image_view;
}|}

let create_color_render_target =
  {|static std::unique_ptr<zrl::Image> CreateColorRenderTarget(const zrl::Core &core) {
  const VkFormat format = VK_FORMAT_B8G8R8A8_UNORM;
  const VkImageUsageFlags usage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
  const VkImageAspectFlags aspects = VK_IMAGE_ASPECT_COLOR_BIT;
  return std::make_unique<zrl::Image>(core.GetLogicalDevice().GetHandle(), 
                                      core.GetSwapchain().GetExtent(), 1, format, 
                                      VK_IMAGE_TILING_OPTIMAL, 
                                      usage, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, aspects,
                                      core.GetLogicalDevice().GetPhysicalDevice().GetMemoryProperties());
}|}

let create_depth_render_target =
  {|static std::unique_ptr<zrl::Image> CreateDepthRenderTarget(const zrl::Core &core) {
  const VkFormat format = VK_FORMAT_D32_SFLOAT;
  const VkImageUsageFlags usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
  const VkImageAspectFlags aspects = VK_IMAGE_ASPECT_DEPTH_BIT;
  return std::make_unique<zrl::Image>(core.GetLogicalDevice().GetHandle(), 
                                      core.GetSwapchain().GetExtent(), 1, format, 
                                      VK_IMAGE_TILING_OPTIMAL, 
                                      usage, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, aspects,
                                      core.GetLogicalDevice().GetPhysicalDevice().GetMemoryProperties());
}|}

let get_or_create_render_pass =
  Function.(
    empty "GetOrCreateRenderPass"
    |> set_return_type "VkRenderPass"
    |> add_param ("const RenderPassDescriptor&", "desc")
    |> append_code_sections
         [ "auto res = render_pass_cache_.find(desc);"
         ; "if (res == render_pass_cache_.end()) {"
         ; "  DLOG << \"creating new render pass\" << '\\n';"
         ; "  std::vector<VkAttachmentDescription> attachment_descriptions;"
         ; "  std::vector<VkAttachmentReference> attachment_references;"
         ; "  std::vector<VkAttachmentReference> color_attachments;"
         ; "  VkAttachmentReference depth_stencil_attachment = {};"
         ; "  depth_stencil_attachment.attachment = VK_ATTACHMENT_UNUSED;"
         ; ""
         ; "  uint32_t attachment_number = 0;"
         ; "  for (const auto &p : desc.attachments) {"
         ; "    VkAttachmentDescription description = {};"
         ; "    description.format = std::get<0>(p);"
         ; "    description.samples = VK_SAMPLE_COUNT_1_BIT;"
         ; "    description.loadOp = std::get<1>(p);"
         ; "    description.storeOp = VK_ATTACHMENT_STORE_OP_STORE;"
         ; "    description.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;"
         ; "    description.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;"
         ; "    description.initialLayout = std::get<2>(p);"
         ; "    description.finalLayout = std::get<3>(p);"
         ; ""
         ; "    VkAttachmentReference reference = {};"
         ; "    reference.attachment = attachment_number;"
         ; "    reference.layout = (std::get<0>(p)== VK_FORMAT_B8G8R8A8_UNORM"
         ; "                        ? VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
         ; "                        : \
            VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL);"
         ; ""
         ; "    if (std::get<0>(p) == VK_FORMAT_B8G8R8A8_UNORM) {"
         ; "      color_attachments.push_back(reference);"
         ; "    } else {"
         ; "      depth_stencil_attachment = reference;"
         ; "    }"
         ; "    attachment_descriptions.push_back(description);"
         ; "    attachment_references.push_back(reference);"
         ; "    ++attachment_number;"
         ; "  }"
         ; ""
         ; "  VkSubpassDescription subpass = {};"
         ; "  subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;"
         ; "  subpass.colorAttachmentCount = \
            static_cast<uint32_t>(color_attachments.size());"
         ; "  subpass.pColorAttachments = color_attachments.data();"
         ; "  subpass.pDepthStencilAttachment = &depth_stencil_attachment;"
         ; "  std::array<VkSubpassDescription, 1> subpass_descriptions = \
            {{subpass}};"
         ; ""
         ; "  VkSubpassDependency final_to_initial = {};"
         ; "  final_to_initial.srcSubpass = VK_SUBPASS_EXTERNAL;"
         ; "  final_to_initial.dstSubpass = 0;"
         ; "  final_to_initial.srcStageMask = \
            VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT;"
         ; "  final_to_initial.dstStageMask = \
            VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;"
         ; "  final_to_initial.srcAccessMask = VK_ACCESS_MEMORY_READ_BIT;"
         ; "  final_to_initial.dstAccessMask = \
            VK_ACCESS_COLOR_ATTACHMENT_READ_BIT |"
         ; "                                   \
            VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;"
         ; "  final_to_initial.dependencyFlags = VK_DEPENDENCY_BY_REGION_BIT;"
         ; ""
         ; "  VkSubpassDependency initial_to_final = {};"
         ; "  initial_to_final.srcSubpass = 0;"
         ; "  initial_to_final.dstSubpass = VK_SUBPASS_EXTERNAL;"
         ; "  initial_to_final.srcStageMask = \
            VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;"
         ; "  initial_to_final.dstStageMask = \
            VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT;"
         ; "  initial_to_final.srcAccessMask = \
            VK_ACCESS_COLOR_ATTACHMENT_READ_BIT |"
         ; "                                   \
            VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;"
         ; "  initial_to_final.dstAccessMask = VK_ACCESS_MEMORY_READ_BIT;"
         ; "  initial_to_final.dependencyFlags = VK_DEPENDENCY_BY_REGION_BIT;"
         ; ""
         ; "  std::array<VkSubpassDependency, 2> dependencies = {{"
         ; "      final_to_initial,"
         ; "      initial_to_final,"
         ; "  }};"
         ; ""
         ; "  VkRenderPass rp = VK_NULL_HANDLE;"
         ; "  VkRenderPassCreateInfo create_info = {};"
         ; "  create_info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;"
         ; "  create_info.pNext = nullptr;"
         ; "  create_info.flags = 0;"
         ; "  create_info.attachmentCount = \
            static_cast<uint32_t>(attachment_descriptions.size());"
         ; "  create_info.pAttachments = attachment_descriptions.data();"
         ; "  create_info.subpassCount = \
            static_cast<uint32_t>(subpass_descriptions.size());"
         ; "  create_info.pSubpasses = subpass_descriptions.data();"
         ; "  create_info.dependencyCount = \
            static_cast<uint32_t>(dependencies.size());"
         ; "  create_info.pDependencies = dependencies.data();"
         ; "  CHECK_VK(vkCreateRenderPass(core_.GetLogicalDevice().GetHandle(),"
         ; "                              &create_info, nullptr, &rp));"
         ; ""
         ; "  render_pass_cache_[desc] = rp;"
         ; "  return rp;"
         ; "}"
         ; "return res->second;" ])

let get_or_create_framebuffer =
  Function.(
    empty "GetOrCreateFramebuffer"
    |> set_return_type "VkFramebuffer"
    |> add_param ("const FramebufferDescriptor&", "desc")
    |> add_param ("VkRenderPass", "render_pass")
    |> append_code_sections
         [ "  auto res = framebuffer_cache_.find(desc);"
         ; "  if (res == framebuffer_cache_.end()) {"
         ; "    DLOG << \"creating new framebuffer\" << '\\n';"
         ; "    VkFramebuffer fb = VK_NULL_HANDLE;"
         ; "    VkFramebufferCreateInfo create_info = {};"
         ; "    create_info.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;"
         ; "    create_info.renderPass = render_pass;"
         ; "    create_info.attachmentCount ="
         ; "        static_cast<uint32_t>(desc.attachments.size());"
         ; "    create_info.pAttachments = desc.attachments.data();"
         ; "    create_info.width = core_.GetSwapchain().GetExtent().width;"
         ; "    create_info.height = core_.GetSwapchain().GetExtent().height;"
         ; "    create_info.layers = 1;"
         ; "    \
            CHECK_VK(vkCreateFramebuffer(core_.GetLogicalDevice().GetHandle(),"
         ; "                                 &create_info, nullptr, &fb));"
         ; "    framebuffer_cache_[desc] = fb;"
         ; "    return fb;"
         ; "  }"
         ; "  return res->second;" ])

let ctor_body name =
  let tmpl =
    Mustache.of_string
      {|  const auto &device = core_.GetLogicalDevice();
  DLOG << "{{name}}: creating gct command pool\n";
  gct_cmd_pool_ = CreateCommandPool(device.GetHandle(), 
                                    device.GetGCTQueueFamily());
  gct_cmd_buffer_ = CreateCommandBuffers(device.GetHandle(),
                                         gct_cmd_pool_,
                                         core.GetSwapchain().GetImageCount());
  gct_cmd_buffer_fence_ = CreateCommandBufferFences(device.GetHandle(),
                                                    core.GetSwapchain().GetImageCount());
  if (device.IsSingleQueue()) {
    DLOG << "{{name}}: reusing gct command pool as present command pool\n";
    present_cmd_pool_ = gct_cmd_pool_;
    present_cmd_buffer_ = gct_cmd_buffer_;
    present_cmd_buffer_fence_ = gct_cmd_buffer_fence_;
  } else {
    DLOG << "{{name}}: creating present command pool\n";
    present_cmd_pool_ = CreateCommandPool(device.GetHandle(), 
                                          device.GetPresentQueueFamily());
    present_cmd_buffer_ = CreateCommandBuffers(device.GetHandle(),
                                               present_cmd_pool_,
                                               core.GetSwapchain().GetImageCount());
    present_cmd_buffer_fence_ = CreateCommandBufferFences(device.GetHandle(),
                                                          core.GetSwapchain().GetImageCount());
  }

  VkSemaphoreCreateInfo semaphore_create_info = {};
  semaphore_create_info.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
  CHECK_VK(vkCreateSemaphore(device.GetHandle(), &semaphore_create_info, nullptr, 
                             &acquire_semaphore_));
  CHECK_VK(vkCreateSemaphore(device.GetHandle(), &semaphore_create_info, nullptr, 
                             &render_semaphore_));
|}
  in
  let o = `O [("name", `String name)] in
  Mustache.render tmpl o

let wait_device_and_queues_idle =
  {|  const VkDevice device = core_.GetLogicalDevice().GetHandle();
  LOG(INFO) << "waiting for gct queue to be idle\n";
  vkQueueWaitIdle(core_.GetLogicalDevice().GetGCTQueue());
  LOG(INFO) << "waiting for present queue to be idle\n";
  vkQueueWaitIdle(core_.GetLogicalDevice().GetPresentQueue());
  LOG(INFO) << "waiting for device to be idle\n";
  vkDeviceWaitIdle(device);
|}

let dtor_body =
  {|  DLOG << "total framebuffers: " << framebuffer_cache_.size() << '\n';
  for (auto p : framebuffer_cache_) {
    vkDestroyFramebuffer(device, p.second, nullptr);
  }
  DLOG << "total render passes: " << render_pass_cache_.size() << '\n';
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

let find_func name funcs err =
  match List.find_opt (fun TypedAst.{fd_name; _} -> fd_name = name) funcs with
  | Some f ->
      Ok f
  | None ->
      err

let rec zrl_to_cpp_type = function
  | Type.TypeRef "int" ->
      "int"
  | Type.TypeRef "float" ->
      "float"
  | Type.TypeRef "fvec2" ->
      "glm::fvec2"
  | Type.TypeRef "fvec3" ->
      "glm::fvec4"
  | Type.TypeRef "fvec4" ->
      "glm::fvec4"
  | Type.TypeRef "ivec2" ->
      "glm::ivec2"
  | Type.TypeRef "ivec3" ->
      "glm::ivec4"
  | Type.TypeRef "ivec4" ->
      "glm::ivec4"
  | Type.TypeRef "uvec2" ->
      "glm::uvec2"
  | Type.TypeRef "uvec3" ->
      "glm::uvec4"
  | Type.TypeRef "uvec4" ->
      "glm::uvec4"
  | Type.TypeRef "fmat2" ->
      "glm::mat2"
  | Type.TypeRef "fmat3" ->
      "glm::mat3"
  | Type.TypeRef "fmat4" ->
      "glm::mat4"
  | Type.Array (t, dims) ->
      let arr t = function
        | Type.OfInt i ->
            Printf.sprintf "std::array<%s, %d>" t i
        | _ ->
            failwith "unsupported array dimension"
      in
      let rec aux = function
        | [] ->
            ""
        | [d] ->
            arr (zrl_to_cpp_type t) d
        | d :: ds ->
            arr (aux ds) d
      in
      aux dims
  | t ->
      failwith "unsupported type: " ^ Type.string_of_type t

let gen_renderer_signature loc rd_name rd_functions r =
  let err = error loc (`MissingRendererEntryPoint rd_name) in
  find_func "main" rd_functions err >>= fun TypedAst.{fd_type; _} ->
  match fd_type with
  | Type.Function (params, _) ->
      List.fold_left
        (fun acc (name, t) ->
          acc >>= fun r ->
          match t with
          | Type.TypeRef (("int" | "float" | "bool") as tname) ->
              Ok
                { r with
                  r_render_function=
                    Function.(r.r_render_function |> add_param (tname, name))
                }
          (* TODO: support vector/matrix types via GLM *)
          | Type.TypeRef "atom" ->
              let tmpl_param = name ^ "AtomType" in
              let param = (Printf.sprintf "const %s&" tmpl_param, name) in
              Ok
                { r with
                  r_render_function=
                    Function.(
                      r.r_render_function
                      |> add_template_param ("class " ^ tmpl_param)
                      |> add_param param) }
          | Type.TypeRef "atomset" ->
              let tmpl_param = name ^ "AtomType" in
              let param =
                ( Printf.sprintf "const std::unordered_set<%s>&" tmpl_param
                , name )
              in
              Ok
                { r with
                  r_render_function=
                    Function.(
                      r.r_render_function
                      |> add_template_param ("class " ^ tmpl_param)
                      |> add_param param) }
          | Type.TypeRef "atomlist" ->
              let tmpl_param = name ^ "AtomType" in
              let param =
                (Printf.sprintf "const std::vector<%s>&" tmpl_param, name)
              in
              Ok
                { r with
                  r_render_function=
                    Function.(
                      r.r_render_function
                      |> add_template_param ("class " ^ tmpl_param)
                      |> add_param param) }
          | _ ->
              error loc
                (`Unsupported
                  (Printf.sprintf "cannot use type %s as renderer argument"
                     (Type.string_of_type t))) )
        (Ok r) params
  | _ ->
      failwith "renderer entry point must be of Function type"

let gen_cpp_builtin_call_id = function
  | "ivec2" ->
      "glm::ivec2"
  | "ivec3" ->
      "glm::ivec3"
  | "ivec4" ->
      "glm::ivec4"
  | "uvec2" ->
      "glm::uvec2"
  | "uvec3" ->
      "glm::uvec3"
  | "uvec4" ->
      "glm::uvec4"
  | "fvec2" ->
      "glm::fvec2"
  | "fvec3" ->
      "glm::fvec3"
  | "fvec4" ->
      "glm::fvec4"
  | "dvec2" ->
      "glm::dvec2"
  | "dvec3" ->
      "glm::dvec3"
  | "dvec4" ->
      "glm::dvec4"
  | "bvec2" ->
      "glm::bvec2"
  | "bvec3" ->
      "glm::bvec3"
  | "bvec4" ->
      "glm::bvec4"
  | "imat2" ->
      "glm::imat2"
  | "imat3" ->
      "glm::imat3"
  | "imat4" ->
      "glm::imat4"
  | "umat2" ->
      "glm::umat2"
  | "umat3" ->
      "glm::umat3"
  | "umat4" ->
      "glm::umat4"
  | "fmat2" ->
      "glm::fmat2"
  | "fmat3" ->
      "glm::fmat3"
  | "fmat4" ->
      "glm::fmat4"
  | "dmat2" ->
      "glm::dmat2"
  | "dmat3" ->
      "glm::dmat3"
  | "dmat4" ->
      "glm::dmat4"
  | "bmat2" ->
      "glm::bmat2"
  | "bmat3" ->
      "glm::bmat3"
  | "bmat4" ->
      "glm::bmat4"
  | other ->
      other

let rec gen_cpp_expression L.{value; _} =
  let open Ast in
  match value with
  | Access (expr, member) ->
      Printf.sprintf "%s.%s" (gen_cpp_expression expr) member
  | Index (expr, indices) ->
      Printf.sprintf "%s[%s]" (gen_cpp_expression expr)
        (String.concat "][" (List.map gen_cpp_expression indices))
  | Call (expr, args) ->
      (* TODO: handle named arguments by reordering *)
      Printf.sprintf "%s(%s)" (gen_cpp_expression expr)
        (String.concat ", " (List.map gen_cpp_expression args))
  | Cast (t, expr) ->
      Printf.sprintf "static_cast<%s>(%s)" (zrl_to_cpp_type t)
        (gen_cpp_expression expr)
  | NamedArg (_, expr) ->
      gen_cpp_expression expr
  | BundledArg exprs ->
      Printf.sprintf "std::make_tuple(%s)"
        (String.concat ", " (List.map gen_cpp_expression exprs))
  | BinExpr (lhs, op, rhs) ->
      Printf.sprintf "%s %s %s" (gen_cpp_expression lhs)
        (Ast.string_of_binop op) (gen_cpp_expression rhs)
  | UnExpr (op, rhs) ->
      Printf.sprintf "%s(%s)" (Ast.string_of_unop op) (gen_cpp_expression rhs)
  | BoolLiteral true ->
      "true"
  | BoolLiteral false ->
      "false"
  | IntLiteral i ->
      string_of_int i
  | FloatLiteral f ->
      string_of_float f
  | Id id ->
      gen_cpp_builtin_call_id id

let is_rt_clear_or_write env op lvalues =
  let open Type in
  let is_rt tname =
    match Env.find_type ~local:false tname env with
    | Some L.{value= RenderTarget _; _} ->
        true
    | _ ->
        false
  in
  match op with
  | Ast.Assign | Ast.AssignPlus ->
      List.for_all
        (function [TypeRef tname], _ when is_rt tname -> true | _ -> false)
        lvalues
  | _ ->
      false

let gen_clear lhs rhs render_func =
  let bindings = List.combine lhs rhs in
  List.fold_left
    (fun render_func ((_, lhs), (_, rhs)) ->
      let rt_name =
        match gen_cpp_expression lhs with
        | "builtin.screen" ->
            "builtin_screen"
        | other ->
            other
      in
      RenderFunction.(
        render_func
        |> add_before_recording
             (Printf.sprintf
                {|if (current_render_pass != VK_NULL_HANDLE && rt_%s_load_op != VK_ATTACHMENT_LOAD_OP_DONT_CARE && rt_%s_load_op != VK_ATTACHMENT_LOAD_OP_CLEAR) { 
                  vkCmdEndRenderPass(gct_cmd_buffer_[image_index]);
                  current_render_pass = VK_NULL_HANDLE;
               }|}
                rt_name rt_name)
        |> add_before_recording
             (Printf.sprintf "rt_%s_load_op = VK_ATTACHMENT_LOAD_OP_CLEAR;"
                rt_name)
        |> add_before_recording
             (Printf.sprintf "rt_%s_clear_value = %s;" rt_name
                (gen_cpp_expression rhs))) )
    render_func bindings

let gen_write lhs rhs render_func =
  let set_clear_value var rt_name = function
    | Color ->
        Printf.sprintf
          {|
          std::memcpy(%s.color.float32, glm::value_ptr(rt_%s_clear_value), sizeof %s.color);
          rt_%s_load_op = VK_ATTACHMENT_LOAD_OP_LOAD;
        |}
          var rt_name var rt_name
    | DepthStencil ->
        Printf.sprintf
          {|
            %s.depthStencil.depth = rt_%s_clear_value.x;
            rt_%s_load_op = VK_ATTACHMENT_LOAD_OP_LOAD;
        |}
          var rt_name rt_name
  in
  let fb_imgs =
    List.map
      (fun (_, lhs) ->
        match gen_cpp_expression lhs with
        | "builtin.screen" ->
            "rt_builtin_screen_[image_index]"
        | id ->
            Printf.sprintf "rt_%s_" id )
      lhs
  in
  let rp_tuples, clear_values =
    List.split
      (List.map
         (fun (i, (_, lhs)) ->
           let rt_name =
             match gen_cpp_expression lhs with
             | "builtin.screen" ->
                 "builtin_screen"
             | other ->
                 other
           in
           let rt_type =
             MapString.find rt_name render_func.RenderFunction.render_targets
           in
           let tuple =
             Printf.sprintf
               "std::make_tuple(rt_%s_format, rt_%s_load_op, rt_%s_layout, \
                VK_IMAGE_LAYOUT_GENERAL)"
               rt_name rt_name rt_name
           in
           let cv_var = Printf.sprintf "clear_values[%d]" i in
           let cv =
             Printf.sprintf
               "if (rt_%s_load_op == VK_ATTACHMENT_LOAD_OP_CLEAR) {%s}" rt_name
               (set_clear_value cv_var rt_name rt_type)
           in
           (tuple, cv) )
         List.(combine (init (length lhs) (fun i -> i)) lhs))
  in
  let layouts =
    List.map
      (fun (_, lhs) ->
        let rt_name =
          match gen_cpp_expression lhs with
          | "builtin.screen" ->
              "builtin_screen"
          | other ->
              other
        in
        Printf.sprintf "rt_%s_layout = VK_IMAGE_LAYOUT_GENERAL;" rt_name )
      lhs
  in
  RenderFunction.(
    render_func |> add_before_recording "{"
    |> add_before_recording
         (Printf.sprintf "RenderPassDescriptor rp_desc {{%s}};"
            (String.concat ", " rp_tuples))
    |> add_before_recording
         (Printf.sprintf "FramebufferDescriptor fb_desc {{%s}};"
            (String.concat ", " fb_imgs))
    |> add_before_recording
         "VkRenderPass render_pass = GetOrCreateRenderPass(rp_desc);"
    |> add_before_recording
         "VkFramebuffer framebuffer = GetOrCreateFramebuffer(fb_desc, \
          render_pass);"
    |> add_before_recording "if (current_render_pass != render_pass) {"
    |> add_before_recording "  if (current_render_pass != VK_NULL_HANDLE) {"
    |> add_before_recording
         "    vkCmdEndRenderPass(gct_cmd_buffer_[image_index]);"
    |> add_before_recording "  }"
    |> add_before_recording (String.concat "\n" layouts)
    |> add_before_recording
         "  std::vector<VkClearValue> clear_values(rp_desc.attachments.size());"
    |> add_before_recording (String.concat "\n" clear_values)
    |> add_before_recording "  VkRenderPassBeginInfo begin_info = {};"
    |> add_before_recording
         "  begin_info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;"
    |> add_before_recording "  begin_info.pNext = nullptr;"
    |> add_before_recording "  begin_info.renderPass = render_pass;"
    |> add_before_recording "  begin_info.framebuffer = framebuffer;"
    |> add_before_recording "  begin_info.renderArea.offset = {0, 0};"
    |> add_before_recording
         "  begin_info.renderArea.extent = core_.GetSwapchain().GetExtent();"
    |> add_before_recording
         "  begin_info.clearValueCount = \
          static_cast<uint32_t>(clear_values.size());"
    |> add_before_recording "  begin_info.pClearValues = clear_values.data();"
    |> add_before_recording
         "  vkCmdBeginRenderPass(gct_cmd_buffer_[image_index], &begin_info, \
          VK_SUBPASS_CONTENTS_INLINE);"
    |> add_before_recording "  current_render_pass = render_pass;"
    |> add_before_recording "  // TODO: execute actual draw call"
    |> add_before_recording "}" |> add_before_recording "}")

let gen_clear_or_write lhs op rhs render_func =
  match op with
  | Ast.Assign ->
      gen_clear lhs rhs render_func
  | Ast.AssignPlus ->
      gen_write lhs rhs render_func
  | _ ->
      failwith ("unexpected operator: " ^ Ast.string_of_assignop op)

let rec gen_cpp_stmt stmt render_func =
  let open TypedAst in
  let env, L.{value= stmt; _} = stmt in
  match stmt with
  | Var {bind_ids; bind_values} | Val {bind_ids; bind_values} -> (
    match (bind_ids, bind_values) with
    | [id], [([typ], value)] ->
        let decl =
          Printf.sprintf "%s %s = %s;" (zrl_to_cpp_type typ) id
            (gen_cpp_expression value)
        in
        RenderFunction.(render_func |> add_before_recording decl)
    | ids, [(types, value)] ->
        let bindings = List.combine ids types in
        let render_func =
          List.fold_left
            (fun render_func (id, typ) ->
              let decl = Printf.sprintf "%s %s;" (zrl_to_cpp_type typ) id in
              RenderFunction.(render_func |> add_before_recording decl) )
            render_func bindings
        in
        let assignment =
          Printf.sprintf "std::tie(%s) = %s;" (String.concat ", " ids)
            (gen_cpp_expression value)
        in
        RenderFunction.(render_func |> add_before_recording assignment)
    | ids, values ->
        let bindings = List.combine ids values in
        List.fold_left
          (fun render_func (id, (t, value)) ->
            let decl =
              Printf.sprintf "%s %s = %s;"
                (zrl_to_cpp_type (List.hd t))
                id (gen_cpp_expression value)
            in
            RenderFunction.(render_func |> add_before_recording decl) )
          render_func bindings )
  | Assignment {asg_op; asg_lvalues; asg_rvalues} -> (
      if is_rt_clear_or_write env asg_op asg_lvalues then
        gen_clear_or_write asg_lvalues asg_op asg_rvalues render_func
      else
        match (asg_lvalues, asg_rvalues) with
        | [(_, lhs)], [([_], rhs)] ->
            let stmt =
              Printf.sprintf "%s %s %s;" (gen_cpp_expression lhs)
                (Ast.string_of_assignop asg_op)
                (gen_cpp_expression rhs)
            in
            RenderFunction.(render_func |> add_before_recording stmt)
        | lhs, [(_, rhs)] ->
            let lvalues =
              String.concat ", "
                (List.map (fun (_, expr) -> gen_cpp_expression expr) lhs)
            in
            let assign_stmt =
              Printf.sprintf "std::tie(%s) = %s;" lvalues
                (gen_cpp_expression rhs)
            in
            RenderFunction.(render_func |> add_before_recording assign_stmt)
        | lhs, rhs ->
            let bindings = List.combine lhs rhs in
            List.fold_left
              (fun render_func ((_, lhs), (_, rhs)) ->
                let stmt =
                  Printf.sprintf "%s %s %s;" (gen_cpp_expression lhs)
                    (Ast.string_of_assignop asg_op)
                    (gen_cpp_expression rhs)
                in
                RenderFunction.(render_func |> add_before_recording stmt) )
              render_func bindings )
  | If {if_cond= _, cond_expr; if_true; if_false} ->
      let render_func =
        RenderFunction.add_before_recording
          (Printf.sprintf "if (%s) {" (gen_cpp_expression cond_expr))
          render_func
      in
      let render_func =
        List.fold_left
          (fun render_func stmt -> gen_cpp_stmt stmt render_func)
          render_func if_true
      in
      let render_func =
        RenderFunction.add_before_recording "} else {" render_func
      in
      let render_func =
        List.fold_left
          (fun render_func stmt -> gen_cpp_stmt stmt render_func)
          render_func if_false
      in
      RenderFunction.add_before_recording "}" render_func
  | ForIter {foriter_id; foriter_it= _, it_expr; foriter_body} ->
      let render_func =
        RenderFunction.add_before_recording
          (Printf.sprintf "for (const auto &%s : %s) {" foriter_id
             (gen_cpp_expression it_expr))
          render_func
      in
      let render_func =
        List.fold_left
          (fun render_func stmt -> gen_cpp_stmt stmt render_func)
          render_func foriter_body
      in
      RenderFunction.add_before_recording "}" render_func
  | ForRange
      { forrange_id
      ; forrange_from= _, from_expr
      ; forrange_to= _, to_expr
      ; forrange_body } ->
      let render_func =
        RenderFunction.add_before_recording
          (Printf.sprintf "for (int %s = (%s); i <= (%s); ++%s) {" forrange_id
             (gen_cpp_expression from_expr)
             (gen_cpp_expression to_expr)
             forrange_id)
          render_func
      in
      let render_func =
        List.fold_left
          (fun render_func stmt -> gen_cpp_stmt stmt render_func)
          render_func forrange_body
      in
      RenderFunction.add_before_recording "}" render_func
  | Return [(_, expr)] ->
      let return_stmt =
        Printf.sprintf "return %s;" (gen_cpp_expression expr)
      in
      RenderFunction.add_before_recording return_stmt render_func
  | Return exprs ->
      let return_stmt =
        Printf.sprintf "return std::make_tuple(%s);"
          (String.concat ", "
             (List.map (fun (_, expr) -> gen_cpp_expression expr) exprs))
      in
      RenderFunction.add_before_recording return_stmt render_func
  | Discard ->
      failwith "'discard' statement cannot be used outside fragment shaders"

let rec gen_cpp_code fd_body render_func =
  match fd_body with
  | [] ->
      Ok render_func
  | stmt :: body ->
      let render_func = gen_cpp_stmt stmt render_func in
      gen_cpp_code body render_func

let gen_renderer_code loc rd_name rd_functions r =
  let err = error loc (`MissingRendererEntryPoint rd_name) in
  find_func "main" rd_functions err >>= fun TypedAst.{fd_body; _} ->
  let render_func =
    RenderFunction.(
      empty r.r_render_targets
      |> add_before_recording
           {|    VkRenderPass current_render_pass = VK_NULL_HANDLE;
    constexpr std::array<VkPipelineStageFlags, 1> wait_stages = {
      VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
    const VkDevice device = core_.GetLogicalDevice().GetHandle();
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
  |}
      |> add_after_recording
           {|    if (current_render_pass != VK_NULL_HANDLE) {
      vkCmdEndRenderPass(gct_cmd_buffer_[image_index]);
    }
    {
      // Extra render pass to flush pending screen clears and transition
      // screen image view into PRESENT layout.
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
    CHECK_VK(vkEndCommandBuffer(gct_cmd_buffer_[image_index]));
    // Render
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

    // Present
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
  |})
  in
  gen_cpp_code fd_body render_func >>= fun render_func ->
  Ok
    { r with
      r_render_function=
        Function.(
          r.r_render_function
          |> append_code_sections (RenderFunction.all_sections render_func)) }

let gen_render_targets rd_type r =
  match rd_type with
  | Type.Function (params, _) ->
      List.fold_left
        (fun acc (name, t) ->
          acc >>= fun r ->
          match t with
          | Type.TypeRef (("rt_rgba" | "rt_ds") as rt_type_name) ->
              let id = "rt_" ^ name ^ "_" in
              let image = ("std::unique_ptr<zrl::Image>", id) in
              let rt_type, fname, fmt, clear_value_cpp_type =
                if rt_type_name = "rt_rgba" then
                  ( Color
                  , "CreateColorRenderTarget"
                  , "VK_FORMAT_B8G8R8A8_UNORM"
                  , "glm::fvec4" )
                else
                  ( DepthStencil
                  , "CreateDepthRenderTarget"
                  , "VK_FORMAT_D32_SFLOAT"
                  , "glm::fvec2" )
              in
              let load_op_member = ("VkAttachmentLoadOp", id ^ "load_op") in
              let clear_value_member =
                (clear_value_cpp_type, id ^ "clear_value")
              in
              Ok
                { r_class=
                    Class.(
                      r.r_class |> add_private_member image
                      |> add_private_member load_op_member
                      |> add_private_member clear_value_member)
                ; r_ctor=
                    Function.(
                      r.r_ctor
                      |> append_code_section
                           (Printf.sprintf "  %s = %s(core_);" id fname)
                      |> add_member_initializer
                           (id ^ "load_op", "VK_ATTACHMENT_LOAD_OP_DONT_CARE"))
                ; r_dtor=
                    Function.(
                      r.r_dtor |> prepend_code_section ("  " ^ id ^ ".reset();"))
                ; r_render_function=
                    Function.(
                      r.r_render_function
                      |> append_code_section
                           (Printf.sprintf
                              "constexpr VkFormat rt_%s_format = %s;" name fmt)
                      |> append_code_section
                           (Printf.sprintf
                              "VkImageLayout rt_%s_layout = \
                               VK_IMAGE_LAYOUT_UNDEFINED;"
                              name))
                ; r_render_targets=
                    MapString.(r.r_render_targets |> add name rt_type) }
          | _ ->
              acc )
        (Ok r) params
  | _ ->
      failwith "renderer must be of Function type"

let gen_renderer_state r =
  Ok
    { r_class=
        Class.(
          r.r_class
          |> add_private_member
               ( "std::unordered_map<RenderPassDescriptor, VkRenderPass, \
                  RenderPassDescriptorHash>"
               , "render_pass_cache_" )
          |> add_private_member
               ( "std::unordered_map<FramebufferDescriptor, VkFramebuffer, \
                  FramebufferDescriptorHash>"
               , "framebuffer_cache_" )
          |> add_private_member
               ("std::vector<VkImageView>", "rt_builtin_screen_")
          |> add_private_member
               ("VkAttachmentLoadOp", "rt_builtin_screen_load_op")
          |> add_private_member ("glm::fvec4", "rt_builtin_screen_clear_value"))
    ; r_ctor=
        Function.(
          r.r_ctor
          |> add_member_initializer
               ("rt_builtin_screen_load_op", "VK_ATTACHMENT_LOAD_OP_DONT_CARE")
          |> append_code_section
               {|  rt_builtin_screen_.resize(core_.GetSwapchain().GetImageCount());
  for (size_t i = 0; i < rt_builtin_screen_.size(); ++i) {
    rt_builtin_screen_[i] = CreateImageView(core_.GetLogicalDevice().GetHandle(),
                                            core_.GetSwapchain().GetImages()[i],
                                            core_.GetSwapchain().GetSurfaceFormat());
  }
  |})
    ; r_dtor=
        Function.(
          r.r_dtor
          |> prepend_code_section
               {|  for (auto image_view : rt_builtin_screen_) {
    vkDestroyImageView(core_.GetLogicalDevice().GetHandle(), image_view, nullptr);
      }|})
    ; r_render_function=
        Function.(
          r.r_render_function
          |> append_code_section
               "VkImageLayout rt_builtin_screen_layout = \
                VK_IMAGE_LAYOUT_UNDEFINED;"
          |> append_code_section
               "constexpr VkFormat rt_builtin_screen_format = \
                VK_FORMAT_B8G8R8A8_UNORM;"
          |> append_code_section "")
    ; r_render_targets=
        MapString.(r.r_render_targets |> add "builtin_screen" Color) }

let gen_renderer loc Config.{cfg_output_directory= dir; _}
    TypedAst.{rd_name; rd_type; rd_functions; _} =
  let r =
    { r_class=
        Class.(
          empty rd_name [] |> add_include "<algorithm>"
          |> add_include "<string>"
          |> add_include "<unordered_set>"
          |> add_include "<unordered_map>"
          |> add_include "<vector>"
          |> add_include "\"vulkan/vulkan.h\""
          |> add_include "\"core/Core.h\""
          |> add_include "\"core/Image.h\""
          |> add_include "\"core/Log.h\""
          |> add_include "\"glm/glm.hpp\""
          |> add_include "\"glm/gtc/type_ptr.hpp\""
          |> add_private_member (render_pass_descriptor_struct, "")
          |> add_private_member (render_pass_descriptor_hash, "")
          |> add_private_member (framebuffer_descriptor_struct, "")
          |> add_private_member (framebuffer_descriptor_hash, "")
          |> add_private_member ("zrl::Core&", "core_")
          |> add_private_member ("VkCommandPool", "gct_cmd_pool_")
          |> add_private_member ("VkCommandPool", "present_cmd_pool_")
          |> add_private_member
               ("std::vector<VkCommandBuffer>", "gct_cmd_buffer_")
          |> add_private_member
               ("std::vector<VkCommandBuffer>", "present_cmd_buffer_")
          |> add_private_member
               ("std::vector<VkFence>", "gct_cmd_buffer_fence_")
          |> add_private_member
               ("std::vector<VkFence>", "present_cmd_buffer_fence_")
          |> add_private_member ("VkSemaphore", "acquire_semaphore_")
          |> add_private_member ("VkSemaphore", "render_semaphore_")
          |> add_private_function get_or_create_render_pass
          |> add_private_function get_or_create_framebuffer
          |> add_static_section create_command_pool
          |> add_static_section create_command_buffers
          |> add_static_section create_command_buffer_fences
          |> add_static_section create_image_view
          |> add_static_section create_color_render_target
          |> add_static_section create_depth_render_target)
    ; r_ctor=
        Function.(
          empty rd_name
          |> add_param ("zrl::Core&", "core")
          |> add_member_initializer ("core_", "core")
          |> add_member_initializer ("gct_cmd_pool_", "VK_NULL_HANDLE")
          |> add_member_initializer ("present_cmd_pool_", "VK_NULL_HANDLE")
          |> add_member_initializer ("acquire_semaphore_", "VK_NULL_HANDLE")
          |> add_member_initializer ("render_semaphore_", "VK_NULL_HANDLE")
          |> append_code_section (ctor_body rd_name))
    ; r_dtor= Function.(empty ("~" ^ rd_name) |> append_code_section dtor_body)
    ; r_render_function= Function.(empty "Render" |> set_return_type "void")
    ; r_render_targets= MapString.empty }
  in
  gen_renderer_state r >>= fun r ->
  gen_render_targets rd_type r >>= fun r ->
  gen_renderer_signature loc rd_name rd_functions r >>= fun r ->
  gen_renderer_code loc rd_name rd_functions r >>= fun r ->
  let output_class =
    Class.(
      r.r_class
      |> add_public_function r.r_ctor
      |> add_public_function
           Function.(
             r.r_dtor |> prepend_code_section wait_device_and_queues_idle)
      |> add_public_function r.r_render_function)
  in
  let hdr_file = Printf.sprintf "%s/%s.h" dir rd_name in
  let src_file = Printf.sprintf "%s/%s.cc" dir rd_name in
  write_file hdr_file (Class.string_of_header output_class) ;
  write_file src_file (Class.string_of_source output_class) ;
  let lib =
    Library.(
      empty rd_name "" |> add_class output_class |> add_dep "//core"
      |> add_dep "@glm" |> add_dep "@vulkan//:sdk")
  in
  Ok (Library.string_of_library lib)

let gen_build c renderer_targets =
  let fname = c.Config.cfg_output_directory ^ "/BUILD" in
  let contents = build renderer_targets in
  let () = write_file fname contents in
  Ok ()

let gen cfg TypedAst.{root_elems; _} =
  List.fold_left
    (fun acc tl ->
      acc >>= fun renderer_targets ->
      let L.{loc; value} = tl in
      match value with
      | TypedAst.RendererDecl rd ->
          gen_renderer loc cfg rd >>= fun t -> Ok (t :: renderer_targets)
      | _ ->
          acc )
    (Ok []) root_elems
  >>= gen_build cfg
