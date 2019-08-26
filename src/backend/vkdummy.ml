open Zrl
open Extensions
open Monad.Result
module L = Located
module MapString = Map.Make (String)
module SetString = Set.Make (String)

module Error = struct
  type t =
    [ `Unsupported of string
    | `MissingRendererEntryPoint of string
    | `InvalidUniformType of Type.t * string
    | `StageMismatch of string * Type.t list * string * Type.t list
    | `MissingInputBinding of string * string
    | `MultipleDepthBuffers ]

  let string_of_error L.{ loc; value } =
    let pos = L.string_of_start_position loc in
    let prefix = Printf.sprintf "%s: backend error" pos in
    match value with
    | `Unsupported msg -> Printf.sprintf "%s: unsupported: %s" prefix msg
    | `MissingRendererEntryPoint r ->
        Printf.sprintf "%s: renderer %s is missing the entry point" prefix r
    | `InvalidUniformType (t, reason) ->
        Printf.sprintf "%s: invalid uniform type '%s': %s" prefix
          (Type.string_of_type t) reason
    | `StageMismatch (src_name, src, dst_name, dst) ->
        let src_sig = String.concat ", " (List.map Type.string_of_type src) in
        let dst_sig = String.concat ", " (List.map Type.string_of_type dst) in
        Printf.sprintf
          "%s: stage output-input signatures must match:\n\
           \t'%s' output signature: (%s)\n\
           \t'%s' input signature: (%s)"
          prefix src_name src_sig dst_name dst_sig
    | `MissingInputBinding (pipeline, input) ->
        Printf.sprintf "%s: missing binding for input %s in call to %s" prefix
          input pipeline
    | `MultipleDepthBuffers ->
        Printf.sprintf
          "%s: at most one depth buffer can be written per clear or write \
           statement."
          prefix
end

type graphics_pipeline = {
  gp_name : string;
  gp_declaration : TypedAst.pipeline_declaration;
  gp_uniforms : (int * int * int * string * Type.t * string) list;
  gp_uniform_set : int MapString.t;
  gp_inputs : (int * string * Type.t) list;
  gp_outputs : (int * Type.t) list;
  gp_vertex_stage : TypedAst.function_declaration;
  gp_geometry_stage : TypedAst.function_declaration option;
  gp_fragment_stage : TypedAst.function_declaration option;
  gp_helper_functions : TypedAst.function_declaration list;
  gp_depth_test_enabled : bool;
  gp_depth_write_enabled : bool;
  gp_depth_compare_op : string;
}

let error loc e = Error L.{ loc; value = e }

let builtin_pos =
  Lexing.{ pos_fname = "builtin"; pos_lnum = 0; pos_cnum = 0; pos_bol = 0 }

let builtin_loc = (builtin_pos, builtin_pos)

let array_type_size dims =
  List.fold_left
    (fun acc -> function Type.OfInt i -> i * acc
      | _ -> failwith "array dimensions must be known at compile time")
    1 dims

let rec has_opaque_members env seen =
  let open Type in
  function
  | Sampler _ | Texture _ -> true
  | Array (t, _) -> has_opaque_members env seen t
  | Record fields ->
      List.exists (fun (_, t) -> has_opaque_members env seen t) fields
  | TypeRef name -> (
      if SetString.mem name seen then false
      else
        match Env.find_type ~local:false name env with
        | Some L.{ value = t; _ } ->
            has_opaque_members env (SetString.add name seen) t
        | _ -> false )
  | _ -> false

let rec glsl_type_locations env =
  let open Type in
  function
  | Primitive Int -> 1
  | Primitive Bool -> 1
  | Primitive Float -> 1
  | Vector (Float, 2) -> 1
  | Vector (Float, 3) -> 1
  | Vector (Float, 4) -> 1
  | Vector (Int, 2) -> 1
  | Vector (Int, 3) -> 1
  | Vector (Int, 4) -> 1
  | Vector (UInt, 2) -> 1
  | Vector (UInt, 3) -> 1
  | Vector (UInt, 4) -> 1
  | Matrix (Float, 2, 2) -> 2
  | Matrix (Float, 3, 3) -> 3
  | Matrix (Float, 4, 4) -> 4
  | Array (t, dims) ->
      let size = array_type_size dims in
      size * glsl_type_locations env t
  | Record fields ->
      List.fold_left
        (fun acc (_, t) -> acc + glsl_type_locations env t)
        0 fields
  | TypeRef name -> (
      match Env.find_type ~local:false name env with
      | Some L.{ value = Record _ as r; _ } -> glsl_type_locations env r
      | Some L.{ value = t; _ } ->
          failwith ("unsupported type: " ^ string_of_type t)
      | None -> failwith ("no such type: " ^ name) )
  | t ->
      failwith
        (Printf.sprintf "unsupported type: '%s'" (Type.string_of_type t))

let rec zrl_to_glsl_type =
  let open Type in
  function
  | Primitive Bool -> "bool"
  | Primitive Int -> "int"
  | Primitive Float -> "float"
  | Vector (Float, 2) -> "vec2"
  | Vector (Float, 3) -> "vec3"
  | Vector (Float, 4) -> "vec4"
  | Vector (Int, 2) -> "ivec2"
  | Vector (Int, 3) -> "ivec3"
  | Vector (Int, 4) -> "ivec4"
  | Vector (UInt, 2) -> "uvec2"
  | Vector (UInt, 3) -> "uvec3"
  | Vector (UInt, 4) -> "uvec4"
  | Matrix (Float, 2, 2) -> "mat2"
  | Matrix (Float, 3, 3) -> "mat3"
  | Matrix (Float, 4, 4) -> "mat4"
  | Sampler 2 -> "sampler2D"
  | SamplerCube -> "samplerCube"
  | TypeRef s -> s
  | Array (t, dims) ->
      let string_of_dim = function
        | OfInt i -> Printf.sprintf "[%d]" i
        | _ -> failwith "unsupported array dimension"
      in
      let dims = List.map string_of_dim dims in
      Printf.sprintf "%s%s" (zrl_to_glsl_type t) (String.concat "" dims)
  | t ->
      failwith
        (Printf.sprintf "unsupported type: '%s'" (Type.string_of_type t))

let rec zrl_to_cpp_type =
  let open Type in
  function
  | Primitive Int -> "int32_t"
  | Primitive UInt -> "uint32_t"
  | Primitive Bool -> "VkBool32"
  | Primitive Float -> "float"
  | Primitive Double -> "double"
  | Vector (Bool, 2) -> "glm::bvec2"
  | Vector (Bool, 3) -> "glm::bvec3"
  | Vector (Bool, 4) -> "glm::bvec4"
  | Vector (Float, 2) -> "glm::fvec2"
  | Vector (Float, 3) -> "glm::fvec3"
  | Vector (Float, 4) -> "glm::fvec4"
  | Vector (Int, 2) -> "glm::ivec2"
  | Vector (Int, 3) -> "glm::ivec3"
  | Vector (Int, 4) -> "glm::ivec4"
  | Vector (UInt, 2) -> "glm::uvec2"
  | Vector (UInt, 3) -> "glm::uvec3"
  | Vector (UInt, 4) -> "glm::uvec4"
  | Vector (Double, 2) -> "glm::dvec2"
  | Vector (Double, 3) -> "glm::dvec3"
  | Vector (Double, 4) -> "glm::dvec4"
  | Matrix (Float, 2, 2) -> "glm::fmat2"
  | Matrix (Float, 3, 3) -> "glm::fmat3"
  | Matrix (Float, 4, 4) -> "glm::fmat4"
  | Array (t, dims) ->
      let arr t = function
        | OfInt i -> Printf.sprintf "std::array<%s, %d>" t i
        | _ -> failwith "unsupported array dimension"
      in
      let rec aux = function
        | [] -> ""
        | [ d ] -> arr (zrl_to_cpp_type t) d
        | d :: ds -> arr (aux ds) d
      in
      aux dims
  | RenderTarget _ -> "RenderTargetReference*"
  | Sampler 2 -> "SampledImage2DReference"
  | SamplerCube -> "SampledCubeMapReference"
  | TypeRef s -> s
  | t -> failwith "unsupported type: " ^ string_of_type t

let call_has_named_args args =
  List.for_all
    (function L.{ value = Ast.NamedArg _; _ } -> true | _ -> false)
    args

let reorder_call_args env expr args =
  match Analysis.check_single_value_expr env expr with
  | Ok (Type.Function (params, _)) ->
      let indexed_params = List.index params in
      let index_of arg =
        let i, _ =
          List.find
            (fun (_, (param_name, _)) ->
              match arg with
              | L.{ value = Ast.NamedArg (arg_name, _); _ } ->
                  arg_name = param_name
              | _ -> failwith "cannot reorder non-named arguments")
            indexed_params
        in
        i
      in
      let cmp arg1 arg2 = index_of arg1 - index_of arg2 in
      List.sort cmp args
  | _ -> failwith "unexpected non-Function type in call expression"

let gen_cpp_builtin_call_id = function
  | "ivec2" -> "glm::ivec2"
  | "ivec3" -> "glm::ivec3"
  | "ivec4" -> "glm::ivec4"
  | "uvec2" -> "glm::uvec2"
  | "uvec3" -> "glm::uvec3"
  | "uvec4" -> "glm::uvec4"
  | "fvec2" -> "glm::fvec2"
  | "fvec3" -> "glm::fvec3"
  | "fvec4" -> "glm::fvec4"
  | "dvec2" -> "glm::dvec2"
  | "dvec3" -> "glm::dvec3"
  | "dvec4" -> "glm::dvec4"
  | "bvec2" -> "glm::bvec2"
  | "bvec3" -> "glm::bvec3"
  | "bvec4" -> "glm::bvec4"
  | "imat2" -> "glm::imat2"
  | "imat3" -> "glm::imat3"
  | "imat4" -> "glm::imat4"
  | "umat2" -> "glm::umat2"
  | "umat3" -> "glm::umat3"
  | "umat4" -> "glm::umat4"
  | "fmat2" -> "glm::fmat2"
  | "fmat3" -> "glm::fmat3"
  | "fmat4" -> "glm::fmat4"
  | "dmat2" -> "glm::dmat2"
  | "dmat3" -> "glm::dmat3"
  | "dmat4" -> "glm::dmat4"
  | "bmat2" -> "glm::bmat2"
  | "bmat3" -> "glm::bmat3"
  | "bmat4" -> "glm::bmat4"
  | other -> other

let rec gen_cpp_expression env L.{ value; _ } =
  let open Ast in
  match value with
  | Access (expr, member) ->
      Printf.sprintf "%s.%s" (gen_cpp_expression env expr) member
  | Index (expr, indices) ->
      Printf.sprintf "%s[%s]"
        (gen_cpp_expression env expr)
        (String.concat "][" (List.map (gen_cpp_expression env) indices))
  | Call (expr, args) ->
      let args =
        if call_has_named_args args then reorder_call_args env expr args
        else args
      in
      Printf.sprintf "%s(%s)"
        (gen_cpp_expression env expr)
        (String.concat ", " (List.map (gen_cpp_expression env) args))
  | Cast (t, expr) ->
      Printf.sprintf "static_cast<%s>(%s)" (zrl_to_cpp_type t)
        (gen_cpp_expression env expr)
  | NamedArg (_, expr) -> gen_cpp_expression env expr
  | BinExpr (lhs, op, rhs) ->
      Printf.sprintf "(%s %s %s)"
        (gen_cpp_expression env lhs)
        (Ast.string_of_binop op)
        (gen_cpp_expression env rhs)
  | UnExpr (op, rhs) ->
      Printf.sprintf "%s(%s)" (Ast.string_of_unop op)
        (gen_cpp_expression env rhs)
  | BoolLiteral true -> "true"
  | BoolLiteral false -> "false"
  | IntLiteral i -> string_of_int i
  | FloatLiteral f -> string_of_float f
  | Id id -> gen_cpp_builtin_call_id id

let is_rt_clear_or_write op lvalues =
  let open Type in
  match op with
  | Ast.Assign | Ast.AssignPlus ->
      List.for_all
        (function [ RenderTarget _ ], _ -> true | _ -> false)
        lvalues
  | _ -> false

let check_at_most_one_depth_buffer loc lhs =
  List.fold_left
    (fun acc (types, _) ->
      acc >>= fun has_depth_buffer ->
      match types with
      | [ Type.RenderTarget DS ] ->
          if has_depth_buffer then error loc `MultipleDepthBuffers else Ok true
      | _ -> acc)
    (Ok false) lhs
  >>= fun _ -> Ok ()

let gen_clear env loc lhs rhs f =
  check_at_most_one_depth_buffer loc lhs >>= fun () ->
  let bindings = List.combine lhs rhs in
  let f =
    List.fold_left
      (fun f ((ts, lhs), (_, rhs)) ->
        let rt_var =
          Printf.sprintf "RenderTargetReference *_rt = (%s);"
            (gen_cpp_expression env lhs)
        in
        let end_current_pass =
          {|  if (current_render_pass_ != VK_NULL_HANDLE && _rt->load_op != VK_ATTACHMENT_LOAD_OP_DONT_CARE && _rt->load_op != VK_ATTACHMENT_LOAD_OP_CLEAR) { 
    vkCmdEndRenderPass(gc_cmd_buffer_[image_index_]);
    current_render_pass_ = VK_NULL_HANDLE;
  }|}
        in
        let update_load_op = "_rt->load_op = VK_ATTACHMENT_LOAD_OP_CLEAR;" in
        let clear_value_expr = gen_cpp_expression env rhs in
        let update_clear_value =
          match ts with
          | [ Type.RenderTarget DS ] ->
              Printf.sprintf "_rt->clear_value.depthStencil.depth = %s;"
                clear_value_expr
          | _ ->
              Printf.sprintf
                "std::memcpy(&_rt->clear_value, glm::value_ptr(%s), sizeof \
                 (%s));"
                clear_value_expr clear_value_expr
        in
        Cpp.Function.(
          f |> append_code_section "{" |> append_code_section rt_var
          |> append_code_section end_current_pass
          |> append_code_section update_load_op
          |> append_code_section update_clear_value
          |> append_code_section "}"))
      f bindings
  in
  Ok f

let gen_shader_stage_create_infos p =
  let vertex_stage =
    [
      Printf.sprintf
        {|
    VkPipelineShaderStageCreateInfo vertex_stage = {};
    vertex_stage.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    vertex_stage.stage = VK_SHADER_STAGE_VERTEX_BIT;
    vertex_stage.module = k%s_vert_glsl_shader_module_;
    vertex_stage.pName = "main";
    vertex_stage.pSpecializationInfo = nullptr;
    shader_stages.push_back(vertex_stage);
  |}
        p.gp_name;
    ]
  in
  let fragment_stage =
    match p.gp_fragment_stage with
    | Some _ ->
        let fragment_stage =
          Printf.sprintf
            {|
    VkPipelineShaderStageCreateInfo fragment_stage = {};
    fragment_stage.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    fragment_stage.stage = VK_SHADER_STAGE_FRAGMENT_BIT;
    fragment_stage.module = k%s_frag_glsl_shader_module_;
    fragment_stage.pName = "main";
    fragment_stage.pSpecializationInfo = nullptr;
    shader_stages.push_back(fragment_stage);
  |}
            p.gp_name
        in
        [ fragment_stage ]
    | None -> [ "" ]
  in
  let stages = List.flatten [ vertex_stage; fragment_stage ] in
  String.concat "\n" stages

let format_of_vertex_input_type t =
  let open Type in
  match t with
  | Primitive Bool -> "VK_FORMAT_R32_UINT"
  | Primitive Int -> "VK_FORMAT_R32_SINT"
  | Primitive UInt -> "VK_FORMAT_R32_UINT"
  | Primitive Float -> "VK_FORMAT_R32_SFLOAT"
  | Vector (Bool, 2) -> "VK_FORMAT_R32G32_UINT"
  | Vector (Bool, 3) -> "VK_FORMAT_R32G32B32_UINT"
  | Vector (Bool, 4) -> "VK_FORMAT_R32G32B32A32_UINT"
  | Vector (Int, 2) -> "VK_FORMAT_R32G32_SINT"
  | Vector (Int, 3) -> "VK_FORMAT_R32G32B32_SINT"
  | Vector (Int, 4) -> "VK_FORMAT_R32G32B32A32_SINT"
  | Vector (UInt, 2) -> "VK_FORMAT_R32G32_UINT"
  | Vector (UInt, 3) -> "VK_FORMAT_R32G32B32_UINT"
  | Vector (UInt, 4) -> "VK_FORMAT_R32G32B32A32_UINT"
  | Vector (Float, 2) -> "VK_FORMAT_R32G32_SFLOAT"
  | Vector (Float, 3) -> "VK_FORMAT_R32G32B32_SFLOAT"
  | Vector (Float, 4) -> "VK_FORMAT_R32G32B32A32_SFLOAT"
  | _ -> failwith ("invalid input vertex type: " ^ Type.string_of_type t)

let gen_vertex_input_state_create_info p =
  let num_inputs = List.length p.gp_inputs in
  let vertex_bindings =
    List.map
      (fun (i, (_, _, t)) ->
        Printf.sprintf
          {|
    vertex_bindings[%d].binding = %d;
    vertex_bindings[%d].stride = sizeof(%s);
    vertex_bindings[%d].inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
    |}
          i i i (zrl_to_cpp_type t) i)
      (List.index p.gp_inputs)
  in
  let vertex_attributes =
    List.map
      (fun (i, (location, _, t)) ->
        Printf.sprintf
          {|
    vertex_attributes[%d].location = %d;
    vertex_attributes[%d].binding = %d;
    vertex_attributes[%d].format = %s;
    vertex_attributes[%d].offset = 0;|}
          i location i i i
          (format_of_vertex_input_type t)
          i)
      (List.index p.gp_inputs)
  in
  Printf.sprintf
    {|
    std::array<VkVertexInputBindingDescription, %d> vertex_bindings;
    std::array<VkVertexInputAttributeDescription, %d> vertex_attributes;

    %s
    %s

    VkPipelineVertexInputStateCreateInfo vertex_input_state = {};
    vertex_input_state.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
    vertex_input_state.pNext = nullptr;
    vertex_input_state.flags = 0;
    vertex_input_state.vertexBindingDescriptionCount = static_cast<uint32_t>(vertex_bindings.size());
    vertex_input_state.pVertexBindingDescriptions = vertex_bindings.data();
    vertex_input_state.vertexAttributeDescriptionCount = static_cast<uint32_t>(vertex_attributes.size());
    vertex_input_state.pVertexAttributeDescriptions = vertex_attributes.data();
  |}
    num_inputs num_inputs
    (String.concat "\n" vertex_bindings)
    (String.concat "\n" (List.rev vertex_attributes))

let gen_create_and_bind_pipeline env p f lhs =
  let shader_stages = gen_shader_stage_create_infos p in
  let vertex_input_state_create_info = gen_vertex_input_state_create_info p in
  let num_color_attachments =
    List.fold_left
      (fun acc (_, lhs) ->
        match Analysis.check_expr env lhs with
        | Ok [ Type.RenderTarget (RGB | RGBA) ] -> 1 + acc
        | Ok [ Type.RenderTarget DS ] -> acc
        | _ -> failwith "unexpected expression type")
      0 lhs
  in
  let color_blend_attachments =
    String.concat "\n"
      (List.init num_color_attachments (fun i ->
           Printf.sprintf
             {|
    color_blend_attachments[%d].blendEnable = VK_FALSE;
    color_blend_attachments[%d].srcColorBlendFactor = VK_BLEND_FACTOR_ONE;
    color_blend_attachments[%d].dstColorBlendFactor = VK_BLEND_FACTOR_ZERO;
    color_blend_attachments[%d].colorBlendOp = VK_BLEND_OP_ADD;
    color_blend_attachments[%d].srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE;
    color_blend_attachments[%d].dstAlphaBlendFactor = VK_BLEND_FACTOR_ZERO;
    color_blend_attachments[%d].alphaBlendOp = VK_BLEND_OP_ADD;
    color_blend_attachments[%d].colorWriteMask =
        VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT |
        VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
  |}
             i i i i i i i i))
  in
  Cpp.Function.(
    f
    |> append_code_section
         (Printf.sprintf
            {|{
    std::vector<VkPipelineShaderStageCreateInfo> shader_stages;

    %s

    %s

    VkPipelineInputAssemblyStateCreateInfo input_assembly_state = {};
    input_assembly_state.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
    input_assembly_state.pNext = nullptr;
    input_assembly_state.flags = 0;
    input_assembly_state.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
    input_assembly_state.primitiveRestartEnable = false;

    VkViewport viewport = {};
    viewport.x = 0.0f;
    viewport.y = 0.0f;
    viewport.width = core_.GetSwapchain().GetExtent().width;
    viewport.height = core_.GetSwapchain().GetExtent().height;
    viewport.minDepth = 0.0f;
    viewport.maxDepth = 1.0f;
    VkRect2D scissor = {};
    scissor.offset = {0, 0};
    scissor.extent = core_.GetSwapchain().GetExtent();
    VkPipelineViewportStateCreateInfo viewport_state = {};
    viewport_state.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
    viewport_state.pNext = nullptr;
    viewport_state.viewportCount = 1;
    viewport_state.pViewports = &viewport;
    viewport_state.scissorCount = 1;
    viewport_state.pScissors = &scissor;

    VkPipelineRasterizationStateCreateInfo rasterization_state = {};
    rasterization_state.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
    rasterization_state.pNext = nullptr;
    rasterization_state.flags = 0;
    rasterization_state.depthClampEnable = VK_FALSE;
    rasterization_state.rasterizerDiscardEnable = VK_FALSE;
    rasterization_state.polygonMode = VK_POLYGON_MODE_FILL;
    rasterization_state.cullMode = VK_CULL_MODE_BACK_BIT;
#ifndef ZRL_FRONT_FACE_COUNTER_CLOCKWISE
    rasterization_state.frontFace = VK_FRONT_FACE_CLOCKWISE;
#else
    rasterization_state.frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE;
#endif
    rasterization_state.depthBiasEnable = VK_FALSE;
    rasterization_state.depthBiasConstantFactor = 0.0f;
    rasterization_state.depthBiasClamp = 0.0f;
    rasterization_state.depthBiasSlopeFactor = 0.0f;
    rasterization_state.lineWidth = 1.0f;

    VkPipelineMultisampleStateCreateInfo multisample_state = {};
    multisample_state.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
    multisample_state.pNext = nullptr;
    multisample_state.flags = 0;
    multisample_state.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;
    multisample_state.sampleShadingEnable = VK_FALSE;
    multisample_state.minSampleShading = 1.0f;
    multisample_state.pSampleMask = nullptr;
    multisample_state.alphaToCoverageEnable = VK_FALSE;
    multisample_state.alphaToOneEnable = VK_FALSE;

    VkPipelineDepthStencilStateCreateInfo depth_stencil_state = {};
    depth_stencil_state.sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
    depth_stencil_state.pNext = nullptr;
    depth_stencil_state.depthTestEnable = %s;
    depth_stencil_state.depthWriteEnable = %s;
    depth_stencil_state.depthCompareOp = %s;
    depth_stencil_state.depthBoundsTestEnable = VK_FALSE;
    depth_stencil_state.stencilTestEnable = VK_FALSE;
    depth_stencil_state.front.compareOp = VK_COMPARE_OP_ALWAYS;
    depth_stencil_state.front.failOp = VK_STENCIL_OP_KEEP;
    depth_stencil_state.front.passOp = VK_STENCIL_OP_KEEP;
    depth_stencil_state.back.compareOp = VK_COMPARE_OP_ALWAYS;
    depth_stencil_state.back.failOp = VK_STENCIL_OP_KEEP;
    depth_stencil_state.back.passOp = VK_STENCIL_OP_KEEP;
    depth_stencil_state.minDepthBounds = 0.0f;
    depth_stencil_state.maxDepthBounds = 1.0f;

    std::array<VkPipelineColorBlendAttachmentState, %d> color_blend_attachments;

    %s

    VkPipelineColorBlendStateCreateInfo color_blend_state = {};
    color_blend_state.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
    color_blend_state.pNext = nullptr;
    color_blend_state.flags = 0;
    color_blend_state.logicOpEnable = VK_FALSE;
    color_blend_state.logicOp = VK_LOGIC_OP_COPY;
    color_blend_state.attachmentCount = static_cast<uint32_t>(color_blend_attachments.size());
    color_blend_state.pAttachments = color_blend_attachments.data();
    color_blend_state.blendConstants[0] = 0.0f;
    color_blend_state.blendConstants[1] = 0.0f;
    color_blend_state.blendConstants[2] = 0.0f;
    color_blend_state.blendConstants[3] = 0.0f;

    VkGraphicsPipelineCreateInfo graphics_pipeline_create_info = {};
    graphics_pipeline_create_info.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
    graphics_pipeline_create_info.pNext = nullptr;
    graphics_pipeline_create_info.flags = 0;
    graphics_pipeline_create_info.stageCount = static_cast<uint32_t>(shader_stages.size());
    graphics_pipeline_create_info.pStages = shader_stages.data();
    graphics_pipeline_create_info.pVertexInputState = &vertex_input_state;
    graphics_pipeline_create_info.pInputAssemblyState = &input_assembly_state;
    graphics_pipeline_create_info.pTessellationState = nullptr;
    graphics_pipeline_create_info.pViewportState = &viewport_state;
    graphics_pipeline_create_info.pRasterizationState = &rasterization_state;
    graphics_pipeline_create_info.pMultisampleState = &multisample_state;
    graphics_pipeline_create_info.pDepthStencilState = &depth_stencil_state;
    graphics_pipeline_create_info.pColorBlendState = &color_blend_state;
    graphics_pipeline_create_info.pDynamicState = nullptr;
    graphics_pipeline_create_info.layout = %s_pipeline_layout_;
    graphics_pipeline_create_info.renderPass = current_render_pass_;
    graphics_pipeline_create_info.subpass = 0;
    graphics_pipeline_create_info.basePipelineHandle = VK_NULL_HANDLE;
    graphics_pipeline_create_info.basePipelineIndex = 0;

    VkPipeline pipeline = VK_NULL_HANDLE;
    const size_t h = std::hash<VkGraphicsPipelineCreateInfo>{}(
        graphics_pipeline_create_info);
    auto res = pipeline_cache_.find(h);
    if (res == pipeline_cache_.end()) {
      DLOG << name_ << ": creating new graphics pipeline" << '\n';
      CHECK_VK(vkCreateGraphicsPipelines(core_.GetLogicalDevice().GetHandle(),
                                         nullptr, 1, &graphics_pipeline_create_info,
                                         nullptr, &pipeline));
      pipeline_cache_[h] = pipeline;
    } else {
      pipeline = res->second;
    }
    if (pipeline != current_pipeline_) {
      vkCmdBindPipeline(gc_cmd_buffer_[image_index_], VK_PIPELINE_BIND_POINT_GRAPHICS,
                        pipeline);
      current_pipeline_ = pipeline;
      const VkPipelineLayout next_pipeline_layout = %s_pipeline_layout_;
      const int first_disturbed_set = pipeline_layout_lcp_.at(
          std::make_pair(current_pipeline_layout_, next_pipeline_layout));
      for (int i = first_disturbed_set; i < kDescriptorSetRegisterCount; ++i) {
        ClearDescriptorSetRegister(i);
      }
      current_pipeline_layout_ = next_pipeline_layout;
    }
}|}
            shader_stages vertex_input_state_create_info
            (if p.gp_depth_test_enabled then "VK_TRUE" else "VK_FALSE")
            (if p.gp_depth_write_enabled then "VK_TRUE" else "VK_FALSE")
            p.gp_depth_compare_op num_color_attachments color_blend_attachments
            p.gp_name p.gp_name))

let collect_atom_bindings loc = function
  | [ (_, L.{ value = Ast.Call (L.{ value = Ast.Id _; _ }, args); _ }) ] ->
      Ok
        (List.map
           (function
             | L.{ value = Ast.NamedArg (lhs, rhs); _ } -> (lhs, rhs)
             | expr ->
                 failwith
                   ( "unsupported pipeline atom binding: "
                   ^ Ast.string_of_expression expr ))
           args)
  | _ ->
      error loc
        (`Unsupported "invalid right hand side of pipeline write statement")

let gen_sampler2D_binding_write set binding count name t wrapped_name =
  let data =
    Printf.sprintf "data%s"
      (if String.length wrapped_name = 0 then "" else "." ^ wrapped_name)
  in
  let comment =
    Printf.sprintf "// SET=%d BINDING=%d COUNT=%d NAME=%s T=%s" set binding
      count name (Type.string_of_type t)
  in
  Printf.sprintf "%s\nBindSampledImage2D(set, %d, %s, images);" comment binding
    data

let gen_samplerCube_binding_write set binding count name t wrapped_name =
  let data =
    Printf.sprintf "data%s"
      (if String.length wrapped_name = 0 then "" else "." ^ wrapped_name)
  in
  let comment =
    Printf.sprintf "// SET=%d BINDING=%d COUNT=%d NAME=%s T=%s" set binding
      count name (Type.string_of_type t)
  in
  Printf.sprintf "%s\nBindCubeMap(set, %d, %s, images);" comment binding data

let gen_ubo_binding_write set binding count name t wrapped_name =
  let data =
    Printf.sprintf "data%s"
      (if String.length wrapped_name = 0 then "" else "." ^ wrapped_name)
  in
  let comment =
    Printf.sprintf "// SET=%d BINDING=%d COUNT=%d NAME=%s T=%s" set binding
      count name (Type.string_of_type t)
  in
  Printf.sprintf
    {|%s
    { 
      const VkDeviceSize size = sizeof(%s);
      while (ubo_buffer_pool_->LargestBlock() < size) {
        const UID oldest_uid = ubo_lru_.Pop();
        UniformResource oldest_resource = std::move(ubo_cache_.at(oldest_uid));
        ubo_cache_.erase(oldest_uid);
        for (const auto block : oldest_resource.blocks) {
          ubo_buffer_pool_->Free(block);
        }
      }
      zrl::Block block = ubo_buffer_pool_->Alloc(size);
      CHECK_PC(block != zrl::kEmptyBlock, "out of memory for UBO");
      ubo_buffer_pool_->Write(block.second, size, &%s);
      blocks.push_back(block);

      VkDescriptorBufferInfo buffer_info = {};
      buffer_info.buffer = ubo_buffer_pool_->GetHandle();
      buffer_info.offset = block.second;
      buffer_info.range = size; 

      VkWriteDescriptorSet write = {};
      write.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
      write.pNext = nullptr;
      write.dstSet = set;
      write.dstBinding = %d;
      write.dstArrayElement = 0;
      write.descriptorCount = 1;
      write.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
      write.pImageInfo = nullptr;
      write.pBufferInfo = &buffer_info;
      write.pTexelBufferView = nullptr;

      vkUpdateDescriptorSets(core_.GetLogicalDevice().GetHandle(),
                             1, &write, 0, nullptr);
    }|}
    comment (zrl_to_cpp_type t) data binding

let gen_uniform_atom_binding env pipeline id expr f =
  let open Cpp in
  let set_type =
    match pipeline.gp_declaration.pd_type with
    | Type.Function (params, _) ->
        let _, t = List.find (fun (name, _) -> name = id) params in
        t
    | _ -> failwith "pipelines must be of Function type"
  in
  let set = MapString.find id pipeline.gp_uniform_set in
  let set_layout =
    Printf.sprintf "%s_%d_descriptor_set_layout_" pipeline.gp_name set
  in
  let bindings =
    List.find_all (fun (s, _, _, _, _, _) -> s = set) pipeline.gp_uniforms
  in
  let binding_writes =
    String.concat "\n"
      (List.map
         (fun (set, binding, count, name, t, wrapped_name) ->
           match t with
           | Type.Sampler 1 ->
               failwith "TODO(ale64bit): binding sampler1D not supported yet"
           | Type.Sampler 2 ->
               gen_sampler2D_binding_write set binding count name t
                 wrapped_name
           | Type.Sampler 3 ->
               failwith "TODO(ale64bit): binding sampler3D not supported yet"
           | Type.SamplerCube ->
               gen_samplerCube_binding_write set binding count name t
                 wrapped_name
           | Type.Array (Sampler _, _) ->
               failwith
                 "TODO(ale64bit): binding array of samplers not supported yet"
           | _ -> gen_ubo_binding_write set binding count name t wrapped_name)
         bindings)
  in
  let can_cache =
    List.for_all
      (fun (_, _, _, _, t, _) ->
        not (has_opaque_members env SetString.empty t))
      bindings
  in
  let trait_id = Printf.sprintf "%s_%s" pipeline.gp_name id in
  let bind_comment =
    Printf.sprintf "// BIND UNIFORM: pipeline=%s uniform=%s expr='%s' set=%d"
      pipeline.gp_name id
      (Ast.string_of_expression expr)
      set
  in
  Function.(
    f
    |> append_code_section
         (Printf.sprintf
            {|%s
    {
      const auto &atom = %s;
      uint32_t uid = 0;
      constexpr bool can_cache = %s;
      %s data;
      %s<std::remove_const_t<
         std::remove_reference_t<
         decltype(atom)>>>{}(atom, uid, nullptr);
      const int set_index = %d;
      const VkDescriptorSetLayout set_layout = %s;
      DescriptorSetRegister &reg = descriptor_set_registers_[set_index];

      if (uid == 0) { // Resource is transient.
        %s<std::remove_const_t<
           std::remove_reference_t<
           decltype(atom)>>>{}(atom, uid, &data);
        if (can_cache && reg.in_use && reg.cached && 
            std::memcmp(reg.data, &data, sizeof(data)) == 0) {
          // Already bound with correct data.
        } else {
          ClearDescriptorSetRegister(set_index);
          const VkDescriptorSet set = RecycleOrAllocateDescriptorSet(set_layout);
          std::vector<zrl::Block> &blocks = ubo_discarded_blocks_[image_index_];
          std::vector<std::unique_ptr<zrl::Image>> &images = discarded_images_[image_index_];
          %s
          discarded_descriptor_sets_[image_index_][set_layout].push_back(set);
          reg.uid = uid;
          reg.in_use = true;
          if (can_cache && blocks.size() == 1 && sizeof(data) < kDescriptorSetRegisterSize) {
            reg.cached = true;
            std::memcpy(reg.data, &data, sizeof(data));
          } else {
            reg.cached = false;
          }
          // Bind the descriptor set.
          vkCmdBindDescriptorSets(gc_cmd_buffer_[image_index_], 
                                  VK_PIPELINE_BIND_POINT_GRAPHICS,
                                  current_pipeline_layout_,
                                  set_index, 1, &set, 0, nullptr);
        }
      } else {
        ClearDescriptorSetRegister(set_index);
        auto it = ubo_cache_.find(uid);
        if (it == ubo_cache_.end()) {
          %s<std::remove_const_t<
             std::remove_reference_t<
             decltype(atom)>>>{}(atom, uid, &data);
          const VkDescriptorSet set = RecycleOrAllocateDescriptorSet(set_layout);
          std::vector<zrl::Block> blocks;
          std::vector<std::unique_ptr<zrl::Image>> images;
          %s
          ubo_lru_.Push(uid);
          ubo_cache_.emplace(uid, 
              UniformResource{uid, set, set_layout, blocks, std::move(images)});
          reg.uid = uid;
          reg.in_use = true;
          reg.cached = false;
          // Bind the descriptor set.
          vkCmdBindDescriptorSets(gc_cmd_buffer_[image_index_], 
                                  VK_PIPELINE_BIND_POINT_GRAPHICS,
                                  current_pipeline_layout_,
                                  set_index, 1, &set, 0, nullptr);
        } else {
          const VkDescriptorSet set = it->second.set;
          ubo_lru_.Push(uid);
          reg.uid = uid;
          reg.in_use = true;
          reg.cached = false;
          // Bind the descriptor set.
          vkCmdBindDescriptorSets(gc_cmd_buffer_[image_index_], 
                                  VK_PIPELINE_BIND_POINT_GRAPHICS,
                                  current_pipeline_layout_,
                                  set_index, 1, &set, 0, nullptr);
        }
      }

    }|}
            bind_comment
            (gen_cpp_expression env expr)
            (string_of_bool can_cache) (zrl_to_cpp_type set_type) trait_id set
            set_layout trait_id binding_writes trait_id binding_writes))

let gen_indices_atom_binding env pipeline expr f =
  let open Cpp in
  let bind_comment =
    Printf.sprintf "// BIND INDICES: pipeline=%s expr='%s'" pipeline.gp_name
      (Ast.string_of_expression expr)
  in
  let trait_id = Printf.sprintf "%s_indices_" pipeline.gp_name in
  Function.(
    f
    |> append_code_section
         (Printf.sprintf
            {|%s
    {
      if (index_count == 0) {
        using UID = uint32_t;
        const auto &atom = %s;
        UID uid = 0;
        const void *src = nullptr;
        VkIndexType index_type = VK_INDEX_TYPE_NONE_NV;
        %s<std::remove_const_t<
           std::remove_reference_t<
           decltype(atom)>>>{}(atom, uid, &src, index_type, index_count);

        if (src != nullptr) {
          CHECK_PC(index_type == VK_INDEX_TYPE_UINT16 ||
                   index_type == VK_INDEX_TYPE_UINT32, "invalid index type");
          const size_t size = (index_type == VK_INDEX_TYPE_UINT16 ? 2 : 4) * index_count;
          // Find an appropriate block for the index buffer.
          zrl::Block block = zrl::kEmptyBlock;
          if (uid == 0) { // This index buffer is transient.
            // Make room for new buffer.
            while (vb_buffer_pool_->LargestBlock() < size) {
              const UID oldest_uid = vb_lru_.Pop();
              const zrl::Block oldest_block = vb_cache_.at(oldest_uid);
              vb_cache_.erase(oldest_uid);
              vb_buffer_pool_->Free(oldest_block);
            }
            block = vb_buffer_pool_->Alloc(size);
            CHECK_PC(block != zrl::kEmptyBlock, "out of memory for index buffer");
            vb_discarded_blocks_[image_index_].push_back(block);
            // Schedule a buffer copy.
            VkBufferCopy buffer_copy = {};
            buffer_copy.srcOffset = staging_buffer_->PushData(size, src);
            buffer_copy.dstOffset = block.second;
            buffer_copy.size = size;
            vb_pending_copies_.push_back(buffer_copy);
          } else {
            auto it = vb_cache_.find(uid);
            if (it == vb_cache_.end()) { // Index buffer is not cached.
              // Make room for new buffer.
              while (vb_buffer_pool_->LargestBlock() < size) {
                const UID oldest_uid = vb_lru_.Pop();
                const zrl::Block oldest_block = vb_cache_.at(oldest_uid);
                vb_cache_.erase(oldest_uid);
                vb_buffer_pool_->Free(oldest_block);
              }
              block = vb_buffer_pool_->Alloc(size);
              CHECK_PC(block != zrl::kEmptyBlock, "out of memory for index buffer");
              vb_lru_.Push(uid);
              vb_cache_[uid] = block;
              // Schedule a buffer copy.
              VkBufferCopy buffer_copy = {};
              buffer_copy.srcOffset = staging_buffer_->PushData(size, src);
              buffer_copy.dstOffset = block.second;
              buffer_copy.size = size;
              vb_pending_copies_.push_back(buffer_copy);
            } else {
              vb_lru_.Push(uid);
              block = it->second;
              // No need to schedule copy. Buffer is already available to device.
            }
          }

          const VkBuffer buffer = vb_buffer_pool_->GetHandle();
          vkCmdBindIndexBuffer(gc_cmd_buffer_[image_index_], buffer, 
                               block.second, index_type);
        }
      }
    }|}
            bind_comment
            (gen_cpp_expression env expr)
            trait_id))

let gen_input_atom_binding env pipeline id expr f =
  let open Cpp in
  let location, _, t =
    List.find (fun (_, name, _) -> name = id) pipeline.gp_inputs
  in
  let bind_comment =
    Printf.sprintf "// BIND INPUT: pipeline=%s input=%s expr='%s'"
      pipeline.gp_name id
      (Ast.string_of_expression expr)
  in
  let trait_id = Printf.sprintf "%s_%s" pipeline.gp_name id in
  Function.(
    f
    |> append_code_section
         (Printf.sprintf
            {|%s
    {
      using UID = uint32_t;
      const auto &atom = %s;
      UID uid = 0;
      size_t size = 0;
      const void *src = nullptr;
      %s<std::remove_const_t<
         std::remove_reference_t<
         decltype(atom)>>>{}(atom, uid, &src, size);

      // Find an appropriate block for the vertex buffer.
      zrl::Block block = zrl::kEmptyBlock;
      if (size == 0) {
        block = dummy_block_;
      } else if (uid == 0) { // This vertex buffer is transient.
        // Make room for new buffer.
        while (vb_buffer_pool_->LargestBlock() < size) {
          const UID oldest_uid = vb_lru_.Pop();
          const zrl::Block oldest_block = vb_cache_.at(oldest_uid);
          vb_cache_.erase(oldest_uid);
          vb_buffer_pool_->Free(oldest_block);
        }
        block = vb_buffer_pool_->Alloc(size);
        CHECK_PC(block != zrl::kEmptyBlock, "out of memory for vertex buffer");
        vb_discarded_blocks_[image_index_].push_back(block);
        // Schedule a buffer copy.
        VkBufferCopy buffer_copy = {};
        buffer_copy.srcOffset = staging_buffer_->PushData(size, src);
        buffer_copy.dstOffset = block.second;
        buffer_copy.size = size;
        vb_pending_copies_.push_back(buffer_copy);
      } else {
        auto it = vb_cache_.find(uid);
        if (it == vb_cache_.end()) { // Vertex buffer is not cached.
          // Make room for new buffer.
          while (vb_buffer_pool_->LargestBlock() < size) {
            const UID oldest_uid = vb_lru_.Pop();
            const zrl::Block oldest_block = vb_cache_.at(oldest_uid);
            vb_cache_.erase(oldest_uid);
            vb_buffer_pool_->Free(oldest_block);
          }
          block = vb_buffer_pool_->Alloc(size);
          CHECK_PC(block != zrl::kEmptyBlock, "out of memory for vertex buffer");
          vb_lru_.Push(uid);
          vb_cache_[uid] = block;
          // Schedule a buffer copy.
          VkBufferCopy buffer_copy = {};
          buffer_copy.srcOffset = staging_buffer_->PushData(size, src);
          buffer_copy.dstOffset = block.second;
          buffer_copy.size = size;
          vb_pending_copies_.push_back(buffer_copy);
        } else {
          vb_lru_.Push(uid);
          block = it->second;
          // No need to schedule copy. Buffer is already available to device.
        }
      }

      const VkBuffer buffer = vb_buffer_pool_->GetHandle();
      vkCmdBindVertexBuffers(gc_cmd_buffer_[image_index_], %d, 1, &buffer, &block.second);

      if (vertex_count == 0) {
        vertex_count = size / sizeof(%s);
      }
    }|}
            bind_comment
            (gen_cpp_expression env expr)
            trait_id location (zrl_to_cpp_type t)))

let gen_atom_bindings env pipeline f bindings =
  let is_input id =
    List.exists (fun (_, name, _) -> id = name) pipeline.gp_inputs
  in
  let is_uniform id = MapString.mem id pipeline.gp_uniform_set in
  let _, all_inputs =
    List.split (List.filter (fun (id, _) -> is_input id) bindings)
  in
  let unique_inputs =
    List.sort_uniq
      (fun e1 e2 ->
        String.compare (gen_cpp_expression env e1) (gen_cpp_expression env e2))
      all_inputs
  in
  let f =
    List.fold_left
      (flip (gen_indices_atom_binding env pipeline))
      f unique_inputs
  in
  List.fold_left
    (fun f (lhs, rhs) ->
      if is_input lhs then gen_input_atom_binding env pipeline lhs rhs f
      else if is_uniform lhs then
        gen_uniform_atom_binding env pipeline lhs rhs f
      else failwith "each atom binding must be an input or uniform")
    f bindings

let check_all_inputs_are_bound loc pipeline bindings =
  List.fold_left
    (fun acc (_, name, _) ->
      acc >>= fun () ->
      let is_bound = List.exists (fun (id, _) -> id = name) bindings in
      if is_bound then Ok ()
      else error loc (`MissingInputBinding (pipeline.gp_name, name)))
    (Ok ()) pipeline.gp_inputs

let gen_write env loc pipeline lhs rhs f =
  check_at_most_one_depth_buffer loc lhs >>= fun () ->
  collect_atom_bindings loc rhs >>= fun bindings ->
  check_all_inputs_are_bound loc pipeline bindings >>= fun () ->
  let rt_exprs =
    String.concat ", "
      (List.map (fun (_, lhs) -> gen_cpp_expression env lhs) lhs)
  in
  let f =
    Cpp.Function.(
      f
      |> append_code_section
           (Printf.sprintf
              {|{ // WRITE
            uint32_t vertex_count = 0;
            uint32_t index_count = 0;
            {  
            const std::vector<RenderTargetReference*> rts = {%s};
            for (auto rt : rts) { rt->target_layout = VK_IMAGE_LAYOUT_GENERAL; }
            VkRenderPass render_pass = GetOrCreateRenderPass(rts);
            VkFramebuffer framebuffer = GetOrCreateFramebuffer(rts, render_pass);
            for (auto rt : rts) { 
              if (rt->load_op == VK_ATTACHMENT_LOAD_OP_CLEAR) {
                rt->load_op = VK_ATTACHMENT_LOAD_OP_LOAD;
              }
            }
            if (current_render_pass_ != render_pass) {
              if (current_render_pass_ != VK_NULL_HANDLE) {
                vkCmdEndRenderPass(gc_cmd_buffer_[image_index_]);
              }
              std::vector<VkClearValue> clear_values(rts.size());
              for (size_t i = 0; i < clear_values.size(); ++i) {
                clear_values[i] = rts[i]->clear_value;
              }
              VkRenderPassBeginInfo begin_info = {};
              begin_info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
              begin_info.pNext = nullptr;
              begin_info.renderPass = render_pass;
              begin_info.framebuffer = framebuffer;
              begin_info.renderArea.offset = {0, 0};
              begin_info.renderArea.extent = core_.GetSwapchain().GetExtent();
              begin_info.clearValueCount = static_cast<uint32_t>(clear_values.size());
              begin_info.pClearValues = clear_values.data();
              vkCmdBeginRenderPass(gc_cmd_buffer_[image_index_], &begin_info, 
                                   VK_SUBPASS_CONTENTS_INLINE);
              current_render_pass_ = render_pass;
              for (auto rt : rts) { rt->current_layout = rt->target_layout; }
            }|}
              rt_exprs))
  in
  let f = gen_create_and_bind_pipeline env pipeline f lhs in
  let f = gen_atom_bindings env pipeline f bindings in
  Ok
    Cpp.Function.(
      f
      |> append_code_section
           {|
           if (index_count > 0) {
             vkCmdDrawIndexed(gc_cmd_buffer_[image_index_], index_count, 1, 0, 0, 0);
           } else {
             vkCmdDraw(gc_cmd_buffer_[image_index_], vertex_count, 1, 0, 0);
           }
         |}
      |> append_code_section "}}")

let extract_pipeline_name_from_write = function
  | _, L.{ value = Ast.Call (L.{ value = Ast.Id id; _ }, _); _ } -> id
  | _ -> failwith "unexpected right-hand side in pipeline write statement"

let gen_clear_or_write env loc pipelines lhs op rhs f =
  match op with
  | Ast.Assign -> gen_clear env loc lhs rhs f
  | Ast.AssignPlus ->
      let () = assert (List.length rhs = 1) in
      let pid = extract_pipeline_name_from_write (List.hd rhs) in
      let p = MapString.find pid pipelines in
      gen_write env loc p lhs rhs f
  | _ ->
      error loc
        (`Unsupported
          (Printf.sprintf "unexpected operator %s" (Ast.string_of_assignop op)))

let rec gen_cpp_stmt pipelines stmt f =
  let open Cpp in
  let open TypedAst in
  let env, L.{ value = stmt; loc } = stmt in
  match stmt with
  | CallExpr (id, args) ->
      let arg_exprs =
        String.concat ", "
          (List.map (fun (_, expr) -> gen_cpp_expression env expr) args)
      in
      Ok
        Function.(
          f
          |> append_code_section
               (Printf.sprintf "%s(%s);" (gen_cpp_builtin_call_id id) arg_exprs))
  | Var { bind_ids; bind_values } | Val { bind_ids; bind_values } ->
      Ok
        ( match (bind_ids, bind_values) with
        | [ id ], [ ([ typ ], value) ] ->
            let decl =
              Printf.sprintf "%s %s = %s;" (zrl_to_cpp_type typ) id
                (gen_cpp_expression env value)
            in
            Function.(f |> append_code_section decl)
        | ids, [ (types, value) ] ->
            let bindings = List.combine ids types in
            let f =
              List.fold_left
                (fun f (id, typ) ->
                  let decl =
                    Printf.sprintf "%s %s;" (zrl_to_cpp_type typ) id
                  in
                  Function.(f |> append_code_section decl))
                f bindings
            in
            let assignment =
              Printf.sprintf "std::tie(%s) = %s;" (String.concat ", " ids)
                (gen_cpp_expression env value)
            in
            Function.(f |> append_code_section assignment)
        | ids, values ->
            let bindings = List.combine ids values in
            List.fold_left
              (fun f (id, (t, value)) ->
                let decl =
                  Printf.sprintf "%s %s = %s;"
                    (zrl_to_cpp_type (List.hd t))
                    id
                    (gen_cpp_expression env value)
                in
                Function.(f |> append_code_section decl))
              f bindings )
  | Assignment { asg_op; asg_lvalues; asg_rvalues } -> (
      if is_rt_clear_or_write asg_op asg_lvalues then
        gen_clear_or_write env loc pipelines asg_lvalues asg_op asg_rvalues f
      else
        match (asg_lvalues, asg_rvalues) with
        | [ (_, lhs) ], [ ([ _ ], rhs) ] ->
            let decl =
              Printf.sprintf "%s %s %s;"
                (gen_cpp_expression env lhs)
                (Ast.string_of_assignop asg_op)
                (gen_cpp_expression env rhs)
            in
            Ok Function.(f |> append_code_section decl)
        | lhs, [ (_, rhs) ] ->
            let lvalues =
              String.concat ", "
                (List.map (fun (_, expr) -> gen_cpp_expression env expr) lhs)
            in
            let assignment =
              Printf.sprintf "std::tie(%s) = %s;" lvalues
                (gen_cpp_expression env rhs)
            in
            Ok Function.(f |> append_code_section assignment)
        | lhs, rhs ->
            let bindings = List.combine lhs rhs in
            Ok
              (List.fold_left
                 (fun f ((_, lhs), (_, rhs)) ->
                   let assignment =
                     Printf.sprintf "%s %s %s;"
                       (gen_cpp_expression env lhs)
                       (Ast.string_of_assignop asg_op)
                       (gen_cpp_expression env rhs)
                   in
                   Function.(f |> append_code_section assignment))
                 f bindings) )
  | If { if_cond = _, cond_expr; if_true; if_false } ->
      let cond =
        Printf.sprintf "if (%s) {" (gen_cpp_expression env cond_expr)
      in
      let f = Function.(f |> append_code_section cond) in
      List.fold_left
        (fun acc stmt -> acc >>= gen_cpp_stmt pipelines stmt)
        (Ok f) if_true
      >>= fun f ->
      let f = Function.(f |> append_code_section "} else {") in
      List.fold_left
        (fun acc stmt -> acc >>= gen_cpp_stmt pipelines stmt)
        (Ok f) if_false
      >>= fun f -> Ok Function.(f |> append_code_section "}")
  | ForIter { foriter_id; foriter_it = _, it_expr; foriter_body } ->
      let header =
        Printf.sprintf "for (const auto &%s : %s) {" foriter_id
          (gen_cpp_expression env it_expr)
      in
      let f = Function.(f |> append_code_section header) in
      List.fold_left
        (fun acc stmt -> acc >>= gen_cpp_stmt pipelines stmt)
        (Ok f) foriter_body
      >>= fun f -> Ok Function.(f |> append_code_section "}")
  | ForRange
      {
        forrange_id;
        forrange_from = _, from_expr;
        forrange_to = _, to_expr;
        forrange_body;
      } ->
      let header =
        Printf.sprintf "for (int %s = (%s); i <= (%s); ++%s) {" forrange_id
          (gen_cpp_expression env from_expr)
          (gen_cpp_expression env to_expr)
          forrange_id
      in
      let f = Function.(f |> append_code_section header) in
      List.fold_left
        (fun acc stmt -> acc >>= gen_cpp_stmt pipelines stmt)
        (Ok f) forrange_body
      >>= fun f -> Ok Function.(f |> append_code_section "}")
  | Return [ (_, expr) ] ->
      let return = Printf.sprintf "return %s;" (gen_cpp_expression env expr) in
      Ok Function.(f |> append_code_section return)
  | Return exprs ->
      let return =
        Printf.sprintf "return std::make_tuple(%s);"
          (String.concat ", "
             (List.map (fun (_, expr) -> gen_cpp_expression env expr) exprs))
      in
      Ok Function.(f |> append_code_section return |> append_code_section "}")
  | Discard ->
      error loc
        (`Unsupported
          "'discard' statement cannot be used outside fragment shaders")

let gen_cpp_function pipelines TypedAst.{ fd_name; fd_type; fd_body; _ } =
  let open Cpp in
  let f = Function.empty fd_name in
  let ret_type =
    match fd_type with
    | Type.Function (_, []) -> "void"
    | Type.Function (_, [ ret ]) -> zrl_to_cpp_type ret
    | Type.Function (_, rets) ->
        Printf.sprintf "std::tuple<%s>"
          (String.concat ", " (List.map zrl_to_cpp_type rets))
    | t ->
        failwith
          (Printf.sprintf "C++ function must have Function type but got '%s'"
             (Type.string_of_type t))
  in
  let f = Function.set_return_type ret_type f in
  let f =
    match fd_type with
    | Type.Function (params, _) ->
        List.fold_left
          (fun f (pname, t) ->
            match t with
            | Type.Primitive _ | Type.Vector _ | Type.Matrix _ ->
                Function.(f |> add_param (zrl_to_cpp_type t, pname))
            | Type.Atom Singleton ->
                let tmpl_param = pname ^ "AtomType" in
                let param = (Printf.sprintf "const %s&" tmpl_param, pname) in
                Function.(
                  f
                  |> add_template_param ("class " ^ tmpl_param)
                  |> add_param param)
            | Type.Atom Set ->
                let tmpl_param = pname ^ "AtomType" in
                let param =
                  ( Printf.sprintf "const std::unordered_set<%s>&" tmpl_param,
                    pname )
                in
                Function.(
                  f
                  |> add_template_param ("class " ^ tmpl_param)
                  |> add_param param)
            | Type.Atom List ->
                let tmpl_param = pname ^ "AtomType" in
                let param =
                  (Printf.sprintf "const std::vector<%s>&" tmpl_param, pname)
                in
                Function.(
                  f
                  |> add_template_param ("class " ^ tmpl_param)
                  |> add_param param)
            | Type.RenderTarget _ ->
                Function.(f |> add_param ("RenderTargetReference*", pname))
            | Type.TypeRef _ ->
                let cr = "const " ^ zrl_to_cpp_type t ^ "&" in
                Function.(f |> add_param (cr, pname))
            | _ ->
                failwith
                  (Printf.sprintf "cannot use type %s as renderer argument"
                     (Type.string_of_type t)))
          f params
    | _ -> failwith "renderer functions must be of Function type"
  in
  List.fold_left
    (fun acc stmt -> acc >>= gen_cpp_stmt pipelines stmt)
    (Ok f) fd_body

let gen_renderer_code pipelines rd_functions r =
  let open RendererEnv in
  List.fold_left
    (fun acc fd ->
      acc >>= fun r ->
      gen_cpp_function pipelines fd >>= fun f ->
      let rclass = Cpp.Class.(r.rclass |> add_private_function f) in
      Ok { r with rclass })
    (Ok r) rd_functions

let gen_render_targets rd_type r =
  let open Cpp in
  match rd_type with
  | Type.Function (params, _) ->
      List.fold_left
        (fun acc (pname, t) ->
          acc >>= fun r ->
          match t with
          | Type.RenderTarget ((RGBA | DS) as rtt) ->
              let img_id = pname ^ "_image_" in
              let ref_id = pname ^ "_ref_" in
              let img_member = ("zrl::Image", img_id) in
              let ref_member = ("RenderTargetReference", ref_id) in
              let ptr_member = ("RenderTargetReference *", pname) in
              let rt_type, format =
                if rtt = RGBA then
                  (RendererEnv.Color, "core_.GetSwapchain().GetSurfaceFormat()")
                else if rtt = DS then
                  (RendererEnv.DepthStencil, "VK_FORMAT_D16_UNORM")
                else failwith "unsupported render target type"
              in
              let img_ctor_args =
                match rt_type with
                | RendererEnv.Color ->
                    "core_, ExpandExtent(core_.GetSwapchain().GetExtent()), \
                     1, 1, core_.GetSwapchain().GetSurfaceFormat(), \
                     VK_IMAGE_TYPE_2D, VK_IMAGE_VIEW_TYPE_2D, \
                     VK_IMAGE_TILING_OPTIMAL, \
                     VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | \
                     VK_IMAGE_USAGE_SAMPLED_BIT, \
                     VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, \
                     VK_IMAGE_ASPECT_COLOR_BIT"
                | RendererEnv.DepthStencil ->
                    "core_, ExpandExtent(core_.GetSwapchain().GetExtent()), \
                     1, 1, VK_FORMAT_D16_UNORM, VK_IMAGE_TYPE_2D, \
                     VK_IMAGE_VIEW_TYPE_2D, VK_IMAGE_TILING_OPTIMAL, \
                     VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT | \
                     VK_IMAGE_USAGE_SAMPLED_BIT, \
                     VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, \
                     VK_IMAGE_ASPECT_DEPTH_BIT"
              in
              let ref_ctor_args =
                Printf.sprintf
                  "{%s, %s.GetViewHandle(), %s.GetHandle(), \
                   VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_UNDEFINED, \
                   VK_ATTACHMENT_LOAD_OP_DONT_CARE, {}}"
                  format img_id img_id
              in
              let r =
                RendererEnv.
                  {
                    r with
                    render_targets =
                      MapString.(r.render_targets |> add pname rt_type);
                    rclass =
                      Class.(
                        r.rclass
                        |> add_private_member img_member
                        |> add_private_member ref_member
                        |> add_private_member ptr_member);
                    ctor =
                      Function.(
                        r.ctor
                        |> add_member_initializer (img_id, img_ctor_args)
                        |> add_member_initializer (ref_id, ref_ctor_args)
                        |> add_member_initializer (pname, "nullptr"));
                    render =
                      Function.(
                        r.render
                        |> append_code_section
                             (Printf.sprintf "%s = &%s;" pname ref_id)
                        |> append_code_section
                             (Printf.sprintf
                                "%s->current_layout = \
                                 VK_IMAGE_LAYOUT_UNDEFINED;"
                                pname)
                        |> append_code_section
                             (Printf.sprintf
                                "%s->load_op = VK_ATTACHMENT_LOAD_OP_DONT_CARE;"
                                pname));
                  }
              in
              Ok r
          | _ -> acc)
        (Ok r) params
  | _ -> failwith "renderer must be of Function type"

let gen_shader_modules glsl_libraries r =
  let shaders = List.flatten (List.map Glsl.Library.shaders glsl_libraries) in
  let r =
    List.fold_left
      (fun r shader ->
        let stage_name =
          match Glsl.Shader.stage shader with
          | Vertex -> "vert"
          | Geometry -> "geom"
          | Fragment -> "frag"
          | Compute -> "comp"
        in
        let shader_name =
          Printf.sprintf "%s.%s" (Glsl.Shader.name shader) stage_name
        in
        let shader_variable_name =
          Printf.sprintf "k%s_%s_glsl_" (Glsl.Shader.name shader) stage_name
        in
        let module_member = shader_variable_name ^ "shader_module_" in
        let rclass =
          Cpp.Class.(
            r.RendererEnv.rclass
            |> add_include (Printf.sprintf {|"%s.glsl.h"|} shader_name)
            |> add_private_member ("VkShaderModule", module_member))
        in
        let ctor =
          Cpp.Function.(
            r.ctor
            |> add_member_initializer (module_member, "VK_NULL_HANDLE")
            |> append_code_section
                 (Printf.sprintf
                    {|  {
    VkShaderModuleCreateInfo create_info = {};
    create_info.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
    create_info.pNext = nullptr;
    create_info.flags = 0;
    create_info.codeSize = sizeof %s;
    create_info.pCode = %s;
    CHECK_VK(vkCreateShaderModule(core_.GetLogicalDevice().GetHandle(), &create_info,
                                  nullptr, &%s));
  }
|}
                    shader_variable_name shader_variable_name module_member))
        in
        let dtor =
          Cpp.Function.(
            r.dtor
            |> append_code_section
                 (Printf.sprintf
                    "vkDestroyShaderModule(core_.GetLogicalDevice().GetHandle(), \
                     %s, nullptr);"
                    module_member))
        in
        RendererEnv.{ r with rclass; ctor; dtor })
      r shaders
  in
  Ok r

let gen_descriptor_set_layouts p r =
  let sets = List.map (fun (set, _, _, _, _, _) -> set) p.gp_uniforms in
  let sets = List.sort_uniq ( - ) sets in
  List.fold_left
    (fun r set ->
      let descriptor_set_layout =
        Printf.sprintf "%s_%d_descriptor_set_layout_" p.gp_name set
      in
      let bindings =
        List.map
          (fun (_, binding, count, name, t, _) ->
            let descriptor_type =
              match t with
              | Type.Sampler _ | Type.SamplerCube | Type.Array (Sampler _, _)
                ->
                  "VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
              | _ -> "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER"
            in
            Printf.sprintf
              {|
                // %s
                bindings[%d].binding = %d;
                bindings[%d].descriptorType = %s;
                bindings[%d].descriptorCount = %d;
                bindings[%d].stageFlags = VK_SHADER_STAGE_ALL_GRAPHICS;
                bindings[%d].pImmutableSamplers = nullptr;
              |}
              name binding binding binding descriptor_type binding count
              binding binding)
          (List.find_all (fun (s, _, _, _, _, _) -> s = set) p.gp_uniforms)
      in
      let rclass =
        Cpp.Class.(
          r.RendererEnv.rclass
          |> add_private_member ("VkDescriptorSetLayout", descriptor_set_layout))
      in
      let ctor =
        Cpp.Function.(
          r.ctor
          |> add_member_initializer (descriptor_set_layout, "VK_NULL_HANDLE")
          |> append_code_section
               (Printf.sprintf
                  {|{ // SET %d
    std::array<VkDescriptorSetLayoutBinding, %d> bindings;

    %s

    VkDescriptorSetLayoutCreateInfo create_info = {};
    create_info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
    create_info.pNext = nullptr;
    create_info.flags = 0;
    create_info.bindingCount = static_cast<uint32_t>(bindings.size());
    create_info.pBindings = bindings.data();
    CHECK_VK(vkCreateDescriptorSetLayout(core_.GetLogicalDevice().GetHandle(), 
                                         &create_info, nullptr, &%s));
}|}
                  set (List.length bindings)
                  (String.concat "\n" bindings)
                  descriptor_set_layout))
      in
      let dtor =
        Cpp.Function.(
          r.dtor
          |> append_code_section
               (Printf.sprintf
                  "vkDestroyDescriptorSetLayout(core_.GetLogicalDevice().GetHandle(), \
                   %s, nullptr);"
                  descriptor_set_layout))
      in
      RendererEnv.{ r with rclass; ctor; dtor })
    r sets

let gen_pipeline_layout pipeline r =
  let pipeline_layout =
    Printf.sprintf "%s_pipeline_layout_" pipeline.gp_name
  in
  let sets =
    List.sort_uniq ( - )
      (List.map
         (fun (_, set) -> set)
         (MapString.bindings pipeline.gp_uniform_set))
  in
  let descriptor_set_layouts =
    List.rev
      (List.fold_left
         (fun layouts set ->
           let descriptor_set_layout =
             Printf.sprintf "%s_%d_descriptor_set_layout_" pipeline.gp_name set
           in
           descriptor_set_layout :: layouts)
         [] sets)
  in
  let rclass =
    Cpp.Class.(
      r.RendererEnv.rclass
      |> add_private_member ("VkPipelineLayout", pipeline_layout))
  in
  let ctor =
    Cpp.Function.(
      r.RendererEnv.ctor
      |> add_member_initializer (pipeline_layout, "VK_NULL_HANDLE")
      |> append_code_section
           (Printf.sprintf
              {|{
  std::array<VkDescriptorSetLayout, %d> descriptor_set_layouts = {%s};
  VkPipelineLayoutCreateInfo create_info = {};
  create_info.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
  create_info.pNext = nullptr;
  create_info.flags = 0;
  create_info.setLayoutCount = static_cast<uint32_t>(descriptor_set_layouts.size());
  create_info.pSetLayouts = descriptor_set_layouts.data();
  create_info.pushConstantRangeCount = 0;
  create_info.pPushConstantRanges = nullptr;
  CHECK_VK(vkCreatePipelineLayout(core_.GetLogicalDevice().GetHandle(), &create_info,
                                  nullptr, &%s));
}|}
              (List.length descriptor_set_layouts)
              (String.concat ", " descriptor_set_layouts)
              pipeline_layout))
  in
  let dtor =
    Cpp.Function.(
      r.dtor
      |> append_code_section
           (Printf.sprintf
              "vkDestroyPipelineLayout(core_.GetLogicalDevice().GetHandle(), \
               %s, nullptr);"
              pipeline_layout))
  in
  RendererEnv.{ r with rclass; ctor; dtor }

let gen_pipeline_members pipelines r =
  Ok
    (List.fold_left
       (fun r (_, pipeline) ->
         r
         |> gen_descriptor_set_layouts pipeline
         |> gen_pipeline_layout pipeline)
       r
       (MapString.bindings pipelines))

let gen_pipeline_layout_lcp_entries pipelines =
  let lcp xs ys =
    let rec aux acc xs ys =
      match (xs, ys) with
      | x :: xs, y :: ys -> if x = y then aux (acc + 1) xs ys else acc
      | _ -> acc
    in
    aux 0 xs ys
  in
  let layouts =
    List.map
      (fun (pname, p) ->
        match p.gp_declaration.pd_type with
        | Type.Function (params, _) ->
            let types = List.map (fun (_, t) -> t) params in
            (pname, types)
        | _ -> failwith "pipeline type must be a Function type")
      (MapString.bindings pipelines)
  in
  let pairs =
    List.flatten
      (List.map (fun x -> List.map (fun y -> (x, y)) layouts) layouts)
  in
  let null_entries =
    List.map
      (fun (pname, _) ->
        Printf.sprintf
          "pipeline_layout_lcp_[std::make_pair(reinterpret_cast<VkPipelineLayout>(VK_NULL_HANDLE), \
           %s_pipeline_layout_)] = 0;"
          pname)
      layouts
  in
  let entries =
    List.map
      (fun ((p1, t1), (p2, t2)) ->
        Printf.sprintf
          "pipeline_layout_lcp_[std::make_pair(%s_pipeline_layout_, \
           %s_pipeline_layout_)] = %d;"
          p1 p2 (lcp t1 t2))
      pairs
  in
  String.concat "\n" (null_entries @ entries)

let gen_pipeline_layout_lcp pipelines r =
  let entries = gen_pipeline_layout_lcp_entries pipelines in
  let ctor =
    Cpp.Function.(r.RendererEnv.ctor |> append_code_section entries)
  in
  Ok RendererEnv.{ r with ctor }

let gen_renderer_library pipelines glsl_libraries
    TypedAst.{ rd_name; rd_type; rd_functions; _ } =
  let open Cpp in
  let r = RendererEnv.empty rd_name in
  gen_shader_modules glsl_libraries r
  >>= gen_pipeline_members pipelines
  >>= gen_pipeline_layout_lcp pipelines
  >>= gen_render_targets rd_type
  >>= gen_renderer_code pipelines rd_functions
  >>= fun r ->
  let output_class = RendererEnv.export r in
  let lib = Library.(empty rd_name |> add_class output_class) in
  Ok lib

let gen_glsl_type td_name td_type =
  let open Type in
  match td_type with
  | Record fields ->
      let fields =
        String.concat "\n  "
          (List.map
             (fun (name, t) ->
               Printf.sprintf "%s %s;" (zrl_to_glsl_type t) name)
             fields)
      in
      Printf.sprintf "struct %s {\n%s\n}" td_name fields
  | _ ->
      failwith
        ( "cannot generate glsl type for non-record type: "
        ^ string_of_type td_type )

let gen_cpp_type loc td_name td_type =
  let open Type in
  match td_type with
  | Record fields ->
      let fields =
        String.concat "\n  "
          (List.map
             (fun (name, t) ->
               Printf.sprintf "%s %s;" (zrl_to_cpp_type t) name)
             fields)
      in
      Ok (Printf.sprintf "struct alignas(16) %s {\n%s\n};" td_name fields)
  | _ ->
      error loc
        (`Unsupported
          ( "cannot generate c++ type for non-Record type: "
          ^ string_of_type td_type ))

let descriptor_set_register_struct =
  {|struct DescriptorSetRegister {
  uint32_t uid = 0;
  bool in_use = false;
  bool cached = false;
  char data[kDescriptorSetRegisterSize]; 
};|}

let merge_hash_function =
  {|template <class T> inline void MergeHash(size_t &h1, const T h2) {
    h1 = h1*31 + static_cast<size_t>(h2);
}|}

let expand_extent_function =
  {|inline VkExtent3D ExpandExtent(VkExtent2D e) {
  return VkExtent3D{e.width, e.height, 1};
}|}

let uniform_resource_struct =
  {|struct UniformResource {
  const uint32_t uid = 0;
  const VkDescriptorSet set = VK_NULL_HANDLE;
  const VkDescriptorSetLayout layout = VK_NULL_HANDLE;
  const std::vector<zrl::Block> blocks;
  std::vector<std::unique_ptr<zrl::Image>> images;
};|}

let render_target_reference_struct =
  {|struct RenderTargetReference {
  const VkFormat format = VK_FORMAT_UNDEFINED;
  const VkImageView attachment = VK_NULL_HANDLE;
  const VkImage image = VK_NULL_HANDLE;
  VkImageLayout current_layout = VK_IMAGE_LAYOUT_UNDEFINED;
  VkImageLayout target_layout = VK_IMAGE_LAYOUT_UNDEFINED;
  VkAttachmentLoadOp load_op = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
  VkClearValue clear_value;
};|}

let sampled_image2D_reference_struct =
  {|struct SampledImage2DReference {
  VkSamplerCreateInfo sampler_create_info;
  RenderTargetReference *rt_ref = nullptr;
  VkFormat format = VK_FORMAT_UNDEFINED;
  VkDeviceSize size = 0;
  uint32_t width = 0;
  uint32_t height = 0;
  uint32_t channels = 0;
  std::vector<std::vector<unsigned char*>> image_data;
  bool build_mipmaps = false;
};|}

let sampled_cubemap_reference_struct =
  {|struct SampledCubeMapReference {
  VkSamplerCreateInfo sampler_create_info;
  VkFormat format = VK_FORMAT_UNDEFINED;
  VkDeviceSize size = 0;
  uint32_t width = 0;
  uint32_t height = 0;
  uint32_t channels = 0;
  std::array<std::vector<unsigned char*>, 6> image_data;
};|}

let render_pass_hash =
  {|struct RenderPassHash { 
  size_t operator()(const std::vector<RenderTargetReference> &rts) const noexcept {
    size_t h = 1;
    for (auto rt : rts) {
      MergeHash(h, rt.format);
      MergeHash(h, rt.current_layout);
      MergeHash(h, rt.target_layout);
      MergeHash(h, rt.load_op);
    }
    return h;
  }
};|}

let render_pass_equal_to =
  {|struct RenderPassEqualTo {
  bool operator()(const std::vector<RenderTargetReference> &a,
                  const std::vector<RenderTargetReference> &b) const noexcept {
    if (a.size() != b.size()) {
      return false;
    }
    for (size_t i = 0; i < a.size(); ++i) {
      if (a[i].format != b[i].format ||
          a[i].current_layout != b[i].current_layout ||
          a[i].target_layout != b[i].target_layout ||
          a[i].load_op != b[i].load_op) {
        return false;
      }
    }
    return true;
  }
};|}

let framebuffer_hash =
  {|struct FramebufferHash {
  size_t operator()(const std::vector<RenderTargetReference> &rts) const noexcept {
    size_t h = 1;
    for (auto rt : rts) {
      MergeHash(h, reinterpret_cast<size_t>(rt.attachment));
    }
    return h;
  }
};|}

let framebuffer_equal_to =
  {|struct FramebufferEqualTo {
  bool operator()(const std::vector<RenderTargetReference> &a,
                  const std::vector<RenderTargetReference> &b) const noexcept {
    if (a.size() != b.size()) {
      return false;
    }
    for (size_t i = 0; i < a.size(); ++i) {
      if (a[i].attachment != b[i].attachment) {
        return false;
      }
    }
    return true;
  }
};|}

let vk_descriptor_set_layout_binding_hash =
  {|namespace std {
template <> struct hash<VkDescriptorSetLayoutBinding> {
  size_t operator()(const VkDescriptorSetLayoutBinding &b) const noexcept {
    size_t h = 1;
    MergeHash(h, b.binding);
    MergeHash(h, b.descriptorType);
    MergeHash(h, b.descriptorCount);
    MergeHash(h, b.stageFlags);
    return h;
  }
};
}|}

let vk_descriptor_set_layout_create_info_hash =
  {|namespace std {
template <> struct hash<VkDescriptorSetLayoutCreateInfo> {
  size_t operator()(const VkDescriptorSetLayoutCreateInfo &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.bindingCount);
    for (uint32_t i = 0; i < info.bindingCount; ++i) {
      MergeHash(h, hash<VkDescriptorSetLayoutBinding>{}(info.pBindings[i]));
    }
    return h;
  }
};
}|}

let vk_pipeline_layout_create_info_hash =
  {|namespace std {
template <> struct hash<VkPipelineLayoutCreateInfo> {
  size_t operator()(const VkPipelineLayoutCreateInfo &info) const noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.setLayoutCount);
    for (uint32_t i = 0; i < info.setLayoutCount; ++i) {
      MergeHash(h, reinterpret_cast<size_t>(info.pSetLayouts[i]));
    }
    MergeHash(h, info.pushConstantRangeCount);
    for (uint32_t i = 0; i < info.pushConstantRangeCount; ++i) {
      MergeHash(h, info.pPushConstantRanges[i].stageFlags);
      MergeHash(h, info.pPushConstantRanges[i].offset);
      MergeHash(h, info.pPushConstantRanges[i].size);
    }
    return h;
  }
};
}|}

let vk_graphics_pipeline_create_info_hash =
  {|
namespace std {

template <> struct hash<VkPipelineShaderStageCreateInfo> {
  size_t operator()(const VkPipelineShaderStageCreateInfo &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.stage);
    MergeHash(h, reinterpret_cast<size_t>(info.module));
    return h;
  }
};
template <> struct hash<VkVertexInputBindingDescription> {
  size_t operator()(const VkVertexInputBindingDescription &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.binding);
    MergeHash(h, info.stride);
    MergeHash(h, info.inputRate);
    return h;
  }
};
template <> struct hash<VkVertexInputAttributeDescription> {
  size_t operator()(const VkVertexInputAttributeDescription &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.location);
    MergeHash(h, info.binding);
    MergeHash(h, info.format);
    MergeHash(h, info.offset);
    return h;
  }
};
template <> struct hash<VkPipelineVertexInputStateCreateInfo> {
  size_t operator()(const VkPipelineVertexInputStateCreateInfo &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.vertexBindingDescriptionCount);
    for (uint32_t i = 0; i < info.vertexBindingDescriptionCount; ++i) {
      MergeHash(h, hash<VkVertexInputBindingDescription>{}(
                       info.pVertexBindingDescriptions[i]));
    }
    MergeHash(h, info.vertexAttributeDescriptionCount);
    for (uint32_t i = 0; i < info.vertexAttributeDescriptionCount; ++i) {
      MergeHash(h, hash<VkVertexInputAttributeDescription>{}(
                       info.pVertexAttributeDescriptions[i]));
    }
    return h;
  }
};
template <> struct hash<VkPipelineInputAssemblyStateCreateInfo> {
  size_t operator()(const VkPipelineInputAssemblyStateCreateInfo &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.topology);
    MergeHash(h, info.primitiveRestartEnable);
    return h;
  }
};
template <> struct hash<VkPipelineTessellationStateCreateInfo> {
  size_t operator()(const VkPipelineTessellationStateCreateInfo &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.patchControlPoints);
    return h;
  }
};
template <> struct hash<VkPipelineViewportStateCreateInfo> {
  size_t operator()(const VkPipelineViewportStateCreateInfo &) const noexcept {
    size_t h = 1;
    // TODO ignore. This can be part of dynamic state.
    return h;
  }
};
template <> struct hash<VkPipelineRasterizationStateCreateInfo> {
  size_t operator()(const VkPipelineRasterizationStateCreateInfo &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.depthClampEnable);
    MergeHash(h, info.rasterizerDiscardEnable);
    MergeHash(h, info.polygonMode);
    MergeHash(h, info.cullMode);
    MergeHash(h, info.frontFace);
    MergeHash(h, info.depthBiasEnable);
    // TODO: MergeHash(h, info.depthBiasConstantFactor);
    // TODO: MergeHash(h, info.depthBiasClamp);
    // TODO: MergeHash(h, info.depthBiasSlopeFactor);
    // TODO: MergeHash(h, info.lineWidth);
    return h;
  }
};
template <> struct hash<VkPipelineMultisampleStateCreateInfo> {
  size_t operator()(const VkPipelineMultisampleStateCreateInfo &) const
      noexcept {
    size_t h = 1;
    // TODO: ignore for now.
    return h;
  }
};
template <> struct hash<VkStencilOpState> {
  size_t operator()(const VkStencilOpState &info) const noexcept {
    size_t h = 1;
    MergeHash(h, info.failOp);
    MergeHash(h, info.passOp);
    MergeHash(h, info.depthFailOp);
    MergeHash(h, info.compareOp);
    MergeHash(h, info.compareMask);
    MergeHash(h, info.writeMask);
    MergeHash(h, info.reference);
    return h;
  }
};
template <> struct hash<VkPipelineDepthStencilStateCreateInfo> {
  size_t operator()(const VkPipelineDepthStencilStateCreateInfo &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.depthTestEnable);
    MergeHash(h, info.depthWriteEnable);
    MergeHash(h, info.depthCompareOp);
    MergeHash(h, info.depthBoundsTestEnable);
    MergeHash(h, info.stencilTestEnable);
    MergeHash(h, hash<VkStencilOpState>{}(info.front));
    MergeHash(h, hash<VkStencilOpState>{}(info.back));
    MergeHash(h, info.minDepthBounds);
    MergeHash(h, info.maxDepthBounds);
    return h;
  }
};
template <> struct hash<VkPipelineColorBlendAttachmentState> {
  size_t operator()(const VkPipelineColorBlendAttachmentState &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.blendEnable);
    MergeHash(h, info.srcColorBlendFactor);
    MergeHash(h, info.dstColorBlendFactor);
    MergeHash(h, info.colorBlendOp);
    MergeHash(h, info.srcAlphaBlendFactor);
    MergeHash(h, info.dstAlphaBlendFactor);
    MergeHash(h, info.alphaBlendOp);
    MergeHash(h, info.colorWriteMask);
    return h;
  }
};
template <> struct hash<VkPipelineColorBlendStateCreateInfo> {
  size_t operator()(const VkPipelineColorBlendStateCreateInfo &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.logicOpEnable);
    MergeHash(h, info.logicOp);
    MergeHash(h, info.attachmentCount);
    for (uint32_t i = 0; i < info.attachmentCount; ++i) {
      MergeHash(
          h, hash<VkPipelineColorBlendAttachmentState>{}(info.pAttachments[i]));
    }
    MergeHash(h, info.blendConstants[0]);
    MergeHash(h, info.blendConstants[1]);
    MergeHash(h, info.blendConstants[2]);
    MergeHash(h, info.blendConstants[3]);
    return h;
  }
};
template <> struct hash<VkPipelineDynamicStateCreateInfo> {
  size_t operator()(const VkPipelineDynamicStateCreateInfo &info) const
      noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.dynamicStateCount);
    for (uint32_t i = 0; i < info.dynamicStateCount; ++i) {
      MergeHash(h, info.pDynamicStates[i]);
    }
    return h;
  }
};
template <> struct hash<VkGraphicsPipelineCreateInfo> {
  size_t operator()(const VkGraphicsPipelineCreateInfo &info) const noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.stageCount);
    for (uint32_t i = 0; i < info.stageCount; ++i) {
      MergeHash(h, hash<VkPipelineShaderStageCreateInfo>{}(info.pStages[i]));
    }
    if (info.pVertexInputState != nullptr) {
      MergeHash(h, hash<VkPipelineVertexInputStateCreateInfo>{}(
                       *info.pVertexInputState));
    }
    if (info.pInputAssemblyState != nullptr) {
      MergeHash(h, hash<VkPipelineInputAssemblyStateCreateInfo>{}(
                       *info.pInputAssemblyState));
    }
    if (info.pTessellationState != nullptr) {
      MergeHash(h, hash<VkPipelineTessellationStateCreateInfo>{}(
                       *info.pTessellationState));
    }
    if (info.pViewportState != nullptr) {
      MergeHash(
          h, hash<VkPipelineViewportStateCreateInfo>{}(*info.pViewportState));
    }
    if (info.pRasterizationState != nullptr) {
      MergeHash(h, hash<VkPipelineRasterizationStateCreateInfo>{}(
                       *info.pRasterizationState));
    }
    if (info.pMultisampleState != nullptr) {
      MergeHash(h, hash<VkPipelineMultisampleStateCreateInfo>{}(
                       *info.pMultisampleState));
    }
    if (info.pDepthStencilState != nullptr) {
      MergeHash(h, hash<VkPipelineDepthStencilStateCreateInfo>{}(
                       *info.pDepthStencilState));
    }
    if (info.pColorBlendState != nullptr) {
      MergeHash(h, hash<VkPipelineColorBlendStateCreateInfo>{}(
                       *info.pColorBlendState));
    }
    if (info.pDynamicState != nullptr) {
      MergeHash(h,
                hash<VkPipelineDynamicStateCreateInfo>{}(*info.pDynamicState));
    }
    MergeHash(h, reinterpret_cast<size_t>(info.layout));
    // MergeHash(h, reinterpret_cast<size_t>(info.renderPass));
    // MergeHash(h, reinterpret_cast<size_t>(info.subpass));
    return h;
  }
};
}|}

let vk_sampler_create_info_hash =
  {|namespace std {
template <> struct hash<VkSamplerCreateInfo> {
  size_t operator()(const VkSamplerCreateInfo &info) const noexcept {
    size_t h = 1;
    MergeHash(h, info.flags);
    MergeHash(h, info.magFilter);
    MergeHash(h, info.minFilter);
    MergeHash(h, info.mipmapMode);
    MergeHash(h, info.addressModeU);
    MergeHash(h, info.addressModeV);
    MergeHash(h, info.addressModeW);
    MergeHash(h, info.mipLodBias);
    MergeHash(h, info.anisotropyEnable);
    MergeHash(h, info.maxAnisotropy);
    MergeHash(h, info.compareEnable);
    MergeHash(h, info.compareOp);
    MergeHash(h, info.minLod);
    MergeHash(h, info.maxLod);
    MergeHash(h, info.borderColor);
    MergeHash(h, info.unnormalizedCoordinates);
    return h;
  }
};
}|}

let vk_sampler_create_info_equal_to =
  {|namespace std {
template <> struct equal_to<VkSamplerCreateInfo> {
  bool operator()(const VkSamplerCreateInfo &x, const VkSamplerCreateInfo &y) 
      const noexcept {
    return std::memcmp(&x, &y, sizeof(VkSamplerCreateInfo)) == 0;
  }
};
}|}

let pipeline_layout_transition_hash =
  {|namespace std {
template <> struct hash<std::pair<VkPipelineLayout, VkPipelineLayout>> {
  size_t operator()(const std::pair<VkPipelineLayout, VkPipelineLayout> &p) const noexcept {
    size_t h = 1;
    MergeHash(h, reinterpret_cast<size_t>(p.first));
    MergeHash(h, reinterpret_cast<size_t>(p.second));
    return h;
  }
};
}|}

let builtin_struct =
  {|struct Builtin {
  RenderTargetReference *screen = nullptr;
};|}

let gen_render_target_binder pname name =
  Printf.sprintf
    {|template <> struct %s_%s<RenderTargetReference *> {
  void operator()(RenderTargetReference *const &rt_ref, uint32_t &uid,
                  SampledImage2DReference *data) const noexcept {
    uid = 0;
    if (data != nullptr) {
      data->rt_ref = rt_ref;
    }
  }
};|}
    pname name

let gen_vector_matrix_type_uniform_binder pname name t =
  let tname = zrl_to_cpp_type t in
  Printf.sprintf
    {|template <> struct %s_%s<%s> {
  void operator()(const %s &t, uint32_t &uid, %s *data) const noexcept {
    uid = 0;
    if (data != nullptr) {
      *data = t;
    }
  }
};|}
    pname name tname tname tname

let gen_types_header pipelines root_elems cpp_constants =
  let cc_header =
    Cpp.Header.(
      empty "Types" |> add_include "<array>"
      |> add_include "<unordered_map>"
      |> add_include {|"glm/glm.hpp"|})
  in
  let cc_header =
    List.fold_left (flip Cpp.Header.add_section) cc_header cpp_constants
  in
  let cc_header =
    Cpp.Header.(
      cc_header
      |> add_section "constexpr int kDescriptorSetRegisterCount = 8;"
      |> add_section "constexpr size_t kDescriptorSetRegisterSize = zrl::_8KB;"
      |> add_section merge_hash_function
      |> add_section expand_extent_function
      |> add_section descriptor_set_register_struct
      |> add_section uniform_resource_struct
      |> add_section render_target_reference_struct
      |> add_section sampled_image2D_reference_struct
      |> add_section sampled_cubemap_reference_struct
      |> add_section render_pass_hash
      |> add_section render_pass_equal_to
      |> add_section framebuffer_hash
      |> add_section framebuffer_equal_to
      |> add_section builtin_struct
      |> add_section vk_descriptor_set_layout_binding_hash
      |> add_section vk_descriptor_set_layout_create_info_hash
      |> add_section vk_pipeline_layout_create_info_hash
      |> add_section vk_graphics_pipeline_create_info_hash
      |> add_section vk_sampler_create_info_hash
      |> add_section vk_sampler_create_info_equal_to
      |> add_section pipeline_layout_transition_hash)
  in
  let bind_traits =
    List.map
      (fun (pname, pipeline) ->
        let uniforms =
          match pipeline.gp_declaration.pd_type with
          | Type.Function (params, _) ->
              List.map
                (fun (name, t) ->
                  let binder_template =
                    Printf.sprintf "template<class T> struct %s_%s;" pname name
                  in
                  match t with
                  | Type.Vector _ | Type.Matrix _ ->
                      let vm_binder =
                        gen_vector_matrix_type_uniform_binder pname name t
                      in
                      String.concat "\n" [ binder_template; vm_binder ]
                  | Type.Sampler 2 ->
                      let rt_binder = gen_render_target_binder pname name in
                      String.concat "\n" [ binder_template; rt_binder ]
                  | _ -> binder_template)
                params
          | _ -> failwith "pipeline types must be Function types"
        in
        let inputs =
          List.map
            (fun (_, name, _) ->
              Printf.sprintf "template<class T> struct %s_%s;" pname name)
            pipeline.gp_inputs
        in
        let indices =
          [ Printf.sprintf "template<class T> struct %s_indices_;" pname ]
        in
        uniforms @ inputs @ indices)
      (MapString.bindings pipelines)
  in
  let cc_header =
    List.fold_left
      (flip Cpp.Header.add_section)
      cc_header (List.flatten bind_traits)
  in
  List.fold_left
    (fun acc tl ->
      acc >>= fun (glsl_types, cc_header) ->
      let L.{ loc; value } = tl in
      match value with
      | TypedAst.TypeDecl { td_name; td_type } ->
          gen_cpp_type loc td_name td_type >>= fun cpp_type ->
          let glsl_type = (td_name, td_type) in
          Ok
            (glsl_type :: glsl_types, Cpp.Header.add_section cpp_type cc_header)
      | _ -> acc)
    (Ok ([], cc_header))
    root_elems

let check_stage_chaining loc src_name from_stage dst_name to_stage =
  match (from_stage, to_stage) with
  | Type.Function (_, outputs), Type.Function (params, _) ->
      let _, inputs = List.split params in
      if outputs = inputs then Ok ()
      else error loc (`StageMismatch (src_name, outputs, dst_name, inputs))
  | _ -> failwith "stages must have Function type"

let gen_uniform_bindings env loc set id t =
  let open Type in
  match t with
  | Vector _ -> Ok [ (set, 0, 1, id, t, "") ]
  | Matrix _ -> Ok [ (set, 0, 1, id, t, "") ]
  | Sampler _ -> Ok [ (set, 0, 1, id, t, "") ]
  | SamplerCube -> Ok [ (set, 0, 1, id, t, "") ]
  | Array (tt, dims) ->
      if Type.is_ref tt && has_opaque_members env SetString.empty tt then
        error loc
          (`InvalidUniformType
            (t, "uniform types cannot contain nested opaque members"))
      else
        let count = array_type_size dims in
        Ok [ (set, 0, count, id, t, "") ]
  | TypeRef name -> (
      match Env.find_type ~local:false name env with
      | Some L.{ value = Record fields as tt; _ } ->
          let top_type = t in
          if has_opaque_members env SetString.empty tt then
            List.fold_left
              (fun acc (i, (name, t)) ->
                acc >>= fun bindings ->
                if Type.is_ref t && has_opaque_members env SetString.empty t
                then
                  error loc
                    (`InvalidUniformType
                      ( top_type,
                        "uniform types cannot contain nested opaque members" ))
                else
                  let new_id = id ^ "_" ^ name ^ "_" in
                  match tt with
                  | Array (_, dims) ->
                      let count = array_type_size dims in
                      let binding = (set, i, count, new_id, t, name) in
                      Ok (binding :: bindings)
                  | _ ->
                      let binding = (set, i, 1, new_id, t, name) in
                      Ok (binding :: bindings))
              (Ok []) (List.index fields)
          else Ok [ (set, 0, 1, id, t, "") ]
      | _ -> Ok [ (set, 0, 1, id, t, "") ] )
  | _ ->
      failwith
        (Printf.sprintf "unexpected pipeline uniform type: %s"
           (Type.string_of_type t))

let refactor_depth_test f =
  let TypedAst.{ fd_body; _ } = f in
  match fd_body with
  (* FIXME(ale64bit): RIP format *)
  | ( _,
      L.
        {
          value =
            If
              {
                if_cond =
                  ( _,
                    L.
                      {
                        value =
                          BinExpr
                            ( L.
                                {
                                  value =
                                    Access
                                      ( L.
                                          {
                                            value =
                                              Access
                                                ( L.{ value = Id "builtin"; _ },
                                                  "fragCoord" );
                                            _;
                                          },
                                        "z" );
                                  _;
                                },
                              ( ( Equal | NotEqual | LessThan | GreaterThan
                                | LessOrEqual | GreaterOrEqual ) as op ),
                              L.{ value = Access (_, "currentDepth"); _ } );
                        _;
                      } );
                if_true = [ (_, L.{ value = Discard; _ }) ];
                if_false = [];
              };
          _;
        } )
    :: other_stmts ->
      let write_enabled, compare_op =
        match op with
        | Equal -> (false, "VK_COMPARE_OP_NOT_EQUAL")
        | NotEqual -> (false, "VK_COMPARE_OP_EQUAL")
        | LessThan -> (true, "VK_COMPARE_OP_GREATER_OR_EQUAL")
        | GreaterThan -> (true, "VK_COMPARE_OP_LESS_OR_EQUAL")
        | LessOrEqual -> (true, "VK_COMPARE_OP_GREATER")
        | GreaterOrEqual -> (true, "VK_COMPARE_OP_LESS")
        | _ -> failwith "cannot happen"
      in
      (true, write_enabled, compare_op, { f with fd_body = other_stmts })
  | ( _,
      L.
        {
          value =
            If
              {
                if_cond =
                  ( _,
                    L.
                      {
                        value =
                          BinExpr
                            ( L.{ value = Access (_, "currentDepth"); _ },
                              ( ( Equal | NotEqual | LessThan | GreaterThan
                                | LessOrEqual | GreaterOrEqual ) as op ),
                              L.
                                {
                                  value =
                                    Access
                                      ( L.
                                          {
                                            value =
                                              Access
                                                ( L.{ value = Id "builtin"; _ },
                                                  "fragCoord" );
                                            _;
                                          },
                                        "z" );
                                  _;
                                } );
                        _;
                      } );
                if_true = [ (_, L.{ value = Discard; _ }) ];
                if_false = [];
              };
          _;
        } )
    :: other_stmts ->
      let write_enabled, compare_op =
        match op with
        | Equal -> (false, "VK_COMPARE_OP_NOT_EQUAL")
        | NotEqual -> (false, "VK_COMPARE_OP_EQUAL")
        | LessThan -> (true, "VK_COMPARE_OP_LESS")
        | GreaterThan -> (true, "VK_COMPARE_OP_GREATER")
        | LessOrEqual -> (true, "VK_COMPARE_OP_LESS_OR_EQUAL")
        | GreaterOrEqual -> (true, "VK_COMPARE_OP_GREATER_OR_EQUAL")
        | _ -> failwith "cannot happen"
      in
      (true, write_enabled, compare_op, { f with fd_body = other_stmts })
  | _ -> (false, false, "VK_COMPARE_OP_NEVER", f)

let gen_graphics_pipeline loc pd =
  let TypedAst.{ pd_env; pd_name; pd_type; pd_functions } = pd in
  let find_func name =
    List.find (fun TypedAst.{ fd_name; _ } -> fd_name = name) pd_functions
  in
  let find_func_opt name =
    List.find_opt (fun TypedAst.{ fd_name; _ } -> fd_name = name) pd_functions
  in
  let exclude_funcs names =
    List.filter
      (fun TypedAst.{ fd_name; _ } ->
        not (List.exists (fun name -> name = fd_name) names))
      pd_functions
  in
  let vertex = find_func "vertex" in
  let fragment = find_func "fragment" in
  let ( gp_depth_test_enabled,
        gp_depth_write_enabled,
        gp_depth_compare_op,
        fragment ) =
    refactor_depth_test fragment
  in
  let TypedAst.{ fd_type = vertex_type; _ } = vertex in
  let TypedAst.{ fd_type = fragment_type; _ } = fragment in
  check_stage_chaining loc "vertex" vertex_type "fragment" fragment_type
  >>= fun () ->
  match (pd_type, vertex_type) with
  | Type.Function (uniforms, outputs), Type.Function (inputs, _) ->
      let uniform_set =
        List.fold_left
          (fun s (i, (name, _)) -> MapString.add name i s)
          MapString.empty (List.index uniforms)
      in
      let _, inputs =
        List.fold_left
          (fun (offset, inputs) (name, t) ->
            (offset + glsl_type_locations pd_env t, (offset, name, t) :: inputs))
          (0, []) inputs
      in
      let outputs = List.index outputs in
      List.fold_left
        (fun acc (i, (name, t)) ->
          acc >>= fun uniforms ->
          gen_uniform_bindings pd_env loc i name t >>= fun bindings ->
          Ok (bindings @ uniforms))
        (Ok []) (List.index uniforms)
      >>= fun uniforms ->
      Ok
        {
          gp_name = pd_name;
          gp_declaration = pd;
          gp_uniforms = uniforms;
          gp_uniform_set = uniform_set;
          gp_inputs = List.rev inputs;
          gp_outputs = outputs;
          gp_vertex_stage = find_func "vertex";
          gp_geometry_stage = find_func_opt "geometry";
          gp_fragment_stage = Some fragment;
          gp_helper_functions =
            exclude_funcs [ "vertex"; "geometry"; "fragment" ];
          gp_depth_test_enabled;
          gp_depth_write_enabled;
          gp_depth_compare_op;
        }
  | _ -> failwith "pipeline must have Function type"

let gen_compute_pipeline loc _ =
  (* TODO: implement *)
  error loc (`Unsupported "compute pipelines not supported")

let gen_pipeline loc pd =
  let TypedAst.{ pd_functions; _ } = pd in
  let has_function name =
    List.exists (fun TypedAst.{ fd_name; _ } -> fd_name = name) pd_functions
  in
  match
    (has_function "vertex", has_function "compute", has_function "main")
  with
  | _, _, true -> error loc (`Unsupported "invalid pipeline function: 'main'")
  | true, false, false -> gen_graphics_pipeline loc pd
  | false, true, false -> gen_compute_pipeline loc pd
  | true, true, _ ->
      error loc
        (`Unsupported
          "ambiguous pipeline type: cannot have both 'vertex' and 'compute' \
           functions)")
  | false, false, _ ->
      error loc
        (`Unsupported
          "unknown pipeline type: must have either 'vertex' or 'compute' \
           function")

let gen_pipelines root_elems =
  List.fold_left
    (fun acc tl ->
      acc >>= fun pipelines ->
      let L.{ loc; value } = tl in
      match value with
      | TypedAst.PipelineDecl pd ->
          gen_pipeline loc pd >>= fun p ->
          Ok MapString.(pipelines |> add pd.pd_name p)
      | _ -> acc)
    (Ok MapString.empty) root_elems

let gen_renderer_libraries pipelines glsl_libraries root_elems =
  List.fold_left
    (fun acc tl ->
      acc >>= fun renderer_libraries ->
      let L.{ value; _ } = tl in
      match value with
      | TypedAst.RendererDecl rd ->
          gen_renderer_library pipelines glsl_libraries rd >>= fun lib ->
          Ok (lib :: renderer_libraries)
      | _ -> acc)
    (Ok []) root_elems

let gen_glsl_builtin_call_id = function
  | "fvec2" -> "vec2"
  | "fvec3" -> "vec3"
  | "fvec4" -> "vec4"
  | "fmat2" -> "mat2"
  | "fmat3" -> "mat3"
  | "fmat4" -> "mat4"
  | other -> other

let is_record_type t env =
  let open Type in
  match t with
  | TypeRef name -> (
      match Env.find_type ~local:false name env with
      | Some L.{ value = Type.Record _; _ } -> true
      | _ -> false )
  | Record _ -> true
  | _ -> false

let rec gen_glsl_expression env L.{ value; _ } =
  let open Ast in
  match value with
  | Access (L.{ value = Id "builtin"; _ }, "position") -> "gl_Position"
  | Access (L.{ value = Id "builtin"; _ }, "vertexID") -> "gl_VertexID"
  | Access (L.{ value = Id "builtin"; _ }, "instanceID") -> "gl_InstanceID"
  | Access (L.{ value = Id "builtin"; _ }, "fragCoord") -> "gl_FragCoord"
  | Access (L.{ value = Id "builtin"; _ }, "frontFacing") -> "gl_FrontFacing"
  | Access (expr, member) -> (
      (* We need to unwrap member accesses for opaque types since GLSL does 
       * not allow mixing opaque with non-opaque members in a struct. *)
      match expr with
      | L.{ value = Id id; _ } -> (
          match Env.find_name_scope id env with
          | Env.Pipeline (_, Type.Function (fields, _)) ->
              let _, param_type =
                List.find (fun (name, _) -> name = id) fields
              in
              if
                is_record_type param_type env
                && has_opaque_members env SetString.empty param_type
              then Printf.sprintf "%s_%s_" id member
              else Printf.sprintf "%s.%s" (gen_glsl_expression env expr) member
          | _ -> Printf.sprintf "%s.%s" (gen_glsl_expression env expr) member )
      | _ -> Printf.sprintf "%s.%s" (gen_glsl_expression env expr) member )
  | Index (expr, indices) ->
      Printf.sprintf "%s[%s]"
        (gen_glsl_expression env expr)
        (String.concat "][" (List.map (gen_glsl_expression env) indices))
  | Call (expr, args) ->
      let args =
        if call_has_named_args args then reorder_call_args env expr args
        else args
      in
      Printf.sprintf "%s(%s)"
        (gen_glsl_expression env expr)
        (String.concat ", " (List.map (gen_glsl_expression env) args))
  | Cast (t, expr) ->
      Printf.sprintf "%s(%s)" (zrl_to_glsl_type t)
        (gen_glsl_expression env expr)
  | NamedArg (_, expr) -> gen_glsl_expression env expr
  | BinExpr (lhs, op, rhs) ->
      Printf.sprintf "(%s %s %s)"
        (gen_glsl_expression env lhs)
        (Ast.string_of_binop op)
        (gen_glsl_expression env rhs)
  | UnExpr (op, rhs) ->
      Printf.sprintf "%s(%s)" (Ast.string_of_unop op)
        (gen_glsl_expression env rhs)
  | BoolLiteral true -> "true"
  | BoolLiteral false -> "false"
  | IntLiteral i -> string_of_int i
  | FloatLiteral f -> string_of_float f
  | Id id -> gen_glsl_builtin_call_id id

let rec gen_glsl_stmt stmt f =
  let open TypedAst in
  let open Glsl in
  let env, L.{ value = stmt; _ } = stmt in
  match stmt with
  | CallExpr (id, args) ->
      let arg_exprs =
        String.concat ", "
          (List.map (fun (_, expr) -> gen_glsl_expression env expr) args)
      in
      Function.(
        f
        |> append_code_section
             (Printf.sprintf "%s(%s);" (gen_glsl_builtin_call_id id) arg_exprs))
  | Var { bind_ids; bind_values } | Val { bind_ids; bind_values } -> (
      match (bind_ids, bind_values) with
      | [ id ], [ ([ typ ], value) ] ->
          let decl =
            Printf.sprintf "%s %s = %s;" (zrl_to_glsl_type typ) id
              (gen_glsl_expression env value)
          in
          Function.append_code_section decl f
      | ids, [ (types, value) ] ->
          let bindings = List.combine ids types in
          let f =
            List.fold_left
              (fun f (id, typ) ->
                let decl = Printf.sprintf "%s %s;" (zrl_to_glsl_type typ) id in
                Function.(f |> append_code_section decl))
              f bindings
          in
          let f = Function.append_code_section "{" f in
          let f =
            match value with
            | L.{ value = Ast.Call (L.{ value = Ast.Id fname; _ }, _); _ } ->
                let ret_type_name = Printf.sprintf "%sRetType" fname in
                let tmp_decl =
                  Printf.sprintf "%s tmp = %s;" ret_type_name
                    (gen_glsl_expression env value)
                in
                let f = Function.append_code_section tmp_decl f in
                let unpacks =
                  List.map
                    (fun (i, id) -> Printf.sprintf "%s = tmp.out%d;" id i)
                    (List.index ids)
                in
                List.fold_left (flip Function.append_code_section) f unpacks
            | _ ->
                failwith
                  "cannot unpack multiple-value from non-function expression"
          in
          let f = Function.append_code_section "}" f in
          f
      | ids, values ->
          let bindings = List.combine ids values in
          List.fold_left
            (fun f (id, (t, value)) ->
              let decl =
                Printf.sprintf "%s %s = %s;"
                  (zrl_to_glsl_type (List.hd t))
                  id
                  (gen_glsl_expression env value)
              in
              Function.append_code_section decl f)
            f bindings )
  | Assignment { asg_op; asg_lvalues; asg_rvalues } -> (
      match (asg_lvalues, asg_rvalues) with
      | [ (_, lhs) ], [ ([ _ ], rhs) ] ->
          let assignment =
            Printf.sprintf "%s %s %s;"
              (gen_glsl_expression env lhs)
              (Ast.string_of_assignop asg_op)
              (gen_glsl_expression env rhs)
          in
          Function.append_code_section assignment f
      | lhs, [ (_, rhs) ] ->
          let f = Function.append_code_section "{" f in
          let f =
            match rhs with
            | L.{ value = Ast.Call (L.{ value = Ast.Id fname; _ }, _); _ } ->
                let ret_type_name = Printf.sprintf "%sRetType" fname in
                let tmp_decl =
                  Printf.sprintf "%s tmp = %s;" ret_type_name
                    (gen_glsl_expression env rhs)
                in
                let f = Function.append_code_section tmp_decl f in
                let unpacks =
                  List.map
                    (fun (i, (_, lhs)) ->
                      Printf.sprintf "%s %s tmp.out%d;"
                        (gen_glsl_expression env lhs)
                        (Ast.string_of_assignop asg_op)
                        i)
                    (List.index lhs)
                in
                List.fold_left (flip Function.append_code_section) f unpacks
            | _ ->
                failwith
                  "cannot unpack multiple-value from non-function expression"
          in
          let f = Function.append_code_section "}" f in
          f
      | lhs, rhs ->
          let bindings = List.combine lhs rhs in
          List.fold_left
            (fun f ((_, lhs), (_, rhs)) ->
              let assignment =
                Printf.sprintf "%s %s %s;"
                  (gen_glsl_expression env lhs)
                  (Ast.string_of_assignop asg_op)
                  (gen_glsl_expression env rhs)
              in
              Function.append_code_section assignment f)
            f bindings )
  | If { if_cond = _, cond_expr; if_true; if_false } ->
      let cond =
        Printf.sprintf "if (%s) {" (gen_glsl_expression env cond_expr)
      in
      let f = Function.(f |> append_code_section cond) in
      let f = List.fold_left (fun f stmt -> gen_glsl_stmt stmt f) f if_true in
      let f = Function.(f |> append_code_section "} else {") in
      let f = List.fold_left (fun f stmt -> gen_glsl_stmt stmt f) f if_false in
      Function.(f |> append_code_section "}")
  | ForIter _ ->
      failwith "TODO(ale64bit): foreach statement not supported in GLSL"
  | ForRange
      {
        forrange_id;
        forrange_from = _, from_expr;
        forrange_to = _, to_expr;
        forrange_body;
      } ->
      let header =
        Printf.sprintf "for (int %s = (%s); i <= (%s); %s++) {" forrange_id
          (gen_glsl_expression env from_expr)
          (gen_glsl_expression env to_expr)
          forrange_id
      in
      let f = Function.(f |> append_code_section header) in
      let f =
        List.fold_left (fun f stmt -> gen_glsl_stmt stmt f) f forrange_body
      in
      Function.(f |> append_code_section "}")
  | Return [ (_, expr) ] ->
      let return =
        Printf.sprintf "return %s;" (gen_glsl_expression env expr)
      in
      Function.(f |> append_code_section return)
  | Return exprs ->
      let fname =
        match Env.match_function_scope env with
        | Some (Env.Function (s, _)) -> s
        | _ -> failwith "return can only be used from function scopes"
      in
      let ret_type_name = Printf.sprintf "%sRetType" fname in
      let return =
        Printf.sprintf "return %s(%s);" ret_type_name
          (String.concat ", "
             (List.map (fun (_, expr) -> gen_glsl_expression env expr) exprs))
      in
      Function.(f |> append_code_section return)
  | Discard -> Function.(f |> append_code_section "discard;")

let gen_glsl_function TypedAst.{ fd_name; fd_type; fd_body; _ } =
  let open Glsl in
  let f = Function.empty fd_name in
  let f, ret_struct =
    match fd_type with
    | Type.Function (params, []) ->
        let f =
          List.fold_left
            (fun f (name, t) ->
              Function.add_in_param (zrl_to_glsl_type t, name) f)
            f params
        in
        (f, None)
    | Type.Function (params, [ ret ]) ->
        let f = Function.(f |> set_return_type (zrl_to_glsl_type ret)) in
        let f =
          List.fold_left
            (fun f (name, t) ->
              Function.add_in_param (zrl_to_glsl_type t, name) f)
            f params
        in
        (f, None)
    | Type.Function (params, rets) ->
        let f =
          List.fold_left
            (fun f (name, t) ->
              Function.add_in_param (zrl_to_glsl_type t, name) f)
            f params
        in
        let ret_type_name = Printf.sprintf "%sRetType" fd_name in
        let f = Function.set_return_type ret_type_name f in
        let t =
          Type.Record
            (List.map
               (fun (i, t) -> (Printf.sprintf "out%d" i, t))
               (List.index rets))
        in
        (f, Some (ret_type_name, t))
    | t ->
        failwith
          (Printf.sprintf "GLSL function must have Function type but got '%s'"
             (Type.string_of_type t))
  in
  let f = List.fold_left (flip gen_glsl_stmt) f fd_body in
  (f, ret_struct)

let gen_shader env helpers structs name stage
    TypedAst.{ fd_name; fd_env; fd_type; _ } =
  let open Glsl in
  match fd_type with
  | Type.Function (params, rets) ->
      let sh = Shader.empty name stage in
      let _, sh =
        List.fold_left
          (fun (offset, sh) (name, t) ->
            ( offset + glsl_type_locations env t,
              Shader.add_input offset (zrl_to_glsl_type t, name) sh ))
          (0, sh) params
      in
      let _, sh =
        List.fold_left
          (fun (offset, sh) (i, t) ->
            let name = Printf.sprintf "zrl_output_%d" i in
            ( offset + glsl_type_locations env t,
              Shader.add_output offset (zrl_to_glsl_type t, name) sh ))
          (0, sh) (List.index rets)
      in
      let sh =
        List.fold_left (flip Shader.add_function) sh (List.rev helpers)
      in
      let sh =
        List.fold_left
          (fun sh (name, t) -> Shader.add_struct (gen_glsl_type name t) sh)
          sh (List.rev structs)
      in
      let output_names =
        List.map
          (fun (i, t) ->
            let output_name = Printf.sprintf "zrl_output_%d" i in
            ([ t ], L.{ value = Ast.Id output_name; loc = builtin_loc }))
          (List.index rets)
      in
      let entry_point_args =
        List.map
          (fun (name, _) -> L.{ value = Ast.Id name; loc = builtin_loc })
          params
      in
      let entry_point_call =
        ( rets,
          L.
            {
              value =
                Ast.Call
                  ( L.{ value = Ast.Id fd_name; loc = builtin_loc },
                    entry_point_args );
              loc = builtin_loc;
            } )
      in
      let param_types = List.map (fun (_, t) -> [ t ]) params in
      let entry_point_stmt =
        if List.length rets = 0 then
          TypedAst.CallExpr (fd_name, List.combine param_types entry_point_args)
        else
          TypedAst.Assignment
            {
              asg_op = Ast.Assign;
              asg_lvalues = output_names;
              asg_rvalues = [ entry_point_call ];
            }
      in
      let entry_point =
        TypedAst.
          {
            fd_env;
            fd_name = "main";
            fd_type = Type.Function ([], []);
            fd_body =
              [ (fd_env, L.{ value = entry_point_stmt; loc = builtin_loc }) ];
          }
      in
      let main_fn, _ = gen_glsl_function entry_point in
      Shader.(sh |> add_function main_fn)
  | _ -> failwith "shader programs must have Function type"

let get_dependent_helpers root_fn helper_fns =
  let rec collect_callees_from_expr_list expr_list =
    List.fold_left
      (fun acc expr -> SetString.union acc (collect_callees_from_expr expr))
      SetString.empty expr_list
  and collect_callees_from_expr L.{ value = expr; _ } =
    let open Ast in
    match expr with
    | Access (expr, _) -> collect_callees_from_expr expr
    | Index (lhs, rhs) ->
        SetString.(
          union
            (collect_callees_from_expr lhs)
            (collect_callees_from_expr_list rhs))
    | Call (L.{ value = Ast.Id id; _ }, args) -> (
        let callees =
          SetString.(
            union (singleton id) (collect_callees_from_expr_list args))
        in
        match
          List.find_opt
            (fun TypedAst.{ fd_name; _ } -> fd_name = id)
            helper_fns
        with
        | Some fd ->
            SetString.union callees
              (List.fold_left
                 (fun acc stmt ->
                   SetString.union acc (collect_callees_from_stmt stmt))
                 SetString.empty fd.fd_body)
        | None -> callees )
    | Call (lhs, rhs) ->
        SetString.(
          union
            (collect_callees_from_expr lhs)
            (collect_callees_from_expr_list rhs))
    | Cast (_, expr) -> collect_callees_from_expr expr
    | NamedArg (_, expr) -> collect_callees_from_expr expr
    | BinExpr (lhs, _, rhs) ->
        SetString.union
          (collect_callees_from_expr lhs)
          (collect_callees_from_expr rhs)
    | UnExpr (_, expr) -> collect_callees_from_expr expr
    | BoolLiteral _ -> SetString.empty
    | IntLiteral _ -> SetString.empty
    | FloatLiteral _ -> SetString.empty
    | Id _ -> SetString.empty
  and collect_callees_from_stmt (_, L.{ value = stmt; _ }) =
    let open TypedAst in
    match stmt with
    | CallExpr (id, args) -> (
        let callees =
          SetString.(
            union (singleton id)
              (List.fold_left
                 (fun acc (_, expr) ->
                   union acc (collect_callees_from_expr expr))
                 empty args))
        in
        match
          List.find_opt
            (fun TypedAst.{ fd_name; _ } -> fd_name = id)
            helper_fns
        with
        | Some fd ->
            SetString.union callees
              (List.fold_left
                 (fun acc stmt ->
                   SetString.union acc (collect_callees_from_stmt stmt))
                 SetString.empty fd.fd_body)
        | None -> callees )
    | Val { bind_values; _ } | Var { bind_values; _ } ->
        List.fold_left
          (fun acc (_, expr) ->
            SetString.union acc (collect_callees_from_expr expr))
          SetString.empty bind_values
    | Assignment { asg_lvalues; asg_rvalues; _ } ->
        SetString.(
          union
            (List.fold_left
               (fun acc (_, expr) ->
                 union acc (collect_callees_from_expr expr))
               empty asg_lvalues)
            (List.fold_left
               (fun acc (_, expr) ->
                 union acc (collect_callees_from_expr expr))
               empty asg_rvalues))
    | If { if_cond = _, expr; if_true; if_false } ->
        SetString.(
          union
            (collect_callees_from_expr expr)
            (union
               (List.fold_left
                  (fun acc stmt -> union acc (collect_callees_from_stmt stmt))
                  empty if_true)
               (List.fold_left
                  (fun acc stmt -> union acc (collect_callees_from_stmt stmt))
                  empty if_false)))
    | ForIter { foriter_it = _, expr; foriter_body; _ } ->
        SetString.(
          union
            (collect_callees_from_expr expr)
            (List.fold_left
               (fun acc stmt -> union acc (collect_callees_from_stmt stmt))
               empty foriter_body))
    | ForRange
        {
          forrange_from = _, from_expr;
          forrange_to = _, to_expr;
          forrange_body;
          _;
        } ->
        SetString.(
          union
            (union
               (collect_callees_from_expr from_expr)
               (collect_callees_from_expr to_expr))
            (List.fold_left
               (fun acc stmt -> union acc (collect_callees_from_stmt stmt))
               empty forrange_body))
    | Return exprs ->
        List.fold_left
          (fun acc (_, expr) ->
            SetString.union acc (collect_callees_from_expr expr))
          SetString.empty exprs
    | Discard -> SetString.empty
  in
  let callees =
    List.fold_left
      (fun acc stmt -> SetString.union acc (collect_callees_from_stmt stmt))
      SetString.empty root_fn.TypedAst.fd_body
  in
  List.filter (fun fd -> SetString.mem fd.TypedAst.fd_name callees) helper_fns

let gen_shaders env name glsl_types pipeline =
  let open Glsl in
  let gen_helpers fd =
    List.fold_left
      (fun (helpers, structs) fd ->
        match gen_glsl_function fd with
        | f, Some s -> (f :: helpers, s :: structs)
        | f, None -> (f :: helpers, structs))
      ([], glsl_types)
      (get_dependent_helpers fd pipeline.gp_helper_functions)
  in
  let helpers, structs = gen_helpers pipeline.gp_vertex_stage in
  let vertex_fn, vertex_ret_struct =
    gen_glsl_function pipeline.gp_vertex_stage
  in
  let vertex_structs =
    match vertex_ret_struct with Some s -> s :: structs | None -> structs
  in
  let vertex_shader =
    [
      gen_shader env (vertex_fn :: helpers) vertex_structs name Shader.Vertex
        pipeline.gp_vertex_stage;
    ]
  in
  let fragment_shader =
    match pipeline.gp_fragment_stage with
    | Some fd ->
        let helpers, structs = gen_helpers fd in
        let fragment_fn, fragment_ret_struct = gen_glsl_function fd in
        let fragment_structs =
          match fragment_ret_struct with
          | Some s -> s :: structs
          | None -> structs
        in
        let shader =
          gen_shader env (fragment_fn :: helpers) fragment_structs name
            Shader.Fragment fd
        in
        [ shader ]
    | None -> []
  in
  Ok (List.flatten [ vertex_shader; fragment_shader ])

let wrap_uniform set binding t name =
  let open Type in
  match t with
  | Sampler _ | SamplerCube | Array (Sampler _, _) ->
      (* Cannot wrap opaque types in blocks in GLSL *)
      (zrl_to_glsl_type t, name)
  | _ ->
      let block_name =
        Printf.sprintf "Set%dBinding%dWrapperBlock_" set binding
      in
      (Printf.sprintf "%s { %s %s; }" block_name (zrl_to_glsl_type t) name, "")

let gen_glsl_libraries env glsl_types glsl_constants pipelines =
  let open Glsl in
  List.fold_left
    (fun acc (name, pipeline) ->
      acc >>= fun glsl_libraries ->
      gen_shaders env name glsl_types pipeline >>= fun shaders ->
      let shaders =
        List.map
          (fun shader ->
            let shader =
              List.fold_left (flip Shader.add_constant) shader glsl_constants
            in
            List.fold_left
              (fun shader (set, binding, _, name, t, _) ->
                Shader.add_uniform set binding
                  (wrap_uniform set binding t name)
                  shader)
              shader pipeline.gp_uniforms)
          shaders
      in
      let lib =
        List.fold_left (flip Library.add_shader) (Library.empty name) shaders
      in
      Ok (lib :: glsl_libraries))
    (Ok [])
    (MapString.bindings pipelines)

let gen_constants env root_elems =
  List.fold_left
    (fun acc tl ->
      acc >>= fun (glsl_constants, cpp_constants) ->
      let L.{ value; _ } = tl in
      match value with
      | TypedAst.ConstDecl { cd_name; cd_value = [ t ], expr } ->
          let glsl_constant =
            Printf.sprintf "const %s %s = %s;" (zrl_to_glsl_type t) cd_name
              (gen_glsl_expression env expr)
          in
          let cpp_constant =
            Printf.sprintf "constexpr %s %s = %s;" (zrl_to_cpp_type t) cd_name
              (gen_cpp_expression env expr)
          in
          Ok (glsl_constant :: glsl_constants, cpp_constant :: cpp_constants)
      | _ -> acc)
    (Ok ([], []))
    root_elems

let gen cfg TypedAst.{ root_elems; root_env; _ } =
  let build =
    Build.(
      empty "BUILD"
      |> load "//core:builddefs.bzl" "COPTS"
      |> load "//core:builddefs.bzl" "DEFINES"
      |> load "//core:glsl_library.bzl" "glsl_library")
  in
  gen_constants root_env root_elems >>= fun (glsl_constants, cpp_constants) ->
  gen_pipelines root_elems >>= fun pipelines ->
  gen_types_header pipelines root_elems cpp_constants
  >>= fun (glsl_types, cc_types_header) ->
  let cc_types =
    Cpp.Library.(
      empty "Types" |> set_copts "COPTS" |> set_defines "DEFINES"
      |> add_header cc_types_header)
  in
  gen_glsl_libraries root_env glsl_types glsl_constants pipelines
  >>= fun glsl_libraries ->
  gen_renderer_libraries pipelines glsl_libraries root_elems
  >>= fun renderer_libraries ->
  let build =
    List.fold_left (flip Build.add_glsl_library) build glsl_libraries
  in
  let build =
    List.fold_left (flip Build.add_cc_library) build renderer_libraries
  in
  Ok
    Build.(
      build |> add_cc_library cc_types
      |> write_to cfg.Config.cfg_output_directory)
