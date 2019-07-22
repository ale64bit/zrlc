open Zrl
open Extensions
open Monad.Result
module L = Located
module MapString = Map.Make (String)

module Error = struct
  type t = [ `Unsupported of string | `MissingRendererEntryPoint of string ]

  let string_of_error L.{ loc; value } =
    let pos = L.string_of_start_position loc in
    let prefix = Printf.sprintf "%s: vkdummy error" pos in
    match value with
    | `Unsupported msg -> Printf.sprintf "%s: unsupported: %s" prefix msg
    | `MissingRendererEntryPoint r ->
        Printf.sprintf "%s: renderer %s is missing the entry point" prefix r
end

type graphics_pipeline = {
  gp_name : string;
  gp_declaration : TypedAst.pipeline_declaration;
  gp_uniforms : (int * int * string * Type.t) list;
  gp_inputs : (int * string * Type.t) list;
  gp_outputs : (int * Type.t) list;
  gp_vertex_stage : TypedAst.function_declaration;
  gp_geometry_stage : TypedAst.function_declaration option;
  gp_fragment_stage : TypedAst.function_declaration option;
  gp_helper_functions : TypedAst.function_declaration list;
}

let flip f x y = f y x

let error loc e = Error L.{ loc; value = e }

let find_func name funcs err =
  match
    List.find_opt (fun TypedAst.{ fd_name; _ } -> fd_name = name) funcs
  with
  | Some f -> Ok f
  | None -> err

let rec glsl_type_locations env = function
  | Type.TypeRef "int" -> 1
  | Type.TypeRef "bool" -> 1
  | Type.TypeRef "float" -> 1
  | Type.TypeRef "fvec2" -> 1
  | Type.TypeRef "fvec3" -> 1
  | Type.TypeRef "fvec4" -> 1
  | Type.TypeRef "ivec2" -> 1
  | Type.TypeRef "ivec3" -> 1
  | Type.TypeRef "ivec4" -> 1
  | Type.TypeRef "uvec2" -> 1
  | Type.TypeRef "uvec3" -> 1
  | Type.TypeRef "uvec4" -> 1
  | Type.TypeRef "fmat2" -> 2
  | Type.TypeRef "fmat3" -> 3
  | Type.TypeRef "fmat4" -> 4
  | Type.Array (t, dims) ->
      let size =
        List.fold_left
          (fun acc dim ->
            match dim with
            | Type.OfInt i -> acc * i
            | _ -> failwith "unsupported array dimension")
          1 dims
      in
      size * glsl_type_locations env t
  | Type.Record fields ->
      List.fold_left
        (fun acc (_, t) -> acc + glsl_type_locations env t)
        0 fields
  | Type.TypeRef name -> (
      match Env.find_type ~local:false name env with
      | Some L.{ value = Type.Record _ as r; _ } -> glsl_type_locations env r
      | Some L.{ value = t; _ } ->
          failwith ("unsupported type: " ^ Type.string_of_type t)
      | None -> failwith ("no such type: " ^ name) )
  | t -> failwith ("unsupported type: " ^ Type.string_of_type t)

let rec zrl_to_glsl_type = function
  | Type.TypeRef "int" -> "int"
  | Type.TypeRef "float" -> "float"
  | Type.TypeRef "fvec2" -> "vec2"
  | Type.TypeRef "fvec3" -> "vec3"
  | Type.TypeRef "fvec4" -> "vec4"
  | Type.TypeRef "ivec2" -> "ivec2"
  | Type.TypeRef "ivec3" -> "ivec3"
  | Type.TypeRef "ivec4" -> "ivec4"
  | Type.TypeRef "uvec2" -> "uvec2"
  | Type.TypeRef "uvec3" -> "uvec3"
  | Type.TypeRef "uvec4" -> "uvec4"
  | Type.TypeRef "fmat2" -> "mat2"
  | Type.TypeRef "fmat3" -> "mat3"
  | Type.TypeRef "fmat4" -> "mat4"
  | Type.TypeRef s -> s
  | Type.Array (t, dims) ->
      let string_of_dim = function
        | Type.OfInt i -> Printf.sprintf "[%d]" i
        | _ -> failwith "unsupported array dimension"
      in
      let dims = List.map string_of_dim dims in
      Printf.sprintf "%s%s" (zrl_to_glsl_type t) (String.concat "" dims)
  | t -> failwith "unsupported type: " ^ Type.string_of_type t

let rec zrl_to_cpp_type = function
  | Type.TypeRef "int" -> "int"
  | Type.TypeRef "bool" -> "bool"
  | Type.TypeRef "float" -> "float"
  | Type.TypeRef "fvec2" -> "glm::fvec2"
  | Type.TypeRef "fvec3" -> "glm::fvec3"
  | Type.TypeRef "fvec4" -> "glm::fvec4"
  | Type.TypeRef "ivec2" -> "glm::ivec2"
  | Type.TypeRef "ivec3" -> "glm::ivec3"
  | Type.TypeRef "ivec4" -> "glm::ivec4"
  | Type.TypeRef "uvec2" -> "glm::uvec2"
  | Type.TypeRef "uvec3" -> "glm::uvec3"
  | Type.TypeRef "uvec4" -> "glm::uvec4"
  | Type.TypeRef "fmat2" -> "glm::fmat2"
  | Type.TypeRef "fmat3" -> "glm::fmat3"
  | Type.TypeRef "fmat4" -> "glm::fmat4"
  | Type.Array (t, dims) ->
      let arr t = function
        | Type.OfInt i -> Printf.sprintf "std::array<%s, %d>" t i
        | _ -> failwith "unsupported array dimension"
      in
      let rec aux = function
        | [] -> ""
        | [ d ] -> arr (zrl_to_cpp_type t) d
        | d :: ds -> arr (aux ds) d
      in
      aux dims
  | Type.TypeRef "rt_rgb" -> "RenderTargetReference*"
  | Type.TypeRef "rt_rgba" -> "RenderTargetReference*"
  | Type.TypeRef "rt_ds" -> "RenderTargetReference*"
  | Type.TypeRef s -> s
  | t -> failwith "unsupported type: " ^ Type.string_of_type t

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

let gen_renderer_signature loc rd_name rd_functions r =
  let open Cpp in
  let err = error loc (`MissingRendererEntryPoint rd_name) in
  find_func "main" rd_functions err >>= fun TypedAst.{ fd_type; _ } ->
  match fd_type with
  | Type.Function (params, _) ->
      List.fold_left
        (fun acc (name, t) ->
          acc >>= fun r ->
          match t with
          | Type.TypeRef (("int" | "float" | "bool") as tname) ->
              Ok
                RendererEnv.
                  {
                    r with
                    render = Function.(r.render |> add_param (tname, name));
                  }
          (* TODO: support vector/matrix types via GLM *)
          | Type.TypeRef "atom" ->
              let tmpl_param = name ^ "AtomType" in
              let param = (Printf.sprintf "const %s&" tmpl_param, name) in
              Ok
                RendererEnv.
                  {
                    r with
                    render =
                      Function.(
                        r.render
                        |> add_template_param ("class " ^ tmpl_param)
                        |> add_param param);
                  }
          | Type.TypeRef "atomset" ->
              let tmpl_param = name ^ "AtomType" in
              let param =
                ( Printf.sprintf "const std::unordered_set<%s>&" tmpl_param,
                  name )
              in
              Ok
                RendererEnv.
                  {
                    r with
                    render =
                      Function.(
                        r.render
                        |> add_template_param ("class " ^ tmpl_param)
                        |> add_param param);
                  }
          | Type.TypeRef "atomlist" ->
              let tmpl_param = name ^ "AtomType" in
              let param =
                (Printf.sprintf "const std::vector<%s>&" tmpl_param, name)
              in
              Ok
                RendererEnv.
                  {
                    r with
                    render =
                      Function.(
                        r.render
                        |> add_template_param ("class " ^ tmpl_param)
                        |> add_param param);
                  }
          | Type.TypeRef tname ->
              let cr = "const " ^ tname ^ "&" in
              Ok
                RendererEnv.
                  {
                    r with
                    render = Function.(r.render |> add_param (cr, name));
                  }
          | _ ->
              error loc
                (`Unsupported
                  (Printf.sprintf "cannot use type %s as renderer argument"
                     (Type.string_of_type t))))
        (Ok r) params
  | _ -> failwith "renderer entry point must be of Function type"

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
  | BundledArg exprs ->
      Printf.sprintf "std::make_tuple(%s)"
        (String.concat ", " (List.map (gen_cpp_expression env) exprs))
  | BinExpr (lhs, op, rhs) ->
      Printf.sprintf "%s %s %s"
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

let is_rt_clear_or_write env op lvalues =
  let open Type in
  let is_rt tname =
    match Env.find_type ~local:false tname env with
    | Some L.{ value = RenderTarget _; _ } -> true
    | _ -> false
  in
  match op with
  | Ast.Assign | Ast.AssignPlus ->
      List.for_all
        (function [ TypeRef tname ], _ when is_rt tname -> true | _ -> false)
        lvalues
  | _ -> false

let gen_clear env lhs rhs r =
  let bindings = List.combine lhs rhs in
  List.fold_left
    (fun r ((_, lhs), (_, rhs)) ->
      let rt_var =
        Printf.sprintf "RenderTargetReference *_rt = (%s);"
          (gen_cpp_expression env lhs)
      in
      let end_current_pass =
        {|  if (current_render_pass_ != VK_NULL_HANDLE && _rt->load_op != VK_ATTACHMENT_LOAD_OP_DONT_CARE && _rt->load_op != VK_ATTACHMENT_LOAD_OP_CLEAR) { 
    vkCmdEndRenderPass(gc_cmd_buffer_[image_index]);
    current_render_pass_ = VK_NULL_HANDLE;
  }|}
      in
      let update_load_op = "_rt->load_op = VK_ATTACHMENT_LOAD_OP_CLEAR;" in
      let clear_value_expr = gen_cpp_expression env rhs in
      let update_clear_value =
        Printf.sprintf
          "std::memcpy(&_rt->clear_value, glm::value_ptr(%s), sizeof (%s));"
          clear_value_expr clear_value_expr
      in
      RendererEnv.(
        r
        |> add_stmt_inside_render_passes rt_var
        |> add_stmt_inside_render_passes end_current_pass
        |> add_stmt_inside_render_passes update_load_op
        |> add_stmt_inside_render_passes update_clear_value))
    r bindings

let gen_shader_stage_create_infos p =
  let vertex_stage =
    [ Printf.sprintf
        {|
    VkPipelineShaderStageCreateInfo vertex_stage = {};
    vertex_stage.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    vertex_stage.stage = VK_SHADER_STAGE_VERTEX_BIT;
    vertex_stage.module = %sVert_shader_module_;
    vertex_stage.pName = "main";
    vertex_stage.pSpecializationInfo = nullptr;
    shader_stages.push_back(vertex_stage);
  |}
        p.gp_name
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
    fragment_stage.module = %sFrag_shader_module_;
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

let rec stride_of_type t =
  let open Type in
  match t with
  | TypeRef "bool" -> 4
  | TypeRef "float" -> 4
  | TypeRef "int" -> 4
  | TypeRef "uint" -> 4
  | TypeRef "bvec2" -> 2 * stride_of_type (TypeRef "bool")
  | TypeRef "bvec3" -> 3 * stride_of_type (TypeRef "bool")
  | TypeRef "bvec4" -> 4 * stride_of_type (TypeRef "bool")
  | TypeRef "fvec2" -> 2 * stride_of_type (TypeRef "float")
  | TypeRef "fvec3" -> 3 * stride_of_type (TypeRef "float")
  | TypeRef "fvec4" -> 4 * stride_of_type (TypeRef "float")
  | TypeRef "ivec2" -> 2 * stride_of_type (TypeRef "int")
  | TypeRef "ivec3" -> 3 * stride_of_type (TypeRef "int")
  | TypeRef "ivec4" -> 4 * stride_of_type (TypeRef "int")
  | TypeRef "uvec2" -> 2 * stride_of_type (TypeRef "uint")
  | TypeRef "uvec3" -> 3 * stride_of_type (TypeRef "uint")
  | TypeRef "uvec4" -> 4 * stride_of_type (TypeRef "uint")
  | TypeRef "fmat2" -> 2 * 2 * stride_of_type (TypeRef "float")
  | TypeRef "fmat3" -> 3 * 3 * stride_of_type (TypeRef "float")
  | TypeRef "fmat4" -> 4 * 4 * stride_of_type (TypeRef "float")
  | Array (t, dims) ->
      let total =
        List.fold_left
          (fun acc d ->
            match d with
            | OfInt i -> i * acc
            | _ -> failwith "unexpected dimension type")
          1 dims
      in
      total * stride_of_type t
  | _ -> failwith ("invalid input vertex type: " ^ Type.string_of_type t)

let format_of_vertex_input_type t =
  let open Type in
  match t with
  | TypeRef "bool" -> "VK_FORMAT_R32_UINT"
  | TypeRef "float" -> "VK_FORMAT_R32_SFLOAT"
  | TypeRef "int" -> "VK_FORMAT_R32_SINT"
  | TypeRef "uint" -> "VK_FORMAT_R32_UINT"
  | TypeRef "bvec2" -> "VK_FORMAT_R32_UINT"
  | TypeRef "bvec3" -> "VK_FORMAT_R32G32_UINT"
  | TypeRef "bvec4" -> "VK_FORMAT_R32G32B32_UINT"
  | TypeRef "fvec2" -> "VK_FORMAT_R32_SFLOAT"
  | TypeRef "fvec3" -> "VK_FORMAT_R32G32_SFLOAT"
  | TypeRef "fvec4" -> "VK_FORMAT_R32G32B32_SFLOAT"
  | TypeRef "ivec2" -> "VK_FORMAT_R32_SINT"
  | TypeRef "ivec3" -> "VK_FORMAT_R32G32_SINT"
  | TypeRef "ivec4" -> "VK_FORMAT_R32G32B32_SINT"
  | TypeRef "uvec2" -> "VK_FORMAT_R32_UINT"
  | TypeRef "uvec3" -> "VK_FORMAT_R32G32_UINT"
  | TypeRef "uvec4" -> "VK_FORMAT_R32G32B32_UINT"
  | _ -> failwith ("invalid input vertex type: " ^ Type.string_of_type t)

let gen_vertex_input_state_create_info p =
  let num_inputs = List.length p.gp_inputs in
  let vertex_bindings =
    List.map
      (fun (i, (_, _, t)) ->
        Printf.sprintf
          {|
    vertex_bindings[%d].binding = %d;
    vertex_bindings[%d].stride = %d;
    vertex_bindings[%d].inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
    |}
          i i i (stride_of_type t) i)
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

let gen_create_and_bind_pipeline env p r lhs =
  let open RendererEnv in
  let shader_stages = gen_shader_stage_create_infos p in
  let vertex_input_state_create_info = gen_vertex_input_state_create_info p in
  let num_color_attachments =
    List.fold_left
      (fun acc (_, lhs) ->
        let rt_name =
          match gen_cpp_expression env lhs with
          | "builtin.screen" -> "builtin_screen"
          | other -> other
        in
        match MapString.find rt_name r.render_targets with
        | Color -> 1 + acc
        | DepthStencil -> acc)
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
  r
  |> add_stmt_inside_render_passes
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
    rasterization_state.frontFace = VK_FRONT_FACE_CLOCKWISE;
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
    depth_stencil_state.depthTestEnable = VK_TRUE;
    depth_stencil_state.depthWriteEnable = VK_TRUE;
    depth_stencil_state.depthCompareOp = VK_COMPARE_OP_LESS_OR_EQUAL;
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
    auto res = pipeline_cache_.find(graphics_pipeline_create_info);
    if (res == pipeline_cache_.end()) {
      DLOG << name_ << ": creating new graphics pipeline" << '\n';
      CHECK_VK(vkCreateGraphicsPipelines(core_.GetLogicalDevice().GetHandle(),
                                         nullptr, 1, &graphics_pipeline_create_info,
                                         nullptr, &pipeline));
      pipeline_cache_[graphics_pipeline_create_info] = pipeline;
    } else {
      pipeline = res->second;
    }
    if (pipeline != current_pipeline_) {
      vkCmdBindPipeline(gc_cmd_buffer_[image_index], VK_PIPELINE_BIND_POINT_GRAPHICS,
                        pipeline);
      current_pipeline_ = pipeline;
    }
}|}
          shader_stages vertex_input_state_create_info num_color_attachments
          color_blend_attachments p.gp_name)

let collect_atom_bindings = function
  | [ (_, L.{ value = Ast.Call (L.{ value = Ast.Id _; _ }, args); _ }) ] ->
      List.map
        (function
          | L.{ value = Ast.NamedArg (lhs, L.{ value = Ast.Id rhs; _ }); _ } ->
              (lhs, rhs)
          | expr ->
              failwith
                ( "unsupported pipeline atom binding: "
                ^ Ast.string_of_expression expr ))
        args
  | _ -> failwith "unsupported right hand side of pipeline write statement"

let gen_vertex_buffer_copies_and_bindings pipeline r bindings =
  let is_input id =
    List.exists (fun (_, name, _) -> id = name) pipeline.gp_inputs
  in
  let is_uniform id =
    List.exists (fun (_, _, name, _) -> id = name) pipeline.gp_uniforms
  in
  List.fold_left
    (fun r (lhs, rhs) ->
      if is_input lhs then (
        (* TODO: implement *)
        Printf.printf "binding atom %s to input %s\n" rhs lhs;
        r )
      else if is_uniform lhs then (
        (* TODO: implement *)
        Printf.printf "binding atom %s to uniform %s\n" rhs lhs;
        r )
      else r)
    r bindings

let gen_write env pipeline lhs rhs r =
  let open RendererEnv in
  let bindings = collect_atom_bindings rhs in
  let rt_exprs =
    String.concat ", "
      (List.map (fun (_, lhs) -> gen_cpp_expression env lhs) lhs)
  in
  let r =
    r
    |> add_stmt_inside_render_passes
         (Printf.sprintf
            {|{
            const std::vector<RenderTargetReference*> rts = {%s};
            for (auto rt : rts) { rt->target_layout = VK_IMAGE_LAYOUT_GENERAL; }
            VkRenderPass render_pass = GetOrCreateRenderPass(rts);
            VkFramebuffer framebuffer = GetOrCreateFramebuffer(rts, render_pass);
            if (current_render_pass_ != render_pass) {
              if (current_render_pass_ != VK_NULL_HANDLE) {
                vkCmdEndRenderPass(gc_cmd_buffer_[image_index]);
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
              vkCmdBeginRenderPass(gc_cmd_buffer_[image_index], &begin_info, 
                                   VK_SUBPASS_CONTENTS_INLINE);
              current_render_pass_ = render_pass;
              for (auto rt : rts) { rt->current_layout = rt->target_layout; }
            }
            |}
            rt_exprs)
  in
  let r = gen_create_and_bind_pipeline env pipeline r lhs in
  let r = gen_vertex_buffer_copies_and_bindings pipeline r bindings in
  r |> add_stmt_inside_render_passes "}"

let extract_pipeline_name_from_write = function
  | _, L.{ value = Ast.Call (L.{ value = Ast.Id id; _ }, _); _ } -> id
  | _ -> failwith "unexpected right-hand side in pipeline write statement"

let gen_clear_or_write env pipelines lhs op rhs r =
  match op with
  | Ast.Assign -> gen_clear env lhs rhs r
  | Ast.AssignPlus ->
      let () = assert (List.length rhs = 1) in
      let pid = extract_pipeline_name_from_write (List.hd rhs) in
      let p = MapString.find pid pipelines in
      gen_write env p lhs rhs r
  | _ -> failwith ("unexpected operator: " ^ Ast.string_of_assignop op)

let rec gen_cpp_stmt pipelines stmt r =
  let open TypedAst in
  let env, L.{ value = stmt; _ } = stmt in
  match stmt with
  | Var { bind_ids; bind_values } | Val { bind_ids; bind_values } -> (
      match (bind_ids, bind_values) with
      | [ id ], [ ([ typ ], value) ] ->
          let decl =
            Printf.sprintf "%s %s = %s;" (zrl_to_cpp_type typ) id
              (gen_cpp_expression env value)
          in
          RendererEnv.(r |> add_stmt_inside_render_passes decl)
      | ids, [ (types, value) ] ->
          let bindings = List.combine ids types in
          let r =
            List.fold_left
              (fun r (id, typ) ->
                let decl = Printf.sprintf "%s %s;" (zrl_to_cpp_type typ) id in
                RendererEnv.(r |> add_stmt_inside_render_passes decl))
              r bindings
          in
          let assignment =
            Printf.sprintf "std::tie(%s) = %s;" (String.concat ", " ids)
              (gen_cpp_expression env value)
          in
          RendererEnv.(r |> add_stmt_inside_render_passes assignment)
      | ids, values ->
          let bindings = List.combine ids values in
          List.fold_left
            (fun r (id, (t, value)) ->
              let decl =
                Printf.sprintf "%s %s = %s;"
                  (zrl_to_cpp_type (List.hd t))
                  id
                  (gen_cpp_expression env value)
              in
              RendererEnv.(r |> add_stmt_inside_render_passes decl))
            r bindings )
  | Assignment { asg_op; asg_lvalues; asg_rvalues } -> (
      if is_rt_clear_or_write env asg_op asg_lvalues then
        gen_clear_or_write env pipelines asg_lvalues asg_op asg_rvalues r
      else
        match (asg_lvalues, asg_rvalues) with
        | [ (_, lhs) ], [ ([ _ ], rhs) ] ->
            let decl =
              Printf.sprintf "%s %s %s;"
                (gen_cpp_expression env lhs)
                (Ast.string_of_assignop asg_op)
                (gen_cpp_expression env rhs)
            in
            RendererEnv.(r |> add_stmt_inside_render_passes decl)
        | lhs, [ (_, rhs) ] ->
            let lvalues =
              String.concat ", "
                (List.map (fun (_, expr) -> gen_cpp_expression env expr) lhs)
            in
            let assignment =
              Printf.sprintf "std::tie(%s) = %s;" lvalues
                (gen_cpp_expression env rhs)
            in
            RendererEnv.(r |> add_stmt_inside_render_passes assignment)
        | lhs, rhs ->
            let bindings = List.combine lhs rhs in
            List.fold_left
              (fun r ((_, lhs), (_, rhs)) ->
                let assignment =
                  Printf.sprintf "%s %s %s;"
                    (gen_cpp_expression env lhs)
                    (Ast.string_of_assignop asg_op)
                    (gen_cpp_expression env rhs)
                in
                RendererEnv.(r |> add_stmt_inside_render_passes assignment))
              r bindings )
  | If { if_cond = _, cond_expr; if_true; if_false } ->
      let cond =
        Printf.sprintf "if (%s) {" (gen_cpp_expression env cond_expr)
      in
      let r = RendererEnv.(r |> add_stmt_inside_render_passes cond) in
      let r =
        List.fold_left (fun r stmt -> gen_cpp_stmt pipelines stmt r) r if_true
      in
      let r = RendererEnv.(r |> add_stmt_inside_render_passes "} else {") in
      let r =
        List.fold_left (fun r stmt -> gen_cpp_stmt pipelines stmt r) r if_false
      in
      RendererEnv.(r |> add_stmt_inside_render_passes "}")
  | ForIter { foriter_id; foriter_it = _, it_expr; foriter_body } ->
      let header =
        Printf.sprintf "for (const auto &%s : %s) {" foriter_id
          (gen_cpp_expression env it_expr)
      in
      let r = RendererEnv.(r |> add_stmt_inside_render_passes header) in
      let r =
        List.fold_left
          (fun r stmt -> gen_cpp_stmt pipelines stmt r)
          r foriter_body
      in
      RendererEnv.(r |> add_stmt_inside_render_passes "}")
  | ForRange
      { forrange_id;
        forrange_from = _, from_expr;
        forrange_to = _, to_expr;
        forrange_body
      } ->
      let header =
        Printf.sprintf "for (int %s = (%s); i <= (%s); ++%s) {" forrange_id
          (gen_cpp_expression env from_expr)
          (gen_cpp_expression env to_expr)
          forrange_id
      in
      let r = RendererEnv.(r |> add_stmt_inside_render_passes header) in
      let r =
        List.fold_left
          (fun r stmt -> gen_cpp_stmt pipelines stmt r)
          r forrange_body
      in
      RendererEnv.(r |> add_stmt_inside_render_passes "}")
  | Return [ (_, expr) ] ->
      let return = Printf.sprintf "return %s;" (gen_cpp_expression env expr) in
      RendererEnv.(r |> add_stmt_inside_render_passes return)
  | Return exprs ->
      let return =
        Printf.sprintf "return std::make_tuple(%s);"
          (String.concat ", "
             (List.map (fun (_, expr) -> gen_cpp_expression env expr) exprs))
      in
      RendererEnv.(
        r
        |> add_stmt_inside_render_passes return
        |> add_stmt_inside_render_passes "}")
  | Discard ->
      failwith "'discard' statement cannot be used outside fragment shaders"

let rec gen_cpp_function pipelines fd_body r =
  match fd_body with
  | [] -> Ok r
  | stmt :: body ->
      gen_cpp_function pipelines body (gen_cpp_stmt pipelines stmt r)

let gen_renderer_code loc pipelines rd_name rd_functions r =
  let err = error loc (`MissingRendererEntryPoint rd_name) in
  find_func "main" rd_functions err >>= fun TypedAst.{ fd_body; _ } ->
  gen_cpp_function pipelines fd_body r

let gen_render_targets rd_type r =
  let open Cpp in
  match rd_type with
  | Type.Function (params, _) ->
      List.fold_left
        (fun acc (name, t) ->
          acc >>= fun r ->
          match t with
          | Type.TypeRef (("rt_rgba" | "rt_ds") as rt_type_name) ->
              let img_id = name ^ "_image_" in
              let ref_id = name ^ "_ref_" in
              let img_member = ("zrl::Image", img_id) in
              let ref_member = ("RenderTargetReference", ref_id) in
              let ptr_member = ("RenderTargetReference *", name) in
              let rt_type, format =
                if rt_type_name = "rt_rgba" then
                  (RendererEnv.Color, "VK_FORMAT_B8G8R8A8_UNORM")
                else if rt_type_name = "rt_ds" then
                  (RendererEnv.DepthStencil, "VK_FORMAT_D32_SFLOAT")
                else failwith "unsupported render target type"
              in
              let img_ctor_args =
                match rt_type with
                | RendererEnv.Color ->
                    "{core_, core_.GetSwapchain().GetExtent(), 1, \
                     VK_FORMAT_B8G8R8A8_UNORM, VK_IMAGE_TILING_OPTIMAL, \
                     VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | \
                     VK_IMAGE_USAGE_SAMPLED_BIT, \
                     VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, \
                     VK_IMAGE_ASPECT_COLOR_BIT}"
                | RendererEnv.DepthStencil ->
                    "{core_, core_.GetSwapchain().GetExtent(), 1, \
                     VK_FORMAT_D32_SFLOAT, VK_IMAGE_TILING_OPTIMAL, \
                     VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, \
                     VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, \
                     VK_IMAGE_ASPECT_DEPTH_BIT}"
              in
              let ref_ctor_args =
                Printf.sprintf
                  "{%s, %s.GetViewHandle(), VK_IMAGE_LAYOUT_UNDEFINED, \
                   VK_IMAGE_LAYOUT_UNDEFINED, \
                   VK_ATTACHMENT_LOAD_OP_DONT_CARE, {}}"
                  format img_id
              in
              let r =
                RendererEnv.
                  {
                    r with
                    render_targets =
                      MapString.(r.render_targets |> add name rt_type);
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
                        |> add_member_initializer (name, "nullptr"));
                  }
              in
              Ok
                RendererEnv.(
                  r
                  |> add_stmt_before_recording
                       (Printf.sprintf "%s = &%s;" name ref_id)
                  |> add_stmt_before_recording
                       (Printf.sprintf
                          "%s->current_layout = VK_IMAGE_LAYOUT_UNDEFINED;"
                          name)
                  |> add_stmt_before_recording
                       (Printf.sprintf
                          "%s->load_op = VK_ATTACHMENT_LOAD_OP_DONT_CARE;" name))
          | _ -> acc)
        (Ok r) params
  | _ -> failwith "renderer must be of Function type"

let check_single_entry_point loc rd_functions =
  if List.length rd_functions <= 1 then Ok ()
  else error loc (`Unsupported "only one function per renderer is allowed")

let gen_shader_modules cfg_bazel_package glsl_libraries r =
  let pkg = String.concat "/" cfg_bazel_package in
  let shaders = List.flatten (List.map Glsl.Library.shaders glsl_libraries) in
  let r =
    List.fold_left
      (fun r shader ->
        let stage_name =
          match Glsl.Shader.stage shader with
          | Vertex -> "Vert"
          | Geometry -> "Geom"
          | Fragment -> "Frag"
          | Compute -> "Comp"
        in
        let shader_name = Glsl.Shader.name shader ^ stage_name in
        let module_member = shader_name ^ "_shader_module_" in
        let rclass =
          Cpp.Class.(
            r.RendererEnv.rclass
            |> add_include (Printf.sprintf {|"%s/%s.h"|} pkg shader_name)
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
    create_info.codeSize = sizeof k%s;
    create_info.pCode = k%s;
    CHECK_VK(vkCreateShaderModule(core_.GetLogicalDevice().GetHandle(), &create_info,
                                  nullptr, &%s));
  }
|}
                    shader_name shader_name module_member))
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
  List.fold_left
    (fun r (_, _, name, t) ->
      let descriptor_set_layout =
        Printf.sprintf "%s_%s_descriptor_set_layout_" p.gp_name name
      in
      let descriptor_type =
        match t with
        | Type.TypeRef "sampler2D" ->
            "VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
        | _ -> "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER"
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
                  {|{
    VkDescriptorSetLayoutBinding binding = {};
    binding.binding = 0;
    binding.descriptorType = %s;
    binding.descriptorCount = 1;
    binding.stageFlags = VK_SHADER_STAGE_ALL_GRAPHICS;
    binding.pImmutableSamplers = nullptr;

    VkDescriptorSetLayoutCreateInfo create_info = {};
    create_info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
    create_info.pNext = nullptr;
    create_info.flags = 0;
    create_info.bindingCount = 1;
    create_info.pBindings = &binding;
    CHECK_VK(vkCreateDescriptorSetLayout(core_.GetLogicalDevice().GetHandle(), 
                                         &create_info, nullptr, &%s));
}|}
                  descriptor_type descriptor_set_layout))
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
    r p.gp_uniforms

let gen_pipeline_layout pipeline r =
  let pipeline_layout =
    Printf.sprintf "%s_pipeline_layout_" pipeline.gp_name
  in
  let descriptor_set_layouts =
    List.rev
      (List.fold_left
         (fun layouts (_, _, name, _) ->
           let descriptor_set_layout =
             Printf.sprintf "%s_%s_descriptor_set_layout_" pipeline.gp_name
               name
           in
           descriptor_set_layout :: layouts)
         [] pipeline.gp_uniforms)
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

let gen_renderer_library loc Config.{ cfg_bazel_package; _ } pipelines
    glsl_libraries TypedAst.{ rd_name; rd_type; rd_functions; _ } =
  let open Cpp in
  let r = RendererEnv.empty rd_name cfg_bazel_package in
  check_single_entry_point loc rd_functions >>= fun () ->
  gen_shader_modules cfg_bazel_package glsl_libraries r
  >>= gen_pipeline_members pipelines
  >>= gen_render_targets rd_type
  >>= gen_renderer_signature loc rd_name rd_functions
  >>= gen_renderer_code loc pipelines rd_name rd_functions
  >>= fun r ->
  let output_class = RendererEnv.export r in
  let types_target = "//" ^ String.concat "/" cfg_bazel_package ^ ":Types" in
  let lib =
    Library.(
      empty rd_name cfg_bazel_package
      |> set_copts "COPTS" |> set_defines "DEFINES" |> add_class output_class
      |> add_dep types_target |> add_dep "//core" |> add_dep "@glm"
      |> add_dep "@vulkan//:sdk")
  in
  Ok lib

let gen_glsl_type td_name td_type =
  let open Type in
  match td_type with
  | Record fields ->
      let fields =
        String.concat "\n"
          (List.map
             (fun (name, t) ->
               Printf.sprintf "%s %s;" (zrl_to_glsl_type t) name)
             fields)
      in
      Printf.sprintf "struct %s { %s }" td_name fields
  | _ ->
      failwith
        ( "cannot generate glsl type for non-record type: "
        ^ string_of_type td_type )

let gen_cpp_type loc td_name td_type =
  let open Type in
  match td_type with
  | Record fields ->
      let fields =
        String.concat "\n"
          (List.map
             (fun (name, t) ->
               Printf.sprintf "alignas(16) %s %s;" (zrl_to_cpp_type t) name)
             fields)
      in
      Ok (Printf.sprintf "struct alignas(16) %s { %s };" td_name fields)
  | _ ->
      error loc
        (`Unsupported
          ( "cannot generate c++ type for non-record type: "
          ^ string_of_type td_type ))

let merge_hash_function =
  {|template <class T> inline void MergeHash(size_t &h1, const T h2) {
  h1 = h1 * 31 + static_cast<size_t>(h2);
}|}

let render_target_reference_struct =
  {|struct RenderTargetReference {
  const VkFormat format = VK_FORMAT_UNDEFINED;
  const VkImageView attachment = VK_NULL_HANDLE;
  VkImageLayout current_layout = VK_IMAGE_LAYOUT_UNDEFINED;
  VkImageLayout target_layout = VK_IMAGE_LAYOUT_UNDEFINED;
  VkAttachmentLoadOp load_op = VK_ATTACHMENT_LOAD_OP_MAX_ENUM;
  VkClearValue clear_value;
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
  constexpr bool operator()(const std::vector<RenderTargetReference> &a,
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
  constexpr bool operator()(const std::vector<RenderTargetReference> &a,
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

template <> struct equal_to<VkGraphicsPipelineCreateInfo> {
  size_t operator()(const VkGraphicsPipelineCreateInfo &info1,
                    const VkGraphicsPipelineCreateInfo &info2) const noexcept {
    // TODO implement
    return hash<VkGraphicsPipelineCreateInfo>{}(info1) ==
           hash<VkGraphicsPipelineCreateInfo>{}(info2);
  }
};

}|}

let builtin_struct =
  {|struct Builtin {
  RenderTargetReference *screen = nullptr;
};|}

let gen_types_header root_elems =
  let cc_header =
    Cpp.Header.(
      empty "Types" |> add_include "<array>"
      |> add_include {|"glm/glm.hpp"|}
      |> add_section merge_hash_function
      |> add_section render_target_reference_struct
      |> add_section render_pass_hash
      |> add_section render_pass_equal_to
      |> add_section framebuffer_hash
      |> add_section framebuffer_equal_to
      |> add_section builtin_struct
      |> add_section vk_descriptor_set_layout_binding_hash
      |> add_section vk_descriptor_set_layout_create_info_hash
      |> add_section vk_pipeline_layout_create_info_hash
      |> add_section vk_graphics_pipeline_create_info_hash)
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

let check_stage_chaining loc from_stage to_stage =
  match (from_stage, to_stage) with
  | Type.Function (_, outputs), Type.Function (params, _) ->
      let _, inputs = List.split params in
      if outputs = inputs then Ok ()
      else
        error loc (`Unsupported "stage outputs must match next stage inputs")
  | _ -> failwith "stages must have Function types"

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
  let TypedAst.{ fd_type = vertex_type; _ } = vertex in
  let TypedAst.{ fd_type = fragment_type; _ } = fragment in
  check_stage_chaining loc vertex_type fragment_type >>= fun () ->
  match (pd_type, vertex_type) with
  | Type.Function (uniforms, outputs), Type.Function (inputs, _) ->
      let uniforms =
        List.(
          map
            (fun (i, (name, t)) -> (i / 8, i mod 8, name, t))
            (index uniforms))
      in
      let _, inputs =
        List.fold_left
          (fun (offset, inputs) (name, t) ->
            (offset + glsl_type_locations pd_env t, (offset, name, t) :: inputs))
          (0, []) inputs
      in
      let outputs = List.index outputs in
      Ok
        {
          gp_name = pd_name;
          gp_declaration = pd;
          gp_uniforms = uniforms;
          gp_inputs = List.rev inputs;
          gp_outputs = outputs;
          gp_vertex_stage = find_func "vertex";
          gp_geometry_stage = find_func_opt "geometry";
          gp_fragment_stage = find_func_opt "fragment";
          gp_helper_functions =
            exclude_funcs [ "vertex"; "geometry"; "fragment" ];
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

let gen_renderer_libraries cfg pipelines glsl_libraries root_elems =
  let pkg = String.concat "/" cfg.Config.cfg_bazel_package in
  List.fold_left
    (fun acc tl ->
      acc >>= fun renderer_libraries ->
      let L.{ loc; value } = tl in
      match value with
      | TypedAst.RendererDecl rd ->
          gen_renderer_library loc cfg pipelines glsl_libraries rd
          >>= fun lib ->
          let lib =
            List.fold_left
              (fun lib glsl_lib ->
                let dep =
                  Printf.sprintf "//%s:%s" pkg (Glsl.Library.name glsl_lib)
                in
                Cpp.Library.add_dep dep lib)
              lib glsl_libraries
          in
          Ok (lib :: renderer_libraries)
      | _ -> acc)
    (Ok []) root_elems

let gen_glsl_builtin_call_id = function
  | "ivec2" -> "ivec2"
  | "ivec3" -> "ivec3"
  | "ivec4" -> "ivec4"
  | "uvec2" -> "uvec2"
  | "uvec3" -> "uvec3"
  | "uvec4" -> "uvec4"
  | "fvec2" -> "vec2"
  | "fvec3" -> "vec3"
  | "fvec4" -> "vec4"
  | "dvec2" -> "dvec2"
  | "dvec3" -> "dvec3"
  | "dvec4" -> "dvec4"
  | "bvec2" -> "bvec2"
  | "bvec3" -> "bvec3"
  | "bvec4" -> "bvec4"
  | "imat2" -> "imat2"
  | "imat3" -> "imat3"
  | "imat4" -> "imat4"
  | "umat2" -> "umat2"
  | "umat3" -> "umat3"
  | "umat4" -> "umat4"
  | "fmat2" -> "mat2"
  | "fmat3" -> "mat3"
  | "fmat4" -> "mat4"
  | "dmat2" -> "dmat2"
  | "dmat3" -> "dmat3"
  | "dmat4" -> "dmat4"
  | "bmat2" -> "bmat2"
  | "bmat3" -> "bmat3"
  | "bmat4" -> "bmat4"
  | other -> other

let rec gen_glsl_expression env L.{ value; _ } =
  let open Ast in
  match value with
  | Access (L.{ value = Id "builtin"; _ }, "position") -> "gl_Position"
  | Access (L.{ value = Id "builtin"; _ }, "vertexID") -> "gl_VertexID"
  | Access (L.{ value = Id "builtin"; _ }, "instanceID") -> "gl_InstanceID"
  | Access (L.{ value = Id "builtin"; _ }, "fragCoord") -> "gl_FragCoord"
  | Access (L.{ value = Id "builtin"; _ }, "frontFacing") -> "gl_FrontFacing"
  | Access (expr, member) ->
      Printf.sprintf "%s.%s" (gen_glsl_expression env expr) member
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
  | BundledArg _ ->
      failwith "cannot convert BundledArg expression to GLSL expression"
  | BinExpr (lhs, op, rhs) ->
      Printf.sprintf "%s %s %s"
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
  | ForIter _ -> failwith "TODO: foreach statement not supported in GLSL"
  | ForRange
      { forrange_id;
        forrange_from = _, from_expr;
        forrange_to = _, to_expr;
        forrange_body
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
        match Env.scope_summary env with
        | Env.Function (s, _) -> s
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
          ( "GLSL function must have Function type but got: "
          ^ Type.string_of_type t )
  in
  let f = List.fold_left (flip gen_glsl_stmt) f fd_body in
  (f, ret_struct)

let builtin_pos =
  Lexing.{ pos_fname = "builtin"; pos_lnum = 0; pos_cnum = 0; pos_bol = 0 }

let builtin_loc = (builtin_pos, builtin_pos)

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
      let entry_point =
        TypedAst.
          {
            fd_env;
            fd_name = "main";
            fd_type = Type.Function ([], []);
            fd_body =
              [ ( fd_env,
                  L.
                    {
                      value =
                        TypedAst.Assignment
                          {
                            asg_op = Ast.Assign;
                            asg_lvalues = output_names;
                            asg_rvalues = [ entry_point_call ];
                          };
                      loc = builtin_loc;
                    } )
              ];
          }
      in
      let main_fn, _ = gen_glsl_function entry_point in
      Shader.(sh |> add_function main_fn)
  | _ -> failwith "shader programs must have Function type"

let gen_shaders env name glsl_types pipeline =
  let open Glsl in
  let helpers, structs =
    List.fold_left
      (fun (helpers, structs) fd ->
        match gen_glsl_function fd with
        | f, Some s -> (f :: helpers, s :: structs)
        | f, None -> (f :: helpers, structs))
      ([], glsl_types) pipeline.gp_helper_functions
  in
  let vertex_fn, vertex_ret_struct =
    gen_glsl_function pipeline.gp_vertex_stage
  in
  let vertex_structs =
    match vertex_ret_struct with Some s -> s :: structs | None -> structs
  in
  let vertex_shader =
    [ gen_shader env (vertex_fn :: helpers) vertex_structs name Shader.Vertex
        pipeline.gp_vertex_stage
    ]
  in
  let fragment_shader =
    match pipeline.gp_fragment_stage with
    | Some fd ->
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

let glsl_uniform_format env t name =
  match t with
  | Type.TypeRef "int" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "float" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "fvec2" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "fvec3" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "fvec4" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "ivec2" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "ivec3" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "ivec4" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "uvec2" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "uvec3" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "uvec4" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "fmat2" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "fmat3" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "fmat4" -> (zrl_to_glsl_type t, name)
  | Type.TypeRef "sampler2D" -> (zrl_to_glsl_type t, name)
  | Type.Array (t, dims) ->
      let dims =
        List.map
          (function
            | Type.OfInt i -> Printf.sprintf "[%d]" i
            | _ -> failwith "unsupported array dimension")
          dims
      in
      (zrl_to_glsl_type t ^ String.concat "" dims, name)
  | Type.TypeRef s -> (
      match Env.find_type ~local:false s env with
      | Some L.{ value = Type.Record fields; _ } ->
          let fields =
            List.map
              (fun (name, t) -> zrl_to_glsl_type t ^ " " ^ name ^ ";")
              fields
          in
          let t = Printf.sprintf "{ %s }" (String.concat " " fields) in
          (name, t)
      | Some L.{ value = t; _ } ->
          failwith ("cannot inline non-record type: " ^ Type.string_of_type t)
      | None -> failwith ("unknown type: " ^ Type.string_of_type t) )
  | t -> failwith ("unsupported type: " ^ Type.string_of_type t)

let gen_glsl_libraries env glsl_types pipelines =
  let open Glsl in
  List.fold_left
    (fun acc (name, pipeline) ->
      acc >>= fun glsl_libraries ->
      gen_shaders env name glsl_types pipeline >>= fun shaders ->
      let shaders =
        List.map
          (fun shader ->
            List.fold_left
              (fun shader (set, binding, name, t) ->
                Shader.add_uniform set binding
                  (glsl_uniform_format env t name)
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

let gen cfg TypedAst.{ root_elems; root_env; _ } =
  let build =
    Build.(
      empty "BUILD"
      |> load "//core:builddefs.bzl" "COPTS"
      |> load "//core:builddefs.bzl" "DEFINES"
      |> load "//core:glsl_library.bzl" "glsl_library")
  in
  gen_types_header root_elems >>= fun (glsl_types, cc_types_header) ->
  let cc_types =
    Cpp.Library.(
      empty "Types" cfg.Config.cfg_bazel_package
      |> set_copts "COPTS" |> set_defines "DEFINES"
      |> add_header cc_types_header)
  in
  gen_pipelines root_elems >>= fun pipelines ->
  gen_glsl_libraries root_env glsl_types pipelines >>= fun glsl_libraries ->
  gen_renderer_libraries cfg pipelines glsl_libraries root_elems
  >>= fun renderer_libraries ->
  let build =
    List.fold_left (flip Build.add_glsl_library) build glsl_libraries
  in
  let build =
    List.fold_left (flip Build.add_cc_library) build renderer_libraries
  in
  Ok
    Build.(
      build |> add_cc_library cc_types |> write_to cfg.cfg_output_directory)
