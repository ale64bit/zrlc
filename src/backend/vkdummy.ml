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
  gp_uniforms : (string * Type.t) list;
  gp_inputs : (string * Type.t) list;
  gp_outputs : Type.t list;
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
  | Type.TypeRef s -> s
  | t -> failwith "unsupported type: " ^ Type.string_of_type t

(*
let call_has_named_args args =
  List.for_all
    (function L.{ value = Ast.NamedArg _; _ } -> true | _ -> false)
    args

let reorder_call_args expr args =
  match expr with
  | [ Type.Function (params, _) ], _ ->
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
*)

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

let rec gen_cpp_expression L.{ value; _ } =
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
  | NamedArg (_, expr) -> gen_cpp_expression expr
  | BundledArg exprs ->
      Printf.sprintf "std::make_tuple(%s)"
        (String.concat ", " (List.map gen_cpp_expression exprs))
  | BinExpr (lhs, op, rhs) ->
      Printf.sprintf "%s %s %s" (gen_cpp_expression lhs)
        (Ast.string_of_binop op) (gen_cpp_expression rhs)
  | UnExpr (op, rhs) ->
      Printf.sprintf "%s(%s)" (Ast.string_of_unop op) (gen_cpp_expression rhs)
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

let gen_clear lhs rhs r =
  let bindings = List.combine lhs rhs in
  List.fold_left
    (fun r ((_, lhs), (_, rhs)) ->
      let rt_name =
        match gen_cpp_expression lhs with
        | "builtin.screen" -> "builtin_screen"
        | other -> other
      in
      let end_current_pass =
        Printf.sprintf
          {|  if (current_render_pass != VK_NULL_HANDLE && rt_%s_load_op != VK_ATTACHMENT_LOAD_OP_DONT_CARE && rt_%s_load_op != VK_ATTACHMENT_LOAD_OP_CLEAR) { 
    vkCmdEndRenderPass(gct_cmd_buffer_[image_index]);
    current_render_pass = VK_NULL_HANDLE;
  }|}
          rt_name rt_name
      in
      let update_load_op =
        Printf.sprintf "rt_%s_load_op = VK_ATTACHMENT_LOAD_OP_CLEAR;" rt_name
      in
      let update_clear_value =
        Printf.sprintf "rt_%s_clear_value = %s;" rt_name
          (gen_cpp_expression rhs)
      in
      RendererEnv.(
        r
        |> add_stmt_inside_render_passes end_current_pass
        |> add_stmt_inside_render_passes update_load_op
        |> add_stmt_inside_render_passes update_clear_value))
    r bindings

let gen_write lhs _ r =
  let open RendererEnv in
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
        | "builtin.screen" -> "rt_builtin_screen_[image_index]"
        | id -> Printf.sprintf "rt_%s_" id)
      lhs
  in
  let rp_tuples, clear_values =
    List.split
      (List.map
         (fun (i, (_, lhs)) ->
           let rt_name =
             match gen_cpp_expression lhs with
             | "builtin.screen" -> "builtin_screen"
             | other -> other
           in
           let rt_type = MapString.find rt_name r.render_targets in
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
           (tuple, cv))
         List.(combine (init (length lhs) (fun i -> i)) lhs))
  in
  let layouts =
    List.map
      (fun (_, lhs) ->
        let rt_name =
          match gen_cpp_expression lhs with
          | "builtin.screen" -> "builtin_screen"
          | other -> other
        in
        Printf.sprintf "rt_%s_layout = VK_IMAGE_LAYOUT_GENERAL;" rt_name)
      lhs
  in
  RendererEnv.(
    r
    |> add_stmt_inside_render_passes "{"
    |> add_stmt_inside_render_passes
         (Printf.sprintf "RenderPassDescriptor rp_desc {{%s}};"
            (String.concat ", " rp_tuples))
    |> add_stmt_inside_render_passes
         (Printf.sprintf "FramebufferDescriptor fb_desc {{%s}};"
            (String.concat ", " fb_imgs))
    |> add_stmt_inside_render_passes
         "VkRenderPass render_pass = GetOrCreateRenderPass(rp_desc);"
    |> add_stmt_inside_render_passes
         "VkFramebuffer framebuffer = GetOrCreateFramebuffer(fb_desc, \
          render_pass);"
    |> add_stmt_inside_render_passes
         "if (current_render_pass != render_pass) {"
    |> add_stmt_inside_render_passes
         "  if (current_render_pass != VK_NULL_HANDLE) {"
    |> add_stmt_inside_render_passes
         "    vkCmdEndRenderPass(gct_cmd_buffer_[image_index]);"
    |> add_stmt_inside_render_passes "  }"
    |> add_stmt_inside_render_passes (String.concat "\n" layouts)
    |> add_stmt_inside_render_passes
         "  std::vector<VkClearValue> clear_values(rp_desc.attachments.size());"
    |> add_stmt_inside_render_passes (String.concat "\n" clear_values)
    |> add_stmt_inside_render_passes "  VkRenderPassBeginInfo begin_info = {};"
    |> add_stmt_inside_render_passes
         "  begin_info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;"
    |> add_stmt_inside_render_passes "  begin_info.pNext = nullptr;"
    |> add_stmt_inside_render_passes "  begin_info.renderPass = render_pass;"
    |> add_stmt_inside_render_passes "  begin_info.framebuffer = framebuffer;"
    |> add_stmt_inside_render_passes "  begin_info.renderArea.offset = {0, 0};"
    |> add_stmt_inside_render_passes
         "  begin_info.renderArea.extent = core_.GetSwapchain().GetExtent();"
    |> add_stmt_inside_render_passes
         "  begin_info.clearValueCount = \
          static_cast<uint32_t>(clear_values.size());"
    |> add_stmt_inside_render_passes
         "  begin_info.pClearValues = clear_values.data();"
    |> add_stmt_inside_render_passes
         "  vkCmdBeginRenderPass(gct_cmd_buffer_[image_index], &begin_info, \
          VK_SUBPASS_CONTENTS_INLINE);"
    |> add_stmt_inside_render_passes "  current_render_pass = render_pass;"
    |> add_stmt_inside_render_passes "}"
    |> add_stmt_inside_render_passes "// TODO: execute actual draw call"
    |> add_stmt_inside_render_passes "}")

let gen_clear_or_write lhs op rhs r =
  match op with
  | Ast.Assign -> gen_clear lhs rhs r
  | Ast.AssignPlus -> gen_write lhs rhs r
  | _ -> failwith ("unexpected operator: " ^ Ast.string_of_assignop op)

let rec gen_cpp_stmt stmt r =
  let open TypedAst in
  let env, L.{ value = stmt; _ } = stmt in
  match stmt with
  | Var { bind_ids; bind_values } | Val { bind_ids; bind_values } -> (
      match (bind_ids, bind_values) with
      | [ id ], [ ([ typ ], value) ] ->
          let decl =
            Printf.sprintf "%s %s = %s;" (zrl_to_cpp_type typ) id
              (gen_cpp_expression value)
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
              (gen_cpp_expression value)
          in
          RendererEnv.(r |> add_stmt_inside_render_passes assignment)
      | ids, values ->
          let bindings = List.combine ids values in
          List.fold_left
            (fun r (id, (t, value)) ->
              let decl =
                Printf.sprintf "%s %s = %s;"
                  (zrl_to_cpp_type (List.hd t))
                  id (gen_cpp_expression value)
              in
              RendererEnv.(r |> add_stmt_inside_render_passes decl))
            r bindings )
  | Assignment { asg_op; asg_lvalues; asg_rvalues } -> (
      if is_rt_clear_or_write env asg_op asg_lvalues then
        gen_clear_or_write asg_lvalues asg_op asg_rvalues r
      else
        match (asg_lvalues, asg_rvalues) with
        | [ (_, lhs) ], [ ([ _ ], rhs) ] ->
            let decl =
              Printf.sprintf "%s %s %s;" (gen_cpp_expression lhs)
                (Ast.string_of_assignop asg_op)
                (gen_cpp_expression rhs)
            in
            RendererEnv.(r |> add_stmt_inside_render_passes decl)
        | lhs, [ (_, rhs) ] ->
            let lvalues =
              String.concat ", "
                (List.map (fun (_, expr) -> gen_cpp_expression expr) lhs)
            in
            let assignment =
              Printf.sprintf "std::tie(%s) = %s;" lvalues
                (gen_cpp_expression rhs)
            in
            RendererEnv.(r |> add_stmt_inside_render_passes assignment)
        | lhs, rhs ->
            let bindings = List.combine lhs rhs in
            List.fold_left
              (fun r ((_, lhs), (_, rhs)) ->
                let assignment =
                  Printf.sprintf "%s %s %s;" (gen_cpp_expression lhs)
                    (Ast.string_of_assignop asg_op)
                    (gen_cpp_expression rhs)
                in
                RendererEnv.(r |> add_stmt_inside_render_passes assignment))
              r bindings )
  | If { if_cond = _, cond_expr; if_true; if_false } ->
      let cond = Printf.sprintf "if (%s) {" (gen_cpp_expression cond_expr) in
      let r = RendererEnv.(r |> add_stmt_inside_render_passes cond) in
      let r = List.fold_left (fun r stmt -> gen_cpp_stmt stmt r) r if_true in
      let r = RendererEnv.(r |> add_stmt_inside_render_passes "} else {") in
      let r = List.fold_left (fun r stmt -> gen_cpp_stmt stmt r) r if_false in
      RendererEnv.(r |> add_stmt_inside_render_passes "}")
  | ForIter { foriter_id; foriter_it = _, it_expr; foriter_body } ->
      let header =
        Printf.sprintf "for (const auto &%s : %s) {" foriter_id
          (gen_cpp_expression it_expr)
      in
      let r = RendererEnv.(r |> add_stmt_inside_render_passes header) in
      let r =
        List.fold_left (fun r stmt -> gen_cpp_stmt stmt r) r foriter_body
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
          (gen_cpp_expression from_expr)
          (gen_cpp_expression to_expr)
          forrange_id
      in
      let r = RendererEnv.(r |> add_stmt_inside_render_passes header) in
      let r =
        List.fold_left (fun r stmt -> gen_cpp_stmt stmt r) r forrange_body
      in
      RendererEnv.(r |> add_stmt_inside_render_passes "}")
  | Return [ (_, expr) ] ->
      let return = Printf.sprintf "return %s;" (gen_cpp_expression expr) in
      RendererEnv.(r |> add_stmt_inside_render_passes return)
  | Return exprs ->
      let return =
        Printf.sprintf "return std::make_tuple(%s);"
          (String.concat ", "
             (List.map (fun (_, expr) -> gen_cpp_expression expr) exprs))
      in
      RendererEnv.(
        r
        |> add_stmt_inside_render_passes return
        |> add_stmt_inside_render_passes "}")
  | Discard ->
      failwith "'discard' statement cannot be used outside fragment shaders"

let rec gen_cpp_function fd_body r =
  match fd_body with
  | [] -> Ok r
  | stmt :: body -> gen_cpp_function body (gen_cpp_stmt stmt r)

let gen_renderer_code loc rd_name rd_functions r =
  let err = error loc (`MissingRendererEntryPoint rd_name) in
  find_func "main" rd_functions err >>= fun TypedAst.{ fd_body; _ } ->
  gen_cpp_function fd_body r

let gen_render_targets rd_type r =
  let open Cpp in
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
                  ( RendererEnv.Color,
                    "CreateColorRenderTarget",
                    "VK_FORMAT_B8G8R8A8_UNORM",
                    "glm::fvec4" )
                else
                  ( RendererEnv.DepthStencil,
                    "CreateDepthRenderTarget",
                    "VK_FORMAT_D32_SFLOAT",
                    "glm::fvec2" )
              in
              let load_op_member = ("VkAttachmentLoadOp", id ^ "load_op") in
              let clear_value_member =
                (clear_value_cpp_type, id ^ "clear_value")
              in
              let r =
                RendererEnv.
                  {
                    r with
                    render_targets =
                      MapString.(r.render_targets |> add name rt_type);
                    rclass =
                      Class.(
                        r.rclass |> add_private_member image
                        |> add_private_member load_op_member
                        |> add_private_member clear_value_member);
                    ctor =
                      Function.(
                        r.ctor
                        |> append_code_section
                             (Printf.sprintf "  %s = %s();" id fname)
                        |> add_member_initializer
                             (id ^ "load_op", "VK_ATTACHMENT_LOAD_OP_DONT_CARE"));
                    dtor =
                      Function.(
                        r.dtor |> prepend_code_section ("  " ^ id ^ ".reset();"));
                  }
              in
              Ok
                RendererEnv.(
                  r
                  |> add_stmt_before_recording
                       (Printf.sprintf "constexpr VkFormat rt_%s_format = %s;"
                          name fmt)
                  |> add_stmt_before_recording
                       (Printf.sprintf
                          "VkImageLayout rt_%s_layout = \
                           VK_IMAGE_LAYOUT_UNDEFINED;"
                          name))
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

let gen_renderer_library loc Config.{ cfg_bazel_package; _ } glsl_libraries
    TypedAst.{ rd_name; rd_type; rd_functions; _ } =
  let open Cpp in
  let r = RendererEnv.empty rd_name cfg_bazel_package in
  check_single_entry_point loc rd_functions >>= fun () ->
  gen_shader_modules cfg_bazel_package glsl_libraries r >>= fun r ->
  gen_render_targets rd_type r >>= fun r ->
  gen_renderer_signature loc rd_name rd_functions r >>= fun r ->
  gen_renderer_code loc rd_name rd_functions r >>= fun r ->
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

let render_pass_descriptor_struct =
  {|struct RenderPassDescriptor {
  std::vector<std::tuple<VkFormat, VkAttachmentLoadOp, VkImageLayout, VkImageLayout>> attachments;

  bool operator==(const RenderPassDescriptor &that) const {
    return attachments == that.attachments;
  }
};|}

let framebuffer_descriptor_struct =
  {|struct FramebufferDescriptor {
  std::vector<VkImageView> attachments;

  bool operator==(const FramebufferDescriptor &that) const {
    return attachments == that.attachments;
  }
};|}

let render_pass_descriptor_hash =
  {|namespace std {
  template<> struct hash<RenderPassDescriptor> { 
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
  };
}|}

let framebuffer_descriptor_hash =
  {|namespace std {
  template<> struct hash<FramebufferDescriptor> {
    size_t operator()(const FramebufferDescriptor &desc) const noexcept {
      size_t h = 0;
      for (const auto &a : desc.attachments) {
        h = h*31 + reinterpret_cast<size_t>(a);
      }
      return h;
    }
  };
}|}

let vk_descriptor_set_layout_binding_hash =
  {|namespace std {
template <> struct hash<VkDescriptorSetLayoutBinding> {
  size_t operator()(const VkDescriptorSetLayoutBinding &b) const noexcept {
    size_t h = b.binding;
    h = h * 31 + b.descriptorType;
    h = h * 31 + b.descriptorCount;
    h = h * 31 + b.stageFlags;
    return h;
  }
};
}|}

let vk_descriptor_set_layout_create_info_hash =
  {|namespace std {
template <> struct hash<VkDescriptorSetLayoutCreateInfo> {
  size_t operator()(const VkDescriptorSetLayoutCreateInfo &info) const
      noexcept {
    size_t h = info.flags;
    h = h * 31 + info.bindingCount;
    for (uint32_t i = 0; i < info.bindingCount; ++i) {
      h = h * 31 + hash<VkDescriptorSetLayoutBinding>{}(info.pBindings[i]);
    }
    return h;
  }
};
}|}

let vk_pipeline_layout_create_info_hash =
  {|namespace std {
template <> struct hash<VkPipelineLayoutCreateInfo> {
  size_t operator()(const VkPipelineLayoutCreateInfo &info) const noexcept {
    size_t h = info.flags;
    h = h * 31 + info.setLayoutCount;
    for (uint32_t i = 0; i < info.setLayoutCount; ++i) {
      h = h * 31 + reinterpret_cast<size_t>(info.pSetLayouts[i]);
    }
    h = h * 31 + info.pushConstantRangeCount;
    for (uint32_t i = 0; i < info.pushConstantRangeCount; ++i) {
      h = h * 31 + info.pPushConstantRanges[i].stageFlags;
      h = h * 31 + info.pPushConstantRanges[i].offset;
      h = h * 31 + info.pPushConstantRanges[i].size;
    }
    return h;
  }
};
}|}

let gen_types_header root_elems =
  let cc_header =
    Cpp.Header.(
      empty "Types" |> add_include "<array>"
      |> add_include {|"glm/glm.hpp"|}
      |> add_section render_pass_descriptor_struct
      |> add_section render_pass_descriptor_hash
      |> add_section framebuffer_descriptor_struct
      |> add_section framebuffer_descriptor_hash
      |> add_section vk_descriptor_set_layout_binding_hash
      |> add_section vk_descriptor_set_layout_create_info_hash
      |> add_section vk_pipeline_layout_create_info_hash)
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
  let TypedAst.{ pd_name; pd_type; pd_functions; _ } = pd in
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
      Ok
        {
          gp_name = pd_name;
          gp_declaration = pd;
          gp_uniforms = uniforms;
          gp_inputs = inputs;
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

let gen_pipeline loc _ pd =
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

let gen_pipelines cfg root_elems =
  List.fold_left
    (fun acc tl ->
      acc >>= fun pipelines ->
      let L.{ loc; value } = tl in
      match value with
      | TypedAst.PipelineDecl pd ->
          gen_pipeline loc cfg pd >>= fun p ->
          Ok MapString.(pipelines |> add pd.pd_name p)
      | _ -> acc)
    (Ok MapString.empty) root_elems

let gen_renderer_libraries cfg glsl_libraries root_elems =
  let pkg = String.concat "/" cfg.Config.cfg_bazel_package in
  List.fold_left
    (fun acc tl ->
      acc >>= fun renderer_libraries ->
      let L.{ loc; value } = tl in
      match value with
      | TypedAst.RendererDecl rd ->
          gen_renderer_library loc cfg glsl_libraries rd >>= fun lib ->
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

let rec gen_glsl_expression L.{ value; _ } =
  let open Ast in
  match value with
  | Access (L.{ value = Id "builtin"; _ }, "position") -> "gl_Position"
  | Access (L.{ value = Id "builtin"; _ }, "vertexID") -> "gl_VertexID"
  | Access (L.{ value = Id "builtin"; _ }, "instanceID") -> "gl_InstanceID"
  | Access (L.{ value = Id "builtin"; _ }, "fragCoord") -> "gl_FragCoord"
  | Access (L.{ value = Id "builtin"; _ }, "frontFacing") -> "gl_FrontFacing"
  | Access (expr, member) ->
      Printf.sprintf "%s.%s" (gen_glsl_expression expr) member
  | Index (expr, indices) ->
      Printf.sprintf "%s[%s]" (gen_glsl_expression expr)
        (String.concat "][" (List.map gen_glsl_expression indices))
  | Call (expr, args) ->
      (* TODO: implement
      let args =
        if call_has_named_args args then reorder_call_args expr args else args
      in
*)
      Printf.sprintf "%s(%s)" (gen_glsl_expression expr)
        (String.concat ", " (List.map gen_glsl_expression args))
  | Cast (t, expr) ->
      Printf.sprintf "%s(%s)" (zrl_to_glsl_type t) (gen_glsl_expression expr)
  | NamedArg (_, expr) -> gen_glsl_expression expr
  | BundledArg _ ->
      failwith "cannot convert BundledArg expression to GLSL expression"
  | BinExpr (lhs, op, rhs) ->
      Printf.sprintf "%s %s %s" (gen_glsl_expression lhs)
        (Ast.string_of_binop op) (gen_glsl_expression rhs)
  | UnExpr (op, rhs) ->
      Printf.sprintf "%s(%s)" (Ast.string_of_unop op) (gen_glsl_expression rhs)
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
              (gen_glsl_expression value)
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
                    (gen_glsl_expression value)
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
                  (gen_glsl_expression value)
              in
              Function.append_code_section decl f)
            f bindings )
  | Assignment { asg_op; asg_lvalues; asg_rvalues } -> (
      match (asg_lvalues, asg_rvalues) with
      | [ (_, lhs) ], [ ([ _ ], rhs) ] ->
          let assignment =
            Printf.sprintf "%s %s %s;" (gen_glsl_expression lhs)
              (Ast.string_of_assignop asg_op)
              (gen_glsl_expression rhs)
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
                    (gen_glsl_expression rhs)
                in
                let f = Function.append_code_section tmp_decl f in
                let unpacks =
                  List.map
                    (fun (i, (_, lhs)) ->
                      Printf.sprintf "%s %s tmp.out%d;"
                        (gen_glsl_expression lhs)
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
                Printf.sprintf "%s %s %s;" (gen_glsl_expression lhs)
                  (Ast.string_of_assignop asg_op)
                  (gen_glsl_expression rhs)
              in
              Function.append_code_section assignment f)
            f bindings )
  | If { if_cond = _, cond_expr; if_true; if_false } ->
      let cond = Printf.sprintf "if (%s) {" (gen_glsl_expression cond_expr) in
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
          (gen_glsl_expression from_expr)
          (gen_glsl_expression to_expr)
          forrange_id
      in
      let f = Function.(f |> append_code_section header) in
      let f =
        List.fold_left (fun f stmt -> gen_glsl_stmt stmt f) f forrange_body
      in
      Function.(f |> append_code_section "}")
  | Return [ (_, expr) ] ->
      let return = Printf.sprintf "return %s;" (gen_glsl_expression expr) in
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
             (List.map (fun (_, expr) -> gen_glsl_expression expr) exprs))
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
              (fun shader (i, (name, t)) ->
                Shader.add_uniform i i (glsl_uniform_format env t name) shader)
              shader
              (List.index pipeline.gp_uniforms))
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
  gen_pipelines cfg root_elems >>= fun pipelines ->
  gen_glsl_libraries root_env glsl_types pipelines >>= fun glsl_libraries ->
  gen_renderer_libraries cfg glsl_libraries root_elems
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
