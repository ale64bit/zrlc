module Function = struct
  type t = {
    name : string;
    parent_class : string;
    return_type : string;
    parameters : (string * string) list;
    template_parameters : string list;
    member_initializers : (string * string) list;
    body : string list;
  }

  let empty name =
    {
      name;
      parent_class = "";
      return_type = "";
      parameters = [];
      template_parameters = [];
      member_initializers = [];
      body = [];
    }

  let name f = f.name

  let params f = List.rev f.parameters

  let template_params f = List.rev f.template_parameters

  let return_type f = f.return_type

  let is_template f =
    match f.template_parameters with [] -> false | _ -> true

  let set_return_type return_type f = { f with return_type }

  let set_parent_class parent_class f = { f with parent_class }

  let add_param p f = { f with parameters = p :: f.parameters }

  let add_template_param p f =
    { f with template_parameters = p :: f.template_parameters }

  let add_member_initializer mi f =
    { f with member_initializers = mi :: f.member_initializers }

  let append_code_section s f = { f with body = s :: f.body }

  let append_code_sections ss f =
    List.fold_left (fun f s -> append_code_section s f) f ss

  let prepend_code_section s f = { f with body = f.body @ [ s ] }

  let prepend_code_sections ss f =
    List.fold_left (fun f s -> prepend_code_section s f) f ss

  let string_of_template_params f =
    match f.template_parameters with
    | [] -> ""
    | tp -> Printf.sprintf "template<%s> " (String.concat ", " (List.rev tp))

  let string_of_signature f =
    let tmpl_params = string_of_template_params f in
    let params =
      String.concat ", "
        (List.map (fun (t, n) -> t ^ " " ^ n) (List.rev f.parameters))
    in
    let res =
      Printf.sprintf "%s%s %s(%s)" tmpl_params f.return_type f.name params
    in
    String.trim res

  let string_of_implementation f =
    let tmpl_params = string_of_template_params f in
    let qual = if f.parent_class <> "" then f.parent_class ^ "::" else "" in
    let member_init =
      match f.member_initializers with
      | [] -> ""
      | mi ->
          Printf.sprintf ": %s "
            (String.concat ", "
               (List.map (fun (m, i) -> m ^ "(" ^ i ^ ")") (List.rev mi)))
    in
    let res =
      Printf.sprintf "%s%s %s%s(%s) %s{\n%s\n}" tmpl_params f.return_type qual
        f.name
        (String.concat ", "
           (List.map (fun (t, n) -> t ^ " " ^ n) (List.rev f.parameters)))
        member_init
        (String.concat "\n" (List.rev f.body))
    in
    String.trim res
end

module Class = struct
  type t = {
    name : string;
    package : string list;
    includes : string list;
    public_functions : Function.t list;
    private_functions : Function.t list;
    private_members : (string * string) list;
    static_sections : string list;
  }

  let empty name =
    {
      name;
      package = [];
      includes = [];
      public_functions = [];
      private_functions = [];
      private_members = [];
      static_sections = [];
    }

  let name c = c.name

  let private_functions c = c.private_functions

  let set_package p c = { c with package = p }

  let add_include inc c = { c with includes = inc :: c.includes }

  let add_public_function f (c : t) =
    let f =
      Function.(if is_template f then f else set_parent_class c.name f)
    in
    { c with public_functions = f :: c.public_functions }

  let add_private_function f (c : t) =
    let f =
      Function.(if is_template f then f else set_parent_class c.name f)
    in
    { c with private_functions = f :: c.private_functions }

  let add_private_member m c =
    { c with private_members = m :: c.private_members }

  let add_static_section s c =
    { c with static_sections = s :: c.static_sections }

  let header_guard c =
    let path = String.concat "_" (List.map String.uppercase_ascii c.package) in
    Printf.sprintf "%s%s_H_"
      (if String.length path > 0 then path ^ "_" else "")
      (String.uppercase_ascii c.name)

  let string_of_header c =
    let hg = header_guard c in
    let includes =
      String.concat "\n"
        (List.map (fun i -> "#include " ^ i) (List.rev c.includes))
    in
    let public_function_signatures =
      String.concat "\n  "
        (List.map
           Function.(
             fun f ->
               if is_template f then
                 f |> set_parent_class "" |> string_of_implementation
               else string_of_signature f ^ ";")
           (List.rev c.public_functions))
    in
    let private_function_signatures =
      String.concat "\n  "
        (List.map
           Function.(
             fun f ->
               if is_template f then
                 f |> set_parent_class "" |> string_of_implementation
               else string_of_signature f ^ ";")
           (List.rev c.private_functions))
    in
    let private_members =
      String.concat "\n  "
        (List.map
           (fun (t, name) -> t ^ " " ^ name ^ ";")
           (List.rev c.private_members))
    in
    Printf.sprintf
      {|#ifndef %s
#define %s

%s

class %s {
public:
  %s
private:
  %s
  %s
};

#endif // %s|}
      hg hg includes c.name public_function_signatures private_members
      private_function_signatures hg

  let string_of_source c =
    let include_path =
      match c.package with [] -> "" | p -> String.concat "/" p ^ "/"
    in
    let non_templates =
      List.filter
        (fun f -> not (Function.is_template f))
        (List.rev c.public_functions @ List.rev c.private_functions)
    in
    let implementations =
      String.concat "\n"
        ( List.rev c.static_sections
        @ List.map Function.string_of_implementation non_templates )
    in
    Printf.sprintf {|#include "%s%s.h"
%s|} include_path c.name implementations
end

module Header = struct
  type t = {
    name : string;
    package : string list;
    includes : string list;
    sections : string list;
  }

  let empty name = { name; package = []; includes = []; sections = [] }

  let name h = h.name

  let set_package p h = { h with package = p }

  let add_include inc h = { h with includes = inc :: h.includes }

  let add_section s h = { h with sections = s :: h.sections }

  let string_of_header h =
    let includes =
      String.concat "\n"
        (List.map (fun i -> "#include " ^ i) (List.rev h.includes))
    in
    let sections = String.concat "\n" (List.rev h.sections) in
    Printf.sprintf {|#pragma once
%s
%s
|} includes sections
end

module Library = struct
  type t = {
    name : string;
    package : string list;
    classes : Class.t list;
    headers : Header.t list;
    copts : string;
    defines : string;
    deps : string list;
  }

  let empty name package =
    {
      name;
      package;
      classes = [];
      headers = [];
      copts = "[]";
      defines = "[]";
      deps = [];
    }

  let classes l = l.classes

  let headers l = l.headers

  let set_copts copts l = { l with copts }

  let set_defines defines l = { l with defines }

  let add_class c l =
    { l with classes = Class.set_package l.package c :: l.classes }

  let add_header h l =
    { l with headers = Header.set_package l.package h :: l.headers }

  let add_dep dep l = { l with deps = dep :: l.deps }

  let string_of_library l =
    let quote_and_comma s = "\"" ^ s ^ "\"," in
    let srcs =
      String.concat "\n        "
        List.(
          map (fun c -> quote_and_comma (Class.name c ^ ".cc")) (rev l.classes))
    in
    let hdrs =
      String.concat "\n        "
        List.(
          map (fun c -> quote_and_comma (Class.name c ^ ".h")) (rev l.classes))
    in
    let single_hdrs =
      String.concat "\n        "
        List.(
          map (fun h -> quote_and_comma (Header.name h ^ ".h")) (rev l.headers))
    in
    let deps =
      String.concat "\n        " List.(map quote_and_comma (rev l.deps))
    in
    Printf.sprintf
      {|cc_library(
    name = "%s",
    srcs = [
        %s
    ],
    hdrs = [
        %s
    ],
    copts = %s,
    defines = %s,
    deps = [
        %s
    ],
)|}
      l.name srcs
      (hdrs ^ "\n" ^ single_hdrs)
      l.copts l.defines deps
end
