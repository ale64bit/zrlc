open OUnit2
open Backend.Cpp

let function_signature_test f want _ =
  assert_equal ~printer:(fun x -> x) want (Function.string_of_signature f)

let function_implementation_test f want _ =
  assert_equal ~printer:(fun x -> x) want (Function.string_of_implementation f)

let class_header_test c want _ =
  assert_equal ~printer:(fun x -> x) want (Class.string_of_header c)

let class_source_test c want _ =
  assert_equal ~printer:(fun x -> x) want (Class.string_of_source c)

(* Tests *)

let test_empty_function_signature =
  let f = Function.(empty "f") in
  let want = "f()" in
  "test_empty_function_signature" >:: function_signature_test f want

let test_empty_function_implementation =
  let f = Function.(empty "f" |> set_return_type "void") in
  let want = {|void f() {

}|} in
  "test_empty_function_implementation" >:: function_implementation_test f want

let test_ctor_signature =
  let f =
    Function.(
      empty "C" |> set_parent_class "C"
      |> add_param ("int", "i")
      |> add_param ("std::string", "s")
      |> add_member_initializer ("foo", "bar"))
  in
  let want = "C(int i, std::string s)" in
  "test_ctor_signature" >:: function_signature_test f want

let test_ctor_implementation =
  let f =
    Function.(
      empty "C" |> set_parent_class "C"
      |> add_param ("int", "i")
      |> add_param ("std::string", "s")
      |> add_member_initializer ("m1", "i1")
      |> add_member_initializer ("m2", "i2")
      |> append_code_section "int x = 1;"
      |> append_code_section "if (x == 2) { exit(1); }"
      |> append_code_section "return true;")
  in
  let want =
    {|C::C(int i, std::string s) : m1(i1), m2(i2) {
int x = 1;
if (x == 2) { exit(1); }
return true;
}|}
  in
  "test_ctor_implementation" >:: function_implementation_test f want

let test_template_signature =
  let f =
    Function.(
      empty "f" |> set_parent_class "C" |> set_return_type "bool"
      |> add_template_param "class A"
      |> add_template_param "class B"
      |> add_param ("int", "i")
      |> add_param ("std::vector<float>", "v")
      |> append_code_section "int x = 1;"
      |> append_code_section "if (x == 2) { exit(1); }"
      |> append_code_section "return true;")
  in
  let want =
    "template<class A, class B> bool f(int i, std::vector<float> v)"
  in
  "test_template_signature" >:: function_signature_test f want

let test_empty_class_header =
  let c = Class.(empty "C" []) in
  let want =
    {|#ifndef C_H_
#define C_H_



class C {
public:
  
private:
  
  
};

#endif // C_H_|}
  in
  "test_empty_class_header" >:: class_header_test c want

let test_empty_class_source =
  let c = Class.(empty "C" []) in
  let want = {|#include "C.h"
|} in
  "test_empty_class_source" >:: class_source_test c want

let test_class_header =
  let ctor = Function.(empty "C" |> set_parent_class "should_be_overriden") in
  let dtor = Function.(empty "~C" |> set_parent_class "should_be_overriden") in
  let public_func =
    Function.(
      empty "f" |> set_return_type "void"
      |> add_param ("int", "i")
      |> add_param ("float", "f"))
  in
  let private_func =
    Function.(empty "g" |> set_return_type "int*" |> add_param ("bool", "b"))
  in
  let template_func =
    Function.(
      empty "h"
      |> set_parent_class "should_be_ignored"
      |> add_template_param "class A"
      |> add_template_param "class B"
      |> set_return_type "A")
  in
  let c =
    Class.(
      empty "C" ["foo"; "bar"]
      |> add_include "<string>" |> add_include "<vector>"
      |> add_private_member ("std::string", "name_")
      |> add_private_member ("std::vector<int>", "stuff_")
      |> add_public_function ctor |> add_public_function dtor
      |> add_public_function public_func
      |> add_private_function private_func
      |> add_public_function template_func)
  in
  let want =
    {|#ifndef FOO_BAR_C_H_
#define FOO_BAR_C_H_

#include <string>
#include <vector>

class C {
public:
  C();
  ~C();
  void f(int i, float f);
  template<class A, class B> A h() {

}
private:
  std::string name_;
  std::vector<int> stuff_;
  int* g(bool b);
};

#endif // FOO_BAR_C_H_|}
  in
  "test_class_header" >:: class_header_test c want

let tests =
  "cpp_suite"
  >::: [ test_empty_function_signature
       ; test_empty_function_implementation
       ; test_ctor_signature
       ; test_ctor_implementation
       ; test_template_signature
       ; test_empty_class_header
       ; test_empty_class_source
       ; test_class_header ]

let _ = run_test_tt_main tests
