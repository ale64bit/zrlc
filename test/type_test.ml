(**
   Copyright 2019 Google LLC

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

open OUnit2
open Zrl

let test_string_of_type typ want _ =
  assert_equal ~printer:(fun x -> x) want (Type.string_of_type typ)

let tests =
  "type_suite"
  >::: [ "test_type_ref"
         >:: test_string_of_type (Type.TypeRef "SomeType") "SomeType";
         "test_primitive_bool"
         >:: test_string_of_type (Type.Primitive Bool) "bool";
         "test_primitive_int"
         >:: test_string_of_type (Type.Primitive Int) "int";
         "test_primitive_uint"
         >:: test_string_of_type (Type.Primitive UInt) "uint";
         "test_primitive_float"
         >:: test_string_of_type (Type.Primitive Float) "float";
         "test_primitive_double"
         >:: test_string_of_type (Type.Primitive Double) "double";
         "test_record"
         >:: test_string_of_type
               Type.(Record [ ("x", Primitive Int); ("f", Primitive Float) ])
               "record { x: int; f: float }";
         "test_array"
         >:: test_string_of_type
               Type.(Array (Primitive Float, [ OfInt 64; OfName "kSize" ]))
               "[64; kSize]float";
         "test_function"
         >:: test_string_of_type
               Type.(
                 Function
                   ( [ ("a", Primitive Double); ("b", Primitive Int) ],
                     [ Primitive Double ] ))
               "fun (a: double, b: int) -> (double)"
       ]

let _ = run_test_tt_main tests
