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
