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

type lexing_position = Lexing.position

let string_of_position pos =
  Printf.sprintf "%s:%d:%d" pos.Lexing.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let lexing_position_to_yojson pos = `String (string_of_position pos)

type 'a t = { loc : lexing_position * lexing_position; value : 'a }
[@@deriving to_yojson]

let string_of_start_position (pos, _) = string_of_position pos

let string_of_location (a, b) =
  Printf.sprintf "%s...%s" (string_of_position a) (string_of_position b)
