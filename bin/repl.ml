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

open Zrl

let rec repl_loop env =
  let () = print_string "#> " in
  let line = read_line () in
  let () = Printf.printf "GOT: '%s'\n" line in
  repl_loop env

let () =
  let env = Env.global in
  repl_loop env
