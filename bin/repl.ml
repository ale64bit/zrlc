open Zrl

let rec repl_loop env =
  let () = print_string "#> " in
  let line = read_line () in
  let () = Printf.printf "GOT: '%s'\n" line in
  repl_loop env

let () =
  let env = Env.global in
  repl_loop env
