type lexing_position = Lexing.position

let string_of_position pos =
  Printf.sprintf "%s:%d:%d" pos.Lexing.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let lexing_position_to_yojson pos = `String (string_of_position pos)

(*
let lexing_position_to_yojson Lexing.{pos_fname; pos_lnum; pos_bol; pos_cnum} =
  `Assoc
    [ ("pos_fname", `String pos_fname)
    ; ("pos_lnum", `Int pos_lnum)
    ; ("pos_bol", `Int pos_bol)
    ; ("pos_cnum", `Int pos_cnum) ]
*)

type 'a t = {loc: lexing_position * lexing_position; value: 'a}
[@@deriving to_yojson]

let string_of_start_position (pos, _) = string_of_position pos
