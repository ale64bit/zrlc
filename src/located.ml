type lexing_position = Lexing.position

let string_of_position pos =
  Printf.sprintf "%s:%d:%d" pos.Lexing.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let lexing_position_to_yojson pos = `String (string_of_position pos)

type 'a t = {loc: lexing_position * lexing_position; value: 'a}
[@@deriving to_yojson]

let string_of_start_position (pos, _) = string_of_position pos

let string_of_location (a, b) =
  Printf.sprintf "%s...%s" (string_of_position a) (string_of_position b)
