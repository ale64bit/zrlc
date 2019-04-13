type 'a t =
  { loc: Lexing.position * Lexing.position; value: 'a }

let string_of_position pos =
  Printf.sprintf "%s:%d:%d" pos.Lexing.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let string_of_start_position (pos, _) =
  string_of_position pos

