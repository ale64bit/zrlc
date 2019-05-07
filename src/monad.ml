let ( >>= ) m f = match m with Ok x -> f x | Error e -> Error e

(* let ( >>= ) m f = match m with Some x -> f x | None -> None *)

let ( >>? ) o v = match o with Some x -> Some x | None -> v
