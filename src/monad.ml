let ( >>= ) m f = match m with Ok x -> f x | Error e -> Error e

let ( >? ) o v = match o with Some x -> Some x | None -> v
