module Option = struct
  let ( >>= ) m f = match m with Some a -> f a | None -> None

  let ( >>? ) m v = match m with Some x -> Some x | None -> v
end

module Result = struct
  let ( >>= ) m f = match m with Ok a -> f a | Error e -> Error e

  let from_opt o err = match o with Some a -> Ok a | None -> err
end
