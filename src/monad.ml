let (>>=) m f = match m with
  | Ok x -> f x
  | Error e -> Error e
