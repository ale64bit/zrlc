let rec drain stream = match Stream.peek stream with
  | None -> []
  | Some _ -> 
      let t = Stream.next stream in
      t :: (drain stream)

let string_of_token_stream l = 
  "[" ^ (String.concat ";" (List.map Rpdl.Token.to_string l)) ^ "]"

