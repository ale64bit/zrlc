module List = struct
  include List

  let fold_results f accu l =
    let open Monad.Result in
    List.fold_left f accu l >>= fun l ->
    Ok (List.rev l)

  let index l = combine (init (length l) (fun i -> i)) l
end

let flip f x y = f y x
