module List = struct
  include List

  let fold_results f accu l =
    let open Monad.Result in
    List.fold_left f accu l >>= fun l -> Ok (List.rev l)
end
