(*
    Reverse a list. (easy)
    OCaml standard library has List.rev but we ask that you reimplement it.

    # rev ["a" ; "b" ; "c"];;
    - : string list = ["c"; "b"; "a"]
*)

let rev xs =
    let rec rev' sx = function
        | []     -> sx
        | x::xs  -> rev' (x::sx) xs
    in rev' [] xs
;;