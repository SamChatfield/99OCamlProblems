(*
    Find the k'th element of a list. (easy)

    # at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
    - : string option = Some "c"
    # at 3 [ "a" ];;
    - : string option = None
*)

let rec at n = function
    | [] -> None
    | x::xs when n = 1 -> Some x
    | _::xs -> at (n-1) xs
;;