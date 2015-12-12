(*
    Find the number of elements of a list. (easy)
    OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution.

    # length [ "a" ; "b" ; "c"];;
    - : int = 3
    # length [];;
    - : int = 0
*)

let length xs =
    let rec length' acc = function
        | []       -> acc
        | x :: xs  -> length' (acc+1) xs
    in length' 0 xs
;;