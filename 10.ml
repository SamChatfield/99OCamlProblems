(*
    # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
    - : (int * string) list =
    [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*)

let encode xs =
    let rec aux cur acc = function
        | []                            -> []
        | [a]                           -> ((fst cur) + 1, a) :: acc
        | a :: (b :: _ as t) when a = b -> aux ((fst cur) + 1, a) acc t
        | a :: (b :: _ as t)            -> aux (0, "") (((fst cur) + 1, a) :: acc) t
    in List.rev (aux (0, "") [] xs)
;;