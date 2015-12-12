(*
    Eliminate consecutive duplicates of list elements. (medium)
    
    # compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
    - : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)

let compress xs =
    let rec compress' last acc = function
        | []                    -> acc
        | x::xs when last <> x  -> compress' x (x::acc) xs
        | x::xs                 -> compress' last acc xs
    in List.rev (compress' "" [] xs)
;;