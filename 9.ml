(*
    Pack consecutive duplicates of list elements into sublists. (medium)

    # pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
    - : string list list =
    [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
    ["e"; "e"; "e"; "e"]]
*)

let pack xs =
    let rec pack' current acc = function
        | []                             -> []
        | [a]                            -> (a :: current) :: acc
        | a :: (b :: _ as t) when a = b  -> pack' (a :: current) acc t
        | a :: (b :: _ as t)             -> pack' [] ((a :: current) :: acc) t
    in List.rev (pack' [] [] xs)
;;