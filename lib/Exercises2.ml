let rec product = function 
    | [] -> 1
    | h :: t -> h * product t

let rec concat_strs = function
    | [] -> ""
    | h :: t -> h ^ concat_strs t

let match_lst = function
    | "bigred" :: _ -> true
    | _ :: _ :: [] -> true
    | _ :: _ :: _ :: _ :: [] -> true
    | a :: b :: _ -> a = b
    | _ -> false

let fifth_element lst = 
    match List.length lst with
    | n when n > 4 -> List.nth lst 4
    | _ -> 0

let rev_sort lst = List.rev (List.sort Stdlib.compare lst)
