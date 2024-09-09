let rec product = function 
    | [] -> 1
    | h :: t -> h * product t

let rec concat_strs = function
    | [] -> ""
    | h :: t -> h ^ concat_strs t
