let row_to_str row = List.fold_left (fun acc x -> acc ^ Printf.sprintf "%d, " x) "" row 

let mat_to_str mat = List.fold_left (fun acc x -> acc ^ row_to_str x ^ "\n") "" mat

let get_row n mat = List.nth mat n

let get_col n mat = List.fold_right (fun row acc -> (List.nth row n) :: acc) mat []
