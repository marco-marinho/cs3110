let row_to_str row = List.fold_left (fun acc x -> acc ^ Printf.sprintf "%d, " x) "" row 

let mat_to_str mat = List.fold_left (fun acc x -> acc ^ row_to_str x ^ "\n") "" mat

let mat = [[1;2;3];[4;5;6];[7;8;9]]

let _ = print_endline (mat_to_str mat)

let transpose = Exercises.Exercises4.transpose mat

let _ = print_endline (mat_to_str transpose)

let a =  [[1;0;1];[2;1;1];[0;1;1];[1;1;2]]
let b = [[1;2;1];[2;3;1];[4;2;2]]

let res = Exercises.Exercises4.mat_mul a b

let _ = print_endline (mat_to_str res)
