let rec repeat f n x =
  if n = 0 then x else repeat f (n-1) (f x)

let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j

let fold_right_pipe f acc lst = List.fold_right f lst acc

let sum_cube_odd n =
  0 -- n
  |> List.filter (fun x -> if x mod 2 = 0 then false else true) 
  |> List.map (fun x -> x * x * x)
  |> fold_right_pipe (+) 0

let rec exists_rec n = function
  | [] -> false
  | h :: _ when h = n -> true
  | _ :: t -> exists_rec n t

let exists_fold n lst = List.fold_right (fun x acc -> (x = n) || acc) lst false

let exists_list n lst = List.mem true (List.map (fun x -> x = n) lst)

let keys lst = List.sort_uniq (fun x y -> if x = y then 0 else 1) (List.map (fun (x, _) -> x) lst)

let valid_matrix mat = 
  match (List.map List.length mat) with 
  | [] -> false
  | _ :: [] -> true
  | h :: t -> if List.for_all (fun x -> x = h) t then true else false

let add_row_vector fst sec = List.map2 (+) fst sec

let add_matrix fst sec = List.map2 add_row_vector fst sec

let dot_prod = List.fold_left2 (fun acc x y -> acc + x * y) 0

let transpose mat =
  let rec aux acc = function
    | [] | [] :: _ -> List.rev acc
    | lst -> aux (List.map List.hd lst::acc) (List.map List.tl lst)
  in
  aux [] mat

let mat_mul fst sec = List.map (fun row -> List.map (fun col -> dot_prod row col) (transpose sec)) fst

