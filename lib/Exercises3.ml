let rec product= function 
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

let last_element = function 
    | [] -> None
    | n -> Some (List.nth n ((List.length n) - 1))

let any_zeros lst = List.mem 0 lst

let rec take_tr lst n acc =
    match n, lst with
    | 0, _ -> List.rev acc
    | _, [] -> List.rev acc
    | n, h :: t -> take_tr t (n-1) (h::acc)

let take lst n = take_tr lst n []

let rec drop lst n =
    match n, lst with
    |0, _ -> lst
    |_, [] -> lst
    |n, _ :: t -> drop t (n-1)

let peak lst = 
    let rec aux_peak ilst acc = 
        match ilst with
        | [] -> acc
        | _ :: [] -> acc
        | n :: m :: _ when n > m -> acc
        | _ :: t -> aux_peak t (acc+1)
    in aux_peak lst 0

let rec monotonic lst operator =
    match lst with 
    | [] -> true
    | _ :: [] -> true
    | h :: w :: t when operator h w -> monotonic (w::t) operator
    | _-> false

let is_unimodal lst = monotonic (take lst (peak lst)) (<) 
                    && monotonic (drop lst (peak lst)) (>)

let rec powerset = function
    | [] -> [[]]
    | h :: t ->
        let pt = powerset t in
        pt @ List.map(fun x -> h :: x) pt

let rec print_int_list = function
| [] -> ()
| h :: t -> Printf.printf("%d\n%!") h; print_int_list t

type poketype = Normal | Fire | Water

type pokemon = {name: string; hp: int; ptype: poketype}

let max_hp lst = 
    let rec aux lst acc =
        match lst with  
        | [] -> acc
        | h :: t -> match acc with
            | None -> Some h.hp
            | Some ohp -> aux t (if h.hp > ohp then Some h.hp else Some ohp)
    in aux lst None


let date_before (y, m, d) (y2, m2, d2) = 
  if y < y2 then true else 
  if y = y2 && m < m2 then true else
  if y = y2 && m = m2 && d < d2 then true else false

let rec earliest_date = function
  | h :: [] -> Some h
  | [] -> None
  | f :: s :: t ->  if date_before f s 
  then earliest_date (f :: t)
  else earliest_date (s :: t)

type 'a tree = 
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let rec depth = function
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)

let rec same_shape ftree stree =
  match ftree, stree with
  | Leaf, Leaf -> true
  | Node(_, fl, fr), Node(_, sl, sr) -> same_shape fl sl && same_shape fr sr
  | _ -> false

let is_bst itree =
  let rec is_bst_aux tree min_val max_val =
    match tree with
    | Leaf -> true
    | Node(v, l, r) -> if v <= min_val || v >= max_val then false else
      is_bst_aux l min_val v && is_bst_aux r v max_val
    in
  is_bst_aux itree min_int max_int
