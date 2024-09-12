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

