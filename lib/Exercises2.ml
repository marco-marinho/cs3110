let double n = 2 * n

let cube n = n *. n *. n

let sign n = if n = 0 then 0 else if n > 0 then 1 else -1

let epsilon = 1.0e-2
let (=.) a b = (abs_float (a-.b)) < epsilon

let circle_area r = 2. *. Float.pi *. r *. r

let rms a b = Float.sqrt((a *. a +. b *. b) /. 2.) 

let valid_date d m = if 1 < d && d < 32 && List.mem m ["Jan"; "Mar"; "May"; "Jul"; "Aug"; "Oct"; "Dec"] then true else
    if 1 < d && d < 31 && List.mem m ["Apr"; "Jun"; "Sep"; "Nov";] then true else
    if 1 < d && d < 29 && m = "Feb" then true else false

let rec fibo = function
| 0 -> 0
| 1 -> 1
| n when n > 1 -> fibo (n - 1) + fibo (n - 2)
| _ -> raise (Invalid_argument "Invalid input")

let rec fibo_fast_helper n pp p = if n = 0 then pp else fibo_fast_helper (n-1) (p) (pp+p)
let fibo_fast n = fibo_fast_helper n 0 1

