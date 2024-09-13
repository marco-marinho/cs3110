let rato: Exercises.Exercises2.pokemon = {name="rato";hp=60;ptype=Fire}
let marco: Exercises.Exercises2.pokemon = {name="marco";hp=80;ptype=Water}

let int_of_intoption = function None -> 0 | Some x -> x
let print_date = function
  | None -> print_endline "None date"
  | Some (y, m, d) -> Printf.printf "%d/%d/%d\n" y m d

let () = Printf.printf "%d" (int_of_intoption (Exercises.Exercises2.max_hp [rato; marco]))

let dates = [(2018, 2, 26); (2020, 10, 8); (1986, 12, 8)]

let _ = print_date (Exercises.Exercises2.earliest_date (dates))

