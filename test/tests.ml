open OUnit2

let epsilon = 1.0e-2
let (=.) a b = (abs_float (a-.b)) < epsilon

let tests_e1 = "test suite for Exercises1" >::: [
  "fibonacci" >:: (fun _ -> assert_equal 55 (Exercises.Exercises1.fibo 10));
  "cube" >:: (fun _ -> assert_equal true ((Exercises.Exercises1.cube 3.) =. 3. *. 3. *. 3. ));
  "circle_area" >:: (fun _ -> assert_equal true ((Exercises.Exercises1.circle_area 1.) =. 2. *. Float.pi));
]

let _ = run_test_tt_main tests_e1

let make_match_lst_test name input output = name >:: (fun _ -> assert_equal output (Exercises.Exercises2.match_lst input))

let tests_e2 = "test suite for Exercises2" >::: [
    make_match_lst_test "bigred" ["bigred"; "rato"; "gato"] true;
    make_match_lst_test "two elements" ["1"; "2"] true;
    make_match_lst_test "four elements" ["1"; "2"; "3"; "4"] true;
    make_match_lst_test "first two equal" ["1"; "1"; "3"] true;
    make_match_lst_test "other" ["1"; "2"; "3"] false;
    "5th element" >:: (fun _ -> assert_equal 5 (Exercises.Exercises2.fifth_element [1;2;3;4;5]));
    "5th element 2" >:: (fun _ -> assert_equal 0 (Exercises.Exercises2.fifth_element [1;2;3;4]));
    "rev_sort" >:: (fun _ -> assert_equal [5;4;3;2;1] (Exercises.Exercises2.rev_sort [1;2;3;4;5]));
]

let _ = run_test_tt_main tests_e2

