open OUnit2

let epsilon = 1.0e-2
let (=.) a b = (abs_float (a-.b)) < epsilon

let tests_e1 = "test suite for Exercises1" >::: [
  "fibonacci" >:: (fun _ -> assert_equal 55 (Exercises.Exercises2.fibo 10));
  "cube" >:: (fun _ -> assert_equal true ((Exercises.Exercises2.cube 3.) =. 3. *. 3. *. 3. ));
  "circle_area" >:: (fun _ -> assert_equal true ((Exercises.Exercises2.circle_area 1.) =. 2. *. Float.pi));
]

let _ = run_test_tt_main tests_e1

let make_match_lst_test name input output = name >:: (fun _ -> assert_equal output (Exercises.Exercises3.match_lst input))

let tests_e2 = "test suite for Exercises2" >::: [
    make_match_lst_test "bigred" ["bigred"; "rato"; "gato"] true;
    make_match_lst_test "two elements" ["1"; "2"] true;
    make_match_lst_test "four elements" ["1"; "2"; "3"; "4"] true;
    make_match_lst_test "first two equal" ["1"; "1"; "3"] true;
    make_match_lst_test "other" ["1"; "2"; "3"] false;
    "5th element" >:: (fun _ -> assert_equal 5 (Exercises.Exercises3.fifth_element [1;2;3;4;5]));
    "5th element 2" >:: (fun _ -> assert_equal 0 (Exercises.Exercises3.fifth_element [1;2;3;4]));
    "rev_sort" >:: (fun _ -> assert_equal [5;4;3;2;1] (Exercises.Exercises3.rev_sort [1;2;3;4;5]));
    "take" >:: (fun _ -> assert_equal [1;2;3;4;5] (Exercises.Exercises3.take [1;2;3;4;5;6;7;8;9;10] 5));
    "drop" >:: (fun _ -> assert_equal [6;7;8;9;10] (Exercises.Exercises3.drop [1;2;3;4;5;6;7;8;9;10] 5));
    "monotonic_inc" >:: (fun _ -> assert_equal true (Exercises.Exercises3.monotonic [1;2;3;4;5;6] (<)));
    "monotonic_dec" >:: (fun _ -> assert_equal true (Exercises.Exercises3.monotonic [5;4;3;2;1] (>)) );
    "is_unimodal_lst" >:: (fun _ -> assert_equal true (Exercises.Exercises3.is_unimodal [1;2;3;4;3;2;1]));
]

let _ = run_test_tt_main tests_e2

let valid_mat = [[1;2;3;4];[4;3;2;1]]
let invalid_mat1 = []
let invalid_mat2 = [[1;2;3]; [1;2]]

let tests_e4 = "test suite for Exercises 4" >::: [
  "valid_mat" >:: (fun _ -> assert_equal true (Exercises.Exercises4.valid_matrix valid_mat));
  "invalid_mat" >:: (fun _ -> assert_equal false (Exercises.Exercises4.valid_matrix invalid_mat1));
  "invalid_mat2" >:: (fun _ -> assert_equal false (Exercises.Exercises4.valid_matrix invalid_mat2));
]

let _ = run_test_tt_main tests_e4
