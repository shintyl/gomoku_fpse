open Core;;
open OUnit2;;
open Board;;

let refboard = ref Board.Vect2Map.empty;;
let test_insert _ = 
  assert_equal [] @@ Sequence.to_list (Board.Vect2Map.to_sequence !refboard);
  refboard := (match Board.insert !refboard (0,1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal [((0,1),'w')] @@ Sequence.to_list (Board.Vect2Map.to_sequence !refboard) 
;;

let boardtests = "Board" >: test_list [
  "Insert" >:: test_insert;
]

let series =
  "Gomoku tests" >::: [
    boardtests;
  ]

let () =
  run_test_tt_main series;