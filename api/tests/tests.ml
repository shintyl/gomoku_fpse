open Core;;
open OUnit2;;
open Board;;

let refboard = ref Board.Vect2Map.empty;;
let test_insert _ = 
  refboard := Board.Vect2Map.empty;
  assert_equal []
    @@ Sequence.to_list (Board.Vect2Map.to_sequence !refboard);
  refboard := (match Board.insert !refboard (0,1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal [((0,1),'w')]
    @@ Sequence.to_list (Board.Vect2Map.to_sequence !refboard);
  refboard := (match Board.insert !refboard (1,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal [((0,1),'w');((1,1)),'b']
    @@ Sequence.to_list (Board.Vect2Map.to_sequence !refboard);
  (*
    Test insertion of position that already exists.
  *)
  refboard := (match Board.insert !refboard (0,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal [((0,1),'w');((1,1)),'b']
    @@ Sequence.to_list (Board.Vect2Map.to_sequence !refboard)
;;

let test_longest_1d _ =
  refboard := Board.Vect2Map.empty;
  assert_equal (0,[]) @@ Board.longest_1d !refboard 'b' (0,1) (1,0);

  refboard := (match Board.insert !refboard (1,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal (1,[(1,0)]) @@ Board.longest_1d !refboard 'b' (1,0) (0,1);
  (*
    Check to make sure that the character is paid attention to.
  *)
  assert_equal (0,[]) @@ Board.longest_1d !refboard 'w' (1,0) (0,1);
  (*
    Check to make sure that there even if a specified line intersects with a
    piece, if the beginning point has no pieces, no pieces are returned.
  *)
  assert_equal (0,[]) @@ Board.longest_1d !refboard 'b' (1,1) (-1,0);
  
  refboard := (match Board.insert !refboard (0,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal (2,[(1,0);(0,1)]) @@ Board.longest_1d !refboard 'b' (1,0) (-1,1);
  assert_equal (2,[(0,1);(1,0)]) @@ Board.longest_1d !refboard 'b' (0,1) (1,-1);
  assert_equal (1,[(0,1)]) @@ Board.longest_1d !refboard 'b' (0,1) (1,0);

  (*
    Check to make sure that character is paid attention to beyond initial position.
  *)
  refboard := (match Board.insert !refboard (2,-1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal (2,[(1,0);(0,1)]) @@ Board.longest_1d !refboard 'b' (1,0) (-1,1)
;;

let test_longest_line _ =
  refboard := Board.Vect2Map.empty;
  assert_equal (0,[]) @@ Board.longest_line !refboard (1,0) (0,1);

  refboard := (match Board.insert !refboard (1,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal (1,[(1,0)]) @@ Board.longest_line !refboard (1,0) (0,1);

  refboard := (match Board.insert !refboard (1,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (0,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,-1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal (2,[(0,1);(1,1)]) @@ Board.longest_line !refboard (0,1) (1,0);
  assert_equal (3,[(1,-1);(1,0);(1,1)]) @@ Board.longest_line !refboard (1,-1) (0,1);
  (*
    Start from middle of line.
  *)
  assert_equal (3,[(1,-1);(1,0);(1,1)]) @@ Board.longest_line !refboard (1,0) (0,1);
  assert_equal (2,[(0,1);(1,0)]) @@ Board.longest_line !refboard (0,1) (1,-1);

  (*
    Check interaction with other types of pieces.
  *)
  refboard := (match Board.insert !refboard (0,0) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-1,0) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (0,-1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,-2) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal (3,[(-1,0);(0,-1);(1,-2)]) @@ Board.longest_line !refboard (-1,0) (1,-1);
  assert_equal (2,[(-1,0);(0,0)]) @@ Board.longest_line !refboard (-1,0) (1,0);
  assert_equal (2,[(0,-1);(0,0)]) @@ Board.longest_line !refboard (0,-1) (0,1);
  assert_equal (2,[(0,0);(0,-1)]) @@ Board.longest_line !refboard (0,0) (0,-1);
  assert_equal (3,[(-1,0);(0,-1);(1,-2)]) @@ Board.longest_line !refboard (0,-1) (1,-1);
;;

let test_insert_check _ =
  refboard := Board.Vect2Map.empty;
  assert_equal []
    @@ Sequence.to_list (Board.Vect2Map.to_sequence !refboard);

  refboard := (match Board.insert !refboard (0,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal (Ok(false)) @@
    Board.insert_check !refboard [(-1,1);(0,1);(1,1);(1,0)] (0,1) 'b';
  
  refboard := (match Board.insert !refboard (1,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (2,0) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (3,-1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal (Error(false)) @@
    Board.insert_check !refboard [(-1,1);(0,1);(1,1);(1,0)] (3,-1) 'b';
  
  refboard := (match Board.insert !refboard (2,-1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (2,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (0,-1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (3,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (3,1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal (Error(false)) @@
    Board.insert_check !refboard [(-1,1);(0,1);(1,1);(1,0)] (3,1) 'w';
  
  refboard := (match Board.insert !refboard (4,-1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (5,-2) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (3,-2) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (3,-3) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (5,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (2,-3) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (6,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (7,2) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (4,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (2,-2) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal (Ok(true)) @@
    Board.insert_check !refboard [(-1,1);(0,1);(1,1);(1,0)] (2,-4) 'w';
    
  refboard := (match Board.insert !refboard (2,-4) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (4,-3) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (5,-3) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (3,-4) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (6,-1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,-2) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (4,-5) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal (Ok(true)) @@
    Board.insert_check !refboard [(-1,1);(0,1);(1,1);(1,0)] (-1,0) 'w';
  assert_equal (Ok(false)) @@
    Board.insert_check !refboard [(-1,1);(0,1);(1,1);(1,0)] (2,-5) 'w';
;;

let test_yojson _ =
  refboard := Board.Vect2Map.empty;
  assert_equal []
    @@ Sequence.to_list (Board.Vect2Map.to_sequence !refboard);

  assert_equal "[]" @@ Board.yojson_of_pieces !refboard;

  refboard := (match Board.insert !refboard (0,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal "[{\"x\":0,\"y\":0,\"c\":\"b\"},{\"x\":1,\"y\":1,\"c\":\"w\"}]"
    @@ Board.yojson_of_pieces !refboard;

  refboard := (match Board.insert !refboard (1,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (2,0) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (3,-1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);

  assert_equal "[{\"x\":0,\"y\":0,\"c\":\"b\"},{\"x\":1,\"y\":0,\"c\":\"b\"},{\"x\":1,\"y\":1,\"c\":\"w\"},{\"x\":2,\"y\":0,\"c\":\"w\"},{\"x\":3,\"y\":-1,\"c\":\"b\"}]"
    @@ Board.yojson_of_pieces !refboard
;;

let test_score_segment _ =
  refboard := Board.Vect2Map.empty;
  refboard := (match Board.insert !refboard (0,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (2,0) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (2,-1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  assert_equal 4 @@ Ai.score_segment !refboard 'b' (-3,0) (1,0) 5 2;
  assert_equal 0 @@ Ai.score_segment !refboard 'b' (0,0) (1,0) 5 2;
  assert_equal 0 @@ Ai.score_segment !refboard 'w' (0,0) (1,0) 5 2;
  assert_equal 9 @@ Ai.score_segment !refboard 'w' (1,1) (1,-1) 2 3;
  assert_equal 3 @@ Ai.score_segment !refboard 'w' (0,2) (1,-1) 2 3;
  assert_equal 3 @@ Ai.score_segment !refboard 'w' (2,0) (1,-1) 2 3
;;

let test_overline _ =
  refboard := Board.Vect2Map.empty;
  refboard := (match Board.insert !refboard (0,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (0,1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,0) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  
  assert_equal true @@ Ai.overline !refboard 'b' (0,0);
  assert_equal true @@ Ai.overline !refboard 'b' (1,1);
  assert_equal false @@ Ai.overline !refboard 'w' (1,1);
  assert_equal true @@ Ai.overline !refboard 'w' (1,0);
  assert_equal false @@ Ai.overline !refboard 'b' (1,0)
;;

let test_overline_score_segment _ =
  refboard := Board.Vect2Map.empty;
  refboard := (match Board.insert !refboard (-2,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-1,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (0,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (2,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (3,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  
  assert_equal 27 @@ Ai.overline_score_segment !refboard 'b' (-3,0) (1,0) 5 3;
  assert_equal 81 @@ Ai.overline_score_segment !refboard 'b' (-2,0) (1,0) 5 3;
  assert_equal 81 @@ Ai.overline_score_segment !refboard 'b' (-1,0) (1,0) 5 3;
  assert_equal 0 @@ Ai.overline_score_segment !refboard 'b' (0,0) (1,0) 5 3;
  assert_equal 9 @@ Ai.overline_score_segment !refboard 'b' (1,0) (1,0) 5 3;
  assert_equal 9 @@ Ai.overline_score_segment !refboard 'b' (2,0) (1,0) 5 3;
  assert_equal 3 @@ Ai.overline_score_segment !refboard 'b' (3,0) (1,0) 5 3
;;

let test_overline_score_pos _ =
  refboard := Board.Vect2Map.empty;
  refboard := (match Board.insert !refboard (0,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (0,-1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,-1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-1,0) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-1,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-2,2) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (0,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,-2) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-2,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (2,-3) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (3,-4) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-3,1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  
  assert_equal 39 @@ Ai.overline_score_pos !refboard 'b' (1,1) (1,0) 5 0 5 3;
  assert_equal 40 @@ Ai.overline_score_pos !refboard 'b' (2,1) (1,0) 5 0 5 3;
  assert_equal 39 @@ Ai.overline_score_pos !refboard 'b' (0,1) (1,0) 5 0 5 3;
  assert_equal 3 @@ Ai.overline_score_pos !refboard 'b' (0,1) (1,1) 5 0 5 3;
  assert_equal 0 @@ Ai.overline_score_pos !refboard 'w' (0,1) (1,1) 5 0 5 3;
  assert_equal 39 @@ Ai.overline_score_pos !refboard 'w' (-2,2) (1,1) 5 0 5 3
;;

let test_advantage_pos _ =
  refboard := Board.Vect2Map.empty;
  refboard := (match Board.insert !refboard (0,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (0,-1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,-1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-1,0) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-1,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-2,2) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (0,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,-2) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-2,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (2,-3) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (3,-4) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-3,1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  
  assert_equal 0 @@ Ai.advantage_pos !refboard 'w' [(-1,1);(0,1);(1,1);(1,0)] (0,0) 5 3;
  assert_equal 16 @@ Ai.advantage_pos !refboard 'w' [(-1,1);(0,1);(1,1);(1,0)] (1,0) 5 3;
  assert_equal 20 @@ Ai.advantage_pos !refboard 'b' [(-1,1);(0,1);(1,1);(1,0)] (1,0) 5 3;
  assert_equal 14 @@ Ai.advantage_pos !refboard 'b' [(-1,1);(0,1);(1,1);(1,0)] (1,0) 5 2;
  assert_equal 0 @@ Ai.advantage_pos !refboard 'w' [(-1,1);(0,1);(1,1);(1,0)] (1,-1) 5 3;
  assert_equal 29 @@ Ai.advantage_pos !refboard 'w' [(-1,1);(0,1);(1,1);(1,0)] (2,-2) 5 3;
  assert_equal 46 @@ Ai.advantage_pos !refboard 'b' [(-1,1);(0,1);(1,1);(1,0)] (2,-2) 5 3
;;

let test_grid _ =
  assert_equal [(0,0);(0,1);(0,2);(1,0);(1,1);(1,2)] @@ Ai.grid (2,3);
  assert_equal [(0,0);(0,1);(0,2);(0,3);(0,4)] @@ Ai.grid (1,5);
  assert_equal [(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6)] @@ Ai.grid (1,7);
  assert_equal [(0,0);(0,1);(1,0);(1,1);(2,0);(2,1)] @@ Ai.grid (3,2);
  assert_equal [] @@ Ai.grid (0,6)
;;

let test_advantage_max _ =
  refboard := Board.Vect2Map.empty;
  refboard := (match Board.insert !refboard (0,0) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (0,-1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,-1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-1,0) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-1,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-2,2) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (0,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (1,-2) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-2,1) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (2,-3) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (3,-4) 'b' with
    | Ok(a) -> a
    | Error(b) -> b);
  refboard := (match Board.insert !refboard (-3,1) 'w' with
    | Ok(a) -> a
    | Error(b) -> b);
  
  assert_equal (79,(2,1)) @@ Ai.advantage_max !refboard [(-1,1);(0,1);(1,1);(1,0)] (19,19) 'b' 'w';
  assert_equal (120,(1,1)) @@ Ai.advantage_max !refboard [(-1,1);(0,1);(1,1);(1,0)] (19,19) 'w' 'b'
;;

let boardtests = "Board" >: test_list [
  "Insert" >:: test_insert;
  "Longest, 1-direction" >:: test_longest_1d;
  "Longest, bidirectional" >:: test_longest_line;
  "Insert check" >:: test_insert_check;
  "Yojson string of pieces" >:: test_yojson;
]

let aitests = "AI" >: test_list [
  "Score segment" >:: test_score_segment;
  "Overline" >:: test_overline;
  "Overline score segment" >:: test_overline_score_segment;
  "Overline score position" >:: test_overline_score_pos;
  "Advantage position" >:: test_advantage_pos;
  "Grid" >:: test_grid;
  (*"Advantage max" >:: test_advantage_max;*)
]

let series =
  "Gomoku tests" >::: [
    boardtests;
    aitests;
  ]

let () =
  run_test_tt_main series;