open Core
open Board

type advantage = Int.t Board.Vect2Map.t

(*
  Score the strategic value of any segment of length len
  starting from (x, y) and in the direction of (dx, dy).
*)
let rec score_segment (p : Board.pieces) (pt : char) ((x, y) : Vect2.t)
    ((dx, dy) : Vect2.t) (len : int) : int =
  if len = 0 then 0
  else match Vect2Map.find p (x, y) with
  | None -> score_segment p pt (x + dx, y + dy) (dx, dy) (len - 1)
  | Some c -> if Char.(<>) c pt then 0
    else score_segment p pt (x + dx, y + dy) (dx, dy) (len - 1) + 1

(*
  Used in conjunction with score_segment to detect possible overlines.
*)
let overline (p : Board.pieces) (pt : char) ((x, y) : Vect2.t) : bool =
  match Vect2Map.find p (x, y) with
  | None -> false
  | Some c -> Char.(=) c pt

(*
  Score the strategic value of any segment of length len
  starting from (x, y) and in the direction of (dx, dy)
  taking into account the possibility of overlines.
*)
let overline_score_segment (p : Board.pieces) (pt : char) ((x, y) : Vect2.t)
    ((dx, dy) : Vect2.t) (len : int) : int =
  if (overline p pt (x - dx, x - dy))
    || (overline p pt (x + len * dx, x + len * dy)) then 0
  else score_segment p pt (x, y) (dx, dy) len

(*
  Score the strategic value of the position (x, y)
  in the direction of (dx, dy), taking the sum of
  the strategic value of all segments of length len
  in the direction (dx, dy) including (x, y).
*)
let rec overline_score_pos (p : Board.pieces) (pt : char) ((x, y) : Vect2.t)
    ((dx, dy) : Vect2.t) (cur : int) (sum : int) (len : int) : int =
  if cur = 0 then sum
  else overline_score_pos p pt (x - dx, y - dy) (dx, dy) (cur - 1) (sum +
    overline_score_segment p pt (x,y) (dx,dy) len) len 

(*
  Find the strategic value/advantage of the position
  (x, y) by taking the directional advantage over
  all directions.
*)
let advantage_pos (p : Board.pieces) (pt : char) (d : dirs) ((x, y) : Vect2.t)
    (len : int) : int =
  let foldfun (i : int) ((dx, dy) : Vect2.t) =
    i + overline_score_pos p pt (x, y) (dx, dy) len 0 len
  in List.fold d ~init:0 ~f:foldfun