open Core
open Board

type advantage = Int.t Board.Vect2Map.t

(*
  Score the strategic value of any segment of length len
  starting from (x, y) and in the direction of (dx, dy).
  Strategic value is exponential on base wt and exponent
  equal to the number of matching pieces in the segment.
  Blockage by an opposite piece automatically renders
  the segment worthless, thus yielding a value of 0.
*)
let rec score_segment (p : Board.pieces) (pt : char) ((x, y) : Vect2.t)
    ((dx, dy) : Vect2.t) (len : int) (wt : int) : int =
  if len = 0 then 1
  else match Vect2Map.find p (x, y) with
  | None -> score_segment p pt (x + dx, y + dy) (dx, dy) (len - 1) wt
  | Some c -> if Char.(<>) c pt then 0
    else (score_segment p pt (x + dx, y + dy) (dx, dy) (len - 1) wt) * wt

(*
  Used in conjunction with score_segment to detect possible overlines.
  Simply returns whether a piece at a given set of coordinates exists
  and is of the same kind as that specified.
*)
let overline (p : Board.pieces) (pt : char) ((x, y) : Vect2.t) : bool =
  match Vect2Map.find p (x, y) with
  | None -> false
  | Some c -> Char.(=) c pt

(*
  Score the strategic value of any segment of length len
  starting from (x, y) and in the direction of (dx, dy)
  taking into account the possibility of overlines.
  Essentially the same as score_segment except having
  overline potential also renders the score 0.
*)
let overline_score_segment (p : Board.pieces) (pt : char) ((x, y) : Vect2.t)
    ((dx, dy) : Vect2.t) (len : int) (wt : int) : int =
  if (overline p pt (x - dx, x - dy))
    || (overline p pt (x + len * dx, x + len * dy)) then 0
  else score_segment p pt (x, y) (dx, dy) len wt

(*
  Score the strategic value of the position (x, y)
  in the direction of (dx, dy), taking the sum of
  the strategic value of all segments of length len
  in the direction (dx, dy) including (x, y).
*)
let rec overline_score_pos (p : Board.pieces) (pt : char) ((x, y) : Vect2.t)
    ((dx, dy) : Vect2.t) (cur : int) (sum : int) (len : int) (wt : int) : int =
  if cur = 0 then sum
  else overline_score_pos p pt (x - dx, y - dy) (dx, dy) (cur - 1) (sum +
    overline_score_segment p pt (x,y) (dx,dy) len) len wt

(*
  Find the strategic value/advantage of the position
  (x, y) by taking the directional advantage over
  all directions. Also set strategic value to 0 if
  the position is already occupied.
*)
let advantage_pos (p : Board.pieces) (pt : char) (d : Board.dirs) ((x, y) : Vect2.t)
    (len : int) (wt : int) : int =
  match Vect2Map.find p (x, y) with
  | Some _ -> 0
  | None -> let foldfun (i : int) ((dx, dy) : Vect2.t) =
      i + overline_score_pos p pt (x, y) (dx, dy) len 0 len wt
    in List.fold d ~init:0 ~f:foldfun

(*
  Get a list of possible positions in an m x n grid.
*)
let grid ((m,n) : Vect2.t) : Vect2.t list =
  let rec gridbuilder (i : int) (j : int) : Vect2.t list =
    if i = m then [] else
    if j = n then gridbuilder (i + 1) 0 else
    (i,j)::(gridbuilder i (j + 1))
  in gridbuilder 0 0

(*
  Get the max double-advantage (sum of advantages of both
  AI and player; useful because a position that is good for
  the player's offense should be occupied by the AI for max
  disruption) and position of said double-advantage from
  around a given point.
  Weighting assigned to AI pieces is 4, weighting assigned
  to player pieces is 3; fulfilling the AI's own lines is
  more important than blocking enemy lines.

  This should be used to determine the ideal position to
  place a piece at (although not guaranteed to be
  effective, it is useful.)
*)
let advantage_max (p : Board.pieces) (d : Board.dirs) ((h,w) : Vect2.t)
    (ai : char) (player : char) : int * Vect2.t =
  let foldfun ((max,vmax) : int * Vect2.t) (vcomp : Vect2.t) =
    let apsum = advantage_pos p ai d vcomp 5 4
      + advantage_pos p player d vcomp 5 3 in
    if apsum > max then (apsum,vcomp) else (max,vmax)
  in List.fold (grid (h,w)) ~init:(0,(0,0)) ~f:foldfun