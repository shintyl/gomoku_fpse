open Core

module Vect2 : Map.Key with type t = int * int = struct
  type t = int * int [@@deriving compare, sexp]
end

module Board = struct
  module Vect2Map : Map.S with type Key.t = Vect2.t = Map.Make (Vect2)

  type piece_location = { x : int; y : int; c : char } [@@deriving yojson]

  type pieces = Char.t Vect2Map.t

  type dirs = Vect2.t List.t

  (*type advantage = Int.t Vect2Map.t*)

  let yojson_of_pieces (m : pieces) =
    Vect2Map.to_alist m
    |> List.map ~f:(fun (v2, c) -> match v2 with x, y -> { x; y; c })
    |> [%to_yojson: piece_location list] |> Yojson.Safe.to_string

  let insert (p : pieces) ((x, y) : Vect2.t) (t : char) :
      (pieces, pieces) result =
    match Vect2Map.find p (x, y) with
    | None -> Ok (Vect2Map.add_exn p ~key:(x, y) ~data:t)
    | Some _ -> Error p

  let rec longest_1d (p : pieces) (pt : char) ((x, y) : Vect2.t)
      ((dx, dy) : Vect2.t) =
    match Vect2Map.find p (x, y) with
    | None -> (0, [])
    | Some c -> (
        if Char.( <> ) c pt then (0, [])
        else
          match longest_1d p pt (x + dx, y + dy) (dx, dy) with
          | a, b -> (a + 1, (x, y) :: b))
  
  let longest_line (p : pieces) ((x, y) : Vect2.t) ((dx, dy) : Vect2.t) =
    match Vect2Map.find p (x, y) with
    | None -> (0, [])
    | Some c -> (
        let v1, l1 = longest_1d p c (x, y) (dx, dy) in
        let v2, l2 = longest_1d p c (x, y) (-dx, -dy) in
        match l2 with
        | _ :: tl -> (v1 + v2 - 1, List.rev_append tl l1)
        | [] -> (0, [])) [@@@coverage off]
        (* should not happen as per longest_1d implementation. *)

  let insert_check (p : pieces) (d : dirs) ((x, y) : Vect2.t) (t : char) :
      (bool, bool) result =
    match insert p (x, y) t with
    | Ok q ->
        let conv (dir : Vect2.t) = longest_line q (x, y) dir in
        let check (b : bool) ((v, _) : int * Vect2.t list) : bool =
          b || v = 5
        in
        Ok (List.fold_left (List.map d ~f:conv) ~f:check ~init:false)
    | Error _ -> Error false
  
  (*
    AI FUNCTIONS
  *)

  (*
    Score the strategic value of any segment of length len
    starting from (x, y) and in the direction of (dx, dy).
  *)
  let rec score_segment (p : pieces) (pt : char) ((x, y) : Vect2.t)
      ((dx, dy) : Vect2.t) (len : int) : int =
    if len = 0 then 0
    else match Vect2Map.find p (x, y) with
    | None -> score_segment p pt (x + dx, y + dy) (dx, dy) (len - 1)
    | Some c -> if Char.(<>) c pt then 0
      else score_segment p pt (x + dx, y + dy) (dx, dy) (len - 1) + 1
  
  (*
    Used in conjunction with score_segment to detect possible overlines.
  *)
  let overline (p : pieces) (pt : char) ((x, y) : Vect2.t) : bool =
    match Vect2Map.find p (x, y) with
    | None -> false
    | Some c -> Char.(=) c pt
  
  (*
    Score the strategic value of any segment of length len
    starting from (x, y) and in the direction of (dx, dy)
    taking into account the possibility of overlines.
  *)
  let overline_score_segment (p : pieces) (pt : char) ((x, y) : Vect2.t)
      ((dx, dy) : Vect2.t) (len : int) : int =
    if (overline p pt (x - dx, x - dy))
      || (overline p pt (x + len * dx, x + len * dy)) then 0
    else score_segment p pt (x, y) (dx, dy) len
  
  let rec overline_score_pos (p : pieces) (pt : char) ((x, y) : Vect2.t)
      ((dx, dy) : Vect2.t) (cur : int) (sum : int) (len : int) : int =
    if cur = 0 then sum
    else overline_score_pos p pt (x - dx, y - dy) (dx, dy) (cur - 1) (sum +
      overline_score_segment p pt (x,y) (dx,dy) len) len 

  let advantage_pos (p : pieces) (pt : char) (d : dirs) ((x, y) : Vect2.t)
      (len : int) : int =
    let foldfun (i : int) ((dx, dy) : Vect2.t) =
      i + overline_score_pos p pt (x, y) (dx, dy) len 0 len
    in List.fold d ~init:0 ~f:foldfun

end