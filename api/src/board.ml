open Core

module Vect2 : (Map.Key with type t = int * int) = struct
    type t = (int * int) [@@deriving compare,sexp];;
end

module Board = struct
    module Vect2Map : (Map.S with type Key.t = Vect2.t) = Map.Make(Vect2);;

    type pieces = (Char.t) Vect2Map.t;;

    type dirs = (Vect2.t) List.t;;

    let insert (p : pieces) ((x,y) : Vect2.t) (t : char) : (pieces, pieces) result =
        match Vect2Map.find p (x,y) with
        | None -> Ok(Vect2Map.add_exn p ~key:(x,y) ~data:t)
        | Some(_) -> Error(p)
    
    let rec longest_1d (p : pieces) (pt : char) ((x,y) : Vect2.t) ((dx,dy) : Vect2.t) =
        match Vect2Map.find p (x,y) with
        | None -> (0,[])
        | Some(c) ->
            if Char.(<>) c pt then (0,[])
            else match longest_1d p pt (x+dx,y+dy) (dx,dy) with (a,b) -> (a+1,(x,y)::b)

    let longest_line (p : pieces) ((x,y) : Vect2.t) ((dx,dy) : Vect2.t) =
        match Vect2Map.find p (x,y) with
        | None -> (0,[])
        | Some(c) ->
            let (v1,l1) = longest_1d p c (x,y) (dx,dy) in
            let (v2,l2) = longest_1d p c (x,y) (-dx,-dy) in
            match l1 with
            | _::tl -> (v1 + v2 - 1, List.rev_append tl l2)
            | [] -> (0,[]) (* should not happen as per longest_1d implementation. *)
    
    let insert_check (p : pieces) (d : dirs) ((x,y) : Vect2.t) (t : char) : bool =
        match insert p (x,y) t with
        | Ok(_) ->
            let conv (dir : Vect2.t) = longest_line p (x,y) dir
            in let check (b : bool) ((v,_) : int * Vect2.t list) : bool = b || (v = 5)
            in List.fold_left (List.map d ~f:conv) ~f:check ~init:false
        | Error(_) -> false
end