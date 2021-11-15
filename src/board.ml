open Core

module Position : (Map.Key with type t = int * int) = struct
    type t = (int * int) [@@deriving compare,sexp];;
end

module Board = struct
    module PositionMap : (Map.S with type Key.t = Position.t) = Map.Make(Position);;

    type pieces = (Char.t) PositionMap.t;;

    let insert (p : pieces) (x : int) (y : int) (t : char) =
        match PositionMap.find p (x,y) with
        | None -> Ok(PositionMap.add_exn p ~key:(x,y) ~data:t)
        | Some(_) -> Error(p)
    
end