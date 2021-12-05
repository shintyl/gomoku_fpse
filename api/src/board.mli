open Core;;

module Vect2 : sig
    type t = (int * int) [@@deriving compare,sexp]
end

module Board : sig
    (*
        Mapping where keys are vectors of 2 elements.
    *)
    module Vect2Map : Map.S with type Key.t = Vect2.t

    (*
        Mapping from vectors of 2 elements (positions on board)
        to characters (representing player or type/color of piece.)
    *)
    type pieces = (Char.t) Vect2Map.t

    (*
        List of vectors of 2 elements, in this case
        representing directions of winnable lines.
    *)
    type dirs = (Vect2.t) List.t

    val yojson_of_pieces : pieces -> string

    (*
        Insert a new piece into a map of pieces if valid.
        Return an error otherwise.
    *)
    val insert : pieces -> Vect2.t -> char -> (pieces, pieces) result
    
    (*
        Get a pair composed of the length of the line and
        the elements of the line in one directional ray.
    *)
    val longest_1d : pieces -> char -> Vect2.t -> Vect2.t -> int * (Vect2.t list)

    (*
        Get a pair composed of the length of the line and
        the elements of the line in one directional line.
    *)
    val longest_line : pieces -> Vect2.t -> Vect2.t -> int * (Vect2.t list)
    
    (*
        Attempt to insert and check if insertion leads to a win.
    *)
    val insert_check : pieces -> dirs -> Vect2.t -> char -> bool
end