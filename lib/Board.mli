type t
(**AF: The abstract "Board" is represented as a record with a 2D list of tiles,
   a list of players, and a mutable value to record the amount in free parking
   RI: Given the board is a n x n square, the first entry is a tile list of size
   n, the following n-2 entries are of length two (to represent the sides) and
   the final entry is a tile list of size n. Player list has no duplicates. The
   amount in free-parking is non-negative**)

val make_board : int -> Player.t list * t
(**[make_board] initializes a hard coded board**)

val print_board : t -> unit
(**[print_board] prints the board (after initialization in its current state**)

val start_tile : Tile.t list list -> Tile.t
(**[start_tile] returns the starting tile of the board**)

val move_player : t -> Player.t -> int -> unit
(**[move_player b p n] moves player [p] [n] spaces on board [b], updating their
   balance if they pass "Go"**)

val move_player_to_jail : t -> Player.t -> unit
(**[move_player_to_jail b p] moves player [p] to Jail (Tile position 10), NOT
   adding $200 balance if they pass "Go"**)

val move_player_to_go : t -> Player.t -> unit
(**[move_player_to_jail b p] moves player [p] to GO (Tile position 0)**)

val remove_player : t -> Player.t -> unit
(**[remove_player b p] removes player [p] from board [b]**)

val get_player_list : t -> Player.t list
(**[get_player_list b] returns the list of players on board [b]**)

val set_player_list : t -> Player.t list -> unit
(**[set_player_list b p] sets the list of players on board [b] to [p]**)

val get_amt_in_free_parking : t -> int
(**[get_amt_in_free_parking b] returns the amount in "Free Parking" on board
   [b]**)

val set_amt_in_free_parking : t -> int -> unit
(**[set_amt_in_free_parking board amt] sets the amount in "Free Parking" on
   board [board] to [amt]**)

val incr_amt_in_free_parking : t -> int -> unit
(**[incr_amt_in_free_parking b p] increments the amount in free parking by [p]**)
