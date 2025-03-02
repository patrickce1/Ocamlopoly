type t
(**AF: a tile is represented as a record for all associated tile information.
   Has fields for tile_id, tile_type, color, name, buy_value, list of
   players_on, base_rent, cost_per_house, num_houses, prop_owner, mortgaged, and
   mortgage_value. RI: tile_id is non-negative. tile_type is one of prop_types.
   color is a valid string of an accepted color. buy_value is non-negative.
   players_on is a mutable list with no duplicates. base_rent is non-negative.
   cost_per_house is non-negative. num_of_houses is non-negative. num_of_houses
   is non-negative. prop_owner is either None or a player in the player list.
   mortgage_value is non-negative**)

type prop_types =
  | Street
  | Railroad
  | Utility
  | ActionItem
      (**prop_types defines the different categories of tiles on the Monopoly
         board**)

val make_tile :
  int ->
  prop_types ->
  string ->
  string ->
  int ->
  int list ->
  int ->
  int ->
  int ->
  int option ->
  int ->
  t
(**[make_tile id tile_type color name buy_value players_on base_rent cost_per_house num_of_houses prop_owner morgage_value]
   creates a new tile, filling all record fields in order**)

val add_player : t -> int -> unit
(**[add_player tile player] adds a player to the tile. **)

val remove_player : t -> int -> unit
(**[remove_player tile player] removes a player from the tile. **)

val current_rent : t -> int
(**[current_rent tile] returns the current rent of the tile. **)

val cost : t -> int
(**[cost tile] returns the cost of the tile. **)

val get_name : t -> string
(**[get_name tile] produces the name of the tile**)

val get_players_on : t -> int list
(**[get_players_on tile] produces the reference of the list of players on that
   tile**)

val get_buy_value : t -> int
(**[get_buy_value tile] produces the buy value of the tile **)

val set_buy_value : t -> int -> unit
(**[set_buy_value tile val] sets the buy value of the [tile] as [val]**)

val get_prop_owner : t -> int option
(**[get_prop_owner] produces the owner of the title*)

val set_prop_owner : t -> int -> unit
(**[set_prop_owner] sets the owner of the title*)

val set_prop_owner_none : t -> unit
(**[set_prop_owner_none] sets the owner of the property to none*)

val get_id : t -> int
(**[get_id tile] produces the id of [tile]*)

val get_color : t -> string
(**[get_color tile] produces the Color field of [tile] *)

val get_mortgage_val : t -> int
(**[get_mortgage_val tile] produces the mortgage value of [tile] *)

val is_mortgaged : t -> bool
(**[is_mortgaged tile] produces a boolean status of [tile] *)

val set_init_players_on : t -> int list -> unit
(**[set_init_players_on tile lst] sets the [tile]'s mutable players_on list to
   [lst]**)

val set_mortgage : t -> bool -> unit
(**[set_mortgage tile bool] sets the mortgage status of the [tile] to [bool]**)

val get_tile_type : t -> prop_types
(**[get_tile_type tile] produces the tile_type of [tile]**)

val to_string : t -> string
(**[to_string tile] produces a string representation of [tile]**)

val string_of_tile_list : t list -> string
(**[string_of_tile_list lst] produces a string representation of [lst]**)

val scale_rent : t -> int -> unit
(**[scale_rent] updates the current rent of the tile to accomodate for scaling
   due to multiple properties in color schem*)
