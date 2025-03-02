type t = {
  name : string;
  id : int;
  mutable balance : int;
  mutable position : Tile.t;
  mutable property_list : Tile.t list;
  mutable doubles_count : int;
  mutable jail : int;
}
(**AF: The abstract "Player" is represented by a record with a field for name,
   id, balance, position, property list, doubles count, and jail. RI: ID must be
   nonnegative. Position must be non-negative. property_lsit contains no
   duplicate. doubles count must be non-negative. Jail must be non-negative **)

val create : string -> int -> Tile.t -> t
(** [create name id start_tile] makes a new player who posseses no properties,
    and a starting balance of 1500 on the starting tile *)

val create_all_players : int -> Tile.t -> t list
(** [create_all_players] initializes [num_players] number of starting players
    and returns a list containing those players *)

val get_name : t -> string
(** [get_name player] returns name of [player] *)

val get_balance : t -> int
(** [get_balance player] returns balance of [player] *)

val get_position : t -> Tile.t
(** [get_position player] returns position of [player] *)

val get_id : t -> int
(** [get_id player] returns id of [player] *)

val get_doubles_count : t -> int
(** [get_doubles_count player] returns the number of times [player] rolls
    doubles *)

val get_property_list : t -> Tile.t list
(** [get_property_list player] returns the list of properties owned by [player] *)

val get_in_jail : t -> int
(** [get_in_jail player] returns the number of turns [player] has been in jail *)

val get_new_property : t -> Tile.t option
(** [get_new_property player] returns the property that [player] has landed on *)

val get_total_val_in_hand : t -> int
val add_balance : t -> int -> unit
val remove_balance : t -> int -> unit

val update_position : t -> Tile.t -> unit
(** [update_position player tile] updates the position of [player] to [tile] *)

val buy_property : t -> Tile.t -> unit
(**[buy_property] takes in the [player] and [property] of property. It deducts
   the cost of the property from the player's balance and adds the property to
   the player's property list. *)

val check_buy_property : t -> Tile.t -> unit
(** [check_buy_property player tile] checks if [player] can buy [tile] *)

val sell_property : t -> t -> Tile.t -> unit
(**[sell_property] takes in the [selling_player],[buying_player] and [property].
   It adds back the cost of the property to the selling player's balance,
   removes the property from the selling player's property list, and calls
   buy_property for the buying player *)

val pay_rent : t -> t -> int -> unit
(** [pay_rent] takes in the [paying_player], [receving_player], and [amt] amount
    to be paid and updates the player balances **)

val lose_to_free_parking : t -> int -> unit
(**[lose_to_free_parking] subtracts [amt] from the [player] balance. This
   function should be used in actions that add money into free parking e.g jail
   payment, income tax, forced payments from community chest/chance *)

val player_from_id : t list -> int -> t
(** [player_from_id] is a recursive function that takes in a [player_list] and a
    [player_id] and returns the player in that list that matches that id**)

val increment_doubles_count : t -> unit
(** [increment_doubles_count] adds one to the [player]'s doubles count *)

val reset_doubles_count : t -> unit
(** [reset_doubles_count] resets [player]'s doubles count back to 0 *)

val increment_jail_count : t -> unit
(** [increment_jail_count] adds one to the [player]'s jail count. 0 represents
    "not in jail" and a nonzero number represents the number of turns they've
    missed *)

val reset_jail_count : t -> unit
(** [reset_jail_count player ] resets [player]'s jail count back to 0 *)

val add_to_balance : t -> int -> unit
(** [add_to_balance] adds [amt] to [player]'s balance *)

val lose_all_prop : t -> unit
val get_prop_name_list : t -> string list
val check_num_color : t -> Tile.t -> int
