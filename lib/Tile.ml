(*The implementation for the Tile interface.*)
[@@@warning "-37-69-33"]

type prop_types =
  | Street
  | Railroad
  | Utility
  | ActionItem

type t = {
  tile_id : int;
  tile_type : prop_types;
  color : string;
  name : string;
  mutable buy_value : int;
  mutable players_on : int list;
  mutable current_rent : int;
  cost_per_house : int;
  mutable num_of_houses : int;
  mutable prop_owner : int option;
  mutable mortgaged : bool;
  mortgage_value : int;
}

let make_tile id tt col name buy players rent house_cost num_houses owner
    mortgage_value : t =
  {
    tile_id = id;
    tile_type = tt;
    color = col;
    name;
    buy_value = buy;
    players_on = players;
    current_rent = rent;
    cost_per_house = house_cost;
    num_of_houses = num_houses;
    prop_owner = owner;
    mortgaged = false;
    mortgage_value;
  }

let cost tile = tile.buy_value
let add_player tile player_id = tile.players_on <- player_id :: tile.players_on

let remove_player tile player_id =
  tile.players_on <- List.filter (fun p -> p <> player_id) tile.players_on

let current_rent tile = tile.current_rent
let get_name tile = tile.name
let get_players_on tile = tile.players_on
let get_buy_value tile = tile.buy_value
let set_buy_value tile value = tile.buy_value <- value
let get_prop_owner title = title.prop_owner
let set_prop_owner title player_id = title.prop_owner <- Some player_id
let set_prop_owner_none title = title.prop_owner <- None
let get_id tile = tile.tile_id
let get_color tile = tile.color
let get_tile_type tile = tile.tile_type
let get_mortgage_val tile = tile.mortgage_value
let is_mortgaged tile = tile.mortgaged
let set_init_players_on tile lst = tile.players_on <- lst

let to_string_lst_help intlist1 =
  let lst = List.map string_of_int intlist1 in
  String.concat ", " lst

let set_mortgage tile bool = tile.mortgaged <- bool

let tile_type_to_str tile_type =
  match tile_type with
  | ActionItem -> "ActionItem"
  | Street -> "Street"
  | Railroad -> "Railroad"
  | Utility -> "Utility"

let to_string tile =
  get_name tile ^ " "
  ^ to_string_lst_help (get_players_on tile)
  ^ " "
  ^ string_of_int (get_buy_value tile)
  ^ " "
  ^ (if get_prop_owner tile = None then "None"
     else string_of_int (Option.get (get_prop_owner tile)))
  ^ " "
  ^ string_of_int (get_id tile)
  ^ " "
  ^ tile_type_to_str (get_tile_type tile)

let string_of_tile_list tiles =
  match tiles with
  | [] -> ""
  | _ ->
      let tile_names = List.map get_name tiles in
      "[" ^ String.concat ", " tile_names ^ "]"

let scale_rent tile num = tile.current_rent <- tile.current_rent * num
