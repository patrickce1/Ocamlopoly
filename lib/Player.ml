[@@@warning "-37-69-33"]

type t = {
  name : string;
  id : int;
  mutable balance : int;
  mutable position : Tile.t;
  mutable property_list : Tile.t list;
  mutable doubles_count : int;
  mutable jail : int;
}

let create name id start_tile =
  {
    name;
    balance = 1500;
    position = start_tile;
    property_list = [];
    id;
    doubles_count = 0;
    jail = 0;
  }

let rec create_all_players num_players start_tile =
  match num_players with
  | 0 -> []
  | _ ->
      let player_with_id = "Player" ^ string_of_int num_players in
      let p = create player_with_id num_players start_tile in
      p :: create_all_players (num_players - 1) start_tile

let get_name player = player.name
let get_balance player = player.balance
let get_position player = player.position
let get_id player = player.id
let get_doubles_count player = player.doubles_count
let get_property_list player = player.property_list
let get_in_jail player = player.jail

let get_new_property player =
  if List.length player.property_list = 0 then None
  else
    let first = List.hd player.property_list in
    Some first

let rec asset_calculator prop_list =
  match prop_list with
  | [] -> 0
  | h :: t ->
      let mort_val =
        if Tile.is_mortgaged h then 0 else Tile.get_mortgage_val h
      in
      mort_val + asset_calculator t

let get_total_val_in_hand player =
  player.balance + asset_calculator player.property_list

let add_balance player amount = player.balance <- player.balance + amount
let remove_balance player amount = player.balance <- player.balance - amount
let update_position player tile = player.position <- tile

let buy_property player property =
  let cost = Tile.get_buy_value property in
  player.balance <- player.balance - cost;
  player.property_list <- property :: player.property_list

let check_buy_property player property =
  let cost = Tile.get_buy_value property in
  if player.balance - cost >= 0 then buy_property player property
  else print_endline "You do not have enough money for this purchase"

let sell_property selling_player buying_player property =
  let cost = Tile.get_buy_value property in
  selling_player.balance <- selling_player.balance + cost;
  selling_player.property_list <-
    List.filter (fun x -> x <> property) selling_player.property_list;
  buy_property buying_player property

let pay_rent paying_player receving_player amt =
  paying_player.balance <- paying_player.balance - amt;
  receving_player.balance <- receving_player.balance + amt

let lose_to_free_parking player amt = player.balance <- player.balance - amt

let rec player_from_id player_list player_id =
  match player_list with
  | h :: t -> if get_id h = player_id then h else player_from_id t player_id
  | [] -> failwith "this player id is not in list"

let increment_doubles_count player =
  player.doubles_count <- player.doubles_count + 1

let reset_doubles_count player = player.doubles_count <- 0
let increment_jail_count player = player.jail <- player.jail + 1
let reset_jail_count player = player.jail <- 0
let add_to_balance player amt = player.balance <- player.balance + amt

let lose_all_prop player =
  let reset_tile_owner (t : Tile.t) : unit = Tile.set_prop_owner_none t in
  List.iter reset_tile_owner player.property_list

let get_prop_name_list player : string list =
  let extract_name prop = Tile.get_name prop in
  List.map extract_name player.property_list

(*FUTURE: buy house*)

let check_num_color player1 tile1 : int =
  let rec count_tiles_with_same_color acc tile_list =
    match tile_list with
    | [] -> acc
    | tile :: rest ->
        if Tile.get_color tile = Tile.get_color tile1 then
          if Tile.is_mortgaged tile then count_tiles_with_same_color acc rest
          else count_tiles_with_same_color (acc + 1) rest
        else count_tiles_with_same_color acc rest
  in
  let tile_list = player1.property_list in
  let multiplier = count_tiles_with_same_color 0 tile_list in
  if multiplier = 0 then 1 else multiplier
