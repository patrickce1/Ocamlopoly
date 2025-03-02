type t = {
  board_update : Tile.t list list;
  mutable player_control : Player.t list;
  mutable amt_in_free_parking : int;
}

(*id tile_type color name buy_value players_on base_rent cost_per_house
  num_of_houses prop_owner morgage*)
let top_row =
  [
    Tile.make_tile 0 ActionItem "White" "GO" 0 [] 0 0 0 None 0;
    Tile.make_tile 1 Street "Magenta" "Med. Ave" 60 [] 2 50 0 None 30;
    Tile.make_tile 2 ActionItem "White" "Community Chest" 0 [] 0 0 0 None 0;
    Tile.make_tile 3 Street "Magenta" "Baltic Ave" 60 [] 4 50 0 None 30;
    Tile.make_tile 4 ActionItem "White" "Income Tax" 0 [] 0 0 0 None 0;
    Tile.make_tile 5 Railroad "Black" "Reading RR" 200 [] 25 0 0 None 0;
    (*dont know if theres railroad logic that I should init differently*)
    Tile.make_tile 6 Street "Gray" "Orient Ave" 100 [] 6 50 0 None 50;
    Tile.make_tile 7 ActionItem "White" "Chance" 0 [] 0 0 0 None 0;
    Tile.make_tile 8 Street "Gray" "Vermont Ave" 100 [] 6 50 0 None 50;
    Tile.make_tile 9 Street "Gray" "Conn Ave" 120 [] 8 50 0 None 50;
    Tile.make_tile 10 ActionItem "White" "Jail" 0 [] 0 0 0 None 0;
  ]

let right_down_col =
  [
    Tile.make_tile 11 Street "Pink" "St Charles Pl" 140 [] 10 100 0 None 70;
    Tile.make_tile 12 Utility "White" "Electric Co" 150 [] 4 0 0 None 0;
    (*Utility tax is 4* the roll that landed the person there or 10* it if they
      own both utilities...*)
    Tile.make_tile 13 Street "Pink" "States Ave" 140 [] 10 100 0 None 70;
    Tile.make_tile 14 Street "Pink" "Virginia Ave" 160 [] 10 100 0 None 80;
    Tile.make_tile 15 Railroad "Black" "Penn RR" 200 [] 25 0 0 None 0;
    Tile.make_tile 16 Street "Orange" "St James Pl" 180 [] 14 100 0 None 90;
    Tile.make_tile 80 ActionItem "White" "Community Chest" 0 [] 0 0 0 None 0;
    Tile.make_tile 18 Street "Orange" "Tennes Ave" 180 [] 14 100 0 None 90;
    Tile.make_tile 19 Street "Orange" "NY Ave" 200 [] 16 100 0 None 100;
  ]

let bottom_row =
  List.rev
    [
      Tile.make_tile 20 ActionItem "White" "Free Parking" 0 [] 0 0 0 None 0;
      Tile.make_tile 21 Street "Red" "Kentucky Ave" 220 [] 18 150 0 None 110;
      Tile.make_tile 22 ActionItem "White" "Chance" 0 [] 0 0 0 None 0;
      Tile.make_tile 23 Street "Red" "Indiana Ave" 220 [] 18 150 0 None 110;
      Tile.make_tile 24 Street "Red" "Illinois Ave" 240 [] 20 150 0 None 120;
      Tile.make_tile 25 Railroad "Black" "B&O RR" 200 [] 25 0 0 None 0;
      Tile.make_tile 26 Street "Yellow" "Atlantic Ave" 260 [] 22 150 0 None 130;
      Tile.make_tile 27 Street "Yellow" "Ventnor Ave" 260 [] 22 150 0 None 130;
      Tile.make_tile 28 Utility "White" "Water Works" 150 [] 4 0 0 None 0;
      Tile.make_tile 29 Street "Yellow" "Marvin Gardens" 280 [] 24 150 0 None
        140;
      Tile.make_tile 30 ActionItem "White" "Go To Jail" 0 [] 0 0 0 None 0;
    ]

let left_up_col =
  [
    Tile.make_tile 31 Street "Green" "Pacific Ave" 300 [] 26 200 0 None 150;
    Tile.make_tile 32 Street "Green" "Carolina Ave" 300 [] 26 200 0 None 150;
    Tile.make_tile 33 ActionItem "White" "Community Chest" 0 [] 0 0 0 None 0;
    Tile.make_tile 34 Street "Green" "Penn Ave" 320 [] 28 200 0 None 160;
    Tile.make_tile 35 Railroad "Black" "Short Line RR" 200 [] 25 0 0 None 0;
    Tile.make_tile 36 ActionItem "White" "Chance" 0 [] 0 0 0 None 0;
    Tile.make_tile 37 Street "Blue" "Park Pl." 350 [] 35 200 0 None 805;
    Tile.make_tile 38 ActionItem "White" "Income Tax" 0 [] 75 0 0 None 0;
    Tile.make_tile 39 Street "Blue" "Broadway" 400 [] 50 200 0 None 200;
  ]
(*back to stsart (id = 0)*)

let start_tile (board_update : Tile.t list list) =
  List.nth (List.nth board_update 0) 0

(**This updates the current players in board*)
let update_players (board : t) player_list = board.player_control <- player_list

let make_board num_players : Player.t list * t =
  let board =
    {
      board_update = [ top_row; right_down_col; bottom_row; left_up_col ];
      player_control = [];
      amt_in_free_parking = 0;
    }
  in
  let player_list =
    let strt = start_tile board.board_update in
    List.rev (Player.create_all_players num_players strt)
  in
  let () =
    Tile.set_init_players_on
      (start_tile board.board_update)
      (List.map Player.get_id player_list);
    update_players board player_list
  in
  (player_list, board)

let tile_players_on (tile : Tile.t) =
  let rec helper lst =
    match lst with
    | [] -> ""
    | h :: t -> string_of_int h ^ " " ^ helper t
  in
  helper (Tile.get_players_on tile)

let buy_value_as_str (tile : Tile.t) = string_of_int (Tile.get_buy_value tile)

let background_color_of_tile tile =
  match Tile.get_color tile with
  | "Red" -> "\027[41m" (* ANSI code for red background *)
  | "Green" -> "\027[42m" (* ANSI code for green background *)
  | "Yellow" -> "\027[43m" (* ANSI code for yellow background *)
  | "Blue" -> "\027[44m" (* ANSI code for blue background *)
  | "Magenta" -> "\027[45m" (* ANSI code for magenta background *)
  | "Cyan" -> "\027[46m" (* ANSI code for cyan background *)
  | "Gray" -> "\027[100m" (*ANSI code for dim white (gray)*)
  | "Pink" -> "\027[105m"
  | "Orange" -> "\027[103m"
  | _ -> "\027[0m" (* Default to reset otherwise *)

let rec row_as_string tile_list (str_accessor : Tile.t -> string) =
  match tile_list with
  | [] -> ""
  | h :: t ->
      let background_color_code = background_color_of_tile h in
      let reset_code = "\027[0m" in
      let str =
        background_color_code
        ^ (if String.length (str_accessor h) > 8 then
             String.sub (str_accessor h) 0 8
           else
             str_accessor h
             ^ String.make (8 - String.length (str_accessor h)) ' ')
        ^ reset_code (* Reset color *) ^ "|"
      in
      str ^ row_as_string t str_accessor

let rec middle_tile_pairs_as_string (left_side : Tile.t list)
    (right_side : Tile.t list) : string =
  let check_name_length_sub tile =
    if String.length (Tile.get_name tile) > 8 then
      String.sub (Tile.get_name tile) 0 8
    else Tile.get_name tile
  in
  match (left_side, right_side) with
  | _ :: _, [] -> ""
  | [], _ :: _ -> ""
  | [], [] -> ""
  | h1 :: t1, h2 :: t2 ->
      let background_color_code1 = background_color_of_tile h1 in
      let background_color_code2 = background_color_of_tile h2 in
      let reset_code = "\027[0m" in
      if t1 = [] then
        "|" ^ background_color_code1 ^ check_name_length_sub h1
        ^ String.make (8 - String.length (check_name_length_sub h1)) ' '
        ^ reset_code ^ "|" ^ String.make 80 ' ' ^ "|" ^ background_color_code2
        ^ check_name_length_sub h2
        ^ String.make (8 - String.length (check_name_length_sub h2)) ' '
        ^ reset_code ^ "|\n|" ^ background_color_code1 ^ tile_players_on h1
        ^ String.make (8 - String.length (tile_players_on h1)) ' '
        ^ reset_code ^ "|" ^ String.make 80 ' ' ^ "|" ^ background_color_code2
        ^ tile_players_on h2
        ^ String.make (8 - String.length (tile_players_on h2)) ' '
        ^ reset_code ^ "|\n|" ^ background_color_code1 ^ buy_value_as_str h1
        ^ String.make (8 - String.length (buy_value_as_str h1)) ' '
        ^ reset_code ^ "|" ^ String.make 80 ' ' ^ "|" ^ background_color_code2
        ^ buy_value_as_str h2
        ^ String.make (8 - String.length (buy_value_as_str h2)) ' '
        ^ reset_code ^ "|" ^ "\n" ^ "|" ^ String.make 8 '_' ^ "|"
        ^ String.make 80 '_' ^ "|" ^ String.make 8 '_' ^ "|"
        ^ middle_tile_pairs_as_string t1 t2
      else
        "|" ^ background_color_code1 ^ check_name_length_sub h1
        ^ String.make (8 - String.length (check_name_length_sub h1)) ' '
        ^ reset_code ^ "|" ^ String.make 80 ' ' ^ "|" ^ background_color_code2
        ^ check_name_length_sub h2
        ^ String.make (8 - String.length (check_name_length_sub h2)) ' '
        ^ reset_code ^ "|\n|" ^ background_color_code1 ^ tile_players_on h1
        ^ String.make (8 - String.length (tile_players_on h1)) ' '
        ^ reset_code ^ "|" ^ String.make 80 ' ' ^ "|" ^ background_color_code2
        ^ tile_players_on h2
        ^ String.make (8 - String.length (tile_players_on h2)) ' '
        ^ reset_code ^ "|\n|" ^ background_color_code1 ^ buy_value_as_str h1
        ^ String.make (8 - String.length (buy_value_as_str h1)) ' '
        ^ reset_code ^ "|" ^ String.make 80 ' ' ^ "|" ^ background_color_code2
        ^ buy_value_as_str h2
        ^ String.make (8 - String.length (buy_value_as_str h2)) ' '
        ^ reset_code ^ "|" ^ "\n" ^ "|" ^ String.make 8 '_' ^ "|"
        ^ String.make 80 ' ' ^ "|" ^ String.make 8 '_' ^ "|" ^ "\n"
        ^ middle_tile_pairs_as_string t1 t2

let print_board board =
  (*Top line*)
  let () =
    print_endline
      (String.make ((8 * List.length top_row) + List.length top_row + 1) '_')
  in

  let () =
    print_endline
      ("|" ^ row_as_string (List.nth board.board_update 0) Tile.get_name)
  in
  let () =
    print_endline
      ("|" ^ row_as_string (List.nth board.board_update 0) tile_players_on)
  in
  let () =
    print_endline
      ("|" ^ row_as_string (List.nth board.board_update 0) buy_value_as_str)
  in
  let () =
    print_endline
      ("|"
      ^ String.make
          ((8 * List.length top_row) + List.length top_row + 1 - 2)
          '_'
      ^ "|")
    (*Middle rows of two*)
  in

  let () =
    print_endline
      (middle_tile_pairs_as_string
         (List.rev (List.nth board.board_update 3))
         (List.nth board.board_update 1))
  in
  let () =
    print_endline
      ("|" ^ row_as_string (List.nth board.board_update 2) Tile.get_name)
  in
  let () =
    print_endline
      ("|" ^ row_as_string (List.nth board.board_update 2) tile_players_on)
  in
  let () =
    print_endline
      ("|" ^ row_as_string (List.nth board.board_update 2) buy_value_as_str)
  in
  print_endline
    ("|"
    ^ String.make ((8 * List.length top_row) + List.length top_row + 1 - 2) '_'
    ^ "|")

(*to_string method*)
let move_player (board : t) (current_player : Player.t) (roll_int : int) =
  let old_tile_id = Player.get_id current_player in
  (*remove player from old tile*)
  let () =
    Tile.remove_player (Player.get_position current_player) old_tile_id
  in

  (*add player to new tile *)
  let new_tile_id =
    (Tile.get_id (Player.get_position current_player) + roll_int) mod 39
  in
  (*check if they passed go*)
  let () =
    if new_tile_id < old_tile_id then begin
      Player.add_to_balance current_player 200;
      print_endline
        ("You passed Go and collected 200. Your new balance is "
        ^ string_of_int (Player.get_balance current_player))
    end
  in
  let tile_of_id (board : t) id =
    let rec find id = function
      | [] -> None
      | h :: t -> if Tile.get_id h = id then Some h else find id t
    in
    find id (List.flatten board.board_update)
  in
  match tile_of_id board new_tile_id with
  | Some tile ->
      let () = Tile.add_player tile (Player.get_id current_player) in
      Player.update_position current_player tile
  | None -> ()

let move_player_to_jail (board : t) (current_player : Player.t) =
  let old_tile_id = Player.get_id current_player in
  let () =
    Tile.remove_player (Player.get_position current_player) old_tile_id
  in
  let new_tile_id = 10 in
  let tile_of_id (board : t) id =
    let rec find id = function
      | [] -> None
      | h :: t -> if Tile.get_id h = id then Some h else find id t
    in
    find id (List.flatten board.board_update)
  in
  match tile_of_id board new_tile_id with
  | Some tile ->
      let () = Tile.add_player tile (Player.get_id current_player) in
      Player.update_position current_player tile
  | None -> ()

let move_player_to_go (board : t) (current_player : Player.t) =
  let old_tile_id = Player.get_id current_player in
  let () =
    Tile.remove_player (Player.get_position current_player) old_tile_id
  in
  let new_tile_id = 0 in
  let tile_of_id (board : t) id =
    let rec find id = function
      | [] -> None
      | h :: t -> if Tile.get_id h = id then Some h else find id t
    in
    find id (List.flatten board.board_update)
  in
  match tile_of_id board new_tile_id with
  | Some tile ->
      let () = Tile.add_player tile (Player.get_id current_player) in
      Player.update_position current_player tile
  | None -> ()

let rec remove_helper list1 player =
  match list1 with
  | [] -> [] (* Base case: empty list *)
  | hd :: tl ->
      (* Match the head and tail of the list *)
      if hd = player then tl
        (* If the head is the element to remove, return the tail *)
      else hd :: remove_helper tl player
(* Otherwise, keep the head and recursively remove from the tail *)

let remove_player (board : t) (player : Player.t) =
  board.player_control <- remove_helper board.player_control player

let get_player_list (board : t) = board.player_control

let set_player_list (board : t) (player_list : Player.t list) =
  board.player_control <- player_list

let get_amt_in_free_parking board = board.amt_in_free_parking

let set_amt_in_free_parking board amt =
  board.amt_in_free_parking <- amt;
  let tile_of_id (board : t) id =
    let rec find id = function
      | [] -> None
      | h :: t -> if Tile.get_id h = id then Some h else find id t
    in
    find id (List.flatten board.board_update)
  in
  match tile_of_id board 20 with
  | Some tile -> Tile.set_buy_value tile amt
  | None -> ()

let incr_amt_in_free_parking board amt =
  board.amt_in_free_parking <- board.amt_in_free_parking + amt;
  let tile_of_id (board : t) id =
    let rec find id = function
      | [] -> None
      | h :: t -> if Tile.get_id h = id then Some h else find id t
    in
    find id (List.flatten board.board_update)
  in
  match tile_of_id board 20 with
  | Some tile -> Tile.set_buy_value tile board.amt_in_free_parking
  | None -> ()

(*The first entry is the full top row of tiles. The "middle rows" are lists with
  two entries. the final row is the bottom row of tiles*)
