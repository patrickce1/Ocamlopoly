open Cs3110_final_project
open OUnit2
open Tile

[@@@warning "-37-69-33-32-26"]

(* Test Tiles *)
let tile_go = Tile.make_tile 0 ActionItem "White" "GO" 0 [] 0 0 0 None 40

let tile_street =
  Tile.make_tile 1 Street "Red" "Med. Ave" 60 [] 15 100 0 None 40

let tile_com_chest =
  Tile.make_tile 10 Railroad "Orange" "Thomas" 50 [] 0 0 0 None 40

let tile_rail =
  Tile.make_tile 11 Utility "Green" "Cornell Health" 60 [] 15 100 0 None 40

(* Test Players *)
let player_hayden = Player.create "Hayden" 1 tile_go
let player_patty = Player.create "Patty" 2 tile_go
let player_nina = Player.create "Nina" 3 tile_go

(* Test Board *)
let players_2p = [ player_hayden; player_patty ]
let players_3p = [ player_hayden; player_patty; player_nina ]
let players_2p, board_2p = Board.make_board 2
let players_3p, board_3p = Board.make_board 3

let player_tests =
  "Player Tests"
  >::: [
         ( "player name" >:: fun _ ->
           assert_equal "Hayden" (Player.get_name player_hayden) );
         ( "player id" >:: fun _ ->
           assert_equal "1" (string_of_int (Player.get_id player_hayden)) );
         ( "player balance and tile" >:: fun _ ->
           assert_equal "1500"
             (string_of_int (Player.get_balance player_hayden)) );
         ( "player update_balance" >:: fun _ ->
           let _ = Player.add_balance player_hayden 100 in
           assert_equal "1600"
             (string_of_int (Player.get_balance player_hayden)) );
         ( "player update_position" >:: fun _ ->
           let _ = Player.update_position player_hayden tile_street in
           assert_equal "Med. Ave  60 None 1 Street"
             (Tile.to_string (Player.get_position player_hayden)) );
         ( "player doubles count" >:: fun _ ->
           assert_equal 0 (Player.get_doubles_count player_hayden);
           Player.increment_doubles_count player_hayden;
           assert_equal 1 (Player.get_doubles_count player_hayden);
           Player.reset_doubles_count player_hayden;
           assert_equal 0 (Player.get_doubles_count player_hayden) );
         ( "player jail count" >:: fun _ ->
           assert_equal 0 (Player.get_in_jail player_hayden);
           Player.increment_jail_count player_hayden;
           assert_equal 1 (Player.get_in_jail player_hayden);
           Player.reset_jail_count player_hayden;
           assert_equal 0 (Player.get_in_jail player_hayden) );
         (* initial position *)
         ( "player get_position" >:: fun _ ->
           let new_position = tile_street in
           Player.update_position player_hayden new_position;
           assert_equal new_position (Player.get_position player_hayden) );
         (* player's property list initially*)
         ( "player get_property_list" >:: fun _ ->
           assert_equal [] (Player.get_property_list player_hayden) );
         (* getting a new property when none are owned *)
         ( "player get_new_property" >:: fun _ ->
           let new_property = Player.get_new_property player_hayden in
           assert_equal None new_property );
         (* test the total value in hand when no properties are owned *)
         ( "player get_total_val_in_hand" >:: fun _ ->
           assert_equal 1500 (Player.get_total_val_in_hand player_nina) );
         ( "player buy_property" >:: fun _ ->
           let properties = Player.get_property_list player_nina in
           print_string (Player.get_name player_nina ^ "'s properties: ");
           Player.buy_property player_nina tile_street;
           assert_equal [ tile_street ] (Player.get_property_list player_nina);
           assert_equal 1440 (Player.get_balance player_nina) );
         ( "add_to_balance" >:: fun _ ->
           let initial_balance = Player.get_balance player_hayden in
           Player.add_to_balance player_hayden 500;
           assert_equal (initial_balance + 500)
             (Player.get_balance player_hayden) );
         ( "player_from_id" >:: fun _ ->
           let players = [ player_hayden; player_patty; player_nina ] in
           assert_equal player_hayden (Player.player_from_id players 1);
           assert_equal player_patty (Player.player_from_id players 2);
           assert_equal player_nina (Player.player_from_id players 3) );
         ( "player_balance_after_passing_go" >:: fun _ ->
           let player = Player.create "Some" 1 tile_go in
           Player.add_balance player 1600;
           let balance = Player.get_balance player in
           assert_equal 3100 (Player.get_balance player) );
         (* Situational Tests *)
         ( "player get_property_list after buying" >:: fun _ ->
           Player.buy_property player_hayden tile_street;
           assert_equal [ tile_street ] (Player.get_property_list player_hayden)
         );
         ( "player get_balance after buying" >:: fun _ ->
           let initial_balance = Player.get_balance player_patty in
           Player.buy_property player_patty tile_rail;
           assert_equal
             (initial_balance - Tile.cost tile_rail)
             (Player.get_balance player_patty) );
         ( "player lose_to_free_parking" >:: fun _ ->
           let initial_balance = Player.get_balance player_patty in
           let lost_amount = 100 in
           Player.lose_to_free_parking player_patty lost_amount;
           assert_equal
             (initial_balance - lost_amount)
             (Player.get_balance player_patty) );
         ( "sell_property_to_another_player" >:: fun _ ->
           let seller = Player.create "Billy" 1 tile_go in
           let buyer = Player.create "Bob" 2 tile_go in

           Player.buy_property seller tile_street;

           Player.sell_property seller buyer tile_street;

           assert_equal 1500 (Player.get_balance seller);
           assert_equal [] (Player.get_property_list seller);

           assert_equal 1440 (Player.get_balance buyer);
           assert_equal [ tile_street ] (Player.get_property_list buyer) );
         (* test cases for incrementing and resetting doubles and jail count *)
         ( "player doubles count multiple increments and resets" >:: fun _ ->
           assert_equal 0 (Player.get_doubles_count player_hayden);
           Player.increment_doubles_count player_hayden;
           Player.increment_doubles_count player_hayden;
           assert_equal 2 (Player.get_doubles_count player_hayden);
           Player.reset_doubles_count player_hayden;
           assert_equal 0 (Player.get_doubles_count player_hayden);
           Player.increment_doubles_count player_hayden;
           assert_equal 1 (Player.get_doubles_count player_hayden) );
         ( "player jail count multiple increments and resets" >:: fun _ ->
           assert_equal 0 (Player.get_in_jail player_hayden);
           Player.increment_jail_count player_hayden;
           Player.increment_jail_count player_hayden;
           Player.increment_jail_count player_hayden;
           assert_equal 3 (Player.get_in_jail player_hayden);
           Player.reset_jail_count player_hayden;
           assert_equal 0 (Player.get_in_jail player_hayden);
           Player.increment_jail_count player_hayden;
           Player.increment_jail_count player_hayden;
           assert_equal 2 (Player.get_in_jail player_hayden) );
         ( "player update balance with negative amount" >:: fun _ ->
           let initial_balance = Player.get_balance player_hayden in
           Player.add_balance player_hayden (-200);
           assert_equal (initial_balance - 200)
             (Player.get_balance player_hayden) );
         (* Fixing Gaps in BISECT *)
         ( "check if player can buy property, then buy the property and sell \
            to other player"
         >:: fun _ ->
           let player1 = Player.create "CMON" 1 tile_go in
           let player2 = Player.create "WORK PLEASE" 2 tile_go in

           (* set player2 property list to an empty list *)
           Player.lose_all_prop player2;

           (* add balance to player1 *)
           Player.add_balance player1 1000;

           (* Player1 buys tile_street *)
           Player.check_buy_property player1 tile_street;
           assert_equal [ tile_street ] (Player.get_property_list player1);
           assert_equal 2440 (Player.get_balance player1);

           (* insufficient funds for player2 buying tile_rail *)
           let tt =
             Tile.string_of_tile_list (Player.get_property_list player2)
           in
           (* player 2 checks the Buys [Cornell Health] *)
           Player.check_buy_property player2 tile_rail;

           assert_equal "" tt;

           (* add balance to player2 now 2440*)
           Player.add_balance player2 500;

           (* player1 sells tile_street to player2 *)
           Player.sell_property player1 player2 tile_street;
           let newP1 =
             Tile.string_of_tile_list (Player.get_property_list player1)
           in
           assert_equal "" newP1;
           let newP2 =
             Tile.string_of_tile_list (Player.get_property_list player2)
           in

           assert_equal "[Med. Ave, Cornell Health]" newP2 );
         ( "remove_balance" >:: fun _ ->
           let player = Player.create "John" 1 tile_go in
           Player.add_balance player 1000;
           Player.remove_balance player 500;
           assert_equal 2000 (Player.get_balance player) );
       ]

let tile_tests =
  "Tile Tests"
  >::: [
         ( "get name" >:: fun _ ->
           assert_equal "Med. Ave" (Tile.get_name tile_street) );
         ( "get buyvalue" >:: fun _ ->
           assert_equal 60 (Tile.get_buy_value tile_street) );
         ( "get prop owner" >:: fun _ ->
           assert_equal None (Tile.get_prop_owner tile_street) );
         ( "set prop_owner" >:: fun _ ->
           Tile.set_prop_owner tile_street 1;
           assert_equal (Some 1) (Tile.get_prop_owner tile_street) );
         ( "set prop owner 0 test case" >:: fun _ ->
           Tile.set_prop_owner_none tile_street;
           assert_equal None (Tile.get_prop_owner tile_street) );
         ("get_id" >:: fun _ -> assert_equal 1 (Tile.get_id tile_street));
         ( "get_mortgage_val" >:: fun _ ->
           assert_equal 40 (Tile.get_mortgage_val tile_street) );
         ( "is_mortgaged" >:: fun _ ->
           assert_equal false (Tile.is_mortgaged tile_street) );
         ( "set_mortgage" >:: fun _ ->
           Tile.set_mortgage tile_street true;
           assert_equal true (Tile.is_mortgaged tile_street) );
         ( "add_player" >:: fun _ ->
           Tile.add_player tile_street 1;
           assert_equal [ 1 ] (Tile.get_players_on tile_street) );
         ( "remove_player" >:: fun _ ->
           Tile.add_player tile_street 1;
           Tile.remove_player tile_street 1;
           assert_equal [] (Tile.get_players_on tile_street) );
         ( "remove_nonexistent_player" >:: fun _ ->
           Tile.remove_player tile_street 1;
           assert_equal [] (Tile.get_players_on tile_street) );
         ( "player remove_balance" >:: fun _ ->
           let initial_balance = Player.get_balance player_hayden in
           let amount_to_remove = 300 in
           Player.remove_balance player_hayden amount_to_remove;
           assert_equal
             (initial_balance - amount_to_remove)
             (Player.get_balance player_hayden) );
         ( "set_init_players_on_empty" >:: fun _ ->
           Tile.set_init_players_on tile_street [];
           assert_equal [] (Tile.get_players_on tile_street) );
         ( "get_tile_type_actionitem" >:: fun _ ->
           assert_equal ActionItem (Tile.get_tile_type tile_go) );
         ( "get_tile_type_railroad" >:: fun _ ->
           assert_equal Railroad (Tile.get_tile_type tile_com_chest) );
         ( "get_tile_type_utility" >:: fun _ ->
           assert_equal Utility (Tile.get_tile_type tile_rail) );
         ( "current_rent" >:: fun _ ->
           assert_equal 15 (Tile.current_rent tile_street) );
         ("cost" >:: fun _ -> assert_equal 60 (Tile.cost tile_street));
         ( "set_init_players_on" >:: fun _ ->
           Tile.set_init_players_on tile_street [ 1; 2 ];
           assert_equal [ 1; 2 ] (Tile.get_players_on tile_street) );
         ( "get_color" >:: fun _ ->
           assert_equal "Red" (Tile.get_color tile_street) );
         ( "get_tile_type" >:: fun _ ->
           assert_equal Street (Tile.get_tile_type tile_street) );
         ( "get_tile_type" >:: fun _ ->
           assert_equal Railroad (Tile.get_tile_type tile_com_chest) );
         ( "get_tile_type" >:: fun _ ->
           assert_equal Utility (Tile.get_tile_type tile_rail) );
         ( "get_tile_type" >:: fun _ ->
           assert_equal ActionItem (Tile.get_tile_type tile_go) );
         ( "make_tile with Street prop_type" >:: fun _ ->
           let street_tile =
             Tile.make_tile 5 Street "Blue" "Broadway" 200 [] 50 100 0 None 100
           in
           assert_equal "Broadway" (Tile.get_name street_tile);
           assert_equal Street (Tile.get_tile_type street_tile) );
         ( "make_tile with Railroad prop_type" >:: fun _ ->
           let railroad_tile =
             Tile.make_tile 15 Railroad "Black" "B&O Railroad" 200 [] 0 0 0 None
               100
           in
           assert_equal "B&O Railroad" (Tile.get_name railroad_tile);
           assert_equal Railroad (Tile.get_tile_type railroad_tile) );
         ( "make_tile with Utility prop_type" >:: fun _ ->
           let utility_tile =
             Tile.make_tile 12 Utility "Yellow" "Electric Company" 150 [] 0 0 0
               None 75
           in
           assert_equal "Electric Company" (Tile.get_name utility_tile);
           assert_equal Utility (Tile.get_tile_type utility_tile) );
         ( "make_tile with ActionItem prop_type" >:: fun _ ->
           let action_tile =
             Tile.make_tile 20 ActionItem "White" "Free Parking" 0 [] 0 0 0 None
               0
           in
           assert_equal "Free Parking" (Tile.get_name action_tile);
           assert_equal ActionItem (Tile.get_tile_type action_tile) );
         ( "set_mortgage_true" >:: fun _ ->
           Tile.set_mortgage tile_street true;
           assert_equal true (Tile.is_mortgaged tile_street) );
         ( "set_mortgage_false" >:: fun _ ->
           Tile.set_mortgage tile_rail false;
           assert_equal false (Tile.is_mortgaged tile_rail) );
         ( "is_mortgaged_initial" >:: fun _ ->
           let new_tile =
             Tile.make_tile 6 Street "Yellow" "Atlantic Ave" 260 [] 22 150 0
               None 130
           in
           assert_equal false (Tile.is_mortgaged new_tile) );
         ( "get_mortgage_val_mortgaged" >:: fun _ ->
           let new_tile =
             Tile.make_tile 7 Street "Yellow" "Ventnor Ave" 260 [] 22 150 0 None
               130
           in
           Tile.set_mortgage new_tile true;
           assert_equal 130 (Tile.get_mortgage_val new_tile) );
         ( "get_mortgage_val_unmortgaged" >:: fun _ ->
           let new_tile =
             Tile.make_tile 8 Street "Green" "Pacific Ave" 300 [] 26 200 0 None
               150
           in
           assert_equal 150 (Tile.get_mortgage_val new_tile) );
         ( "set_mortgage_toggle_multiple" >:: fun _ ->
           let new_tile =
             Tile.make_tile 9 Street "Green" "North Carolina Ave" 300 [] 26 200
               0 None 150
           in
           Tile.set_mortgage new_tile true;
           assert_equal true (Tile.is_mortgaged new_tile);
           Tile.set_mortgage new_tile false;
           assert_equal false (Tile.is_mortgaged new_tile);
           Tile.set_mortgage new_tile true;
           assert_equal true (Tile.is_mortgaged new_tile) );
         ( "to_string_actionitem" >:: fun _ ->
           assert_equal "GO  0 None 0 ActionItem" (Tile.to_string tile_go) );
         ( "to_string_railroad" >:: fun _ ->
           assert_equal "Thomas  50 None 10 Railroad"
             (Tile.to_string tile_com_chest) );
         ( "to_string_utility" >:: fun _ ->
           assert_equal "Cornell Health  60 None 11 Utility"
             (Tile.to_string tile_rail) );
         ( "string_of_tile_list_single" >:: fun _ ->
           let tt = Tile.string_of_tile_list [ tile_go ] in
           print_endline ("||||||" ^ tt ^ "----");
           assert_equal "[GO]" (Tile.string_of_tile_list [ tile_go ]) );
         ( "string_of_tile_list_empty" >:: fun _ ->
           assert_equal "" (Tile.string_of_tile_list []) );
         ( "string_of_tile_list_multiple" >:: fun _ ->
           let tt = Tile.string_of_tile_list [ tile_go; tile_street ] in
           print_endline ("||||||" ^ tt ^ "|||||||||||");
           assert_equal "[GO, Med. Ave]"
             (Tile.string_of_tile_list [ tile_go; tile_street ]) );
         (* Ensure Proper Prices on Props *)
         ("cost" >:: fun _ -> assert_equal 60 (Tile.cost tile_street));
         ("cost" >:: fun _ -> assert_equal 50 (Tile.cost tile_com_chest));
         ("cost" >:: fun _ -> assert_equal 60 (Tile.cost tile_rail));
         ("cost" >:: fun _ -> assert_equal 0 (Tile.cost tile_go));
         (* Ensure Proper Rent on Props *)
         ( "current_rent" >:: fun _ ->
           assert_equal 15 (Tile.current_rent tile_street) );
         ( "current_rent" >:: fun _ ->
           assert_equal 0 (Tile.current_rent tile_com_chest) );
         ( "current_rent" >:: fun _ ->
           assert_equal 15 (Tile.current_rent tile_rail) );
         ("current_rent" >:: fun _ -> assert_equal 0 (Tile.current_rent tile_go));
         (* Ensure Proper Color on Props *)
         ( "get_color" >:: fun _ ->
           assert_equal "Red" (Tile.get_color tile_street) );
         ( "get_color" >:: fun _ ->
           assert_equal "Orange" (Tile.get_color tile_com_chest) );
         ( "get_color" >:: fun _ ->
           assert_equal "Green" (Tile.get_color tile_rail) );
         ("get_color" >:: fun _ -> assert_equal "White" (Tile.get_color tile_go));
         (* Ensure Proper Tile Type on Props *)
         ( "get_tile_type" >:: fun _ ->
           assert_equal Street (Tile.get_tile_type tile_street) );
         ( "get_tile_type" >:: fun _ ->
           assert_equal Railroad (Tile.get_tile_type tile_com_chest) );
         ( "get_tile_type" >:: fun _ ->
           assert_equal Utility (Tile.get_tile_type tile_rail) );
         ( "get_tile_type" >:: fun _ ->
           assert_equal ActionItem (Tile.get_tile_type tile_go) );
         ( "rent_with_two_properties_of_same_type_1_mnortgaged" >:: fun _ ->
           let player = Player.create "John" 1 tile_go in
           let prop1 =
             Tile.make_tile 1 Street "Blue" "Mediterranean Avenue" 60 [] 2 50 0
               None 30
           in
           let prop2 =
             Tile.make_tile 3 Street "Blue" "Baltic Avenue" 60 [] 2 50 0 None 30
           in

           Tile.set_mortgage prop2 true;

           Player.buy_property player prop1;
           Player.buy_property player prop2;

           Tile.set_prop_owner prop1 (Player.get_id player);
           Tile.set_prop_owner prop2 (Player.get_id player);

           Tile.scale_rent prop1 (Player.check_num_color player prop1);

           Tile.scale_rent prop2 (Player.check_num_color player prop2);

           let pcr1 = "RENT 1: " ^ string_of_int (Tile.current_rent prop1) in
           let pcr2 =
             "RENT 2(MORTGAGED): " ^ string_of_int (Tile.current_rent prop2)
           in
           print_endline pcr1;
           print_endline pcr2;

           let cr1 = Tile.current_rent prop1 in
           let cr2 = Tile.current_rent prop2 in

           assert_equal 2 cr1 );
         ( "rent_with_two_properties_of_same_type" >:: fun _ ->
           let player = Player.create "John" 1 tile_go in
           let prop1 =
             Tile.make_tile 1 Street "Blue" "Mediterranean Avenue" 60 [] 2 50 0
               None 30
           in
           let prop2 =
             Tile.make_tile 3 Street "Blue" "Baltic Avenue" 60 [] 2 50 0 None 30
           in

           Player.buy_property player prop1;
           Player.buy_property player prop2;

           Tile.set_prop_owner prop1 (Player.get_id player);
           Tile.set_prop_owner prop2 (Player.get_id player);

           Tile.scale_rent prop1 (Player.check_num_color player prop1);

           Tile.scale_rent prop2 (Player.check_num_color player prop2);

           let pcr1 = "Yooooo" ^ string_of_int (Tile.current_rent prop1) in
           let pcr2 = "ITS PCR2" ^ string_of_int (Tile.current_rent prop2) in
           print_endline pcr1;
           print_endline pcr2;

           let cr1 = Tile.current_rent prop1 in
           let cr2 = Tile.current_rent prop2 in

           assert_equal 4 cr1;
           assert_equal 4 cr2 );
         ( "rent_with_one_property" >:: fun _ ->
           let player = Player.create "John" 1 tile_go in
           let prop =
             Tile.make_tile 1 Street "Blue" "Mediterranean Avenue" 60 [] 2 50 0
               None 30
           in

           Player.buy_property player prop;

           Tile.set_prop_owner prop (Player.get_id player);

           assert_equal 2 (Tile.current_rent prop) );
         ( "rent_with_three_properties_of_same_type" >:: fun _ ->
           let player = Player.create "Jimmy" 1 tile_go in
           let prop1 =
             Tile.make_tile 1 Street "Pink" "St. Charles Place" 140 [] 10 100 0
               None 70
           in
           let prop2 =
             Tile.make_tile 3 Street "Pink" "States Avenue" 140 [] 10 100 0 None
               70
           in
           let prop3 =
             Tile.make_tile 4 Street "Pink" "Virginia\n            Avenue" 160
               [] 10 100 0 None 80
           in

           Player.buy_property player prop1;
           Player.buy_property player prop2;
           Player.buy_property player prop3;

           Tile.set_prop_owner prop1 (Player.get_id player);
           Tile.set_prop_owner prop2 (Player.get_id player);
           Tile.set_prop_owner prop3 (Player.get_id player);

           Tile.scale_rent prop1 (Player.check_num_color player prop1);
           Tile.scale_rent prop2 (Player.check_num_color player prop2);
           Tile.scale_rent prop3 (Player.check_num_color player prop3);

           let p1 = "PROP 1:" ^ string_of_int (Tile.current_rent prop1) in
           let p2 = "PROP 2:" ^ string_of_int (Tile.current_rent prop2) in
           let p3 = "PROP 3:" ^ string_of_int (Tile.current_rent prop3) in

           print_endline p1;
           print_endline p2;
           print_endline p3;

           assert_equal 30 (Tile.current_rent prop1);
           assert_equal 30 (Tile.current_rent prop2);
           assert_equal 30 (Tile.current_rent prop3) );
         ( "to_string_with_owner" >:: fun _ ->
           let tile =
             Tile.make_tile 1 Street "Red" "Mediterranean Avenue" 60 [] 2 50 0
               (Some 1) 30
           in

           Tile.set_prop_owner tile 1;

           (* Add some players on the tile *)
           Tile.add_player tile 2;
           Tile.add_player tile 3;
           let p2 = " WOWWW: " ^ Tile.to_string tile in
           print_endline p2;

           (* Check the string representation of the tile *)
           assert_equal "Mediterranean Avenue 3, 2 60 1 1 Street"
             (Tile.to_string tile) );
       ]

let board_tests =
  "Board Tests"
  >::: [
         ( "start_tile" >:: fun _ ->
           let board_tiles =
             [ [ tile_go; tile_street ]; [ tile_com_chest; tile_rail ] ]
           in
           assert_equal tile_go (Board.start_tile board_tiles) );
         ( "move_player_negative" >:: fun _ ->
           let players, board = Board.make_board 2 in
           let player = List.hd players in
           Board.move_player board player (-3) );
         ( "make_board_returns_board_and_players" >:: fun _ ->
           let players, board = Board.make_board 2 in
           assert_equal 2 (List.length players);
           assert_equal players (Board.get_player_list board) );
         ( "make_board_0_players" >:: fun _ ->
           let players, board = Board.make_board 0 in
           assert_equal [] players;
           assert_equal [] (Board.get_player_list board) );
         ( "make_board_2_players" >:: fun _ ->
           let players, board = Board.make_board 2 in
           assert_equal 2 (List.length players);
           assert_equal players (Board.get_player_list board);
           assert_equal 0 (Board.get_amt_in_free_parking board) );
         ( "make_board_4_players" >:: fun _ ->
           let players, board = Board.make_board 4 in
           assert_equal 4 (List.length players);
           assert_equal players (Board.get_player_list board);
           assert_equal 0 (Board.get_amt_in_free_parking board) );
         ( "remove_player_updates_player_list" >:: fun _ ->
           let player = List.hd players_3p in
           Board.remove_player board_3p player;
           assert_equal 2 (List.length (Board.get_player_list board_3p)) );
         ( "set_and_get_player_list" >:: fun _ ->
           let new_players = List.tl players_2p in

           Board.set_player_list board_2p new_players;
           assert_equal new_players (Board.get_player_list board_2p) );
         ( "update_and_get_amt_in_free_parking" >:: fun _ ->
           Board.set_amt_in_free_parking board_2p 100;

           assert_equal 100 (Board.get_amt_in_free_parking board_2p);
           Board.incr_amt_in_free_parking board_2p 50;

           assert_equal 150 (Board.get_amt_in_free_parking board_2p) );
         ( "incr_amt_in_free_parking_negative" >:: fun _ ->
           Board.set_amt_in_free_parking board_2p 100;
           Board.incr_amt_in_free_parking board_2p (-50);
           assert_equal 50 (Board.get_amt_in_free_parking board_2p) );
         ( "set_empty_player_list" >:: fun _ ->
           Board.set_player_list board_2p [];
           assert_equal [] (Board.get_player_list board_2p) );
         ( "incr_amt_in_free_parking_multiple_times" >:: fun _ ->
           Board.set_amt_in_free_parking board_2p 0;

           Board.incr_amt_in_free_parking board_2p 20;

           Board.incr_amt_in_free_parking board_2p 30;

           Board.incr_amt_in_free_parking board_2p 50;

           assert_equal 100 (Board.get_amt_in_free_parking board_2p) );
         ( "set_player_list_to_empty" >:: fun _ ->
           let _, board = Board.make_board 3 in
           Board.set_player_list board [];
           assert_equal [] (Board.get_player_list board) );
         ( "remove_multiple_players" >:: fun _ ->
           let players, board = Board.make_board 4 in
           let player1 = List.hd players in
           let player2 = List.nth players 1 in
           Board.remove_player board player1;
           Board.remove_player board player2;
           assert_equal 2 (List.length (Board.get_player_list board)) );
         ( "remove_last_player" >:: fun _ ->
           let players, board = Board.make_board 1 in
           let player = List.hd players in
           Board.remove_player board player;
           assert_equal [] (Board.get_player_list board) );
         ( "set_and_get_amt_in_free_parking" >:: fun _ ->
           let _, board = Board.make_board 2 in
           Board.set_amt_in_free_parking board 500;
           assert_equal 500 (Board.get_amt_in_free_parking board) );
         ( "set_negative_amt_in_free_parking" >:: fun _ ->
           let _, board = Board.make_board 2 in
           Board.set_amt_in_free_parking board (-100);
           assert_equal (-100) (Board.get_amt_in_free_parking board) );
         ( "incr_amt_in_free_parking_negative" >:: fun _ ->
           let _, board = Board.make_board 2 in
           Board.set_amt_in_free_parking board 200;
           Board.incr_amt_in_free_parking board (-50);
           assert_equal 150 (Board.get_amt_in_free_parking board) );
         ( "move_player_to_same_tile" >:: fun _ ->
           let players, board = Board.make_board 2 in
           let player = List.hd players in
           let initial_position = Player.get_position player in
           Board.move_player board player 0;
           assert_equal initial_position (Player.get_position player) );
         ( "move_player_to_new_tile" >:: fun _ ->
           let players, board = Board.make_board 2 in
           let player = List.hd players in
           let initial_position = Player.get_position player in
           Board.move_player board player 3;
           assert_bool "Player position should change after moving"
             (not (initial_position = Player.get_position player)) );
         ( "print_board" >:: fun _ ->
           let empty_board = Board.make_board 0 |> snd in
           Board.print_board empty_board );
         ( "move_player_to_jail" >:: fun _ ->
           let players, board = Board.make_board 2 in
           let player = List.hd players in

           let initial_position = Player.get_position player in

           Board.move_player_to_jail board player;

           let new_position = Player.get_position player in

           assert_bool "Player position should change after moving to jail"
             (initial_position <> new_position);

           assert_equal 10 (Tile.get_id new_position) );
       ]

let test_suite = "All Tests" >::: [ player_tests; tile_tests; board_tests ]
let _ = run_test_tt_main test_suite
