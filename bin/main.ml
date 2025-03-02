module Player = Cs3110_final_project.Player
module Tile = Cs3110_final_project.Tile
module Board = Cs3110_final_project.Board

(*initiates Random*)
let () = Random.self_init ()

(**[roll_dice] returns a random integer between 1 and 6*)
let roll_dice () = Random.int 6 + 1

(** [roll] returns a pair of two numbers between 1 and 6 representing the result
    of a 2-die toss*)
let roll () = (roll_dice (), roll_dice ())

let list_to_string (lst : string list) : string =
match lst with 
|[] -> "None"
| _ -> String.concat ", " lst



let handle_unmortgage (current_player:Player.t) (prop_name :string)=
match List.find_opt (fun r -> Tile.get_name r = prop_name) (Player.get_property_list current_player) with
  | Some property ->
      if (not(Tile.is_mortgaged property)) then print_endline ("This property is not mortgaged") else 
      if Player.get_balance current_player > Tile.get_mortgage_val property then(
        
        Tile.set_mortgage property false; 
        Player.remove_balance (current_player) (Tile.get_mortgage_val property);
        print_endline ("Property '" ^ Tile.get_name property ^ "' is now unmortgaged and " ^ 
        string_of_int(Tile.get_mortgage_val property)^ " has been returned to your account")
      )
      else
        print_endline "You do not have enough money to unmortgage this property. Press enter to continue";
    
  | None ->
      print_endline ("You do not own this property") 
    


let handle_buy_property ( current_player : Player.t) (current_tile : Tile.t )=
  let new_prop = Player.get_new_property current_player in
  if new_prop = Some current_tile then begin
    Tile.set_prop_owner current_tile (Player.get_id current_player);
    print_endline
      ("Purchased. Your new balance is "
      ^ string_of_int (Player.get_balance current_player)
      ^ ". Your total property list is "
      ^ String.concat ", "
          (List.map Tile.get_name
            (Player.get_property_list current_player))
      ^ ". End of Roll")
  end
  else print_endline "You do not have enough money" 

let handle_mortgage_helper ( current_player : Player.t) (prop_name : string) = 
  match List.find_opt (fun r -> Tile.get_name r = prop_name) (Player.get_property_list current_player) with
  | Some property ->
      if (Tile.is_mortgaged property) then begin print_endline ("This property is mortgaged already") end else begin
      Tile.set_mortgage property true; 
      Player.add_balance (current_player) (Tile.get_mortgage_val property);
      print_endline ("Property '" ^ Tile.get_name property ^ "' is now mortgaged.") 
    end
  | None ->
      (* Property not found *) 
      begin 
      print_endline ("You do not own this property") 
      end


let rec mortgage_decision (decide : string) (current_player : Player.t) : unit =
  let decide = String.uppercase_ascii decide in
  if decide = "Y" then begin
    print_newline ();
    print_endline ("Your list of properties is " ^ (list_to_string (Player.get_prop_name_list current_player)));
    print_string "Mortgage(M) or Unmortgage(U)?";
    
    match read_line () with
    | "U" ->
        print_endline "What property do you want to unmortgage?";
        handle_unmortgage current_player (read_line ());
        print_string "Do you want to perform another mortgage(M) or unmortgage(U) action? (Y/N)";
        mortgage_decision (read_line ()) current_player
    | "M" ->
        print_endline "What property do you want to mortgage?";
        handle_mortgage_helper current_player (read_line ());
        print_string "Do you want to perform another mortgage(M) or unmortgage(U) action? (Y/N)";
        mortgage_decision (read_line ()) current_player
    | _ ->
        print_endline "Invalid input. Please enter 'M' to mortgage or 'U' to unmortgage.";
        print_string "Do you want to perform another mortgage(M) or unmortgage(U) action? (Y/N)";
        mortgage_decision (read_line ()) current_player
  end
  else if decide = "N" then
    print_endline "Press enter to roll the dice > "

  else begin
    print_string "Invalid input. Please enter Yes(Y) or No(N).";
    mortgage_decision(read_line()) (current_player) 
  end
      

  
(**Removes the player from the board once bankrupt. All the money and assets are lost. Nees*)
let handle_bankrupt player board= begin
  print_endline "In bankrupt";
  Player.lose_all_prop player;
  Board.remove_player board player;
  let line = Player.get_name player ^ "is currently bankrupt and has now been removed from the game" in
  print_endline line
end

let rec handle_payment (current_player : Player.t) (current_tile : Tile.t) (board : Board.t) (player_list : Player.t list) (tile_owner_p : int) (action : string) =
  match action with
  | "pay for tile" ->
      begin
        print_string
          ("This property is unowned. Do you want to buy Property " ^
           Tile.get_name current_tile ^ " with cost " ^
           string_of_int (Tile.get_buy_value current_tile) ^
           "? Your current balance is " ^
           string_of_int (Player.get_balance current_player) ^
           "? (Y/N)> ");
        let buy_response = read_line () in
        if buy_response = "N" then ()
        else begin
          Player.check_buy_property current_player current_tile;
          let new_prop = Player.get_new_property current_player in
          match new_prop with
          | Some tile when tile = current_tile ->
              handle_buy_property current_player current_tile
          | _ ->
              print_endline "Would you like to mortgage a property? Y/N";
              let response = read_line () in
              if response = "Y" then begin 
                print_endline ("Your list of properties is " ^ (list_to_string (Player.get_prop_name_list current_player)));
                print_endline "What property would you like to mortgage?";
                let prop_name = read_line () in
                handle_mortgage_helper current_player prop_name;
                handle_payment current_player current_tile board player_list tile_owner_p "pay for tile"
              end
              else ()  (* Do nothing if the user chooses not to mortgage *)
        end        
      end
  | "pay player" ->
      begin
        print_endline "In pay player";
        if Tile.is_mortgaged current_tile then 
          print_endline "This property is mortgaged. Rent does not need to be payed" 
        else
          let ()= Tile.scale_rent (current_tile) (Player.check_num_color current_player current_tile) in
          let rent_to_pay= Tile.current_rent current_tile in
          if Player.get_total_val_in_hand current_player < rent_to_pay then begin
            print_endline "You are bankrupt.";
            handle_bankrupt current_player board
          end else if Player.get_balance current_player < rent_to_pay then begin
            print_endline "You do not have enough money.";
            print_endline "Would you like to mortgage a property? Y/N";
            let response = read_line () in
            if response = "Y" then begin 
              print_endline ("Your list of properties is " ^ (list_to_string (Player.get_prop_name_list current_player)));
              print_endline "What property would you like to mortgage?";
              let prop_name = read_line () in
              handle_mortgage_helper current_player prop_name;
              handle_payment current_player current_tile board player_list tile_owner_p "pay player"
            end else ()  (* Do nothing if the user chooses not to mortgage *)
          end else begin
            let tile_owner_ = Player.player_from_id player_list tile_owner_p in
            Player.pay_rent current_player tile_owner_ (rent_to_pay);
            print_endline
              ("This is " ^
              Player.get_name tile_owner_ ^ "'s property. You paid " ^
              string_of_int (rent_to_pay) ^ " in rent to them. " ^
              "You now have a balance of " ^ string_of_int (Player.get_balance current_player) ^ ". End of Roll")
          end
      end
  | "jail" -> 
    begin 
      if Player.get_total_val_in_hand current_player > 50 then
        if (Player.get_balance current_player > 50) then begin 
          Player.lose_to_free_parking current_player 50 ;
          Board.incr_amt_in_free_parking board 50; 
          Player.reset_jail_count current_player;
          print_endline
            ("You paid $50 fine and are kicked out of jail. Your balance is now "
            ^ string_of_int (Player.get_balance current_player));
        end
        else 
          begin
          print_endline "You do not have enough money to pay out of jail. ";
          print_endline "You have to mortgage a property!";
          print_endline ("Your list of properties is " ^ (list_to_string (Player.get_prop_name_list current_player)));
          print_endline "What property would you like to mortgage?";
          let prop_name = read_line () in
          handle_mortgage_helper current_player prop_name;
          handle_payment current_player current_tile board player_list tile_owner_p "jail"
                

          end
    else begin
      print_endline "You do not have enough money or assets to pay out of jail.";
      handle_bankrupt current_player board;
    end
      end
  
  | "income tax" -> begin
    print_string "You landed on Income Tax. Pay 10% (Option A) or $200 (Option B) (A/B) > ";
    if read_line () = "A" then 
      begin
        let net_worth= Player.get_total_val_in_hand current_player in
        (*FUTURE: EXPAND OUT NETWORTH*)
        let tax=net_worth/10 in
        if (Player.get_balance current_player > tax) then
          begin
            Player.lose_to_free_parking current_player tax;
            Board.incr_amt_in_free_parking board tax;
      
            print_endline("You paid "^string_of_int tax^" in Income Tax. Your new balance is "
            ^ string_of_int (Player.get_balance current_player))
          end
        else
          begin
          print_endline "You do not have any cash to make this payment. What property will you like to mortgage?";
          let prop_name = read_line () in
          handle_mortgage_helper current_player prop_name;
          handle_payment current_player current_tile board player_list tile_owner_p "income tax"
          end
      end
    else 
      if (Player.get_total_val_in_hand current_player >200) then
        if (Player.get_balance current_player >200) then
          begin
            Player.lose_to_free_parking current_player 200;
            Board.incr_amt_in_free_parking board 200;
            print_endline("You paid $200 in Income Tax. Your new balance is "
            ^ string_of_int (Player.get_balance current_player))
          end
        else
          begin 
          print_endline "You do not have any cash to make this payment. What property will you like to mortgage?";
          let prop_list_as_string = (list_to_string (Player.get_prop_name_list current_player)) in
          print_endline ("Your list of properties is " ^ prop_list_as_string);
          let prop_name = read_line () in
          handle_mortgage_helper current_player prop_name;
          handle_payment current_player current_tile board player_list tile_owner_p "income tax"

          end
      else 
        begin
        print_endline "You are unable to pay $200 with your money or assets. Instead. Pay 10% (Option A)";
        handle_payment current_player current_tile board player_list tile_owner_p "income tax"
        end
  end

  | _ -> ()

(**[tile_action] allows the [current_player] to move [this_roll] number of tiles
   on the [board], prints the board, and allows the player buy the property if
   its unowned, pay rent to the property owner, or do nothing if the player owns
   the property *)
let tile_action (current_player : Player.t) (board:Board.t)  =
  let player_list= Board.get_player_list board in
  let current_tile = Player.get_position current_player in
  let () = print_endline ("You landed on " ^ Tile.get_name current_tile) in
  match Tile.get_tile_type current_tile with
  | ActionItem ->
      if Tile.get_name current_tile = "Go To Jail" then
        begin
          Player.increment_jail_count current_player;
          Board.move_player_to_jail board current_player;
          print_endline "Go to Jail and lose your next 3 turns."
        end
      else if Tile.get_name current_tile = "GO" then ()
      else if Tile.get_name current_tile = "Free Parking" then
        let prize=Board.get_amt_in_free_parking board in
        let ()= Board.set_amt_in_free_parking board 0 in
        let () = Player.add_to_balance current_player prize in
        print_endline
          ("You collected "^string_of_int prize^" from Free Parking. Your new balance is "
          ^ string_of_int (Player.get_balance current_player))
      else if Tile.get_name current_tile = "Income Tax" then
        handle_payment current_player current_tile board player_list 0 "income tax"
      else if Tile.get_name current_tile = "Jail" then print_endline "Just Visiting"
      else if Tile.get_name current_tile = "Community Chest" || Tile.get_name current_tile = "Chance" then
        begin 
          print_endline("Press enter to read your card: >");
          let _ = read_line() in
          let card= Random.int 13 in
          if card = 0 then 
            begin
              Player.increment_jail_count current_player;
              Board.move_player_to_jail board current_player;
              print_endline "Go to Jail! You are moved to jail and lose your next 3 turns."
            end
          else if card=1 then 
            let ()= Player.add_to_balance current_player 10 in
            print_endline("You have won second prize in a beauty contest. Collect $10.");
            let line = "Your new balance is " ^ string_of_int (Player.get_balance current_player) in
            print_endline line
          else if card=2 then 
            let()=Player.add_to_balance current_player (50*( List.length (Board.get_player_list board))) in
            let action x = Player.lose_to_free_parking x 50 in
            let()= List.iter action (Board.get_player_list board) in 
            print_endline("Grand opera opening. Collect $50 from every player.");
            let line = "Your balance is now " ^ string_of_int (Player.get_balance current_player) in
            print_endline line
          else if card=3 then 
            let ()= Player.add_to_balance current_player 200 in
            print_endline("Bank error in your favor. Collect $200.");
            let line = "Your balance is now " ^ string_of_int (Player.get_balance current_player) in
            print_endline line
          else if card=4 then
            let () = Player.lose_to_free_parking current_player 15 in 
            let () = Board.incr_amt_in_free_parking board 15 in 
            print_endline("Pay parking ticket $15.");
            let line = "Your balance is now " ^ string_of_int (Player.get_balance current_player) in
            print_endline line
          else if card=5 then 
            let ()= Player.add_to_balance current_player 100 in
            print_endline("Your Xmas fund matures. Collect $100.");
            let line = "Your balance is now " ^ string_of_int (Player.get_balance current_player) in
            print_endline line
          else if card=6 then 
            begin
              Board.move_player_to_go board current_player;
              Player.add_to_balance current_player 200;
              print_endline("You got sent to Go!");
              let line = "Your balance is now " ^ string_of_int (Player.get_balance current_player) in
              print_endline line
            end
          else if card=7 then
            let () = Player.lose_to_free_parking current_player 20 in 
            let () = Board.incr_amt_in_free_parking board 20 in 
            print_endline("Pay poor tax of $20.");
            let line ="Your balance is now " ^ string_of_int (Player.get_balance current_player) in
            print_endline line
          else if card=8 then
            begin
              Board.move_player board current_player (-3); 
              print_endline("Move back 3 spaces. Don't act on the tile you landed on ")
            end
          else if card=9 then
            begin
              Board.move_player board current_player (-10); 
              print_endline("Move back 10 spaces. Don't act on the tile you landed on ")
            end
          else if card=10 then
            let () = Player.lose_to_free_parking current_player 50 in 
            let () = Board.incr_amt_in_free_parking board 50 in 
            print_endline("Lose your wallet, containing $50.");
            print_endline("Your balance is now "^string_of_int (Player.get_balance current_player))
          else if card=11 then
            let () = Player.lose_to_free_parking current_player 150 in 
            let () = Board.incr_amt_in_free_parking board 150 in 
            print_endline("Pay tax of $150.");
            print_endline("Your balance is now "^string_of_int (Player.get_balance current_player))
          else 
            let ()= Player.lose_to_free_parking current_player 150 in 
            let ()=Board.incr_amt_in_free_parking board 150 in 
            print_endline("Pay school tax of $150.");
            print_endline ("Your balance is now " ^ string_of_int (Player.get_balance current_player)) 
        end
      else print_endline "THIS ACTIONITEM IS NOT YET IMPLEMENTED"
  | _ -> begin
      let tile_owner_option = Tile.get_prop_owner current_tile in
      match tile_owner_option with
      | None -> begin
        handle_payment current_player current_tile board player_list 0 "pay for tile";
        let new_prop = Player.get_new_property current_player in
        match new_prop with
          |Some tile when tile = current_tile -> 
              print_endline "End of Roll"
          |_ -> print_endline "Property not bought. End of Roll"
        end
      | Some tile_owner_p -> begin
          if tile_owner_p = Player.get_id current_player then
            print_endline "This is your property. End of Roll"
          else begin
            handle_payment current_player current_tile board player_list tile_owner_p "pay player"
          end
        end
    end

(**[turn] allows the [current_player] to roll the dice, move on the board, and
   take actions given their new position. The players can play up to 3 turns if
   they roll doubles. On their third doubles roll, they get sent to jail **)
let rec turn (board : Board.t) (current_player : Player.t)
     : Board.t =
  
  let () =
    
    if (Player.get_in_jail current_player != 0 && Player.get_in_jail current_player < 4) then (
      print_endline " ";
      print_string
        ("It is "
        ^ Player.get_name current_player
        ^ "'s turn. You are currently in jail. Would you like to pay $50 to \
           get out of jail? Your current balance is "
        ^ string_of_int (Player.get_balance current_player)
        ^ " (Y/N) > ");
      if read_line () = "Y" then 
        begin
        handle_payment current_player (Player.get_position current_player) board (Board.get_player_list board) 0 "jail";
        print_string "Press enter to roll the dice"
        end
      else print_string "You're still in jail. Press enter to roll the dice > ")
    
    else if (Player.get_in_jail current_player = 4) then (
      print_newline ();
      print_endline
        ("It is "
        ^ Player.get_name current_player
        ^ "'s turn.");
      handle_payment current_player (Player.get_position current_player) board (Board.get_player_list board) 0 "jail";
      print_string "Do you want to perform another mortgage(M) or unmortgage(U) action? (Y/N) > ";
      mortgage_decision (read_line())(current_player))
    
    else 
      print_endline " ";
      print_string
        ("It is "
        ^ Player.get_name current_player
        ^ "'s turn. ");
      print_endline ("Your list of properties is " ^ (list_to_string (Player.get_prop_name_list current_player)));
      print_string "Do you want to perform a mortgage(M) or unmortgage(U) action? (Y/N) > ";
      mortgage_decision (read_line())(current_player)
  in
  let _ = read_line () in
  let this_roll = roll () in
  print_endline
    (Player.get_name current_player
    ^ " rolled ("
    ^ string_of_int (fst this_roll)
    ^ ","
    ^ string_of_int (snd this_roll)
    ^ "). ");
  match Player.get_in_jail current_player with
    | 0 ->
        if fst this_roll = snd this_roll then (
          match Player.get_doubles_count current_player with
          | 2 ->
              begin
                Player.increment_jail_count current_player;
                Player.reset_doubles_count current_player;
                Board.move_player_to_jail board current_player;
                print_endline ("You rolled doubles 3 times and were caught Speeding. Go to Jail!")
              end;
              board
          | _ ->
              let num_steps = fst this_roll + snd this_roll in
              Board.move_player board current_player num_steps;
              Board.print_board board;
              tile_action current_player board ;
              let _ = Player.increment_doubles_count current_player in
              let () = print_endline " " in
              let () = print_endline "You rolled doubles -- roll again!" in
              turn board current_player 
        )
        else
          let num_steps = fst this_roll + snd this_roll in
          Board.move_player board current_player num_steps;
          Board.print_board board;
          tile_action current_player board;
          board
    | _ ->
        if fst this_roll = snd this_roll then (
          print_endline "You rolled doubles and got out of jail!";
          let _ = Player.reset_jail_count current_player in
          let num_steps = fst this_roll + snd this_roll in
          Board.move_player board current_player num_steps;
          Board.print_board board;
          tile_action current_player board ;
          board
        )
        else
          let _ =
            Player.increment_jail_count current_player;
            print_endline "You didn't roll doubles. Better luck next time."
          in
          board
  
(**[adjust_player_list] takes in a list of players [plist] and puts the first
   element at the back**)
let adjust_player_list (plist : Player.t list) : Player.t list =
  match plist with
  | h :: t -> t @ [ h ]
  | _ -> plist

let update_player_list (board : Board.t) =  
  Board.set_player_list board (adjust_player_list (Board.get_player_list board))

(**[play_game] takes in a [player_list] and [board] and checks if the game has
   ended. If not, it allows the next player to play their turn**)
let rec play_game (board : Board.t) =
  let player_list = Board.get_player_list board in
  if List.length player_list = 1 then
    print_endline
      ("CONGRAGULATIONS " ^ Player.get_name (List.nth player_list 0) ^ " WINS!")
  else begin
    let current_player = List.nth player_list 0 in
    let changed_board = turn board current_player  in
    let _ = Player.reset_doubles_count current_player in
    let _ = update_player_list board in
    play_game changed_board
  end

(** [start_game] prints a welcome message, initializes players, creates a board,
    and starts game play*)
    let start_game =
      print_endline "Welcome to OCAMLopoly!";
      let () = print_string "How many players? > " in
      let num_players = int_of_string (read_line ()) in
      let () = assert (num_players > 1) in
      let () = assert (num_players < 9) in
      let player_list, board = Board.make_board num_players in
      print_endline " ";
      print_endline
        ("Your players are: "
        ^ String.concat " " (List.map Player.get_name player_list)
        ^ ".");
      print_endline "Let's play! Below is the starting board: ";
      Board.print_board board;  
      print_endline " ";
      play_game board

let () = start_game