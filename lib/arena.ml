open Engine

type trace = (hpos * vpos) list
type endplay = Win of player | Giveup of player | Draw

(* Pretty printers *)
let pp_trace = pp_poslist

let pp_endplay fmt ep =
  match ep with
  | Win p -> Format.fprintf fmt "%a won the game" pp_player (Some p)
  | Giveup p -> Format.fprintf fmt "%a gave up :(" pp_player (Some p)
  | _ -> Format.fprintf fmt "Draw"

(* Equality functions*)
let equal_trace (t1 : pos list) (t2 : pos list) = t1 = t2
let equal_endplay e1 e2 = e1 = e2

let check_pos board (h, v) =
  List.exists (equal_pos (h, v)) (Verif.free_pos board)

let player_function player f_p1 f_p2 = match player with X -> f_p1 | _ -> f_p2

let endgame board trace =
  Format.printf "board : @[<v>%a@]@." pp_board board;
  Format.printf "trace : @[<v>%a@]@." pp_trace trace;
  ()

let end_status board trace ((giveup, player) : bool * player) =
  let open Verif in
  (if giveup then Format.printf "@[<v>%a@]@," pp_endplay (Giveup player)
   else
     match (win board X, win board O) with
     | true, true -> Format.printf "@[<v>%a@]@," pp_endplay Draw
     | true, false -> Format.printf "@[<v>%a@]@," pp_endplay (Win X)
     | _ -> Format.printf "@[<v>%a@]@," pp_endplay (Win O));
  match (cnt_points board X, cnt_points board O) with
  | _, 0 -> Format.printf "X : 64 - O : 0@."
  | 0, _ -> Format.printf "X : 0 - O : 64@."
  | cnt_X, cnt_O ->
      Format.printf "X : %d - O : %d@." cnt_X cnt_O;
      endgame board trace

let rec play (player : player) (board : board)
    (f_player : player -> board -> (hpos * vpos) option) (trace : trace)
    cpt_invalid =
  let open Engine.Verif in
  if cpt_invalid > 5 || not (Engine.correct_board board) then (board, trace)
  else
    let choice = f_player player board in
    match choice with
    | None -> (board, trace)
    | Some p ->
        if check_pos board p then
          let to_change = move board (Some player) p in
          if List.length to_change = 1 then
            (Format.printf "Invalid move ! Try again, you have %d tries left.@."
               (5 - cpt_invalid);
             play player board f_player trace)
              (cpt_invalid + 1)
          else (set board player to_change, List.append trace [ p ])
        else (
          Format.printf "Invalid move ! Try again, you have %d tries left.@."
            (5 - cpt_invalid);
          play player board f_player trace (cpt_invalid + 1))

(*player 1 : X | player 2 : O*)
let game function_player1 function_player2 =
  let open Verif in
  let rec go board player function_player1 function_player2 (trace : trace) =
    if (not (can_play board X)) && not (can_play board O) then
      end_status board trace (false, player)
    else
      let current_player, current_function =
        if not (can_play board (swap_player player)) then
          (player, player_function player function_player1 function_player2)
        else
          ( swap_player player,
            player_function (swap_player player) function_player1
              function_player2 )
      in
      let new_board, new_trace =
        play current_player board current_function trace 0
      in
      if equal_board board new_board then
        end_status board trace (true, current_player)
      else
        go new_board current_player function_player1 function_player2 new_trace
  in
  go init_board O function_player1 function_player2 []

(*Player's functions*)

let player_teletype p b =
  if not (Engine.correct_board b) then None
  else (
    Format.printf "Board:  @[<v>%a@]@," pp_board b;
    Format.printf "@[<v>It's player %a's turn.@," pp_player (Some p);
    Format.printf "@[<v>Possible moves : %a@]@," pp_poslist
      (Verif.possible_move_list p b);

    Format.printf "Choose your move : @]@.";
    try
      Scanf.scanf "%c%d\n" (fun i j ->
          Some (Pos.h (int_of_char i - int_of_char 'A'), Pos.v j))
    with Scanf.Scan_failure _ -> None)

let player_random p b =
  if not (Engine.correct_board b) then None
  else
    let open Verif in
    let list_of_move = possible_move_list p b in
    if List.length list_of_move > 0 then
      Some (List.nth list_of_move (Random.int (List.length list_of_move)))
    else None

let player_giveup p b =
  if not (Engine.correct_board b) then None
  else (
    ignore (p, b);
    None)

let player_invalid p b =
  if not (Engine.correct_board b) then None
  else (
    ignore p;
    let rec invalid_pos b free_positions =
      match free_positions with
      | [] -> None
      | pos :: tl ->
          if List.mem pos free_positions then Some pos else invalid_pos b tl
    in
    invalid_pos b (Verif.free_pos b))

let player_invalid2 p b =
  ignore (p, b);
  Some (H (-1), V (-1))
