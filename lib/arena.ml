open Engine

type trace = (hpos * vpos) list
type endplay = Win of player | Giveup of player | Draw

let pp_trace fmt t = List.iter (fun p -> Format.fprintf fmt "%a " pp_pos p) t
let equal_trace (t1 : pos list) (t2 : pos list) = t1 = t2
(*List.equal equal_pos t1 t2*)

let pp_endplay fmt ep =
  match ep with
  | Win p -> Format.fprintf fmt "%a won the game" pp_player (Some p)
  | Giveup p -> Format.fprintf fmt "%a gave up :(" pp_player (Some p)
  | _ -> Format.fprintf fmt "Draw"

let equal_endplay e1 e2 = e1 = e2
let check_pos board (h, v) = List.exists (equal_pos (h, v)) (free_pos board)

let rec play (player : player) (board : board)
    (f_player : player -> board -> (hpos * vpos) option) (trace : trace) =
  let open Engine.Verif in
  let choice = f_player player board in

  match choice with
  | None -> (board, trace)
  | Some p ->
      if check_pos board p then
        let to_change = move board (Some player) p in
        if List.length to_change = 1 then play player board f_player trace
        else (set board player to_change, List.append trace [ p ])
      else play player board f_player trace

let player_fonction player f_p1 f_p2 = match player with X -> f_p1 | _ -> f_p2

let endgame board trace status =
  Format.printf "@[<v>%a@]@," pp_endplay status;
  Format.printf "board : @[<v>%a@]@." pp_board board;
  Format.printf "trace : @[<v>%a@]@." pp_trace trace;
  ()

(*player 1 : X | player 2 : O*)
let game fonction_player1 fonction_player2 init_board =
  let open Verif in
  let rec go board player fonction_player1 fonction_player2 (trace : trace) =
    let new_board, new_trace =
      play (swap_player player) board
        (player_fonction player fonction_player1 fonction_player2)
        trace
    in
    if equal_board board new_board then
      endgame board trace (Giveup (swap_player player))
    else if win new_board player && win new_board (swap_player player) then
      endgame new_board new_trace Draw
    else if win new_board player then endgame new_board new_trace (Win player)
    else if win new_board player then
      endgame new_board new_trace (Win (swap_player player))
    else
      go new_board (swap_player player) fonction_player1 fonction_player2
        new_trace
  in
  go init_board O fonction_player1 fonction_player2 []

let player_teletype p b =
  Format.printf "@[<v>It's player %a's turn.@," pp_player (Some p);
  Format.printf "Board:  @[<v>%a@]@," pp_board b;

  Format.printf "Choose your move : @]@.";
  try
    Scanf.scanf "%c%d\n" (fun i j ->
        Some (Pos.h (int_of_char i - int_of_char 'A'), Pos.v j))
  with Scanf.Scan_failure _ -> None

let player_random p b =
  ignore (p, b);
  Some (H 1, V 1)

let player_giveup p b =
  ignore (p, b);
  Some (H 1, V 1)
