open Engine

type trace = (hpos * vpos) list
type endplay = Win of player | Giveup of player | Draw

type result = {
  trace : trace;  (** Trace *)
  endplay : endplay;  (** Outcome *)
  final : board;  (** Final state *)
}

let pp_trace fmt t = List.iter (fun p -> Format.fprintf fmt "%a " pp_pos p) t
let equal_trace (t1 : pos list) (t2 : pos list) = t1 = t2
(*List.equal equal_pos t1 t2*)

let pp_endplay fmt ep =
  match ep with
  | Win p -> Format.fprintf fmt "%a won the game" pp_player (Some p)
  | Giveup p -> Format.fprintf fmt "%a gave up" pp_player (Some p)
  | _ -> Format.fprintf fmt "Draw"

let equal_endplay e1 e2 = e1 = e2

let arena p b f =
  ignore (p, b, f);
  Lwt.return { trace = []; endplay = Draw; final = [] }

let rec possible_move_list_aux p b l free_pos =
  let open Verif in
  match free_pos with
  | h :: t ->
    if List.length (move b (Some p) h) > 1 then
      possible_move_list_aux p b (l @ [ h ]) t
    else possible_move_list_aux p b l t
  | [] -> l

let possible_move_list p b = possible_move_list_aux p b [] (free_pos b)

let player_teletype p b =
  Format.printf "@[<v>It's player %a's turn.@," pp_player (Some p);
  Format.printf "Board:  @[<v>%a@]@," pp_board b;
  Format.printf "Possible moves : @[<v>%a@]@," pp_poslist
    (possible_move_list p b);
  Format.printf "Choose your move : @]";
  try
    Scanf.scanf "%c%d\n" (fun i j ->
        Lwt.return (Some (Pos.h (int_of_char i - int_of_char 'A'), Pos.v j)))
  with Scanf.Scan_failure _ -> Lwt.return None

let player_random b =
  ignore b;
  Lwt.return None

let player_giveup b =
  ignore b;
  Lwt.return None
