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

let player_teletype p b =
  ignore (p, b);
  Lwt.return None

let player_random b =
  ignore b;
  Lwt.return None

let player_giveup b =
  ignore b;
  Lwt.return None
