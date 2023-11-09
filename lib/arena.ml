open Engine

type trace = (hpos * vpos) list

let pp_trace fmt t = ignore (fmt, t)

let equal_trace t1 t2 =
  ignore (t1, t2);
  true

type endplay = Win of player | Giveup of player | Draw

let pp_endplay fmt ep = ignore (fmt, ep)

let equal_endplay e1 e2 =
  ignore (e1, e2);
  true

type result = {
  trace : trace;  (** Trace *)
  endplay : endplay;  (** Outcome *)
  final : board;  (** Final state *)
}
(** Final state of the game *)

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
