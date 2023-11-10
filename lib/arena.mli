open Engine

type trace = (hpos * vpos) list

val pp_trace : Format.formatter -> trace -> unit
val equal_trace : trace -> trace -> bool

type endplay = Win of player | Giveup of player | Draw

val pp_endplay : Format.formatter -> endplay -> unit
val equal_endplay : endplay -> endplay -> bool

type result = {
  trace : trace;  (** Trace *)
  endplay : endplay;  (** Outcome *)
  final : board;  (** Final state *)
}
(** Final state of the game *)

val arena :
  player ->
  board ->
  (player -> board -> (hpos * vpos) option Lwt.t) ->
  (* fonction joueur *)
  result Lwt.t
(** [arena ~init_player ~init_board players] simulates a game between
    [players X] and [players O]. By default, [init_player] is [X]
    while [init_board] is [empty]. **)

val possible_move_list : player -> board -> pos list  
val player_teletype : player -> board -> pos option Lwt.t
val player_random : board -> pos option Lwt.t

val player_giveup : board -> pos option Lwt.t
(** [player_giveup] always fails to propose a move. *)
