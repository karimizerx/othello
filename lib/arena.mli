open Engine

type trace = (hpos * vpos) list
type endplay = Win of player | Giveup of player | Draw

(* Pretty printers *)
val pp_trace : Format.formatter -> trace -> unit
val pp_endplay : Format.formatter -> endplay -> unit

(* Equality functions *)
val equal_trace : trace -> trace -> bool
val equal_endplay : endplay -> endplay -> bool

val check_pos : board -> pos -> bool
(** checks whether if pos is a free position on the board *)

val player_function :
  player ->
  (player -> board -> pos option) ->
  (player -> board -> pos option) ->
  player ->
  board ->
  pos option
(** returns the fonction associated to the player : X : player1 / O : player2 *)

val play :
  player ->
  board ->
  (player -> board -> (hpos * vpos) option) ->
  trace ->
  int ->
  board * trace
(** plays 1 move *)

val game :
  (player -> board -> (hpos * vpos) option) ->
  (player -> board -> (hpos * vpos) option) ->
  unit
(** manages the game *)

(* Players Functions *)

val player_teletype : player -> board -> pos option
(** [player_teletype player board] returns the position the user teletyped *)

val player_random : player -> board -> pos option
(** [player_random player board]  return a random & valid position for [player] *)

val player_giveup : player -> board -> pos option
(** [player_giveup player board] always fails to propose a move. *)

val player_invalid : player -> board -> pos option
(** [player_invalid player board] always returns a position in the free positions, but not in the possible moves *)

val player_invalid2 : player -> board -> pos option
(** [player_invalid2 player board] always returns a position outside of the board*)
