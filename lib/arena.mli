open Engine

(*list that keeps track of the moves done by the players until the and of the game*)
type trace = (hpos * vpos) list

val pp_trace : Format.formatter -> trace -> unit
val equal_trace : trace -> trace -> bool

type endplay = Win of player | Giveup of player | Draw

val pp_endplay : Format.formatter -> endplay -> unit
val equal_endplay : endplay -> endplay -> bool

(*checks whether if pos is a free position on the board*)
val check_pos : board -> pos -> bool

(*returns the fonction associated with the player : X : player1 / O : player2*)
val player_function :
  player ->
  (player -> board -> pos option) ->
  (player -> board -> pos option) ->
  player ->
  board ->
  pos option

(*prints the result*)
val endgame : board -> trace -> endplay -> unit

(*plays 1 move*)
val play :
  player ->
  board ->
  (player -> board -> (hpos * vpos) option) ->
  trace ->
  board * trace

(*manages the game*)
val game :
  (player -> board -> (hpos * vpos) option) ->
  (player -> board -> (hpos * vpos) option) ->
  board ->
  unit

val player_teletype : player -> board -> pos option
val player_random : player -> board -> pos option
val player_giveup : player -> board -> pos option
