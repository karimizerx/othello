open Engine

type trace = (hpos * vpos) list

val pp_trace : Format.formatter -> trace -> unit
val equal_trace : trace -> trace -> bool

type endplay = Win of player | Giveup of player | Draw

val pp_endplay : Format.formatter -> endplay -> unit
val equal_endplay : endplay -> endplay -> bool

val play :
  player ->
  board ->
  (player -> board -> (hpos * vpos) option) ->
  trace ->
  board * trace

val game :
  (player -> board -> (hpos * vpos) option) ->
  (player -> board -> (hpos * vpos) option) ->
  board ->
  unit

val player_teletype : player -> board -> pos option
val player_random : player -> board -> pos option
val player_giveup : player -> board -> pos option
