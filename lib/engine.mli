type player = X | O
type xpos = X of int
type ypos = Y of int
type pos = xpos * ypos
type board = player option list list

module Pos : sig
  val x : int -> xpos
  val y : int -> ypos
end

(* Pretty printers *)
val pp_player : Format.formatter -> player -> unit
val pp_xpos : Format.formatter -> xpos -> unit
val pp_ypos : Format.formatter -> ypos -> unit
val equal_xpos : xpos -> xpos -> bool
val equal_ypos : ypos -> ypos -> bool
val equal_pos : pos -> pos -> bool

exception Invalid_xpos
exception Invalid_ypos
exception Invalid_move

val init : unit -> board
val get : board -> pos -> player option
val set : board -> pos -> player -> board

module Verif : sig
  val win : board -> player -> bool
  val move : board -> player -> pos -> bool
  val can_play : board -> player -> bool
end
