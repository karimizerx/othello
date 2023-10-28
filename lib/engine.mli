type player = X | O
type hpos = H of int
type vpos = V of int
type pos = hpos * vpos
type board = player option list list

module Pos : sig
  val h : int -> hpos
  val v : int -> vpos
end

(* Pretty printers *)
val pp_player : Format.formatter -> player option -> unit
val pp_hpos : Format.formatter -> hpos -> unit
val pp_vpos : Format.formatter -> vpos -> unit
val pp_board : Format.formatter -> board -> unit
val equal_hpos : hpos -> hpos -> bool
val equal_vpos : vpos -> vpos -> bool
val equal_pos : pos -> pos -> bool

exception Invalid_xpos
exception Invalid_ypos
exception Invalid_move

val init : unit -> board

(* [get board pos] return the state of the [board] at the given position [pos]. *)
val get : board -> pos -> player option

(* [set board pos player] puts the [player]'s mark at the given position [pos] of the [board]. *)
val set : board -> pos -> player -> board

module Verif : sig
  val win : board -> player -> bool

  (*move returns all the positions to set with player, no check whether if the origin position is empty and on the board*)
  val move : board -> player option -> pos -> pos list
  val can_play : board -> player -> bool
end
