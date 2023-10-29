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
val pp_pos : Format.formatter -> pos -> unit
val pp_poslist : Format.formatter -> pos list -> unit
val pp_board : Format.formatter -> board -> unit

(* Equality functions *)
val equal_player : player option -> player option -> bool
val equal_hpos : hpos -> hpos -> bool
val equal_vpos : vpos -> vpos -> bool
val equal_pos : pos -> pos -> bool
val equal_board : board -> board -> bool

(* Exceptions *)
exception Invalid_hpos
exception Invalid_vpos
exception Invalid_move

val init : unit -> board

(* [get board pos] return the state of the [board] at the given position [pos]. *)
val get : board -> pos -> player option

(* [set board pos player] puts the [player]'s mark at the given position [pos] of the [board]. *)
val set : board -> pos -> player -> board

(* [free_pos board] return the list of all positions not taken by any player *)
val free_pos : board -> pos list

module Verif : sig
  val win : board -> player -> bool

  (* move returns all the positions to set with player, no check whether if the origin position is empty and on the board *)
  val move : board -> player option -> pos -> pos list

  (* [can_play board player] return [true] if the [player] has at least one mark & one solution to play. *)
  val can_play : board -> player -> bool
end
