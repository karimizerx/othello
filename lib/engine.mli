type player = X | O
type hpos = H of int
type vpos = V of int
type pos = hpos * vpos
type board = player option list list

module Pos : sig
  val h : int -> hpos
  val v : int -> vpos
end

(** Pretty printers *)

val pp_player : Format.formatter -> player option -> unit
val pp_hpos : Format.formatter -> hpos -> unit
val pp_vpos : Format.formatter -> vpos -> unit
val pp_pos : Format.formatter -> pos -> unit
val pp_poslist : Format.formatter -> pos list -> unit
val pp_board : Format.formatter -> board -> unit

val equal_player : player option -> player option -> bool
(** Equality functions *)

val equal_hpos : hpos -> hpos -> bool
val equal_vpos : vpos -> vpos -> bool
val equal_pos : pos -> pos -> bool
val equal_board : board -> board -> bool
val equal_list_player : player option list -> player option list -> bool

exception Invalid_hpos
(** Exceptions *)

exception Invalid_vpos
exception Invalid_move

val init : player option list list -> board
val new_board : board

val get : board -> pos -> player option
(** [get board pos] return the state of the [board] at the given position [pos]. *)

val set : board -> player -> pos list -> board
(** [set board pos player] puts the [player]'s mark at the given position [pos] of the [board]. *)

val free_pos : board -> pos list
(** [free_pos board] return the list of all positions not taken by any player *)

val swap_player : player -> player

module Verif : sig
  val win : board -> player -> bool

  val move : board -> player option -> pos -> pos list
  (** move returns all the positions to set with player, no check whether if the origin position is empty and on the board *)

  val possible_move_list : player -> board -> pos list
  (** possible_move_list return a list of the possible pos for the next move*)

  val can_play : board -> player -> bool
  (** [can_play board player] return [true] if the [player] has at least one mark & one solution to play. *)
end
