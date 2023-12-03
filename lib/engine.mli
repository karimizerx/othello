type player = X | O
type hpos = H of int
type vpos = V of int
type pos = hpos * vpos
type board = player option list list

module Pos : sig
  val h : int -> hpos
  val v : int -> vpos
end

(* Exception *)

exception Invalid_move

(* Pretty printers *)

val pp_player : Format.formatter -> player option -> unit
val pp_hpos : Format.formatter -> hpos -> unit
val pp_vpos : Format.formatter -> vpos -> unit
val pp_pos : Format.formatter -> pos -> unit
val pp_poslist : Format.formatter -> pos list -> unit
val pp_board : Format.formatter -> board -> unit

(* Verif of the elements *)

val correct_board : board -> bool

(* Equality functions *)

val equal_player : player option -> player option -> bool
val equal_hpos : hpos -> hpos -> bool
val equal_vpos : vpos -> vpos -> bool
val equal_pos : pos -> pos -> bool
val equal_board : board -> board -> bool
val equal_list_player : player option list -> player option list -> bool

val init_board : board
(** [init_board] returns the opening board *)

val get : board -> pos -> player option
(** [get board pos] returns the player at the position *)

val set : board -> player -> pos list -> board
(** [set board pos_list player] returns the board, with all the positions of the list set to player *)

val swap_player : player -> player
(** [swap_player player] returns the other player*)

module Verif : sig
  val move : board -> player option -> pos -> pos list
  (** [move board player pos] returns the position list of positions to set with player, returns [pos] if the move isn't valid, no check whether if the position is empty / on the board  *)

  val free_pos : board -> pos list
  (** [free_pos board] returns the list of all the positions not taken by any player *)

  val possible_move_list : player -> board -> pos list
  (** [possible_move_list player board] returns the list of the valid positions for player's next move *)

  val can_play : board -> player -> bool
  (** [can_play board player] returns true if player has at least one valid move *)

  val cnt_points : board -> player -> int
  (** [cnt_points board player] returns player's points *)

  val win : board -> player -> bool
  (** [win board player] returns true if player has won, returns false if the one of the player can play, returns true for both players if they are tied*)
end
