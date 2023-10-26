type player = X | O
type xpos = X of int
type ypos = Y of int
type pos = xpos * ypos
type board = player option list list

module Pos = struct
  let x i = X i
  let y i = Y i
end

(* Pretty printers *)
let pp_player oc (p : player) =
  match p with X -> Format.fprintf oc "X" | _ -> Format.fprintf oc "O"

let pp_xpos oc n = match n with X i -> Format.fprintf oc "Pos.(x %d)" i
let pp_xpos oc n = match n with Y i -> Format.fprintf oc "Pos.(y %d)" i

let equal_xpos a b =
  ignore (a, b);
  true

let equal_ypos a b =
  ignore (a, b);
  true

let equal_pos a b =
  ignore (a, b);
  true

exception Invalid_xpos
exception Invalid_ypos
exception Invalid_move

let init () = []

let get b p =
  ignore (b, p);
  None

let set b p pl =
  ignore (b, p, pl);
  []

module Verif = struct
  let win b p =
    ignore (b, p);
    true

  let move b p po =
    ignore (b, p, po);
    true

  let can_play b p =
    ignore (b, p);
    true
end
