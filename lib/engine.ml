type player = X | O
type hpos = H of int
type vpos = V of int
type pos = hpos * vpos
type board = player option list list

module Pos = struct
  let h i = H i
  let v i = V i
end

(* Pretty printers *)
let pp_player oc (p : player) =
  match p with X -> Format.fprintf oc "X" | _ -> Format.fprintf oc "O"

let pp_hpos fmt n = match n with H i -> Format.fprintf fmt "Pos.(h %d)" i
let pp_vpos fmt n = match n with V i -> Format.fprintf fmt "Pos.(v %d)" i

let pp_board fmt board =
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          match y with
          | None -> Format.fprintf fmt "•"
          | Some p -> Format.fprintf fmt "%a" pp_player p)
        x;
      Format.fprintf fmt "@\n")
    board

let equal_hpos a b =
  ignore (a, b);
  true

let equal_vpos a b =
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
