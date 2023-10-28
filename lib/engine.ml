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

let pp_hpos oc n = match n with H i -> Format.fprintf oc "Pos.(h %d)" i
let pp_vpos oc n = match n with V i -> Format.fprintf oc "Pos.(v %d)" i
let equal_hpos (H i) (H j) = i = j
let equal_vpos (V i) (V j) = i = j

let equal_pos ((x1, y1) : pos) ((x2, y2) : pos) =
  equal_hpos x1 x2 && equal_vpos y1 y2

exception Invalid_xpos
exception Invalid_ypos
exception Invalid_move

let init b =
  assert (List.length b = 8);
  assert (List.for_all (fun line -> List.length line = 8) b);
  b

let new_board : board =
  let l = List.init 3 (fun _ -> List.init 8 (fun _ -> None)) in
  let centers =
    List.init 3 (fun _ -> None)
    @ [ Some O; Some X ]
    @ List.init 3 (fun _ -> None)
  in
  init (l @ [ centers ] @ [ List.rev centers ] @ l)

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
