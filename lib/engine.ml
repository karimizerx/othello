type player = X | O
type xpos = X of int
type ypos = Y of int
type pos = xpos * ypos
type board = player option list list

module Pos = struct
  let x i = X i
  let y i = Y i
end


let equal_xpos (X i) (X j) =  i = j
let equal_ypos (Y i) (Y j) = i = j
let equal_pos (x1,y1) (x2,y2)= (equal_xpos x1 x2) && (equal_ypos y1 y2)

exception Invalid_xpos
exception Invalid_ypos
exception Invalid_move

let init b = 
  assert (List.length b = 8);
  assert (List.for_all (fun line -> List.length line = 8) b); 
  b

let new_board = 
  let l = List.init 3 (fun _ -> List.init 8 (fun _ -> None )) in
  let centers = (List.init 3 (fun _ -> None)) @ [Some O;Some X] @ (List.init 3 (fun _ -> None)) in 
  init (l @ [centers] @ ([List.rev centers]) @ l)


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
