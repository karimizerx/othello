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
let pp_player fmt (p : player option) =
  match p with
  | None -> Format.fprintf fmt "•"
  | Some X -> Format.fprintf fmt "X"
  | _ -> Format.fprintf fmt "O"

let equal_hpos (H i) (H j) = i = j
let equal_vpos (V i) (V j) = i = j

let equal_pos ((x1, y1) : pos) ((x2, y2) : pos) =
  equal_hpos x1 x2 && equal_vpos y1 y2

let equal_player p1 p2 =
  match (p1, p2) with
  | None, None -> true
  | Some X, Some X -> true
  | Some O, Some O -> true
  | _, _ -> false

let rec equal_list_player (l1 : player option list) (l2 : player option list) =
  match (l1, l2) with
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false
  | h1 :: t1, h2 :: t2 -> equal_player h1 h2 && equal_list_player t1 t2

let equal_board (b : board) (b1 : board) =
  equal_list_player (List.flatten b) (List.flatten b1)

let pp_hpos fmt n = match n with H i -> Format.fprintf fmt "Pos.(h %d)" i
let pp_vpos fmt n = match n with V i -> Format.fprintf fmt "Pos.(v %d)" i

let pp_board fmt board =
  List.iter
    (fun x ->
      List.iter (fun y -> Format.fprintf fmt "%a" pp_player y) x;
      Format.fprintf fmt "@\n")
    board

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

let get b = function H h, V v -> List.nth (List.nth b h) v

let set b po pl =
  match po with
  | H h, V v ->
      List.mapi
        (fun i line ->
          if i = h then
            List.mapi (fun j el -> if j = v then Some pl else el) line
          else line)
        b

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
