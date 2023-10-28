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

let pp_hpos fmt n = match n with H i -> Format.fprintf fmt "Pos.(h %d)" i
let pp_vpos fmt n = match n with V i -> Format.fprintf fmt "Pos.(v %d)" i

let pp_board fmt board =
  List.iter
    (fun x ->
      List.iter (fun y -> Format.fprintf fmt "%a" pp_player y) x;
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
