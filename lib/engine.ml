type player = X | O
type hpos = H of int
type vpos = V of int
type pos = hpos * vpos
type board = player option list list

module Pos = struct
  let h i = H i
  let v i = V i
end

exception Invalid_move

(* Pretty printers *)

let pp_player fmt (pl : player option) =
  match pl with
  | None -> Format.fprintf fmt "•"
  | Some X -> Format.fprintf fmt "X"
  | _ -> Format.fprintf fmt "O"

let pp_hpos fmt n =
  match n with
  | H i -> Format.fprintf fmt "%c" (char_of_int (i + int_of_char 'A'))

let pp_vpos fmt n = match n with V i -> Format.fprintf fmt "%d" i
let pp_pos fmt ((h, v) : pos) = Format.fprintf fmt "%a%a" pp_hpos h pp_vpos v

let pp_poslist fmt polist =
  List.iter (fun y -> Format.fprintf fmt "%a " pp_pos y) polist

let pp_board fmt board =
  List.iteri
    (fun v x ->
      if v == 0 then (
        let l = List.init (List.length board) (fun j -> j) in
        List.iter
          (fun i ->
            Format.fprintf fmt "%s%d   " (if i == 0 then "   │ " else "") i)
          l;
        Format.fprintf fmt "@,";
        List.iter
          (fun i -> Format.fprintf fmt "%s────" (if i == 0 then "───┼" else ""))
          l;
        Format.fprintf fmt "@,");
      List.iteri
        (fun h y ->
          Format.fprintf fmt "%s%a   "
            (if h = 0 then
               " "
               ^ (v + int_of_char 'A' |> char_of_int |> String.make 1)
               ^ " │ "
             else "")
            pp_player y)
        x;
      Format.fprintf fmt "@,   │@,")
    board

(* Equality functions *)

let equal_hpos (H i) (H j) = i = j
let equal_vpos (V i) (V j) = i = j

let equal_pos ((x1, y1) : pos) ((x2, y2) : pos) =
  equal_hpos x1 x2 && equal_vpos y1 y2

let equal_player p1 p2 =
  match (p1, p2) with
  | None, None -> true
  | Some X, Some X -> true
  | Some O, Some O -> true
  | Some _, None | None, Some _ -> false
  | Some X, Some O | Some O, Some X -> false

let rec equal_list_player (l1 : player option list) (l2 : player option list) =
  match (l1, l2) with
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false
  | h1 :: t1, h2 :: t2 -> equal_player h1 h2 && equal_list_player t1 t2

let equal_board (b : board) (b1 : board) =
  equal_list_player (List.flatten b) (List.flatten b1)

let init_board : board =
  let init b =
    assert (List.length b = 8);
    assert (List.for_all (fun line -> List.length line = 8) b);
    b
  in
  let l = List.init 3 (fun _ -> List.init 8 (fun _ -> None)) in
  let centers =
    List.init 3 (fun _ -> None)
    @ [ Some O; Some X ]
    @ List.init 3 (fun _ -> None)
  in
  init (l @ [ centers ] @ [ List.rev centers ] @ l)

let get (b : board) ((H h, V v) : pos) = List.nth (List.nth b h) v

let rec set (b : board) pl pos_list =
  match pos_list with
  | [] -> b
  | (H h, V v) :: tl ->
      set
        (List.mapi
           (fun i line ->
             if i = h then
               List.mapi (fun j el -> if j = v then Some pl else el) line
             else line)
           b)
        pl tl

let swap_player = function X -> O | O -> X
let swap_player_opt = function None -> None | Some p -> Some (swap_player p)

module Verif = struct
  let not_border_dir ((H h, V v) : pos) dir =
    match dir with
    | 0 ->
        if equal_vpos (V v) (V 0) then raise Invalid_move (*west*)
        else (H h, V (v - 1))
    | 1 ->
        if equal_hpos (H h) (H 0) || equal_vpos (V v) (V 0) then
          raise Invalid_move (*north west*)
        else (H (h - 1), V (v - 1))
    | 2 ->
        if equal_hpos (H h) (H 0) then raise Invalid_move (*north*)
        else (H (h - 1), V v)
    | 3 ->
        if equal_vpos (V v) (V 7) || equal_hpos (H h) (H 0) then
          raise Invalid_move (*north east*)
        else (H (h - 1), V (v + 1))
    | 4 ->
        if equal_vpos (V v) (V 7) then raise Invalid_move (*east*)
        else (H h, V (v + 1))
    | 5 ->
        if equal_hpos (H h) (H 7) || equal_vpos (V v) (V 7) then
          raise Invalid_move (*south east*)
        else (H (h + 1), V (v + 1))
    | 6 ->
        if equal_hpos (H h) (H 7) then raise Invalid_move (*south*)
        else (H (h + 1), V v)
    | _ ->
        if equal_hpos (H h) (H 7) || equal_vpos (V v) (V 0) then
          raise Invalid_move (*south west*)
        else (H (h + 1), V (v - 1))

  (*returns the next board case in dir direction if it contains player*)
  let next_pos_player board player pos dir =
    try
      let next_pos = not_border_dir pos dir in
      if equal_player (get board next_pos) player then next_pos
      else (H (-2), V (-2))
    with _ -> (H (-1), V (-1))

  let rec same_player_line board player pos dir res =
    let next_pos = next_pos_player board player pos dir in
    if equal_pos next_pos (H (-1), V (-1)) then []
      (*reached end of board before reaching an opponent*)
    else if equal_pos next_pos (H (-2), V (-2)) then
      (*reached an empty square or an opponent*)
      if get board (not_border_dir pos dir) = swap_player_opt player then res
      else []
    else same_player_line board player next_pos dir (res @ [ next_pos ])

  (*returns all the positions we need to set after a move by player, from pos in dir*)
  let move_dir board player pos dir =
    let next_pos = next_pos_player board (swap_player_opt player) pos dir in
    if equal_pos next_pos (H (-1), V (-1)) then []
      (*first next pos is out of board*)
    else if equal_pos next_pos (H (-2), V (-2)) then []
      (*first next pos contains same player as pos, or no player*)
    else
      same_player_line board (swap_player_opt player) next_pos dir [ next_pos ]

  let move (b : board) pl pos =
    let rec move_in b pl pos dir l_pos =
      if dir > 7 then l_pos
      else
        let new_l = move_dir b pl pos dir in
        move_in b pl pos (dir + 1) (l_pos @ new_l)
    in
    move_in b pl pos 0 [ pos ]

  let free_pos b : pos list =
    let copy_b = List.append [] b in
    let listofpos =
      List.concat
        (List.mapi
           (fun i line -> List.mapi (fun j _ -> (Pos.h i, Pos.v j)) line)
           copy_b)
    in
    List.filter
      (fun pos -> match get b pos with None -> true | _ -> false)
      listofpos

  let possible_move_list p b =
    let rec possible_move_list_aux p b l free_pos =
      match free_pos with
      | h :: t ->
          if List.length (move b (Some p) h) > 1 then
            possible_move_list_aux p b (l @ [ h ]) t
          else possible_move_list_aux p b l t
      | [] -> l
    in
    possible_move_list_aux p b [] (free_pos b)

  let can_play b pl = List.length (possible_move_list pl b) > 0

  let cnt_points b p =
    b |> List.flatten
    |> List.fold_left
         (fun i cp -> match cp with Some x when p = x -> i + 1 | _ -> i)
         0

  let win b p =
    if can_play b p || can_play b (swap_player p) then false
    else cnt_points b p >= cnt_points b (swap_player p)
end
