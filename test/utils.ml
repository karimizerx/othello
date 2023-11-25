open Omizer2mizer.Engine
open Omizer2mizer.Arena

let player = Alcotest.testable pp_player equal_player
let hpos = Alcotest.testable pp_hpos equal_hpos
let vpos = Alcotest.testable pp_vpos equal_vpos
let board = Alcotest.testable pp_board equal_board

let res_new_board =
  [
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; Some O; Some X; None; None; None ];
    [ None; None; None; Some X; Some O; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
  ]

let b0 =
  [
    [ Some X; Some X; Some O; None; Some X; Some X; None; None ];
    [ Some O; Some X; Some O; Some O; Some O; None; Some O; Some X ];
    [ Some X; Some X; Some O; None; Some X; Some O; Some O; None ];
    [ Some X; Some X; Some O; None; Some X; Some O; Some O; None ];
    [ Some X; Some X; None; None; Some X; Some X; None; None ];
    [ Some O; None; Some O; Some X; Some O; None; Some O; Some X ];
    [ Some X; Some O; Some O; None; Some X; Some O; Some O; None ];
    [ Some X; Some O; Some O; None; Some X; Some O; Some O; None ];
  ]

let b1 =
  [
    [ Some X; Some X; None; None; Some X; Some X; None; None ];
    [ Some O; None; Some O; Some X; Some O; None; Some O; Some X ];
    [ Some X; Some O; Some O; None; Some X; Some O; Some O; None ];
    [ Some X; Some O; Some O; None; Some X; Some O; Some O; None ];
    [ Some X; Some X; None; None; Some X; Some X; None; None ];
    [ Some O; None; Some O; Some X; Some O; None; Some O; Some X ];
    [ Some X; Some O; Some O; None; Some X; Some O; Some O; None ];
    [ Some X; Some O; Some O; None; Some X; Some O; Some O; None ];
  ]

let b2 =
  [
    [ None; None; None; None; None; None; None; Some X ];
    [ None; None; None; Some O; None; None; Some X; None ];
    [ None; None; None; Some O; None; Some X; None; None ];
    [ None; None; None; Some O; Some X; None; None; None ];
    [ None; None; None; Some X; Some O; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
  ]

let b3 =
  [
    [ Some O; Some X; None; None; Some X; Some X; None; None ];
    [ Some O; None; Some O; Some X; Some O; None; Some O; Some X ];
    [ Some X; Some O; Some O; None; Some X; Some O; Some O; None ];
    [ Some X; Some O; Some O; None; Some X; Some O; Some O; None ];
    [ Some X; Some X; None; None; Some X; Some X; None; None ];
    [ Some O; None; Some O; Some X; Some O; None; Some O; Some X ];
    [ Some X; Some O; Some O; None; Some X; Some O; Some O; None ];
    [ Some X; Some O; Some O; None; Some X; Some O; Some O; Some O ];
  ]

(* Case :  Only [X]'s mark *)
let bX =
  [
    [ Some X; Some X; None; None; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; None; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; None; Some X ];
  ]

let only_x =
  [
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
  ]

(* Case :  No solution for player [O] *)
let bNSO =
  [
    [ Some X; Some X; None; None; Some X; Some X; None; None ];
    [ Some X; Some O; None; None; Some X; Some X; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
  ]

let beq =
  [
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
  ]

let end_match = 
  [
    [ Some X; Some X; None; None; Some X; Some X; None; None ];
    [ Some X; None; None; None; Some X; Some X; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; Some O; None; None ];
    [ None; None; None; None; None; None; None; None ];
  ]

let wrong_size_board =
    [  
      [ None; None; None; None; None; None; None; None ];
      [ None; None; None; None; None; None; None; None ];
      [ None; None; None; None; None; None; None; None ];
      [ None; None; None; None; None; None; None; None ];
      [ None; None; None; None; None; None; None; None ];
      [ None; None; None; None; None; None; None; None ];
      [ Some X; None; None; None; None; None; None; Some X ];
      [ Some O; None; None; None; None; None; None; Some O];
      [ Some O; None; None; None; None; None; None; None ];
      [ None; None; None; None; None; None; None; None ];
    ]

(* Pretty printer for (pos option)'s tests. *)
let pp_pos_opt po =
  match po with None -> "" | Some p -> Format.asprintf "%a" pp_pos p

let trace = [ (H 3, V 2); (H 5, V 6); (H 2, V 3) ]
let endp = Win X
