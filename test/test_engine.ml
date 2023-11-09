open Omizer2mizer.Engine

let player = Alcotest.testable pp_player equal_player
let hpos = Alcotest.testable pp_hpos equal_hpos
let vpos = Alcotest.testable pp_vpos equal_vpos
let board = Alcotest.testable pp_board equal_board

let b =
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

let test_pp_player1 =
  let result = Format.asprintf "%a" pp_player (Some O) in
  Alcotest.test_case "pp_player" `Quick (fun () ->
      Alcotest.(check string) "same result" "O" result)

let test_pp_player2 =
  let result = Format.asprintf "%a" pp_player None in
  Alcotest.test_case "pp_player" `Quick (fun () ->
      Alcotest.(check string) "same result" "•" result)

let test_pp_hpos =
  let result = Format.asprintf "%a" pp_hpos (Pos.h 7) in
  Alcotest.test_case "pp_hpos" `Quick (fun () ->
      Alcotest.(check string) "same result" "H" result)

let test_pp_vpos =
  let result = Format.asprintf "%a" pp_vpos (Pos.v 7) in
  Alcotest.test_case "pp_vpos" `Quick (fun () ->
      Alcotest.(check string) "same result" "7" result)

let test_pp_pos =
  let result = Format.asprintf "%a" pp_pos (Pos.h 7, Pos.v 3) in
  Alcotest.test_case "pp_pos" `Quick (fun () ->
      Alcotest.(check string) "same result" "H3" result)

let test_pp_poslist =
  let result =
    Format.asprintf "%a" pp_poslist
      [ (Pos.h 0, Pos.v 0); (Pos.h 1, Pos.v 4); (Pos.h 5, Pos.v 7) ]
  in
  let desired = "A0 B4 F7 " in
  Alcotest.test_case "pp_poslist" `Quick (fun () ->
      Alcotest.(check string) "same result" desired result)

let test_pp_board =
  let result = Format.asprintf "@[<h>%a@]" pp_board b in
  let desired =
    "   \226\148\130 0   1   2   3   4   5   6   7   \
     \226\148\128\226\148\128\226\148\128\226\148\188\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128 \
     A \226\148\130 X   X   \226\128\162   \226\128\162   X   X   \
     \226\128\162   \226\128\162      \226\148\130 B \226\148\130 O   \
     \226\128\162   O   X   O   \226\128\162   O   X      \226\148\130 C \
     \226\148\130 X   O   O   \226\128\162   X   O   O   \226\128\162      \
     \226\148\130 D \226\148\130 X   O   O   \226\128\162   X   O   O   \
     \226\128\162      \226\148\130 E \226\148\130 X   X   \226\128\162   \
     \226\128\162   X   X   \226\128\162   \226\128\162      \226\148\130 F \
     \226\148\130 O   \226\128\162   O   X   O   \226\128\162   O   X      \
     \226\148\130 G \226\148\130 X   O   O   \226\128\162   X   O   O   \
     \226\128\162      \226\148\130 H \226\148\130 X   O   O   \226\128\162   \
     X   O   O   \226\128\162      \226\148\130"
  in
  Alcotest.test_case "pp_board" `Quick (fun () ->
      Alcotest.(check string) "same result" desired result)

let test_new_board =
  Alcotest.test_case "new_board" `Quick (fun () ->
      Alcotest.(check board) "same result" new_board res_new_board)

let test_equal_hpos =
  Alcotest.test_case "equal_hpos" `Quick (fun () ->
      Alcotest.(check bool) "same result" (equal_hpos (H 2) (H 2)) true)

let test_equal_vpos =
  Alcotest.test_case "equal_vpos" `Quick (fun () ->
      Alcotest.(check bool) "same result" (equal_vpos (V 2) (V 8)) false)

let test_equal_pos =
  Alcotest.test_case "equal_pos" `Quick (fun () ->
      Alcotest.(check bool) "same result" (equal_pos (H 1, V 2) (H 1, V 2)) true)

let test_set =
  let set1 = set b (Pos.h 0, Pos.v 0) (O : player) in
  let result = set set1 (Pos.h 7, Pos.v 7) (O : player) in
  let desired = b3 in
  Alcotest.test_case "set" `Quick (fun () ->
      Alcotest.(check board) "same result" desired result)

let test_get =
  let result =
    [
      get b (Pos.h 0, Pos.v 0);
      get b (Pos.h 3, Pos.v 1);
      get b (Pos.h 6, Pos.v 7);
    ]
  in
  let desired = [ Some X; Some O; None ] in
  Alcotest.test_case "get" `Quick (fun () ->
      Alcotest.(check (list player)) "same result" desired result)

let test_free_pos =
  let result = free_pos bX in
  let desired =
    [
      (Pos.h 0, Pos.v 2);
      (Pos.h 0, Pos.v 3);
      (Pos.h 1, Pos.v 3);
      (Pos.h 7, Pos.v 6);
    ]
  in
  Alcotest.test_case "free_pos" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_win_1 =
  let result = Verif.win bX X in
  let desired = true in
  Alcotest.test_case "x win" `Quick (fun () ->
      Alcotest.(check bool) "same result" desired result)

let test_win_2 =
  let result = Verif.win beq X in
  let desired = false in
  Alcotest.test_case "equality" `Quick (fun () ->
      Alcotest.(check bool) "same result" desired result)

let test_win_3 =
  let result = Verif.win b X && Verif.win b O in
  let desired = false in
  Alcotest.test_case "winner uniqueness" `Quick (fun () ->
      Alcotest.(check bool) "same result" desired result)

let test_move1 =
  let desired =
    [
      (Pos.h 1, Pos.v 1);
      (Pos.h 1, Pos.v 2);
      (Pos.h 2, Pos.v 1);
      (Pos.h 3, Pos.v 1);
    ]
  in
  let result = Verif.move b (Some X) (Pos.h 1, Pos.v 1) in
  Alcotest.test_case "move" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_move2 =
  let desired = [ (Pos.h 0, Pos.v 7) ] in
  let result = Verif.move b (Some X) (Pos.h 0, Pos.v 7) in
  Alcotest.test_case "move" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_move3 =
  let desired =
    [ (Pos.h 7, Pos.v 7); (Pos.h 7, Pos.v 6); (Pos.h 7, Pos.v 5) ]
  in
  let result = Verif.move b (Some X) (Pos.h 7, Pos.v 7) in
  Alcotest.test_case "move" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_move4 =
  let desired = [ (Pos.h 5, Pos.v 6) ] in
  let result = Verif.move b (Some O) (Pos.h 5, Pos.v 6) in
  Alcotest.test_case "move" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_move5 =
  let desired = [ (Pos.h 5, Pos.v 2) ] in
  let result = Verif.move b2 (Some O) (Pos.h 5, Pos.v 2) in
  Alcotest.test_case "move" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_move6 =
  let desired =
    [
      (Pos.h 0, Pos.v 3);
      (Pos.h 1, Pos.v 3);
      (Pos.h 2, Pos.v 3);
      (Pos.h 3, Pos.v 3);
    ]
  in
  let result = Verif.move b2 (Some X) (Pos.h 0, Pos.v 3) in
  Alcotest.test_case "move" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_can_play1 =
  let open Verif in
  let result = can_play b O in
  Alcotest.test_case "can_play" `Quick (fun () ->
      Alcotest.(check bool) "same result" true result)

let test_can_play2 =
  let open Verif in
  let result = can_play bX O in
  Alcotest.test_case "can_play" `Quick (fun () ->
      Alcotest.(check bool) "same result" false result)

let test_can_play3 =
  let open Verif in
  let result = can_play bNSO O in
  Alcotest.test_case "can_play" `Quick (fun () ->
      Alcotest.(check bool) "same result" true result)

let () =
  let open Alcotest in
  run "Engine"
    [
      ( "pp",
        [
          test_pp_player1;
          test_pp_player2;
          test_pp_hpos;
          test_pp_vpos;
          test_pp_board;
          test_pp_pos;
          test_pp_poslist;
        ] );
      ("get, set & free_pos", [ test_set; test_get; test_free_pos ]);
      ("can_play", [ test_can_play1; test_can_play2; test_can_play3 ]);
      ("win", [ test_win_1; test_win_2; test_win_3 ]);
      ( "move",
        [
          test_move1; test_move2; test_move3; test_move4; test_move5; test_move6;
        ] );
      ("new_board", [ test_new_board ]);
      ("equal", [ test_equal_hpos; test_equal_vpos; test_equal_pos ]);
    ]
