open Omizer2mizer.Engine
open Utils

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
  let result = Format.asprintf "@[<h>%a@]" pp_board b1 in
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
      Alcotest.(check board) "same result" res_new_board res_new_board)

let test_equal_hpos =
  Alcotest.test_case "equal_hpos" `Quick (fun () ->
      Alcotest.(check bool) "same result" (equal_hpos (H 2) (H 2)) true)

let test_equal_hpos_gen =
  let open QCheck in 
  Test.make ~count:1000 ~name:"equal hpos multiple runs"
  (pair small_int small_int) (fun (h1, h2) ->
    (equal_hpos (H h1) (H h2)) = (h1 = h2) 
  )

let test_equal_vpos =
  Alcotest.test_case "equal_vpos" `Quick (fun () ->
      Alcotest.(check bool) "same result" (equal_vpos (V 2) (V 8)) false)

let test_equal_vpos_gen =
  let open QCheck in 
  Test.make ~count:1000 ~name:"equal vpos multiple runs"
  (pair small_int small_int) (fun (v1, v2) ->
    (equal_vpos (V v1) (V v2)) = (v1 = v2) 
  )

let test_equal_pos =
  Alcotest.test_case "equal_pos" `Quick (fun () ->
      Alcotest.(check bool) "same result" (equal_pos (H 1, V 2) (H 1, V 2)) true)

let test_equal_pos_gen =
  let open QCheck in 
  Test.make ~count:1000 ~name:"equal pos multiple runs"
  (quad small_int small_int small_int small_int) (fun (h1, v1, h2, v2) ->
    (equal_pos (H h1, V v1) (H h2, V v2)) = (h1 = h2 && v1 = v2) 
  )
let test_set =
  let set1 = set b1 (O : player) [ (Pos.h 0, Pos.v 0) ] in
  let result = set set1 (O : player) [ (Pos.h 7, Pos.v 7) ] in
  let desired = b3 in
  Alcotest.test_case "set" `Quick (fun () ->
      Alcotest.(check board) "same result" desired result)

let test_get =
  let result =
    [
      get b1 (Pos.h 0, Pos.v 0);
      get b1 (Pos.h 3, Pos.v 1);
      get b1 (Pos.h 6, Pos.v 7);
    ]
  in
  let desired = [ Some X; Some O; None ] in
  Alcotest.test_case "get" `Quick (fun () ->
      Alcotest.(check (list player)) "same result" desired result)

let test_free_pos =
  let result = Verif.free_pos bX in
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
  let result = Verif.win only_x X in
  let desired = true in
  Alcotest.test_case "x win" `Quick (fun () ->
      Alcotest.(check bool) "same result" desired result)

let test_win_2 =
  let result = Verif.(win beq X, win beq O) in
  let desired = (true, true) in
  Alcotest.test_case "equality" `Quick (fun () ->
      Alcotest.(check (pair bool bool)) "same result" desired result)

let test_win_3 =
  let result = Verif.win b1 X && Verif.win b1 O in
  let desired = false in
  Alcotest.test_case "winner uniqueness (when not draw)" `Quick (fun () ->
      Alcotest.(check bool) "same result" desired result)

let test_win_nonfill_board =
  let result = Verif.win end_match X in
  let desired = true in
  Alcotest.test_case "players out of moves" `Quick (fun () ->
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
  let result = Verif.move b1 (Some X) (Pos.h 1, Pos.v 1) in
  Alcotest.test_case "move" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_move2 =
  let desired =
    [ (Pos.h 0, Pos.v 7); (Pos.h 1, Pos.v 6); (Pos.h 2, Pos.v 5) ]
  in
  let result = Verif.move b1 (Some X) (Pos.h 0, Pos.v 7) in
  Alcotest.test_case "move" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_move3 =
  let desired =
    [ (Pos.h 7, Pos.v 7); (Pos.h 7, Pos.v 6); (Pos.h 7, Pos.v 5) ]
  in
  let result = Verif.move b1 (Some X) (Pos.h 7, Pos.v 7) in
  Alcotest.test_case "move" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_move4 =
  let desired = [ (Pos.h 5, Pos.v 6) ] in
  let result = Verif.move b1 (Some O) (Pos.h 5, Pos.v 6) in
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

let test_move7 =
  let desired =
    [ (Pos.h 0, Pos.v 3); (Pos.h 0, Pos.v 2); (Pos.h 1, Pos.v 2) ]
  in
  let result = Verif.move b0 (Some X) (Pos.h 0, Pos.v 3) in
  Alcotest.test_case "move" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_possible_move_list =
  let open Verif in
  let result = possible_move_list X res_new_board in
  let desired =
    [
      (Pos.h 2, Pos.v 3);
      (Pos.h 3, Pos.v 2);
      (Pos.h 4, Pos.v 5);
      (Pos.h 5, Pos.v 4);
    ]
  in
  Alcotest.test_case "possible_move_list" `Quick (fun () ->
      Alcotest.(check (list (pair hpos vpos))) "same result" desired result)

let test_can_play1 =
  let open Verif in
  let result = can_play b1 O in
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
      Alcotest.(check bool) "same result" false result)

let test_out_of_bounds_move =
  let open Verif in
  Alcotest.test_case "wrong board" `Quick (fun () -> 
    Alcotest.(check (list (pair hpos vpos))) "same result" 
    [] (possible_move_list X wrong_size_board))

let test_out_of_bounds_valid_move =
  let open Verif in
  Alcotest.test_case "wrong board" `Quick (fun () -> 
    Alcotest.(check (list (pair hpos vpos))) "same result" 
    [] (move wrong_size_board (Some X) (H 6, V 7)))
    

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
      ("possible_move_list", [ test_possible_move_list; test_out_of_bounds_move]);
      ("can_play", [ test_can_play1; test_can_play2; test_can_play3; ]);
      ("win", [ test_win_1; test_win_2; test_win_3; test_win_nonfill_board ]);
      ( "move",
        [
          test_move1;
          test_move2;
          test_move3;
          test_move4;
          test_move5;
          test_move6;
          test_move7;
          test_out_of_bounds_valid_move; 
        ] );
      ("new_board", [ test_new_board ]);
      ("equal", [ test_equal_hpos; 
                  QCheck_alcotest.to_alcotest test_equal_hpos_gen;
                  test_equal_vpos; 
                  QCheck_alcotest.to_alcotest test_equal_vpos_gen;                
                  test_equal_pos;
                  QCheck_alcotest.to_alcotest test_equal_pos_gen; ]);
    ]
