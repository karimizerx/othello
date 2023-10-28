open Omizer2mizer.Engine

(*let player = Alcotest.testable pp_player equal_player
let hpos = Alcotest.testable pp_hpos equal_hpos
let vpos = Alcotest.testable pp_vpos equal_vpos*)
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
      Alcotest.(check string) "same result" "Pos.(h 7)" result)

let test_pp_vpos =
  let result = Format.asprintf "%a" pp_vpos (Pos.v 7) in
  Alcotest.test_case "pp_vpos" `Quick (fun () ->
      Alcotest.(check string) "same result" "Pos.(v 7)" result)

let test_pp_board =
  let result = Format.asprintf "%a" pp_board b in
  let desired =
    "XX••XX••\n\
     O•OXO•OX\n\
     XOO•XOO•\n\
     XOO•XOO•\n\
     XX••XX••\n\
     O•OXO•OX\n\
     XOO•XOO•\n\
     XOO•XOO•\n"
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
  let setter1 = set b (Pos.h 0, Pos.v 0) (O : player) in
  let setter2 = set setter1 (Pos.h 7, Pos.v 7) (O : player) in
  let result = Format.asprintf "%a" pp_board setter2 in
  let desired =
    "OX••XX••\n\
     O•OXO•OX\n\
     XOO•XOO•\n\
     XOO•XOO•\n\
     XX••XX••\n\
     O•OXO•OX\n\
     XOO•XOO•\n\
     XOO•XOOO\n"
  in
  Alcotest.test_case "set" `Quick (fun () ->
      Alcotest.(check string) "same result" desired result)

let test_get1 =
  let getter = get b (Pos.h 0, Pos.v 0) in
  let result = Format.asprintf "%a" pp_player getter in
  Alcotest.test_case "get" `Quick (fun () ->
      Alcotest.(check string) "same result" "X" result)

let test_get2 =
  let getter = get b (Pos.h 7, Pos.v 7) in
  let result = Format.asprintf "%a" pp_player getter in
  Alcotest.test_case "get" `Quick (fun () ->
      Alcotest.(check string) "same result" "•" result)

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
        ] );
      ("new_board", [ test_new_board ]);
      ("equal", [ test_equal_hpos; test_equal_vpos; test_equal_pos ]);
      ("get & set", [ test_set; test_get1; test_get2 ]);
    ]
