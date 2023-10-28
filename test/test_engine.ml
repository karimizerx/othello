open Omizer2mizer.Engine

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
      ("get & set", [ test_set; test_get1; test_get2 ]);
    ]
