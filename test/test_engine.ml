open Omizer2mizer.Engine

let test_pp_player =
  let result = Format.asprintf "%a" pp_player (O : player) in
  Alcotest.test_case "pp_player" `Quick (fun () ->
      Alcotest.(check string) "same result" "O" result)

let test_pp_hpos =
  let result = Format.asprintf "%a" pp_hpos (Pos.h 7) in
  Alcotest.test_case "pp_hpos" `Quick (fun () ->
      Alcotest.(check string) "same result" "Pos.(h 7)" result)

let test_pp_vpos =
  let result = Format.asprintf "%a" pp_vpos (Pos.v 7) in
  Alcotest.test_case "pp_vpos" `Quick (fun () ->
      Alcotest.(check string) "same result" "Pos.(v 7)" result)

let test_pp_board =
  let result =
    Format.asprintf "%a" pp_board
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
  in
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

let () =
  let open Alcotest in
  run "Engine"
    [ ("pp", [ test_pp_player; test_pp_hpos; test_pp_vpos; test_pp_board ]) ]
