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

let test_new_board =
  Alcotest.test_case "new_board" `Quick (fun () ->
      Alcotest.(check (option list list))
        "same result" res_new_board new_board)

let () =
  let open Alcotest in
  run "Engine" [ ("pp", [ test_pp_player; test_pp_hpos; test_pp_vpos; test_new_board ]) ]
