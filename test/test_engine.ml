open Omizer2mizer.Engine

let test_pp_player =
  let result = Format.asprintf "%a" pp_player (O : player) in
  Alcotest.test_case "pp_player" `Quick (fun () ->
      Alcotest.(check string) "same result" "O" result)

let test_pp_xpos =
  let result = Format.asprintf "%a" pp_xpos (Pos.x 7) in
  Alcotest.test_case "pp_xpos" `Quick (fun () ->
      Alcotest.(check string) "same result" "Pos.(x 7)" result)

let test_pp_ypos =
  let result = Format.asprintf "%a" pp_ypos (Pos.y 7) in
  Alcotest.test_case "pp_ypos" `Quick (fun () ->
      Alcotest.(check string) "same result" "Pos.(y 7)" result)

let () =
  let open Alcotest in
  run "Engine" [ ("pp", [ test_pp_player; test_pp_xpos; test_pp_ypos ]) ]
