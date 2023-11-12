open Omizer2mizer.Engine
open Omizer2mizer.Arena

(*let trace = Alcotest.testable pp_trace equal_trace
  let endplay = Alcotest.testable pp_endplay equal_endplay*)

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

let trace = [ (H 3, V 2); (H 5, V 6); (H 2, V 3) ]
let endp = Win X

let test_pp_trace =
  let result = Format.asprintf "%a" pp_trace trace in
  let desired = "D2 F6 C3 " in
  Alcotest.test_case "pp_trace" `Quick (fun () ->
      Alcotest.(check string) "same result" desired result)

let test_pp_endplay =
  let result = Format.asprintf "%a" pp_endplay endp in
  let desired = "X won the game" in
  Alcotest.test_case "pp_endplay" `Quick (fun () ->
      Alcotest.(check string) "same result" desired result)

let test_check_pos_1 =
  let result = check_pos res_new_board (H (-1), V 0) in
  Alcotest.test_case "check_pos_1" `Quick (fun () ->
      Alcotest.(check bool) "same result" false result)

let test_check_pos_2 =
  let result = check_pos res_new_board (H 2, V 0) in
  Alcotest.test_case "check_pos_2" `Quick (fun () ->
      Alcotest.(check bool) "same result" true result)

let test_check_pos_3 =
  let result = check_pos res_new_board (H 3, V 3) in
  Alcotest.test_case "check_pos_3" `Quick (fun () ->
      Alcotest.(check bool) "same result" false result)

let () =
  let open Alcotest in
  run "Arena"
    [
      ("pp", [ test_pp_trace; test_pp_endplay ]);
      ("check pos", [ test_check_pos_1; test_check_pos_2; test_check_pos_3 ]);
    ]
