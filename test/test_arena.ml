open Omizer2mizer.Engine
open Omizer2mizer.Arena

(*let trace = Alcotest.testable pp_trace equal_trace
  let endplay = Alcotest.testable pp_endplay equal_endplay*)

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

let () =
  let open Alcotest in
  run "Arena" [ ("pp", [ test_pp_trace; test_pp_endplay ]) ]
