open Omizer2mizer.Engine
open Omizer2mizer.Arena

(*let trace = Alcotest.testable pp_trace equal_trace
  let endplay = Alcotest.testable pp_endplay equal_endplay*)

  let hpos = Alcotest.testable pp_hpos equal_hpos
  let vpos = Alcotest.testable pp_vpos equal_vpos
  let trace = [ (H 3, V 2); (H 5, V 6); (H 2, V 3) ]
  let endp = Win X
  
  let default_board =
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

let test_possible_move_list =
  let result = possible_move_list X default_board in
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



let () =
  let open Alcotest in
  run "Arena" [ ("pp", [ test_pp_trace; test_pp_endplay ]);("possible_move_list", [test_possible_move_list]) ]
