open Omizer2mizer.Engine
open Omizer2mizer.Arena
open Utils

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

let test_player_random =
  let result = player_random X res_new_board in
  match result with
  (*if it return a pos, we check if it's in the legal positions*)
  | Some position ->
      Alcotest.test_case "player_random" `Quick (fun () ->
          Alcotest.(check bool)
            "same result"
            (List.exists
               (fun p -> if equal_pos p position then true else false)
               [
                 (Pos.h 2, Pos.v 3);
                 (Pos.h 3, Pos.v 2);
                 (Pos.h 5, Pos.v 4);
                 (Pos.h 4, Pos.v 5);
               ])
            true)
      (*Error if it returns None*)
  | None ->
      Alcotest.test_case "player_random" `Quick (fun () ->
          Alcotest.(check bool) "same result" true false)

let test_player_random2 =
  let result = player_random O res_new_board in
  match result with
  (*if it return a pos, we check if it's in the legal positions*)
  | Some position ->
      Alcotest.test_case "player_random" `Quick (fun () ->
          Alcotest.(check bool)
            "same result"
            (List.exists
               (fun p -> if equal_pos p position then true else false)
               [
                 (Pos.h 5, Pos.v 3);
                 (Pos.h 3, Pos.v 5);
                 (Pos.h 2, Pos.v 4);
                 (Pos.h 4, Pos.v 2);
               ])
            true)
      (*Error if it returns None*)
  | None ->
      Alcotest.test_case "player_random" `Quick (fun () ->
          Alcotest.(check bool) "same result" true false)

let test_player_random3 =
  let r = player_random O bNSO in
  let result = pp_pos_opt r in
  Alcotest.test_case "player_random" `Quick (fun () ->
      Alcotest.(check string) "same result" "" result)

let test_player_random4 =
  let r = player_random O only_x in
  let result = pp_pos_opt r in
  Alcotest.test_case "player_random" `Quick (fun () ->
      Alcotest.(check string) "same result" "" result)

let test_player_random5 =
  let r = player_random X beq in
  let result = pp_pos_opt r in
  Alcotest.test_case "player_random" `Quick (fun () ->
      Alcotest.(check string) "same result" "" result)

let test_player_giveup =
  let r = player_random X beq in
  let result = pp_pos_opt r in
  Alcotest.test_case "player_giveup" `Quick (fun () ->
      Alcotest.(check string) "same result" "" result)

let () =
  let open Alcotest in
  run "Arena"
    [
      ("pp", [ test_pp_trace; test_pp_endplay ]);
      ("check pos", [ test_check_pos_1; test_check_pos_2; test_check_pos_3 ]);
      ( "player",
        [
          test_player_random;
          test_player_random2;
          test_player_random3;
          test_player_random4;
          test_player_random5;
          test_player_giveup;
        ] );
    ]
