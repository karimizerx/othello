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

(* Case :  No solution for player [O] *)
let bNSO =
  [
    [ Some X; Some X; None; None; Some X; Some X; None; None ];
    [ Some X; Some O; None; None; Some X; Some X; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
  ]

let beq =
  [
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
    [ Some X; Some X; Some X; Some X; Some O; Some O; Some O; Some O ];
  ]

let only_x =
  [
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
    [ Some X; Some X; Some X; Some X; Some X; Some X; Some X; Some X ];
  ]

(* Pretty printer for (pos option)'s tests. *)
let pp_pos_opt po =
  match po with None -> "" | Some p -> Format.asprintf "%a" pp_pos p

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

let generator_move (possible_moves : pos list) =
  let open QCheck in
  Gen.oneof (List.map (fun x -> Gen.return x) possible_moves)

let generator_board : board QCheck.Gen.t =
 fun st ->
  let open Verif in
  let function_player p b =
    let moves = possible_move_list p b in
    match moves with [] -> None | _ -> Some (generator_move moves st)
  in
  let rec go board player function_player1 function_player2 (trace : trace) =
    if (not (can_play board X)) && not (can_play board O) then board
    else
      let current_player, current_function =
        if not (can_play board (swap_player player)) then
          (player, player_function player function_player1 function_player2)
        else
          ( swap_player player,
            player_function (swap_player player) function_player1
              function_player2 )
      in
      let new_board, new_trace =
        play current_player board current_function trace
      in
      if equal_board board new_board then board
      else
        go new_board current_player function_player1 function_player2 new_trace
  in
  go new_board O function_player function_player []

let arbitrary_board =
  QCheck.make ~print:(Format.asprintf "%a" pp_board) generator_board

let test_board =
  let open QCheck in
  Test.make ~count:100 ~name:"p1 and p2 can't play" arbitrary_board (fun b ->
      Verif.((not (can_play b X)) && not (can_play b O)))

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
      ( "game (QCheck #startupnation)",
        [ QCheck_alcotest.to_alcotest test_board ] );
    ]
