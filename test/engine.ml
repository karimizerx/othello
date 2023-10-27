let t =
  Alcotest.test_case "test-template" `Quick (fun () ->
      Alcotest.(check string) "same result" "Bonjour" ("Bon" ^ "jour"))

let () =
  let open Alcotest in
  run "Template" [ ("test", [ t ]) ]
