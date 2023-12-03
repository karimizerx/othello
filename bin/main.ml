open Omizer2mizer.Arena

let () =
  Random.self_init ();
  Format.open_vbox 0;
  game player_teletype player_teletype;
  Format.close_box ();
  Format.printf "@."
