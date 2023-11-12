open Omizer2mizer.Arena
open Omizer2mizer.Engine

let () =
  Format.open_vbox 0;
  let _ = game player_teletype player_teletype new_board in

  Format.close_box ();
  Format.printf "@."
