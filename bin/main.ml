open Omizer2mizer.Arena
open Omizer2mizer.Engine

let () =
  Format.open_vbox 0;
  game player_teletype player_teletype new_board;
  Format.close_box ();
  Format.printf "@."
