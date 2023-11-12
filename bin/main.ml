open Omizer2mizer.Arena
open Omizer2mizer.Engine

let () =
  Random.self_init ();
  Format.open_vbox 0;
  game player_random player_random new_board;
  Format.close_box ();
  Format.printf "@."
