open Defs
open Shape
open Game
open Visual

(* seed the random number generator *)
let _ = Random.self_init ()

(* this is the main function that contains the main loop *)
let main f_init f_end f_key f_mouse f_except = 
  f_init ();
  try
    while true do
      try
        let s = Graphics.wait_next_event
                    [Graphics.Key_pressed; Graphics.Button_down ] in
        if s.Graphics.keypressed then f_key s.Graphics.key
        else if s.Graphics.button 
        then f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
      with 
      (* handle exceptions from the while loop *)
          End -> raise End
        | e   -> f_except e
    done
  with
    (* handle the end exception if it is raised again *)
      End -> f_end ();;

let f_init () = 
  Graphics.open_graph (" " ^ (string_of_int window_width) ^ "x" 
                           ^ (string_of_int window_height));
  Graphics.set_window_title "Schmitriz Linux";
  Graphics.set_color Graphics.blue;
  Graphics.auto_synchronize false;
  new_game ()

let f_end () = ()

exception NoGame
let get_game () = 
  match !state with
    None -> raise End
  | Some game -> game

let f_key key =
  let game = get_game () in
  ( match key with
      'q' -> raise End
    | 'j' -> move_shape_left game
    | 'l' -> move_shape_right game
    | 'd' -> force_drop_shape game
    | 'i' -> clear_rows game
    | ' ' -> freefall_shape game
    | _   -> rotate_shape game );
  redraw game

let f_mouse x y = ()

let f_except e = raise e

let _ = main f_init f_end f_key f_mouse f_except
