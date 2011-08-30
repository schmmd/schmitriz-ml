open Defs
open Shape
open Visual

type signal = Abort | Restart

type gamestate = {
    mutable p : shape; 
    mutable coord : int * int; 
    mutable high  : int;
    mutable score : int;
    mutable lines : int;
    mutable level : int;
    mutable grav_chan : signal Event.channel;
    mutable mutex : Mutex.t;
    mutable drawsquare : shape -> int * int -> unit;
    matrix : shape option array array
  }


let state : gamestate option ref = ref None

let redraw game =
  Graphics.clear_graph ();
  draw_shape game.drawsquare game.p game.coord;
  draw_matrix game.drawsquare game.matrix;
  draw_score (game.level, game.lines, game.score, game.high);
  Graphics.synchronize ()

let end_game game =   
  let _ = state := None in   
  let _ = (if game.high < game.score then 
    let _ = game.high <- game.score in
    let _ = redraw game in
    let file = Unix.openfile "highscore.txt" 
                  [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
                  0o644 in
    let str = string_of_int game.high in
    let _ = Unix.write file str 0 (String.length str) in
    Unix.close file) in
  Event.sync (Event.send game.grav_chan Abort)

let left  (x, y) = (x - 1, y)
let right (x, y) = (x + 1, y)
let up    (x, y) = (x, y + 1)
let down  (x, y) = (x, y - 1)

(* moves a shape along the x-axis by vx.  Checks if the
 * piece fits at the new coord and moves if it does *)
let move_shape_laterally game vx =
  let to_coord = (vx + fst game.coord, snd game.coord) in
  if (not (out_of_bounds game.p to_coord))
  && (not (is_collision game.matrix game.p to_coord))
  then game.coord <- to_coord
let move_shape_left  game = move_shape_laterally game (-1)
let move_shape_right game = move_shape_laterally game  1

let rotate_shape game =
  let to_shape = rotate game.p in
  if (not (out_of_bounds to_shape game.coord))
  && (not (is_collision game.matrix to_shape game.coord))
  then game.p <- rotate game.p

let clear_rows game =
  (* returns a list containing all full rows *)
  let rec find_full_rows j rows =
    if j < Array.length game.matrix - 1
    then ( if Array.fold_right 
                (fun x b -> b && (x != None)) game.matrix.(j) true
           then find_full_rows (j + 1) (j :: rows) 
           else find_full_rows (j + 1) rows )
    else rows
  in
  let remove_row j =
    let height = Array.length game.matrix in (* calculate the array height *)
    let width  = Array.length game.matrix.(0) in (* calculate the array width *)
    (* blit the rows down over the row to remove *)
    let _ = Array.blit game.matrix (j + 1)  (* from *)
                       game.matrix (j)      (*  to  *)
                       (height - j - 1) in  (* length *)
    (* keep count of how many lines are cleared *)
    let _ = game.lines <- game.lines + 1 in
    (* put an empty row at the top *)
    game.matrix.(height - 1) <- Array.create width None
  in
  (*
  let invert_rows image rows =
    let inv_vec = Array.map (fun c -> inv_color c) in
    let matrix = Graphics.dump_image image in
    Graphics.make_image
         (Array.mapi 
         (fun i x -> if (List.mem i rows) then inv_vec x else x) 
            matrix)
  in
  *)
  let rows = find_full_rows 0 [] in
  List.iter (fun j -> remove_row j) rows

let restart_gravity game =
  Event.sync (Event.send game.grav_chan Restart)

(* post: returns true if the piece is dropped.
 * returns false if the piece cannot be dropped any more. *)
let rec drop_shape game =
  let points () = 
    5 +             (* for the piece *)
    2 * game.level  (* for the level *)
    (* the drop height points are taken care of in freefall_shape *)
  in
  let to_coord = down game.coord in
  let land_shape () = (* handle the case when the shape hits the bottom *)
    let _ = petrify game.matrix game.p game.coord in
    let _ = game.score <- game.score + points () in  (* add to score *)
    let _ = game.level <- game.lines / 20 in       (* adjust level *)
    let _ = clear_rows game in 
    if above_bounds game.p to_coord (* if the piece is above the grid *)
    then (* then end the game *)
      end_game game
    else                            (* otherwise set it and make a new shape *)
      let _ = game.p <- random_shape () in
      let _ = restart_gravity game in
      game.coord <- start_shape_point game.p;
  in
  let _ = Mutex.lock game.mutex in (* lock the mutex so grav/main seperate *)
  let return = (* save the return so we can unlock at the end *)
    if (out_of_bounds game.p to_coord)
      || (is_collision game.matrix game.p to_coord)
    then let _ = land_shape ()            in false
    else let _ = game.coord <- (to_coord) in true in
  let _ = Mutex.unlock game.mutex in (* unlock the mutex *)
  return                             (* return the boolean value *)

(* The loop run in the gravity thread. *)
and grav_handler (g, chin) =
  let rec abort ch =
    Event.sync (Event.send ch Abort)
  in
  let rec grav chin =
    match !g with
      Some game ->
        (* calculate the delay time *)
        let delay_time = 
          if game.level < 6 
          then (8.0 -. ((float_of_int game.lines) /. 20.0)) /. 10.0
          else (0.2 -. (((float_of_int (game.lines - 120)) /. 20.0)) /. 40.0)
        in
        (*print_string ((string_of_int game.level) ^ " lines " ^ 
                      (string_of_int game.lines) ^ " with delay " ^
                      (string_of_float delay_time) ^ "\n");
                      flush stdout;*)
        (* delay the thread *)
        let _ = Thread.delay delay_time in
        (* poll channel to see if we should abort thread *)
        let _ = match Event.poll (Event.receive chin) with
             Some Abort -> Thread.exit ()
           | Some Restart -> Thread.exit ()
           | None -> ()
        in
        (* drop the shape and redraw the game *)
        let _ = drop_shape game in
        let _ = redraw game in
        grav chin
    | None -> Thread.exit ()
  in
    let chout = Event.new_channel () in
    let _ = Thread.create grav chout in
    match Event.sync (Event.receive chin) with
    Restart -> let _ = Thread.create abort chout in
               grav_handler (g, chin)
    | Abort -> Thread.create abort chout

let force_drop_shape game =
  let _ = drop_shape game in
  let _ = restart_gravity game in
  ()

let freefall_shape game =
  game.score <- game.score + (snd game.coord); (* add height to score *)
  while (drop_shape game) do () done           (* drop it all the way *)


(*** High Level Stuff ***)

let new_game () = 
  let shape = random_shape () in
  let game = 
    {
      p = shape;
      coord = (start_shape_point shape);
      matrix = Array.make_matrix grid_height grid_width None;
      high = 0; score = 0; level = 0; lines = 0;
      grav_chan = Event.new_channel (); 
      drawsquare = draw_filled_outlined_square;
      mutex = Mutex.create ()
    } in
  let _ = try
    let file = Unix.openfile "highscore.txt" [Unix.O_RDONLY] 0o644 in
    let s = String.make 10 '0' in
    let n = Unix.read file s 0 10 in
    let _ = game.high <- (int_of_string (String.sub s 0 n)) in
    Unix.close file
  with Unix.Unix_error(i, command, name) -> () in
  let _ = state := Some game in
  let _ = Thread.create grav_handler (state, game.grav_chan) in
  redraw game;
