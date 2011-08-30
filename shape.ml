open Defs

type shape = 
    Square of int 
  | Long of int
  | BackwardL of int
  | L of int
  | T of int
  | S of int
  | Z of int

let random_shape () =
  match (Random.int 7) with
    0 -> Square 0
  | 1 -> Long 0
  | 2 -> BackwardL 0
  | 3 -> L 0
  | 4 -> T 0
  | 5 -> S 0
  | 6 -> Z 0
  | _ -> raise Error

let get_shape_data_raw p = 
  match p with
      Square rot -> 
        [| ([| [| true; true |];
               [| true; true |] |], (0, 0, 0, 0)) |]
    | Long rot -> 
        [| ([| [| false; true; false; false |];
               [| false; true; false; false |];
               [| false; true; false; false |];
               [| false; true; false; false |] |], (0, 1, 0, 2));

           ([| [| false; false; false; false |];
               [| false; false; false; false |];
               [| true;  true;  true;  true  |];
               [| false; false; false; false |] |], (2, 0, 1, 0));

           ([| [| false; false; true;  false |];
               [| false; false; true;  false |];
               [| false; false; true;  false |];
               [| false; false; true;  false |] |], (0, 2, 0, 1));

           ([| [| false; false; false; false |];
               [| true;  true;  true;  true  |];
               [| false; false; false; false |];
               [| false; false; false; false |] |], (1, 0, 2, 0)) |]
    | BackwardL rot ->
        [| ([| [| false; true;  false |];
               [| false; true;  false |];
               [| true;  true;  false |] |], (0, 0, 0, 1));

           ([| [| false; false; false |];
               [| true;  true;  true  |];
               [| false; false; true  |] |], (1, 0, 0, 0));

           ([| [| false; true;  true  |];
               [| false; true;  false |];
               [| false; true;  false |] |], (0, 1, 0, 0));

           ([| [| true;  false; false |];
               [| true;  true;  true  |];
               [| false; false; false |] |], (0, 0, 1, 0));

           ([| [| false; true; false |];
               [| false; true; false |];
               [| true;  true; false |] |], (0, 0, 0, 1)) |]
    | L rot ->
        [| ([| [| false; true;  false |];
               [| false; true;  false |];
               [| false; true;  true  |] |], (0, 1, 0, 0));

           ([| [| false; false; true  |];
               [| true;  true;  true  |];
               [| false; false; false |] |], (0, 0, 1, 0));

           ([| [| true;  true;  false |];
               [| false; true;  false |];
               [| false; true;  false |] |], (0, 0, 0, 1));

           ([| [| false; false; false |];
               [| true;  true;  true  |];
               [| true;  false; false |] |], (1, 0, 0, 0)) |]
    | T rot ->
        [| ([| [| false; true;  false |];
               [| true;  true;  true  |];
               [| false; false; false |] |], (0, 0, 1, 0));

           ([| [| false; true;  false |];
               [| true;  true;  false |];
               [| false; true;  false |] |], (0, 0, 0, 1));

           ([| [| false; false; false |];
               [| true;  true;  true  |];
               [| false; true;  false |] |], (1, 0, 0, 0));

           ([| [| false; true;  false |];
               [| false; true;  true  |];
               [| false; true;  false |] |], (0, 1, 0, 0)) |]
    | S rot -> 
        [| ([| [| false; true;  true  |];
               [| true;  true;  false |];
               [| false; false; false |] |], (0, 0, 1, 0));

           ([| [| true;  false; false |];
               [| true;  true;  false |];
               [| false; true;  false |] |], (0, 0, 0, 1));

           ([| [| false; false; false |];
               [| false; true;  true  |];
               [| true;  true;  false |] |], (1, 0, 0, 0));

           ([| [| false; true;  false |];
               [| false; true;  true  |];
               [| false; false; true  |] |], (0, 1, 0, 0)) |]
    | Z rot -> 
        [| ([| [| true;  true;  false |];
               [| false; true;  true  |];
               [| false; false; false |] |], (0, 0, 1, 0));

           ([| [| false; true;  false |];
               [| true;  true;  false |];
               [| true;  false; false |] |], (0, 0, 0, 1));

           ([| [| false; false; false |];
               [| true;  true;  false |];
               [| false; true;  true  |] |], (1, 0, 0, 0));

           ([| [| false; false; true  |];
               [| false; true;  true  |];
               [| false; true;  false |] |], (0, 1, 0, 0)) |]

let get_shape_data p =
  match p with
    Square rot -> (get_shape_data_raw p).(rot)
  | Long rot -> (get_shape_data_raw p).(rot)
  | BackwardL rot -> (get_shape_data_raw p).(rot)
  | L rot -> (get_shape_data_raw p).(rot)
  | T rot -> (get_shape_data_raw p).(rot)
  | S rot -> (get_shape_data_raw p).(rot)
  | Z rot -> (get_shape_data_raw p).(rot)

let get_abstract_shape p = 
  fst (get_shape_data p)

let shape_rect p (x, y) =
  let (sh, (t, l, b, r)) = get_shape_data p in
  let width  = Array.length sh.(0) in
  let height = Array.length sh in
  ((x + l, y + b), (x + width - r, y + height - t))

let start_shape_point p =
  let (abs, (_, _, shape_bottom, _)) = get_shape_data p in
  let shape_width = (Array.length (abs.(0))) in
  ((grid_width - shape_width) / 2, grid_height - shape_bottom - 1)

let rotate p =
  let rotations = Array.length (get_shape_data_raw p) in
  match p with
      Square rot  -> Square ((rot + 1)mod rotations)
    | Long rot    -> Long ((rot + 1) mod rotations)
    | BackwardL rot -> BackwardL ((rot + 1) mod rotations)
    | L rot -> L ((rot + 1) mod rotations)
    | T rot -> T ((rot + 1) mod rotations)
    | S rot -> S ((rot + 1) mod rotations)
    | Z rot -> Z ((rot + 1) mod rotations)

(* tests if piece p is too far left, right, or down *)
let out_of_bounds p (x, y) =
  let (x1, y1), (x2, y2) = shape_rect p (x, y) in
  if (x1 < 0) || (y1 < 0) || (x2 > grid_width)
  then true
  else false

let above_bounds p (x, y) =
  let (x1, y1), (x2, y2) = shape_rect p (x, y) in
  if (y2 >= grid_height)
  then true
  else false

let petrify matrix p (x, y) =
  try 
    let abs = get_abstract_shape p in
    let y = y + (Array.length abs) - 1 in
    for j = 0 to (Array.length abs) - 1 do
      for i = 0 to (Array.length abs) - 1 do
        if abs.(j).(i) = true then (matrix.(y - j).(x + i) <- Some p)
      done
    done; true
  with
    Invalid_argument s -> false

let is_collision matrix p (x, y) =
  let abs = get_abstract_shape p in
  let y = y + (Array.length abs) - 1 in
  let ret = ref false in
  for j = 0 to (Array.length abs) - 1 do
    for i = 0 to (Array.length abs) - 1 do
      if abs.(j).(i) = true 
      && y - j < grid_height (* make sure the y is not above the matrix *)
      && matrix.(y - j).(x + i) != None
      then ret := true
    done
  done;
  !ret

let draw_filled_square p (x, y) =
  let c = match p with
      Square a -> Graphics.blue
    | Long a -> Graphics.black
    | BackwardL a -> Graphics.magenta
    | L a -> Graphics.cyan
    | T a -> Graphics.red
    | S a -> Graphics.yellow
    | Z a -> Graphics.green
  in
  Graphics.set_color c;
  Graphics.fill_rect x y square_width square_height

let draw_filled_outlined_square p (x, y) =
  let _ = draw_filled_square p (x, y) in
  let _ = Graphics.set_color Graphics.white in
  Graphics.draw_rect x y square_width square_height
