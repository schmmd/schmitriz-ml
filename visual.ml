open Defs
open Shape

let abstract_to_real_coord (vx, vy) =
  (vx * square_width, stats_height + vy * square_height)

let draw_square vx vy = 
  let (x, y) = abstract_to_real_coord (vx, vy) in
  Graphics.fill_rect x y square_width square_height

let draw_matrix drawsquare matrix =
  for j = 0 to (Array.length matrix) - 1 do
    for i = 0 to (Array.length matrix.(j) - 1) do
      match matrix.(j).(i) with
        Some p -> drawsquare p (abstract_to_real_coord (i, j))
      | None -> ()
    done
  done

let draw_shape drawsquare p (vx, vy) = 
  let draw_abstract abs x y = 
    (* y must be adjusted for the correct drawing because y must be drawn 
     * from the bottom up for the drawing to match the abstract array
     * representations. *)
    let _ = Graphics.set_color Graphics.blue in
    let y = y + (Array.length abs) - 1 in
    for j = 0 to (Array.length abs) - 1 do
      for i = 0 to (Array.length abs.(j)) - 1 do
        if abs.(j).(i) 
        && y - j < grid_height
        then drawsquare p (abstract_to_real_coord ((x + i), (y - j)))
      done
    done
  in
  draw_abstract (get_abstract_shape p) vx vy

let draw_score (level, lines, score, high) = 
  let border = 8 in
  (* draw the horizontal lines *)
  Graphics.set_color Graphics.black;
  Graphics.moveto 0 stats_height;
  Graphics.lineto window_width stats_height;
  Graphics.moveto 0 (window_height - stats_height);
  Graphics.lineto window_width (window_height - stats_height);
  (* draw the level and lines *)
  let text = "Level " ^ (string_of_int level) in
  Graphics.moveto (window_width - border - 
                      (fst (Graphics.text_size text))) border;
  Graphics.draw_string text;
  let text = "Lines " ^ (string_of_int lines) in
  Graphics.moveto border border;
  Graphics.draw_string text;
  (* draw the score and high *)
  let text = "Score " ^ (string_of_int score) in
  Graphics.moveto border (window_height - border - 
                             (snd (Graphics.text_size text)));
  Graphics.draw_string text;
  let text = "High " ^ (string_of_int high) in
  Graphics.moveto (window_width - border - (fst (Graphics.text_size text)))
                  (window_height - border - (snd (Graphics.text_size text)));
  Graphics.draw_string text;
  ()

let from_rgb (c : Graphics.color) =
  let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256
  in (r, g, b)

let inv_color (c : Graphics.color) = 
  let (r, g, b) = from_rgb c
  in Graphics.rgb (255 - r) (255 - g) (255 - c)

