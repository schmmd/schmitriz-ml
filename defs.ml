(* used for fun *)
exception Error

(* used to abort the program from within *)
exception End

let square_width = 20
let square_height = 20

let grid_width = 10
let grid_height = 20

let stats_height = 30

let window_width = (grid_width * square_width)
let window_height = (grid_height * square_height) + (stats_height * 2)

let lines_per_level = 20
