type shape

(* creation and modification *)
val random_shape : unit -> shape
val rotate : shape -> shape

(* bounds checking *)
val out_of_bounds : shape -> int * int -> bool
val above_bounds  : shape -> int * int -> bool

(* dimensions of shapes *)
val start_shape_point : shape -> int * int

(* interactions with the matrix *)
val petrify : shape option array array -> shape -> int * int -> bool
val is_collision : 'a option array array -> shape -> int * int -> bool

(* for visual use *)
val get_abstract_shape : shape -> bool array array
val draw_filled_square : shape -> int * int -> unit
val draw_filled_outlined_square : shape -> int * int -> unit
