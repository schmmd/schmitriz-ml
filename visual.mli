val draw_matrix : 
    ('a -> int * int -> unit) -> 'a option array array -> unit
val draw_shape :
    (Shape.shape -> int * int -> unit) -> Shape.shape -> int * int -> unit
val draw_score : int * int * int * int -> unit

val inv_color : Graphics.color -> Graphics.color
