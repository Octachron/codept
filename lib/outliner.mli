(** Monotonic outliner for m2l ast *)


(** resulting signature *)


(** Create an outliner adapted for the environment type *)
module Make :
  functor (Envt : Stage.envt) (Param : Stage.param)
    -> Stage.outliner with type envt := Envt.t
