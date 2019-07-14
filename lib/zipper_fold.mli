module Zpath(X:Zipper_def.fold): Zipper_def.s with module T = X

module Make(X:Zipper_def.fold)(Env:Stage.envt):
  Stage.generic_outliner with
  type envt := Env.t and type final := X.m2l
  and type 'a with_param := 'a Stage.param
