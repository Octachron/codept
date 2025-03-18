module Outline(Env : Stage.envt):
 Stage.generic_outliner with
  type envt := Env.t and type final := Deps.t
  and type 'a with_param := 'a Stage.param

module Make(Env:Stage.envt)(Param:Stage.param):
  Stage.outliner with type envt := Env.t 
[@@warning "-67"]
