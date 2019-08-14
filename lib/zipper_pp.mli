
type dp = Format.formatter -> unit
type 'a dprinter = 'a -> dp


module type Result_printer = sig
  module T: Zipper_def.tree

  val pp_opens: T.opens -> dp -> dp
  val pp_bindrec: T.bind_rec dprinter
  val pp_me: T.module_expr dprinter
  val pp_mt: T.module_type dprinter
  val pp_m2l: T.m2l dprinter
  val pp_packed: T.packed dprinter
  val pp_values: T.values dprinter
  val pp_access: T.access dprinter
  val pp_path: T.path dprinter
  val pp_path_expr_args: T.path_expr_args dprinter

end

module Make(Def:Zipper_def.s)(R:Result_printer with module T := Def.T): sig
  val pp: Zipper_skeleton.path_in_context Def.zipper Pp.t
end

module Opaque(Def:Zipper_def.s) : Result_printer with module T := Def.T
