
type dp = Format.formatter -> unit
type 'a dprinter = 'a -> dp


module type Result_printer = sig
  module T: Zipper_def.tree

  val pp_opens: T.opens -> dp -> dp
  val pp_bindrec: T.bind_rec dprinter
  val pp_me: T.module_expr dprinter
  val pp_mt: T.module_type dprinter
  val pp_with_constraints: T.with_constraints dprinter
  val pp_m2l: T.m2l dprinter
  val pp_minor: T.minor dprinter
  val pp_minors: T.minors dprinter
  val pp_access: T.access dprinter
  val pp_path: T.path dprinter
  val pp_path_expr: T.path_expr dprinter

end

module Make(Def:Zipper_def.s)(R:Result_printer with module T := Def.T): sig
  val pp: Zipper_skeleton.path_in_context Def.zipper Pp.t
end

module Opaque(Def:Zipper_def.s) : Result_printer with module T := Def.T
