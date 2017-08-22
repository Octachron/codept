open Schematic

module Lbl: sig
  type deps
  type m2l
  type sig'
end

val sign: (Lbl.sig', Module.t list) Ext.t
val m2l: (Lbl.m2l, M2l.t) Ext.t

type unit_association =
  { module_path: Name.t list ; ml: string option; mli: string option }
type p = Paths.S.t
type local = { path: p; file:p}
type lib = { path: p; lib: p; file:p}
type unknown = p
type dep = { local: local list; lib: lib list; unknown: unknown list }
type item = { file: string; dependencies: dep }
type deps = { dependencies: item list; atlas: unit_association list }

val x: (Lbl.deps, deps) Ext.t
val schema: (Lbl.deps, deps) Ext.t
