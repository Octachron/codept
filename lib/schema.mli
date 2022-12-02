open Schematic

module Lbl: sig
  type deps
  type m2l
  type namespace
end

val namespace: (Lbl.namespace, Module.Namespace.t) Ext.t

val m2l: (Lbl.m2l, M2l.t) Ext.t

type p = Namespaced.t
type local_association = { path: p; ml: string option; mli: string option }
type library_module = { path:p; lib:p}

type unit = { file:string; deps: p list }
type deps = {
  dependencies: unit list;
  local: local_association list;
  library: library_module list;
  unknown: p list;
}

val x: (Lbl.deps, deps) Ext.t
val schema: (Lbl.deps, deps) Ext.t
