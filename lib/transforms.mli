
type param = {
  fault_handler: Fault.handler;
  epsilon_dependencies: bool;
  transparent_extension_nodes: bool;
  transparent_aliases: bool
}


type answer_type =
  | Namespace of Module.dict
  | Mty of Module.sty

type answer = { name: Name.t; kind: answer_type }
val pp_answer: answer Pp.t

type 'a query_result =
  { main:'a; deps: Deps.t; msgs: Fault.t list }

val gen_include: Fault.handler -> Uloc.t ->
  Id.seed -> Module.level -> Module.Partial.t -> Summary.t

val open_: Fault.handler -> Uloc.t ->
  Module.Partial.t -> Summary.t


val open_diverge: Fault.handler -> Uloc.t ->
  answer -> Summary.t


val apply_arg: Fault.handler -> Uloc.t ->
  f:Module.Partial.t -> arg:Module.Partial.t
  -> Module.Partial.t


val with_deletions: Paths.S.set -> Module.signature -> Module.signature

val bind_summary: Module.level -> Name.t -> Module.Partial.t -> Summary.t
