
type param = {
  policy: Fault.Policy.t;
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

val gen_include: Fault.Policy.t -> Fault.loc ->
  Module.level -> Module.Partial.t -> Summary.t

val open_: Fault.Policy.t -> Fault.loc ->
  Module.Partial.t -> Summary.t


val open_diverge: Fault.Policy.t -> Fault.loc ->
  answer -> Summary.t


val apply_arg: Fault.Policy.t -> Fault.loc ->
  f:Module.Partial.t -> arg:Module.Partial.t
  -> Module.Partial.t


val with_deletions: Paths.S.set -> Module.signature -> Module.signature

val bind_summary: Module.level -> Name.t -> Module.Partial.t -> Summary.t
