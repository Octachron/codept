type t = {
  modules: Module.t Module.M.t;
  types: Module.t Module.M.t;
  unresolved: Unresolved.focus;
  signature: Module.explicit_signature }

let get kind env = match kind with
  | Epath.Module -> env.modules
  | Epath.Module_type -> env.types

let update kind f env = match kind with
  | Epath.Module -> {env with modules = f env.modules }
  | Epath.Module_type ->  {env with types = f env.types }

let qualify k path = k, path
let loc k path = Unresolved.Loc (qualify k path, Epath.Set.empty )
let extern u = Unresolved.Extern u


let find (q,path) env = Module.find path @@ get q env
let empty = {
  modules = Module.M.empty;
  types = Module.M.empty;
  unresolved = Unresolved.start; signature = Module.empty_sig }


let unresolved env = env.unresolved
let umap f env = { env with unresolved = f env.unresolved }
let up = umap Unresolved.up

let refocus env env'  =
  let env' = umap (Unresolved.refocus_on @@ unresolved env) env' in
  { env' with modules = env.modules; types = env.types }
