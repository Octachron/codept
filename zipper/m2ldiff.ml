open M2l
module Arg = Module.Arg

type a = Paths.Simple.t * (Loc.t * Deps.Edge.t)



type env = Summary.t
type level = Module.level = Module | Module_type

type 'state path_in_context =
  { loc: Fault.loc;
    edge:Deps.Edge.t option;
    level: level;
    state: 'state;
    path: Paths.S.t
  }

let default_edge = Option.default Deps.Edge.Normal

type 'p param = 'p
  constraint 'p = <
    path:'path;
    module_expr: 'module_expr;
    access: 'access;
    packed: 'packed;
    module_type:'module_type;
    m2l:'m2l;
    expr:'expr;
    value: 'values;
    annotation:'annotation;
    bind_rec:'bind_rec;
    ext:'ext;
    path_expr:'path_expr;
    path_expr_args:'path_expr_args;
    opens:'opens;
    state:'state;
    state_diff:'state_diff
  >

type ('a,'state) with_state = { r:'a; state:'state}

class virtual ['params] fold = object
  constraint
    'params = <
      path:'path;
      module_expr: 'module_expr;
      access: 'access;
      packed: 'packed;
      module_type:'module_type;
      m2l:'m2l;
      expr:'expr;
      value: 'values;
      annotation:'annotation;
      bind_rec:'bind_rec;
      ext:'ext;
      path_expr:'path_expr;
      path_expr_args:'path_expr_args;
      opens:'opens;
      state:'state;
      state_diff:'state_diff
    > param

  method virtual resolve :
    'state path_in_context -> ('path, unit) result

  method virtual abstract : 'module_expr
  method virtual access :  'access -> 'access
  method virtual access_add :
    'path -> Fault.loc -> Deps.Edge.t -> 'access -> 'access
  method virtual access_init : 'access
  method virtual add_packed :
    Fault.loc -> 'module_expr -> 'packed -> 'packed
  method virtual alias : 'path -> 'module_type
  method virtual annot : 'packed -> 'access -> 'values -> 'annotation

  method virtual apply : Fault.loc -> 'module_expr -> 'module_expr -> 'module_expr

  method virtual bind : Name.t -> 'module_expr -> ('expr,'state_diff) with_state
  method virtual bind_alias: 'state -> Name.t -> Paths.S.t ->
    ('expr,'state_diff) with_state
  method virtual bind_rec : 'bind_rec -> ('expr,'state_diff) with_state
  method virtual bind_rec_add :
    string -> 'module_expr -> 'bind_rec -> ('bind_rec,'state_diff) with_state
  method virtual bind_rec_init : 'bind_rec
  method virtual bind_sig : string -> 'module_type -> ('expr,'state_diff) with_state

  method virtual expr_ext : string -> 'ext -> ('expr,'state_diff) with_state
  method virtual expr_include : loc:Fault.loc -> 'module_expr -> ('expr,'state_diff) with_state
  method virtual expr_open : loc:Fault.loc -> 'module_expr -> ('expr,'state_diff) with_state
  method virtual ext_module : 'm2l -> 'ext
  method virtual ext_val : 'annotation -> 'ext

  method virtual m2l_add : Fault.loc -> ('expr,'state_diff) with_state -> 'm2l -> 'm2l
  method virtual m2l_init : 'm2l
  method virtual m2l: 'm2l  -> 'm2l

  method virtual me_constraint : 'module_expr -> 'module_type -> 'module_expr
  method virtual me_ext : loc:Fault.loc -> string -> 'ext -> 'module_expr
  method virtual me_fun :
    'module_type Arg.t option -> 'module_expr -> 'module_expr
  method virtual me_ident : 'path -> 'module_expr
  method virtual me_val : 'annotation -> 'module_expr
  method virtual minor : 'annotation -> ('expr,'state_diff) with_state

  method virtual mt_ext : loc:Fault.loc -> string -> 'ext -> 'module_type
  method virtual mt_fun :
    'module_type Arg.t option -> 'module_type -> 'module_type
  method virtual mt_ident : 'path_expr -> 'module_type
  method virtual mt_of : 'module_expr -> 'module_type
  method virtual mt_sig : 'm2l -> 'module_type
  method virtual mt_with :
    'access -> Paths.Simple.set -> 'module_type -> 'module_type
  method virtual open_add :
    'path -> 'opens -> 'opens
  method virtual open_init : 'opens
  method virtual open_me : 'opens -> 'module_expr -> 'module_expr

  method virtual packed_init : 'packed

  method virtual path_expr :
    'path -> 'path_expr_args -> 'path_expr
  method virtual path_expr_arg :
    int -> 'path_expr -> 'path_expr_args -> 'path_expr_args
  method virtual path_expr_arg_init : 'path_expr_args
  method virtual sig_abstract : 'module_type
  method virtual sig_include : Fault.loc -> 'module_type -> ('expr,'state_diff) with_state
  method virtual str : 'm2l -> 'module_expr
  method virtual unpacked : 'module_expr

  method virtual value_add : 'm2l -> 'values -> 'values
  method virtual value_init : 'values
  method virtual values : 'values -> 'values

  method virtual state_bind_arg: 'state -> 'module_type Arg.t -> 'state
  method virtual state_open_path: loc:Fault.loc -> 'state -> 'path -> 'state
  method virtual state_merge: 'state -> 'state_diff -> 'state
  method virtual state_is_alias: 'state -> Paths.S.t -> bool


end

module Fold_left = struct
  type waccess = W of access [@@unboxed]

  type ('p, 'focus) expr =
    | Open: ('p param, module_expr) expr
    | Include: ('p param, module_expr) expr
    | SigInclude: ('p param, module_type) expr
    | Bind: Name.t -> ('p param, module_expr) expr
    | Bind_sig: Name.t -> ('p param,module_type) expr
    | Bind_rec:
        {left: 'bind_rec;
         name:Name.t;
         state:'state;
         right:module_expr bind list
        } -> (<state:'state;bind_rec:'bind_rec;..> param, module_expr) expr
    | Minor: ('p param, annotation) expr
    | Extension_node: string -> ('p param, extension_core) expr

  type ('p, 'focus) annot =
    | Packed: {
        left:'packed;
        loc:Fault.loc;
        right: module_expr Loc.ext list;
        access:access;
        values: m2l list }
        -> (<packed:'packed;..> param, module_expr) annot
    | Access :
        {
          packed:'packed;
          values: m2l list
        }
        -> (<packed:'packed;..> param, waccess) annot
    | Values:
        { packed: 'packed;
          access:'access;
          left: 'values;
          state:'state;
          right: m2l list
        }
        -> (<state:'state;packed:'packed; value:'values; access:'access; ..> param, m2l) annot

  type 'p acc =
    {left: 'access;
     right:a list
    } constraint 'p = <access:'access; .. > param


  type  ('p, 'f) path_expr =
    | Main: (int * Paths.Expr.t) list -> (<state:'state; ..> param, 'state path_in_context) path_expr
    | Arg: {
        main: 'path;
        left: 'path_expr_args;
        pos:int;
        right: (int * Paths.Expr.t) list
      } -> (<path:'path; path_expr_args:'path_expr_args; ..> param, Paths.Expr.t) path_expr


  type ('p, 'focus) me =
    | Ident: (<state:'state;..> param, 'state path_in_context) me
    | Apply_left: module_expr -> (_ param , module_expr) me
    | Apply_right: 'module_expr -> (<module_expr:'module_expr; ..> param, module_expr) me
    | Fun_left: {name:string; body:module_expr}
        -> (_ param, module_type) me
    | Fun_right: ('module_type Arg.t, 'state) with_state option
        -> (<module_type:'module_type;state:'state; ..> param, module_expr) me
    | Constraint_left: module_type -> (_ param, module_expr) me
    | Constraint_right: 'module_expr -> (<module_expr:'module_expr; ..> param, module_type) me
    | Str: (_ param, m2l) me
    | Val: (_ param, annotation) me
    | Extension_node: string -> (_ param, extension_core) me
    | Open_me_left:
        { left: 'opens;
          right:Paths.S.t list;
          state:'state;
          expr:module_expr
        } -> (<opens:'opens;state:'state; ..> param, 'state path_in_context) me
    | Open_me_right: {opens:'opens;state:'state} ->
        (<opens:'opens; state:'state; ..> param, module_expr) me

  type ('p,'focus) mt =
    | Alias: (<state:'state;..> param, 'state path_in_context) mt
    | Ident: (_ param, Paths.Expr.t) mt
    | Sig: (_ param, m2l) mt
    | Fun_left: {name:string; body:module_type}
        -> ( _ param, module_type) mt
    | Fun_right: ('module_type Arg.t, 'state) with_state option
        -> (< module_type:'module_type; state:'state; ..> param , module_type) mt
    | With_access:
        {body:module_type; deletions: Paths.S.set }
        -> (_ param, waccess) mt
    | With_body:
        {access:'access; deletions:Paths.S.set }
        -> (<access:'access;..> param, module_type) mt
    | Of: (_ param, module_expr) mt
    | Extension_node: string -> (_ param, extension_core) mt

  type 'focus ext =
    | Mod: m2l ext
    | Val: annotation ext

  type none = No

  type ('p, 'elt,'from) elt =
    | M2l: {left:'m2l;
            loc:Fault.loc;
            state:'state;
            right:m2l}
        -> (<m2l:'m2l;state:'state; ..> param, expression, m2l) elt
    | Expr: ('p,'elt) expr -> ('p,'elt,expression) elt
    | Annot: ('p,'elt) annot -> ('p,'elt, annotation) elt
    | Me: ('p,'elt) me -> ('p,'elt, module_expr) elt
    | Mt: ('p,'elt) mt -> ('p,'elt, module_type) elt
    | Access: (<state:'state;..> param as 'p) acc
        -> ('p, 'state path_in_context, waccess) elt
    | Ext: 'elt ext -> ('p param, 'elt, extension_core) elt
    | Path_expr: ('p, 'elt) path_expr -> ('p,'elt, Paths.Expr.t) elt

  type ('p,'f) path =
    | []: (_ param, m2l) path
    | (::): ('p, 'focus,'from) elt * ('p,'from) path -> ('p,'focus) path

end
(*
type 'a witness = W: ('a,'from) elt -> 'a witness [@@unboxed]

let witness (type a) (path:a path): a witness = match path with
  | [] -> W Start
  | a :: _ -> W a
*)


type ('param, 'result) zipper =
 { path: ('param param, 'result) Fold_left.path; focus: 'result }

let start focus = { path = []; focus}

module L = struct
  type 'a t = 'a list = [] | (::) of 'a * 'a t
end
module Fold = struct
  open Fold_left

  type id_fold = <
    path:Paths.S.t;
    module_expr:module_expr;
    access:access;
    annotation:annotation;
    bind_rec: module_expr bind list;
    expr: expression;
    module_type:module_type;
    ext:extension_core;
    m2l:m2l;
    opens: Paths.S.t list;
    packed: module_expr Loc.ext list;
    path_expr:Paths.Expr.t;
    path_expr_args: (int * Paths.Expr.t) list;
    value: m2l list;
    state:unit;
    state_diff:unit;
  > fold
  module Ok = Mresult.Ok
  let ((>>=), (>>|)) = Ok.((>>=), (>>|))

  let apkg (f,_) = f

  let update_state f r =
    f#state_merge r.state

  let rec m2l
      (folder: _ fold) path left ~pkg ~initial_state ~state = function
    | L.[] -> Ok (folder#m2l left)
    | {Loc.loc; data=a} :: right ->
      let loc = pkg, loc in
      expr folder (M2l {left;loc;state=initial_state;right} :: path) ~loc ~state a >>= fun r ->
      let left = folder#m2l_add loc r left in
      let state = update_state folder r state in
      m2l folder path left ~pkg ~initial_state ~state right
  and m2l_start f path left ~pkg ~state = m2l f path left ~pkg ~state ~initial_state:state
  and expr folder path ~loc ~state expr = match expr with
    | Defs _ -> assert false (* to be removed *)
    | Open m -> me folder (Expr Open::path) ~loc ~state m
      >>| folder#expr_open ~loc
    | Include m ->
      me folder (Expr Include :: path) ~loc ~state m
      >>| folder#expr_include ~loc
    | SigInclude m ->
      mt folder (Expr SigInclude :: path) ~loc ~state m
      >>| folder#sig_include loc
    | Bind {name; expr=Ident s} when folder#state_is_alias state s ->
      Ok (folder#bind_alias state name s)
    | Bind {name; expr} ->
      me folder (Expr (Bind name) :: path) ~loc ~state expr
      >>| folder#bind name
    | Bind_sig {name; expr} ->
      mt folder (Expr (Bind_sig name) :: path) ~loc ~state expr
      >>| folder#bind_sig name
    | Bind_rec l ->
      bind_rec folder path ~loc ~state folder#bind_rec_init l >>|
      folder#bind_rec
    | Minor m ->
      minor folder (Expr Minor :: path) ~pkg:(apkg loc) ~state m >>| folder#minor
    | Extension_node {name;extension} ->
      ext ~pkg:(apkg loc) ~state folder (Expr (Extension_node name) :: path) extension
      >>| folder#expr_ext name
  and me folder path  ~loc ~state = function
    | Resolved _ -> assert false (* to be removed *)
    | Ident s ->
      resolve folder (Me Ident :: path) ~loc ~state ~level:Module s
      >>| folder#me_ident
    | Apply {f; x} ->
      me folder (Me (Apply_left x)::path) ~loc ~state f >>= fun f ->
      me folder (Me (Apply_right f)::path) ~loc ~state x >>|
      folder#apply loc f
    | Fun {arg = None; body } ->
      me folder (Me (Fun_right None) :: path) ~loc ~state body
      >>| folder#me_fun None
    | Fun {arg = Some arg ; body } ->
      let pth = Me (Fun_left {name=arg.name; body})  :: path in
      mt folder pth ~loc ~state arg.signature >>= fun signature ->
      let r = {arg with signature} in
      let state = folder#state_bind_arg state r in
      let arg = Some { r; state } in
      me folder (Me (Fun_right arg) :: path ) ~loc ~state body >>|
      folder#me_fun (Some r)
    | Constraint (mex,mty) ->
      me folder (Me (Constraint_left mty)::path) ~loc ~state  mex >>= fun me ->
      mt folder (Me (Constraint_right me)::path) ~loc ~state mty >>|
      folder#me_constraint me
    | Str items ->
      m2l_start folder (Me Str :: path) ~pkg:(apkg loc) ~state folder#m2l_init items
      >>| folder#str
    | Val v -> minor folder (Me Val::path) ~pkg:(apkg loc) ~state v >>| folder#me_val
    | Extension_node {name;extension=e} ->
      ext ~pkg:(apkg loc) ~state folder (Me (Extension_node name) :: path) e >>| folder#me_ext ~loc name
    | Abstract -> Ok folder#abstract
    | Unpacked -> Ok folder#unpacked
    | Open_me {opens; expr; _ } ->
      open_all folder path expr folder#open_init ~loc ~state opens >>=
      open_right folder path ~loc ~state expr
  and open_right folder path expr ~state ~loc r =
    me folder (Me (Open_me_right {state;opens=r.r}) :: path) ~loc ~state:r.state expr >>|
    folder#open_me r.r
  and open_all_rec folder path expr left ~loc ~initial_state ~state = function
    | L.[] -> Ok {state; r=left}
    | a :: right ->
      let path' = Me (Open_me_left {left; right; state=initial_state; expr}) :: path in
      resolve folder path' ~state ~loc ~level:Module a >>= fun a ->
      let state = folder#state_open_path ~loc state a in
      open_all folder path expr (folder#open_add a left) ~loc ~state right
  and open_all folder path expr left ~state ~loc =
    open_all_rec folder path expr left ~loc ~initial_state:state ~state
  and mt folder path ~loc ~state = function
    | Resolved _ -> assert false (* to be removed *)
    | Alias id ->
      resolve folder (Mt Alias :: path) ~loc ~state ~level:Module_type id
      >>| folder#alias
    | Ident ids ->
      path_expr folder (Mt Ident :: path) ~loc ~state ids >>| folder#mt_ident
    | Sig items ->
      m2l_start folder (Mt Sig :: path) ~pkg:(apkg loc) ~state folder#m2l_init items
      >>| folder#mt_sig
    | Fun {arg = None; body } ->
      mt folder (Mt (Fun_right None)::path) ~loc ~state body
      >>| folder#mt_fun None
    | Fun {arg = Some arg; body } ->
      let arg_path = Mt(Fun_left {name=arg.name; body} )::path in
      mt folder arg_path ~loc ~state arg.signature >>= fun signature ->
      let r = { arg with signature } in
      let state = folder#state_bind_arg state r in
      let arg = Some {state; r} in
      mt folder (Mt(Fun_right arg)::path) ~state ~loc body >>|
      folder#mt_fun (Some r)
    | With {body;deletions;access=a} ->
      let access_path = Mt (With_access {body;deletions})::path in
      access ~pkg:(apkg loc) ~state folder access_path a >>= fun access ->
      mt folder (Mt (With_body {access;deletions})::path) ~loc ~state body >>|
      folder#mt_with access deletions
    | Of m -> me folder (Mt Of :: path) ~loc ~state m >>| folder#mt_of
    | Extension_node {name;extension=e} ->
      ext ~pkg:(apkg loc) ~state folder (Mt (Extension_node name)::path)  e >>| folder#mt_ext ~loc name
    | Abstract -> Ok folder#sig_abstract
  and bind_rec folder path left ~loc ~state = function
    | L.[] -> Ok left
    | {name;expr} :: right ->
      me folder (Expr(Bind_rec{left;state;name;right}) :: path) ~loc ~state expr
      >>= fun me ->
      let { r; state } = folder#bind_rec_add name me left in
      bind_rec folder path r ~loc ~state right
  and path_expr_args folder path ~loc ~state main left = function
    | L.[] -> Ok (folder#path_expr main left)
    | (n, arg) :: right ->
      let path_arg = Path_expr (Arg {main;left;pos=n;right})::path in
      path_expr folder path_arg arg ~loc ~state >>= fun x ->
      path_expr_args folder path main ~loc ~state
        (folder#path_expr_arg n x left) right
  and path_expr folder ctx ~loc ~state {Paths.Expr.path; args} =
    resolve ~level:Module_type ~loc ~state folder
      (Path_expr (Main args) :: ctx) path >>= fun main ->
    path_expr_args folder ctx main folder#path_expr_arg_init ~loc ~state args
  and minor folder path ~pkg ~state mn =
    let i = folder#packed_init in
    packed folder path mn.access mn.values i  ~pkg ~state mn.packed >>= fun packed ->
    let fpath = Annot (Access {packed; values=mn.values}) :: path in
    access folder fpath mn.access ~pkg ~state >>= fun access ->
    values folder path packed access ~pkg ~state mn.values
  and values folder path packed access ~pkg ~state values =
    m2ls folder path packed access ~state ~pkg folder#value_init values >>|
    folder#annot packed access
  and m2ls folder path packed access ~pkg ~state left = function
    | L.[] -> Ok (folder#values left)
    | a :: right ->
      let fpath = Annot (Values {packed;access; left; state; right}) :: path in
      m2l_start folder fpath folder#m2l_init ~pkg ~state a >>= fun a ->
      m2ls folder path packed access (folder#value_add a left) ~pkg ~state right
  and packed folder path access values left ~pkg ~state = function
    | L.[] -> Ok left
    | (a: _ Loc.ext) :: right ->
      let loc = pkg, a.loc in
      let fpath =
        Annot (Packed { access; values; left ; loc; right})::path in
      me folder fpath a.data ~state ~loc  >>= fun m ->
      packed folder path access values ~pkg ~state
        (folder#add_packed loc m left) right
  and access folder path ~pkg ~state s =
    access_step ~state ~pkg folder path folder#access_init (Paths.S.Map.bindings s)
  and access_step folder path left ~pkg ~state = function
    | [] -> Ok (folder#access left)
    | (a, (loc,edge)) :: right ->
      let loc = pkg, loc in
      resolve folder ~state ~loc ~edge ~level:Module
        (Access {left;right}::path) a >>= fun a ->
      access_step folder path ~pkg ~state (folder#access_add a loc edge left) right
  and ext folder path ~pkg ~state = function
    | Module m -> m2l_start ~pkg ~state folder (Ext Mod :: path) folder#m2l_init m
      >>| folder#ext_module
    | Val v -> minor ~state ~pkg folder (Ext Val :: path) v >>| folder#ext_val
  and resolve folder path ~loc ~level ~state ?edge s =
    let px = { edge; level; loc; state; path = s } in
    match folder#resolve px with
    | Error () -> Error ( { path; focus= px })
    | Ok _ as x -> x

  let rec restart (f:_ fold) (z: (_, _ path_in_context) zipper) :
    (t, (_, _ path_in_context) zipper) result =
    let v = z.focus in
    let state, loc = v.state, v.loc in
    match f#resolve z.focus with
    | Error _ -> Error z
    | Ok x -> self_restart f @@ match z.path with
      | Me Ident :: rest ->
        restart_me f ~state ~loc (rest: (_,module_expr) path) (f#me_ident x)
      | Me (Open_me_left {left;right;state=restart;expr}) :: path ->
        open_all f ~state ~loc path expr (f#open_add x left) right >>= fun r ->
        open_right f path expr ~loc ~state r >>= fun me ->
        restart_me f ~state:restart ~loc (path:(_,module_expr) path) me
      | Access a :: rest ->
        access_step f rest ~pkg:(apkg loc) ~state (f#access_add x v.loc (default_edge v.edge) a.left) a.right
        >>= restart_access ~loc ~state f (rest: (_,waccess) path)
      | Mt Alias :: path ->
        restart_mt ~loc ~state f (path: (_,module_type) path) (f#alias x)
      | Path_expr Main args :: path ->
        path_expr_args ~state ~loc f path x f#path_expr_arg_init args >>=
        restart_path_expr ~loc ~state f (path: (_,Paths.Expr.t) path)
      | _ -> .
  and self_restart f = function
    | Error z -> restart f z
    | Ok _ as x -> x
  and restart_me f ~state ~loc path x = match path with
    | Expr Include :: rest ->
      restart_expr ~state f (rest: (_,expression) path) (f#expr_include ~loc x)
    | Expr Open :: rest ->
      restart_expr ~state f (rest: (_,expression) path) (f#expr_open ~loc x)
    | Annot (Packed p) :: path ->
      let pkg = apkg loc in
      let left = f#add_packed p.loc x p.left in
      packed f path p.access p.values left ~pkg ~state p.right  >>= fun packed ->
      let fpath = Annot (Access {packed; values=p.values}) :: path in
      access f fpath ~pkg ~state p.access >>= fun access ->
      values f path packed access ~pkg ~state p.values >>=
      restart_annot ~loc ~state f (path: (_,annotation) path)
    | Me (Apply_left xx) :: path ->
      me f (Me (Apply_right x)::path) ~state ~loc xx
      >>| f#apply loc x
      >>= restart_me ~loc ~state f path
    | Mt Of :: path -> restart_mt ~loc ~state f (path: (_,module_type) path) (f#mt_of x)
    | Me(Apply_right fn) :: path -> restart_me f path ~loc ~state (f#apply loc fn x)
    | Me(Fun_right None) :: path ->
      restart_me f path ~state ~loc (f#me_fun None x)
    | Me(Fun_right Some {r;state}) :: path ->
      restart_me f path ~state ~loc (f#me_fun (Some r) x)
    | Me (Constraint_left mty) :: path ->
      mt f (Me (Constraint_right x)::path) ~loc ~state mty >>= fun mt ->
      restart_me f path ~loc ~state (f#me_constraint x mt)
    | Me (Open_me_right {opens;state}) :: path ->
      restart_me f path ~loc ~state (f#open_me opens x)
    | Expr (Bind name) :: path ->
      restart_expr f (path: (_,expression) path) ~state (f#bind name x)
    | Expr (Bind_rec {left;name;state;right}) :: path  ->
      let {r=left; state=more} = f#bind_rec_add name x left in
      let state = f#state_merge state more in
      bind_rec f path left ~loc ~state right >>| f#bind_rec >>=
      restart_expr f ~state (path: (_,expression) path)
    | _ -> .
  and restart_expr f path ~state x = match path with
    | M2l {left;loc;right; state=restart } :: path ->
      let state  = f#state_merge state x.state in
      m2l f path (f#m2l_add loc x left) ~pkg:(apkg loc) ~initial_state:restart ~state right >>=
      restart_m2l ~loc ~state:restart f (path: (_,m2l) path)
    | _ -> .
  and restart_mt f path ~state ~loc x = match path with
    | Expr (Bind_sig name) :: path ->
      restart_expr ~state f (path: (_,expression) path) (f#bind_sig name x)
    | Me Fun_left {name;body} :: path ->
      let r =  {Arg.signature = x; name } in
      me f (Me(Fun_right (Some {r;state})) :: path ) ~loc ~state body >>|
      f#me_fun (Some r) >>= restart_me f path ~loc ~state
    | Mt Fun_left {name;body} :: path ->
      let r = {Arg.signature = x; name } in
      let inner_state = f#state_bind_arg state r in
      mt f (Mt(Fun_right(Some {r;state})) :: path ) ~loc ~state:inner_state body >>|
     f#mt_fun (Some r) >>= restart_mt ~loc ~state f path
    | Mt Fun_right (Some {r=arg;state}) :: path ->
      restart_mt ~loc ~state f path (f#mt_fun (Some arg) x)
    | Mt Fun_right None :: path ->
      restart_mt ~loc ~state f path (f#mt_fun None x)
    | Mt With_body {access;deletions} :: path ->
      restart_mt ~loc ~state f path (f#mt_with access deletions x)
    | Me Constraint_right body :: path ->
      restart_me ~loc ~state f (path: (_,module_expr) path) (f#me_constraint body x)
    | Expr SigInclude :: path ->
      restart_expr ~state f (path: (_,expression) path) (f#sig_include loc x)
    | _ -> .
  and restart_path_expr ~loc ~state f path x = match path with
    | Mt Ident :: path ->
      restart_mt f (path: (_,module_type) path) ~state ~loc  (f#mt_ident x)
    | Path_expr Arg {main;left;pos;right} :: path ->
      let left = f#path_expr_arg pos x left in
      path_expr_args f path main left right ~loc ~state >>=
      restart_path_expr ~loc ~state f path
    | _ -> .
  and restart_access ~loc ~state f  path x = match path with
    | Annot (Access mn) :: path ->
      values ~pkg:(apkg loc) ~state f path mn.packed x mn.values >>=
      restart_annot ~loc ~state f (path: (_,annotation) path)
    | Mt With_access {deletions;body} :: path ->
      mt ~loc ~state f (Mt(With_body {deletions;access=x}) :: path ) body
      >>| f#mt_with x deletions
      >>= restart_mt ~loc ~state f path
    | _ -> .
  and restart_annot f path ~loc ~state x = match path with
    | Expr Minor :: path ->
      restart_expr ~state f (path: (_,expression) path) (f#minor x)
    | Me Val :: path ->
      restart_me ~loc ~state f (path: (_,module_expr) path) (f#me_val x)
    | Ext Val :: path ->
      restart_ext f (path: (_,extension_core) path) ~loc ~state (f#ext_val x)
    | _ -> .
  and restart_ext ~loc ~state f path x = match path with
    | Expr (Extension_node name) :: path ->
      restart_expr ~state f (path: (_,expression) path) (f#expr_ext name x)
    | Me (Extension_node name) :: path ->
      restart_me f (path: (_,module_expr) path) ~loc ~state (f#me_ext ~loc name x)
    | Mt (Extension_node name) :: path ->
      restart_mt f (path: (_,module_type) path) ~loc ~state (f#mt_ext ~loc name x)
    | _ -> .
  and restart_m2l f ~loc ~state path x = match path with
    | [] -> Ok x
    | Me Str :: path -> restart_me ~loc ~state f (path: (_,module_expr) path) (f#str x)
    | Mt Sig :: path -> restart_mt ~loc ~state f (path: (_,module_type) path) (f#mt_sig x)
    | Annot (Values {packed;access;state;left;right}) :: path ->
      let left = f#value_add x left in
      m2ls f path packed access left ~pkg:(apkg loc) ~state right >>| f#annot packed access
      >>= restart_annot ~loc ~state f (path: (_,annotation) path)
    | Ext Mod :: path ->
      restart_ext f ~loc ~state (path: (_,extension_core) path) (f#ext_module x)
    | _ :: _ -> .
  let ustart f x =m2l_start f [] f#m2l_init x
end

open L

let (<+>) r state = { r; state }

type 'a no_state = ('a,unit) with_state

let no_state x = x <+> ()

class id = object
  inherit [_]fold
  method resolve x = Ok x.path

  method abstract: module_expr = Abstract
  method access l: access = l
  method access_add p loc edge a = Paths.S.Map.add p (snd loc,edge) a
  method access_init = Paths.S.Map.empty
  method add_packed loc me l = {Loc.loc=snd loc;data=me} :: l
  method alias x = Alias x
  method apply _ f x = Apply {f;x}
  method annot packed access values = {packed;access;values}
  method bind name me = no_state @@ Bind {name; expr = me}
  method bind_alias _ name s = no_state @@ Bind {name; expr = Ident s}

  method bind_rec l = no_state @@ Bind_rec l
  method bind_rec_add  name expr l = no_state @@ {name;expr} :: l
  method bind_rec_init = []
  method bind_sig name mt = no_state @@ Bind_sig {name;expr=mt}
  method expr_ext name ext = no_state (Extension_node {name;extension=ext}:expression)
  method expr_include ~loc:_ x: expression no_state = no_state (Include x)
  method expr_open ~loc:_ x: expression no_state = no_state (Open x)
  method ext_module m = Module m
  method ext_val m: extension_core = Val m
  method m2l_add loc x l = {Loc.loc=snd loc; data=x.r} :: l
  method m2l_init = []
  method m2l l = List.rev l
  method me_constraint m mt: module_expr = Constraint(m,mt)
  method me_ext ~loc:_ name extension: module_expr = Extension_node {name;extension}
  method me_fun arg body: module_expr = Fun {arg;body}
  method me_ident x: module_expr = Ident x
  method me_val x: module_expr = Val x
  method minor annot = no_state (Minor annot)
  method mt_ext ~loc:_ name extension: module_type = Extension_node {name;extension}
  method mt_fun arg body: module_type = Fun {arg;body}
  method mt_of x = Of x
  method mt_ident x : module_type = Ident x
  method mt_with access deletions mt = With {access;deletions;body=mt}
  method mt_sig x = Sig x
  method open_add s l = s :: l
  method open_init = []
  method open_me opens expr = Open_me {opens;expr;resolved=Summary.empty}
  method packed_init = []
  method path_expr path args = {Paths.Expr.path; args }
  method path_expr_arg n x l = (n,x) :: l
  method path_expr_arg_init = []
  method sig_include _ m = no_state (SigInclude m)
  method sig_abstract: module_type = Abstract
  method str x = (Str x: module_expr)
  method unpacked = Unpacked
  method value_add m2l l = m2l :: l
  method value_init = []
  method values l = List.rev l

  method state_open_path ~loc:_ () _ = ()
  method state_bind_arg () _ = ()
  method state_merge () () = ()
  method state_is_alias () _ = false

end


let id state = () <+> state
let none = () <+> ()

class iterator = object
  inherit [ <ext:unit;..> ]fold
  method resolve _s = Ok ()

  method abstract = ()
  method access () = ()
  method access_add _p _loc _edge () = ()
  method access_init = ()
  method add_packed _loc _me () = ()
  method alias () = ()
  method apply _ () () = ()
  method annot () () () = ()
  method bind  _name () = none
  method bind_rec () = none
  method bind_rec_add _name () () = none
  method bind_rec_init = ()
  method bind_sig _name () = none
  method bind_alias () _ _ = none

  method expr_ext _name _ = none
  method expr_include ~loc:_ () = none
  method expr_open ~loc:_ () = none
  method ext_module () =()
  method ext_val () = ()
  method m2l_add _loc _ () = ()
  method m2l_init = ()
  method m2l () = ()
  method me_constraint () () = ()
  method me_ext ~loc:_ _name () = ()
  method me_fun _arg () = ()
  method me_ident () = ()
  method me_val () = ()
  method minor () = none
  method mt_ext ~loc:_ _name _ = ()
  method mt_fun _arg ()  = ()
  method mt_of () = ()
  method mt_ident () = ()
  method mt_with () _deletions () = ()
  method mt_sig () = ()
  method open_add () () = ()
  method open_init = ()
  method open_me () () = ()
  method packed_init = ()
  method path_expr () () = ()
  method path_expr_arg _n () () = ()
  method path_expr_arg_init = ()
  method sig_include _ () = none
  method sig_abstract = ()
  method str () = ()
  method unpacked = ()
  method value_add () () = ()
  method value_init = ()
  method values () = ()

  method state_bind_arg () _ = ()
  method state_open_path ~loc:_ () _ = ()
  method state_merge () () = ()
  method state_is_alias () _ = false
end


(*
class pair (base:_ fold) (next: _ fold) = object
  inherit [_]fold
  method resolve x = match base#resolve x, next#resolve x with
    | Ok b, Ok n -> Ok (b,n)
    | Error (), Ok _ | Ok _, Error () | Error (), Error () -> Error ()

  method abstract = base#abstract, next#abstract
  method access y = next#access y
  method access_add p loc edge acc = next#access_add p loc edge acc
  method access_init = next#access_init
  method add_packed loc me packed = next#add_packed loc me packed
  method alias (x,y) = base#alias x, next#alias y
  method apply _ () () = ()
  method annot loc (f,f') (_,x') =
    base#apply loc f,
    next#apply loc f' x'

  method bind  name (x,y) =
    { state = base#bind name x; r = next#bind name y }
  method bind_rec (x,y) =
    {state=base#bind_rec x; r = next#bind_rec y}
  method bind_rec_add name (x,y) (xacc, yacc) =
    let l = base#bind_rec name x xacc in
    { l with r = l.r, next#bind_rec name y yacc}
  method bind_rec_init = base#bind_rec_init, next#bind_rec_init
  method bind_sig name (x, y) =
        { state = base#bind_sig name x; r = next#bind_sig name y }
  method bind_alias state name p =
    let state = base#bind_alias state name p in
    { state; r = next#bind_alias state name }

  method expr_ext _name _ = none
  method expr_include ~loc:_ () = none
  method expr_open ~loc:_ () = none
  method ext_module x =next#ext_module x
  method ext_val x = next#ext_module x
  method m2l_add loc expr (defs,y) =
    S.merge defs (Y.defined expr.state), next#m2l_add loc expr y
  method m2l_init = ()
  method m2l () = ()
  method me_constraint () () = ()
  method me_ext ~loc:_ _name () = ()
  method me_fun _arg () = ()
  method me_ident () = ()
  method me_val () = ()
  method minor () = none
  method mt_ext ~loc:_ _name _ = ()
  method mt_fun _arg ()  = ()
  method mt_of () = ()
  method mt_ident () = ()
  method mt_with () _deletions () = ()
  method mt_sig () = ()
  method open_add () () = ()
  method open_init = ()
  method open_me () () = ()
  method packed_init = ()
  method path_expr () () = ()
  method path_expr_arg _n () () = ()
  method path_expr_arg_init = ()
  method sig_include _ () = none
  method sig_abstract = ()
  method str () = ()
  method unpacked = ()
  method value_add () () = ()
  method value_init = ()
  method values () = ()

  method state_bind_arg () _ = ()
  method state_open_path ~loc:_ () _ = ()
  method state_merge () () = ()
  method state_is_alias () _ = false

end
*)


class failing_id = object
  inherit id
  method! resolve s = if Random.float 1. > 0.99 then Ok s.path else Error ()
end

let pkg = Paths.Pkg.local "internal"
let failing_deep_id x =
  let f = new failing_id in
  let rec try_at_most n x =
    match x with
      | Ok x -> x
      | Error e ->
        try_at_most (n-1) (Fold.restart f e) in
  try_at_most 1000 (Fold.ustart ~state:() ~pkg f x)

let deep_nop x =
  Fold.ustart ~state:() ~pkg (new iterator) x
