
module L = struct
  type 'a t = 'a list = [] | (::) of 'a * 'a t
end

module Sk = M2l_skel
module Arg = Module.Arg

type a = Paths.Simple.t * (Loc.t * Deps.Edge.t)
type level = Module.level = Module | Module_type

type 'state path_in_context = 'state Sk.path_in_context =
  { loc: Fault.loc;
    edge:Deps.Edge.t option;
    level: Module.level;
    ctx: 'state;
    path: Paths.S.t
  }

type ('a,'b) with_state = ('a,'b) Sk.with_state = { r:'a; state:'b}

module type fold = sig
 type path
 type module_expr
 type access
 type packed
 type module_type
 type m2l
 type expr
 type values
 type annotation
 type bind_rec
 type ext
 type path_expr
 type path_expr_args
 type opens
 type state
 type state_diff

  val resolve : state -> state_diff path_in_context -> (path, unit) result

  val abstract : module_expr
  val access :  access -> access
  val access_add :
    path -> Fault.loc -> Deps.Edge.t -> access -> access
  val access_init : access
  val add_packed :
    Fault.loc -> module_expr -> packed -> packed
  val alias : path -> module_type
  val annot : packed -> access -> values -> annotation

  val apply : Fault.loc -> module_expr -> module_expr -> module_expr

  val bind : Name.t -> module_expr -> (expr,state_diff) with_state
  val bind_alias: state -> Name.t -> Paths.S.t ->
    (expr,state_diff) with_state
  val bind_rec : bind_rec -> (expr,state_diff) with_state
  val bind_rec_add :
    string -> module_expr -> bind_rec -> (bind_rec,state_diff) with_state
  val bind_rec_init : bind_rec
  val bind_sig : string -> module_type -> (expr,state_diff) with_state

  val expr_ext : string -> ext -> (expr,state_diff) with_state
  val expr_include : loc:Fault.loc -> module_expr -> (expr,state_diff) with_state
  val expr_open : loc:Fault.loc -> module_expr -> (expr,state_diff) with_state
  val ext_module : m2l -> ext
  val ext_val : annotation -> ext

  val m2l_add : Fault.loc -> (expr,state_diff) with_state -> m2l -> m2l
  val m2l_init : m2l
  val m2l: m2l  -> m2l

  val me_constraint : module_expr -> module_type -> module_expr
  val me_ext : loc:Fault.loc -> string -> ext -> module_expr
  val me_fun :
    module_type Arg.t option -> module_expr -> module_expr
  val me_ident : path -> module_expr
  val me_val : annotation -> module_expr
  val minor : annotation -> (expr,state_diff) with_state

  val mt_ext : loc:Fault.loc -> string -> ext -> module_type
  val mt_fun :
    module_type Arg.t option -> module_type -> module_type
  val mt_ident : path_expr -> module_type
  val mt_of : module_expr -> module_type
  val mt_sig : m2l -> module_type
  val mt_with :
    access -> Paths.Simple.set -> module_type -> module_type
  val open_add :
    path -> opens -> opens
  val open_init : opens
  val open_me : opens -> module_expr -> module_expr

  val packed_init : packed

  val path_expr :
    path -> path_expr_args -> path_expr
  val path_expr_arg :
    int -> path_expr -> path_expr_args -> path_expr_args
  val path_expr_arg_init : path_expr_args
  val sig_abstract : module_type
  val sig_include : Fault.loc -> module_type -> (expr,state_diff) with_state
  val str : m2l -> module_expr
  val unpacked : module_expr

  val value_add : m2l -> values -> values
  val value_init : values
  val values : values -> values

  val state_bind_arg: state -> module_type Arg.t -> state
  val state_open_path: loc:Fault.loc -> state -> path -> state
  val state_merge: state -> state_diff -> state
  val state_is_alias: state -> Paths.S.t -> bool

  val state_diff: state -> state_diff
  val state_restart: state -> state_diff -> state

end

let apkg (f,_) = f
module Ok = Mresult.Ok

let ((>>=), (>>|)) = Ok.((>>=), (>>|))

module Fold(F:fold) = struct
  open F
  type nonrec path_in_context = state_diff path_in_context
  module Path = struct
    type waccess = W of M2l.access [@@unboxed]

    type 'focus expr =
      | Open: M2l.module_expr expr
      | Include:  M2l.module_expr expr
      | SigInclude:  M2l.module_type expr
      | Bind: Name.t ->  M2l.module_expr expr
      | Bind_sig: Name.t -> M2l.module_type expr
      | Bind_rec:
          {left: bind_rec;
           name:Name.t;
           right:M2l.module_expr M2l.bind list
          } -> M2l.module_expr expr
      | Minor:  M2l.annotation expr
      | Extension_node: string ->  M2l.extension_core expr

    type 'focus annot =
      | Packed: {
          left:packed;
          loc:Fault.loc;
          right: M2l.module_expr Loc.ext list;
          access: M2l.access;
          values: M2l.m2l list }
          -> M2l.module_expr annot
      | Access :
          {
            packed: packed;
            values: M2l.m2l list
          }
          -> waccess annot
      | Values:
          { packed: packed;
            access: access;
            left: values;
            right: M2l.m2l list
          }
          -> M2l.m2l annot

    type acc =
      {left: access;
       right:a list
      }

    type  'f path_expr =
      | Main: (int * Paths.Expr.t) list -> path_in_context path_expr
      | Arg: {
          main: path;
          left: path_expr_args;
          pos:int;
          right: (int * Paths.Expr.t) list
        } -> Paths.Expr.t path_expr


    type 'focus me =
      | Ident: path_in_context me
      | Apply_left: M2l.module_expr -> M2l.module_expr me
      | Apply_right: module_expr -> M2l.module_expr me
      | Fun_left: {name:string; body:M2l.module_expr} -> M2l.module_type me
      | Fun_right:
          (module_type Arg.t,state) with_state option -> M2l.module_expr me
      | Constraint_left: M2l.module_type -> M2l.module_expr me
      | Constraint_right: module_expr -> M2l.module_type me
      | Str: M2l.m2l me
      | Val: M2l.annotation me
      | Extension_node: string -> M2l.extension_core me
      | Open_me_left:
          { left: opens;
            right:Paths.S.t list;
            diff:state_diff;
            expr:M2l.module_expr
          } -> path_in_context me
      | Open_me_right: {opens:opens; state:state} -> M2l.module_expr me

    type 'focus mt =
      | Alias: path_in_context mt
      | Ident: Paths.Expr.t mt
      | Sig: M2l.m2l mt
      | Fun_left: {name:string; body:M2l.module_type} -> M2l.module_type mt
      | Fun_right: (module_type Arg.t,state) with_state option
          -> M2l.module_type mt
      | With_access:
          {body:M2l.module_type; deletions: Paths.S.set} -> waccess mt
      | With_body:
          {access:access; deletions:Paths.S.set } -> M2l.module_type mt
      | Of: M2l.module_expr mt
      | Extension_node: string -> M2l.extension_core mt

    type 'focus ext =
      | Mod: M2l.m2l ext
      | Val: M2l.annotation ext

    type ('elt,'from) elt =
      | M2l: {left:m2l;
              loc:Fault.loc;
              state:state;
              right:M2l.m2l}
          -> (M2l.expression, M2l.m2l) elt
      | Expr: 'elt expr -> ('elt,M2l.expression) elt
      | Annot: 'elt annot -> ('elt, M2l.annotation) elt
      | Me: 'elt me -> ('elt, M2l.module_expr) elt
      | Mt: 'elt mt -> ('elt, M2l.module_type) elt
      | Access: acc -> (path_in_context, waccess) elt
      | Ext: 'elt ext ->  ('elt, M2l.extension_core) elt
      | Path_expr: 'elt path_expr -> ('elt, Paths.Expr.t) elt

    type 'f path =
      | []: M2l.m2l path
      | (::): ('focus,'from) elt * 'from path -> 'focus path

    type 'result zipper = { path: 'result path; focus: 'result }

  end
  open Path

  let update_state state (r:_ with_state) = state_merge state r.state
  let default_edge = Option.default Deps.Edge.Normal


  let rec m2l path left ~pkg ~initial_state ~state = function
    | L.[] -> Ok (F.m2l left)
    | {Loc.loc; data=a} :: right ->
      let loc = pkg, loc in
      expr (M2l {left;loc;state=initial_state;right} :: path) ~loc ~state a >>= fun r ->
      let left = F.m2l_add loc r left in
      let state = update_state state r in
      m2l path left ~pkg ~initial_state ~state right
  and m2l_start path left ~pkg ~state = m2l path left ~pkg ~state ~initial_state:state
  and expr path ~loc ~state expr = match expr with
    | Defs _ -> assert false (* to be removed *)
    | Open m -> me (Expr Open::path) ~loc ~state m
      >>| F.expr_open ~loc
    | Include m ->
      me (Expr Include :: path) ~loc ~state m
      >>| F.expr_include ~loc
    | SigInclude m ->
      mt (Expr SigInclude :: path) ~loc ~state m
      >>| F.sig_include loc
    | Bind {name; expr=Ident s} when F.state_is_alias state s ->
      Ok (F.bind_alias state name s)
    | Bind {name; expr} ->
      me (Expr (Bind name) :: path) ~loc ~state expr
      >>| F.bind name
    | Bind_sig {name; expr} ->
      mt (Expr (Bind_sig name) :: path) ~loc ~state expr
      >>| F.bind_sig name
    | Bind_rec l ->
      bind_rec path ~loc ~state F.bind_rec_init l >>|
      F.bind_rec
    | Minor m ->
      minor (Expr Minor :: path) ~pkg:(apkg loc) ~state m >>| F.minor
    | Extension_node {name;extension} ->
      ext ~pkg:(apkg loc) ~state (Expr (Extension_node name) :: path) extension
      >>| F.expr_ext name
  and me path  ~loc ~state = function
    | Resolved _ -> assert false (* to be removed *)
    | Ident s ->
      resolve (Me Ident :: path) ~loc ~state ~level:Module s
      >>| F.me_ident
    | Apply {f; x} ->
      me (Me (Apply_left x)::path) ~loc ~state f >>= fun f ->
      me (Me (Apply_right f)::path) ~loc ~state x >>|
      F.apply loc f
    | Fun {arg = None; body } ->
      me (Me (Fun_right None) :: path) ~loc ~state body
      >>| F.me_fun None
    | Fun {arg = Some arg ; body } ->
      let pth = Me (Fun_left {name=arg.name; body})  :: path in
      mt pth ~loc ~state arg.signature >>= fun signature ->
      let r = {arg with signature} in
      let new_state = F.state_bind_arg state r in
      let arg = Some {r; state} in
      me (Me (Fun_right arg) :: path ) ~loc ~state:new_state body >>|
      F.me_fun (Some r)
    | Constraint (mex,mty) ->
      me (Me (Constraint_left mty)::path) ~loc ~state  mex >>= fun me ->
      mt (Me (Constraint_right me)::path) ~loc ~state mty >>|
      F.me_constraint me
    | Str items ->
      m2l_start (Me Str :: path) ~pkg:(apkg loc) ~state F.m2l_init items
      >>| F.str
    | Val v -> minor (Me Val::path) ~pkg:(apkg loc) ~state v >>| F.me_val
    | Extension_node {name;extension=e} ->
      ext ~pkg:(apkg loc) ~state (Me (Extension_node name) :: path) e >>| F.me_ext ~loc name
    | Abstract -> Ok F.abstract
    | Unpacked -> Ok F.unpacked
    | Open_me {opens; expr; _ } ->
      open_all path expr F.open_init ~loc ~state opens >>=
      open_right path ~loc ~state expr
  and open_right path expr ~state ~loc r =
    me (Me (Open_me_right {state;opens=r.r}) :: path) ~loc ~state:r.state expr >>|
    F.open_me r.r
  and open_all_rec path expr left ~loc ~initial_state ~state = function
    | L.[] -> Ok {state; r=left}
    | a :: right ->
      let diff = state_diff initial_state in
      let path' = Me (Open_me_left {left; right; diff; expr}) :: path in
      resolve path' ~state ~loc ~level:Module a >>= fun a ->
      let state = F.state_open_path ~loc state a in
      open_all path expr (F.open_add a left) ~loc ~state right
  and open_all path expr left ~state ~loc =
    open_all_rec path expr left ~loc ~initial_state:state ~state
  and mt path ~loc ~state = function
    | Resolved _ -> assert false (* to be removed *)
    | Alias id ->
      resolve (Mt Alias :: path) ~loc ~state ~level:Module_type id
      >>| F.alias
    | Ident ids ->
      path_expr (Mt Ident :: path) ~loc ~state ids >>| F.mt_ident
    | Sig items ->
      m2l_start (Mt Sig :: path) ~pkg:(apkg loc) ~state F.m2l_init items
      >>| F.mt_sig
    | Fun {arg = None; body } ->
      mt (Mt (Fun_right None)::path) ~loc ~state body
      >>| F.mt_fun None
    | Fun {arg = Some arg; body } ->
      let arg_path = Mt(Fun_left {name=arg.name; body} )::path in
      mt arg_path ~loc ~state arg.signature >>= fun signature ->
      let r = { arg with signature } in
      let state = F.state_bind_arg state r in
      let arg = Some {state; r} in
      mt (Mt(Fun_right arg)::path) ~state ~loc body >>|
      F.mt_fun (Some r)
    | With {body;deletions;access=a} ->
      let access_path = Mt (With_access {body;deletions})::path in
      access ~pkg:(apkg loc) ~state access_path a >>= fun access ->
      mt (Mt (With_body {access;deletions})::path) ~loc ~state body >>|
      F.mt_with access deletions
    | Of m -> me (Mt Of :: path) ~loc ~state m >>| F.mt_of
    | Extension_node {name;extension=e} ->
      ext ~pkg:(apkg loc) ~state (Mt (Extension_node name)::path)  e
      >>| F.mt_ext ~loc name
    | Abstract -> Ok F.sig_abstract
  and bind_rec path left ~loc ~state = function
    | L.[] -> Ok left
    | {name;expr} :: right ->
      me (Expr(Bind_rec{left;name;right}) :: path) ~loc ~state expr
      >>= fun me ->
      let more = F.bind_rec_add name me left in
      let state = update_state state more in
      bind_rec path more.r ~loc ~state right
  and path_expr_args path ~loc ~state main left = function
    | L.[] -> Ok (F.path_expr main left)
    | (n, arg) :: right ->
      let path_arg = Path_expr (Arg {main;left;pos=n;right})::path in
      path_expr path_arg arg ~loc ~state >>= fun x ->
      path_expr_args path main ~loc ~state
        (F.path_expr_arg n x left) right
  and path_expr ctx ~loc ~state {Paths.Expr.path; args} =
    resolve ~level:Module_type ~loc ~state
      (Path_expr (Main args) :: ctx) path >>= fun main ->
    path_expr_args ctx main F.path_expr_arg_init ~loc ~state args
  and minor path ~pkg ~state mn =
    let i = F.packed_init in
    packed path mn.access mn.values i  ~pkg ~state mn.packed >>= fun packed ->
    let fpath = Annot (Access {packed; values=mn.values}) :: path in
    access fpath mn.access ~pkg ~state >>= fun access ->
    values path packed access ~pkg ~state mn.values
  and values path packed access ~pkg ~state values =
    m2ls path packed access ~state ~pkg F.value_init values >>|
    F.annot packed access
  and m2ls path packed access ~pkg ~state left = function
    | L.[] -> Ok (F.values left)
    | a :: right ->
      let fpath = Annot (Values {packed;access; left;right}) :: path in
      m2l_start fpath F.m2l_init ~pkg ~state a >>= fun a ->
      m2ls path packed access (F.value_add a left) ~pkg ~state right
  and packed path access values left ~pkg ~state = function
    | L.[] -> Ok left
    | (a: _ Loc.ext) :: right ->
      let loc = pkg, a.loc in
      let fpath =
        Annot (Packed { access; values; left ; loc; right})::path in
      me fpath a.data ~state ~loc  >>= fun m ->
      packed path access values ~pkg ~state
        (F.add_packed loc m left) right
  and access path ~pkg ~state s =
    access_step ~state ~pkg path F.access_init (Paths.S.Map.bindings s)
  and access_step path left ~pkg ~state = function
    | [] -> Ok (F.access left)
    | (a, (loc,edge)) :: right ->
      let loc = pkg, loc in
      resolve ~state ~loc ~edge ~level:Module
        (Access {left;right}::path) a >>= fun a ->
      access_step path ~pkg ~state (F.access_add a loc edge left) right
  and ext path ~pkg ~state = function
    | Module m -> m2l_start ~pkg ~state (Ext Mod :: path) F.m2l_init m
      >>| F.ext_module
    | Val v -> minor ~state ~pkg (Ext Val :: path) v >>| F.ext_val
  and resolve path ~loc ~level ~state ?edge s =
    let px = { edge; level; loc; ctx=state_diff state; path = s } in
    match F.resolve state px with
    | Error () -> Error ( { path; focus= px })
    | Ok _ as x -> x

  open M2l
  let rec restart state z =
    let v = z.focus in
    let loc = v.loc in
    match F.resolve state v with
    | Error _ -> Error z
    | Ok x -> self_restart state @@ match z.path with
      | Me Ident :: rest ->
        restart_me ~state ~loc (rest: module_expr path) (F.me_ident x)
      | Me (Open_me_left {left;right;diff;expr}) :: path ->
        open_all ~state ~loc path expr (F.open_add x left) right >>= fun r ->
        open_right path expr ~loc ~state r >>= fun me ->
        let state = state_restart state diff in
        restart_me ~state ~loc (path:module_expr path) me
      | Access a :: rest ->
        access_step rest ~pkg:(apkg loc) ~state (F.access_add x v.loc (default_edge v.edge) a.left) a.right
        >>= restart_access ~loc ~state (rest: waccess path)
      | Mt Alias :: path ->
        restart_mt ~loc ~state (path: module_type path) (F.alias x)
      | Path_expr Main args :: path ->
        path_expr_args ~state ~loc path x F.path_expr_arg_init args >>=
        restart_path_expr ~loc ~state (path: Paths.Expr.t path)
      | _ -> .
  and self_restart state = function
    | Error z -> restart state z
    | Ok _ as x -> x
  and restart_me: state:_ -> loc:_ -> module_expr path -> _ = fun ~state ~loc path x -> match path with
    | Expr Include :: rest ->
      restart_expr ~state rest (F.expr_include ~loc x)
    | Expr Open :: rest ->
      restart_expr ~state rest (F.expr_open ~loc x)
    | Annot (Packed p) :: path ->
      let pkg = apkg loc in
      let left = F.add_packed p.loc x p.left in
      packed path p.access p.values left ~pkg ~state p.right  >>= fun packed ->
      let fpath = Annot (Access {packed; values=p.values}) :: path in
      access fpath ~pkg ~state p.access >>= fun access ->
      values path packed access ~pkg ~state p.values >>=
      restart_annot ~loc ~state path
    | Me (Apply_left xx) :: path ->
      me (Me (Apply_right x)::path) ~state ~loc xx
      >>| F.apply loc x
      >>= restart_me ~loc ~state path
    | Mt Of :: path -> restart_mt ~loc ~state path (F.mt_of x)
    | Me(Apply_right fn) :: path -> restart_me path ~loc ~state (F.apply loc fn x)
    | Me(Fun_right None) :: path ->
      restart_me path ~state ~loc (F.me_fun None x)
    | Me(Fun_right Some {r;state}) :: path ->
      restart_me path ~state ~loc (F.me_fun (Some r) x)
    | Me (Constraint_left mty) :: path ->
      mt (Me (Constraint_right x)::path) ~loc ~state mty >>= fun mt ->
      restart_me path ~loc ~state (F.me_constraint x mt)
    | Me (Open_me_right {opens;state}) :: path ->
      restart_me path ~loc ~state (F.open_me opens x)
    | Expr (Bind name) :: path ->
      restart_expr path ~state (F.bind name x)
    | Expr (Bind_rec {left;name;right}) :: path  ->
      let {r=left; state=more} = F.bind_rec_add name x left in
      let state = F.state_merge state more in
      bind_rec path left ~loc ~state right >>| F.bind_rec >>=
      restart_expr ~state path
    | _ -> .
  and restart_expr: expression path -> state:_ -> _ with_state -> _ =
    fun path ~state x ->
    match path with
    | M2l {left;loc;right; state=restart } :: path ->
      let state  = F.state_merge state x.state in
      m2l path (F.m2l_add loc x left) ~pkg:(apkg loc) ~initial_state:restart ~state right >>=
      restart_m2l ~loc ~state:restart (path: m2l path)
    | _ -> .
  and restart_mt: module_type path -> _ = fun path ~state ~loc x ->
    match path with
    | Expr (Bind_sig name) :: path ->
      restart_expr ~state path (F.bind_sig name x)
    | Me Fun_left {name;body} :: path ->
      let r =  {Arg.signature = x; name } in
      me (Me(Fun_right (Some {r;state})) :: path ) ~loc ~state body >>|
      F.me_fun (Some r) >>= restart_me path ~loc ~state
    | Mt Fun_left {name;body} :: path ->
      let r = {Arg.signature = x; name } in
      let inner_state = F.state_bind_arg state r in
      mt (Mt(Fun_right(Some {r;state})) :: path ) ~loc ~state:inner_state body >>|
     F.mt_fun (Some r) >>= restart_mt ~loc ~state path
    | Mt Fun_right (Some {r=arg;state}) :: path ->
      restart_mt ~loc ~state path (F.mt_fun (Some arg) x)
    | Mt Fun_right None :: path ->
      restart_mt ~loc ~state path (F.mt_fun None x)
    | Mt With_body {access;deletions} :: path ->
      restart_mt ~loc ~state path (F.mt_with access deletions x)
    | Me Constraint_right body :: path ->
      restart_me ~loc ~state path (F.me_constraint body x)
    | Expr SigInclude :: path ->
      restart_expr ~state path (F.sig_include loc x)
    | _ -> .
  and restart_path_expr: loc:_ -> state:_ -> Paths.Expr.t path -> _ =
    fun ~loc ~state path x -> match path with
    | Mt Ident :: path ->
      restart_mt (path: module_type path) ~state ~loc  (F.mt_ident x)
    | Path_expr Arg {main;left;pos;right} :: path ->
      let left = F.path_expr_arg pos x left in
      path_expr_args path main left right ~loc ~state >>=
      restart_path_expr ~loc ~state path
    | _ -> .
  and restart_access ~loc ~state  path x = match path with
    | Annot (Access mn) :: path ->
      values ~pkg:(apkg loc) ~state path mn.packed x mn.values >>=
      restart_annot ~loc ~state path
    | Mt With_access {deletions;body} :: path ->
      mt ~loc ~state (Mt(With_body {deletions;access=x}) :: path ) body
      >>| F.mt_with x deletions
      >>= restart_mt ~loc ~state path
    | _ -> .
  and restart_annot: annotation path -> _ =
    fun path ~loc ~state x -> match path with
    | Expr Minor :: path -> restart_expr ~state path (F.minor x)
    | Me Val :: path -> restart_me ~loc ~state path (F.me_val x)
    | Ext Val :: path -> restart_ext path ~loc ~state (F.ext_val x)
    | _ -> .
  and restart_ext: extension_core path -> _ =
    fun path ~loc ~state x -> match path with
    | Expr (Extension_node name) :: path ->
      restart_expr ~state path (F.expr_ext name x)
    | Me (Extension_node name) :: path ->
      restart_me path ~loc ~state (F.me_ext ~loc name x)
    | Mt (Extension_node name) :: path ->
      restart_mt path ~loc ~state (F.mt_ext ~loc name x)
    | _ -> .
  and restart_m2l ~loc ~state path x = match path with
    | [] -> Ok x
    | Me Str :: path -> restart_me ~loc ~state path (F.str x)
    | Mt Sig :: path -> restart_mt ~loc ~state path (F.mt_sig x)
    | Annot (Values {packed;access;left;right}) :: path ->
      let left = F.value_add x left in
      m2ls path packed access left ~pkg:(apkg loc) ~state right
      >>| F.annot packed access
      >>= restart_annot ~loc ~state path
    | Ext Mod :: path -> restart_ext ~loc ~state path (F.ext_module x)
    | _ :: _ -> .
  let ustart x =m2l_start [] F.m2l_init x


  let restart state z =
    let state = state_restart state z.focus.ctx in
    restart state z
end
