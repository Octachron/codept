[@@@warning "-37"]

module L = struct
  type 'a t = 'a list = [] | (::) of 'a * 'a t
end

module Arg = Module.Arg

type a = Paths.Simple.t * (Loc.t * Deps.Edge.t)
type level = Module.level = Module | Module_type

type ('a,'b) pair = { backbone:'a; user:'b }

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

  val path : M2l_skel.query -> path
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

  val bind : Name.t -> module_expr -> expr
  val bind_alias: Name.t -> Paths.S.t -> expr
  val bind_rec : bind_rec -> expr
  val bind_rec_add :
    string -> module_expr -> bind_rec -> bind_rec
  val bind_rec_init : bind_rec
  val bind_sig : string -> module_type -> expr

  val expr_ext : string -> ext -> expr
  val expr_include : loc:Fault.loc -> module_expr -> expr
  val expr_open : loc:Fault.loc -> module_expr -> expr
  val ext_module : m2l -> ext
  val ext_val : annotation -> ext

  val m2l_add : Fault.loc -> expr -> m2l -> m2l
  val m2l_init : m2l
  val m2l: m2l  -> m2l

  val me_constraint : module_expr -> module_type -> module_expr
  val me_ext : loc:Fault.loc -> string -> ext -> module_expr
  val me_fun :
    module_type Arg.t option -> module_expr -> module_expr
  val me_ident : path -> module_expr
  val me_val : annotation -> module_expr
  val minor : annotation -> expr

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
  val sig_include : loc:Fault.loc -> module_type -> expr
  val str : m2l -> module_expr
  val unpacked : module_expr

  val value_add : m2l -> values -> values
  val value_init : values
  val values : values -> values

end

let apkg (f,_) = f
module Ok = Mresult.Ok

let ((>>=), (>>|)) = Ok.((>>=), (>>|))

module Make(F:fold)(Env:Outliner.envt) = struct

  module Sk=M2l_skel.Make(Env)
  type path = (M2l_skel.path, F.path) pair
  type module_expr = (Sk.module_like, F.module_expr) pair
  type access = F.access
  type packed = F.packed
  type module_type = (Sk.module_like, F.module_type) pair
  type m2l = (Sk.m2l, F.m2l) pair
  type values = F.values
  type bind_rec = (Sk.state_diff, F.bind_rec) pair
  type path_expr_args = F.path_expr_args
  type opens = F.opens

  type path_in_context = Sk.path_in_context

  (* Unused types
  type expr = (Sk.state_diff, F.expr) pair
  type path_expr = F.path_expr
  type ext = F.ext
  type annotation = F.annotation
  *)

  module Path = struct

    type waccess = W of access [@@unboxed]

    type 'focus expr =
      | Open: M2l.module_expr expr
      | Include:  M2l.module_expr expr
      | SigInclude:  M2l.module_type expr
      | Bind: Name.t ->  M2l.module_expr expr
      | Bind_sig: Name.t -> M2l.module_type expr
      | Bind_rec_sig:
          {
            diff: Sk.state_diff;
            left: (Name.t * F.module_type * M2l.module_expr) list;
            name: Name.t;
            expr: M2l.module_expr;
            right: M2l.module_expr M2l.bind list
          } -> M2l.module_type expr
      | Bind_rec:
          {
            left: bind_rec;
            name:Name.t;
            mt: F.module_type;
            right: (Name.t * F.module_type * M2l.module_expr) list;
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
          (module_type Arg.t * Sk.state_diff ) option -> M2l.module_expr me
      | Constraint_left: M2l.module_type -> M2l.module_expr me
      | Constraint_right: module_expr -> M2l.module_type me
      | Str: M2l.m2l me
      | Val: M2l.annotation me
      | Extension_node: string -> M2l.extension_core me
      | Open_me_left:
          { left: opens;
            right:Paths.S.t list;
            diff:Sk.state_diff;
            expr:M2l.module_expr
          } -> path_in_context me
      | Open_me_right:
          {opens:opens; state:Sk.state_diff} -> M2l.module_expr me

    type 'focus mt =
      | Alias: path_in_context mt
      | Ident: Paths.Expr.t mt
      | Sig: M2l.m2l mt
      | Fun_left: {name:string; body:M2l.module_type} -> M2l.module_type mt
      | Fun_right: (module_type Arg.t * Sk.state_diff) option
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
              state:Sk.state_diff;
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

  module D = struct
    module Helpers = struct
      let const backbone user = {backbone; user}
      let fork f g x = { backbone = f x; user = g x }
      let fork2 f g x y = { backbone = f x y; user = g x y }
      let both f g x = { backbone = f x.backbone; user = g x.user }
      let both2 f g x y =
        { backbone = f x.backbone y.backbone; user = g x.user y.user }
      let user f x = { x with user = f x.user }
      let user_ml f x = { backbone = Sk.empty_diff; user = f x }
      let user_me f x = { backbone = Sk.empty; user = f x }
    end open Helpers

    let path = fork Sk.path F.path
    let arg name signature = {Arg.name; signature}
    let mk_arg var name = both2 (fun s -> Sk.fn (Some(arg name s)))
        (fun s -> var (Some(arg name s)))
    let m2l = user F.m2l
    let m2l_add loc = both2 Sk.m2l_add (F.m2l_add loc)
    let expr_open param loc = both  (Sk.opened param ~loc) (F.expr_open ~loc)
    let gen_include var param loc = both (Sk.included param loc) (var ~loc)
    let expr_include = gen_include F.expr_include
    let sig_include = gen_include F.sig_include
    let bind_alias state = fork2 (Sk.State.bind_alias state) F.bind_alias
    let bind name = both (Sk.bind name) (F.bind name)
    let bind_sig name = both (Sk.bind_sig name) (F.bind_sig name)
    let minor = user_ml F.minor
    let expr_ext name = user_ml (F.expr_ext name)
    let me_ident = both Sk.ident F.me_ident
    let apply param loc f =
      both2 (fun f _ -> Sk.apply param loc f) (F.apply loc) f
    let me_fun_none =  both (Sk.fn None) (F.me_fun None)
    let mt_fun_none =  both (Sk.fn None) (F.mt_fun None)
    let me_constraint me = user (F.me_constraint me.user)
    let str = both Sk.str F.str
    let me_val x = const Sk.unpacked (F.me_val x)
    let me_ext loc name = user_me (F.me_ext ~loc name)
    let abstract = const Sk.abstract F.abstract
    let unpacked = const Sk.unpacked F.unpacked
    let open_me opens = user (F.open_me opens)
    let alias = both Sk.ident F.alias
    let mt_ident = both Sk.ident F.mt_ident
    let mt_sig = both Sk.str F.mt_sig
    let mt_with access deletions =
      both (Sk.m_with deletions) (F.mt_with access deletions)
    let mt_of = user F.mt_of
    let mt_ext loc name = user_me (F.mt_ext ~loc name)
    let sig_abstract = const Sk.abstract F.sig_abstract
    let init_rec diff = const diff F.bind_rec_init
    let bind_rec = user F.bind_rec
    let bind_rec_add name me mt =
      user (F.bind_rec_add name (F.me_constraint me.user mt))
    let path_expr left = user (fun y -> F.path_expr y left)
  end

  let dual_resolve ~param ~state ~path px =
    match Sk.resolve param state px with
    | Error () -> Error ( { path; focus= px })
    | Ok x -> Ok (D.path x)

  let default_edge = Option.default Deps.Edge.Normal


  let fn_gen sel wrap k ~param ~loc ~state ~path name body signature =
    let state = Sk.State.bind_arg state {name; signature=signature.backbone } in
    let arg = Some ({ Arg.name; signature }, Sk.State.diff state) in
    k (wrap arg :: path) ~param ~loc ~state body >>| D.mk_arg sel name signature
  let fn_me = fn_gen F.me_fun (fun x -> Me (Fun_right x))
  let fn_mt = fn_gen F.mt_fun (fun x -> Mt (Fun_right x))

  let rec m2l ~param path left ~pkg ~state = function
    | L.[] -> Ok (D.m2l left)
    | {Loc.loc; data=a} :: right ->
      let loc = pkg, loc in
      expr ~param ~loc ~state
        (M2l {left;loc;state=Sk.State.diff state;right} :: path)  a
      >>= fun r ->
      let state = Sk.State.merge state r.backbone in
      let left = D.m2l_add loc r left in
      m2l ~param path left  ~pkg ~state right
  and m2l_start ~param path ~pkg ~state =
    m2l ~param path {backbone=Sk.m2l_init; user=F.m2l_init} ~pkg ~state
  and expr ~param path ~loc ~state expr = match expr with
    | Defs _ -> assert false (* to be removed *)
    | Open m -> me ~param (Expr Open::path) ~loc ~state m
      >>| D.expr_open param loc
    | Include m ->
      me  ~param (Expr Include :: path) ~loc ~state m
      >>| D.expr_include param loc
    | SigInclude m ->
      mt (Expr SigInclude :: path) ~param ~loc ~state m
      >>| D.sig_include param loc
    | Bind {name; expr=(Ident s|Constraint(Abstract, Alias s))}
      when Sk.State.is_alias param state s -> Ok (D.bind_alias state name s)
    | Bind {name; expr} ->
      me ~param (Expr (Bind name) :: path) ~loc ~state expr
      >>| D.bind name
    | Bind_sig {name; expr} ->
      mt (Expr (Bind_sig name) :: path) ~param ~loc ~state expr
      >>| D.bind_sig name
    | Bind_rec l ->
      let state = Sk.State.rec_approximate state l in
      bind_rec_sig path Sk.bind_rec_init L.[] ~param ~loc ~state l
    | Minor m ->
      minor (Expr Minor :: path) ~param ~pkg:(apkg loc) ~state m
      >>| D.minor
    | Extension_node {name;extension} ->
      ext ~pkg:(apkg loc) ~param ~state (Expr (Extension_node name) :: path)
        extension >>| D.expr_ext name
  and me path  ~param ~loc ~state = function
    | Resolved _ -> assert false (* to be removed *)
    | Ident s ->
      resolve (Me Ident :: path) ~param ~loc ~state ~level:Module s
      >>| D.me_ident
    | Apply {f; x} ->
      me (Me (Apply_left x)::path) ~param ~loc ~state f >>= fun f ->
      me (Me (Apply_right f)::path)  ~param ~loc ~state x >>|
      D.apply param loc f
    | Fun {arg = None; body } ->
      me (Me (Fun_right None) :: path)  ~param ~loc ~state body
      >>| D.me_fun_none
    | Fun {arg = Some {name;signature} ; body } ->
      let pth = Me (Fun_left {name; body})  :: path in
      mt pth ~param ~loc ~state signature >>=
      fn_me me ~path ~param ~loc ~state name body
    | Constraint (mex,mty) ->
      me (Me (Constraint_left mty)::path) ~loc ~state ~param  mex >>= fun me ->
      mt (Me (Constraint_right me)::path) ~loc ~param ~state mty >>|
      D.me_constraint me
    | Str items ->
      m2l_start (Me Str :: path) ~param ~pkg:(apkg loc) ~state items
      >>| D.str
    | Val v -> minor (Me Val::path) ~param ~pkg:(apkg loc) ~state v
      >>| D.me_val
    | Extension_node {name;extension=e} ->
      ext ~param ~pkg:(apkg loc) ~state (Me (Extension_node name) :: path) e >>|
      D.me_ext loc name
    | Abstract -> Ok D.abstract
    | Unpacked -> Ok D.unpacked
    | Open_me {opens; expr; _ } ->
      open_all path expr F.open_init ~loc ~param ~state opens
  and open_right path expr ~param ~loc ~state opens =
    me (Me (Open_me_right {state=Sk.State.diff state;opens}) :: path)
      ~param ~loc ~state expr >>| D.open_me opens
  and open_all_rec path expr left ~loc ~param ~diff ~state = function
    | L.[] -> open_right path expr ~param ~loc ~state left
    | a :: right ->
      let path' = Me (Open_me_left {left; right; diff; expr}) :: path in
      resolve path' ~param ~state ~loc ~level:Module a >>= fun a ->
      let state = Sk.State.open_path ~param ~loc state a.backbone in
      open_all_rec path expr (F.open_add a.user left) ~param ~loc ~diff
        ~state right
  and open_all path expr left ~state ~loc ~param =
    open_all_rec path expr left ~param ~loc ~diff:(Sk.State.diff state) ~state
  and mt path ~param ~loc ~state = function
    | Resolved _ -> assert false (* to be removed *)
    | Alias id ->
      resolve (Mt Alias :: path) ~param ~loc ~state ~level:Module id
      >>| D.alias
    | Ident ids ->
      path_expr ~level:Module_type (Mt Ident :: path) ~param ~loc ~state ids
      >>| D.mt_ident
    | Sig items ->
      m2l_start (Mt Sig :: path) ~pkg:(apkg loc) ~state ~param items
      >>| D.mt_sig
    | Fun {arg = None; body } ->
      mt (Mt (Fun_right None)::path) ~loc ~state ~param body
      >>| D.mt_fun_none
    | Fun {arg = Some {Arg.name;signature}; body } ->
      let arg_path = Mt(Fun_left {name; body} )::path in
      mt arg_path ~loc ~param ~state signature >>=
      fn_mt mt ~path ~param ~state ~loc name body
    | With {body;deletions;access=a} ->
      let access_path = Mt (With_access {body;deletions})::path in
      access ~param ~pkg:(apkg loc) ~state access_path a >>= fun access ->
      mt (Mt (With_body {access;deletions})::path) ~param ~loc ~state body
      >>| D.mt_with access deletions
    | Of m -> me (Mt Of :: path) ~param ~loc ~state m >>| D.mt_of
    | Extension_node {name;extension=e} -> Sk.ext param loc name;
      ext ~pkg:(apkg loc) ~param ~state (Mt (Extension_node name)::path)  e
      >>| D.mt_ext loc name
    | Abstract -> Ok D.sig_abstract
  and bind_rec_sig path diff left ~param ~loc ~state = function
    | L.[] ->
      let state = Sk.State.merge state diff in
      bind_rec path (D.init_rec diff) ~param ~loc ~state (List.rev left)
    | {M2l.name; expr=M2l.Constraint(me,ty)} :: right ->
      mt (Expr(Bind_rec_sig{diff;left;name;expr=me; right}) :: path) ~loc ~param ~state ty
      >>= fun mt ->
      let diff = Sk.bind_rec_add name mt.backbone diff in
      bind_rec_sig path diff L.((name, mt.user, me)::left) ~loc ~state ~param right
    | {M2l.name; expr} :: right ->
      bind_rec_sig path diff left ~param ~loc ~state
        ({M2l.name; expr=M2l.(Constraint(expr,Abstract))}::right)
  and bind_rec path left ~param ~loc ~state = function
    | L.[] -> Ok (D.bind_rec left)
    | (name,mt,mex) :: right ->
      me (Expr(Bind_rec{left;name;mt;right}) :: path) ~loc ~param ~state mex
      >>= fun me ->
      let left = D.bind_rec_add name me mt left in
      bind_rec path left ~loc ~state ~param right
  and path_expr_args path ~loc ~state ~param main left = function
    | L.[] -> Ok (D.path_expr left main)
    | (n, arg) :: right ->
      let path_arg = Path_expr (Arg {main;left;pos=n;right})::path in
      path_expr ~level:Module path_arg arg ~loc ~state ~param >>= fun x ->
      path_expr_args path main ~loc ~state ~param
        (F.path_expr_arg n x.user left) right
  and path_expr ~level ctx ~loc ~param ~state {Paths.Expr.path; args} =
    resolve ~level ~loc ~state ~param
      (Path_expr (Main args) :: ctx) path >>= fun main ->
    path_expr_args ctx main F.path_expr_arg_init ~param ~loc ~state args
  and minor path ~pkg ~param ~state mn =
    let i = F.packed_init in
    packed path mn.access mn.values i  ~param ~pkg ~state mn.packed
  and values path packed ~param ~pkg ~state values access =
    m2ls path packed access ~param ~state ~pkg F.value_init values >>|
    F.annot packed access
  and m2ls path packed access ~param ~pkg ~state left = function
    | L.[] -> Ok (F.values left)
    | a :: right ->
      let fpath = Annot (Values {packed;access; left;right}) :: path in
      m2l_start fpath ~param ~pkg ~state a >>= fun a ->
      m2ls path packed access (F.value_add a.user left)
        ~param ~pkg ~state right
  and packed path accs vals left ~param ~pkg ~state = function
    | L.[] ->
      let fpath = Annot (Access {packed=left; values=vals}) :: path in
      access fpath ~pkg ~param ~state accs >>=
      values path left ~param ~pkg ~state vals
    | (a: _ Loc.ext) :: right ->
      let loc = pkg, a.loc in
      let fpath =
        Annot (Packed { access=accs; values=vals; left ; loc; right})::path in
      me fpath a.data ~param ~state ~loc >>= fun m ->
      packed path accs vals ~param ~pkg ~state (F.add_packed loc m.user left)
        right
  and access path ~pkg ~param ~state s =
    access_step ~state ~pkg ~param path F.access_init (Paths.S.Map.bindings s)
  and access_step path left ~param ~pkg ~state = function
    | [] -> Ok (F.access left)
    | (a, (loc,edge)) :: right ->
      let loc = pkg, loc in
      resolve ~param ~state ~loc ~edge ~level:Module
        (Access {left;right}::path) a >>= fun a ->
      access_step path ~param ~pkg ~state (F.access_add a.user loc edge left) right
  and ext path ~param ~pkg ~state = function
    | Module m -> m2l_start ~param ~pkg ~state (Ext Mod :: path) m
      >>| fun m -> F.ext_module m.user
    | Val v -> minor ~state ~pkg ~param (Ext Val :: path) v >>| F.ext_val
  and resolve path ~param ~loc ~level ~state ?edge s =
    let px = { Sk.edge; level; loc; ctx=Sk.State.diff state; path = s } in
    dual_resolve ~param ~state ~path px

  open M2l
  let rec restart ~param state z =
    let v = z.focus in
    let loc = v.Sk.loc in
    match dual_resolve ~param ~state ~path:z.path v with
    | Error _ -> Error z
    | Ok x -> match z.path with
      | Me Ident :: rest ->
        restart_me ~param ~state ~loc rest (D.me_ident x)
      | Me Open_me_left {left;right;diff;expr} :: path ->
        let state = Sk.State.open_path ~param ~loc state x.backbone in
        open_all ~state ~param ~loc path expr (F.open_add x.user left) right >>=
        restart_me ~param ~state:(Sk.State.restart state diff) ~loc path
      | Access a :: rest ->
        let r = F.access_add x.user v.loc (default_edge v.edge) a.left in
        access_step rest ~pkg:(apkg loc) ~param ~state r a.right
        >>= restart_access ~param ~loc ~state (rest: waccess path)
      | Mt Alias :: path ->
        restart_mt ~param ~loc ~state path (D.alias x)
      | Path_expr Main args :: path ->
        path_expr_args ~param ~state ~loc path x F.path_expr_arg_init args >>=
        restart_path_expr ~param ~loc ~state path
      | _ -> .
  and restart_me: module_expr path -> _ = fun path ~state ~loc ~param x -> match path with
    | Expr Include :: rest ->
      restart_expr ~state ~param rest (D.expr_include param loc x)
    | Expr Open :: rest ->
      restart_expr ~param ~state rest (D.expr_open param loc x)
    | Annot (Packed p) :: path ->
      let pkg = apkg loc in
      let left = F.add_packed p.loc x.user p.left in
      packed path p.access p.values left ~param ~pkg ~state p.right >>=
      restart_annot ~loc ~param ~state path
    | Me (Apply_left xx) :: path ->
      me (Me (Apply_right x)::path) ~param ~state ~loc xx
      >>| D.apply param loc x
      >>= restart_me ~loc ~state ~param path
    | Mt Of :: path -> restart_mt ~loc ~state ~param path (D.mt_of x)
    | Me(Apply_right fn) :: path ->
      restart_me path ~loc ~param ~state (D.apply param loc fn x)
    | Me(Fun_right None) :: path ->
      restart_me path ~state ~loc ~param (D.me_fun_none x)
    | Me(Fun_right Some (r,diff)) :: path ->
      let state = Sk.State.restart state diff in
      restart_me path ~state ~loc ~param (D.mk_arg F.me_fun r.name r.signature x)
    | Me (Constraint_left mty) :: path ->
      mt (Me (Constraint_right x)::path) ~loc ~param ~state mty >>= fun mt ->
      restart_me path ~loc ~state ~param (D.me_constraint x mt)
    | Me (Open_me_right {opens;state=diff}) :: path ->
      let state = Sk.State.restart state diff in
      restart_me path ~loc ~state ~param (D.open_me opens x)
    | Expr (Bind name) :: path ->
      restart_expr path ~state ~param (D.bind name x)
    | Expr (Bind_rec {left;name;mt;right}) :: path  ->
      let left = D.bind_rec_add name x mt left in
      bind_rec path left ~loc ~param ~state right >>=
      restart_expr ~state ~param path
    | _ -> .
  and restart_expr: expression path -> _ =
    fun path ~state ~param x ->
    match path with
    | M2l {left;loc;right; state=restart } :: path ->
      let state = Sk.State.merge state x.backbone in
      let left = D.m2l_add loc x left in
      m2l path left ~pkg:(apkg loc) ~param ~state right >>=
      restart_m2l ~param ~loc ~state:(Sk.State.restart state restart) path
    | _ -> .
  and restart_mt: module_type path -> _ = fun path ~state ~param ~loc x ->
    match path with
    | Expr (Bind_sig name) :: path ->
      restart_expr ~state ~param path (D.bind_sig name x)
    | Me Fun_left {name;body} :: path ->
      fn_me me ~path ~param ~loc ~state name body x
      >>= restart_me path ~loc ~param ~state
    | Mt Fun_left {name;body} :: path ->
      fn_mt mt ~path ~loc ~state ~param name body x
      >>= restart_mt ~loc ~param ~state path
    | Mt Fun_right (Some (arg,diff)) :: path ->
      let state = Sk.State.restart state diff in
      restart_mt ~loc ~state ~param path (D.mk_arg F.mt_fun arg.name arg.signature x)
    | Mt Fun_right None :: path ->
      restart_mt ~loc ~param ~state path (D.mt_fun_none x)
    | Mt With_body {access;deletions} :: path ->
      restart_mt ~loc ~param ~state path (D.mt_with access deletions x)
    | Me Constraint_right body :: path ->
      restart_me ~loc ~param ~state path (D.me_constraint body x)
    | Expr SigInclude :: path ->
      restart_expr ~state ~param path (D.sig_include param loc x)
    | Expr Bind_rec_sig {diff; left; name; expr; right} :: path ->
      bind_rec_sig path (Sk.bind_rec_add name x.backbone diff)
        ((name, x.user, expr) :: left) ~param ~loc ~state right >>=
      restart_expr ~state ~param path
    | _ -> .
  and restart_path_expr: Paths.Expr.t path -> _ =
    fun path ~loc ~param ~state x -> match path with
    | Mt Ident :: path ->
      restart_mt path ~param ~state ~loc (D.mt_ident x)
    | Path_expr Arg {main;left;pos;right} :: path ->
      let left = F.path_expr_arg pos x.user left in
      path_expr_args path main left right ~loc ~param ~state >>=
      restart_path_expr ~loc ~param ~state path
    | _ -> .
  and restart_access ~loc ~param ~state  path x = match path with
    | Annot (Access mn) :: path ->
      values ~pkg:(apkg loc) ~param ~state path mn.packed mn.values x >>=
      restart_annot ~loc ~state ~param path
    | Mt With_access {deletions;body} :: path ->
      mt ~loc ~state ~param (Mt(With_body {deletions;access=x}) :: path ) body
      >>| D.mt_with x deletions
      >>= restart_mt ~loc ~state ~param path
    | _ -> .
  and restart_annot: annotation path -> _ =
    fun path ~loc ~param ~state x -> match path with
    | Expr Minor :: path -> restart_expr ~param ~state path (D.minor x)
    | Me Val :: path -> restart_me ~loc ~param ~state path (D.me_val x)
    | Ext Val :: path -> restart_ext path ~param ~loc ~state (F.ext_val x)
    | _ -> .
  and restart_ext: extension_core path -> _ =
    fun path ~loc ~param ~state x -> match path with
    | Expr (Extension_node name) :: path ->
      restart_expr ~state ~param path (D.expr_ext name x)
    | Me (Extension_node name) :: path ->
      restart_me path ~loc ~param ~state (D.me_ext loc name x)
    | Mt (Extension_node name) :: path ->
      restart_mt path ~loc ~param ~state (D.mt_ext loc name x)
    | _ -> .
  and restart_m2l: m2l path -> _ = fun path ~loc ~param ~state x ->
    match path with
    | [] -> Ok x
    | Me Str :: path ->
      restart_me ~loc ~param ~state path (D.str x)
    | Mt Sig :: path ->
      restart_mt ~loc ~param ~state path (D.mt_sig x)
    | Annot (Values {packed;access;left;right}) :: path ->
      let left = F.value_add x.user left in
      m2ls path packed access left ~pkg:(apkg loc) ~param ~state right
      >>| F.annot packed access
      >>= restart_annot ~loc ~state ~param path
    | Ext Mod :: path -> restart_ext ~loc ~param ~state path (F.ext_module x.user)
    | _ :: _ -> .

  let unpack x = Sk.final x.backbone, x.user

  type on_going =
    | On_going of path_in_context zipper
    | Initial of M2l.t
 let initial x = Initial x

  let start ~pkg param env x =
    m2l_start ~pkg ~state:(Sk.State.from_env env) ~param [] x >>|
    unpack

  let restart param env z =
    let state = Sk.State.from_env ~diff:z.focus.Sk.ctx env in
    restart ~param state z >>| unpack

  let next ~pkg param env x =
    let r = match x with
      | Initial x -> start ~pkg param env x
      | On_going x -> restart param env x in
    Mresult.Error.fmap (fun x -> On_going x) r

  let block = function
    | Initial _ -> None
    | On_going x ->
      let f = x.focus in
      Some {Loc.loc = snd f.loc; data= Sk.State.peek f.ctx, f.path }

  let pp _ppf = assert false

  let recursive_patching ongoing y = match ongoing with
    | Initial _ as x -> x
    | On_going x ->
      let focus = x.focus in
      let ctx = Sk.State.rec_patch y focus.ctx in
      let focus = { focus with ctx } in
      On_going { x with focus }
end


module type S = sig
  type envt
  type on_going
  type final
  val initial: M2l.t -> on_going
  val next:
    pkg:Paths.P.t -> Transforms.param -> envt -> on_going
    -> (final, on_going) result

  val block: on_going -> (Summary.t * Paths.S.t) Loc.ext option

  val recursive_patching: on_going -> Summary.t -> on_going

  val pp: on_going Pp.t
end
