[@@@warning "-37"]

let debug fmt = Format.ifprintf Pp.err ("Debug:" ^^ fmt ^^"@.")

module L = struct
  type 'a t = 'a list = [] | (::) of 'a * 'a t
end

module Arg = Module.Arg

type level = Module.level = Module | Module_type


module Zdef = Zipper_def
module Sk = Zipper_skeleton

let apkg (f,_) = f
module Ok = Mresult.Ok


module Zip(F:Zdef.fold)(State:Sk.state) = struct
  open Zdef
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
  let mk_arg var name =
    both2
      (fun s f -> Sk.fn ~f ~x:(Some(arg name s)))
      (fun s -> var (Some(arg name s)))
  let m2l = user F.m2l
  let m2l_add state loc r left =
    State.merge state r.backbone, both2 Sk.m2l_add (F.m2l_add loc) r left

  let expr_open param loc = both  (Sk.opened param ~loc) (F.expr_open ~loc)

  let gen_include var param loc = both (Sk.included param loc) (var ~loc)
  let expr_include = gen_include F.expr_include
  let sig_include = gen_include F.sig_include
  let bind_alias state = fork2 (State.bind_alias state) F.bind_alias
  let bind name = both (Sk.bind name) (F.bind name)
  let local_bind name me x = user_me (F.local_bind name me.user) x
  let local_open me x = user_me (F.local_open me.user) x
  let bind_sig name = both (Sk.bind_sig name) (F.bind_sig name)
  let minor = user_ml F.minor
  let minor_ext loc name = user_me (F.minor_ext ~loc name)
  let pack = user F.pack
  let expr_ext name = user_ml (F.expr_ext name)
  let me_ident = both Sk.ident F.me_ident
  let apply param loc f =
    both2 (fun f x -> Sk.apply param loc ~f ~x) (F.apply loc) f
  let me_fun_none =  both (fun f -> Sk.fn ~f ~x:None) (F.me_fun None)
  let mt_fun_none =  both (fun f -> Sk.fn ~f ~x:None) (F.mt_fun None)
  let me_constraint me = user (F.me_constraint me.user)
  let str = both Sk.str F.str
  let me_val x = const Sk.unpacked (F.me_val x)
  let me_ext loc name = user_me (F.me_ext ~loc name)
  let abstract pkg = const (Sk.abstract pkg) F.abstract
  let unpacked = const Sk.unpacked F.unpacked
  let open_me opens = user (F.open_me opens)
  let alias = both Sk.ident F.alias
  let mt_ident = user F.mt_ident
  let mt_sig = both Sk.str F.mt_sig
  let mt_with access deletions =
    both (Sk.m_with deletions) (F.mt_with access deletions)
  let mt_of = user F.mt_of
  let mt_ext loc name = user_me (F.mt_ext ~loc name)
  let sig_abstract x  = const (Sk.abstract x) F.sig_abstract
  let init_rec diff = const diff F.bind_rec_init
  let bind_rec = user F.bind_rec
  let bind_rec_add name me mt =
    user (F.bind_rec_add name (F.me_constraint me.user mt))
  let path_expr_pure = both Sk.ident F.path_expr_pure
  let path_expr_app param loc ~level  ~f ~x opt =
      { backbone = Sk.proj ~level (Sk.apply param loc ~f:f.backbone ~x:x.backbone) opt ;
        user = F.path_expr_app f.user x.user opt }
end

let ((>>=), (>>|)) = Ok.((>>=), (>>|))

module  Zpath(F:Zdef.fold): Zdef.s with module T = F = struct
  module rec M :  Zdef.s with module T := F = M
  include M
  module T = F
end

module Make(F:Zdef.fold)(Env:Stage.envt) = struct

  module Path = Zpath(F)
  open Path
  type 'a path = 'a Path.t
  module State=Zipper_skeleton.State(Env)
  module D = Zip(F)(State)

  let resolve0 ~param ~state ~path:p px =
    match State.resolve param state px with
    | Error () -> Error ( { path=p; focus= px })
    | Ok x -> Ok (D.path x)

  (* Unused types
  type expr = (Sk.state_diff, F.expr) pair
  type path_expr = F.path_expr
  type ext = F.ext
  type annotation = F.annotation
  *)

  let _default_edge = Option.default Deps.Edge.Normal


  let fn_gen sel wrap k ~param ~loc ~state ~path name body signature =
    let state =
      State.bind_arg state {name; signature=signature.Zdef.backbone } in
    let arg = Some ({ Arg.name; signature }, State.diff state) in
    k (wrap arg :: path) ~param ~loc ~state body >>| D.mk_arg sel name signature
  let fn_me = fn_gen F.me_fun (fun x -> Me (Fun_right x))
  let fn_mt = fn_gen F.mt_fun (fun x -> Mt (Fun_right x))

  let rec m2l ~param path left ~pkg ~state : _ L.t -> _ = function
    | [] -> Ok (D.m2l left)
    | {Loc.loc; data=a} :: right ->
      let loc = pkg, loc in
      expr ~param ~loc ~state
        (M2l {left;loc;state=State.diff state;right} :: path)  a
      >>= fun r ->
      let state, left = D.m2l_add state loc r left in
      m2l ~param path left  ~pkg ~state right
  and m2l_start ~param path ~pkg ~state =
    m2l ~param path {backbone=Sk.m2l_init; user=F.m2l_init} ~pkg ~state
  and expr ~param path ~loc ~state expr = match expr with
    | Open m -> me ~param (Expr Open::path) ~loc ~state m
      >>| D.expr_open param loc
    | Include m ->
      me  ~param (Expr Include :: path) ~loc ~state m
      >>| D.expr_include param loc
    | SigInclude m ->
      mt (Expr SigInclude :: path) ~param ~loc ~state m
      >>| D.sig_include param loc
    | Bind {name; expr=(Ident s|Constraint(Abstract, Alias s))}
      when State.is_alias param state s -> Ok (D.bind_alias state name s)
    | Bind {name; expr} ->
      me ~param (Expr (Bind name) :: path) ~loc ~state expr
      >>| D.bind name
    | Bind_sig {name; expr} ->
      mt (Expr (Bind_sig name) :: path) ~param ~loc ~state expr
      >>| D.bind_sig name
    | Bind_rec l ->
      let state = State.rec_approximate state l in
      bind_rec_sig path Sk.bind_rec_init L.[] ~param ~loc ~state l
    | Minor x ->
      minors (Expr Minors :: path) ~param ~pkg:(apkg loc) ~state x
      >>| D.minor
    | Extension_node {name;extension} ->
      ext ~pkg:(apkg loc) ~param ~state (Expr (Extension_node name) :: path)
        extension >>| D.expr_ext name
  and me path  ~param ~loc ~state = function
    | Ident s ->
      resolve (Me Ident :: path) ~param ~loc ~state ~level:Module s
      >>| D.me_ident
    | Apply {f; x} ->
      debug "syntactic apply: %a(%a)@." M2l.pp_me f M2l.pp_me x;
      me (Me (Apply_left x)::path)  ~param ~loc ~state f >>= fun f ->
      me (Me (Apply_right f)::path) ~param ~loc ~state x >>|
      D.apply param loc f
    | Fun {arg = None; body } ->
      me (Me (Fun_right None) :: path) ~param ~loc ~state body
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
    | Val v -> minors (Me Val::path) ~param ~pkg:(apkg loc) ~state v
      >>| D.me_val
    | Extension_node {name;extension=e} ->
      ext ~param ~pkg:(apkg loc) ~state (Me (Extension_node name) :: path) e >>|
      D.me_ext loc name
    | Abstract -> Ok (D.abstract @@ fst loc)
    | Unpacked -> Ok D.unpacked
    | Open_me {opens; expr; _ } ->
      open_all path expr F.open_init ~loc ~param ~state opens
  and open_right path expr ~param ~loc ~state opens =
    me (Me (Open_me_right {state=State.diff state;opens}) :: path)
      ~param ~loc ~state expr >>| D.open_me opens
  and open_all_rec path expr left ~loc ~param ~diff ~state : _ L.t -> _ = function
    | [] -> open_right path expr ~param ~loc ~state left
    | {Loc.data=a;loc=subloc} :: right ->
      let open_loc = (apkg loc,subloc) in
      let path' = Me (Open_me_left {left; right; loc=(snd loc); diff; expr}) :: path in
      resolve path' ~param ~state ~loc:open_loc
        ~level:Module a >>= fun a ->
      let state = State.open_path ~param ~loc:open_loc state a.backbone in
      open_all_rec path expr (F.open_add a.user left) ~param ~loc ~diff
        ~state right
  and open_all path expr left ~state ~loc ~param =
    open_all_rec path expr left ~param ~loc ~diff:(State.diff state) ~state
  and mt path ~param ~loc ~state = function
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
    | With {body;deletions;minors=a} ->
      let access_path = Mt (With_access {body;deletions})::path in
      minors ~param ~pkg:(apkg loc) ~state access_path a >>= fun minors ->
      mt (Mt (With_body {minors;deletions})::path) ~param ~loc ~state body
      >>| D.mt_with minors deletions
    | Of m -> me (Mt Of :: path) ~param ~loc ~state m >>| D.mt_of
    | Extension_node {name;extension=e} -> Sk.ext param loc name;
      ext ~pkg:(apkg loc) ~param ~state (Mt (Extension_node name)::path)  e
      >>| D.mt_ext loc name
    | Abstract -> Ok (D.sig_abstract @@ fst loc)
  and bind_rec_sig path diff left ~param ~loc ~state : _ L.t -> _ = function
    | [] ->
      let state = State.merge state diff in
      bind_rec path (D.init_rec diff) ~param ~loc ~state (List.rev left)
    | {M2l.name; expr=M2l.Constraint(me,ty)} :: right ->
      mt (Expr(Bind_rec_sig{diff;left;name;expr=me; right}) :: path) ~loc ~param ~state ty
      >>= fun mt ->
      let diff = Sk.bind_rec_add name mt.backbone diff in
      bind_rec_sig path diff L.((name, mt.user, me)::left) ~loc ~state ~param right
    | {M2l.name; expr} :: right ->
      bind_rec_sig path diff left ~param ~loc ~state
        ({M2l.name; expr=M2l.(Constraint(expr,Abstract))}::right)
  and bind_rec path left ~param ~loc ~state : _ L.t -> _ = function
    | [] -> Ok (D.bind_rec left)
    | (name,mt,mex) :: right ->
      me (Expr(Bind_rec{left;name;mt;right}) :: path) ~loc ~param ~state mex
      >>= fun me ->
      let left = D.bind_rec_add name me mt left in
      bind_rec path left ~loc ~state ~param right
  and path_expr_gen ~level ctx ?edge ~loc ~param ~state = function
    | Paths.Expr.Simple x ->
      resolve ~level ~loc ~state ?edge ~param (Path_expr Simple :: ctx) x >>| D.path_expr_pure
    | Apply {f;x;proj} ->
      path_expr_gen ?edge ~level:Module (Path_expr(App_f (x,None))::ctx) ~loc ~param ~state f >>= fun f ->
      path_expr ?edge ~level:Module (Path_expr(App_x (f,None))::ctx) ~loc ~param ~state x >>| fun x ->
      D.path_expr_app param loc ~level ~f ~x proj
  and path_expr ?edge ~level ctx ~loc ~param ~state x = path_expr_gen
      ?edge ~level ctx ~loc ~param ~state x
  and gen_minors path ~pkg ~param ~state left = function
    | L.[] ->
      debug "minors end@."; Ok left
    | a :: right ->
      minor (Minors {left;right} :: path) ~pkg ~param ~state a
      >>= fun a ->
      gen_minors path ~pkg ~param ~state (F.add_minor a left) right
  and minors path ~pkg ~param ~state x =
    debug "minors: %a@." Summary.pp State.(peek @@ diff state);
    gen_minors path ~pkg ~param ~state F.empty_minors x
  and minor path ~pkg ~param ~state =
    function
    | Access x ->
      access (Minor Access :: path) ~pkg ~param ~state x
    | Pack m ->
      me (Minor Pack :: path) ~param ~loc:(pkg, m.Loc.loc) ~state m.Loc.data
        >>| fun {user; _ } -> F.pack user
    | Extension_node {data;loc} ->
      ext (Minor (Extension_node data.name) :: path)
        ~param ~pkg ~state data.extension
        >>| fun x -> F.minor_ext ~loc:(pkg,loc) data.name x
    | Local_open (loc,e,m) ->
      debug "Local open: %a@." M2l.pp_me e;
      let diff0 = State.diff state in
      me (Minor (Local_open_left (diff0,loc,m)) :: path)
        ~param ~loc:(pkg, loc) ~state e >>= fun e ->
      let diff = Sk.opened param ~loc:(pkg,loc) e.backbone in
      let state = State.merge state diff in
      (*Format.eprintf "@[opened %a@ | state:%a@]@."
        Sk.pp_ml e.backbone
        Summary.pp State.(peek @@ diff state);*)
      minors (Minor (Local_open_right (diff0,e)) :: path)
        ~pkg ~param ~state m
      >>| fun m -> F.local_open e.user m
    | Local_bind (loc,{name;expr},m) ->
      let diff0 = State.diff state in
      me (Minor (Local_bind_left (diff0,name,m)) :: path)
        ~param ~loc:(pkg, loc) ~state expr >>= fun e ->
      let diff = Sk.bind name e.backbone in
      let state = State.merge state diff in
      minors (Minor (Local_bind_right (diff0,name,e)) :: path)
        ~pkg ~param ~state m
      >>| fun m -> F.local_open e.user m
    | _ -> .
  and access path ~pkg ~param ~state s =
    access_step ~state ~pkg ~param path F.access_init (Paths.E.Map.bindings s)
  and access_step path left ~param ~pkg ~state :
    (Paths.Expr.t * _ ) L.t -> _ = function
    | [] -> Ok (F.access left)
    | (a, (loc,edge)) :: right ->
      let loc = pkg, loc in
      path_expr (Access {left;right} :: path) ~edge ~loc ~state ~param
        ~level:Module a >>= fun a ->
      access_step path ~param ~pkg ~state (F.access_add a.user loc edge left) right
  and ext path ~param ~pkg ~state = function
    | Module m -> m2l_start ~param ~pkg ~state (Ext Mod :: path) m
      >>| fun m -> F.ext_module m.user
    | Val v -> minors ~state ~pkg ~param (Ext Val :: path) v >>| F.ext_val
  and resolve path ~param ~loc ~level ~state ?edge s =
    let px = { Sk.edge; level; loc; ctx=State.diff state; path = s } in
    resolve0 ~param ~state ~path px

  open M2l
  let rec restart ~param state z =
    let v = z.focus in
    let loc = v.Sk.loc in
    match resolve0 ~param ~state ~path:z.path v with
    | Error _ -> Error z
    | Ok x -> match z.path with
      | Me Ident :: rest ->
        restart_me ~param ~state ~loc (rest: module_expr path) (D.me_ident x)
      | Me Open_me_left {left;right;diff;loc=body_loc;expr} :: path ->
        let state = State.open_path ~param ~loc state x.backbone in
        open_all ~state ~param ~loc:(apkg loc, body_loc) path expr (F.open_add x.user left) right >>=
        restart_me ~param ~state:(State.restart state diff) ~loc (path:module_expr path)
      | Mt Alias :: path ->
        restart_mt ~param ~loc ~state (path: module_type path) (D.alias x)
      | Path_expr Simple :: path ->
        restart_path_expr ~param ~loc ~state path (D.path_expr_pure x)
      | _ -> .
  and restart_me: module_expr Path.t -> _ = fun path ~state ~loc ~param x -> match path with
    | Expr Include :: rest ->
      restart_expr ~state ~param (rest: expression path) (D.expr_include param loc x)
    | Expr Open :: rest ->
      restart_expr ~param ~state (rest: expression path) (D.expr_open param loc x)
    | Minor Pack :: path ->
      let pkg = apkg loc in
      restart_minor (path: M2l.minor path) ~state ~loc ~param ~pkg (D.pack x)
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
      let state = State.restart state diff in
      restart_me path ~state ~loc ~param (D.mk_arg F.me_fun r.name r.signature x)
    | Me (Constraint_left mty) :: path ->
      mt (Me (Constraint_right x)::path) ~loc ~param ~state mty >>= fun mt ->
      restart_me path ~loc ~state ~param (D.me_constraint x mt)
    | Me (Open_me_right {opens;state=diff}) :: path ->
      let state = State.restart state diff in
      restart_me path ~loc ~state ~param (D.open_me opens x)
    | Expr (Bind name) :: path ->
      restart_expr (path: expression path) ~state ~param (D.bind name x)
    | Expr (Bind_rec {left;name;mt;right}) :: path  ->
      let left = D.bind_rec_add name x mt left in
      bind_rec path left ~loc ~param ~state right >>=
      restart_expr ~state ~param (path: expression path)
    | Minor Local_bind_left (diff0,no,body) :: path ->
      let diff = Sk.bind no x.backbone in
      let state' = State.merge state diff in
      minors
        (Minor (Local_bind_right (diff0,no,x))::path)
        ~param ~state:state' ~pkg:(apkg loc)
        body
      >>= fun body ->
      let state = State.restart state diff in
      restart_minor (path: minor path) ~param ~loc ~state ~pkg:(apkg loc)
        (D.local_bind no x body)
    | Minor Local_open_left (diff0,loc_open,m) :: path ->
      let diff = Sk.opened param ~loc:(apkg loc, loc_open) x.backbone in
      let state' = State.merge state diff in
      minors (Minor (Local_open_right (diff0,x)) :: path)
        ~param ~state:state' ~pkg:(apkg loc) m >>= fun minors ->
      restart_minor (path: M2l.minor path)
        ~pkg:(apkg loc) ~param ~state ~loc
        (D.local_open x minors)
    | _ -> .
  and restart_expr: expression path -> _ =
    fun path ~state ~param x ->
    match path with
    | M2l {left;loc;right; state=restart } :: path ->
      let state, left = D.m2l_add state loc x left in
      m2l path left ~pkg:(apkg loc) ~param ~state right >>=
      restart_m2l ~param ~loc ~state:(State.restart state restart) (path: m2l path)
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
      let state = State.restart state diff in
      restart_mt ~loc ~state ~param path (D.mk_arg F.mt_fun arg.name arg.signature x)
    | Mt Fun_right None :: path ->
      restart_mt ~loc ~param ~state path (D.mt_fun_none x)
    | Mt With_body {minors;deletions} :: path ->
      restart_mt ~loc ~param ~state path (D.mt_with minors deletions x)
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
    | Path_expr App_f (arg,proj) :: path ->
      arg  |>
      path_expr ~level:Module ~loc ~param ~state (Path_expr(App_x (x,proj))::path) >>=
      restart_path_expr ~loc ~param ~state path
    | Path_expr App_x (f,None) :: path ->
      restart_path_expr ~loc ~param ~state path (D.path_expr_app ~level:Module param loc ~f ~x None)
    | Path_expr App_x (f,Some _proj) :: path ->
      (* FIXME: compute path application and projection dependencies *)
      restart_path_expr ~loc ~param ~state  path (D.path_expr_app param loc ~level:Module ~f ~x None)
    | Access a :: (Minor Access :: rest as all) ->
      let edge = Deps.Edge.Normal (* default_edge v.edge *) in
      let r = F.access_add x.user loc edge a.left in
        access_step all ~pkg:(apkg loc) ~param ~state r a.right
        >>= fun m ->
        restart_minor ~param ~state ~loc ~pkg:(apkg loc)
          (rest: minor path) {user=m; backbone=Sk.empty}
   | _ -> .
  and restart_minor path ~pkg ~param ~loc ~state x =
    match path with
    | Minors {left; right} :: path ->
      gen_minors path (F.add_minor x.user left) ~pkg ~param ~state right
      >>= restart_minors (path: M2l.minor list path)
        ~param ~pkg ~loc ~state
    | _ -> .
  and restart_minors (path:M2l.minor list path)
      ~param ~pkg ~loc  ~state x = match path with
    | Expr Minors :: path ->
      restart_expr path ~param ~state (D.minor x)
    | Minor Local_open_right (diff0,expr) :: path ->
      let state = State.restart state diff0 in
      restart_minor path ~loc ~state ~param ~pkg (D.local_open expr x)
    | Me Val :: path ->
      restart_me path ~state ~loc ~param (D.me_val x)
    | Mt With_access {body;deletions} :: path ->
      mt (Mt(With_body {deletions;minors=x}) :: path)
        ~loc ~state ~param body
      >>| D.mt_with x deletions
      >>= restart_mt ~loc ~state ~param path
    | Ext Val :: path ->
      restart_ext (path: extension_core path) ~param ~loc ~state
        (F.ext_val x)
    | Minor Local_bind_right (diff0,no,expr) :: path ->
      let state = State.restart state diff0 in
      restart_minor (path:minor path) ~pkg ~param ~loc ~state
        (D.local_bind no expr x)
    | _ -> .
  and restart_ext: extension_core path -> _ =
    fun path ~loc ~param ~state x -> match path with
    | Expr (Extension_node name) :: path ->
      restart_expr ~state ~param path (D.expr_ext name x)
    | Me (Extension_node name) :: path ->
      restart_me path ~loc ~param ~state (D.me_ext loc name x)
    | Mt (Extension_node name) :: path ->
      restart_mt path ~loc ~param ~state (D.mt_ext loc name x)
    | Minor (Extension_node name) :: path ->
      restart_minor path ~loc ~pkg:(apkg loc) ~param ~state
        (D.minor_ext loc name x)
    | _ -> .
  and restart_m2l: m2l path -> _ = fun path ~loc ~param ~state x ->
    match path with
    | [] -> Ok x
    | Me Str :: path ->
      restart_me ~loc ~param ~state path (D.str x)
    | Mt Sig :: path ->
      restart_mt ~loc ~param ~state path (D.mt_sig x)
(*    | Annot (Values {packed;access;left;right}) :: path ->
      let left = F.value_add x.user left in
      m2ls path packed access left ~pkg:(apkg loc) ~param ~state right
      >>| F.annot packed access
      >>= restart_annot ~loc ~state ~param path
*)
    | Ext Mod :: path -> restart_ext ~loc ~param ~state path (F.ext_module x.user)
    | _ :: _ -> .

  let unpack x = Sk.final x.Zdef.backbone, x.Zdef.user

  type on_going =
    | On_going of Sk.path_in_context zipper
    | Initial of M2l.t
 let initial x = Initial x

  let start ~pkg param env x =
    m2l_start ~pkg ~state:(State.from_env env) ~param [] x >>|
    unpack

  let restart param env z =
    let state = State.from_env ~diff:z.focus.Sk.ctx env in
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
      Some {Loc.loc = snd f.loc; data= State.peek f.ctx, f.path }


  module Pp = Zipper_pp.Make(Path)(Zipper_pp.Opaque(Path))
  let pp ppf = function
    | Initial m2l -> M2l.pp ppf m2l
    | On_going g -> Pp.pp ppf g

  let recursive_patching ongoing y = match ongoing with
    | Initial _ as x -> x
    | On_going x ->
      let focus = x.focus in
      let ctx = State.rec_patch y focus.ctx in
      let focus = { focus with ctx } in
      On_going { x with focus }
end
