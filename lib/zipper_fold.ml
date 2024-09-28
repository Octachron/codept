[@@@warning "-37"]
open Debug

module L = struct
  type 'a t = 'a list = [] | (::) of 'a * 'a t
end

module Arg = Module.Arg

type level = Module.level = Module | Module_type


module Zdef = Zipper_def
module Sk = Zipper_skeleton

type full_ctx = { uloc: Uloc.t; seed: Id.seed }
type ctx = { seed:Id.seed; pkg:Pkg.t }
let with_loc loc ({seed;pkg}:ctx) = {uloc={pkg;loc}; seed }
let rm_loc { uloc; seed } = { pkg=uloc.pkg; seed }

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

  let gen_include lvl var param ctx = both (Sk.included param ctx.uloc ctx.seed lvl) (var ~loc:ctx.uloc)
  let expr_include = gen_include Module F.expr_include
  let sig_include = gen_include  Module_type F.sig_include
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
  let mt_with original  = user (F.mt_with original)
  let mt_of = user F.mt_of
  let mt_ext loc name = user_me (F.mt_ext ~loc name)
  let sig_abstract x  = const (Sk.abstract x) F.sig_abstract
  let init_rec diff = const diff F.bind_rec_init
  let bind_rec = user F.bind_rec
  let bind_rec_add name me mt =
    user (F.bind_rec_add name (F.me_constraint me.user mt))
  let path_expr_pure = both Sk.ident F.path_expr_pure
  let path_expr_app param loc  ~f ~x =
      { backbone = Sk.apply param loc ~f:f.backbone ~x:x.backbone ;
        user = F.path_expr_app f.user x.user }

  let path_expr_proj app_res proj proj_res =
    {
      backbone = Sk.ident proj_res.backbone;
      user = F.path_expr_proj app_res.user proj proj_res.user
    }

  let me_proj app_res proj proj_res =
    {
      backbone = Sk.ident proj_res.backbone;
      user = F.me_proj app_res.user proj proj_res.user
    }


  let with_type with_cstr cstrs =
    { backbone = cstrs.backbone; user = F.with_type with_cstr cstrs.user }

  let with_lhs lhs cstrs =
    {backbone = cstrs.backbone; user =  F.with_lhs lhs.user cstrs.user }

  let with_module ~delete ~lhs me cstrs =
    {backbone = Sk.with_module ~delete ~lhs ~rhs:me.backbone cstrs.backbone;
     user =  F.with_module ~delete ~lhs ~rhs:me.user cstrs.user }

  let with_module_type ~delete ~lhs mt cstrs =
    {backbone = Sk.with_module_type ~delete ~lhs ~rhs:mt.backbone cstrs.backbone;
     user =  F.with_module_type ~delete ~lhs ~rhs:mt.user cstrs.user }

  let with_init body = { backbone = body.backbone; user = F.with_init }

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


  let fn_gen sel wrap k ~param ~(ctx:full_ctx) ~state ~path name body signature =
    let state =
      State.bind_arg state {name; signature=signature.Zdef.backbone } in
    let arg = Some ({ Arg.name; signature }, State.diff state) in
    k (wrap arg :: path) ~param ~ctx ~state body >>| D.mk_arg sel name signature
  let fn_me = fn_gen F.me_fun (fun x -> Me (Fun_right x))
  let fn_mt = fn_gen F.mt_fun (fun x -> Mt (Fun_right x))

  let rec m2l ~param ~(ctx:ctx) path left ~state : _ L.t -> _ = function
    | [] -> Ok (D.m2l left)
    | {Loc.loc; data=a} :: right ->
      let lctx = with_loc loc ctx in
      expr ~param ~ctx:lctx ~state
        (M2l {left;loc=lctx.uloc;state=State.diff state;right} :: path)  a
      >>= fun r ->
      let state, left = D.m2l_add state lctx.uloc r left in
      m2l ~param ~ctx path left  ~state right
  and m2l_start ~param ~ctx path ~state =
    m2l ~param ~ctx path {backbone=Sk.m2l_init; user=F.m2l_init} ~state
  and expr ~param ~ctx path ~state expr = match expr with
    | Open m -> me ~param (Expr Open::path) ~ctx ~state m
      >>| D.expr_open param ctx.uloc
    | Include m ->
      me ~param (Expr Include :: path) ~ctx ~state m
      >>| D.expr_include param ctx
    | SigInclude m ->
      mt (Expr SigInclude :: path) ~param ~ctx ~state m
      >>| D.sig_include param ctx
    | Bind {name; expr=(Ident s|Constraint(Abstract, Alias s))}
      when State.is_alias param state s -> Ok (D.bind_alias state name s)
    | Bind {name; expr} ->
      me ~param (Expr (Bind name) :: path) ~ctx ~state expr
      >>| D.bind name
    | Bind_sig {name; expr} ->
      mt (Expr (Bind_sig name) :: path) ~param ~ctx ~state expr
      >>| D.bind_sig name
    | Bind_rec l ->
      let state = State.rec_approximate state l in
      bind_rec_sig path Sk.bind_rec_init L.[] ~param ~ctx ~state l
    | Minor x ->
      minors (Expr Minors :: path) ~param ~ctx:(rm_loc ctx) ~state x
      >>| D.minor
    | Extension_node {name;extension} ->
      ext ~param ~ctx:(rm_loc ctx) ~state (Expr (Extension_node name) :: path)
        extension >>| D.expr_ext name
  and me path  ~param ~ctx ~state = function
    | Ident s ->
      resolve (Me Ident :: path) ~param ~ctx ~state ~level:Module s
      >>| D.me_ident
    | Apply {f; x} ->
      debug "syntactic apply: %a(%a)@." M2l.pp_me f M2l.pp_me x;
      me (Me (Apply_left x)::path) ~param ~ctx ~state f >>= fun f ->
      me (Me (Apply_right f)::path) ~param ~ctx ~state x >>|
      D.apply param ctx.uloc f
    | Proj {me=mep;proj} ->
      me (Me (Proj_left proj)::path) ~param ~ctx ~state mep >>=
      me_proj ~state path ~param ~ctx proj
    | Fun {arg = None; body } ->
      me (Me (Fun_right None) :: path) ~param ~ctx ~state body
      >>| D.me_fun_none
    | Fun {arg = Some {name;signature} ; body } ->
      let diff = State.diff state in
      let pth = Me (Fun_left {name; diff; body})  :: path in
      mt pth ~param ~ctx ~state signature >>=
      fn_me me ~path ~param ~ctx ~state name body
    | Constraint (mex,mty) ->
      me (Me (Constraint_left mty)::path) ~ctx ~state ~param  mex >>= fun me ->
      mt (Me (Constraint_right me)::path) ~ctx ~param ~state mty >>|
      D.me_constraint me
    | Str items ->
      m2l_start (Me Str :: path) ~ctx:(rm_loc ctx) ~param ~state items
      >>| D.str
    | Val v -> minors (Me Val::path) ~param ~ctx:(rm_loc ctx) ~state v
      >>| D.me_val
    | Extension_node {name;extension=e} ->
      ext ~param ~ctx:(rm_loc ctx) ~state (Me (Extension_node name) :: path) e >>|
      D.me_ext ctx.uloc name
    | Abstract -> Ok (D.abstract ctx.seed)
    | Unpacked -> Ok D.unpacked
    | Open_me {opens; expr; _ } ->
      open_all path expr F.open_init ~param ~ctx ~state opens
  and open_right path expr ~param ~ctx ~state opens =
    me (Me (Open_me_right {state=State.diff state;opens}) :: path)
      ~param ~ctx ~state expr >>| D.open_me opens
  and open_all_rec path expr left ~ctx ~param ~diff ~state : _ L.t -> _ = function
    | [] -> open_right path expr ~param ~ctx ~state left
    | {Loc.data=a;loc=subloc} :: right ->
      let open_ctx = { ctx with uloc = {Uloc.pkg=ctx.uloc.pkg; loc = subloc} } in
      let path' = Me (Open_me_left {left; right; loc=ctx.uloc.loc; diff; expr}) :: path in
      resolve path' ~param ~state ~ctx:open_ctx
        ~level:Module a >>= fun a ->
      let state = State.open_path ~param ~loc:open_ctx.uloc state a.backbone in
      open_all_rec path expr (F.open_add a.user left) ~param ~ctx ~diff
        ~state right
  and open_all path expr left ~state ~ctx ~param =
    open_all_rec path expr left ~param ~ctx ~diff:(State.diff state) ~state
  and mt path ~param ~ctx ~state = function
    | Alias id ->
      resolve (Mt Alias :: path) ~param ~ctx ~state ~level:Module id
      >>| D.alias
    | Ident ids ->
      path_expr ~level:Module_type (Mt Ident :: path) ~param ~ctx ~state ids
      >>| D.mt_ident
    | Sig items ->
      m2l_start (Mt Sig :: path) ~ctx:(rm_loc ctx) ~state ~param items
      >>| D.mt_sig
    | Fun {arg = None; body } ->
      mt (Mt (Fun_right None)::path) ~param ~ctx ~state body
      >>| D.mt_fun_none
    | Fun {arg = Some {Arg.name;signature}; body } ->
      let diff = State.diff state in
      let arg_path = Mt(Fun_left {name; diff; body} )::path in
      mt arg_path ~param ~ctx ~state signature >>=
      fn_mt mt ~path ~param ~ctx ~state name body
    | With {body;with_constraints=with_cstrs} ->
      mt (Mt (With_body with_cstrs)::path) ~param ~ctx ~state body >>= fun body ->
      with_constraints path ~param ~ctx ~state ~original_body:body ~with_action:(D.with_init body)
        with_cstrs
      >>|  D.mt_with body.user
    | Of m -> me (Mt Of :: path) ~param ~ctx ~state m >>| D.mt_of
    | Extension_node {name;extension=e} -> Sk.ext param ctx.uloc name;
      ext ~ctx:(rm_loc ctx) ~param ~state (Mt (Extension_node name)::path)  e
      >>| D.mt_ext ctx.uloc name
    | Abstract -> Ok (D.sig_abstract ctx.seed)
  and with_constraints path ~param ~ctx ~state ~original_body ~with_action = function
    | [] -> Ok with_action
    | a :: q ->
      let elt = With_constraints {original_body; right=q} in
      with_constraint (Mt elt::path) ~param ~ctx ~state with_action a >>= fun with_action ->
      with_constraints path ~param ~ctx ~state ~original_body ~with_action q
  and with_constraint path ~param ~ctx ~state with_action {lhs;delete;rhs} =
    let path' = With_constraint (With_lhs {body=with_action;lhs;delete;rhs}) :: path in
    match rhs, List.rev lhs with
    | Type _, ([_]| []) -> with_constraint_rhs path ~param ~ctx ~state lhs delete with_action rhs
    | Type _, _ :: sublhs | _, sublhs ->
      let sublhs = List.rev sublhs in
      let level : level = match rhs with Type _ | Module _ -> Module | Module_type _ -> Module_type in
      resolve path' ~level ~param ~ctx ~state ?within:(Sk.signature with_action.backbone) sublhs >>= fun l ->
      with_constraint_rhs path ~param ~ctx ~state lhs delete (D.with_lhs l with_action) rhs
  and with_constraint_rhs path ~param ~ctx ~state lhs delete with_action = function
    | Type m ->
      let path = With_constraint (With_type with_action)::path in
      minors path ~param ~ctx:(rm_loc ctx) ~state m >>| fun m ->
      D.with_type m with_action
    | Module rhs ->
      let path = With_constraint (With_module {body=with_action;delete;lhs}):: path in
      resolve path ~param ~ctx ~state ~level:Module rhs.data >>| fun me ->
          D.with_module ~delete ~lhs (D.me_ident me) with_action
    | Module_type rhs ->
      let path = With_constraint (With_module_type {body=with_action; delete;lhs}) :: path in
      mt path ~param ~ctx ~state rhs >>| fun mt ->
      D.with_module_type ~delete ~lhs mt with_action
  and bind_rec_sig path diff left ~param ~ctx ~state : _ L.t -> _ = function
    | [] ->
      let state = State.merge state diff in
      bind_rec path (D.init_rec diff) ~param ~ctx ~state (List.rev left)
    | {M2l.name; expr=M2l.Constraint(me,ty)} :: right ->
      mt (Expr(Bind_rec_sig{diff;left;name;expr=me; right}) :: path) ~param ~ctx ~state ty
      >>= fun mt ->
      let diff = Sk.bind_rec_add name mt.backbone diff in
      bind_rec_sig path diff L.((name, mt.user, me)::left) ~state ~param ~ctx right
    | {M2l.name; expr} :: right ->
      bind_rec_sig path diff left ~param ~ctx ~state
        ({M2l.name; expr=M2l.(Constraint(expr,Abstract))}::right)
  and bind_rec path left ~param ~ctx ~state : _ L.t -> _ = function
    | [] -> Ok (D.bind_rec left)
    | (name,mt,mex) :: right ->
      me (Expr(Bind_rec{left;name;mt; right}) :: path) ~param ~ctx ~state mex
      >>= fun me ->
      let left = D.bind_rec_add name me mt left in
      bind_rec path left ~state ~param ~ctx right
  and path_expr_gen ~level path ?edge ~ctx ~param ~state = function
    | Paths.Expr.Simple x ->
      resolve ~level ~ctx ~state ?edge ~param (Path_expr Simple :: path) x >>| D.path_expr_pure
    | Apply {f;x;proj} ->
      let proj = pack_proj level edge proj in
      path_expr_gen ?edge ~level:Module (Path_expr(App_f (x,proj))::path) ~ctx ~param ~state f >>= fun f ->
      path_expr ?edge ~level:Module (Path_expr(App_x (f,proj))::path) ~param ~ctx ~state x >>=
      path_expr_proj ~state ~ctx path param proj f
  and pack_proj level edge proj = Option.fmap (fun p ->(level, Option.(edge><Deps.Edge.Normal), p)) proj
  and path_expr_proj ~state path param ~ctx proj f x =
      let res = D.path_expr_app param ctx.uloc ~f ~x in
      match proj with
      | None -> Ok res
      | Some (level,edge,proj) ->
        let path = Path_expr (Proj(res,proj)) :: path in
        resolve path ?within:(Sk.signature res.backbone) ~state ~level ~ctx
          ~edge ~param proj
        >>| D.path_expr_proj res proj
  and me_proj ~state path ~param ~ctx proj me =
      let path = Me (Proj_right (me,proj)) :: path in
      resolve path ?within:(Sk.signature me.backbone) ~state ~level:Module ~ctx
        ~param proj
      >>| D.me_proj me proj
  and path_expr ?edge ~level path ~ctx ~param ~state x = path_expr_gen
      ?edge ~level path ~ctx ~param ~state x
  and gen_minors path ~param ~ctx ~state left =
    let open L in
    function
    | [] ->
      debug "minors end@."; Ok left
    | a :: right ->
      minor Path.(Minors {left;right} :: path) ~param ~ctx ~state a
      >>= fun a ->
      gen_minors path ~param ~state ~ctx (F.add_minor a left) right
  and minors path ~param ~ctx ~state x =
    debug "minors: %a@." Summary.pp State.(peek @@ diff state);
    gen_minors path ~param ~ctx ~state F.empty_minors x
  and minor path ~param ~ctx ~state =
    function
    | Access x ->
      access (Minor Access :: path) ~param ~ctx ~state x
    | Pack m ->
      me (Minor Pack :: path) ~param ~ctx:(with_loc m.Loc.loc ctx) ~state m.Loc.data
        >>| fun {user; _ } -> F.pack user
    | Extension_node {data;loc} ->
      ext (Minor (Extension_node data.name) :: path)
        ~param ~ctx ~state data.extension
        >>| fun x -> F.minor_ext ~loc:{Uloc.pkg=ctx.pkg;loc} data.name x
    | Local_open (loc,e,m) ->
      debug "Local open: %a@." M2l.pp_me e;
      let diff0 = State.diff state in
      let lctx = with_loc loc ctx in
      me (Minor (Local_open_left (diff0,loc,m)) :: path)
        ~param ~ctx:lctx ~state e >>= fun e ->
      let diff = Sk.opened param ~loc:lctx.uloc e.backbone in
      let state = State.merge state diff in
      debug "@[opened %a@ | state:%a@]@."
        Sk.pp_ml e.backbone
        Summary.pp State.(peek @@ diff state);
      minors (Minor (Local_open_right (diff0,e)) :: path)
        ~ctx ~param ~state m
      >>| fun m -> F.local_open e.user m
    | Local_bind (loc,{name;expr},m) ->
      let diff0 = State.diff state in
      me (Minor (Local_bind_left (diff0,name,m)) :: path)
        ~param ~ctx:(with_loc loc ctx) ~state expr >>= fun e ->
      let diff = Sk.bind name e.backbone in
      let state = State.merge state diff in
      minors (Minor (Local_bind_right (diff0,name,e)) :: path)
        ~ctx ~param ~state m
      >>| fun m -> F.local_open e.user m
    | _ -> .
  and access path ~param ~ctx ~state s =
    access_step ~state ~param ~ctx path F.access_init (Paths.E.Map.bindings s)
  and access_step path left ~param ~ctx ~state :
    (Paths.Expr.t * _ ) L.t -> _ = function
    | [] -> Ok (F.access left)
    | (a, (loc,edge)) :: right ->
      let lctx = with_loc loc ctx in
      path_expr (Access {left;right} :: path) ~edge ~ctx:lctx ~state ~param
        ~level:Module a >>= fun a ->
      access_step path ~param ~ctx ~state (F.access_add a.user lctx.uloc edge left) right
  and ext path ~param ~ctx ~state = function
    | Module m -> m2l_start ~param ~ctx ~state (Ext Mod :: path) m
      >>| fun m -> F.ext_module m.user
    | Val v -> minors ~state ~param ~ctx (Ext Val :: path) v >>| F.ext_val
  and resolve path ~param ~ctx ~level ~state ?edge ?within s =
    let px = { Sk.edge; level; loc=ctx.uloc; ctx=State.diff state; seed=ctx.seed; path = s; within } in
    resolve0 ~param ~state ~path px

  open M2l
  let rec restart ~param state z =
    let v = z.focus in
    let uloc = v.Sk.loc in
    let seed = v.Sk.seed in
    let ctx = { seed; uloc } in
    match resolve0 ~param ~state ~path:z.path v with
    | Error _ -> Error z
    | Ok x -> match z.path with
      | Me Ident :: rest ->
        restart_me ~param ~state ~ctx (rest: module_expr path) (D.me_ident x)
      | Me Open_me_left {left;right;diff;loc=body_loc;expr} :: path ->
        let state = State.open_path ~param ~loc:uloc state x.backbone in
        open_all ~state ~param ~ctx:(with_loc body_loc @@ rm_loc ctx) path expr (F.open_add x.user left) right >>=
        restart_me ~param ~state:(State.restart state diff) ~ctx (path:module_expr path)
      | Mt Alias :: path ->
        restart_mt ~param ~ctx ~state (path: module_type path) (D.alias x)
      | Path_expr Simple :: path ->
        restart_path_expr ~param ~ctx ~state (path:Paths.Expr.t path) (D.path_expr_pure x)
     | Path_expr (Proj (app_res,proj)) :: path ->
        restart_path_expr ~param ~ctx ~state (path:Paths.Expr.t path) (D.path_expr_proj app_res proj x)
      | Me (Proj_right (me,proj)) :: path ->
        restart_me ~param ~ctx ~state (path:module_expr path) (D.me_proj me proj x)
     | With_constraint With_module {body;lhs; delete} :: path ->
       restart_with (path: M2l.with_constraint path) ~param ~state ~ctx
         (D.with_module ~delete ~lhs (D.me_ident x) body)
     | With_constraint With_lhs {body;lhs;delete;rhs} :: path ->
       let path : M2l.with_constraint path = path in
       with_constraint path ~param ~state ~ctx body {lhs;delete;rhs}
       >>= restart_with path ~param ~state ~ctx
     | _ -> .
  and restart_me: module_expr Path.t -> _ = fun path ~state ~ctx ~param x -> match path with
    | Expr Include :: rest ->
      restart_expr ~state  ~param ~ctx (rest: expression path) (D.expr_include param ctx x)
    | Expr Open :: rest ->
      restart_expr ~param ~ctx ~state (rest: expression path) (D.expr_open param ctx.uloc x)
    | Minor Pack :: path ->
      restart_minor (path: M2l.minor path) ~state ~ctx ~param (D.pack x)
    | Me (Apply_left xx) :: path ->
      me (Me (Apply_right x)::path) ~param ~state ~ctx xx
      >>| D.apply param ctx.uloc x
      >>= restart_me ~ctx ~state ~param path
    | Mt Of :: path -> restart_mt ~ctx ~state ~param path (D.mt_of x)
    | Me(Apply_right fn) :: path ->
      restart_me path ~ctx ~param ~state (D.apply param ctx.uloc fn x)
    | Me (Proj_left proj) :: path ->
      me_proj ~state path ~param ~ctx proj x >>=
      restart_me path ~ctx ~param ~state
    | Me(Fun_right None) :: path ->
      restart_me path ~state ~ctx ~param (D.me_fun_none x)
    | Me(Fun_right Some (r,diff)) :: path ->
      let state = State.restart state diff in
      restart_me path ~state ~ctx ~param (D.mk_arg F.me_fun r.name r.signature x)
    | Me (Constraint_left mty) :: path ->
      mt (Me (Constraint_right x)::path) ~ctx ~param ~state mty >>= fun mt ->
      restart_me path ~ctx ~state ~param (D.me_constraint x mt)
    | Me (Open_me_right {opens;state=diff}) :: path ->
      let state = State.restart state diff in
      restart_me path ~ctx ~state ~param (D.open_me opens x)
    | Expr (Bind name) :: path ->
      restart_expr (path: expression path) ~state ~param ~ctx (D.bind name x)
    | Expr (Bind_rec {left;name;mt;right}) :: path  ->
      let left = D.bind_rec_add name x mt left in
      let state = State.restart state left.backbone in
      bind_rec path left ~ctx ~param ~state right >>=
      restart_expr ~state ~ctx ~param (path: expression path)
    | Minor Local_bind_left (diff0,no,body) :: path ->
      let diff = Sk.bind no x.backbone in
      let state' = State.merge state diff in
      minors
        (Minor (Local_bind_right (diff0,no,x))::path)
        ~param ~ctx:(rm_loc ctx) ~state:state'
        body
      >>= fun body ->
      let state = State.restart state diff in
      restart_minor (path: minor path) ~param ~ctx ~state
        (D.local_bind no x body)
    | Minor Local_open_left (diff0,loc_open,m) :: path ->
      let loc = { Uloc.pkg= ctx.uloc.pkg; loc=loc_open } in
      let diff = Sk.opened param ~loc x.backbone in
      let state' = State.merge state diff in
      minors (Minor (Local_open_right (diff0,x)) :: path)
        ~param ~ctx:(rm_loc ctx) ~state:state' m >>= fun minors ->
      restart_minor (path: M2l.minor path)
        ~ctx ~param ~state
        (D.local_open x minors)
    | _ -> .
  and restart_expr: expression path -> _ =
    fun path ~state ~(ctx:full_ctx) ~param x ->
    match path with
    | M2l {left;loc;right; state=restart } :: path ->
      let state = State.restart state restart in
      let state, left = D.m2l_add state loc x left in
      m2l path left ~ctx:{ seed= ctx.seed; pkg = loc.pkg } ~param ~state right >>=
      restart_m2l ~param ~ctx ~state (path: m2l path)
    | _ -> .
  and restart_mt: module_type path -> _ = fun path ~state ~param ~ctx x ->
    match path with
    | Expr (Bind_sig name) :: path ->
      restart_expr ~state ~ctx ~param path (D.bind_sig name x)
    | Me Fun_left {name;diff;body} :: path ->
      let state = State.restart state diff in
      fn_me me ~path ~param ~ctx ~state name body x
      >>= restart_me path ~ctx ~param ~state
    | Mt Fun_left {name;diff;body} :: path ->
      let state = State.restart state diff in
      fn_mt mt ~path ~ctx ~state ~param name body x
      >>= restart_mt ~ctx ~param ~state path
    | Mt Fun_right (Some (arg,diff)) :: path ->
      let state = State.restart state diff in
      restart_mt  ~state ~param ~ctx path (D.mk_arg F.mt_fun arg.name arg.signature x)
    | Mt Fun_right None :: path ->
      restart_mt ~param ~ctx ~state path (D.mt_fun_none x)
    | Mt With_body constraints :: path ->
      with_constraints path ~param ~ctx ~state ~original_body:x ~with_action:(D.with_init x)
        constraints
        >>| D.mt_with x.user >>= restart_mt ~param ~ctx ~state path
    | With_constraint With_module_type {body; delete; lhs } :: path ->
      restart_with path ~param ~ctx ~state (D.with_module_type ~delete ~lhs x body)
    | Me Constraint_right body :: path ->
      restart_me ~param ~ctx ~state path (D.me_constraint body x)
    | Expr SigInclude :: path ->
      restart_expr ~state ~ctx ~param path (D.sig_include param ctx x)
    | Expr Bind_rec_sig {diff; left; name; expr; right} :: path ->
      bind_rec_sig path (Sk.bind_rec_add name x.backbone diff)
        ((name, x.user, expr) :: left) ~param ~ctx ~state  right >>=
      restart_expr ~ctx ~state ~param path
    | _ -> .
  and restart_with: with_constraint path -> _ = fun path ~state ~param ~ctx x ->
    match path with
    | Mt With_constraints {original_body; right} :: path ->
      with_constraints path ~param ~ctx ~state ~original_body ~with_action:x right
      >>| D.mt_with original_body.user >>= restart_mt ~param ~ctx ~state path
    | _ -> .
  and restart_path_expr: Paths.Expr.t path -> _ =
    fun path  ~param ~ctx ~state x -> match path with
    | Mt Ident :: path ->
      restart_mt path ~param ~state ~ctx (D.mt_ident x)
    | Path_expr App_f (arg,proj) :: path ->
      arg  |>
      path_expr ~level:Module ~ctx ~param ~state (Path_expr(App_x (x,proj))::path) >>=
      restart_path_expr ~ctx ~param ~state path
    | Path_expr App_x (f,proj) :: path ->
      path_expr_proj ~ctx ~state path param proj f x >>=
      restart_path_expr ~ctx ~param ~state path
    | Access a :: (Minor Access :: rest as all) ->
      let edge = Deps.Edge.Normal (* default_edge v.edge *) in
      let r = F.access_add x.user ctx.uloc edge a.left in
        access_step all ~ctx:(rm_loc ctx) ~param ~state r a.right
        >>= fun m ->
        restart_minor ~param ~state ~ctx
          (rest: minor path) {user=m; backbone=Sk.empty}
   | _ -> .
  and restart_minor path ~param ~ctx ~state x =
    match path with
    | Minors {left; right} :: path ->
      gen_minors path (F.add_minor x.user left) ~ctx:(rm_loc ctx) ~param ~state right
      >>= restart_minors (path: M2l.minor list path)
        ~param ~ctx ~state
    | _ -> .
  and restart_minors (path:M2l.minor list path)
      ~param ~ctx ~state x = match path with
    | Expr Minors :: path ->
      restart_expr path ~param ~ctx ~state (D.minor x)
    | Minor Local_open_right (diff0,expr) :: path ->
      let state = State.restart state diff0 in
      restart_minor path ~state ~param ~ctx (D.local_open expr x)
    | Me Val :: path ->
      restart_me path ~state ~ctx ~param (D.me_val x)
    | With_constraint With_type body :: path ->
      restart_with ~ctx ~state ~param path (D.with_type x body)
    | Ext Val :: path ->
      restart_ext (path: extension_core path) ~ctx ~param ~state
        (F.ext_val x)
    | Minor Local_bind_right (diff0,no,expr) :: path ->
      let state = State.restart state diff0 in
      restart_minor (path:minor path) ~ctx ~param ~state
        (D.local_bind no expr x)
    | _ -> .
  and restart_ext: extension_core path -> _ =
    fun path ~ctx ~param ~state x -> match path with
    | Expr (Extension_node name) :: path ->
      restart_expr ~state ~ctx ~param path (D.expr_ext name x)
    | Me (Extension_node name) :: path ->
      restart_me path ~ctx ~param ~state (D.me_ext ctx.uloc name x)
    | Mt (Extension_node name) :: path ->
      restart_mt path ~ctx ~param ~state (D.mt_ext ctx.uloc name x)
    | Minor (Extension_node name) :: path ->
      restart_minor path ~ctx ~param ~state
        (D.minor_ext ctx.uloc name x)
    | _ -> .
  and restart_m2l: m2l path -> _ = fun path ~ctx ~param ~state x ->
    match path with
    | [] -> Ok x
    | Me Str :: path ->
      restart_me ~ctx ~param ~state path (D.str x)
    | Mt Sig :: path ->
      restart_mt ~param ~ctx ~state path (D.mt_sig x)
    | Ext Mod :: path -> restart_ext ~ctx ~param ~state path (F.ext_module x.user)
    | _ :: _ -> .

  let unpack x = Sk.final x.Zdef.backbone, x.Zdef.user

  type on_going =
    | On_going of Sk.path_in_context zipper
    | Initial of M2l.t
 let initial x = Initial x

  let start ~pkg param env x =
    let seed = Id.create_seed pkg in
    let ctx = { seed; pkg } in
    m2l_start ~ctx ~state:(State.from_env env) ~param [] x >>|
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
      Some {Loc.loc = f.loc.loc; data= State.peek f.ctx, f.path }


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
