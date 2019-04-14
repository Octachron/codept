open M2l
module Arg = Module.Arg

type a = Paths.Simple.t * (Loc.t * Deps.Edge.t)

class virtual id_core =  object

  method virtual ident : Paths.Simple.t -> (Paths.Simple.t, unit) result

  method virtual abstract : module_expr
  method virtual access :  access -> access
  method virtual access_add :
    Paths.S.t -> Loc.t -> Deps.Edge.t -> access -> access
  method virtual access_init : access
  method virtual add_packed :
    Loc.t -> module_expr -> module_expr Loc.ext list
    -> module_expr Loc.ext list
  method virtual alias : Paths.Simple.t -> module_type
  method virtual annot :
    module_expr Loc.ext list -> access -> m2l list -> annotation
  method virtual apply : module_expr -> module_expr -> module_expr

  method virtual bind : string -> module_expr -> expression
  method virtual bind_rec : module_expr bind list -> expression
  method virtual bind_rec_add :
    string -> module_expr -> module_expr bind list -> module_expr bind list
  method virtual bind_rec_init : module_expr bind list
  method virtual bind_sig : string -> module_type -> expression

  method virtual expr_ext : string -> extension_core -> expression
  method virtual expr_include : module_expr -> expression
  method virtual expr_open : module_expr -> expression
  method virtual ext_module : t -> extension_core
  method virtual ext_val : annotation -> extension_core

  method virtual m2l_add : Loc.t -> expression -> t -> t
  method virtual m2l_init : t
  method virtual m2l: m2l -> m2l

  method virtual me_constraint : module_expr -> module_type -> module_expr
  method virtual me_ext : string -> extension_core -> module_expr
  method virtual me_fun :
    module_type Arg.t option -> module_expr -> module_expr
  method virtual me_ident : Paths.Simple.t -> module_expr
  method virtual me_val : annotation -> module_expr
  method virtual minor : annotation -> expression

  method virtual mt_ext : string -> extension_core -> module_type
  method virtual mt_fun :
    module_type Arg.t option -> module_type -> module_type
  method virtual mt_ident : Paths.Expr.t -> module_type
  method virtual mt_of : module_expr -> module_type
  method virtual mt_sig : t -> module_type
  method virtual mt_with :
    access -> Paths.Simple.set -> module_type -> module_type
  method virtual open_add :
    Paths.Simple.t -> Paths.Simple.t list -> Paths.Simple.t list
  method virtual open_init : Paths.Simple.t list
  method virtual open_me : Paths.Simple.t list -> module_expr -> module_expr

  method virtual packed_init : module_expr Loc.ext list

  method virtual path_expr :
    Paths.Simple.t -> (int * Paths.Expr.t) list -> Paths.Expr.t
  method virtual path_expr_arg :
    int -> Paths.Expr.t -> ((int * Paths.Expr.t) list as 'args) -> 'args
  method virtual path_expr_arg_init : (int * Paths.Expr.t) list
  method virtual sig_abstract : module_type
  method virtual sig_include : module_type -> expression
  method virtual str : t -> module_expr
  method virtual unpacked : module_expr

  method virtual value_add : t -> t list -> t list
  method virtual value_init : t list
  method virtual values : t list -> t list
end

module Path = struct
  type waccess = W of access [@@unboxed]

  type 'focus expr =
    | Open: module_expr expr
    | Include: module_expr expr
    | SigInclude: module_type expr
    | Bind: Name.t -> module_expr expr
    | Bind_sig: Name.t -> module_type expr
    | Bind_rec:
        {left: module_expr bind list;
         name:Name.t;
         right:module_expr bind list
        } -> module_expr expr
    | Minor: annotation expr
    | Extension_node: string -> extension_core expr

  type 'focus annot =
    | Packed: {
        left:module_expr Loc.ext list;
        loc:Loc.t;
        right: module_expr Loc.ext list;
        access:access;
        values: m2l list }
        -> module_expr annot
    | Access :
        {
          packed:module_expr Loc.ext list;
          values: m2l list
        }
        -> waccess annot
    | Values:
        { packed: module_expr Loc.ext list;
          access:access;
          left: m2l list;
          right: m2l list
        }
        -> m2l annot

  type acc =
    {left: access;
     loc:Loc.t; edge: Deps.Edge.t;
     right:a list
    }


  type  'f path_expr =
    | Main: (int * Paths.Expr.t) list -> Paths.S.t path_expr
    | Arg: {
        main: Paths.S.t;
        left: (int * Paths.Expr.t) list;
        pos:int;
        right: (int * Paths.Expr.t) list
      } -> Paths.Expr.t path_expr


  type 'focus me =
    | Ident: Paths.S.t me
    | Apply_left: module_expr -> module_expr me
    | Apply_right: module_expr -> module_expr me
    | Fun_left: string * module_expr -> module_type me
    | Fun_right: module_type Arg.t option -> module_expr me
    | Constraint_left: module_type -> module_expr me
    | Constraint_right: module_expr -> module_type me
    | Str: m2l me
    | Val: annotation me
    | Extension_node: string -> extension_core me
    | Open_me_left:
        { left: Paths.S.t list; right:Paths.S.t list; expr:module_expr }
        -> Paths.S.t me
    | Open_me_right: Paths.S.t list -> module_expr me

  and 'focus mt =
    | Alias: Paths.Simple.t mt
    | Ident: Paths.Expr.t mt
    | Sig: m2l mt
    | Fun_left: string * module_type -> module_type mt
    | Fun_right: module_type Arg.t option -> module_type mt
    | With_access:
        {body:module_type; deletions: Paths.S.set }
        -> waccess mt
    | With_body:
        {access:access; deletions:Paths.S.set } -> module_type mt
    | Of: module_expr mt
    | Extension_node: string -> extension_core mt

  type 'focus ext =
    | Module: m2l ext
    | Val: annotation ext

  type diff = {left:m2l; right:m2l}
  let empty = {left=[]; right=[] }


  type none = No

  type ('elt,'from) elt =
    | M2l: {left:m2l; loc:Loc.t; right:m2l} -> (expression, m2l) elt
    | Expr: 'elt expr -> ('elt,expression) elt
    | Annot: 'elt annot -> ('elt, annotation) elt
    | Me: 'elt me -> ('elt, module_expr) elt
    | Mt: 'elt mt -> ('elt, module_type) elt
    | Access: acc -> (Paths.S.t, waccess) elt
    | Ext: 'elt ext -> ('elt, extension_core) elt
    | Path_expr: 'elt path_expr -> ('elt, Paths.Expr.t) elt

  type _ path =
    | []: m2l path
    | (::): ('focus,'from) elt * 'from path -> 'focus path

end
(*
type 'a witness = W: ('a,'from) elt -> 'a witness [@@unboxed]

let witness (type a) (path:a path): a witness = match path with
  | [] -> W Start
  | a :: _ -> W a
*)

type 'result zipper =
 { path: 'result Path.path; focus: 'result }

let start focus = { path = []; focus}

module L = struct
  type 'a t = 'a list = [] | (::) of 'a * 'a t
end
module Fold = struct
  open Path
  module Ok = Mresult.Ok
  let ((>>=), (>>|)) = Ok.((>>=), (>>|))

  let rec m2l (folder:id_core) path left = function
    | L.[] -> Ok (folder#m2l left)
    | {Loc.loc; data=a} :: right ->
      expr folder (M2l {left;loc;right} :: path) a >>= fun a ->
      m2l folder path (folder#m2l_add loc a left) right
  and expr folder path expr: (expression,_) result = match expr with
    | Defs _ -> assert false (* to be removed *)
    | Open m -> me folder (Expr Open::path) m >>| folder#expr_open
    | Include m -> me folder (Expr Include :: path) m >>| folder#expr_include
    | SigInclude m ->
      mt folder (Expr SigInclude :: path) m >>| folder#sig_include
    | Bind {name; expr} ->
      me folder (Expr (Bind name) :: path) expr >>| folder#bind name
    | Bind_sig {name; expr} ->
      mt folder (Expr (Bind_sig name) :: path) expr >>| folder#bind_sig name
    | Bind_rec l ->
      bind_rec folder path folder#bind_rec_init l >>| folder#bind_rec
    | Minor m -> minor folder (Expr Minor :: path) m >>| folder#minor
    | Extension_node {name;extension} ->
      ext folder (Expr (Extension_node name) :: path) extension
      >>| folder#expr_ext name
  and me folder path = function
    | Resolved _ -> assert false (* to be removed *)
    | Ident s -> ident folder (Me Ident :: path) s >>| folder#me_ident
    | Apply {f; x} ->
      me folder (Me (Apply_left x)::path) f >>= fun f ->
      me folder (Me (Apply_right f)::path) x >>|
      folder#apply f
    | Fun {arg = None; body } ->
      me folder (Me (Fun_right None) :: path) body >>| folder#me_fun None
    | Fun {arg = Some arg ; body } ->
      let pth = Me (Fun_left (arg.name, body))  :: path in
      mt folder pth arg.signature >>= fun signature ->
      let arg = Some {arg with signature} in
      me folder (Me (Fun_right arg) :: path ) body >>|
      folder#me_fun arg
    | Constraint (mex,mty) ->
      me folder (Me (Constraint_left mty)::path) mex >>= fun me ->
      mt folder (Me (Constraint_right me)::path) mty >>|
      folder#me_constraint me
    | Str items ->
      m2l folder (Me Str :: path) folder#m2l_init items >>| folder#str
    | Val v -> minor folder (Me Val::path) v >>| folder#me_val
    | Extension_node {name;extension=e} ->
      ext folder (Me (Extension_node name) :: path) e >>| folder#me_ext name
    | Abstract -> Ok folder#abstract
    | Unpacked -> Ok folder#unpacked
    | Open_me {opens; expr; _ } ->
      open_all folder path expr folder#open_init opens >>=
      open_right folder path expr
  and open_right folder path expr opens =
    me folder (Me (Open_me_right opens) :: path) expr >>|
    folder#open_me opens
  and open_all folder path expr left = function
    | L.[] -> Ok left
    | a :: right ->
      let path' = Me (Open_me_left {left; right; expr}) :: path in
      ident folder path' a >>= fun a ->
      open_all folder path expr (folder#open_add a left) right
  and mt folder path = function
    | Resolved _ -> assert false (* to be removed *)
    | Alias id -> ident folder (Mt Alias :: path) id >>| folder#alias
    | Ident ids ->
      path_expr folder (Mt Ident :: path) ids >>| folder#mt_ident
    | Sig items ->
      m2l folder (Mt Sig :: path) folder#m2l_init items >>| folder#mt_sig
    | Fun {arg = None; body } ->
      mt folder (Mt (Fun_right None)::path) body >>| folder#mt_fun None
    | Fun {arg = Some arg; body } ->
      let arg_path = Mt(Fun_left(arg.name,body))::path in
      mt folder arg_path arg.signature >>= fun signature ->
      let arg = Some { arg with signature } in
      mt folder (Mt(Fun_right arg)::path) body >>|
      folder#mt_fun arg
    | With {body;deletions;access=a} ->
      let access_path = Mt (With_access {body;deletions})::path in
      access folder access_path a >>= fun access ->
      mt folder (Mt (With_body {access;deletions})::path) body >>|
      folder#mt_with access deletions
    | Of m -> me folder (Mt Of :: path) m >>| folder#mt_of
    | Extension_node {name;extension=e} ->
      ext folder (Mt (Extension_node name)::path)  e >>| folder#mt_ext name
    | Abstract -> Ok folder#sig_abstract
  and bind_rec folder path left = function
    | L.[] -> Ok left
    | {name;expr} :: right ->
      me folder (Expr(Bind_rec{left; name;right}) :: path) expr >>= fun me ->
      bind_rec folder path (folder#bind_rec_add name me left) right
  and path_expr_args folder path main left = function
    | L.[] -> Ok (folder#path_expr main left)
    | (n, arg) :: right ->
      let path_arg = Path_expr (Arg {main;left;pos=n;right})::path in
      path_expr folder path_arg arg >>= fun x ->
      path_expr_args folder path main (folder#path_expr_arg n x left) right
  and path_expr folder ctx {Paths.Expr.path; args}: (Paths.Expr.t,_) result =
    ident folder (Path_expr (Main args) :: ctx) path >>= fun main ->
    path_expr_args folder ctx main folder#path_expr_arg_init args
  and minor folder path mn =
    let i = folder#packed_init in
    packed folder path mn.access mn.values i mn.packed >>= fun packed ->
    let fpath = Annot (Access {packed; values=mn.values}) :: path in
    access folder fpath mn.access >>= fun access ->
    values folder path packed access mn.values
  and values folder path packed access values =
    m2ls folder path packed access folder#value_init values >>|
    folder#annot packed access
  and m2ls folder path packed access left = function
    | L.[] -> Ok (folder#values left)
    | a :: right ->
      let fpath = Annot (Values {packed;access; left; right}) :: path in
      m2l folder fpath folder#m2l_init a >>= fun a ->
      m2ls folder path packed access (folder#value_add a left) right
  and packed folder path access values left = function
    | L.[] -> Ok left
    | (a: _ Loc.ext) :: right ->
      let fpath =
        Annot (Packed { access; values; left ; loc = a.loc; right})::path in
      me folder fpath a.data >>= fun m ->
      packed folder path access values (folder#add_packed a.loc m left) right
  and access folder path s =
    access_step folder path folder#access_init (Paths.S.Map.bindings s)
  and access_step folder path left = function
    | [] -> Ok (folder#access left)
    | (a, (loc,edge)) :: right ->
      ident folder (Access {left;loc;edge;right}::path) a >>= fun a ->
      access_step folder path (folder#access_add a loc edge left) right
  and ext folder path = function
    | Module m -> m2l folder (Ext Module :: path) folder#m2l_init m
      >>| folder#ext_module
    | Val v -> minor folder (Ext Val :: path) v >>| folder#ext_val
  and ident folder path s =
    match folder#ident s with
    | Error () -> Error ( { path; focus=s })
    | Ok _ as x -> x

  let rec restart (f:id_core) z: (t, Paths.S.t zipper) result =
    match f#ident z.focus with
    | Error _ -> Error z
    | Ok x -> self_restart f @@ match (z.path:Paths.S.t path) with
      | Me Ident :: rest ->
        restart_me f (rest: module_expr path) (f#me_ident x)
      | Me (Open_me_left {left;right;expr}) :: path ->
        open_all f path expr (f#open_add x left) right >>=
        open_right f path expr >>= fun me ->
        restart_me f (path:module_expr path) me
      | Access a :: rest ->
        access_step f rest (f#access_add x a.loc a.edge a.left) a.right
        >>= restart_access f (rest: waccess path)
      | Mt Alias :: path ->
        restart_mt f (path: module_type path) (f#alias x)
      | Path_expr Main args :: path ->
        path_expr_args f path x f#path_expr_arg_init args >>=
        restart_path_expr f (path: Paths.Expr.t path)
      | _ -> .
  and self_restart f = function
    | Error z -> restart f z
    | Ok _ as x -> x
  and restart_me f path x = match path with
    | Expr Include :: rest ->
      restart_expr f (rest: expression path) (f#expr_include x)
    | Expr Open :: rest ->
      restart_expr f (rest: expression path) (f#expr_open x)
    | Annot (Packed p) :: path ->
      let left = f#add_packed p.loc x p.left in
      packed f path p.access p.values left p.right  >>= fun packed ->
      let fpath = Annot (Access {packed; values=p.values}) :: path in
      access f fpath p.access >>= fun access ->
      values f path packed access p.values >>=
      restart_annot f (path: annotation path)
    | Me (Apply_left xx) :: path ->
      me f (Me (Apply_right x)::path) xx
      >>| f#apply x
      >>= restart_me f path
    | Mt Of :: path -> restart_mt f (path: module_type path) (f#mt_of x)
    | Me(Apply_right fn) :: path -> restart_me f path (f#apply fn x)
    | Me(Fun_right fn) :: path ->
      restart_me f path (f#me_fun fn x)
    | Me (Constraint_left mty) :: path ->
      mt f (Me (Constraint_right x)::path) mty >>= fun mt ->
      restart_me f path (f#me_constraint x mt)
    | Me (Open_me_right opens) :: path ->
      restart_me f path (f#open_me opens x)
    | Expr (Bind name) :: path ->
      restart_expr f (path: expression path) (f#bind name x)
    | Expr (Bind_rec {left;name;right}) :: path  ->
      let left = f#bind_rec_add name x left in
      bind_rec f path left right >>| f#bind_rec >>=
      restart_expr f (path: expression path)
    | _ -> .
  and restart_expr f path x = match path with
    | M2l {left;loc;right} :: path ->
      m2l f path (f#m2l_add loc x left) right >>=
      restart_m2l f (path: m2l path)
    | _ -> .
  and restart_mt f path x = match path with
    | Expr (Bind_sig name) :: path ->
      restart_expr f (path: expression path) (f#bind_sig name x)
    | Me Fun_left(name,body) :: path ->
      let arg = Some {Arg.signature = x; name } in
      me f (Me(Fun_right arg) :: path ) body >>|
      f#me_fun arg >>= restart_me f path
    | Mt Fun_left (name,body) :: path ->
     let arg = Some {Arg.signature = x; name } in
      mt f (Mt(Fun_right arg) :: path ) body >>|
      f#mt_fun arg >>= restart_mt f path
    | Mt Fun_right arg :: path ->
      restart_mt f path (f#mt_fun arg x)
    | Mt With_body {access;deletions} :: path ->
      restart_mt f path (f#mt_with access deletions x)
    | Me Constraint_right body :: path ->
      restart_me f (path:module_expr path) (f#me_constraint body x)
    | Expr SigInclude :: path ->
      restart_expr f (path: expression path) (f#sig_include x)
    | _ -> .
  and restart_path_expr f path x = match path with
    | Mt Ident :: path ->
      restart_mt f (path:module_type path) (f#mt_ident x)
    | Path_expr Arg {main;left;pos;right} :: path ->
      let left = f#path_expr_arg pos x left in
      path_expr_args f path main left right >>=
      restart_path_expr f path
    | _ -> .
  and restart_access f  path x = match path with
    | Annot (Access mn) :: path ->
      values f path mn.packed x mn.values >>=
      restart_annot f (path: annotation path)
    | Mt With_access {deletions;body} :: path ->
      mt f (Mt(With_body {deletions;access=x}) :: path ) body
      >>| f#mt_with x deletions
      >>= restart_mt f path
    | _ -> .
  and restart_annot f path x = match path with
    | Expr Minor :: path ->
      restart_expr f (path:expression path) (f#minor x)
    | Me Val :: path ->
      restart_me f (path:module_expr path) (f#me_val x)
    | Ext Val :: path ->
      restart_ext f (path:extension_core path) (f#ext_val x)
    | _ -> .
  and restart_ext f path x = match path with
    | Expr (Extension_node name) :: path ->
      restart_expr f (path:expression path) (f#expr_ext name x)
    | Me (Extension_node name) :: path ->
      restart_me f (path:module_expr path) (f#me_ext name x)
    | Mt (Extension_node name) :: path ->
      restart_mt f (path:module_type path) (f#mt_ext name x)
    | _ -> .
  and restart_m2l f path x = match path with
    | [] -> Ok x
    | Me Str :: path -> restart_me f (path:module_expr path) (f#str x)
    | Mt Sig :: path -> restart_mt f (path:module_type path) (f#mt_sig x)
    | Annot (Values {packed;access;left;right}) :: path ->
      let left = f#value_add x left in
      m2ls f path packed access left right >>| f#annot packed access
      >>= restart_annot f (path:annotation path)
    | Ext Module :: path ->
      restart_ext f (path: extension_core path) (f#ext_module x)
    | _ :: _ -> .
  let ustart f x =m2l f [] f#m2l_init x
end

open L
class id = object
  inherit id_core
  method ident s = Ok s

  method abstract: module_expr = Abstract
  method access l: access = l
  method access_add p loc edge a = Paths.S.Map.add p (loc,edge) a
  method access_init = Paths.S.Map.empty
  method add_packed loc me l = {Loc.loc;data=me} :: l
  method alias x = Alias x
  method apply f x = Apply {f;x}
  method annot packed access values = {packed;access;values}
  method bind name me = Bind {name; expr = me }
  method bind_rec l = Bind_rec l
  method bind_rec_add name expr l = {name;expr} :: l
  method bind_rec_init = []
  method bind_sig name mt = Bind_sig {name;expr=mt}
  method expr_ext name ext = (Extension_node {name;extension=ext}:expression)
  method expr_include x: expression = Include x
  method expr_open x: expression = Open x
  method ext_module m = Module m
  method ext_val m: extension_core = Val m
  method m2l_add loc x l = {Loc.loc; data=x} :: l
  method m2l_init = []
  method m2l l = List.rev l
  method me_constraint m mt: module_expr = Constraint(m,mt)
  method me_ext name extension: module_expr = Extension_node {name;extension}
  method me_fun arg body: module_expr = Fun {arg;body}
  method me_ident x: module_expr = Ident x
  method me_val x: module_expr = Val x
  method minor annot = Minor annot
  method mt_ext name extension: module_type = Extension_node {name;extension}
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
  method sig_include m = SigInclude m
  method sig_abstract: module_type = Abstract
  method str x = (Str x: module_expr)
  method unpacked = Unpacked
  method value_add m2l l = m2l :: l
  method value_init = []
  method values l = List.rev l

end

class failing_id = object
  inherit id
  method! ident s = if Random.float 1. > 0.99 then Ok s else Error ()
end

let failing_deep_id x =
  let f = new failing_id in
  let rec try_at_most n x =
    match x with
      | Ok x -> x
      | Error e ->
        try_at_most (n-1) (Fold.restart f e) in
  try_at_most 1000 (Fold.ustart f x)
