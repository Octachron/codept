
module M = Module

module Arg = M.Arg
module Sm = Summary
module Def = Summary.Def

module P = M.Partial

type 'a bind = {name:Name.t; expr:'a}

module Edge = struct
  type t = Normal | Epsilon
  let max x y = if x = Epsilon then Epsilon else y
end

type kind = Structure | Signature


type expression =
  | Defs of Summary.t (** Resolved module actions M = … / include … / open … *)
  | Open of Paths.Simple.t (** [open A.B.C] ⇒ [Open [A;B;C]]  *)
  | Include of module_expr (** [struct include A end] *)
  | SigInclude of module_type
  (** [struct include A end] *)
  | Bind of module_expr bind
  (** [struct module A = struct … end] *)
  | Bind_sig of module_type bind   (** [struct module type A = sig … end] *)
  | Bind_rec of module_expr bind list
  (** [module rec A = … and B = … and …] *)
  | Minor of annotation
  (** value level expression.
      Invariant: for any pure interpreter [f], [ f (Minor m :: q ) ≡ f q ],
      i.e, this expression constructor is only meaningful for dependencies
      tracking.
 *)
  | Extension_node of extension
  (** [[%ext …]] *)
and annotation =
  { access: Edge.t Name.map
  (** [M.N.L.x] ⇒ access \{M = Normal \}
      type t = A.t ⇒ access \{ M = ε \}
  *)
  ; values: m2l list (** − [let open A in …] ⇒ [[ Open A; …  ]]
                         − [let module M = … ] ⇒ [[ Include A; … ]]
                     *)
  ; packed: module_expr list (** [(module M)] *)
  }
and module_expr =
  | Resolved of P.t
  (** Already resolved module expression, generally
      used in subexpression when waiting for other parts
      of module expression to be resolved
  *)
  | Ident of Paths.Simple.t (** [A.B…] **)
  | Apply of {f: module_expr; x:module_expr} (** [F(X)] *)
  | Fun of module_expr fn (** [functor (X:S) -> M] *)
  | Constraint of module_expr * module_type (** [M:S] *)
  | Str of m2l (** [struct … end] *)
  | Val of annotation (** [val … ] *)
  | Extension_node of extension (** [[%ext …]] *)
  | Abstract
  (** empty module expression, used as a placeholder.
      In particular, it is useful for constraining first class module unpacking
      as [Constraint(Abstract, signature)]. *)
  | Unpacked (** [(module M)] *)
  | Open_me of { resolved: Summary.t; opens:Paths.Simple.t list; expr:module_expr}
  (** M.(…N.( module_expr)…)
      Note: This construction does not exist (yet?) in OCaml proper.
      It is used here to simplify the interaction between
      pattern open and first class module.*)
and module_type =
  | Resolved of P.t (** same as in the module type *)
  | Alias of Paths.Simple.t (** [module m = A…]  *)
  | Ident of Paths.Expr.t
  (** module M : F(X).s
      Note: Paths.Expr is used due to [F.(X).s] expressions
      that do not have an equivalent on the module level
 *)
  | Sig of m2l (** [sig … end] *)
  | Fun of module_type fn (** [functor (X:S) → M] *)
  | With of {
      body: module_type;
      deletions: Name.set
      (* ; equalities: (Npath.t * Epath.t) list *)
    }
  (** [S with module N := …]
      we are only tracking module level modification
  *)
  | Of of module_expr (** [module type of …] *)
  | Extension_node of extension (** [%%… ] *)
  | Abstract (** placeholder *)
and extension = {name:string; extension:extension_core}
and extension_core =
  | Module of m2l
  | Val of annotation
and m2l = expression Loc.ext list
and 'a fn = { arg: module_type Arg.t option; body:'a }

type t = m2l

let annot_empty = { access=Name.Map.empty; values = []; packed = [] }




(** S-expression serialization *)
module More_sexp = struct
  open Sexp
  module R = Sexp.Record

  let fix r impl = fix (impl r)
  let fix' r impl = fix' (impl r)
  type recursive_sexp =
    {
      expr: recursive_sexp -> unit -> (expression, one_and_many) Sexp.impl;
      me: recursive_sexp -> unit -> (module_expr, one_and_many) Sexp.impl;
      mt: recursive_sexp -> unit -> (module_type, one_and_many) Sexp.impl;
      annot: recursive_sexp -> unit -> (annotation,many) Sexp.impl;
      ext: recursive_sexp -> unit -> (extension,many) Sexp.impl
    }

  (** Expression *)

  let definition =
    let open Summary in
    convr
      (pair Module.Sig.sexp Module.Sig.sexp)
      (fun (a,b) -> {visible=b;defined=a})
      (fun d -> d.defined, d.visible )

  let defs = C {
    name= "Defs";
    proj = (function Defs d -> Some d | _ -> None);
    inj = (fun x -> Defs x);
    impl = definition;
    default = Some Summary.empty
  }

  let op_n = C {
      name = "Open";
      proj = (function Open p -> Some p | _ -> None);
      inj = (fun x -> Open x);
      impl = list string;
      default = None
    }

  let includ_ r = C {
      name = "Include";
      proj = (function Include i -> Some i | _ -> None);
      inj = (fun x -> Include x);
      impl = fix' r r.me;
      default = None
    }

  let sig_include r = C {
      name = "SigInclude";
      proj = (function SigInclude i -> Some i | _ -> None);
      inj = (fun x -> SigInclude x);
      impl = fix' r r.mt;
      default = None
    }

  let bind_c r core = conv
      { f = (fun (name,expr) -> {name;expr});
        fr = (fun x -> x.name, x.expr)
      } @@
    pair string (fix' r core)

  let bind r = C {
      name = "Bind";
      proj = (function Bind b -> Some b | _ -> None );
      inj = (fun x -> Bind x);
      impl = bind_c r r.me;
      default = None
    }

  let bind_sig r = C {
      name = "Bind_sig";
      proj = (function Bind_sig b -> Some b | _ -> None );
      inj = (fun x -> Bind_sig x);
      impl = bind_c r r.mt;
      default = None
    }

  let bind_rec r =
    C {
      name = "Bind_rec";
      proj = (function Bind_rec b -> Some b | _ -> None );
      inj = (fun x -> Bind_rec x);
      impl = list @@ bind_c r r.me;
      default = None;
    }

    let minor r =
    C {
      name = "Minor";
      proj = (function Minor mn -> Some mn | _ -> None );
      inj = (fun x -> Minor x);
      impl = fix r r.annot;
      default = Some annot_empty;
    }

  let extension_node r =
    C {
      name = "Extension_node";
      proj = (function Extension_node ext -> Some ext | _ -> None );
      inj = (fun x -> Extension_node x);
      impl = fix r r.ext;
      default = None;
    }

  let expr r () =
    sum [ defs; op_n; includ_ r; sig_include r; bind r; bind_sig r;
          bind_rec r; minor r; extension_node r ]

  (** edge *)
  let edge = sum [simple_constr "Normal" Edge.Normal;
                  simple_constr "ε" Edge.Epsilon]

  (** Annotation *)
  let access = R.( key Many "access" Name.Map.empty )
  let values = R.( key Many "values" [] )
  let packed = R.( key Many "packed" [] )

  let nameset = convr (list string) Name.Set.of_list Name.Set.elements

  let namemap = convr
      (list @@ major_minor string Edge.Normal edge)
      (List.fold_left (fun m (k,e) -> Name.Map.add k e m) Name.Map.empty )
      Name.Map.bindings

  let m2l r () = list @@ Loc.Sexp.ext @@ fix' r expr
  let annot r () =
    let r = record R.[field access namemap;
             field values (list @@  fix r m2l);
             field packed (list @@ fix' r r.me)
            ] in
    let f x = { access = R.get access x; values = R.get values x;
                packed = R.get packed x
              } in
    let fr r = R.(create [ access := r.access; values := r.values;
                           packed := r.packed ]
                 ) in
    conv {f;fr} r

  (** Module expr *)

  let resolved =
    C { name="Resolved"; proj =(function Resolved p -> Some p | _ -> None);
        inj = (fun p -> Resolved p); impl = Module.Partial.sexp;
        default = None
      }

  let ident =
    C { name="Ident"; proj=(function Ident p->Some p|_->None);
        inj=(fun p ->Ident p); impl = Paths.Simple.sexp;
        default = None;
      }

  let apply r =
    C { name="Apply"; proj=( function Apply {f;x} -> Some (f,x) | _ -> None );
        inj = (fun (f,x) -> Apply {f;x}); impl = pair (fix' r r.me) (fix' r r.me);
        default = None
      }

  let fn r inner =
    convr (pair (opt @@ Module.Arg.sexp @@ fix' r r.mt) inner)
      (fun (arg,body) -> {arg; body})
      (fun r -> r.arg, r.body)

  let func r =
    C { name = "Fun"; proj = (function Fun f -> Some f | _ -> None);
        inj = (fun f -> Fun f); impl = fn r (fix' r r.me);
        default = None
      }

  let constraint_ r =
    let proj = (function Constraint(a,b) -> Some (a, b)| _ -> None) in
    C { name = "Constraint"; proj;
        inj = (fun (a,b) -> Constraint(a,b));
        impl = pair (fix' r r.me) (fix' r r.mt);
        default = Some (Abstract, Sig [] ) (* module M [: sig end]*)
      }

  let str r =
    C { name = "Str"; proj = (function Str l -> Some l| _ -> None);
        inj = (fun l -> Str l); impl = fix r m2l;
        default = Some []
      }

  let val_ r =
    C { name = "Val"; proj = (function Val a -> Some a| _ -> None);
        inj = (fun a -> Val a); impl = fix r r.annot;
        default = Some annot_empty
      }

  let extension_node r =
    C { name = "Extension_node";
        proj = (function (Extension_node a:module_expr) -> Some a | _ -> None);
        inj = (fun a -> Extension_node a); impl = fix r r.ext;
        default = None
      }

  let abstract = simple_constr "Abstract" Abstract
  let unpacked = simple_constr "Unpacked" Unpacked

  let open_me r =
    C{ name="Open_me";
       proj = (function
           | Open_me {resolved; opens; expr} -> Some( resolved,(opens,expr))
           | _ -> None );
       inj= (fun (a, (b,c)) -> Open_me {resolved=a; opens = b ; expr = c} );
       impl = pair definition (pair (list Paths.Simple.sexp) (fix' r r.me) );
       default = None;
     }


  let me r () = sum [resolved;ident;apply r; func r; constraint_ r;
                  str r; val_ r; extension_node r; abstract; unpacked;
                  open_me r]

  let resolved_t =
    C { name="Resolved";
        proj =(function (Resolved p:module_type) -> Some p | _ -> None);
        inj = (fun p -> Resolved p); impl = Module.Partial.sexp;
        default = None;
      }

  let alias =
    C { name="Alias";
        proj =(function Alias p -> Some p | _ -> None);
        inj = (fun p -> Alias p); impl = Paths.Simple.sexp;
        default = None
      }

  let ident_t =
    C { name="Ident";
        proj =(function (Ident p:module_type) -> Some p | _ -> None);
        inj = (fun p -> Ident p); impl = Paths.Expr.sexp;
        default = None
      }

  let sig_ r =
    C { name="Sig";
        proj =(function Sig s -> Some s | _ -> None);
        inj = (fun s -> Sig s); impl = fix r m2l;
        default = Some [];
      }

  let fun_t r =
    C { name = "Fun"; proj = (function (Fun f: module_type) -> Some f | _ -> None);
        inj = (fun f -> Fun f); impl = fn r (fix' r r.mt);
        default = None
      }

  let with_ r =
    C { name="With";
        proj =(function With {body;deletions} -> Some (body,deletions) | _ -> None);
        inj = (fun (a,b) -> With {body=a;deletions=b} );
        impl = major_minor (fix' r r.mt) Name.Set.empty nameset;
        default = None
      }

    let of_ r =
    C { name="Of";
        proj =(function Of me -> Some me | _ -> None);
        inj = (fun me -> Of me); impl = (fix' r r.me);
        default = None;
      }

    let extension_node_t r =
      C { name = "Extension_node";
          proj = (function (Extension_node a:module_type) -> Some a | _ -> None);
          inj = (fun a -> Extension_node a); impl = fix r r.ext;
          default = None
        }

    let abstract_t = simple_constr "Abstract" (Abstract:module_type)

    let mt r () = sum [ resolved_t; alias; ident_t; sig_ r; fun_t r; with_ r;
                     of_ r; extension_node_t r; abstract_t ]

    let ext_mod r =
      C {name = "Module"; proj = (function Module m2l -> Some m2l | _ -> None );
         inj = (fun m2l -> Module m2l); impl = fix r m2l;
         default = Some []
        }

    let ext_val r =
      C {name = "Val";
         proj = (function (Val mn: extension_core) -> Some mn | _ -> None );
         inj = (fun mn -> Val mn); impl = (fix r r.annot);
         default = Some annot_empty
        }

    let ext_core r = sum [ ext_mod r; ext_val r ]

    let  ext r () = convr (pair string @@ ext_core r)
        (fun (a,b) -> {name=a;extension=b} )
        (fun r -> r.name, r.extension)

    let recursive = { expr; me; mt; ext; annot }
    let m2l = m2l recursive ()
    let expr = expr recursive ()
    let me = me recursive ()
    let mt = mt recursive ()
    let annot = annot recursive ()

end

let sexp = More_sexp.m2l

(** The Block module computes the first dependencies needed to be resolved
    before any interpreter can make progress evaluating a given code block *)
module Block = struct

  let (+|) = Summary.Def.(+|)
  let either x f y =
    Mresult.Error.bind ( fun def ->
        Mresult.fmap
          (fun def' -> def +| def' )
          (fun (def', name) -> ( def +| def', name) )
          (f y)
          ) x

  let rec first acc f = function
    | [] -> Error acc
    | a :: q -> match f acc a with
      | Error acc -> first acc f q
      | Ok _ as ok ->  ok

  let data x = x.Loc.data

  let lift def = function
    | None -> Error def
    | Some x -> Ok(def,x)

  let rec me defs =
    let err = Error defs in
    let ok name = Ok(defs, name) in
    function
    | Resolved _ -> err
    | Ident n -> ok (List.hd n)
    | Apply {f; x} -> either (me defs f) (me defs)  x
    | Fun fn ->
      either
        Mresult.Ok.( (lift defs fn.arg) >>= fun (defs, {Arg.signature;_}) ->
                     mt defs signature)
        (me defs) fn.body
    | Constraint (e,t) -> either (me defs e) (mt defs) t
    | Str code -> Mresult.Ok.fmap data @@ m2l defs code
    | Val _ | Extension_node _ | Abstract | Unpacked -> err
    | Open_me {opens = []; expr; _ } -> me defs expr
    | Open_me {opens = a::_ ; _ } -> ok (List.hd a)
  and mt defs =
    let err = Error defs in
    let ok x = Ok(defs, x) in
    function
    | Resolved _ -> err
    | Alias n -> ok (List.hd n)
    | Ident e ->  ok (Paths.Expr.prefix e)
    | Sig code -> Mresult.Ok.fmap data @@ m2l defs code
    | Fun fn ->
      either Mresult.Ok.( (lift defs fn.arg) >>= fun (defs, {Arg.signature;_}) ->
                          mt defs signature)
        (mt defs) fn.body
    | With { body; _ } -> mt defs body
    | Of e -> me defs e
    | Extension_node _
    | Abstract -> err
  and expr defs  =
    let err = Error defs and ok name = Ok(defs, name) in
    function
    | Defs d -> Error ( defs +| d )
    | Open p -> ok ( List.hd p)
    | Include e -> me defs e
    | SigInclude t -> mt defs t
    | Bind {expr;_} -> me defs expr
    | Bind_sig {expr;_} -> mt defs expr
    | Bind_rec l ->
      first defs (fun defs b -> me defs b.expr) l
    | Minor m -> minor defs m
    | Extension_node _ -> err
  and expr_loc defs {Loc.loc;data} =
      Mresult.Ok.fmap (fun data -> {Loc.loc;data}) @@ expr defs data
  and m2l defs code = first defs expr_loc code
  and minor defs m =
    if Name.Map.cardinal m.access > 0 then
      Ok (defs, fst @@ Name.Map.choose m.access)
    else
      either Mresult.Ok.(first defs m2l m.values >>| data) (first defs me) m.packed

  let resolve_alias find name =
    match (find name: Module.t) with
    | Alias {path = name :: _ ; _ } -> name
    | _ -> name
    | exception Not_found -> name

  let find y name =
    let defined = y.Summary.defined in
    let rec find defs name =
      match defs with
      | Module.Blank -> raise Not_found
      | Module.Exact d -> Name.Map.find name d.Module.modules
      | Module.Divergence d ->
        match Name.Map.find name d.after.modules with
        | x -> x
        | exception Not_found -> find d.before name in
    find defined name

  let m2l code =
    match m2l Summary.empty code with
    | Error _ -> None
    | Ok x -> let defs, n = x.data in
      Some { x with data = resolve_alias (find defs) n }


end

module Annot = struct
  open Loc
  type t = annotation Loc.ext
  let empty = { data = annot_empty; loc = Nowhere }
  let is_empty x  = x.data = annot_empty

  module Access = struct
    type t = Edge.t Name.map
    let merge = Name.Map.merge (fun k x y -> match x, y with
        | Some x, Some y -> Some (Edge.max x y)
        | None, (Some _ as x) | (Some _ as x), None -> x
        | None, None -> None
      )

    let empty = Name.Map.empty
  end

  let merge x y =
    let a1 = x.data and a2 = y.data in
    let data =
    { access= Access.merge a1.access a2.access;
      values = a1.values @ a2.values;
      packed = a1.packed @ a2.packed
    } in
    {Loc.data; loc = Loc.merge x.loc y.loc}


  let (++) = merge

  let union l =
    List.fold_left (++) empty l
  let union_map f l =
    List.fold_left (fun res x -> res ++ f x ) empty l

  let access name =
    Loc.create name.loc { annot_empty with
                          access = Name.Map.singleton name.data Edge.Normal }

  let abbrev name =
    Loc.create name.loc { annot_empty with
                          access = Name.Map.singleton name.data Edge.Epsilon }


  let value v =
    let loc =
      let ext acc x = Loc.merge acc x.loc in
      List.fold_left (List.fold_left ext) Nowhere v in
    Loc.create loc { annot_empty with values = v }

  let pack o = Loc.create o.loc { annot_empty with packed = o.data }

  let opt f x = Option.( x >>| f >< empty )

end

(** Helper function *)
module Build = struct
  let ghost data  = {Loc.data; loc= Nowhere }

  let minor x = Minor x
  let access path =
    Loc.fmap minor @@ Annot.access @@ Loc.fmap Paths.Expr.prefix path

  let open_ path = Loc.fmap (fun x -> Open x) path
  let value v = Loc.fmap minor @@ Annot.value v
  let pack o = Loc.fmap minor @@ Annot.pack o

  let open_me ml expr = match expr with
    | Open_me o -> Open_me { o with opens = ml @ o.opens }
    | expr ->  Open_me { resolved = Summary.empty; opens = ml; expr }


  let demote_str fn arg =
    let body = Fun fn in
    match arg with
    | None -> { arg=None; body }
    | Some ({name;signature}: _ Arg.t) ->
      { arg = Some {name; signature=Sig [Loc.nowhere @@ Defs signature]}; body }

  let demote_sig fn arg : module_type fn  =
    let body : module_type = Fun fn in
    match arg with
    | None -> { arg=None; body }
    | Some ({name;signature}: _ Arg.t) ->
      { arg = Some {name; signature=Sig [Loc.nowhere @@ Defs signature]}; body }

  let fn_sig arg : module_type = Fun arg
  let fn arg : module_expr  = Fun arg

end


let rec pp_expression ppf = function
  | Defs defs -> Pp.fp ppf "define %a" Summary.pp defs

  | Minor annot -> pp_annot ppf annot
  | Open npath -> Pp.fp ppf "@[<hv>open %a@]" Paths.Simple.pp npath
  | Include me -> Pp.fp ppf "@[<hv>include [%a]@]" pp_me me
  | SigInclude mt -> Pp.fp ppf "@[<hv>include type [%a]@]" pp_mt mt

  | Bind bind -> pp_bind ppf bind
  | Bind_sig bind -> pp_bind_sig ppf bind
  | Extension_node e -> pp_extension ppf e
  | Bind_rec bs ->
    Pp.fp ppf "rec@[<hv>[ %a ]@]"
      (Pp.(list ~sep:(s "@, and @,")) @@ pp_bind ) bs
and pp_annot ppf {access;values; packed} =
  Pp.fp ppf "%a%a%a"
    pp_access access
    Pp.(opt_list ~sep:(s " @,") ~pre:(s "@,values: ") pp) values
    Pp.(opt_list ~sep:(s " @,") ~pre:(s "packed: ") pp_opaque) packed
and pp_access ppf s =  if Name.Map.cardinal s = 0 then () else
    Pp.fp ppf "access:@[<hv>{%a}@]" (Pp.list pp_access_elt) (Name.Map.bindings s)
and pp_access_elt ppf (name,edge) =
  Pp.fp ppf "%s%s" (if edge = Edge.Normal then "" else " -ε->") name
and pp_opaque ppf me = Pp.fp ppf "⟨%a⟩" pp_me me
and pp_bind ppf {name;expr} =
  match expr with
  | Constraint(Abstract, Alias np) ->
    Pp.fp ppf "@[module %s ≡ %a" name Paths.Simple.pp np
  | Constraint(Abstract, mt) ->
    Pp.fp ppf "@[module %s:@[<hv>%a@] @]" name pp_mt mt
  | Constraint(Unpacked, mt) ->
    Pp.fp ppf "@[(module %s:@[<hv>%a@])@]" name pp_mt mt
  | Unpacked ->
    Pp.fp ppf "(module %s)" name
  | _ ->
    Pp.fp ppf "@[module %s = @,@[<hv>%a@] @]" name pp_me expr
and pp_bind_sig ppf {name;expr} =
  match expr with
  | Alias id ->
    Pp.fp ppf "@[module type %s ≡ %a @]" name Paths.Simple.pp id
  | Abstract ->
    Pp.fp ppf "@[module type %s@]" name
  | _ ->
    Pp.fp ppf "@[module type %s = @,@[<hv>%a@] @]" name pp_mt expr
and pp_me ppf = function
  | Resolved fdefs -> Pp.fp ppf "✔%a" P.pp fdefs
  | Ident np -> Paths.Simple.pp ppf np
  | Str m2l -> Pp.fp ppf "@,struct@, %a end" pp m2l
  | Apply {f;x} -> Pp.fp ppf "%a(@,%a@,)" pp_me f pp_me x
  | Fun { arg; body } -> Pp.fp ppf "%a@,→%a" (Arg.pp pp_mt) arg pp_me body
  | Constraint (me,mt) -> Pp.fp ppf "%a: @,%a" pp_me me pp_mt mt
  | Val annot -> Pp.fp ppf "⟨val %a⟩" pp_annot annot
  | Extension_node ext -> Pp.fp ppf "%a" pp_extension ext
  | Abstract -> Pp.fp ppf "⟨abstract⟩"
  | Unpacked -> Pp.fp ppf "⟨unpacked⟩"
  | Open_me {opens = a :: q ; resolved; expr} ->
    Pp.fp ppf "%a.(%a)" Paths.Simple.pp a pp_me (Open_me{opens=q;resolved;expr})
  | Open_me {opens=[]; resolved; expr} ->
    Pp.fp ppf "⟨context:%a⟩ %a" Summary.pp resolved pp_me expr

and pp_mt ppf = function
  | Resolved fdefs -> Pp.fp ppf "✔%a" P.pp fdefs
  | Alias np -> Pp.fp ppf "(≡)%a" Paths.Simple.pp np
  | Ident np -> Paths.Expr.pp ppf np
  | Sig m2l -> Pp.fp ppf "@,sig@, %a end" pp m2l
  | Fun { arg; body } ->  Pp.fp ppf "%a@,→%a" (Arg.pp pp_mt) arg pp_mt body
  | With {body; deletions} ->
    Pp.fp ppf "%a@,/%a" pp_mt body Name.Set.pp deletions
  | Of me -> Pp.fp ppf "module type of@, %a" pp_me me
  | Extension_node ext -> Pp.fp ppf "%a" pp_extension ext
  | Abstract -> Pp.fp ppf "⟨abstract⟩"
and pp_extension ppf x = Pp.fp ppf "[%%%s @[<hv>%a@]]" x.name pp_extension_core
    x.extension
and pp_extension_core ppf = function
  | Module m -> pp ppf m
  | Val m -> pp_annot ppf m
and pp ppf = Pp.fp ppf "@[<hv2>[@,%a@,]@]"
    Pp.(list ~sep:(s " @,") pp_expression_with_loc)
and pp_expression_with_loc ppf e = Pp.fp ppf "%a(%a)"
    pp_expression e.data Loc.pp e.loc


(** {Normalize} computes the normal form of a given m2l code fragment *)
module Normalize = struct

  let halt l = false, l
  let continue l = true, l


  let (+:) x (more,l) =
    more, x :: l

  let cminor x = Minor x

  let rec all : m2l -> bool * m2l  = function
    | {data=Defs d1;_} :: {data=Defs d2; _ } :: q ->
      all @@ (Loc.nowhere @@ Defs Def.(  d1 +| d2)) :: q
    | {data = Defs _; _ } as e :: q ->
      let more, q = all q in more, e :: q
    | { data = Minor m; loc } :: q ->
      Loc.fmap cminor (minor {Loc.data=m;loc})  +: all q
    | { data = (Open _ | Include _ | SigInclude _ | Bind_sig _ | Bind _ | Bind_rec _
      | Extension_node _ ); _ }
      :: _ as l ->
      halt l
    | [] -> halt []
  and minor v =
    List.fold_left value (Loc.fmap (fun data -> { data with values = [] }) v)
      v.data.values
  and value mn p =
    match snd @@ all p with
    | [] -> mn
    | { data = Minor m; loc } :: q ->
      let mn = Annot.merge mn {data=m;loc} in
         { mn with data = { mn.data with values = q :: mn.data.values } }
    | l -> Loc.fmap (fun mn -> { mn with values = l :: mn.values }) mn

end


module Sig_only = struct

  let (|||) (b,x) (b',y) = b || b', (x,y)
  let (@::) a (b,l) = (b, a :: l)
  let (@:::) (b,a) (b',l) = (b || b', a :: l)

  let map f (b, x) = (b, f x)


  let sig_arg inner = function
    | None as a -> false, a
    | Some r ->
      map (fun s -> Some { r with Arg.signature = s }) (inner r.signature )

  let rec rev keep_opens = function
    | { Loc.data = Defs _ ; _ } :: l -> rev keep_opens l
    | { data = Minor _; _ } :: l -> rev keep_opens l
    | { data= Open _; _ }  as o :: q ->
      if keep_opens then  o @:: rev keep_opens q
      else rev keep_opens q
    | { data = Extension_node _; _ } as a :: q  -> a @:: rev true q
    | { data = (SigInclude _| Include _); _ }  as a :: q -> a @:: rev true q
    | { data = Bind {name;expr}; loc }  :: q ->
      let k, expr = mex expr in
      { Loc.data = Bind{name;expr}; loc } @:: rev (k||keep_opens) q
    | { data = (Bind_sig  _ | Bind_rec _); _ }  as a :: q
      -> a @:: rev true q
    | [] -> false, []
  and main l = map List.rev (rev false @@ List.rev l)
  and mex = function
    | (Resolved _ | Val _ | Abstract | Unpacked )  as a-> false, a
    | (Ident _ | Apply _ | Extension_node _ ) as a -> true, a
    | Fun {arg;body} ->
      let b, arg = sig_arg mty arg in
      let b', body = mex body in
      b || b', Fun {arg;body}
    | Constraint (me,mt) ->
      map (fun (x,y) -> Constraint (x,y)) (mex me ||| mty mt)
    | Str s ->
      map (fun x -> Str x) (main s)
    | Open_me ({ expr; _ } as r) ->
      let b, expr = mex expr in
      b , Open_me { r with expr}
  and mty = function
    | (Resolved _ | Abstract as a ) -> false, a
    | (Alias _ | Ident _ | Extension_node _ as a ) -> true, a
    | Sig code -> map (fun x -> Sig x) (main code)
    | Fun {arg;body} ->
      let k, arg = sig_arg mty arg in
      let k', body = mty body in
      k || k', Fun {arg;body}
    | With ({ body; _ } as r) ->
      map (fun x -> With { r with body=x}) (mty body)
    | Of e -> map (fun x -> Of x) (mex e)

  let filter m2l = snd @@ main m2l

end
