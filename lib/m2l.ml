
module M = Module

module Arg = M.Arg

module P = M.Partial

type 'a bind = {name:Name.t; expr:'a}

type kind = Structure | Signature


type expression =
  | Defs of Summary.t (** Resolved module actions M = … / include … / open … *)
  | Open of module_expr (** [open A.B.C] ⇒ [Open [A;B;C]]  *)
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
and access = (Loc.t * Deps.Edge.t) Paths.S.map
and annotation =
  { access: access
  (** [M.N.L.x] ⇒ access \{M = Normal \}
      type t = A.t ⇒ access \{ M = ε \}
  *)
  ; values: m2l list (** − [let open A in …] ⇒ [[ Open A; …  ]]
                         − [let module M = … ] ⇒ [[ Include A; … ]]
                     *)
  ; packed: module_expr Loc.ext list (** [(module M)] *)
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
      deletions: Paths.S.set;
      access: access
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

let annot_empty = { access= Paths.S.Map.empty; values = []; packed = [] }




(** Schematic serialization *)
module Sch = struct
  module S = Paths.S.Set
  open Schematic

  let l x = if x = L.[] then None else Some x
  let (><) = Option.(><)
  module Access = Label(struct let l = "access" end)
  module Values = Label(struct let l = "values" end)
  module Packed = Label(struct let l = "packed" end)


  let rec raw_expr =
    Sum [ "Defs", Summary.sch; "Open", module_expr; "Include_me", module_expr;
          "SigInclude", module_type; "Bind", [String; module_expr];
          "Bind_sig", [String; module_type]; "Bind_rec", Array [String;module_expr];
          "Minor", annotation; "Extension_node", extension ]
  and expr = Custom{fwd=expr_fwd;rev=expr_bwd;id=["M2l"; "expr"] ;sch=raw_expr}
  and expr_fwd = let open Tuple in function
    | Defs s -> C (Z s)
    | Open p -> C (S (Z p))
    | Include me -> C(S (S (Z me)))
    | SigInclude mt -> C(S(S(S(Z mt))))
    | Bind {expr;name} -> C(S(S(S(S(Z [name;expr])))))
    | Bind_sig {expr;name} -> C(S(S(S(S(S(Z [name;expr]))))))
    | Bind_rec rb ->
      C(S(S(S(S(S(S(Z (List.map (fun {expr;name} -> [name;expr]) rb))))))))
    | Minor a -> C(S(S(S(S(S(S(S(Z a))))))))
    | Extension_node ext -> C(S(S(S(S(S(S(S(S(Z ext)))))))))
  and expr_bwd = let open Tuple in function
    | C Z x -> Defs x
    | C S Z x -> Open x
    | C S S Z x -> Include x
    | C S S S Z x -> SigInclude x
    | C S S S S Z [name;expr] -> Bind {name;expr}
    | C S S S S S Z [name;expr] -> Bind_sig {name;expr}
    | C S S S S S S Z x -> Bind_rec (List.map (fun [name;expr] -> {name;expr}) x)
    | C S S S S S S S Z x -> Minor x
    | C S S S S S S S S Z x -> Extension_node x
    | _ -> .
  and expr_loc = Custom { sch = [expr;Loc.Sch.t];
                          fwd = (fun x -> Tuple.[x.Loc.data;x.loc]);
                          rev = Tuple.(fun [data;loc] -> {data;loc});
                          id = [ "M2l"; "with_loc"; "expr"];
                        }
  and m2l = Array expr_loc
  and access = Custom {sch=access_raw;fwd=access_fwd;rev=access_rev;
                       id=["M2l"; "access"]}
  and access_raw = Array [Paths.S.sch; Loc.Sch.t; Deps.Edge.sch]
  and access_fwd x =
    Paths.S.Map.fold (fun k (x,y) l -> L.(Tuple.[k;x;y] :: l)) x L.[]
  and access_rev a = let open Tuple in
    List.fold_left (fun m [k;x;y] ->Paths.S.Map.add k (x,y) m)
      Paths.S.Map.empty a
  and module_expr =
    Custom{ sch = me_raw ; fwd = me_fwd; rev = me_rev; id = ["M2l"; "module_expr"] }
  and me_raw =
    Sum [ "Resolved", Module.Partial.sch; "Ident", Paths.S.sch;
          "Apply", [module_expr;module_expr]; "Fun",[arg; module_expr];
          "Constraint",[module_expr;module_type]; "Str",m2l; "Val",annotation;
          "Extension_node",extension; "Abstract",Void; "Unpacked", Void;
          "Open_me",[Summary.sch; Array Paths.S.sch; module_expr] ]
  and me_fwd = let open Tuple in
    function
    | Resolved x -> C (Z x)
    | Ident x -> C(S (Z x))
    | Apply {f;x} -> C(S (S (Z [f;x])))
    | Fun {arg;body} -> C (S (S ( S (Z [arg;body]))))
    | Constraint (x,y) -> C(S(S(S(S(Z[x;y])))))
    | Str x -> C(S(S(S(S(S(Z x))))))
    | Val x -> C(S(S(S(S(S(S(Z x)))))))
    | Extension_node x -> C(S(S(S(S(S(S(S(Z x))))))))
    | Abstract -> C(S(S(S(S(S(S(S(S E))))))))
    | Unpacked -> C(S(S(S(S(S(S(S(S(S E)))))))))
    | Open_me r -> C(S(S(S(S(S(S(S(S(S(S(Z [r.resolved;r.opens;r.expr])))))))))))
  and me_rev = let open Tuple in
    function
    | C Z x -> Resolved x
    | C S Z x -> Ident x
    | C S S Z [f;x] -> Apply{f;x}
    | C S S S Z [arg;body] -> Fun{arg;body}
    | C S S S S Z [x;y] -> Constraint(x,y)
    | C S S S S S Z x -> Str x
    | C S S S S S S Z x -> Val x
    | C S S S S S S S Z x -> Extension_node x
    | C S S S S S S S S E -> Abstract
    | C S S S S S S S S S E -> Unpacked
    | C S S S S S S S S S S Z [resolved;opens;expr] ->
      Open_me {resolved;opens;expr}
    | _ -> .
  and module_type =
    Custom { sch = mt_sch; fwd = mt_fwd; rev = mt_rev; id=["M2l"; "module_type"]}
  and mt_sch =
    Sum [ "Resolved",Module.Partial.sch; "Alias",Paths.S.sch;"Ident",Paths.E.sch;
          "Sig",m2l;"Fun",[ arg; module_type ];
          "With",[ module_type; Array (Array String); access ];
          "Of", module_expr; "Extension_node", extension; "Abstract", Void ]
  and mt_fwd = let open Tuple in function
      | Resolved x -> C (Z x)
      | Alias x -> C (S (Z x))
      | Ident x -> C (S (S (Z x)))
      | Sig x -> C(S (S (S (Z x))))
      | Fun x -> C(S (S (S (S (Z [x.arg; x.body])))))
      | With x -> C(S (S (S (S (S (Z [x.body; S.elements x.deletions; x.access]))))))
      | Of x ->  C(S (S (S (S (S (S (Z x)))))))
      | Extension_node x ->  C(S (S (S (S (S (S (S (Z x))))))))
      | Abstract ->  C(S (S (S (S (S (S (S (S E))))))))
  and mt_rev = let open Tuple in function
      | C Z x -> Resolved x
      | C S Z x -> Alias x
      | C S S Z x -> Ident x
      | C S S S Z x -> Sig x
      | C S S S S Z [arg;body] -> Fun {arg;body}
      | C S S S S S Z [body;deletions;access] ->
        With {body;deletions = S.of_list deletions; access}
      | C S S S S S S Z x -> Of x
      | C S S S S S S S Z x -> Extension_node x
      | C S S S S S S S S E -> Abstract
      | _ -> .
  and annotation =
    Custom{ fwd=ann_f; rev = ann_r; sch = ann_s; id = ["M2l"; "annotation"] }
  and ann_s = Obj [
      Opt, Access.l, access;
      Opt, Values.l, Array m2l; Opt, Packed.l, Array [module_expr;Loc.Sch.t]
    ]
  and ann_f x = Record.[
    Access.l $=? (default Paths.S.Map.empty x.access);
    Values.l $=? l x.values;
    Packed.l $=? l (List.map (fun x -> Tuple.[x.Loc.data; x.loc]) x.packed)
  ]
  and ann_r = Record.(fun [_,a;_,v;_,p] ->
      { access= a >< Paths.S.Map.empty
      ; values = v >< [];
        packed =List.map Tuple.(fun [data;loc] -> {Loc.data;loc}) (p><[])
      })

  and extension =
    Custom {sch = [ String; ext ];
            fwd = (fun {name;extension} -> Tuple.[name;extension] );
            rev = Tuple.(fun [name;extension] -> {name;extension} );
            id = ["M2l";"extension"]
           }
  and ext =
    Custom {sch = Sum["Module",m2l;"Val",annotation];
            fwd = ext_fwd; rev = ext_rev; id=["M2l"; "ext"] }
  and ext_fwd = function Module x -> C (Z x) | Val x-> C (S (Z x))
  and ext_rev =function C Z x -> Module x | C S Z x -> Val x | _ -> .
  and arg =
    Custom { sch = Sum[ "None",Void; "Some",[String; module_type] ];
             fwd = arg_fwd;
             rev = arg_rev;
             id = ["M2l"; "arg"] }
  and arg_fwd = function
    | None -> C E
    | Some (a:_ Module.Arg.t) -> C(S(Z Tuple.[a.name;a.signature]))
  and arg_rev = let open Tuple in
    function C E -> None | C S Z [n;s] -> Some {name=n;signature=s} | _ -> .

end let sch = Sch.m2l


(** The Block module computes the first dependencies needed to be resolved
    before any interpreter can make progress evaluating a given code block *)
module Block = struct

  let (+|) = Summary.(+|)
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
    | Ident n -> ok n
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
    | Open_me {opens = a::_ ; _ } -> ok a
  and mt defs =
    let err = Error defs in
    let ok x = Ok(defs, x) in
    function
    | Resolved _ -> err
    | Alias n -> ok n
    | Ident e ->  ok (List.hd @@ Paths.Expr.multiples e)
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
    let err = Error defs in
    function
    | Defs d -> Error ( defs +| d )
    | Open p -> me defs p
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
    if Paths.S.Map.cardinal m.access > 0 then
      Ok (defs, fst @@ Paths.S.Map.choose m.access)
    else
      either Mresult.Ok.(first defs m2l m.values >>| data)
        (first defs @@ fun s x -> me s x.Loc.data)
        m.packed

  let m2l code =
    match m2l Summary.empty code with
    | Error _ -> None
    | Ok x -> Some x
end

module Annot = struct
  open Loc
  type t = annotation Loc.ext
  let empty = { data = annot_empty; loc = Nowhere }
  let is_empty x  = x.data = annot_empty

  module Access = struct
    type t = access
    let merge = Paths.S.Map.merge (fun _k x y -> match x, y with
        | Some (l,x), Some (l',y) ->
          if x = Deps.Edge.Normal && y = Deps.Edge.Epsilon then
            Some(l',y)
          else
            Some(l, Deps.Edge.max x y)
        | None, (Some _ as x) | (Some _ as x), None -> x
        | None, None -> None
      )

    let empty = Paths.S.Map.empty
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

  let access {loc;data} =
    Loc.create loc
      { annot_empty with
        access = Paths.S.Map.singleton data (loc, Deps.Edge.Normal) }

  let abbrev {loc;data} =
    Loc.create loc
      { annot_empty with
        access = Paths.S.Map.singleton data (loc, Deps.Edge.Epsilon) }


  let value v =
    let loc =
      let ext acc x = Loc.merge acc x.loc in
      List.fold_left (List.fold_left ext) Nowhere v in
    Loc.create loc { annot_empty with values = v }

  let pack x = Loc.create x.loc { annot_empty with packed = x.data  }

  let opt f x = Option.( x >>| f >< empty )

  let epsilon_promote = Loc.fmap @@ fun annot ->
    { annot with
      access = Paths.S.Map.map (fun (l,_) -> l, Deps.Edge.Epsilon) annot.access }

end

(** Helper function *)
module Build = struct
  let ghost data  = {Loc.data; loc= Nowhere }

  let minor x = Minor x
  let access path =
    Loc.fmap minor @@ Annot.access @@ Loc.fmap Paths.Expr.concrete path

  let open_ path = Loc.fmap (fun x -> Open x) path
  let open_path x = open_ (Loc.fmap (fun x -> Ident x) x)

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
  | Open me -> Pp.fp ppf "@[open [%a]@]" pp_me me
  | Include me -> Pp.fp ppf "@[include [%a]@]" pp_me me
  | SigInclude mt -> Pp.fp ppf "@[include type [%a]@]" pp_mt mt

  | Bind bind -> pp_bind ppf bind
  | Bind_sig bind -> pp_bind_sig ppf bind
  | Extension_node e -> pp_extension ppf e
  | Bind_rec bs ->
    Pp.fp ppf "rec@[[ %a ]@]"
      (Pp.(list ~sep:(s "@, and @,")) @@ pp_bind ) bs
and pp_annot ppf {access; values; packed} =
  let sep = Pp.s ";@ " in
  let post = Pp.s "@]" in
  Pp.fp ppf "%a%a%a"
    pp_access access
    Pp.(opt_list ~sep ~pre:(s "@[<2>values: ") ~post pp_simple) values
    Pp.(opt_list ~sep ~pre:(s "@,@[<2>packed: ") ~post pp_opaque) packed
and pp_access ppf s =  if Paths.S.Map.cardinal s = 0 then () else
    Pp.fp ppf "@[<2>access: {%a}@]@," (Pp.list pp_access_elt) (Paths.S.Map.bindings s)
and pp_access_elt ppf (name, (loc,edge)) =
  Pp.fp ppf "%s%a(%a)" (if edge = Deps.Edge.Normal then "" else "ε∙")
    Paths.S.pp name
    Loc.pp loc
and pp_opaque ppf me = Pp.fp ppf "⟨%a(%a)⟩" pp_me me.data Loc.pp me.loc
and pp_bind ppf {name;expr} =
  match expr with
  | Constraint(Abstract, Alias np) ->
    Pp.fp ppf "@[module %s ≡ %a@]" name Paths.Simple.pp np
  | Constraint(Abstract, mt) ->
    Pp.fp ppf "@[<2>module %s:%a@]" name pp_mt mt
  | Constraint(Unpacked, mt) ->
    Pp.fp ppf "@[<2>(module %s:%a)@]" name pp_mt mt
  | Unpacked ->
    Pp.fp ppf "(module %s)" name
  | Open_me {opens = a :: q ; resolved; expr} ->
    Pp.fp ppf "%a.(%a)" Paths.Simple.pp a pp_bind
      {name; expr = Open_me{opens=q;resolved;expr} }
  | Open_me {opens=[]; resolved; expr} ->
    Pp.fp ppf "⟨context:%a⟩ %a"
      Summary.pp resolved pp_bind {name;expr}
  | _ ->
    Pp.fp ppf "@[<2>module %s =@ %a@]" name pp_me expr
and pp_bind_sig ppf {name;expr} =
  match expr with
  | Alias id ->
    Pp.fp ppf "@[module type %s ≡ %a@]" name Paths.Simple.pp id
  | Abstract ->
    Pp.fp ppf "@[module type %s@]" name
  | _ ->
    Pp.fp ppf "@[<2>module type %s =@ %a @]" name pp_mt expr
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
  | With {body; deletions;access} ->
    Pp.fp ppf "%a@,/%a (%a)" pp_mt body Paths.S.Set.pp deletions pp_access access
  | Of me -> Pp.fp ppf "module type of@, %a" pp_me me
  | Extension_node ext -> Pp.fp ppf "%a" pp_extension ext
  | Abstract -> Pp.fp ppf "⟨abstract⟩"
and pp_extension ppf x = Pp.fp ppf "[%%%s @[%a@]]" x.name pp_extension_core
    x.extension
and pp_extension_core ppf = function
  | Module m -> pp ppf m
  | Val m -> pp_annot ppf m
and pp_simple ppf =
  Pp.list ~sep:(Pp.s " @,") pp_expression_with_loc ppf
and pp ppf = Pp.fp ppf "@[<2>[@,%a@,]@]" pp_simple
and pp_expression_with_loc ppf e = Pp.fp ppf "%a(%a)"
    pp_expression e.data Loc.pp e.loc


(** {Normalize} computes the normal form of a given m2l code fragment *)
module Normalize = struct

  let halt l = false, l

  let (+:) x (more,l) =
    more, x :: l

  let cminor x = Minor x

  let rec all : m2l -> bool * m2l  = function
    | {data=Defs d1;_} :: {data=Defs d2; _ } :: q ->
      all @@ (Loc.nowhere @@ Defs Summary.(  d1 +| d2)) :: q
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
