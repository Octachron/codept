let lex src = Lexing.from_string src

let parse x =  Sparser.main Slex.main (lex x)

let counter = ref 0


let round_trip sch x =
  incr counter;
  let print scheme ppf x = Schematic.minify ppf "%a" (scheme sch) x in
  let s scheme = Format.asprintf "%a" (print scheme) x in
  let map2 f (x,y) = (f x, f y) in
  let j, s = map2 s Schematic.(json,sexp) in
  Format.printf "@[%d)@ " !counter;
  let check name s = match Schematic.retype sch (parse s) with
  | Some y ->
    if x = y then
      Format.printf "%s:OK" name
    else
      Format.printf "@[%s:partial (%s)" name s
  | None -> Format.printf "%s: failure (%s)@" name s in
  check "json" j;
  Format.printf "@ ";
  check "sexp" s;
  Format.printf "@]@."

let round_trip' sch src trip =
  incr counter;
  Format.printf "@[%d) " !counter;
  let rec round_trip' start sch src = function
    | [] | [_] ->
      if start = src then
        Format.printf "success"
      else
          Format.printf "failure, expected:\n%s\ngot:\n%s@." start src
  | _ :: (next :: _ as rest) ->
      let u = parse src in
      match Schematic.retype sch u with
      | None -> Format.printf "failure, typing error (%s)@." src
      | Some x ->
        let print ppf x = Schematic.minify ppf "%a" (next sch) x in
        let s = Format.asprintf "%a" print x in
        round_trip' start sch s rest in
  round_trip' src sch src trip;
  Format.printf "@]@."

    open Schematic

    module Y = Label(struct let l = "y" end)
    module X = Label(struct let l = "x" end)
    module Z = Label(struct let l = "z" end)
    ;; round_trip String "𒆍𒀭𒊏𒆠"
    ;; round_trip Int 105
    ;; round_trip Bool false
    ;; round_trip Float 1.
    ;; round_trip (Array Int) [2;3;4;5]
    ;; round_trip [Int;Float] [1;1.]

    ;; round_trip [String; [Int;Bool]; Array (Array Int)]
      Tuple.["One"; [1;false]; L.[[1];[2];[3]]]

    ;; round_trip (Obj [Req,X.l,Int;Req,Y.l,Int]) [ X.l $= 1; Y.l $= 2 ]

    ;; round_trip (Array(Sum ["Int", Int;"String", String]))
      [C (Z 1); C (S (Z "1"))]

    ;; round_trip (Obj [Opt,X.l,Int;Opt,Y.l,String;Req,Z.l,Float])
      [ X.l $=? Some 1; skip Y.l; Z.l $= 2.]

;; round_trip' M2l.sch
  {|(((Minor((access(((Name)(Simple(7 24 30))Normal)((Module)(Simple(6 9 17))Normal)((Fault)(Simple(4 61 68))Normal)))))(Multiline((4 0)(7 53))))((Bind_sig(envt(Sig(((Minor((access(((Summary)(Simple(16 18 27))Normal)((Paths Simple)(Simple(13 19 33))Normal)((Paths S)(Simple(18 32 41))Normal)((Namespaced)(Simple(17 44 56))Normal)((Module)(Simple(14 33 45))Normal)((Format)(Simple(21 10 26))Normal)((Deps Edge)(Simple(14 18 29))Normal)))))(Multiline((13 2)(21 39))))))))(Multiline((10 0)(22 3))))((Bind_sig(with_deps(Sig(((Minor((access(((Deps)(Simple(26 17 23))Normal)))))(Simple(26 2 23)))))))(Multiline((24 0)(28 3))))((Bind_sig(envt_with_deps(Sig(((SigInclude(With((Ident(A envt))()())))(Simple(32 2 31)))((SigInclude(With((Ident(A with_deps))()())))(Simple(33 2 36)))))))(Multiline((30 0)(34 3))))((Bind_sig(param(Sig(((Minor((access(((Fault Policy)(Simple(39 14 28))Normal)))))(Simple(39 2 28)))))))(Multiline((37 0)(43 3))))((Bind_sig(s(Sig(((Minor((access(((Paths P)(Simple(49 12 21))Normal)((Module Sig)(Simple(49 50 62))Normal)((M2l)(Simple(49 33 38))Normal)))))(Simple(49 2 77)))))))(Multiline((46 0)(50 3))))((Bind(Make(Constraint(Abstract(Fun((Some(Envt(Ident(A envt))))(Fun((Some(Param(Ident(A param))))(With((Ident(A s))()(((Envt)(Simple(54 63 69))Epsilon))))))))))))(Multiline((53 0)(54 69)))))|}
  [sexp;json;sexp;json;sexp;sexp;json;json;sexp]

;; round_trip' (Array Module.Schema.module')
  {|((M((name Outliner)(origin(Unit((Local(lib outliner.mli))(Outliner))))(modules((M((name Make)(args((Some((name Envt)))(Some((name Param)))))))))(module_types((M((name envt)))(M((name envt_with_deps)))(M((name param)))(M((name s)))(M((name with_deps))))))))|}
  [sexp;json;sexp;json;json;sexp;sexp;json;json;sexp]
