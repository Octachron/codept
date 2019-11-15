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
  {|(((Minor((access(((Format)(Simple(2 10 26))Epsilon)))))(Simple(2 0 34)))((Bind_sig(Result_printer(Sig(((Bind(T(Constraint(Abstract(Ident(S(Zipper_def tree)))))))(Simple(7 2 27)))((Minor((access(((T)(Simple(9 16 23))Normal)))))(Multiline((9 2)(18 40))))))))(Multiline((6 0)(20 3))))((Bind(Make(Fun((Some(Def(Ident(S(Zipper_def s)))))(Fun((Some(R(With((Ident(S(Result_printer)))((T))(((Def T)(Simple(23 64 69))Normal))))))(Str(((Open(Ident(Def)))(Simple(24 2 10)))((Bind(Sk(Ident(Zipper_skeleton))))(Simple(25 2 29)))((Minor((access(((Zipper_skeleton)(Simple(36 52 72))Normal)((Zipper_def)(Simple(31 18 33))Normal)((Sk)(Simple(39 21 39))Normal)((R)(Simple(34 59 66))Normal)((Pp)(Simple(28 16 21))Normal)((Paths S Set)(Simple(122 40 60))Normal)((Paths S)(Simple(35 19 29))Normal)((Paths Expr)(Simple(145 17 29))Normal)((Paths E)(Simple(150 16 26))Normal)((Module Arg)(Simple(34 40 55))Normal)((M2l)(Simple(51 56 65))Normal)((Format)(Simple(36 19 33))Normal)))))(Multiline((26 2)(180 12))))))))))))(Multiline((23 0)(182 3))))((Bind(Opaque(Fun((Some(X(Ident(S(Zipper_def s)))))(Constraint((Str(((Minor((access(((Pp)(Simple(186 24 29))Normal)))))(Simple(186 2 37)))))(With((Ident(S(Result_printer)))((T))(((X T)(Simple(185 64 67))Normal))))))))))(Multiline((185 0)(197 3)))))|}
  [sexp;json;sexp;json;sexp;sexp;json;json;sexp]

;; round_trip' (Array Module.Schema.module')
  {|((M((name Outliner)(origin(Unit((Local(lib outliner.mli))(Outliner))))(modules((M((name Make)(args((Some((name Envt)))(Some((name Param)))))))))(module_types((M((name envt)))(M((name envt_with_deps)))(M((name param)))(M((name s)))(M((name with_deps))))))))|}
  [sexp;json;sexp;json;json;sexp;sexp;json;json;sexp]
