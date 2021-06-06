let lex src = Lexing.from_string src

let parse x =  Sparser.main Slex.main (lex x)

let counter = ref 0


let error = ref false
let ok ppf = Format.fprintf ppf "\x1b[32mok\x1b[0m"
let failure ppf = Format.fprintf ppf "\x1b[31mfailure\x1b[0m"
let fail fmt =
  Format.kfprintf (fun ppf -> Format.fprintf ppf "[%t]@." failure; error := true)
    Format.std_formatter fmt
let log fmt =
  Format.ikfprintf (fun ppf -> Format.ifprintf ppf "[%t]@." ok) Format.std_formatter fmt


let round_trip sch x =
  incr counter;
  let print scheme ppf x = Schematic.minify ppf "%a" (scheme sch) x in
  let s scheme = Format.asprintf "%a" (print scheme) x in
  let map2 f (x,y) = (f x, f y) in
  let j, s = map2 s Schematic.(json,sexp) in
  log "%d)@." !counter;
  let check name s = match Schematic.retype sch (parse s) with
  | Some y ->
    if x = y then
      log "%s" name
    else
      fail "@[%s:partial (%s)" name s
  | None -> fail "%s: failure (%s)@" name s in
  check "json" j;
  check "sexp" s

let round_trip' name sch src trip =
  incr counter;
  log "%d) " !counter;
  let rec round_trip' start sch src = function
    | [] | [_] ->
      if start = src then
        log "%s" name
      else
        fail "failure, expected:\n%s\ngot:\n%s@." start src
  | _ :: (next :: _ as rest) ->
      let u = parse src in
      match Schematic.retype sch u with
      | None -> fail "failure, typing error (%s)@." src
      | Some x ->
        let print ppf x = Schematic.minify ppf "%a" (next sch) x in
        let s = Format.asprintf "%a" print x in
        round_trip' start sch s rest in
  round_trip' src sch src trip

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

;; round_trip' "lib/stage.mli" M2l.sch (* from lib/stage.mli *)
{|(((Bind_sig((Some envt)(Sig(((Minor((Access(((S(Uloc))(Simple(6 4 10))Normal)((S(Transforms))(Simple(7 30 53))Normal)((S(Summary))(Simple(15 20 29))Normal)((S(Paths Simple))(Simple(6 51 65))Normal)((S(Paths S))(Simple(21 32 41))Normal)((S(Namespaced))(Simple(18 44 56))Normal)((S(Name))(Simple(21 45 51))Normal)((S(Module))(Simple(6 35 47))Normal)((S(Format))(Simple(24 10 26))Normal)((S(Deps Edge))(Simple(6 20 31))Normal)))))(Simple(24 2 39)))))))(Multiline((2 0)(25 3))))((Bind_sig((Some param)(Sig(((Minor((Access(((S(Fault Policy))(Simple(30 14 28))Normal)))))(Simple(30 2 28)))))))(Multiline((28 0)(34 3))))((Minor((Access(((S(Transforms))(Simple(36 16 32))Epsilon)))))(Simple(36 0 38)))((Bind_sig((Some generic_outliner)(Sig(((Minor((Access(((S(Summary))(Simple(51 26 35))Normal)((S(Pp))(Simple(55 19 23))Normal)((S(Pkg))(Simple(48 8 13))Normal)((S(Paths S))(Simple(51 38 47))Normal)((S(Module Sig))(Simple(49 8 20))Normal)((S(M2l))(Simple(46 15 20))Normal)((S(Loc))(Simple(51 49 56))Normal)))))(Simple(55 2 23)))))))(Multiline((40 0)(57 3))))((Bind_sig((Some outliner)(With((Ident(S(generic_outliner)))((Type((Access(((S(Deps))(Simple(61 16 22))Epsilon)))))(Type()))))))(Multiline((60 0)(61 60)))))|}
  [sexp;json;sexp;json;sexp;sexp;json;json;sexp]

(* sig from solver.mli *)
;; round_trip' "solver.mli" (Array (pair String Module.Schema.module'))
  {|((Solver(M((origin(Unit((Local(lib solver.mli))(Solver))))(modules((Directed(Fun((Some((Some Envt)(M())))(Fun((Some((Some Param)(M())))(Fun((Some((Some Eval)(M())))(M()))))))))(Failure(M()))(Make(Fun((Some((Some Envt)(M())))(Fun((Some((Some Param)(M())))(Fun((Some((Some Eval)(M())))(M()))))))))))))))|}
  [sexp;json;sexp;json;json;sexp;sexp;json;json;sexp]

let () =
  if !error then exit 1 else exit 0
