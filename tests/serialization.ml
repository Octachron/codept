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
{|(((Bind_sig((Some envt)(Sig(((Minor((Access(((S(Transforms))(Simple(7 30 53))Normal)((S(Summary))(Simple(9 20 29))Normal)((S(Paths Simple))(Simple(6 54 68))Normal)((S(Paths S))(Simple(15 32 41))Normal)((S(Namespaced))(Simple(12 44 56))Normal)((S(Module))(Simple(6 38 50))Normal)((S(Format))(Simple(18 10 26))Normal)((S(Fault))(Simple(6 4 13))Normal)((S(Deps Edge))(Simple(6 23 34))Normal)))))(Simple(18 2 39)))))))(Multiline((2 0)(19 3))))((Bind_sig((Some param)(Sig(((Minor((Access(((S(Fault Policy))(Simple(24 14 28))Normal)))))(Simple(24 2 28)))))))(Multiline((22 0)(28 3))))((Minor((Access(((S(Transforms))(Simple(30 16 32))Epsilon)))))(Simple(30 0 38)))((Bind_sig((Some generic_outliner)(Sig(((Minor((Access(((S(Summary))(Simple(45 26 35))Normal)((S(Pp))(Simple(49 19 23))Normal)((S(Paths S))(Simple(45 38 47))Normal)((S(Paths P))(Simple(42 8 17))Normal)((S(Module Sig))(Simple(43 8 20))Normal)((S(M2l))(Simple(40 15 20))Normal)((S(Loc))(Simple(45 49 56))Normal)))))(Simple(49 2 23)))))))(Multiline((34 0)(51 3))))((Bind_sig((Some outliner)(With((Ident(S(generic_outliner)))()((Access(((S(Deps))(Simple(55 16 22))Epsilon))))))))(Multiline((54 0)(55 60)))))|}
  [sexp;json;sexp;json;sexp;sexp;json;json;sexp]

(* sig from solver.mli *)
;; round_trip' "solver.mli" (Array (pair String Module.Schema.module'))
  {|((Solver(M((origin(Unit((Local(lib solver.mli))(Solver))))(modules((Directed(Fun((Some((Some Envt)(M())))(Fun((Some((Some Param)(M())))(Fun((Some((Some Eval)(M())))(M()))))))))(Failure(M()))(Make(Fun((Some((Some Envt)(M())))(Fun((Some((Some Param)(M())))(Fun((Some((Some Eval)(M())))(M()))))))))))))))|}
  [sexp;json;sexp;json;json;sexp;sexp;json;json;sexp]

let () =
  if !error then exit 1 else exit 0
