open M2l

let std = Format.std_formatter

module File() = struct
  let lex_test = Lexing.from_channel @@ open_in Sys.argv.(1)

  let ast = Parse.implementation lex_test

  let () =
    let start =  Ast_analyzer.structure ast in
    Pp.fp std "M2l: %a@." M2l.pp start;
    start
    |> Compute.basic
    |> Normalize.all
    |> snd
    |> Pp.fp std  "Basic:\n %a@." M2l.pp;
    match start |> Compute.Sg.m2l M2l.empty_sig with
    | Done (_state,d) -> Pp.fp std "Computation finished:\n %a@." M2l.pp_signature d
    | Halted h -> Pp.fp std "Computation halted at:\n %a@." M2l.pp h

end

module R = File()
