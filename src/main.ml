open M2l

let std = Format.std_formatter

module File() = struct
  let lex_test = Lexing.from_channel @@ open_in Sys.argv.(1)

  let ast = Parse.implementation lex_test

  let () =
    let start =  Ast_analyzer.structure ast in
    Pp.fp std "%a@." M2l.pp start;
    start
    |> Compute.basic
    |> Normalize.all
    |> snd
    |> Pp.fp std  "%a@." M2l.pp
end

module R = File()
