open M2l
open Ast_analyzer

let std = Format.std_formatter

module File() = struct
  let lex_test = Lexing.from_channel @@ open_in Sys.argv.(1)

  let ast = Parse.implementation lex_test

  let () =
    Ast_analyzer.structure ast
    |> M2l.pp std
end

module R = File()
