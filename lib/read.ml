

(** Format type *)
type format =
  | Src (** standard source file *)
  | M2l (** M2l serialized file *)
  | Parsetree (** parsetree ast file *)
  | Cmi

(** Extend M2l.kind to include the format of read file *)
type kind = { format: format; kind: M2l.kind }

type ocaml_parsing_error = Syntax of Syntaxerr.error | Lexer of Lexer.error
type error = Ocaml of ocaml_parsing_error | Serialized of Schematic.Ext.error

let name str = Unitname.modulize str

let ok x = Ok x

let parse_implementation input =
  try
    Pparse_compat.implementation input
  with
  | Syntaxerr.Error _ ->
    let ast = Parse.use_file (Lexing.from_channel @@ open_in input) in
    let drop_directive x l = match x with
      | Parsetree.Ptop_def x -> x @ l
      | Ptop_dir _ -> l in
    List.(fold_right drop_directive ast [])

let source_file kind filename =
  Location.input_name := filename;
  let input_file = Pparse.preprocess filename in
  let code =  try ok @@
      match kind with
      | M2l.Structure ->
        Ast_converter.structure @@ parse_implementation input_file
      | M2l.Signature ->
        Ast_converter.signature @@
        Pparse_compat.interface input_file
    with
    | Syntaxerr.Error msg -> Error (Ocaml (Syntax msg))
    | Lexer.Error(e,_) -> Error (Ocaml (Lexer e))
  in
  Pparse.remove_preprocessed input_file;
  code

let file_raw {format;kind} filename =
  match format with
  | Src | Parsetree -> source_file kind filename
  | M2l ->
    let file = open_in filename in
    let lex = Lexing.from_channel file in
    begin
      (*    match M2l.sexp.parse @@ Sexp_parse.many Sexp_lex.main lex with*)
      match Schematic.Ext.strict Schema.m2l @@ Sparser.main Slex.main lex with
      | Ok m2l -> close_in file; ok m2l
      | Error e -> close_in file; Error (Serialized e)
      | exception Parsing.Parse_error -> close_in file;
        Error (Serialized Schematic.Ext.Unknown_format)
    end
  | Cmi  -> ok @@ Cmi.m2l filename

let file {format;kind} filename =
  let name = name filename in
  name, file_raw {format;kind} filename
