
type reader = {
  sign:  string -> (Module.Namespace.t, Schematic.Ext.error) result;
  m2l: Fault.Policy.t -> Read.kind -> string
    -> Namespaced.t -> Unit.s;
  findlib: Common.task -> Findlib.query -> Common.task ;
  env: Module.dict
}

type writer = {
  sign: Schematic.format -> Format.formatter -> Module.Namespace.t -> unit;
  m2l: Schematic.format -> (Read.kind * string) -> Format.formatter -> M2l.t -> unit
}

type t = {
  reader: reader;
  writer: writer;
}

module Findlib = struct
(** Small import *)
let add_ppx ppx =
  let first_ppx = Compenv.first_ppx in
  first_ppx := ppx :: !first_ppx

let lib (task:Common.task ref) f =
  task := { !task with libs = f :: (!task).libs }

let expand task query =
  let task = ref task in
  let result = Findlib.process query in
  Option.iter (fun pp -> Clflags.preprocessor := Some pp) result.pp;
  List.iter (lib task) result.libs; List.iter add_ppx result.ppxs;
  !task
end

let parse_sig lexbuf=
  try
    Schematic.Ext.strict Schema.namespace @@ Sparser.main Slex.main lexbuf
  with
  Sparser.Error -> Error Unknown_format

let read_sigfile filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let sigs = parse_sig lexbuf in
  close_in chan;
  sigs




let direct = {
  reader = {
    sign = read_sigfile;
    m2l = Unit.read_file;
    env = Name.Map.empty;
    findlib = Findlib.expand
  };
  writer = {
    m2l =  (fun format _filename ppf m2l ->
        match format with
        | Json -> Schematic.minify ppf "%a@.\n" (Schematic.Ext.json Schema.m2l) m2l
        | Sexp ->  Schematic.minify ppf "%a@.\n" (Schematic.Ext.sexp Schema.m2l) m2l

      );
    sign =
      (fun format ppf (mds: Module.Namespace.t) ->
         match format with
         | Sexp ->  Schematic.minify ppf "%a@.\n"
                      (Schematic.Ext.sexp Schema.namespace) mds
         | Json ->  Schematic.minify ppf "%a@.\n"
                      (Schematic.Ext.json Schema.namespace) mds
      )
  }
}
