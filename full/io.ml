

type reader = {
  sign: string -> Module.t list option;
  m2l: Fault.Policy.t -> Read.kind -> string -> Namespaced.t -> Unit.s;
  findlib: Common.task -> Findlib.query -> Common.task ;
  env: Module.Def.t
}

type writer = {
  sign: string -> Format.formatter -> Module.t list -> unit;
  m2l: (Read.kind * string) -> Format.formatter -> M2l.t -> unit
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
  Sexp.( (list Module.sexp).parse )
  @@ Sexp_parse.many Sexp_lex.main
  @@ lexbuf

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
    env = Module.Def.empty;
    findlib = Findlib.expand
  };
  writer = {
    m2l =  (fun _filename ppf m2l ->
        Pp.fp ppf  "%a@." Sexp.pp @@ M2l.sexp.embed m2l );
    sign =
      (fun _ ppf (mds: Module.t list) ->
        mds
        |> Sexp.( embed @@ list Module.sexp)
        |> Pp.fp ppf "@[%a@]@." Sexp.pp
      )
  }
}
