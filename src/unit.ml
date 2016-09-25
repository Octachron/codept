type spath = string list

module Map = StrMap

type package = {name:string; content: content }
and content = Subpackages of package Map.t | Modules of Module.t Map.t

type package_path = Local | Sys of spath

type path = { package: package_path ; file: spath }

type t = {
  name: string;
  path: path;
  signature: Module.explicit_signature;
  unresolved: Unresolved.direct_map;
  dependencies: path list
}

type kind = Structure | Signature


let (@<) f g x = f @@ g x
let (%>) f g x = x |> f |> g

let read_file env kind name =
  let extract env = Envt.(env.unresolved, env.signature) in
  let normalize (focus, sign) =
    match sign with
    | Module.Sig esn ->
      ( focus, esn )
    | Module.Alias _ | Module.Fun _ -> assert false
  in
  let parse_and_analyze = match kind with
    | Structure -> Parse.implementation %> Parse_ml.structure env %> extract
    | Signature -> Parse.interface %> Parse_ml.signature env %> normalize in
  let foc, signature = parse_and_analyze @@ Lexing.from_channel @@ open_in name in
  { name;
    path = { package= Local; file=[name] };
    signature;
    unresolved = Unresolved.map foc;
    dependencies = []
  }
