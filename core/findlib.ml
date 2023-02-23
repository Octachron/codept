
  type query =
    { pkgs: Name.t list;
      predicates: string list;
      syntaxes: Name.t list;
      ppxopts: string list Name.map;
      ppopt: string list
    }

type result = { libs: string list; ppxs: string list; pp: string option }

let pp ppf r =
  Pp.fp ppf "libs={%a};@; ppxs={%a};@; pp = %a "
    Pp.(list estring) r.libs Pp.(list estring) r.ppxs
    Pp.(opt estring) r.pp

let run cmd =
  let cin = Unix.open_process_in cmd in
  let rec read l =
  try
    match input_line cin with
    | "" -> read l
    | s -> read (s::l)
  with
    End_of_file -> List.rev l in
  read []

let run_word cmd = match run cmd with
  | [a] -> Some a
  | _ -> None

let query q = run@@ String.concat " " ("ocamlfind query -r " :: q)
let printppx q =
  let s = run_word @@ String.concat " " ("ocamlfind printppx " :: q) in
  Option.fmap (fun s -> String.sub s 4 @@ String.length s - 4) s

let archive q = run @@ String.concat " " ("ocamlfind query -format %a":: q)

let find_pred info =
  let p = String.concat "," info.predicates in
  if p = "" then [] else
    ["-predicates"; p ]

let camlp4 = "camlp4"
let filter predicates syntax pkg =
  archive @@ (pkg :: predicates) @ ["-pp"; "-predicates"; syntax] <> []

let find_pp info syntax pkgs =
  let predicates = find_pred info in
  let g = List.filter (filter predicates syntax) pkgs in
  let main_pp = camlp4 in
  let includes l i =
    List.fold_left (fun acc x ->  "-I" :: x :: acc ) l @@ query (i::predicates) in
  let i = List.fold_left includes [] g in
  main_pp :: i
  @ archive ( main_pp :: predicates @ ["-pp"; "-predicates"; syntax]
              @ g )
  @ List.rev info.ppopt

let process_pkg info name =
  let predicates = find_pred info in
  let dirs = query @@  predicates @ [name] in
  let ppxopt = Option.default [] @@ Name.Map.find_opt name info.ppxopts in
  let ppx = Option.fmap (fun s -> String.concat " " @@ s :: ppxopt) @@
    printppx @@ predicates @ [name] in
  dirs, ppx

let pkg info pkg = { info with pkgs = pkg :: info.pkgs }
let syntax info syntax = { info with syntaxes = syntax :: info.syntaxes }
let ppxopt info opt =
  match Support.cuts ~empty:false ~sep:"," opt with
  | [] | [_] -> info
  | a :: q ->
    let q = String.concat "," q in
    let m = info.ppxopts in
    let q = Option.( Name.Map.find_opt a m >>| (List.cons q) >< [q] ) in
    let m = Name.Map.add a q m in
    { info with ppxopts = m }

let ppopt info opt = { info with ppopt = opt :: info.ppopt }

let predicates info s =
  let l = List.map String.trim @@ Support.cuts ~empty:false ~sep:"," s in
  { info with predicates = l @ info.predicates }

let empty =
    {
      pkgs = [];
      syntaxes =[];
      ppopt = [];
      ppxopts = Name.Map.empty;
      predicates = []
    }

let process_pp info name pp =
  try
    let s = find_pp info name info.pkgs in
    let s = String.concat " " s in
    Some s
  with Not_found -> pp

let ( |:: ) x l = Option.( x >>| (fun x -> x :: l) >< l)

let process info =
  let syntaxes = Name.Set.of_list (info.syntaxes) in
  let pp = Name.Set.fold (process_pp info) syntaxes None in
  let libs, ppxs =
    List.fold_left (fun (libs,ppxs) pkg ->
        let lib, ppx = process_pkg info pkg in
        (lib @ libs, ppx |:: ppxs))
      ([],[]) info.pkgs
  in
  {ppxs;libs;pp}
