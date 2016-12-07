
module Level = struct
type t = int
let whisper = 0
let notification = 1
let warning = 2
let error = 3
let critical = 4
end


let critical fmt =
  Format.kfprintf
  (fun _ppf -> exit 1) Format.err_formatter
    ("@[[\x1b[91mCritical error\x1b[39m]: @[<hov>"^^fmt^^"@]@]@.")

let error fmt =
  Format.eprintf @@
  "@[[\x1b[31mError\x1b[39m]: @[<hov>"^^fmt^^"@]@]@."

let warning fmt = Format.eprintf @@
  "@[<hov2>[\x1b[35mWarning\x1b[39m]:@,@ @[" ^^ fmt ^^ "@]@]@."

let notification fmt = Format.eprintf @@
  "@[<hov2>[\x1b[36mNotification\x1b[39m]:@,@ @[" ^^ fmt ^^ "@]@]@."

let whisper fmt = Format.eprintf @@
  "@[<hov2>[Misc]:@,@ @[" ^^ fmt ^^ "@]@]@."

let log level fmt =
  let fns = [| whisper; notification; warning; error; critical |] in
  if level <= Level.whisper then Format.ifprintf Format.err_formatter fmt
  else if level >= Level.critical then
    critical fmt
  else
    fns.(level) fmt

let llog fmt = fun level -> log level fmt
let with_lvl f = fun lvl -> f lvl

type 'a t = { path: Paths.S.t; send: int -> 'a }


(** Warnings *)
let extension =
{ path = ["extension"; "ignored"];
  send = (fun lvl (name, _)  ->
      log lvl "extension node %s ignored." name.Location.txt)
}

let  generic_first_class=
{ path = ["first_class";"gen"];
  send = llog "first-class modules are very partially handled for now."
}

let opened_first_class =
{ path = ["first_class"; "open"];
    send = (fun lvl ->
      log lvl "First-class module %s was opened while its signature was unknown."
      )
  }

let included_first_class =
  { path = ["first_class"; "included"];
    send =  llog "First-class module was included while its signature was unknown."
  }

let applied_structure =
  { path = ["typing"; "apply"; "structure"];
    send = (fun lvl -> log lvl "Only functor can be applied, got:%a"
               Module.Partial.pp)
  }

let signature_expected =
  { path = ["typing"; "signature_expected"];
    send = (fun lvl -> log lvl "A signature, i.e. not a functor was expected; got:%a"
               Module.Partial.pp)
  }


let applied_unknown =
  { path = ["typing"; "apply"; "unknown"];
    send = (fun lvl -> log lvl "Only functor can be applied, got:%a"
               Module.Partial.pp)
  }


let concordant_approximation =
  { path = ["parsing"; "approximation"; "concordant"];
    send = (fun lvl path -> log lvl
             "Approximate parsing of %a.\n\
              However, lower and upper bound agreed upon dependencies."
        Paths.P.pp path
           )
  }

let discordant_approximation =
  { path = ["parsing"; "approximation"; "discordant"];
    send = (fun lvl path lower diff -> log lvl
               "Approximate parsing of %a.\n\
                Computed dependencies: at least {%a}, maybe: {%a}"
        Paths.P.pp path
        Pp.(list string) lower
        Pp.(list string) diff
           )
  }


(** Syntax errors *)

let print_loc ppf loc =
  let (msg_file, msg_line, msg_chars, msg_to) =
  ("File \"", "\", line ", ", characters ", "-") in
  let open Location in
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    Format.fprintf ppf "%s%a%s%i" msg_file print_filename file msg_line line;
    if startchar >= 0 then
      Format.fprintf ppf "%s%i%s%i" msg_chars startchar msg_to endchar

let syntaxerr =
  { path = ["parsing"; "syntax"];
    send = (fun lvl error ->
        log lvl "Syntax error\n %a" print_loc
          (Syntaxerr.location_of_error error)
      )
  }

module Polycy = struct
  type t = Level of Level.t | Map of Level.t * t Name.Map.t
  let rec find pol l  =
    match pol, l with
    | Level h, _ -> h
    | Map (h,m), a :: q  ->
      begin
        try find (Name.Map.find a m) q with
          Not_found -> h
      end
    | Map (h,_), [] -> h

  let rec set (path,lvl) env = match path, env with
    | [], Level _ -> Level lvl
    | [], Map (_,m) -> Map(lvl,m)
    | a :: q, Level h ->
      Map(h, Name.Map.singleton a @@ set (q,lvl) @@ Level h)
    | a :: q, Map(h, m) ->
      let env' = try
          Name.Map.find a m with
      | Not_found -> Level h in
      let elt = set (q,lvl) env' in
      let m = Name.Map.add a elt m in
      Map(h, m)

  let set_err (error,lvl) polycy = set (error.path,lvl) polycy

  let strict = Level Level.critical

  let default =
    Level Level.critical
    |> set_err (applied_unknown, Level.warning )
    |> set (["first_class"], Level.warning )
    |> set (["extension"], Level.warning)

  let parsing_approx =
    Level Level.critical
    |> set_err (applied_unknown, Level.warning )
    |> set (["first_class"], Level.warning )
    |> set (["extension"], Level.warning)
    |> set (["parsing"], Level.warning)
    |> set_err (concordant_approximation, Level.notification)

  let lax =
    Level Level.critical
    |> set_err (applied_unknown, Level.warning )
    |> set (["first_class"], Level.warning )
    |> set (["extension"], Level.warning)
    |> set (["parsing"], Level.warning)
    |> set_err (concordant_approximation, Level.notification)
    |> set (["typing"], Level.error)

  let quiet = Level Level.whisper

  end

let set = Polycy.set_err
let send polycy error =
  error.send @@ Polycy.find polycy error.path
