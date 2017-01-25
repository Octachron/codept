
module Level = struct
type t = int
let whisper = 0
let notification = 1
let warning = 2
let error = 3
let critical = 4

let of_int n = if n < whisper then
    whisper
  else if n > critical then
    critical
  else
    n

let of_string =
  function
  | "whisper" | "0" -> whisper
  | "notification" | "1" -> notification
  | "warning" | "2" -> warning
  | "error" | "3" -> error
  | "critical" | "4" -> critical
  | _ -> whisper

let to_string =
  function
  | 0 -> "whisper"
  | 1 -> "notification"
  | 2 -> "warning"
  | 3 -> "error"
  | 4 -> "critical"
  | _ -> "whisper"

let to_decorator = function
  | 4 -> "\x1b[91m"
  | 3 -> "\x1b[31m"
  | 2 -> "\x1b[35m"
  | 1 -> "\x1b[36m"
  | _ -> ""
end

module Log = struct

  type 'k status =
    | Ok: ('a -> unit) status
    | Fail: ('a -> 'b) status

  let kont (type k): k status -> k = function
    | Ok -> ignore
    | Fail -> (fun _x -> exit 1)

  let kf st = Format.kfprintf (kont st) Format.err_formatter

  let kcritical k fmt =
    kf k @@
      ("@[[\x1b[91mCritical error\x1b[39m]: "^^fmt ^^"@]@." )

  let kerror k fmt =
    kf k
    ("@[[\x1b[31m%s\x1b[39m]: "^^fmt^^"@]@.")
    (if k = Ok then "Error" else "Fatal error")

  let kwarning k fmt = kf k
    ("@[[\x1b[35m%s\x1b[39m]: " ^^ fmt ^^"@]@." )
    (if k = Ok then "Warning" else "Fatal warning")

  let knotification k fmt = kf k
    ("@[[\x1b[36m%s\x1b[39m]: " ^^ fmt ^^"@]@.")
      (if k = Ok then "Notification" else "Fatal notification")


  let kwhisper k fmt = kf k
    ("@[[%s]: " ^^ fmt ^^"@]@.")
    (if k = Ok then "Miscellaneous" else "Fatal accident")

  let critical fmt = kcritical Fail fmt
  let error fmt = kerror Ok fmt
  let warning fmt = kwarning Ok fmt
  let notification fmt = kwarning Ok fmt
  let whisper fmt = kwhisper Ok fmt

end

type log_info = { silent:Level.t; level:Level.t; exit:Level.t}
let log i fmt =
  let fns = Log.[| kwhisper; knotification; kwarning; kerror; kcritical |] in
  if i.level <= i.silent then
    Format.ifprintf Format.err_formatter fmt
  else if i.level >= Level.critical then
    Log.critical fmt
  else
    begin
      let k = if i.level >= i.exit then
          Log.Fail
        else
          Log.Ok  in
      fns.(i.level) k fmt
    end

type explanation = string
type 'a fault = { path: Paths.S.t; expl: explanation; log: log_info -> 'a }
type 'a t = 'a fault

type loc = Paths.Pkg.t * Loc.t
let loc ppf (path,x)= Pp.fp ppf "%a:%a" Paths.Pkg.pp path Loc.pp x

(** Warnings *)
let extension_ignored =
  { path = ["extension"; "ignored"];
    expl = "The payload of an extension node was ignored";
  log = (fun lvl ->
      log lvl "%a, extension node %s ignored." loc)
}

let extension_traversed =
  { path = ["extension"; "traversed"];
    expl = "The payload of the extension node was handled has a standard \
           OCaml code.";
  log = (fun lvl  ->
      log lvl "%a, @ extension node %s traversed." loc)
}



let opened_first_class =
  { path = ["first_class"; "open"];
    expl= "A first-class module was opened while its signature was deemed \
           unresolved. Consequently, all inferred dependendency after this \
           point may be an over-approximation.";
    log = (fun lvl ->
      log lvl "%a,@ first-class module %s was opened while its signature was \
               unknown." loc
      )
  }

let included_first_class =
  { path = ["first_class"; "included"];
    expl = "A first-class module was included while its signature was deemed \
           unresolved. Consequently, all inferred dependendency after this \
           point may be an over-approximation.";
    log = (fun lvl ->
        log lvl
          "%a, @ first-class module was included while its signature was unknown."
          loc
      )
  }

let applied_structure =
  { path = ["typing"; "apply"; "structure"];
    expl = "Signature fault: a module that was inferred as not a functor \
            was applied like a functor.";
    log = (fun lvl l -> log lvl "%a, @ only functor can be applied, got:%a"
               loc l Module.Partial.pp)
  }

let structure_expected =
  { path = ["typing"; "structure_expected"];
    log = (fun lvl l -> log lvl "%a, @ a structure, i.e. not a functor was \
                                 expected; got:%a"
              loc l
              Module.Partial.pp);
    expl = "Signature fault: a functor was not expected in this situation."
  }


let applied_unknown =
  { path = ["typing"; "apply"; "unknown"];
    log = (fun lvl l -> log lvl "%a, @ only functor can be applied, hopefully the\
                                 unknown module (%a) is a functor"
              loc l
              Module.Partial.pp);
    expl = "Signature fault: an unknown module was applied like a functor.";
  }


let unknown_approximated =
  { path = ["typing"; "unknown"; "approximation"];
    log = (fun lvl path l ->
        log lvl "%a,@ a non-resolvable module, ⟨%a⟩, has been \
                 replaced by an approximation"
          loc l Paths.S.pp path);
    expl = "Signature fault: an unknown module was approximated, yielding, \
            possibly to an over-approximation of dependencies";
  }


let concordant_approximation =
  { path = ["parsing"; "approximation"; "concordant"];
    expl = "Parsing fault: The signature and dependency of an unit were obtained \
            using the heuristic approximative parser. However, the lower and upper \
            bounds for dependencies yield the same set. Inferred dependencies \
            might be exact.";
    log = (fun lvl path -> log lvl
             "Approximate parsing of %a.\n\
              However, lower and upper bound agreed upon dependencies."
        Paths.P.pp path
           )
  }

let discordant_approximation =
  { path = ["parsing"; "approximation"; "discordant"];
    expl = "Parsing fault: The signature and dependencies of an unit were obtained \
            using the heuristic approximative parser. Moreover, the lower and upper \
            bounds for dependencies yield different sets. Codept shall use \
            the dependencies upper bound as a safe over-approximation but \
            dependency problems might arise.";
    log = (fun lvl path lower diff -> log lvl
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
    expl = "Parsing fault: not syntactically valide input file.";
    log = (fun lvl error ->
        log lvl "Syntax error\n %a" print_loc
          (Syntaxerr.location_of_error error)
      )
  }

module Polycy = struct

  type map =
    | Level of {expl: explanation; lvl: Level.t option}
    | Map of {expl:explanation; lvl:Level.t option; map: map Name.map}

  type t = { silent: Level.t; exit:Level.t; map:map}
  type polycy = t

  let rec find default pol l  =
    let with_default = Option.default default in
    match pol, l with
    | Level h, _ -> with_default h.lvl
    | Map m, a :: q  ->
      begin
        let h = with_default m.lvl in
        try find h (Name.Map.find a m.map) q with
          Not_found -> h
      end
    | Map m, [] -> with_default m.lvl

  let level {map; exit; _ } error = find exit map error.path

  let rec set (path,expl,lvl) env = match path, env with
    | [], Level l -> Level {expl = Option.default l.expl expl;lvl }
    | [], Map m -> Map { m with lvl; expl = Option.default m.expl expl }
    | a :: q, Level l ->
      Map{ lvl = None; expl = "";
           map=Name.Map.singleton a @@ set (q,expl,lvl) @@ Level l
         }
    | a :: q, Map m ->
      let env' = try
          Name.Map.find a m.map with
      | Not_found -> Level {lvl=m.lvl; expl = m.expl} in
      let elt = set (q,expl,lvl) env' in
      let map = Name.Map.add a elt m.map in
      Map{m with map}

  let register (p,expl) pol = { pol with map = set (p,expl,None) pol.map }
  let set (p,expl,lvl) pol = { pol with map = set (p,expl, Some lvl) pol.map }

  let set_err (error,lvl) polycy = set (error.path,Some error.expl, lvl) polycy
  let register_err error polycy = register (error.path,Some error.expl) polycy

  let default =
    let open Level in
    { silent = whisper; exit = error;
      map = Level { expl = ""; lvl = Some critical } }
    |> set_err (applied_unknown, warning )
    |> set (["first_class"], Some "First-class module faults", warning )
    |> set_err (opened_first_class, warning)
    |> set_err (included_first_class, warning)
    |> set (["extension"],  Some "Extension node faults", warning)
    |> set_err (extension_ignored, warning)
    |> set_err (extension_traversed, notification)
    |> set (["parsing"], Some "Parsing faults",  error)
    |> register_err syntaxerr
    |> set_err (discordant_approximation, warning)
    |> set_err (concordant_approximation, notification)
    |> set (["typing"], Some "Typing faults", warning)
    |> set_err (applied_structure, warning)
    |> set_err (structure_expected,warning)
    |> set_err (applied_unknown, notification)
    |> set_err (unknown_approximated, notification)


  let strict =
    let open Level in
    { default with exit = Level.notification }
    |> set (["typing"], Some "Typing faults", error)
    |> set_err (applied_structure, error)
    |> set_err (structure_expected,error)


  let parsing_approx =
    default |> set_err (syntaxerr, Level.warning)


  let lax =
    { parsing_approx with exit = Level.critical }

  let quiet = { lax with silent = Level.error }



  let pp_lvl ppf = function
    | None -> ()
    | Some lvl ->
      Pp.fp ppf "[%s%s\x1b[39m]"
        (Level.to_decorator lvl) (Level.to_string lvl)

  let rec pp_map ppf = function
    | name, Level {expl; lvl} ->
      Pp.fp ppf "@;−%s%a:@;@[<hov>%a@]"
        name
        pp_lvl lvl
        Format.pp_print_text expl
    | name, Map { lvl; expl; map } ->
      Pp.fp ppf "@;−%s%a:%s@; @[<v2> %a @]"
        name
        pp_lvl lvl
        expl
        Pp.( list ~sep:(s "@;") @@ pp_map) (Name.Map.bindings map)


  let pp ppf pol = Pp.fp ppf "%a@." pp_map ("Polycy",pol.map)

  end

let set = Polycy.set_err
let handle (polycy:Polycy.t) error =
  error.log {
    level =
      Polycy.level polycy error;
    silent = polycy.silent;
    exit = polycy.exit;
  }

let is_silent polycy fault =
  Polycy.level polycy fault <= polycy.silent
