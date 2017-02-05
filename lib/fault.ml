
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

let mark_open_tag tag =
  let b = "\x1b[1m" in
  match of_string tag with
  | 4 -> b ^ "\x1b[91m"
  | 3 -> b ^ "\x1b[31m"
  | 2 -> b ^ "\x1b[35m"
  | 1 -> b ^ "\x1b[36m"
  | _ -> match tag with
    | "loc" -> b
    | "title" -> b
    | "m" -> b
    | _ -> b

let mark_close_tag _tag =
  "\x1b[0m"
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
      ("@[[@{<critical>Critical error@}]: "^^fmt ^^"@]@." )

  let kerror k fmt =
    kf k
    ("@[[@{<error>%s@}]: "^^fmt^^"@]@.")
    (if k = Ok then "Error" else "Fatal error")

  let kwarning k fmt = kf k
    ("@[[@{<warning>%s@}]: " ^^ fmt ^^"@]@." )
    (if k = Ok then "Warning" else "Fatal warning")

  let knotification k fmt = kf k
    ("@[[@{<notification>%s@}]: " ^^ fmt ^^"@]@.")
      (if k = Ok then "Notification" else "Fatal notification")


  let kwhisper k fmt = kf k
    ("@[[@{<whisper>%s@}]: " ^^ fmt ^^"@]@.")
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
let loc ppf (path,x)= Pp.fp ppf "@{<loc>%a:%a@}" Paths.Pkg.pp path Loc.pp x

let enable_colors ppf =
  Format.pp_set_tags ppf true;
  Format.pp_set_mark_tags ppf true;
  Format.pp_set_formatter_tag_functions ppf
    Level.{ (Format.pp_get_formatter_tag_functions ppf ()) with
      mark_open_tag; mark_close_tag }

module Policy = struct

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


  let pp_lvl ppf = function
    | None -> ()
    | Some lvl ->
      let name = Level.to_string lvl in
      Pp.fp ppf "[";
      Format.open_tag name;
      Pp.string ppf name;
      Format.close_tag ();
      Pp.fp ppf "]"

  let rec pp_map ppf = function
    | name, Level {expl; lvl} ->
      Pp.fp ppf "@;−@{<title>%s@}%a:@;@[<hov>%a@]"
        name
        pp_lvl lvl
        Format.pp_print_text expl
    | name, Map { lvl; expl; map } ->
      Pp.fp ppf "@;−@{<title>%s@}%a:%s@; @[<v2> %a @]"
        name
        pp_lvl lvl
        expl
        Pp.( list ~sep:(s "@;") @@ pp_map) (Name.Map.bindings map)


  let pp ppf pol = Pp.fp ppf "%a@." pp_map ("Policy",pol.map)

  end

let set = Policy.set_err
let handle (polycy:Policy.t) error =
  error.log {
    level =
      Policy.level polycy error;
    silent = polycy.silent;
    exit = polycy.exit;
  }

let is_silent polycy fault =
  Policy.level polycy fault <= polycy.silent
