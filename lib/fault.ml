
module Level = struct
type t = Format_tags.t
let info = Format_tags.Info
let notification = Format_tags.Notification
let warning = Format_tags.Warning
let error = Format_tags.Error
let critical = Format_tags.Critical

let of_string =
  function
  | "info" | "0" -> info
  | "notification" | "1" -> notification
  | "warning" | "2" -> warning
  | "error" | "3" -> error
  | "critical" | "4" -> critical
  | _ -> info

let to_int = function
  | Format_tags.Info -> 0
  | Format_tags.Notification -> 1
  | Format_tags.Warning -> 2
  | Format_tags.Error -> 3
  | Format_tags.Critical -> 4
  | _ -> 0

  let to_string = Format_tags.to_string

end

module Log = struct

  type 'k status =
    | Ok: (Format.formatter -> unit) status
    | Fail: (Format.formatter -> 'b) status

  let kont (type k): k status -> k = function
    | Ok -> ignore
    | Fail -> (fun _x -> exit 1)

  let kf k st = Format.kfprintf (fun ppf -> k ppf; kont st ppf)
      Format.err_formatter

  let msg (type a) lvl simple fatal (st: (_->a) status) (fmt: (_,_,_,a) format4) =
    let title = match st with
      | Ok -> simple
      | Fail -> fatal in
    Format.eprintf "@[[%a]: "
      (Format_tags.tagged lvl) title;
    kf (fun ppf -> Format.fprintf ppf "@]@.") st fmt

  let kcritical x = msg Level.critical "Critical error" "Critical error" x

  let kerror x = msg Level.error "Error" "Fatal error" x
  let kwarning x = msg Level.warning "Warning" "Fatal warning" x
  let knotification x = msg Level.notification "Notification" "Fatal notification" x
  let kinfo x = msg Level.info "Misc" "Fatal accident" x

  let critical fmt = kcritical Fail fmt
  let error fmt = kerror Ok fmt
  let warning fmt = kwarning Ok fmt
  let notification fmt = kwarning Ok fmt
  let info fmt = kinfo Ok fmt

end

type log_info = { silent:Level.t; level:Level.t; exit:Level.t}
let log i fmt =
  let fns = Log.[| kinfo; knotification; kwarning; kerror; kcritical |] in
  if i.level < i.silent then
    Format.ifprintf Format.err_formatter fmt
  else if i.level >= Level.critical then
    Log.critical fmt
  else
    begin
      let k = if i.level >= i.exit then
          Log.Fail
        else
          Log.Ok  in
      fns.(Level.to_int i.level) k fmt
    end

type explanation = string
type 'a fault = { path: Paths.S.t; expl: explanation; log: log_info -> 'a }
type 'a t = 'a fault

type loc = Paths.Pkg.t * Loc.t
let loc ppf =
  let sub ppf (path,x) = Pp.fp ppf "%a:%a" Paths.Pkg.pp path Loc.pp x in
  Format_tags.(with_tag Loc) sub ppf

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
      Pp.fp ppf "[%a]" Format_tags.(tagged lvl) name

  let rec pp_map ppf = function
    | name, Level {expl; lvl} ->
      Pp.fp ppf "@;−%a%a:@;@[<hov>%a@]"
        Format_tags.(tagged Title) name
        pp_lvl lvl
        Format.pp_print_text expl
    | name, Map { lvl; expl; map } ->
      Pp.fp ppf "@;−%a%a:%s@; @[<v2> %a @]"
        Format_tags.(tagged Title) name
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
