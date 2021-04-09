
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

type 'a itag = ..
module type tag = sig
  type arg
  type _ itag+= E: arg itag
end
type 'a tag = (module tag with type arg = 'a)
type dtag = Dyn: 'a tag -> dtag [@@unboxed]
type fault = Err: 'a tag * 'a -> fault
type t = fault

let tag (type t) (): t tag =
  (module struct type arg = t type _ itag += E: arg itag end)


type 'a printer = Format.formatter -> 'a -> unit

module Log = struct

  let msg lvl simple fatal critical printer ppf x=
    let title = if critical then fatal else simple in
    Format.fprintf ppf "@[[%a]: %a@]@."
      (Format_tags.tagged lvl) title
    printer x

  let kcritical x = msg Level.critical "Critical error" "Critical error" x

  let kerror x = msg Level.error "Error" "Fatal error" x
  let kwarning x = msg Level.warning "Warning" "Fatal warning" x
  let knotification x = msg Level.notification "Notification" "Fatal notification" x
  let kinfo x = msg Level.info "Misc" "Fatal accident" x

  let critical fmt = kcritical true fmt
  let error fmt = kerror false fmt
  let warning fmt = kwarning false fmt
  let notification fmt = kwarning false fmt
  let info fmt = kinfo false fmt

end

type log_info = { silent:Level.t; level:Level.t; exit:Level.t}
let log i printer ppf x =
  let fns = Log.[| kinfo; knotification; kwarning; kerror; kcritical |] in
  let fn x = fns.(Level.to_int i.level) (i.level >= i.exit) printer ppf x in
  if i.level < i.silent then ()
  else if i.level >= Level.critical then
    Log.critical printer ppf x
  else if i.level >= i.exit then
    (fn x; exit 1)
  else
    fn x

type explanation = string

type 'a info = {
  tag: 'a tag;
  path: Paths.S.t;
  expl: explanation;
  printer: Format.formatter -> 'a -> unit
}

let info path expl printer =
  let tag = tag () in
  { tag; path; expl; printer }

let emit info x = Err(info.tag,x)

type dyn_info = Info: 'a info -> dyn_info [@@unboxed]

let check (type a) ((module X): a tag) (Info info) =
  let module Y = (val info.tag) in
  match Y.E with
  | X.E -> Some (info:a info)
  | _ -> None

module Policy = struct

  type map =
    | Level of {expl: explanation; lvl: Level.t option}
    | Map of {expl:explanation; lvl:Level.t option; map: map Name.map}

  module Register = Map.Make(struct type t = dtag let compare=compare end)
  type t = {
    silent: Level.t;
    exit:Level.t;
    map:map;
    register: dyn_info Register.t
  }
  type policy = t

  let make ~silent ~exit =
    let map = Level { expl = ""; lvl = Some Level.critical } in
    { silent; exit; map; register = Register.empty }

  let find_info tag p =
    Register.find (Dyn tag) p.register

  let rec find_lvl default pol l  =
    let with_default = Option.default default in
    match pol, l with
    | Level h, _ -> with_default h.lvl
    | Map m, a :: q  ->
      begin
        let h = with_default m.lvl in
        try find_lvl h (Name.Map.find a m.map) q with
          Not_found -> h
      end
    | Map m, [] -> with_default m.lvl

  let level_info {map; exit; _ } (Info error) =
    find_lvl exit map error.path

  let level p (Err(tag,_)) =
    let info = find_info tag p in
    level_info p info

  let rec set ?lvl ?expl path env = match path, env with
    | [], Level l -> Level {expl = Option.default l.expl expl;lvl }
    | [], Map m -> Map { m with lvl; expl = Option.default m.expl expl }
    | a :: q, Level l ->
      Map{ lvl = None; expl = "";
           map=Name.Map.singleton a @@ set ?lvl ?expl q @@ Level l
         }
    | a :: q, Map m ->
      let env' =
        Option.default (Level {lvl=m.lvl; expl = m.expl})
          (Name.Map.find_opt a m.map) in
      let elt = set ?lvl ?expl q env' in
      let map = Name.Map.add a elt m.map in
      Map{m with map}

  let set ?lvl ?expl p pol = { pol with map = set ?lvl ?expl p pol.map }

  let register ?lvl error policy =
    let policy = set ?lvl ~expl:error.expl error.path policy in
    let register = Register.add (Dyn error.tag) (Info error) policy.register in
    { policy with register }

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

  let set_exit exit p = { p with exit }
  let set_silent silent p = {p with silent}

  end

let register = Policy.register
let handle policy (Err(tag,e)) =
  let info = Policy.find_info tag policy in
  let log_info = {
    level = Policy.level_info policy info;
    silent = policy.silent;
    exit = policy.exit;
  } in
  let ppf = Format.err_formatter in
  Option.iter (fun info -> log log_info info.printer ppf e) (check tag info)

let raise policy info x = handle policy (emit info x)

let is_silent policy info =
  Policy.level_info policy (Info info) <= policy.silent
