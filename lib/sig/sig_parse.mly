%token <string> TEXT
%token L, R, ARGS, MODULES, MODULE_TYPES, EOF

%type <Module.t list> top_modules
%type <Module.signature> top_signature
%start top_signature
%start top_modules

%{
open Module
let relocate m = {m with origin =
  Unit {source= Special "Persistent signature"; file = [m.name] }
  }
%}

%%

top_modules:
  | EOF {[]}
  | module_ top_modules {List.map relocate ($1 :: $2)}

top_signature:
  | signature EOF {$1}

module_:
  | L module_def R { $2 }

module_def:
  | TEXT L ARGS module_args R signature { Module.create ~args:$4 $1 $6}
  | TEXT signature { Module.create $1 $2}

module_args:
  |            { [] }
  | module_arg module_args { $1 :: $2 }

module_arg:
  | L R { None }
  | L module_ R { Some $2 }

signature:
  | { Sig.empty }
  | L MODULES modules R module_types_opt { Sig.(merge (of_list $3) $5) }
  | L MODULE_TYPES modules R {Sig.of_list_type $3}

module_types_opt:
| {Sig.empty}
| L MODULE_TYPES modules R { Sig.of_list_type $3 }

modules:
  | {[]}
  | module_ modules { $1 :: $2 }
