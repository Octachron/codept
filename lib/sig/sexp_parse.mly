%token <string> ATOM
%token L, R, EOF

%type <Sexp.any> sexp
%type <Sexp.many Sexp.t> many
%type <Sexp.atomic Sexp.t> atom
%start sexp, many, atom

%{
open Sexp
%}

%%

sexp:
  | ATOM EOF { Any(Atom ($1)) }
  | L list R EOF { Any(List($2)) }

many:
| L list R EOF { List $2 }

atom:
| ATOM EOF { Atom $1 }

list:
  | { [] }
  | sexp0 list { $1 :: $2 }

sexp0:
  | ATOM { Any(Atom $1) }
  | L list R { Any(List $2) }
