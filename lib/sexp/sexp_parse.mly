%token <string> ATOM
%token L, R, EOF

%type <Sexp.any> sexp
%type <Sexp.many Sexp.t> many
%type <Sexp.atomic Sexp.t> atom
%type <Sexp.one_and_many Sexp.t> keyed
%start sexp, many, keyed, atom

%{
open Sexp
%}

%%

sexp:
  | ATOM EOF { Any(Atom ($1)) }
  | L list0 R EOF { $2 }

many:
| L list R EOF { List( $2 ) }

keyed:
| L ATOM list R EOF { Keyed_list ($2,$3) }

atom:
| ATOM EOF { Atom $1 }

list0:
  | ATOM list { Any( Keyed_list($1, $2) ) }
  | L R list {Any (List (Any (List []) :: $3)) }
  | L list0 R list { Any ( List ($2 :: $4) ) }

list:
| { [] }
| sexp0 list { $1 :: $2 }

sexp0:
  | ATOM { Any(Atom $1) }
  | L list0 R { $2 }
