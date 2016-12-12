%token <string> ATOM
%token L, R, EOF EOF

%type <Sexp.t> sexp
%start atom, list

%{
open List
%}

%%

sexp:
  | ATOM EOF { Atom ($1) }
  | list EOF { $1 }


list:
  | { List [] }
  | sexp' list { List ($1 :: $2) }

sexp':
  | ATOM { Atom $1 }
  | L list R { List $2 }
