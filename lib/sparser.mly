%token <string> ATOM
%token LS, RS, LC, RC
%token COLON, COMMA
%token L, R, EOF

%type <Schematic.Untyped.t> main
%start main

%{
open Schematic.Untyped
%}

%%

main:
  | L l=slist R EOF { List l }
  | LS a=j_array RS EOF { Array a }
  | LC o=j_obj RC EOF { Obj o }
  | a = ATOM EOF { Atom a }

slist:
  | l = list(sexp) { l  }

sexp:
  | a = ATOM    { Atom a }
  | L l=slist R { List l }

j_array:
  | a = separated_list(COMMA,json) { a  }

j_obj:
  | l = separated_list(COMMA,field) {  l }

field:
  k=ATOM COLON v=json {k,v}

json:
  | s=ATOM          { Atom s  }
  | LS a=j_array RS { Array a }
  | LC o=j_obj RC   { Obj o   }
