
module Basics = struct
  
  exception Error
  
  type token = 
    | R
    | L
    | EOF
    | ATOM of (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 14 "lib/sexp/sexp_parse.ml"
  )
  
end

include Basics

let _eRR =
  Basics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState29
  | MenhirState27
  | MenhirState25
  | MenhirState18
  | MenhirState9
  | MenhirState7
  | MenhirState6

# 10 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
  
open Sexp

# 44 "lib/sexp/sexp_parse.ml"

let rec _menhir_goto_list0 : _menhir_env -> 'ttv_tail -> 'tv_list0 -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv149) * 'tv_list0) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | R ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv145) * 'tv_list0) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv141) * 'tv_list0)) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv139) * 'tv_list0)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, (_2 : 'tv_list0)) = _menhir_stack in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 4 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.any)
# 72 "lib/sexp/sexp_parse.ml"
            ) = 
# 18 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
                  ( _2 )
# 76 "lib/sexp/sexp_parse.ml"
             in
            _menhir_goto_sexp _menhir_env _menhir_stack _v) : 'freshtv140)) : 'freshtv142)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv143) * 'tv_list0)) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv144)) : 'freshtv146)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv147) * 'tv_list0) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv148)) : 'freshtv150)

and _menhir_goto__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_sexp0) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * 'tv_sexp0) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_sexp0)), _, (_2 : 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = _menhir_stack in
        let _v : 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list = 
# 35 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
             ( _1 :: _2 )
# 105 "lib/sexp/sexp_parse.ml"
         in
        _menhir_goto__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv78)) : 'freshtv80)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | R ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv83 * _menhir_state) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv81 * _menhir_state) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_sexp0 = 
# 39 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
             ( Any (List (_2)) )
# 126 "lib/sexp/sexp_parse.ml"
             in
            _menhir_goto_sexp0 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv82)) : 'freshtv84)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv85 * _menhir_state) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)) : 'freshtv88)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv105) * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 141 "lib/sexp/sexp_parse.ml"
        )) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | R ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv101) * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 151 "lib/sexp/sexp_parse.ml"
            )) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EOF ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv97) * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 161 "lib/sexp/sexp_parse.ml"
                )) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv95) * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 167 "lib/sexp/sexp_parse.ml"
                )) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, (_2 : (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 172 "lib/sexp/sexp_parse.ml"
                ))), _, (_3 : 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _1 = () in
                let _v : (
# 7 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.one_and_many Sexp.t)
# 180 "lib/sexp/sexp_parse.ml"
                ) = 
# 24 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
                    ( Keyed_list (_2,_3) )
# 184 "lib/sexp/sexp_parse.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv93) = _menhir_stack in
                let (_v : (
# 7 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.one_and_many Sexp.t)
# 191 "lib/sexp/sexp_parse.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv91) = Obj.magic _menhir_stack in
                let (_v : (
# 7 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.one_and_many Sexp.t)
# 198 "lib/sexp/sexp_parse.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv89) = Obj.magic _menhir_stack in
                let ((_1 : (
# 7 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.one_and_many Sexp.t)
# 205 "lib/sexp/sexp_parse.ml"
                )) : (
# 7 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.one_and_many Sexp.t)
# 209 "lib/sexp/sexp_parse.ml"
                )) = _v in
                (Obj.magic _1 : 'freshtv90)) : 'freshtv92)) : 'freshtv94)) : 'freshtv96)) : 'freshtv98)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv99) * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 219 "lib/sexp/sexp_parse.ml"
                )) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)) : 'freshtv102)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv103) * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 230 "lib/sexp/sexp_parse.ml"
            )) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)) : 'freshtv106)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | R ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv119) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EOF ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv115) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv113) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, (_2 : 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _v : (
# 5 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.many Sexp.t)
# 258 "lib/sexp/sexp_parse.ml"
                ) = 
# 21 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
               ( List( _2 ) )
# 262 "lib/sexp/sexp_parse.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv111) = _menhir_stack in
                let (_v : (
# 5 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.many Sexp.t)
# 269 "lib/sexp/sexp_parse.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv109) = Obj.magic _menhir_stack in
                let (_v : (
# 5 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.many Sexp.t)
# 276 "lib/sexp/sexp_parse.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv107) = Obj.magic _menhir_stack in
                let ((_1 : (
# 5 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.many Sexp.t)
# 283 "lib/sexp/sexp_parse.ml"
                )) : (
# 5 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.many Sexp.t)
# 287 "lib/sexp/sexp_parse.ml"
                )) = _v in
                (Obj.magic _1 : 'freshtv108)) : 'freshtv110)) : 'freshtv112)) : 'freshtv114)) : 'freshtv116)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv117) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)) : 'freshtv120)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv121) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)) : 'freshtv124)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | R ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv125) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | L ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | R ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv126)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv127) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)) : 'freshtv130)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv133) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv131) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, (_2 : 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)), _, (_4 : 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_list0 = 
# 31 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
                  (Any (List ( Any(List(_2)) :: _4)) )
# 344 "lib/sexp/sexp_parse.ml"
         in
        _menhir_goto_list0 _menhir_env _menhir_stack _v) : 'freshtv132)) : 'freshtv134)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv137 * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 352 "lib/sexp/sexp_parse.ml"
        )) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 358 "lib/sexp/sexp_parse.ml"
        )) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, (_1 : (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 363 "lib/sexp/sexp_parse.ml"
        ))), _, (_2 : 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = _menhir_stack in
        let _v : 'tv_list0 = 
# 30 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
              ( Any( Keyed_list(_1, _2) ) )
# 368 "lib/sexp/sexp_parse.ml"
         in
        _menhir_goto_list0 _menhir_env _menhir_stack _v) : 'freshtv136)) : 'freshtv138)

and _menhir_goto_sexp0 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sexp0 -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv75 * _menhir_state * 'tv_sexp0) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | L ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | R ->
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv76)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 399 "lib/sexp/sexp_parse.ml"
        )) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv62)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv63) * _menhir_state * 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv66)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv68)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_sexp0) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73) * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 430 "lib/sexp/sexp_parse.ml"
        )) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv74)

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list = 
# 34 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
  ( [] )
# 439 "lib/sexp/sexp_parse.ml"
     in
    _menhir_goto__home_ombre_Projets_Ocaml_codept_lib_sexp_sexp_parse_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | L ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | R ->
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 463 "lib/sexp/sexp_parse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv59) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 473 "lib/sexp/sexp_parse.ml"
    )) : (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 477 "lib/sexp/sexp_parse.ml"
    )) = _v in
    ((let _v : 'tv_sexp0 = 
# 38 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
         ( Any(Atom _1) )
# 482 "lib/sexp/sexp_parse.ml"
     in
    _menhir_goto_sexp0 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv60)

and _menhir_goto_sexp : _menhir_env -> 'ttv_tail -> (
# 4 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.any)
# 489 "lib/sexp/sexp_parse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
    let (_v : (
# 4 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.any)
# 497 "lib/sexp/sexp_parse.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
    let ((_1 : (
# 4 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.any)
# 504 "lib/sexp/sexp_parse.ml"
    )) : (
# 4 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.any)
# 508 "lib/sexp/sexp_parse.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv56)) : 'freshtv58)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and atom : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 6 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.atomic Sexp.t)
# 537 "lib/sexp/sexp_parse.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv53) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49) = Obj.magic _menhir_stack in
        let (_v : (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 552 "lib/sexp/sexp_parse.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv45 * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 563 "lib/sexp/sexp_parse.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv43 * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 569 "lib/sexp/sexp_parse.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, (_1 : (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 574 "lib/sexp/sexp_parse.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 6 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.atomic Sexp.t)
# 580 "lib/sexp/sexp_parse.ml"
            ) = 
# 27 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
           ( Atom _1 )
# 584 "lib/sexp/sexp_parse.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv41) = _menhir_stack in
            let (_v : (
# 6 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.atomic Sexp.t)
# 591 "lib/sexp/sexp_parse.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv39) = Obj.magic _menhir_stack in
            let (_v : (
# 6 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.atomic Sexp.t)
# 598 "lib/sexp/sexp_parse.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv37) = Obj.magic _menhir_stack in
            let ((_1 : (
# 6 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.atomic Sexp.t)
# 605 "lib/sexp/sexp_parse.ml"
            )) : (
# 6 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.atomic Sexp.t)
# 609 "lib/sexp/sexp_parse.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv38)) : 'freshtv40)) : 'freshtv42)) : 'freshtv44)) : 'freshtv46)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv47 * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 619 "lib/sexp/sexp_parse.ml"
            )) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv48)) : 'freshtv50)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv52)) : 'freshtv54))

and keyed : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 7 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.one_and_many Sexp.t)
# 632 "lib/sexp/sexp_parse.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv35) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | L ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ATOM _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv27) = Obj.magic _menhir_stack in
            let (_v : (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 653 "lib/sexp/sexp_parse.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | L ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | R ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv29) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv30)) : 'freshtv32)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv34)) : 'freshtv36))

and many : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 5 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.many Sexp.t)
# 685 "lib/sexp/sexp_parse.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv25) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | L ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ATOM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | L ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | R ->
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv24)) : 'freshtv26))

and sexp : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 4 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.any)
# 720 "lib/sexp/sexp_parse.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv19) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
        let (_v : (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 735 "lib/sexp/sexp_parse.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv3 * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 746 "lib/sexp/sexp_parse.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1 * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 752 "lib/sexp/sexp_parse.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, (_1 : (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 757 "lib/sexp/sexp_parse.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 4 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
      (Sexp.any)
# 763 "lib/sexp/sexp_parse.ml"
            ) = 
# 17 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
             ( Any(Atom (_1)) )
# 767 "lib/sexp/sexp_parse.ml"
             in
            _menhir_goto_sexp _menhir_env _menhir_stack _v) : 'freshtv2)) : 'freshtv4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv5 * (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 777 "lib/sexp/sexp_parse.ml"
            )) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv6)) : 'freshtv8)
    | L ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ATOM _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
            let (_v : (
# 1 "/home/ombre/Projets/Ocaml/codept/lib/sexp/sexp_parse.mly"
       (string)
# 792 "lib/sexp/sexp_parse.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | L ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | R ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv10)
        | L ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | L ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | R ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv12)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv14)) : 'freshtv16)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv18)) : 'freshtv20))

# 220 "/home/ombre/.opam/4.04.0+flambda/lib/menhir/standard.mly"
  


# 841 "lib/sexp/sexp_parse.ml"
