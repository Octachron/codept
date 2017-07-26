
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | RS
    | RC
    | R
    | LS
    | LC
    | L
    | EOF
    | COMMA
    | COLON
    | ATOM of (
# 1 "lib/sparser.mly"
       (string)
# 20 "lib/sparser.ml"
  )
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState34
  | MenhirState30
  | MenhirState29
  | MenhirState25
  | MenhirState18
  | MenhirState13
  | MenhirState5
  | MenhirState3
  | MenhirState2
  | MenhirState1

# 9 "lib/sparser.mly"
  
open Schematic.Untyped

# 53 "lib/sparser.ml"

let rec _menhir_goto_separated_nonempty_list_COMMA_json_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Schematic.Untyped.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState1 | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Schematic.Untyped.t list)) = _v in
        let _v : (Schematic.Untyped.t list) = 
# 130 "/home/ombre/.opam/4.05.0+flambda/lib/menhir/standard.mly"
    ( x )
# 65 "lib/sparser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs0 : (Schematic.Untyped.t list)) = _v in
        let _v : (Schematic.Untyped.t list) = let a =
          let xs = xs0 in
          
# 206 "/home/ombre/.opam/4.05.0+flambda/lib/menhir/standard.mly"
    ( xs )
# 75 "lib/sparser.ml"
          
        in
        
# 31 "lib/sparser.mly"
                                   ( a  )
# 81 "lib/sparser.ml"
         in
        _menhir_goto_j_array _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Schematic.Untyped.t list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Schematic.Untyped.t))) = _menhir_stack in
        let _2 = () in
        let _v : (Schematic.Untyped.t list) = 
# 217 "/home/ombre/.opam/4.05.0+flambda/lib/menhir/standard.mly"
    ( x :: xs )
# 93 "lib/sparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_json_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Schematic.Untyped.t) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState25 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Schematic.Untyped.t) list)) = _v in
        let _v : ((string * Schematic.Untyped.t) list) = 
# 130 "/home/ombre/.opam/4.05.0+flambda/lib/menhir/standard.mly"
    ( x )
# 109 "lib/sparser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs0 : ((string * Schematic.Untyped.t) list)) = _v in
        let _v : ((string * Schematic.Untyped.t) list) = let l =
          let xs = xs0 in
          
# 206 "/home/ombre/.opam/4.05.0+flambda/lib/menhir/standard.mly"
    ( xs )
# 119 "lib/sparser.ml"
          
        in
        
# 35 "lib/sparser.mly"
                                    (  l )
# 125 "lib/sparser.ml"
         in
        _menhir_goto_j_obj _menhir_env _menhir_stack _menhir_s _v
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Schematic.Untyped.t) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Schematic.Untyped.t))) = _menhir_stack in
        let _2 = () in
        let _v : ((string * Schematic.Untyped.t) list) = 
# 217 "/home/ombre/.opam/4.05.0+flambda/lib/menhir/standard.mly"
    ( x :: xs )
# 137 "lib/sparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_j_array : _menhir_env -> 'ttv_tail -> _menhir_state -> (Schematic.Untyped.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (a : (Schematic.Untyped.t list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Schematic.Untyped.t) = 
# 42 "lib/sparser.mly"
                    ( Array a )
# 167 "lib/sparser.ml"
             in
            _menhir_goto_json _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, (a : (Schematic.Untyped.t list))) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _v : (
# 6 "lib/sparser.mly"
      (Schematic.Untyped.t)
# 196 "lib/sparser.ml"
                ) = 
# 17 "lib/sparser.mly"
                        ( Array a )
# 200 "lib/sparser.ml"
                 in
                _menhir_goto_main _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_json : _menhir_env -> 'ttv_tail -> _menhir_state -> (Schematic.Untyped.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (k : (
# 1 "lib/sparser.mly"
       (string)
# 228 "lib/sparser.ml"
        ))), _, (v : (Schematic.Untyped.t))) = _menhir_stack in
        let _2 = () in
        let _v : (string * Schematic.Untyped.t) = 
# 38 "lib/sparser.mly"
                      (k,v)
# 234 "lib/sparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ATOM _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
        | RC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (string * Schematic.Untyped.t))) = _menhir_stack in
            let _v : ((string * Schematic.Untyped.t) list) = 
# 215 "/home/ombre/.opam/4.05.0+flambda/lib/menhir/standard.mly"
    ( [ x ] )
# 258 "lib/sparser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_field_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState1 | MenhirState18 | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ATOM _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | LC ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | LS ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
        | RS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Schematic.Untyped.t))) = _menhir_stack in
            let _v : (Schematic.Untyped.t list) = 
# 215 "/home/ombre/.opam/4.05.0+flambda/lib/menhir/standard.mly"
    ( [ x ] )
# 293 "lib/sparser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_json_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_j_obj : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Schematic.Untyped.t) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (o : ((string * Schematic.Untyped.t) list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Schematic.Untyped.t) = 
# 43 "lib/sparser.mly"
                    ( Obj o   )
# 324 "lib/sparser.ml"
             in
            _menhir_goto_json _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, (o : ((string * Schematic.Untyped.t) list))) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _v : (
# 6 "lib/sparser.mly"
      (Schematic.Untyped.t)
# 353 "lib/sparser.ml"
                ) = 
# 18 "lib/sparser.mly"
                      ( Obj o )
# 357 "lib/sparser.ml"
                 in
                _menhir_goto_main _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_sexp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Schematic.Untyped.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Schematic.Untyped.t list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Schematic.Untyped.t))) = _menhir_stack in
        let _v : (Schematic.Untyped.t list) = 
# 187 "/home/ombre/.opam/4.05.0+flambda/lib/menhir/standard.mly"
    ( x :: xs )
# 386 "lib/sparser.ml"
         in
        _menhir_goto_list_sexp_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState29 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (l : (Schematic.Untyped.t list)) = _v in
        let _v : (Schematic.Untyped.t list) = 
# 23 "lib/sparser.mly"
                   ( l  )
# 396 "lib/sparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState30 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | R ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (l : (Schematic.Untyped.t list))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (Schematic.Untyped.t) = 
# 27 "lib/sparser.mly"
                ( List l )
# 415 "lib/sparser.ml"
                 in
                _menhir_goto_sexp _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState29 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | R ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EOF ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _, (l : (Schematic.Untyped.t list))) = _menhir_stack in
                    let _4 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : (
# 6 "lib/sparser.mly"
      (Schematic.Untyped.t)
# 444 "lib/sparser.ml"
                    ) = 
# 16 "lib/sparser.mly"
                    ( List l )
# 448 "lib/sparser.ml"
                     in
                    _menhir_goto_main _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        _menhir_fail ()

and _menhir_goto_sexp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Schematic.Untyped.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | L ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | R ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Schematic.Untyped.t list) = 
# 30 "lib/sparser.mly"
                                   ( [] )
# 491 "lib/sparser.ml"
     in
    _menhir_goto_j_array _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | LC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | RS ->
        _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | RC ->
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "lib/sparser.mly"
       (string)
# 532 "lib/sparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (
# 1 "lib/sparser.mly"
       (string)
# 540 "lib/sparser.ml"
    )) = _v in
    let _v : (Schematic.Untyped.t) = 
# 41 "lib/sparser.mly"
                    ( Atom s  )
# 545 "lib/sparser.ml"
     in
    _menhir_goto_json _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Schematic.Untyped.t) list) = 
# 34 "lib/sparser.mly"
                                    ( [] )
# 554 "lib/sparser.ml"
     in
    _menhir_goto_j_obj _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "lib/sparser.mly"
       (string)
# 561 "lib/sparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ATOM _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | LC ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | LS ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Schematic.Untyped.t list) = 
# 185 "/home/ombre/.opam/4.05.0+flambda/lib/menhir/standard.mly"
    ( [] )
# 636 "lib/sparser.ml"
     in
    _menhir_goto_list_sexp_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | L ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | R ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "lib/sparser.mly"
       (string)
# 660 "lib/sparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (a : (
# 1 "lib/sparser.mly"
       (string)
# 668 "lib/sparser.ml"
    )) = _v in
    let _v : (Schematic.Untyped.t) = 
# 26 "lib/sparser.mly"
                ( Atom a )
# 673 "lib/sparser.ml"
     in
    _menhir_goto_sexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_main : _menhir_env -> 'ttv_tail -> (
# 6 "lib/sparser.mly"
      (Schematic.Untyped.t)
# 680 "lib/sparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 6 "lib/sparser.mly"
      (Schematic.Untyped.t)
# 688 "lib/sparser.ml"
    )) = _v in
    Obj.magic _1

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

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 6 "lib/sparser.mly"
      (Schematic.Untyped.t)
# 707 "lib/sparser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, (a : (
# 1 "lib/sparser.mly"
       (string)
# 733 "lib/sparser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 6 "lib/sparser.mly"
      (Schematic.Untyped.t)
# 739 "lib/sparser.ml"
            ) = 
# 19 "lib/sparser.mly"
                 ( Atom a )
# 743 "lib/sparser.ml"
             in
            _menhir_goto_main _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | L ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ATOM _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | L ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | R ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
    | LC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ATOM _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | RC ->
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
    | LS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ATOM _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
        | LC ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | LS ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | RS ->
            _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)

# 219 "/home/ombre/.opam/4.05.0+flambda/lib/menhir/standard.mly"
  


# 806 "lib/sparser.ml"
