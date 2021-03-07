open OUnit2
open OUnitTest
open Yamllexer

let tokens s =
  let tokfun = make_token () in
  let lb = Lexing.from_string s in
  let rec tokrec () =
    match tokfun lb with
      (("EOI",""), _) as t -> [t]
    | t -> t::(tokrec ())
  in tokrec ()

let tests = "lexing" >::: [
    "indent-1" >:: (fun ctxt ->
        ()
      )
  ; "token-1" >:: (fun ctxt ->
        let tok = make_token () in
        let lb = Lexing.from_string "  " in
        assert_equal ("INDENT", "") (fst (tok lb))
      )
  ; "token-2" >:: (fun ctxt ->
        let tok = make_token () in
        let lb = Lexing.from_string "  " in
        assert_equal ("INDENT", "") (fst (tok lb)) ;
        assert_equal ("EOI", "") (fst (tok lb))
      )
]
;;

if not !Sys.interactive then
  run_test_tt_main tests
;;
