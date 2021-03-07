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
    "token-1" >:: (fun ctxt ->
        assert_equal [("INDENT", ""); ("EOI", "")]
          (List.map fst (tokens "  "))
      )
]
;;

if not !Sys.interactive then
  run_test_tt_main tests
;;
