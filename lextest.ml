open OUnit2
open OUnitTest

let tests = "lexing" >::: [
    "indent-1" >:: (fun ctxt ->
        ()
      )
]
;;

if not !Sys.interactive then
  run_test_tt_main tests
;;
