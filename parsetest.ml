open OUnit2
open OUnitTest
open Yamllexer
open Yamlparser

let pa s =
  s |> Stream.of_string |> Grammar.Entry.parse yamlval_eoi

let busted_tests = "busted" >::: [
  ]

let ok_tests = "ok" >::: [
    "simple" >:: (fun ctxt ->
        assert_equal ~printer:Yaml.to_string_exn
          (`String {|"1"|})
          (pa {|"1"|})
      )
  ]
;;

let tests = "all" >::: [busted_tests ; ok_tests]

if not !Sys.interactive then
  run_test_tt_main tests
;;
