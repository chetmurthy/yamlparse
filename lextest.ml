open OUnit2
open OUnitTest
open Yamllexer

let pp_token pps (a,b) = Fmt.(pf pps "(%a,%a)" Dump.string a Dump.string b)
let pp_tokens l = Fmt.(str "%a" (list ~sep:(const string ";") pp_token) l)

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
        assert_equal ~printer:pp_tokens [("INDENT", ""); ("EOI", "")]
          (List.map fst (tokens "  "))
      ; assert_equal ~printer:pp_tokens [("INDENT", ""); ("EOI", "")]
          (List.map fst (tokens {|  
    
  |}))
      )
]
;;

if not !Sys.interactive then
  run_test_tt_main tests
;;
