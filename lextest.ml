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
    "token-2" >:: (fun ctxt ->
        assert_raises (Failure "pop_styles: dedent did not move back to previous indent position")
          (fun () -> (tokens {|
  a: b
    b: c
   d: e
|}))
      )
  ; "token-2''" >:: (fun ctxt ->
        assert_equal ~printer:pp_tokens
          [("INDENT","")
          ;("KEY","a")
          ;("INDENT","")
          ;("KEY","b")
          ;("RAWSTRING","c")
          ;("KEY","d")
          ;("RAWSTRING","e")
          ;("DEDENT","")
          ;("DEDENT","")
          ;("EOI","")
          ]
          (List.map fst (tokens {|
  a:
   b: c
   d: e
|}))
      )
  ; "token-3" >:: (fun ctxt ->
        assert_equal ~printer:pp_tokens
          [("KEY","a")
          ;("DQSTRING",{|"b"|})
          ;("EOI","")]
          (List.map fst (tokens {|a : "b"|}))
      )
  ; "token-4" >:: (fun ctxt ->
        assert_equal ~printer:pp_tokens
          [("KEY","a")
          ;("RAWSTRING",{|b: "c"|})
          ;("EOI","")]
          (List.map fst (tokens {|a : b: "c"|}))
      )
  ; "token-5" >:: (fun ctxt ->
        assert_equal ~printer:pp_tokens
          [("KEY","a")
          ;("RAWSTRING","b c")
          ;("EOI","")]
          (List.map fst (tokens {|a : b c|}))
      )
  ; "token-5" >:: (fun ctxt ->
        assert_equal ~printer:pp_tokens
          [("DASH","");("BLOCKSTRING","a");("EOI","")]
          (List.map fst (tokens {|- a|}))
      )
]
;;

if not !Sys.interactive then
  run_test_tt_main tests
;;
