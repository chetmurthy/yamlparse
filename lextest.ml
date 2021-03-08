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

let lex_test name toks s =
  name >:: (fun ctxt ->
      assert_equal ~printer:pp_tokens
        toks
        (List.map fst (tokens s))
    )

let lex_test1 (a,b,c) = lex_test a b c


let busted_tests = "busted" >::: [
    "token-1" >:: (fun ctxt ->
        assert_raises (Failure "pop_styles: dedent did not move back to previous indent position")
          (fun () -> (tokens {|
  a: b
    b: c
   d: e
|}))
      )
  ]

let ok_tests = "ok" >::: (List.map lex_test1 [
    ("token-2''"
    ,[("INDENT","");("KEY","a");("INDENT","");("KEY","b")
     ;("RAWSTRING","c");("KEY","d");("RAWSTRING","e");("DEDENT","")
     ;("DEDENT","");("EOI","")]
    ,{|
  a:
   b: c
   d: e
|})
  ;("token-3"
   ,[("KEY","a");("DQSTRING",{|"b"|});("EOI","")]
   ,{|a : "b"|})
  ;("token-4"
   ,[("KEY","a");("RAWSTRING",{|b: "c"|});("EOI","")]
   ,{|a : b: "c"|})

  ;("token-5"
   ,[("KEY","a");("RAWSTRING","b c");("EOI","")]
   ,{|a : b c|})
  ;("token-6"
   ,[("KEY","a");("RAWSTRING","b c");("KEY","b");("RAWSTRING","b c");("EOI","")]
   ,{|
a : b c
b : b c
|})
  ;("token-7"
   ,[("DASH","");("BLOCKSTRING","a");("EOI","")]
   ,{|- a|})

  ;("token-8"
   ,[("DASH","");("BLOCKSTRING","a");("DASH","")
    ;("BLOCKSTRING","b");("EOI","")]
   ,{|
- a
- b
|})
  ;("token-9"
   ,[("DQSTRING",{|"foo"|});("EOI","")]
       ,{|
"foo"
|})
  ;("token-10"
   ,[("DQSTRING","\"foo\"");("EOI","")]
   ,{|
  "foo"
|})
  ;("token-11"
   ,[("KEY","a");("DQSTRING",{|"foo"|});("EOI","")]
   ,{|
a:
  "foo"
|})

  ])
;;

let tests = "all" >::: [busted_tests ; ok_tests]

if not !Sys.interactive then
  run_test_tt_main tests
;;
