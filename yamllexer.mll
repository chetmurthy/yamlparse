{

let locate ~comments ?spos lb v =
  let open Lexing in
  let spos = match spos with None -> lexeme_start_p lb | Some spos -> spos in
  let epos = lexeme_end lb in
  let loc = Ploc.make_loc "" spos.pos_lnum spos.pos_bol (spos.pos_cnum, epos) comments in
  (v, loc)

let locate_no_comments ?spos lb v =
  let open Lexing in
  let spos = match spos with None -> lexeme_start_p lb | Some spos -> spos in
  locate ~spos:spos ~comments:"" lb v

module St = struct
type style =
    BLOCK of int
  | FLOW

type value_token =
    NEWLINE
  | KEY of string
  | DQKEY of string
  | DASH
  | DQSTRING of string
  | BLOCKSTRING of string
  | EOI

type token = (string * string) * Ploc.t

type t = {
  mutable pushback : token list
; mutable style_stack : style list
; mutable bol : bool
}

let mk () = {
  pushback = []
; style_stack = [BLOCK 0]
; bol = true
}
end

}

let indent = ' '+
let wschar = [' ' '\t']
let linews = [' ' '\t']+
let newline = '\r'? '\n'
let linechar = [^ '\r' '\n']
let decimal_digit = ['0'-'9']
let decimal = decimal_digit+
let comment = "#" [^ '\n']* '\n'
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

(* stolen from OCaml 4.12.0 source *)
let float =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?

(* stolen w/mods from OCaml 4.12.0 source *)
let dqstring = '"' ( [^ '"']
    | '\\' (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '])
    | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
    | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7']
    | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
   ) '"'

rule _indent = parse
  | indent ? { locate_no_comments lexbuf () }

and _valuetoken = parse
  | newline { let rv = locate_no_comments lexbuf St.NEWLINE in Lexing.new_line lexbuf ; Some rv }
  | (ident as keys) wschar* ":" wschar+ { Some (locate_no_comments lexbuf (St.KEY keys)) }
  | (dqstring as keys) wschar* ":" wschar+ { Some (locate_no_comments lexbuf (St.DQKEY keys)) }
  | "- " { Some (locate_no_comments lexbuf St.DASH) }
  | eof { Some (locate_no_comments lexbuf St.EOI) }
  | "" { None }

and _blockstring = parse
  | dqstring { locate_no_comments lexbuf (St.DQSTRING (Lexing.lexeme lexbuf)) }
  | linechar+ { locate_no_comments lexbuf (St.BLOCKSTRING (Lexing.lexeme lexbuf)) }

{
open St
let rec pop_styles loc rev_pushback = function
    ((BLOCK m)::sst, n) when n < m -> pop_styles loc ((("DEDENT",""),loc)::rev_pushback) (sst, n)
  | ((BLOCK m)::sst, n) when n = m && m > 0 -> ((("DEDENT",""),loc)::rev_pushback, sst)
  | ((BLOCK m)::sst, n) when n = m && m = 0 ->
    assert (sst = []) ;
    ((("DEDENT",""),loc)::rev_pushback, [BLOCK 0])
  | _ -> failwith "pop_styles: dedent did not move back to previous indent position"

let rec tokenize st lexbuf =
  match st with
    {pushback = h :: t} ->
    st.pushback <- t ;
    h

  | {pushback = [] ; bol = true; style_stack = ((BLOCK m)::t as sst)} ->
    let (_,loc) = _indent lexbuf in
    let n = Ploc.last_pos loc - Ploc.bol_pos loc in
    st.bol <- false ;
    if n = m then
      tokenize st lexbuf
    else if n > m then begin
      st.style_stack <- (BLOCK n)::sst ;
      (("INDENT",""),loc)
    end
    else (* n < m *) begin
      let (rev_pushback, new_sst) = pop_styles loc [] (st.style_stack, n) in
      st.pushback <- List.rev rev_pushback ;
      st.style_stack <- new_sst ;
      tokenize st lexbuf
    end

  | {pushback = []; bol = false ; style_stack = (BLOCK _)::_} ->
    match _valuetoken lexbuf with
      Some (EOI,loc) -> (("EOI",""),loc)
    | Some (NEWLINE,loc) ->
       st.bol <- true ;
       tokenize st lexbuf
    | Some (KEY k, loc) -> (("KEY", k), loc)
    | Some (DQKEY k, loc) -> (("DQKEY", k), loc)
    | None ->
      match _blockstring lexbuf with
        (BLOCKSTRING s,loc) -> (("BLOCKSTRING",s),loc)
      | (DQSTRING k, loc) -> (("DQSTRING", k), loc)

let make_token () =
  let st = St.mk() in
  fun lexbuf -> tokenize st lexbuf
}
