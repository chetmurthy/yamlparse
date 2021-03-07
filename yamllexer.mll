{

let locate ~comments lb v =
  let open Lexing in
  let spos = lexeme_start_p lb in
  let epos = lexeme_end lb in
  let loc = Ploc.make_loc "" spos.pos_lnum spos.pos_bol (spos.pos_cnum, epos) comments in
  (v, loc)

let locate_no_comments lb v = locate ~comments:"" lb v

module St = struct
type style =
    BLOCK of int
  | FLOW

type value_token =
    NEWLINE
  | KEY of string
  | DASH
  | DQSTRING of string
  | RAWSTRING of string
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

and _key_or_ident keys = parse
  | wschar* ":" wschar+ { locate_no_comments lexbuf (St.KEY keys) }
  | linechar* { locate_no_comments lexbuf (St.RAWSTRING (keys^(Lexing.lexeme lexbuf))) }

and _valuetoken = parse
  | newline { let rv = locate_no_comments lexbuf St.NEWLINE in Lexing.new_line lexbuf ; rv }
  | ident { _key_or_ident (Lexing.lexeme lexbuf) lexbuf }
  | dqstring { _key_or_ident (Lexing.lexeme lexbuf) lexbuf }
  | eof { locate_no_comments lexbuf St.EOI }

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
      (EOI,loc) -> (("EOI",""),loc)
    | (NEWLINE,loc) ->
       st.bol <- true ;
       tokenize st lexbuf

let make_token () =
  let st = St.mk() in
  fun lexbuf -> tokenize st lexbuf
}
