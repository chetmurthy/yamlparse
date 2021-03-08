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

type token = (string * string) * Ploc.t

module St = struct
type style =
    BLOCK of int
  | FLOW

type value_token =
    NEWLINE
  | KEY of string
  | KEYVALUE of string * string
  | DASH
  | DQSTRING of string
  | BLOCKSTRING of string
  | EOI

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
   )* '"'

rule _indent = parse
  | indent ? { locate_no_comments lexbuf () }

and _valuetoken st = parse
  | newline {
      let rv = locate_no_comments lexbuf St.NEWLINE in
      st.St.bol <- true ;
      Lexing.new_line lexbuf ;
      Some rv }
  | ((ident|dqstring) as keys) wschar* ":" wschar+
    (((dqstring as vals) wschar* )
    | ((linechar # '"') linechar* as vals)) (newline|eof)
      { let rv = Some (locate_no_comments lexbuf (St.KEYVALUE (keys, vals))) in
        st.St.bol <- true ;
        Lexing.new_line lexbuf ;
        rv
      }
  | ((ident|dqstring) as keys) wschar* ":" wschar* (newline|eof)
      { let rv = Some (locate_no_comments lexbuf (St.KEY keys)) in
        st.St.bol <- true ;
        Lexing.new_line lexbuf ;
        rv
      }

  | "- " { Some (locate_no_comments lexbuf St.DASH) }
  | "-" (newline|eof) {
      let rv = Some (locate_no_comments lexbuf St.DASH) in
      st.St.bol <- true ;
      Lexing.new_line lexbuf ;
      rv
    }
  | eof { Some (locate_no_comments lexbuf St.EOI) }
  | "" { None }

and _blockstring = parse
  | dqstring { locate_no_comments lexbuf (St.DQSTRING (Lexing.lexeme lexbuf)) }
  | (linechar # '"') linechar* { locate_no_comments lexbuf (St.BLOCKSTRING (Lexing.lexeme lexbuf)) }

{
open St
let rec pop_styles loc (rev_pushback : token list) = function
    ((BLOCK m)::sst, n) when n < m -> pop_styles loc ((("DEDENT",""),loc)::rev_pushback) (sst, n)
  | ((BLOCK m)::sst, n) when n = m && m > 0 -> ((("DEDENT",""),loc)::rev_pushback, sst)
  | ((BLOCK m)::sst, n) when n = m && m = 0 ->
    assert (sst = []) ;
    (rev_pushback, [BLOCK 0])
  | _ -> failwith "pop_styles: dedent did not move back to previous indent position"

let extract_indent_position = function
    (("EOI",""), _) -> 0
  | (_, loc) ->
    Ploc.first_pos loc - Ploc.bol_pos loc

let handle_indents_with st toks =
  assert (st.pushback = []) ;
  assert (toks <> []) ;
  match st.style_stack with
    (BLOCK m)::_ ->
    let (_, loc as tok) = List.hd toks in
    let n = extract_indent_position tok in
    if n = m then begin
      st.pushback <- List.tl toks ;
      List.hd toks
    end
    else if n > m then begin
      st.style_stack <- (BLOCK n)::st.style_stack ;
      st.pushback <- toks ;
      (("INDENT",""),loc)
    end
    else (* n < m *) begin
      let (rev_pushback, new_sst) = pop_styles loc [] (st.style_stack, n) in
      let new_pushback = (List.rev rev_pushback)@toks in
      st.pushback <- List.tl new_pushback ;
      st.style_stack <- new_sst ;
      List.hd new_pushback
    end  

let rec tokenize st lexbuf =
  match st with
    {pushback = h :: t} ->
    st.pushback <- t ;
    h

  | {pushback = [] ; bol = true; style_stack = ((BLOCK m)::_)} ->
    _indent lexbuf ;
    st.bol <- false ;
    tokenize st lexbuf

  | {pushback = []; bol = false ; style_stack = (BLOCK _)::_} ->
    match _valuetoken st lexbuf with
      Some (EOI,loc) ->
      handle_indents_with st [(("EOI",""),loc)]
      
    | Some (NEWLINE,loc) ->
       tokenize st lexbuf

    | Some (KEYVALUE (k,v), loc) ->
      let valtok =
        if String.get v 0 = '"' then
          (("DQSTRING",v),loc)
        else (("RAWSTRING",v),loc) in
      let keytok =
        if String.get k 0 = '"' then
          (("DQKEY", k), loc)
        else
          (("KEY", k), loc) in
      handle_indents_with st [keytok;valtok]

    | Some (KEY k, loc) ->
      let keytok =
        if String.get k 0 = '"' then
          (("DQKEY", k), loc)
        else
          (("KEY", k), loc) in
      handle_indents_with st [keytok]

    | Some (DASH, loc) ->
      handle_indents_with st [(("DASH",""),loc)]

    | None ->
      match _blockstring lexbuf with
        (BLOCKSTRING s,loc) -> (("BLOCKSTRING",s),loc)
      | (DQSTRING k, loc) -> (("DQSTRING", k), loc)

let make_token () =
  let st = St.mk() in
  fun lexbuf -> tokenize st lexbuf
}
