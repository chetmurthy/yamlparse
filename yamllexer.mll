{

let locate ~comments lb v =
  let loc = Ploc.make_unlined (Lexing.lexeme_start lb, Lexing.lexeme_end lb) in
  let loc = Ploc.with_comment loc comments in
  (v, loc)

module St = struct
type t = {
  pushback : (string * string) list ref
  ; indent_stack : int list ref
  ; mutable indent : int
  ; mutable set_indent : bool
  ; mutable bol : bool
}

let mk () = {
  pushback = ref []
; indent_stack = ref []
; set_indent = false
; bol = true
; indent = 0
}
end

}

let linews = [' ' '\t']+
let newline = '\r'? '\n'
let indent = ' '+
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

rule _valuetoken comments = parse
| comment { _valuetoken (comments^(Lexing.lexeme lexbuf)) lexbuf }
| "(" { locate ~comments lexbuf ("","(") }
| ")" { locate ~comments lexbuf ("",")") }
| "[" { locate ~comments lexbuf ("","[") }
| "]" { locate ~comments lexbuf ("","]") }
| "{" { locate ~comments lexbuf ("","{") }
| "}" { locate ~comments lexbuf ("","}") }
| "|" { locate ~comments lexbuf ("","|") }
| ":" { locate ~comments lexbuf ("",":") }
| "," { locate ~comments lexbuf ("",",") }
| "---" { locate ~comments lexbuf ("","---") }
| "..." { locate ~comments lexbuf ("","...") }
| "-" { locate ~comments lexbuf ("","-") }
| "~" { locate ~comments lexbuf ("","~") }
| decimal as dec { locate ~comments lexbuf ("INT",dec) }
| float as f { locate ~comments lexbuf ("REAL",f) }
| ident as id { locate ~comments lexbuf ("IDENT",id) }
| dqstring as qs { locate ~comments lexbuf ("DQSTRING",qs) }
| eof { locate ~comments lexbuf ("EOI","") }

and _identcolon st comments id = parse
| linews? ":" linews { locate ~comments lexbuf ("IDENTCOLONSPACE", id) }
| "" { locate ~comments lexbuf ("IDENT", id) }

and _rawtoken st comments = parse
| ("---"|"..."|"- ") as tok { locate ~comments lexbuf ("",tok) }
| ident as id { _identcolon st comments id lexbuf }

and _indent st = parse
| indent? as indent { String.length indent }

{

let _token st lexbuf =
  let open St in
  if st.bol then begin
    st.bol <- false ;
    let ind = _ident st lexbuf in
    

(*
let make_token () =
  let st = St.mk () in
  fun lb -> _token st lb
*)
}
