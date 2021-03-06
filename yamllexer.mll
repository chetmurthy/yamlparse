{

let locate ~comments lb v =
  let loc = Ploc.make_unlined (Lexing.lexeme_start lb, Lexing.lexeme_end lb) in
  let loc = Ploc.with_comment loc comments in
  (v, loc)

let locate_no_comments lb v =
  let loc = Ploc.make_unlined (Lexing.lexeme_start lb, Lexing.lexeme_end lb) in
  (v, loc)

module St = struct
type style =
    BLOCK of int
  | FLOW

type token = (string * string) * Ploc.t

type t = {
  pushback : token list ref
; style_stack : style list ref
}

let mk () = {
  pushback = ref []
; style_stack = ref [BLOCK 0]
}
end

}

let indent = ' '+
let wschar = [' ' '\t']
let linews = [' ' '\t']+
let newline = '\r'? '\n'
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

rule tok1 = parse
  | "true" { true }
  | "false" { false }

and tok2 = parse
  | "1" { 1 }
  | "0" { 0 }
  
and indent = parse
  | indent ? { locate_no_comments lexbuf (String.length (Lexing.lexeme lexbuf)) }

{

let x = 1

}
