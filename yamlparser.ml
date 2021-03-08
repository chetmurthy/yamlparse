(* camlp5r *)
(* yamlparser.ml,v *)

value input_file = ref "" ;

value lexer = Plexing.lexer_func_of_ocamllex_located (Yamllexer.make_token()) ;
value lexer = {Plexing.tok_func = lexer;
 Plexing.tok_using _ = (); Plexing.tok_removing _ = ();
 Plexing.tok_match = Plexing.default_match;
 Plexing.tok_text = Plexing.lexer_text;
 Plexing.tok_comm = None} ;

open Yaml ;

value g = Grammar.gcreate lexer;
value (yamlval : Grammar.Entry.e Yaml.\value) = Grammar.Entry.create g "yamlval";
value yamlval_eoi = Grammar.Entry.create g "yamlval_eoi";

(*
    type value =
        [ `A of value list
        | `Bool of bool
        | `Float of float
        | `Null
        | `O of (string * value) list
        | `String of string ]
*)

EXTEND
  GLOBAL: yamlval yamlval_eoi ;

  yamlval : [ [
      s = DQSTRING -> `String s
    | s = BLOCKSTRING -> `String s
    | c = composite -> c
    | INDENT ; c = composite ; DEDENT -> c
    ] ]
  ;

  composite : [ [
      k = [ k = KEY -> k | k = DQKEY -> k ] ; v = yamlval ; l = blockdict -> `O [(k, v) :: l]
    | DASH ; v0 = yamlval ; l=blocklist -> `A [v0 :: l]
    ] ]
  ;

  blockdict : [ [
      l = LIST0 [ k = [ k = KEY -> k | k = DQKEY -> k ] ; v = yamlval -> (k,v) ] -> l
    ] ]
  ;

  blocklist : [ [
      l = LIST0 [ DASH ; v0 = yamlval -> v0 ] -> l
    ] ]
  ;

  yamlval_eoi : [ [ l = yamlval ; EOI -> l ] ] ;
END;

value parse_yamlval = Grammar.Entry.parse yamlval ;
value parse_yamlval_eoi = Grammar.Entry.parse yamlval_eoi ;
