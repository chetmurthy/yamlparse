(* camlp5r *)
(* yamlparser.ml,v *)

value input_file = ref "" ;

value lexer = Plexing.lexer_func_of_ocamllex_located Yamllexer.token ;
value lexer = {Plexing.tok_func = lexer;
 Plexing.tok_using _ = (); Plexing.tok_removing _ = ();
 Plexing.tok_match = Plexing.default_match;
 Plexing.tok_text = Plexing.lexer_text;
 Plexing.tok_comm = None} ;

open Yaml ;

value g = Grammar.gcreate lexer;
value yamlval = Grammar.Entry.create g "yamlval";
value yamlval_eoi = Grammar.Entry.create g "yamlval_eoi";

EXTEND
  GLOBAL: yamlval yamlval_eoi ;

  yamlval : [ [
      
    ] ]
  ;

  yamlval_eoi : [ [ l = yamlval ; EOI -> l ] ] ;
END;

value parse_yamlval = Grammar.Entry.parse yamlval ;
value parse_yamlval_eoi = Grammar.Entry.parse yamlval_eoi ;
