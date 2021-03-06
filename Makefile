
OCAMLFIND=ocamlfind
PACKAGES=fmt,camlp5.extprint,camlp5.extend,camlp5.pprintf,pcre,yaml

all: yamlparser yamlparser.opt

yamlparser: yamllexer.cmo yamlparser.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -linkpkg -syntax camlp5r $^ -o $@

yamlparser.opt: yamllexer.cmx yamlparser.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) -package $(PACKAGES) -linkpkg -syntax camlp5r $^ -o $@

test:: all
	echo '1+1 ; 1 - 1; 1 + (2 * 3)' | ./yamlparser

.SUFFIXES: .mll .ml .cmo .cmx

yamllexer.cmo: yamllexer.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5o -c $<

yamllexer.cmx: yamllexer.ml
	$(OCAMLFIND) ocamlopt $(DEBUG) -package $(PACKAGES) -syntax camlp5o -c $<

yamlparser.cmo: yamlparser.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

yamlparser.cmx: yamlparser.ml
	$(OCAMLFIND) ocamlopt $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

.mll.ml:
	ocamllex $<

clean:
	rm -f yamlparser yamlparser.opt *.cm* *.o yamllexer.ml


depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5o yamllexer.ml > .depend.NEW || true
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r yamlparser.ml >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
