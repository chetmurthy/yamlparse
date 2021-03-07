
OCAMLFIND=ocamlfind
NOT_OCAMLFIND=not-ocamlfind
PACKAGES=fmt,camlp5.extprint,camlp5.extend,camlp5.pprintf,pcre,yaml

all: lextest #yamlparser yamlparser.opt

lextest: yamllexer.cmo lextest.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

yamlparser: yamllexer.cmo yamlparser.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -linkpkg -linkall -syntax camlp5r $^ -o $@

yamlparser.opt: yamllexer.cmx yamlparser.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) -package $(PACKAGES) -linkpkg -linkall -syntax camlp5r $^ -o $@

test:: all
	mkdir -p _build
	./lextest
#	echo '1+1 ; 1 - 1; 1 + (2 * 3)' | ./yamlparser

.SUFFIXES: .mll .ml .cmo .cmx

foo.ppo: foo.ml
	$(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.pr_o -syntax camlp5o $< > $@

yamllexer.cmo: yamllexer.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5o -c $<

yamllexer.cmx: yamllexer.ml
	$(OCAMLFIND) ocamlopt $(DEBUG) -package $(PACKAGES) -syntax camlp5o -c $<

lextest.cmo: lextest.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -syntax camlp5o -c $<

yamlparser.cmo: yamlparser.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

yamlparser.cmx: yamlparser.ml
	$(OCAMLFIND) ocamlopt $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

.mll.ml:
	ocamllex $<
#	perl -p -i -e 's,#.*,,' $@

clean:
	rm -f lextest yamlparser yamlparser.opt *.cm* *.o yamllexer.ml _build *.log *.cache


depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5o yamllexer.ml lextest.ml > .depend.NEW || true
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r yamlparser.ml >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
