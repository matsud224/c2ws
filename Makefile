OCAML = ocamlc
OCAMLYACC = ocamlyacc 
OCAMLLEX = ocamllex

c2ws:	syntax.ml myparser.mli myparser.ml mylexer.ml main.ml
	$(OCAML) syntax.ml myparser.mli myparser.ml mylexer.ml main.ml -o c2ws
myparser.mli:	myparser.mly
	$(OCAMLYACC) myparser.mly
myparser.ml:	myparser.mly
	$(OCAMLYACC) myparser.mly
mylexer.ml:	mylexer.mll
	$(OCAMLLEX) mylexer.mll

top:	syntax.ml myparser.mli myparser.ml mylexer.ml
	ocamlmktop syntax.ml myparser.mli myparser.ml mylexer.ml -o myc.top

clean:
	rm -f c2ws
