.DEFAULT: markov.native
markov.native:
	ocamlbuild -use-ocamlfind -tags 'package(extlib,yojson,curl,pcre)' $@
