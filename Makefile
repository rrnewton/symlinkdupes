

bc:
	ocamlc unix.cma dirtree.cma rutils.cma symlinkdupes.ml

nc:
	ocamlopt unix.cmxa dirtree.cmxa rutils.cmxa  symlinkdupes.ml

