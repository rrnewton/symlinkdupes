
TARGET=symlinkdupes

bc:
	ocamlc unix.cma dirtree.cma rutils.cma symlinkdupes.ml -o $(TARGET)

nc:
	ocamlopt unix.cmxa dirtree.cmxa rutils.cmxa  symlinkdupes.ml -o $(TARGET)

#install:
#	cp a.out 

install:
	dirname `which ocamlc` | cp $(TARGET) -

clean: cleanup
	rm -f *.cmxa *.cma *.a  debug $(TARGET)
