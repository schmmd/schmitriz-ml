OPTIONS=graphics.cma

all:
	ocamlc -c defs.ml
	ocamlc -c shape.mli shape.ml
	ocamlc -c visual.mli visual.ml
	ocamlc -thread -c game.ml
	ocamlc -thread -c main.ml
	ocamlc -thread unix.cma threads.cma -o schmitriz.out graphics.cma defs.cmo shape.cmo visual.cmo game.cmo main.cmo
#	ocamlopt -I +threads graphics.cmxa unix.cmxa threads.cmxa -o schmitriz defs.ml shape.ml visual.ml game.ml main.ml

clean:
	-rm *.cmo *.cmi *.cmx *.o
