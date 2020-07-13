all: tests absbook evaluation expr miniml 

tests: tests.ml
	ocamlbuild -use-ocamlfind tests.byte

absbook: absbook.ml
	ocamlbuild -use-ocamlfind absbook.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

clean:
	rm -rf _build *.byte
