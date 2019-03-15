lec5q:
	ocamlbuild -use-ocamlfind -package qcheck,ppx_deriving.show src/lecture5/exercises5queue.byte


clean:
	ocamlbuild -clean