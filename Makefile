.PHONY:	test

play:
	OCAMLRUNPARAM=b dune exec run/run.exe

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

clean:
	bisect-clean
	dune clean

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

doc:
	dune build @doc
	
opendoc: doc
	@bash opendoc.sh	