.PHONY:	test

play: build
	OCAMLRUNPARAM=b dune exec run/run.exe

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

build: clean
	dune build

clean: bisect-clean
	dune clean

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

opendoc:
	dune build @doc
	
doc: opendoc
	@bash opendoc.sh	