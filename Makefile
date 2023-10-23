play:
	OCAMLRUNPARAM=b dune exec run/run.exe

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

clean:
	dune clean
