test:
	dune exec test/main.exe

clean:
	dune clean

play:
	OCAMLRUNPARAM= dune exec run/run.exe