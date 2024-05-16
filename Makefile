all: simpelml.exe
	dune exec lib/simpelml.exe exampels/test.sm

simpelml.exe:
	dune build lib/simpelml.exe

clean:
	dune clean

.PHONY: all clean simpelml.exe