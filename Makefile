all: SimpelML.exe
	dune exec lib/simpelml.exe exampels/test.sm

SimpelML.exe:
	dune build lib/simpelml.exe

clean:
	dune clean

.PHONY:
	all clean lib/simpelml.exe
