all: SimpelML.exe
	dune exec lib/simpelml.exe exampels/test.sm

SimpelML.exe:
	dune build

clean:
	dune clean

.PHONY:
	all clean lib/simpelml.exe
