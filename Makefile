all: SimpelML.exe
	dune exec bin/main.exe exampels/test.sm

SimpelML.exe:
	dune build bin/main.exe

clean:
	dune clean

.PHONY:
	all clean bin/main.exe
