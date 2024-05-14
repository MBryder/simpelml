all: simpelml.exe
	dune exec lib/simpelml.exe exampels/test.sm

simpelml.exe:
	dune build lib/simpelml.exe

lexerTest:
	dune exec unit_tests/testModule.exe

clean:
	dune clean

test_simpelml.exe:
	dune build unit_tests/simpelml.exe

parserTest:
	dune exec unit_tests/simpelml.exe exampels/astUnitTest.sm

interpTest:
	dune exec lib/simpelml.exe exampels/interpUnitTest.sm

.PHONY: all clean simpelml.exe lexerTest test_simpelml.exe parserTest interpTest