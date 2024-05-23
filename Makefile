all: simpelml.exe combined.sm
	dune exec lib/simpelml.exe exampels/test.sm

combined.sm: exampels/stdlib.sm exampels/test.sm
	cat exampels/stdlib.sm exampels/uatParticipant.sm > combined.sm

simpelml.exe:
	dune build lib/simpelml.exe

lexerTest:
	dune exec unit_tests/testModule.exe

clean:
	dune clean
	rm -f combined.sm

test_simpelml.exe:
	dune build unit_tests/simpelml.exe

parserTest:
	dune exec unit_tests/simpelml.exe exampels/astUnitTest.sm

interpTest:
	dune exec lib/simpelml.exe exampels/interpUnitTest.sm

.PHONY: all clean simpelml.exe lexerTest test_simpelml.exe parserTest interpTest combined.sm
