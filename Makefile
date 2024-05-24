all: simpleml.exe combined.sm
	dune exec lib/simpelml.exe combined.sm

combined.sm: examples/stdlib.sm examples/test.sm
	cat examples/stdlib.sm examples/test.sm > combined.sm

simpleml.exe:
	dune build lib/simpleml.exe

lexerTest:
	dune exec unit_tests/testModule.exe

clean:
	dune clean
	rm -f combined.sm

test_simpleml.exe:
	dune build unit_tests/simpleml.exe

parserTest:
	dune exec unit_tests/simpleml.exe examples/astUnitTest.sm

interpTest:
	dune exec lib/simpleml.exe examples/interpUnitTest.sm

.PHONY: all clean simpleml.exe lexerTest test_simplel.exe parserTest interpTest combined.sm
