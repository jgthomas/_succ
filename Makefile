
.PHONY: test lint

build:
	stack build --pedantic


test:
	stack test && ./func_test.sh


lint:
	hlint src/ app/
