
PROJECT=succ

.PHONY: test lint


all: build test lint


build:
	stack build --pedantic


test:
	stack test && ./func_test.sh


lint:
	hlint src/ app/


docs:
	stack haddock --no-haddock-deps ${PROJECT}
