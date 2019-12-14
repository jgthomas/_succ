
PROJECT=succ

.PHONY: test lint


all: build test lint


build:
	stack build --pedantic


test: unit func


unit:
	stack test


func:
	./func_test.sh


lint:
	hlint src/ app/


docs:
	stack haddock --no-haddock-deps ${PROJECT}
