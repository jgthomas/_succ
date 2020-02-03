
PROJECT=succ

.PHONY: test lint clean coverage graph


all: test lint


build:
	stack build --pedantic


test: unit func


unit:
	stack build --pedantic --test


func:
	./func_test.sh


lint:
	hlint src/ app/



coverage: clean
	stack test --coverage


clean:
	stack clean


docs:
	stack haddock --no-haddock-deps ${PROJECT}


graph:
	find src -name '*.hs' | xargs graphmod -q | xdot -
