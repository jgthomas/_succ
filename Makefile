
PROJECT=succ

.PHONY: test lint clean coverage graph


all: test lint weed

build:
	stack build --pedantic

test: unit func

unit:
	stack build --pedantic --test

func:
	./func_test.sh

lint:
	stack exec -- hlint src/ app/

coverage: clean
	stack test --coverage && stack hpc report ${PROJECT} --open

clean:
	stack clean

docs:
	stack haddock --no-haddock-deps ${PROJECT}

graph:
	find src -name '*.hs' | xargs graphmod -q | xdot -

weed:
	stack exec weeder

ide:
	stack exec ghcid
