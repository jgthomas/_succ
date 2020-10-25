
PROJECT=succ

LINTER=hlint
FORMATTER=ormolu
DEAD_CODE=weeder
GRAPHS=graphmod
IDE=ghcid


.PHONY: test lint clean coverage graph weed ide setup


all: test lint weed

build:
	stack build --pedantic

test: unit func

unit:
	stack build --pedantic --test

func:
	./func_test.sh

lint:
	stack exec -- ${LINTER} src/ app/

coverage: clean
	stack test --coverage && stack hpc report ${PROJECT} --open

clean:
	stack clean

docs:
	stack haddock --no-haddock-deps ${PROJECT}

graph:
	find src -name '*.hs' | xargs stack exec -- ${GRAPHS} -q | xdot -

weed:
	stack exec ${DEAD_CODE}

ide:
	stack exec ${IDE}

setup:
	stack build --copy-compiler-tool ${LINTER} ${FORMATTER} ${DEAD_CODE} ${GRAPHS} ${IDE}
