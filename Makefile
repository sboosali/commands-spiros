PACKAGE = commands-spiros
VERSION = 0.0.0

HC = cabal exec -- ghc

CODE = config # tests


# # # # # # # # # # # # # # # # # # 

default: check

all: build test run document style

configure:
	cabal configure --enable-tests

build: configure
	cabal build

run:
	cabal run

serve:
	echo 'curl  -X POST  -H "Content-Type: application/json"  -d '"'"'["par","round","grave","camel","lit","async","break","break","action"]'"'"' "http://localhost:1337/recognition/"'
	cabal run serve

bench:
	cabal configure --enable-benchmarks && cabal bench

test:
# cabal install ../commands-core --force-reinstall --enable-tests
# cabal exec -- ghc-pkg unregister commands-core-0.0.0
	cabal configure --enable-tests
	cabal test && echo && cat dist/test/*-tests.log


document:
	cabal haddock
	open dist/doc/html/$(PACKAGE)/index.html

style:
	hlint --hint=HLint.hs  *.hs $(CODE)

fix:
	git vogue fix --all

check:
#	cabal build --ghc-options="-fforce-recomp -fno-code"
	cabal build --ghc-options="-fno-code"


# # # # # # # # # # # # # # # # # # 

default: Haskell

clean:
	rm -f Main *.{o,hi,dyn_o,dyn_hi}

fresh: clean
	rm -fr dist

.PHONY: default clean fresh all build test document style check bench
