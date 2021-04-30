.PHONY: doc test

doc:
	cabal v2-haddock

test:
	cabal v2-test
