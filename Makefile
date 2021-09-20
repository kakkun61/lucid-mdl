PWSH = pwsh

.PHONY: build
build:
	cabal v2-build lucid-mdl:lib:lucid-mdl

.PHONY: build-deps
build-deps:
	cabal v2-build lucid-mdl:lib:lucid-mdl --only-dependencies

.PHONY: test
test: doctest spec

.PHONY: doctest
doctest:
	cabal v2-test doctest

.PHONY: spec
spec:
	cabal v2-test spec

.PHONY: repl
repl:
	cabal v2-repl

.PHONY: example
example:
	cabal build lucid-mdl:exe:example

.PHONY: format
format:
	$(PWSH) -Command "& { Get-ChildItem -Filter '*.hs' -Recurse src, example, test | ForEach-Object { stylish-haskell -i $$_.FullName } }"

.PHONY: lint
lint:
	hlint src example

.PHONY: doc
doc:
	cabal v2-haddock

.PHONY: clean
clean:
	cabal v2-clean

.PHONY: targets
targets:
	$(PWSH) -Command "& { Get-Content .\Makefile | Where-Object { $$_ -like '.PHONY*' } | ForEach-Object { $$_.Substring(8) } }"
