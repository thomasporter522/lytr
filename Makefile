HTML_DIR=_build/default/src/web/www
HTML_FILE=$(HTML_DIR)/index.html

.PHONY: all deps release clean open watch test test-watch

all:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

deps:
	opam update
	opam switch import opam.export

change-deps:
	opam switch export opam.export

release:
	dune build src --profile release

echo-html:
	@echo "$(shell pwd)/_build/default/src/web/www/index.html"

clean:
	dune clean

open:
	open "$(HTML_FILE)"

watch:
	dune build @src/fmt --auto-promote src --profile dev --watch

test:
	dune build @fmt --auto-promote || true
	dune runtest --force

test-watch:
	dune build @fmt --auto-promote || true
	dune runtest --watch
