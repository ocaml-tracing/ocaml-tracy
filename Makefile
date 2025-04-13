
all:
	@dune build @install

clean:
	@dune clean

WATCH?= @install @runtest
watch:
	@dune build $(WATCH) -w

format:
	@dune build @fmt --auto-promote

update-submodules:
	@git submodule update --init

VERSION?=
dune-release-distrib:
	@[ -n "$(VERSION)" ] || (echo "make sure to pass VERSION " && exit 1)
	@echo "using version $(VERSION)"
	@git tag -d v$(VERSION) || true
	dune-release tag -f v$(VERSION)
	dune-release distrib -p tracy-client --include-submodules -t v$(VERSION) -V $(VERSION)
	echo "distrib done, publishing"
	dune-release publish -p tracy-client -t v$(VERSION) -V $(VERSION)
	echo "publishing done"

dune-release-opam-publish:
	@[ -n "$(VERSION)" ] || (echo "make sure to pass VERSION " && exit 1)
	@echo "make sure you ran `make dune-release-distrib` first"
	dune-release opam pkg -t v$(VERSION) -V $(VERSION)
	dune-release opam submit -t v$(VERSION) -V $(VERSION)

.PHONY: clean watch dune-release-distrib dune-release-opam-publish
