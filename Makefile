INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: build install uninstall clean doc all publish-doc

build:
	dune build @install

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

utop:
	dune utop lib

clean:
	dune clean

doc:
	dune build @doc

all: build doc

publish-doc:
	(cd _build && rm -rf gh-pages && \
         git clone -b gh-pages --single-branch git@github.com:pveber/ocaml-r.git gh-pages)
	rsync -a --delete --exclude=.git _build/default/_doc/_html/ _build/gh-pages/
	(cd _build/gh-pages && git add * && git commit -m "$(shell date)" && git push origin gh-pages)
