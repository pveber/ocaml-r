INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: build install uninstall clean doc all

build:
	jbuilder build @install

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

utop:
	jbuilder utop lib

clean:
	jbuilder clean

doc:
	jbuilder build @doc

all: build doc
