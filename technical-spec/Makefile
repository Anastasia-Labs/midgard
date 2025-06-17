
.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help      -- show this help"
	@echo "  shell     -- open nix dev shell"
	@echo "  spec      -- build the specification midgard.pdf"
	@echo "  build     -- alias for 'make spec'"
	@echo "  clean     -- clean up latexmk files"
	@echo "  nix-build -- open nix shell & build"
	@echo "  nix-clean -- open nix shell & clean"

.PHONY: shell
shell:
	nix develop

.PHONY: spec
spec:
	latexmk -pdf -verbose -file-line-error -synctex=1 -interaction=nonstopmode

.PHONY: build
build: spec

.PHONY: clean
clean:
	latexmk -C
	find . -type f -name "*.aux" -exec rm {} \;
	rm gitHeadLocal.gin

.PHONY: nix-build
nix-build:
	nix develop --command $(MAKE) build

.PHONY: nix-clean
nix-clean:
	nix develop --command $(MAKE) clean
