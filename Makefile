
.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help               -- show this help"
	@echo "  enable-git-hooks   -- enable git hooks in this local repo clone"
	@echo "  disable-git-hooks  -- enable git hooks in this local repo clone"
	@echo "  enable-graphify-post-commit -- install the Graphify refresh hook without replacing other hooks"
	@echo "  refresh-graphify-graph -- refresh the external Graphify graph from a coherent checkout"
	@echo "  spec               -- build the specification technical-spec/midgard.pdf"
	@echo "  spec-clean         -- clean the latexmk files in technical-spec"
	@echo "  validation-one-step-cross-language -- verify TS-generated one-step evidence on Aiken"

.PHONY: enable-git-hooks
enable-git-hooks:
	git config core.hooksPath .githooks

.PHONY: disable-git-hooks
disable-git-hooks:
	git config --unset core.hooksPath

.PHONY: enable-graphify-post-commit
enable-graphify-post-commit:
	mkdir -p "$$(git rev-parse --git-path hooks)"
	ln -sfn "$$(pwd)/.githooks/post-commit" "$$(git rev-parse --git-path hooks)/post-commit"

.PHONY: refresh-graphify-graph
refresh-graphify-graph:
	.githooks/post-commit --foreground

.PHONY: spec
spec:
	$(MAKE) -C technical-spec nix-build

.PHONY: spec-clean
spec-clean:
	$(MAKE) -C technical-spec nix-clean

.PHONY: validation-one-step-cross-language
validation-one-step-cross-language:
	cd demo/midgard-validation && pnpm run build
	cd demo/midgard-validation && node scripts/generate-validation-one-step-aiken-fixture.mjs
	cd onchain/aiken && node scripts/guard-focused-selector.mjs midgard/validation_one_step_cross_language
