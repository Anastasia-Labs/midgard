
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
	@echo "  carriage-exec-ledger-v1 -- verify the §8.10 carriage execution ledger against a fresh aiken check"
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

# The `proof-v1-envelope` gate retired with the counted publication receipt
# chain it measured (#587). It ran the generated
# `proof-v1-fragment-envelope.test.ak` fixture and asserted a per-item chunk
# proof count plus an absent-fragment rejection — both statements about
# `bounded_collection_v1` openings, which `docs/spec/midgard-tx.md` §4's flat
# field hash makes unsatisfiable. The §8 replacement for what it guarded is the
# carriage exit measurement below, so a gate replaces a gate rather than a gate
# becoming a comment.
#
# `MIDGARD_AIKEN_BIN` selects the runner and defaults to `aiken` on PATH; the
# repo's measurements are taken with the patched fork (see
# `docs/spec/midgard-tx.md` §8.10).
.PHONY: carriage-exec-ledger-v1
carriage-exec-ledger-v1:
	cd onchain/aiken && node scripts/verify-carriage-exec-ledger-v1.mjs

.PHONY: validation-one-step-cross-language
validation-one-step-cross-language:
	cd demo/midgard-validation && pnpm run build
	cd demo/midgard-validation && node scripts/generate-validation-one-step-aiken-fixture.mjs
	cd onchain/aiken && aiken check -m 'midgard/validation_one_step_cross_language.{..}' -e
