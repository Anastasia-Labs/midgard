.PHONY: check build differential

check:
	cargo fmt --check
	cargo test --release --locked

build:
	wasm-pack build --locked --target nodejs --release --out-dir ../../.architecture-f-wasm

differential: build
	node ../../scripts/mpf-event-flat-wasm-prototype.mjs
