.PHONY: check build

check:
	cargo fmt --check
	cargo test --release --locked

build:
	wasm-pack build --locked --target nodejs --release --out-dir ../../.architecture-f-wasm
