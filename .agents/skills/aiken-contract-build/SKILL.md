---
name: aiken-contract-build
description: Use when compiling, rebuilding, or debugging Midgard Aiken contracts under onchain/aiken, especially when verbose traces are needed for Plutus/Aiken script failures or emulator diagnostics.
---

# Aiken Contract Build

## Build Commands

Run Aiken commands from the contract project directory:

```bash
cd onchain/aiken
```

Midgard demo, emulator, Preprod, and e2e contract builds use the Aiken
`testnet` environment. Select it explicitly so environment-dependent protocol
parameters in `plutus.json` match the off-chain testnet configuration.

For a traced diagnostic blueprint, use:

```bash
aiken build --env testnet --trace-level verbose --trace-filter all
```

For a normal blueprint without traces, use:

```bash
aiken build --env testnet
```

## Focused Checks

Use the fork repository and revision pinned in
`.github/workflows/aiken-ci.yml`; `aiken.toml` records the base version, which
alone cannot identify the patched compiler. Record the actual local
`aiken --version` and match the CI pin before producing release evidence.
`onchain/aiken/scripts/pinned-compiler.mjs` asserts it before the repository
scripts and the pre-commit hook run `aiken`. Blind spot: a raw `aiken` command
typed by hand is never checked, and `aikup` can silently repoint `aiken` at a
stock release. [script: onchain/aiken/scripts/pinned-compiler.mjs]

Run large Midgard vectors one compiler process at a time. Use the repository
guard for one exact test:

```bash
node scripts/run-focused-check.mjs \
  midgard/validation_machine/machine_types \
  exact_test_name
```

When several exact tests in the same module are required, pass every unique
name to one guard invocation rather than recompiling the tree for each.
[review]

```bash
node scripts/run-focused-check.mjs \
  midgard/validation_machine/machine_types \
  first_exact_test_name \
  second_exact_test_name
```

The guard constructs one module-qualified exact selector per name and fails
unless exactly that many tests are collected and all pass. Do not combine a
bare test name with `aiken check -e`: Aiken can collect zero tests and still
exit successfully. Blind spot: the guard protects only runs made through it; a
raw `aiken check -m` that collects zero tests still exits 0.
[script: onchain/aiken/scripts/run-focused-check.mjs]
For a dotted test filename, pass the full source module without `.ak`; for
example, tests in `cek-data-traverse.max-cardano.test.ak` use
`midgard/cek-data-traverse.max-cardano.test` plus the exact test name. The guard
constructs the shortened Aiken selector internally and checks the full reported
module identity. Use the prefix before the first dot only for a direct
`aiken check -m` selector, where it can also select sibling test modules.
When invoking Aiken directly, use:

```bash
aiken check \
  -m 'midgard/validation_machine/machine_types.{exact_test_name}' \
  -e --plain-numbers
```

To run **every** test in one module, the selector still needs the brace form,
with `..` in place of a name:

```bash
aiken check -m 'midgard/native_tx_field_access_v1.{..}'
```

A bare module selector — `-m midgard/native_tx_field_access_v1`, with no
`.{...}` — matches **zero test scenarios**. Aiken prints
`Summary 0 errors` and exits 0, which reads exactly like a green module. The
patched fork emits a `Suspicious test filter (-m) yielding no test scenarios`
warning first, but the exit code is still 0, so a gate that checks only the
status passes while running nothing. Any evidence gathered with the bare form
is void; re-gather it with the brace form.

`onchain/aiken/scripts/guard-focused-selector.mjs` takes bare module selectors on purpose and
is safe with them, because it fails closed on a zero collected total — a
stronger check than the selector shape. Prefer it, or
`scripts/run-focused-check.mjs`, over hand-rolled `aiken check -m` lines.

Evidence must report a nonzero collected total, not only the process exit code.
[review]

## Repo Workflow

- Treat `onchain/aiken/plutus.json` as the generated blueprint output unless a task explicitly chooses another `--out` path.
- When a node or emulator test must use the freshly built real contracts, set `MIDGARD_REAL_BLUEPRINT_PATH` to the absolute `onchain/aiken/plutus.json` path. [review]
- If tracing is needed for a script failure, rebuild with `--trace-level verbose --trace-filter all` before rerunning the failing emulator test.
- Preserve production correctness: do not switch transaction completion to `.complete({ localUPLCEval: false })` to bypass failures. Blind spot: that scan covers only `demo/midgard-fault-proofs`. [script: demo/midgard-fault-proofs/tests/wave0-shared-substrate.test.ts]

## Disposable Final-Tree Builds

When building from an isolated copy of `onchain/aiken`, never carry the
source checkout's ignored `build/` cache or generated `plutus.json` into the
copy. Aiken can otherwise reuse stale cached schema metadata and emit a
blueprint that does not describe the copied source, even though compilation
exits successfully. [review]

Before invoking Aiken, resolve and validate the disposable destination, then
ensure only that destination has no `build/` directory or `plutus.json`.
Never clean the shared repository checkout to prepare an isolated build.
After the build, verify a consequential schema/count/hash from the generated
blueprint against current source before using it as test or release evidence;
an exit code alone is insufficient. [review]
