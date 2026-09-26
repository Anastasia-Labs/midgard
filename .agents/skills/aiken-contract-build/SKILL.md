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

Run large Midgard vectors one compiler process at a time. Use the repository
guard for one exact test:

```bash
node scripts/run-focused-check.mjs \
  midgard/validation_machine/machine_types \
  exact_test_name
```

When several exact tests in the same module are required, pass every unique
name to one guard invocation rather than recompiling the tree for each:

```bash
node scripts/run-focused-check.mjs \
  midgard/validation_machine/machine_types \
  first_exact_test_name \
  second_exact_test_name
```

The guard constructs one module-qualified exact selector per name and fails
unless exactly that many tests are collected and all pass. Do not combine a
bare test name with `aiken check -e`: Aiken can collect zero tests and still
exit successfully.
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

To run every test in the modules whose name **contains** a string, use the
brace form with `..`, or the same text with no braces; on the pinned fork both
collect the same set (as of 2026-09-26, `aiken v1.1.23+5adf783`):

```bash
aiken check -m 'midgard/native_tx_field_access_v1.{..}'
```

The module part is a substring match, so this also runs
`midgard/native_tx_field_access_v1.test` and
`midgard/native_tx_field_access_v1_golden.test`. What collects **zero** tests
and still exits 0 is a selector with no `/` and no `.` (`-m name_v1` is read
as a test-name filter, not a module) and a dotted module name (the text after
the first `.` becomes a test-name filter). The fork's
`Suspicious test filter (-m) yielding no test scenarios` warning fires only
for a single test-name filter, and did not print at all with output piped. A
gate that checks only the exit status passes while running nothing. Full
selector rules: [references/cli-traps.md](references/cli-traps.md).

`scripts/guard-focused-selector.mjs` takes bare module selectors on purpose and
is safe with them, because it fails closed on a zero collected total — a
stronger check than the selector shape. Prefer it, or
`scripts/run-focused-check.mjs`, over hand-rolled `aiken check -m` lines.

Evidence must report a nonzero collected total, not only the process exit code.

## Repo Workflow

- Treat `onchain/aiken/plutus.json` as the generated blueprint output unless a task explicitly chooses another `--out` path.
- When a node or emulator test must use the freshly built real contracts, set `MIDGARD_REAL_BLUEPRINT_PATH` to the absolute `onchain/aiken/plutus.json` path.
- If tracing is needed for a script failure, rebuild with `--trace-level verbose --trace-filter all` before rerunning the failing emulator test.
- Preserve production correctness: do not switch transaction completion to `.complete({ localUPLCEval: false })` to bypass failures.

## Disposable Final-Tree Builds

When building from an isolated copy of `onchain/aiken`, never carry the
source checkout's ignored `build/` cache or generated `plutus.json` into the
copy. Aiken can otherwise reuse stale cached schema metadata and emit a
blueprint that does not describe the copied source, even though compilation
exits successfully.

Before invoking Aiken, resolve and validate the disposable destination, then
ensure only that destination has no `build/` directory or `plutus.json`.
Never clean the shared repository checkout to prepare an isolated build.
After the build, verify a consequential schema/count/hash from the generated
blueprint against current source before using it as test or release evidence;
an exit code alone is insufficient.

## Pinned Compiler

Before any build, check or format, confirm the compiler:

```bash
node onchain/aiken/scripts/pinned-compiler.mjs
```

It exits 0 only when the binary (`MIDGARD_AIKEN_BIN`, else `aiken` on `PATH`)
prints the `AIKEN_FORK_VERSION` that both `aiken-ci.yml` and
`midgard-node-ci.yml` declare (`aiken v1.1.23+5adf783` as of 2026-09-26), and
exits 1 otherwise [script: onchain/aiken/scripts/pinned-compiler.mjs]. Stock
v1.1.22 compiles unsound expect-decoders: it keys them by local type name, so
two same-named types in different modules share one decoder
(`.github/workflows/midgard-node-ci.yml` header, issue #521). The local
`aiken` is an aikup-managed symlink, and running `aikup` can silently repoint
it at stock; set `MIDGARD_AIKEN_BIN` to the fork's binary to pin it
regardless of `PATH`.

The assertion runs inside `run-focused-check.mjs`, `guard-focused-selector.mjs`
and the pre-commit hook when `.ak` files are staged [hook: pre-commit], and
`deployment:build` makes its own check against `aiken-ci.yml`
[script: demo/scripts/deployment-profiles.mjs]. CI installs only the fork
[ci: Aiken CI/Assert the pinned compiler identity and put it on PATH]. Blind
spots: a hand-typed `aiken build` or `aiken check` checks nothing, and
`deployment:build` ignores `MIDGARD_AIKEN_BIN` and uses the `aiken` on
`PATH`.

## Which Build Path

| You need                                                                                                                            | Use                                                                         |
| ----------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------- |
| A blueprint the node, or any suite that loads it through `midgard-contracts.ts`, will accept; anything matching CI                  | `pnpm --dir demo deployment:build preprod-testing` from the repository root |
| A traced diagnostic build for a suite that reads the blueprint path directly (for example `demo/midgard-validation` emulator tests) | `aiken build --env testnet --trace-level verbose --trace-filter all`        |
| A blueprint kept apart from the checkout's (traced, comparison, disposable)                                                         | `aiken build --env testnet --out <existing-dir>/<file>.json`                |

CI builds the blueprint with `deployment:build preprod-testing`
[ci: Midgard Node CI/Build testnet Aiken blueprint]. It also writes
`plutus.json.deployment.json`, and the node refuses a blueprint whose build
record is missing or whose recorded hash differs from the file
[runtime: verifyBlueprintDeploymentProfile]. So a plain or traced
`aiken build` output is invisible to the node's loader, and building over a
`deployment:build` output invalidates its record. `deployment:build` with any
profile other than `preprod-testing` rewrites the tracked
`demo/midgard-core/src/generated-deployment-profiles.ts`; do not commit that
[ci: Aiken CI/Check deployment profiles].

`onchain/aiken/plutus.json` is never committed [hook: pre-commit]. Blind spot:
the hook only runs where `.githooks/install` was run, `MIDGARD_SKIP_HOOKS=1`
skips it, and no CI step checks for a tracked blueprint. Stage explicit paths.

Read [references/build-paths.md](references/build-paths.md) when you need
which files each command writes, how the build record is checked, or the
`--out` details.

## The Environment Switch

`--env testnet` compiles `onchain/aiken/env/testnet.ak`, a generated copy of
`env/preprod-testing.ak`. Omitting `--env` compiles `env/default.ak`, a copy of
`env/mainnet.ak`: seven-day block maturity, public-schedule bond and
penalties. The env modules differ only in the generated profile block
(timing, economics, limits); they are regenerated from
`config/deployments/*.yaml` and drift fails
[ci: Aiken CI/Check deployment profiles]. A plain `aiken check`, and the two
focused-check scripts unless `MIDGARD_AIKEN_ENV` is set, run under the default
(mainnet) env [review]. Details in
[references/build-paths.md](references/build-paths.md#the-environment-switch).

## Output And Process Traps

As of 2026-09-26 on the pinned fork:

- With stdout not a terminal, a compile error in `aiken check` or
  `aiken build` exits 1 and prints **no diagnostic**. Trust the exit code, and
  rerun under a pseudo-terminal (`script -qec "aiken check" /dev/null`) to
  read the error [review].
- The JSON test report prints only when stdout is not a terminal, with or
  without `-e`; the focused-check scripts depend on that, so never wrap them
  in `script` [review].
- Stop only processes you started, by PID. `pkill -f aiken` also kills other
  sessions' builds and checks [review].
- Build a copied tree in a directory with no `build/` and no `plutus.json`
  (see Disposable Final-Tree Builds) [review].

Read [references/cli-traps.md](references/cli-traps.md) when a focused count
surprises you or a run printed nothing.

## Language Traps

- A one-item `and {}` does not compile
  (`aiken::check::illegal::logical_op_chain`); write the bare expression
  [ci: Aiken CI/Compile and run the Aiken test suite with the pinned fork].
- `test … fail` over a multi-conjunct `and {}` passes when any conjunct is
  false. See
  [`7d01f2b71` in the writing-tests catalogue](../writing-tests/references/mistakes-we-make.md#green-whatever-the-code-does)
  [review].

## Measured Limits

- A single-step on-chain fold over transaction outputs fit about a hundred
  items: about 120k memory per output under the 20% reserve, measured for the
  networkId forced direction (commit `36546279`, 2026-09-04). Past that, use a
  resumable checkpointed walk as that family did; re-measure for your own
  per-item cost [review].
