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

For a traced diagnostic blueprint, use (from `onchain/aiken`):

```bash
../../scripts/aiken build --env testnet --trace-level verbose --trace-filter all
```

For a normal blueprint without traces, use (from `onchain/aiken`):

```bash
../../scripts/aiken build --env testnet
```

## Focused Checks

Run large Midgard vectors one compiler process at a time, and run every check
through one of two guards. Each fails closed when it collects zero tests, and
each prints the largest unit-test memory and CPU units with their share of the
GOAL_SPEC §3.3 basis (a reading, not a gate; a test is not a transaction):

- **Exact tests in one module:** pass every unique name to one invocation
  rather than recompiling the tree for each. It fails unless exactly that many
  tests from that one module pass. Its stdout stays Aiken's report (the
  script-budget judge parses it); the summary goes to stderr.
  [script: onchain/aiken/scripts/run-focused-check.mjs]

  ```bash
  node scripts/run-focused-check.mjs \
    midgard/validation_machine/machine_types \
    first_exact_test_name \
    second_exact_test_name
  ```

  For a dotted test filename, pass the full source module without `.ak`; for
  example, tests in `cek-data-traverse.max-cardano.test.ak` use
  `midgard/cek-data-traverse.max-cardano.test` plus the exact test name. The
  guard builds the shortened Aiken selector and checks the reported module.

- **Every test in the modules a selector matches:**
  `node scripts/guard-focused-selector.mjs <module-selector>...`, or `--all`
  for the whole suite. The module part is a substring match, so
  `midgard/native_tx_field_access_v1` also runs
  `midgard/native_tx_field_access_v1.test` and
  `midgard/native_tx_field_access_v1_golden.test`. It prints one JSON line per
  selector: the collected count and the `exUnits` summary.

A raw `aiken check -m` exits 0 when it collects nothing: a selector with no `/`
and no `.` (`-m name_v1` is read as a test-name filter, not a module), a dotted
module name (the text after the first `.` becomes a test-name filter), or a
bare test name with `-e`. The fork's `Suspicious test filter` warning fires
only for a single test-name filter, and not at all with output piped. Full
selector rules: [references/cli-traps.md](references/cli-traps.md).

**Never gate or report on a hand-rolled `aiken check -m`**; run the selector
through one of those two scripts. Blind spot: nothing stops a hand-typed
`aiken check -m` from being run or quoted as evidence. [review]

A CI workflow step never runs `aiken check` directly; it goes through one of
the two guards. Blind spot: the lint reads `run:` text, so a script that CI
calls and that spawns `aiken check` itself is not seen.
[ci: repo-tools-ci/Lint the workflows]

Evidence must report a nonzero collected total, not only the process exit code.
[review]

## Repo Workflow

- Treat `onchain/aiken/plutus.json` as the generated blueprint output unless a task explicitly chooses another `--out` path.
- Run suites with `node scripts/contrib.mjs test`; its prepare step leaves a ready blueprint (copied from a checkout with identical inputs, else `deployment:build`) and refuses `MIDGARD_REAL_BLUEPRINT_PATH`. A raw run rebuilds a missing or stale default blueprint in its stamp setup; a blueprint it names there must carry a fresh build record, or the setup refuses it. [script: demo/midgard-test-support/blueprint-stamp-setup.js]
- If tracing is needed for a script failure, rebuild with `--trace-level verbose --trace-filter all` before rerunning the failing emulator test.
- Preserve production correctness: do not switch transaction completion to `.complete({ localUPLCEval: false })` to bypass failures. Blind spot: pre-existing uses are baselined with reasons (`docs/agents/lint-rules.md`). [eslint: midgard/local-uplc-eval]

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

## Pinned Compiler

The compiler is the `aiken` input of `demo/flake.nix`, locked to one commit
in `demo/flake.lock`. Run it as `scripts/aiken <args>` (from `onchain/aiken`:
`../../scripts/aiken`). It needs nix and nothing else: it builds exactly the
locked commit (cached in the nix store after the first time) and runs it.
[script: scripts/lib/aiken.mjs]

Every repository script, the pre-commit hook and CI reach the compiler only
this way, so none of them can run a different aiken; nothing reads `aiken`
from `PATH`. To move the pin, update the flake input
(`nix flake update aiken --flake ./demo`) and commit the lock. A hand-typed
bare `aiken` runs whatever is on your `PATH`, which is not the pin: never use
one for a build, a check or evidence. [review]

## Which Build Path

| You need                                                                                                                            | Use                                                                                |
| ----------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------- |
| A blueprint the node, or any suite that loads it through `midgard-contracts.ts`, will accept; anything matching CI                  | `pnpm --dir demo deployment:build preprod-testing` from the repository root        |
| A traced diagnostic build for a suite that reads the blueprint path directly (for example `demo/midgard-validation` emulator tests) | `../../scripts/aiken build --env testnet --trace-level verbose --trace-filter all` |
| A blueprint kept apart from the checkout's (traced, comparison, disposable)                                                         | `../../scripts/aiken build --env testnet --out <existing-dir>/<file>.json`         |

CI builds the blueprint with `deployment:build preprod-testing` (Node CI
step "Build testnet Aiken blueprint"). It also writes
`plutus.json.deployment.json`, and the node refuses a blueprint whose build
record is missing or whose recorded hash differs from the file
(`verifyBlueprintDeploymentProfile`). So a plain or traced
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
  rerun under a pseudo-terminal (`script -qec "../../scripts/aiken check" /dev/null`) to
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
