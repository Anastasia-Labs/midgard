# Midgard Node Tools

End-to-end, stress, and acceptance tooling that drives a Midgard node from the
outside. It is a separate package with its own binary on purpose: none of these
commands ship in the operator's `midgard-node/dist/index.js`, so demo and
benchmark behavior can never leak into a running node (AGENTS.md: such behavior
must be explicit, isolated, and unavailable by default).

## What This Package Holds

- `src/index.ts`: the `midgard-node-tools` CLI (`dist/index.js`).
- `src/commands/`: the e2e finalizer and state-correction acceptance, the
  managed-service and step-runner commands, stress wallets, the corpus
  generator/verifier, the bounded L2 stress harness, the Phase 4 genesis-ledger
  and T1 recovery gates, and the Phase 4 pipelined-commit process acceptance
  controller.
- `src/e2e/`: the process supervisor, structured step runner, run summary,
  owned-process-group records, DA gates, and the pipelined-commit process
  harness.
- `devnet/phase4-process/`: the isolated local-devnet assets for the Phase 4
  gate (see [`docs/PHASE4_PIPELINED_COMMIT_PROCESS_ACCEPTANCE.md`](docs/PHASE4_PIPELINED_COMMIT_PROCESS_ACCEPTANCE.md)).
- `scripts/verify-phase4-pipelined-process-summary.mjs`: the offline verifier
  for the acceptance summary the controller writes.
- `tests/`: the suites for all of the above.

## How It Relates To `midgard-node`

The tooling compiles the operator package from source. `midgard-node`'s
`exports` map carries only the `midgard-source` condition
(`midgard-node/<subpath>` for `src/`, `midgard-node/tests/<subpath>` for test
helpers); tsc, typescript-eslint, and vitest resolve it directly, and `tsup`
inlines every `midgard-node` module this bundle imports. The operator package
never grows a per-module dist for anyone to resolve, and nothing here is
reachable from the operator binary.

Neither package has a `@/` alias: use relative specifiers inside a package and
`midgard-node/<subpath>` from here. ESLint enforces both.

## Build And Run

```sh
cd demo/midgard-node-tools
pnpm build
node dist/index.js --help
```

Runtime configuration is the node's: the CLI loads the same dotenv and
`NodeConfig` the operator binary does, so run it from (or point it at) the node
checkout whose `.env`, `logs/`, and `dist/index.js` a command should use.

## Commands

| Command                                                              | Purpose                                                                    |
| -------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| `e2e-run-step`, `e2e-start-service`, `e2e-clean-owned-process-group` | Structured step runner, managed service start, fail-closed process cleanup |
| `e2e-finalize-summary`                                               | Collect endpoint/database evidence and write `summary.json` + `summary.md` |
| `e2e-stress-l2-throughput`                                           | Opt-in bounded L2 transfer stress with SQL-grounded stage metrics          |
| `create-l2-wallet`, `stress-wallets:*`                               | Persisted stress wallets: create, prepare, fan-out, consolidate, drain     |
| `stress-corpus-generate`, `stress-corpus-verify`                     | Signed NDJSON transaction corpus for repeatable benchmarks                 |
| `phase4-genesis-ledger`, `phase4-t1-probe`, `phase4-t1-advance`      | Gated Phase 4 local-devnet genesis and T1 recovery commands                |
| `e2e-pipelined-commit-process-acceptance`                            | The Phase 4 crash/restart and two-node process acceptance matrix           |

The operator-facing runbook that sequences these is
`.agents/skills/midgard-e2e-acceptance` at the repository root; it invokes them
as `node "$TOOLS_CLI" <command>` and operator commands as
`node dist/index.js <command>` from `demo/midgard-node`.

## Parallel Fanout Stress Wallets

Parallel fanout stress uses independent L2 wallets so concurrent workers do not
race on the same wallet UTxO. Generate a larger pool once, source the generated
env file, then prepare the subset needed for a run. Run from the node checkout
so the wallet directory and `.env` resolve there:

```sh
cd demo/midgard-node
TOOLS_CLI=../midgard-node-tools/dist/index.js

node "$TOOLS_CLI" create-l2-wallet \
  --count 128 \
  --out-dir .stress-wallets

. .stress-wallets/stress-wallets.env

node "$TOOLS_CLI" stress-wallets:prepare \
  --count 64 \
  --lovelace-per-wallet 12000000 \
  --out-dir .stress-wallets
```

`stress-wallets:prepare` reads existing wallet JSON files, creates missing files
only when `--create-missing` is passed, submits one deposit for each wallet that
does not already have sufficient spendable L2 funding, runs deposit projection,
and verifies each wallet through the node's `/utxos?address=...` endpoint. The
wallet directory contains private seed phrases and is gitignored.

After preparation, pass the generated argument file to the stress runner. Use 16
as the first serious concurrency target, then 32 and 64:

```sh
STRESS_WALLET_ARGS="$(tr '\n' ' ' < .stress-wallets/stress-wallets.args)"

node "$TOOLS_CLI" e2e-stress-l2-throughput \
  --mode parallel-fanout \
  --count 256 \
  --concurrency 16 \
  $STRESS_WALLET_ARGS

node "$TOOLS_CLI" e2e-stress-l2-throughput \
  --mode parallel-fanout \
  --count 512 \
  --concurrency 32 \
  --unsafe-allow-large-stress \
  $STRESS_WALLET_ARGS

node "$TOOLS_CLI" e2e-stress-l2-throughput \
  --mode parallel-fanout \
  --count 1024 \
  --concurrency 64 \
  --unsafe-allow-large-stress \
  $STRESS_WALLET_ARGS
```

The corpus generator and verifier are documented next to the throughput
benchmark scripts they feed, in
[`../midgard-node/README.md`](../midgard-node/README.md#valid-throughput-stress-test).

## Testing

```sh
cd demo/midgard-node-tools
pnpm run typecheck
pnpm run lint
pnpm test
```

The vitest suite reuses `midgard-node`'s per-worker Postgres shard scheme under
its own database prefix (`midgard_tools_test_w<N>`), so it never shares a
database with a concurrently running node suite. `pnpm test` also runs the
offline summary-verifier tests and the Phase 4 devnet asset tests.
