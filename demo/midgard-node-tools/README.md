# Midgard Node Tools

End-to-end, stress, and acceptance tooling that drives a Midgard node from the
outside. It is a separate package with its own binary on purpose: none of these
commands ship in the operator's `midgard-node/dist/index.js`, which keeps these entrypoints outside the operator CLI. Runtime benchmark
options still require their explicit configuration gates.

## What This Package Holds

- `src/index.ts`: the `midgard-node-tools` CLI (`dist/index.js`).
- `src/commands/`: the e2e finalizer and state-correction acceptance, the
  managed-service commands, stress wallets, the corpus
  generator/verifier, the bounded L2 stress harness, the Phase 4 genesis-ledger
  gate, and the Phase 4 journal-kill recovery acceptance controller.
- `src/e2e/`: the process supervisor, structured step runner, run summary,
  owned-process-group records, DA gates, and the journal-kill process
  harness.
- `devnet/phase4-process/`: the isolated local-devnet assets for the Phase 4
  gate (see [`docs/PHASE4_JOURNAL_KILL_RECOVERY_ACCEPTANCE.md`](docs/PHASE4_JOURNAL_KILL_RECOVERY_ACCEPTANCE.md)).
- `scripts/verify-phase4-journal-kill-recovery-summary.mjs`: the offline verifier
  for the acceptance summary the controller writes.
- `tests/`: the suites for all of the above.

## How It Relates To `midgard-node`

The tooling compiles the operator package from source. `midgard-node`'s
`exports` map carries only the `midgard-source` condition
(`midgard-node/<subpath>` for `src/`, `midgard-node/tests/<subpath>` for test
helpers); tsc, typescript-eslint, and vitest resolve it directly, and `tsup`
inlines every `midgard-node` module this bundle imports. The operator package
never grows a per-module dist for anyone to resolve, and the operator CLI does not register these tooling commands.

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

## Persistent Preprod setup and wallet tests

Run `pnpm --dir demo/midgard-node-tools run e2e-stack --config /absolute/path/stack.json`
from the repository root. It saves confirmed progress, resumes the deployment,
starts the existing Compose stack and checks deposits, transfers and automatic
withdrawal payments. See [configuration and recovery](docs/PREPROD_STACK.md).
This is the only live acceptance flow; the operating runbook is
`.agents/skills/midgard-e2e-acceptance` at the repository root.

## Commands

| Command                                              | Purpose                                                                                            |
| ---------------------------------------------------- | -------------------------------------------------------------------------------------------------- |
| `e2e-stack`                                          | One-command persistent Preprod setup, resume and wallet journeys                                   |
| `e2e-start-service`, `e2e-clean-owned-process-group` | Managed service start, fail-closed process cleanup                                                 |
| `e2e-finalize-summary`                               | Re-derives an `e2e-stack` run into the release-readiness dashboard (`summary.json` + `summary.md`) |
| `e2e-stress-l2-throughput`                           | Opt-in bounded L2 transfer stress with SQL-grounded stage metrics                                  |
| `create-l2-wallet`, `stress-wallets:*`               | Persisted stress wallets: create, prepare, fan-out, consolidate, drain                             |
| `stress-corpus-generate`, `stress-corpus-verify`     | Signed NDJSON transaction corpus for repeatable benchmarks                                         |
| `phase4-genesis-ledger`                              | Gated Phase 4 local-devnet genesis command                                                         |
| `e2e-journal-kill-recovery-acceptance`               | The Phase 4 two-node journal-before-submit SIGKILL recovery acceptance                             |

`e2e-stack` records every command it runs through the structured step runner
in its run directory's `attempts/`. `e2e-start-service` supervises
hand-started devnet processes (see `.agents/skills/running-the-devnet`).
`e2e-finalize-summary --stack-config <path>` re-derives an `e2e-stack` run's
functional evidence from its journal, receipts, node and database. Its
state-correction gates stay blocked as not run, because the stack produces no
fault-proof evidence; see
`.agents/skills/midgard-e2e-acceptance/references/release-readiness.md`.

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
  --verify-timeout-ms 1200000 \
  --out-dir .stress-wallets
```

`stress-wallets:prepare` reads existing wallet JSON files, creates missing files
only when `--create-missing` is passed, submits one deposit for each wallet that
does not already have sufficient spendable L2 funding, and waits until the
running node's `/utxos?address=...` endpoint shows each wallet funded. The node
ingests and projects the deposits itself; they become spendable once a
confirmed header includes them. A deposit is due five minutes after its
validity upper bound on the testing profiles, so the example allows 20 minutes
instead of the five-minute default. The
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

## Devnet wallet fee runway

Fresh `devnet-stack` runs fund all fourteen roles once from the generated
chain's genesis UTxO. There is no role-wallet refill loop. The private chain
has 2 billion ADA total supply, with 1 billion delegated and 1 billion in the
bootstrap UTxO. This is an isolated-chain funding difference, not a change to
Preprod protocol parameters. Existing deployments are not changed.

The policy in `devnet/preprod/funding-policy.json` targets **180 days with a
5× margin**. Nine submitting service wallets reserve 20 ADA per transaction
at 4,320 transactions/day (one per mean 20-second Cardano block). These are
**planning estimates**, not measured role burn or enforced admission limits.
Each reserves 77,760,000 ADA for fees, representing 900 days at that planning
rate or 180 days at five times the rate. Startup protocol capital is additional.
Each wallet also receives a separate 50 ADA pure-ADA collateral output, above
150% of the 20 ADA planning fee. Users retain five independent spending outputs.

| Role                             |         Planned tx/day | Main funding, ADA |
| -------------------------------- | ---------------------: | ----------------: |
| operator                         |                  4,320 |        77,960,000 |
| merge                            |                  4,320 |        77,810,000 |
| settlement                       |                  4,320 |        77,810,000 |
| daSubmitter0, daSubmitter1       |             4,320 each |   77,780,000 each |
| daAvailability0, daAvailability1 |             4,320 each |   77,780,000 each |
| watcherProver                    |                  4,320 |        77,810,000 |
| watcherAvailability              |                  4,320 |        77,780,000 |
| referenceScript                  |           startup only |           100,000 |
| daCosigner                       | off-chain signing only |             1,000 |
| userA, userB, userC              |                24 each |      482,000 each |

Total bootstrap role funding including collateral is **701,837,700 ADA**,
leaving 298,162,300 ADA before bootstrap fees and existing reserve-float costs.
Reference-script startup capital covers 1,000 publications at the 20 ADA
planning fee with the same 5× margin. Users reserve 432,000 ADA for fees at
24 transactions/day plus their original 50,000 ADA journey holdings; transfers
and deposits consume holdings separately from fees.

Read-only evidence assessed on 2026-10-02: lc1 reference-publication logs from
2026-09-30 contain 339 and 397 submitted transactions in two deployment runs;
the largest observed signed transaction is 16,265 bytes. At the pinned 44
lovelace/byte plus 155,381 fixed fee, that size alone contributes 871,041
lovelace. Maximum pinned execution charges add 1,730,750 lovelace; reference
script surcharges are additional. The 20 ADA planning fee leaves a large
allowance for them and is not claimed as a protocol maximum. The public
bootstrap and reserve-float signed transactions paid 308,061 and 170,473
lovelace respectively; these are controller fees, **not measurements of role
burn**. No representative steady-state role fee series was available in the
logs, and the watcher availability database checkpoint contained no intents.
Re-measure actual daily burn after the final fresh run; usage exceeding the
planning envelope or protocol-cost changes shorten this conditional runway.

The legacy seven-wallet `phase4-process/scripts/fund-wallets.sh` bootstrap
uses the same policy: 78 million ADA plus 50 ADA collateral per wallet,
546,000,350 ADA total. It is a separate bootstrap flow, not an additional
allocation in the fourteen-wallet stack. Both fund once; neither schedules
online treasury transfers to role wallets.

## Devnet service refusals

`node dist/devnet-stack.js status --run-dir /absolute/run` reports a service
that exited 78 as `ready: false`, with reason
`configuration_or_deployment_refused` and a `refusal` record containing its
current `refusalId` and log path. The refusal survives supervisor replacement.
Exit 70 and signal exits retain the normal bounded restart policy. Elapsed time
or changing a file never authorizes another attempt after exit 78.

Correct the cause shown in the role's log using that role's normal operational
procedure, then request one validation attempt:

```sh
node dist/devnet-stack.js recover-service --run-dir /absolute/run \
  --service watcher --refusal-id TOKEN_FROM_STATUS \
  --note "Operator explanation of the external correction"
```

The note records an operator's explanation; it is not verified configuration
or chain evidence. The command checks the existing run, completed deployment,
public contract-manifest pin, fresh code and running supervisor's service set.
The queued permission binds that exact runtime code and service set. The daemon
checks both when consuming it and again after asynchronous prestart work; stale
or incomplete permissions stay refused and need a new explicit request. Missing
recorded identities or deployment artifacts are refused without generating or
copying replacements. Restore exact recorded state or explicitly create a new
run through the deployment procedure.

The child retains all its normal integrity checks. The refusal clears only
when that attempt answers its configured readiness probe positively; a failed
attempt stays held, and another exit 78 creates a new token. Roles without a
validated readiness probe require an explicit service-specific recovery design.

The public retained-DA reader uses its own `/readyz` on the declared loopback
port `7406 + portOffset`; `/healthz` alone never clears a refusal. The reader
checks its bound libp2p listener and a live SELECT with its recorded read-only
database role. Restore connectivity and the exact recorded SELECT privileges;
do not substitute committee credentials or recreate retained tables. Request
`recover-service --service public-retained-da` with the token shown in status
and an explanation of that correction. The same code, deployment and service
set guards apply; switching the port or credentials requires the normal
controller/deployment procedure, not an old queued recovery request.

History archive/tunnel/recorder roles still remain held after an exit 78 until
an authenticated service-specific readiness check is supported.
A plain HTTP response or TLS connection is insufficient.

Known watcher refusals require correcting runtime configuration, restoring the
verified deployment authority/rule bundle for the same deployment, or matching
the release's finality policy. System errors such as an unavailable file retain
the watcher's transient classification. A correction that changes deployment
identity, genesis, protocol parameters or the public manifest requires the
normal explicitly authorized deployment procedure and a new run when required;
`recover-service` cannot adopt that change. Broken ancestry or missing retained
history needs authenticated recovery/backfill, not a configuration guess,
checkpoint reset or timer clearance.
