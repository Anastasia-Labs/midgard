# Live Acceptance Runbook

Use this reference for every fresh, attach and resume run. Read it completely
before changing live state. Every run is one command, `e2e-stack`; the
configuration, secrets, ports and funding model it expects are specified in
[PREPROD_STACK.md](../../../../demo/midgard-node-tools/docs/PREPROD_STACK.md).

## Contents

1. [Prepare the configuration](#prepare-the-configuration)
2. [Check it offline](#check-it-offline)
3. [Choose where to run](#choose-where-to-run)
4. [Run](#run)
5. [What each step does](#what-each-step-does)
6. [Evidence](#evidence)
7. [Operate the running stack](#operate-the-running-stack)
8. [Report](#report)

## Prepare the configuration

Start from the repository root. Keep local Preprod provider state intact.

```bash
REPO_ROOT="${REPO_ROOT:-$(git rev-parse --show-toplevel)}"
TOOLS_DIR="$REPO_ROOT/demo/midgard-node-tools"
NODE_DIR="$REPO_ROOT/demo/midgard-node"
STACK_CONFIG="${STACK_CONFIG:?absolute path of the stack configuration JSON}"
```

Copy `demo/midgard-node-tools/config/preprod-stack.example.json` to a path
outside the repository and replace its absolute paths. The file names secret
environment variables; it never holds their values. Verify the node `.env` it
points at, without printing seed phrases:

- `NETWORK=Preprod` and `MIDGARD_DEPLOYMENT_PROFILE=preprod-testing`;
- `L1_PROVIDER=Kupmios`, loopback Kupo and Ogmios URLs, and no
  `L1_PROVIDER_FAILOVER`;
- `RUN_GENESIS_ON_STARTUP=false` and the exact L2 `MIN_FEE_A`/`MIN_FEE_B`;
- an explicit `MIDGARD_POSTGRES_HOST_PORT` that is neither 5433 nor 55433
  (both belong to test databases);
- every wallet, DA member, transport and DA password variable the
  configuration names, with the stack-only secrets under distinct `STACK_`
  names and each wallet role distinct;
- `DA_THRESHOLD` between `ceil(2 * members / 3)` and the member count; and
- the L1 history source pin `L1_HISTORY_GENESIS_LOSSLESS_SHA256`, set to the
  `sha256` that `node dist/index.js history-genesis-pin` prints against the
  intended chain. `listen` refuses to start without it, and a different value
  means a different chain, not a setting to refresh.

Fund the wallets to the configured budgets before a fresh run, each with a
plain output of at least 5 ADA. Attach and resume need only 5 ADA of working
capital per wallet plus that output. Put the watcher release input and secret
files in place as PREPROD_STACK.md describes; setup never fabricates funding
measurements.

Install the workspace dependencies with the repository's pnpm, and use Node
22.16 or newer, the pinned Aiken compiler, Docker Compose 2.21 or newer, Go,
Rust/Cargo and Linux `flock`. Setup builds the Preprod contract profile, the
workspace runtimes and the service images itself.

## Check it offline

`--check` loads the configuration and runs every check that needs no build,
network or service. It starts nothing and spends nothing. It loads the built
workspace packages, so build them once first:

```bash
pnpm --dir "$TOOLS_DIR" run e2e-stack --config "$STACK_CONFIG" --check
```

It prints the network, node and run directories, and the configured number of
journey cycles. Fix every refusal before a live run. The Compose version, the
`compose config` rendering, the watcher key and bearer decoding and the
prover/availability seed match are checked later, after the builds.

## Choose where to run

- **Fresh**: fresh local storage is required: a Postgres volume that was never
  used or was only migrated, and an empty or absent node `db` directory. Do not
  delete existing volumes to get one. Run from a separate linked worktree; the
  operator Compose wrapper gives it its own project, volumes, host ports and
  node directory. Point `nodeRoot`, `envFile` and `runDirectory` at that
  worktree.
- **Attach or resume**: run the same command with the same configuration file
  from the same checkout. The node directory is bound to the run directory and
  identity recorded in `deploymentInfo/full-stack-intent.json`; another
  configuration is refused there.

A run's identity is its network, deployment profile, node and run
directories, wallet seeds, DA members, transports, threshold, owners and
cosigner, watcher keys, and release signer and program commitments. Timeouts,
journey size, budgets, ports, templates and the watcher bearer may change
between runs; a change that reaches the generated services regenerates them
and recreates the affected containers. Raising `journey.cycles` on a complete
deployment runs only the new cycles.

## Run

Run the full setup and wallet journeys:

```bash
pnpm --dir "$TOOLS_DIR" run e2e-stack --config "$STACK_CONFIG"
```

Or finish after setup and readiness, leaving Compose supervising the
services:

```bash
pnpm --dir "$TOOLS_DIR" run e2e-stack --config "$STACK_CONFIG" --setup-only
```

The package script rebuilds the tooling first. An already built tool runs the
same command as `node demo/midgard-node-tools/dist/index.js e2e-stack`. The
command prints `<step>: running` and `<step>: confirmed` for each step and a
JSON summary at the end.

To attach or resume, run exactly the same command again. Provider startup can
take hours on a first run while the Preprod snapshots download and
synchronize; run it where it can be left running, and do not interrupt a slow
step to retry it.

When the command stops with an error, do not rerun it blindly. Read the
message and follow [recovery.md](recovery.md) first.

## What each step does

Before the journaled steps, prerequisites check the Compose version, build the
contracts and workspace runtimes, and record `prerequisites.json`. The steps
then run in this order. `providers`, `storage` and `wallets` run their checks
on every run; every other step already confirmed in `stack-journal.json` is
reconciled against Cardano and its saved records, never repeated.

| Step                    | What it does                                                                                                                                                                 | On rerun                                                                                                                                   |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `providers`             | Starts the local Cardano node with Ogmios, Kupo and Postgres through the operator Compose wrapper, then runs the node's `l1-provider-preflight`.                             | Starts and checks them again.                                                                                                              |
| `storage`               | Checks that storage is fresh (fresh run) or holds this deployment's marker and event history (attach).                                                                       | Refuses mismatched storage.                                                                                                                |
| `wallets`               | Checks each wallet's budget (fresh) or working capital (resume) and its plain 5 ADA output.                                                                                  | Checks again.                                                                                                                              |
| `nonce`                 | Runs the node's `prepare-hub-oracle-one-shot-nonce`, which records the signed nonce in the deployment run state before first submitting it.                                  | Completes a nonce that landed or resubmits exactly the recorded bytes; never builds a second nonce. Attaches when `init` is already final. |
| `references`            | Publishes the node-runtime reference scripts and confirms them with `reconcile reference-scripts-complete --scope node-runtime`.                                             | Confirms published scripts before publishing any missing one.                                                                              |
| `initialize`            | Runs `db:migrate` and `init`, then verifies the finalized deployment manifest.                                                                                               | Reconstructs the manifest from Cardano when `init` confirmed unrecorded; waits when a finalized `init` left the tip.                       |
| `operator`              | Registers and activates the operator with `register-active-operator` after `operator-status`.                                                                                | Resumes from registered; refuses a duplicate.                                                                                              |
| `runtime-configuration` | Generates the node environment, DA committee, public retained DA, watcher and authority services, and `<runDirectory>/services/compose.json`.                                | Reuses them while their input digest matches; otherwise regenerates.                                                                       |
| `storage-identity`      | Writes or verifies the Postgres deployment marker and the durable storage identity.                                                                                          | Refuses a changed identity.                                                                                                                |
| `services`              | Builds images, pins the native owner, starts DA storage, checks the DA submitter wallet, starts the committee, runs the producer DA preflight, then starts node and watcher. | Confirms a ready stack from one snapshot; otherwise starts the services again.                                                             |
| `cycle-N-deposit`       | Submits a deposit with a stable submission ID and waits for automatic absorption and the exact L2 credit.                                                                    | Reuses the saved intent and receipt.                                                                                                       |
| `cycle-N-transfer`      | Saves the signed transfer before sending it, waits for commitment, retrieves the payload over public libp2p DA and waits for confirmed-ledger finality.                      | Resends the same bytes after a lost response.                                                                                              |
| `cycle-N-withdrawal`    | Submits the recipient's withdrawal and waits for the node's automatic payout to the exact destination and value.                                                             | Verifies an already paid payout through its historical transaction.                                                                        |

The generated node environment sets `MIN_QUEUE_LENGTH_FOR_MERGING=1` for these
small journeys and uses the existing automatic merge worker. No merge, payout
or repair command is ever run. The committee is started and ready before the
producer's DA preflight, and the node starts only after that preflight.

## Evidence

Everything lives under the configured `runDirectory`:

- `stack-journal.json`: the step journal, written atomically and fsynced, with
  the run identity digest;
- `attempts/<step>-<uuid>.log` and `.json`: the raw log and structured summary
  of every command the stack ran;
- `deployment-run-state.json`: the node's deployment run state, including the
  signed nonce;
- `prerequisites.json`, `<runDirectory>/services/compose.json` and the generated service
  configuration;
- `cycle-N-*.json`: per-cycle intents, receipts, balances and payout evidence,
  and the `da-<header>.cbor`/`.json` payloads retrieved over public DA; and
- `setup-summary.json` or `journey-summary.json`
  (`midgard-full-stack-summary-v1`), with `result`, `confirmedSteps` and
  `cycles`.

The deployment manifest is in the node directory's `deploymentInfo/`. These
records are private: they hold transaction bodies and intents but no secret
values. Preserve all of them with the run.

## Operate the running stack

Manage the services only through the operator Compose wrapper, from the node
directory, with the generated override:

```bash
cd "$NODE_DIR"
RUN_DIRECTORY="${RUN_DIRECTORY:?the configured runDirectory}"
STACK_COMPOSE=(bash scripts/operator-compose.sh --env-file .env
  -f docker-compose.yaml -f docker-compose.kupmios.yaml
  -f "$RUN_DIRECTORY/services/compose.json")
"${STACK_COMPOSE[@]}" ps
"${STACK_COMPOSE[@]}" logs --no-color midgard-node
```

`restart` on that project keeps the generated settings. Do not start the stack
with the node README's plain `docker compose ... up`, which reads the `.env`
holding the stack secrets without the generated settings. To change the
services, change the configuration and rerun `e2e-stack`.

Read-only checks the stack itself polls: node `/healthz`, `/readyz` and
`/tx-status?tx_hash=<hash>` at the configured `endpoint`, each committee
member's `/readyz` on `committeeApiBase + index`, and the watcher's
`/v1/status`. Print the wrapper's derived ports with
`bash scripts/operator-compose.sh --print-env --env-file .env`.

## Report

Report, with artifact paths rather than bodies:

- run mode, reason and configuration path (never secret values);
- the final summary's `result`, `confirmedSteps` and `cycles`;
- the deployment manifest ID and the nonce, `init` and operator transaction
  hashes from the journal;
- per cycle: the deposit, transfer and withdrawal hashes, the committed header,
  the public DA payload file, and the exact L2 and payout values;
- every stop, its message, the diagnosis and the rerun that followed; and
- that release readiness was not run, unless
  [release-readiness.md](release-readiness.md) was followed.

Do not call the run complete while any step is `running` in the journal or
the summary is missing.
