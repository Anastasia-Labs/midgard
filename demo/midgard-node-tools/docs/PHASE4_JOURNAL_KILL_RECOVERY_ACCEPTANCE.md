# Phase 4 journal-kill recovery acceptance

This is the operator gate for recovery from a commit journal that was written
but never submitted. It runs two real `node dist/index.js listen` processes on
the default commit path against one local Kupmios-backed devnet and one shared
Postgres database. Each node has its own MPF stores. It is intentionally
separate from the fast synthetic harness tests.

The command is destructive to the configured local-devnet test state. It
refuses public-network targets, refuses implicit genesis/init, and will not run
unless the explicit acceptance token and a matched reset command are present.

## Matched snapshot prerequisite

`MIDGARD_PHASE4_MATCHED_RESET_COMMAND` must restore the Cardano local-devnet
chain, Postgres state, deployment manifest, and operator state from the same
snapshot. It must also stop any old Midgard `listen` processes. Restoring only
Postgres or only the chain is invalid and will fail the post-reset deployment,
provider, PHAS, and reference-script preflights.

The reset command receives `MIDGARD_PHASE4_SCENARIO_LABEL`
(`journal-kill-contention`). Do not point it at Preprod or Mainnet.

## Command

The acceptance controller is a `midgard-node-tools` command; the node under
test is the operator package's own `dist/index.js`. Build both, then invoke
the gate from `demo/midgard-node-tools` (the package script points it at the
sibling node root, where the operator binary, `logs/`, and the isolation env
resolve):

```bash
pnpm --dir ../midgard-node build
pnpm build
# First generate, bootstrap, and capture the matched isolated devnet described
# in devnet/phase4-process/README.md. Use its immutable acceptance.env.
export MIDGARD_PHASE4_RUN_DIR=/absolute/path/to/matched-phase4-run
export MIDGARD_PHASE4_PROCESS_ENV_FILE="$MIDGARD_PHASE4_RUN_DIR/secrets/acceptance.env"
export MIDGARD_PHASE4_PROCESS_DEPLOYMENT_MANIFEST_PATH="$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/contract-deployment-info.json"
export MIDGARD_PHASE4_PROCESS_RUN_DIR="$MIDGARD_PHASE4_RUN_DIR/acceptance"
export MIDGARD_DOTENV_MODE=disabled
export MIDGARD_PHASE4_PROCESS_ACCEPTANCE=journal-kill-recovery-live-v1
export MIDGARD_PHASE4_PROCESS_TARGET=local-devnet
export MIDGARD_PHASE4_MATCHED_RESET_COMMAND="$PWD/devnet/phase4-process/scripts/reset.sh"
pnpm accept:phase4:journal-kill-recovery
```

The two configured genesis wallet seeds must be distinct and funded in the
restored L2 genesis state. A seed node uses wallet A to commit block N and then
receives a wallet-B transfer that stays retained for N+1; the two contending
nodes race to commit that N+1 payload.

Optional isolated port/path controls:

- `MIDGARD_PHASE4_NODE_A_PORT` (default `3101`)
- `MIDGARD_PHASE4_NODE_B_PORT` (default `3102`)
- `MIDGARD_PHASE4_NODE_A_METRICS_PORT` (default `4101`)
- `MIDGARD_PHASE4_NODE_B_METRICS_PORT` (default `4102`)
- `MIDGARD_PHASE4_STATE_QUEUE_LEASE_TTL_MS` (default `5000`)
- `MIDGARD_PHASE4_PROCESS_TIMEOUT_MS` (default `600000`; must cover the
  journal validity bound plus the 30-second unsubmitted-recovery grace)
- `MIDGARD_PHASE4_PROCESS_RUN_DIR` (default timestamped directory under
  `logs/`)

## Required outcomes

The command fails on the first missing prerequisite or failed assertion. It
does not silently skip.

Both nodes are armed with the one-shot `journal_prepared_before_submit` crash
checkpoint. The first node to reach it consumes the arm file, which means it
holds the state-queue mutation lease and has written its pending-finalization
journal but has not submitted. The harness SIGKILLs that node. The surviving
node must then log, in this order:

1. `Skipping block commitment trigger because the state-queue mutation lease is busy`
   while the killed winner's lease is still live;
2. `abandoning unsubmitted journal and recovering canonical state_queue tip`
   after the lease has expired and the recovery grace has passed;
3. `Block submitted; local finalization is intentionally deferred until L1 confirmation.`
   for its own block.

After the run, Postgres must hold exactly one active journal (the survivor's)
and a `failed` lease record whose error is `lease expired before release`.

Raw process logs and `summary.json` are written to the selected run directory.

## Independent offline verification

After copying or freezing `summary.json`, verify it without starting a node,
connecting to providers, or mutating Docker/devnet state:

```bash
pnpm verify:phase4:journal-kill-recovery-summary -- /absolute/path/to/summary.json
```

The verifier accepts only the exact
`midgard-phase4-journal-kill-recovery-acceptance-v1` evidence schema. It checks
the isolated snapshot identity and PHAS registration, that the winner was
SIGKILLed at the journal checkpoint, that the survivor was stopped by its stop
file after the three ordered log lines above, the single survivor journal, and
the expired-lease record. It also prints the summary SHA-256 and bound artifact
identity for the freeze record.

Exit status `0` means the summary passed, `1` means the artifact is malformed
or does not satisfy the schema or invariants, and `2` means the invocation is
invalid or the requested file cannot be read. This is an offline consistency
verifier; it does not replace preserving the raw logs, reset attestations, or
snapshot files named by the acceptance run.
