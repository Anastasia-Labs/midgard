# Phase 4 pipelined-commit process acceptance

This is the operator gate for throughput Phase 4 §6.3 and §6.4. It runs real
`node dist/index.js listen` processes against one local Kupmios-backed devnet
and one shared Postgres database. It is intentionally separate from the fast
synthetic supervisor tests.

The command is destructive to the configured local-devnet test state. It
refuses public-network targets, refuses implicit genesis/init, and will not run
unless the explicit acceptance token and a matched reset command are present.

## Matched snapshot prerequisite

`MIDGARD_PHASE4_MATCHED_RESET_COMMAND` must restore the Cardano local-devnet
chain, Postgres state, deployment manifest, and operator state from the same
snapshot. It must also stop any old Midgard `listen` processes. Restoring only
Postgres or only the chain is invalid and will fail the post-reset deployment,
provider, PHAS, and reference-script preflights.

The reset command receives `MIDGARD_PHASE4_SCENARIO_LABEL`. Each crash/control
or contention case invokes it independently so flag-on and flag-off runs start
from the same protocol identity. Do not point this at Preprod or Mainnet.

## Command

Build first, then invoke the acceptance gate from `demo/midgard-node`:

```bash
pnpm build
set -a
. ./.env
set +a
export POSTGRES_HOST=127.0.0.1
export POSTGRES_PORT=5433
export MIDGARD_PHASE4_PROCESS_ACCEPTANCE=pipelined-commit-live-v1
export MIDGARD_PHASE4_PROCESS_TARGET=local-devnet
export MIDGARD_PHASE4_MATCHED_RESET_COMMAND='/absolute/path/to/restore-matched-phase4-snapshot.sh'
# Must advance/evict the submitted N tip and print the reviewed recovery
# attestation. L2 header hashes are 56 lowercase hex characters. Cardano
# transaction/block hashes and SHA-256 digests are 64 lowercase hex characters.
export MIDGARD_PHASE4_T1_RECOVERY_COMMAND='/absolute/path/to/evict-or-advance-t1-tip.sh'
pnpm accept:phase4:pipelined-process
```

The two configured genesis wallet seeds must be distinct and funded in the
restored L2 genesis state. The command uses wallet A to seed block N and wallet
B to seed the retained N+1 payload.

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

It runs and records:

1. One matched-snapshot T1 recovery plus three actual-process SIGKILL/restart cases: mid speculative build,
   candidate ready while N is unconfirmed, and confirmation wake before the
   N+1 journal write.
2. For T1, the restarted real process must build its next candidate with the
   recovered canonical tip hash as `base_header_hash` before submission. For
   every crash case: no journal beyond N after the crash, a fresh candidate
   after restart, a real N+1 submission, a matched-snapshot flag-off control,
   and normalized DB-state equality including exact retained transaction IDs
   and CBOR.
3. Two normal `listen` processes with private MPF paths and shared Postgres:
   one submission winner, a lease-Busy loser, then T2 invalidation.
4. A separate two-process journal-before-submit SIGKILL: one journal winner,
   lease expiry, unsubmitted-journal recovery, and survivor submission.

Raw process logs and `summary.json` are written to the selected run directory.

## Independent offline verification

After copying or freezing `summary.json`, verify it without starting a node,
connecting to providers, or mutating Docker/devnet state:

```bash
pnpm verify:phase4:pipelined-process-summary -- /absolute/path/to/summary.json
```

The verifier accepts only the exact
`midgard-phase4-pipelined-commit-process-acceptance-v1` evidence schema. It
checks the isolated snapshot identity, ordered crash matrix and signals,
restart markers, no-journal-beyond-base invariant, flag-on/flag-off logical DB
equality, T1 recovery attestation and payload continuity, and both contention
outcomes. It also prints the summary SHA-256 and bound artifact identity for
the freeze record.

Exit status `0` means the summary passed, `1` means the artifact is malformed
or does not satisfy the schema or invariants, and `2` means the invocation is
invalid or the requested file cannot be read. This is an offline consistency
verifier; it does not replace preserving the raw logs, reset attestations, or
snapshot files named by the acceptance run.
