# Release-Readiness Gates

Read this reference when asked whether a live run proves release readiness,
fault proofs, state correction or crash/rollback recovery. An `e2e-stack` run
proves the functional deployment, DA, finality and payout path, and the
finalizer re-derives that proof from the run's own records. It does not
produce the state-correction evidence, so the release-readiness verdict stays
blocked.

## Contents

1. [What the gates require](#what-the-gates-require)
2. [What the finalizer reads from a stack run](#what-the-finalizer-reads-from-a-stack-run)
3. [What to run and report](#what-to-run-and-report)
4. [Finalizer inputs](#finalizer-inputs)

## What the gates require

The release-readiness verdict is written by the `midgard-node-tools` command
`e2e-finalize-summary` into `summary.json` and `summary.md`, with
`functionalVerdict`, `cleanRunVerdict`, `verdict` and `nextSafeAction`. Beyond
the functional checks, it gates on these state-correction labels:

- `state_correction_acceptance`: every launch-scope family, in canonical
  catalogue order, detected from public L1+DA by the production watcher, with
  its route, proof init/steps, the permanent proof token, state-queue removal
  and correction, and resumed verification;
- `state_correction_exact_economics`: the exact operator slash and prover
  reward, expected and observed in lovelace;
- `withdrawal_reserve_payout`: a real withdrawal through order, reserve,
  payout init, every payout add and conclude, with exact destination and value
  digests;
- `forced_classification_directions`: a valid block marked invalid and
  restored, and an invalid block marked valid and corrected, both
  watcher-driven;
- `watcher_crash_rollback_matrix`: the 22 cases of
  `REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS` in
  `e2e-state-correction-acceptance.ts`, in order, each with zero duplicate
  submissions, lost evidence, false verified states or unrecoverable
  workflows; and
- `state_correction_final_reconciliation`: an empty state queue, retained
  proof tokens and a drained node database, re-read by the local Kupmios
  authority.

The aggregate claim is only an index. The finalizer re-derives every gate from
immutable workflow journals, raw Kupo and Ogmios responses with recomputable
digests, raw recovery outputs, a raw node-database export, the deployment
manifest, blueprint, catalogue, parameters, and a live re-read through local
Kupmios. A bundle of mutually consistent files without that live read stays
blocked.

## What the finalizer reads from a stack run

Run the finalizer against the same stack configuration, on the host that ran
the stack, while its services are still up. It loads the configuration the way
the stack's `--check` does, so the run directory, node endpoint, database port,
admin key and local Kupmios are the stack's own, and it refuses provider
failover and remote endpoints. From the run directory it re-applies the checks
the stack made before confirming each step:

- `stack_run_identity`: `stack-journal.json` belongs to this configuration,
  records exactly the steps it runs, all `complete`, and
  `journey-summary.json` was written from that journal;
- `stack_deposit_credit`: each `cycle-N-deposit` receipt's event, its complete
  settlement job, and the exact configured L2 credit;
- `stack_transfer_finality`: each saved signed transfer's identity and fee,
  its commitment and confirmed-ledger finality, the unchanged public DA bytes,
  and the exact sender and recipient deltas;
- `stack_withdrawal_payout`: each withdrawal of the transferred output, one
  payout included on Cardano with the exact destination and value, and the
  exact L2 debit;
- `stack_storage_identity` and `stack_settlement`: the database is this run's,
  and still holds one complete deposit job and exactly one confirmed payout,
  the journal's own, per cycle.

With `--mode fresh`, `stack_fresh_deployment` also requires that this run
directory's `attempts/` ran every command that creates the deployment, and
`stack_attempt_quality` reports any command that failed, timed out or was left
unfinished as recovery evidence.

No part of the stack runs a fault-proof family, a forced classification, or a
crash/rollback drill, and nothing writes the state-correction aggregate or its
independent sources. Without them the six state-correction gates are
`blocked` with reason "not run", so the verdict reads blocked, never
release-ready. Do not recreate hand-run deployment or journey steps, or build
drills into the stack, to change that. [review]

## What to run and report

The deterministic parser and gate rehearsal submits nothing and touches no
deployment. Run it when the finalizer or state-correction sources change:

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT/demo/midgard-node-tools"
NODE_ENV=emulator pnpm exec vitest run \
  tests/e2e-state-correction-acceptance.test.ts \
  tests/e2e-state-correction-reconciliation.test.ts \
  tests/e2e-state-correction-local-authority.test.ts
```

To re-derive a completed run, build the tooling and point the finalizer at
the stack configuration. `--stack-config` must be an absolute path:

```bash
pnpm --dir "$REPO_ROOT/demo/midgard-node-tools" run build
cd "$REPO_ROOT"
node demo/midgard-node-tools/dist/index.js e2e-finalize-summary \
  --stack-config "$STACK_CONFIG" --mode fresh --out-dir <summary directory>
```

Use `--mode fresh` only for the run that created the deployment; an attached
or resumed run passes `--mode attach` or `--mode resume`. Report the stack
gates from `summary.md`, name the state-correction gates above as not run, and
never report a green journey as release readiness.

## Finalizer inputs

The finalizer takes:

- `--stack-config`, `--mode`, `--out-dir`, `--node-log` and
  `--admin-api-key-env`;
- optional `--stress-summary` (see [benchmark.md](benchmark.md));
- `--state-correction-evidence`, `--state-correction-deployment-manifest`,
  `--state-correction-blueprint`, `--state-correction-catalogue`,
  `--state-correction-parameters` and `--state-correction-final-snapshot`; and
- repeatable `--state-correction-workflow-journal`,
  `--state-correction-l1-observation` and
  `--state-correction-recovery-observation`.

It builds its live authority from the stack's loopback `L1_KUPO_KEY` and
`L1_OGMIOS_KEY` and the stack's database, and refuses provider failover or a
remote endpoint.
