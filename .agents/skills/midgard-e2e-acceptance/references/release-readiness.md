# Release-Readiness Gates

Read this reference when asked whether a live run proves release readiness,
fault proofs, state correction or crash/rollback recovery. An `e2e-stack` run
proves the functional deployment, DA, finality and payout path. It does not
produce the evidence these gates need.

## Contents

1. [What the gates require](#what-the-gates-require)
2. [Why an e2e-stack run cannot satisfy them](#why-an-e2e-stack-run-cannot-satisfy-them)
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

## Why an e2e-stack run cannot satisfy them

- Mode `fresh` requires step summaries with the IDs in
  `REQUIRED_FRESH_E2E_STEP_IDS` (for example `hub-oracle-nonce`,
  `init-protocol`, `await-automatic-merge`) and the transaction labels in
  `REQUIRED_FRESH_TRANSACTION_LABELS`. The stack's journal and `attempts/`
  records use its own step IDs and are not in that shape.
- The finalizer expects exactly one consumed deposit and exactly two accepted
  L2 transactions, a baseline the stack's cycles do not match.
- No part of the stack runs a fault-proof family, a forced classification, or
  a crash/rollback drill, and nothing writes the state-correction aggregate or
  its independent sources.

So the finalizer cannot pass on an `e2e-stack` run. Do not recreate hand-run
deployment or journey steps to feed it: the one-command stack is the only
live flow. Whether the finalizer is ported onto the stack's records or
retired is an owner decision. [review]

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

For a live run, report release readiness as **not run**, name the gates above
as outstanding, and state which functional evidence the stack's
`journey-summary.json` does provide. Never report a green journey as release
readiness.

## Finalizer inputs

For reference when the finalizer is ported or retired, it takes:

- `--mode`, `--run-id`, `--out-dir`, `--node-url`, `--node-log`;
- repeatable `--step-summary` and `--tx <label:txHash:status:source>`;
- optional `--stress-summary` (see [benchmark.md](benchmark.md));
- `--state-correction-evidence`, `--state-correction-deployment-manifest`,
  `--state-correction-blueprint`, `--state-correction-catalogue`,
  `--state-correction-parameters` and `--state-correction-final-snapshot`; and
- repeatable `--state-correction-workflow-journal`,
  `--state-correction-l1-observation` and
  `--state-correction-recovery-observation`.

It builds its live authority from `L1_PROVIDER=Kupmios`, the loopback
`L1_KUPO_KEY` and `L1_OGMIOS_KEY`, and the node database, and refuses provider
failover or a remote endpoint.
