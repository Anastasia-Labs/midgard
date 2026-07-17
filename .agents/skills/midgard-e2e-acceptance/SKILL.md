---
name: midgard-e2e-acceptance
description: Run, resume, diagnose, or assess merge and release readiness for the Midgard demo-node live end-to-end acceptance flow. Use for fresh or interrupted Preprod deployments, local Kupmios, reference scripts, operator lifecycle, libp2p DA publication and attestation, deposits, L2 transfers, automatic merge/finality, structured evidence, and bounded opt-in throughput checks.
---

# Midgard E2E Acceptance

Treat this as production L2 acceptance. Preserve deployment identity, durable
state, transaction evidence, and the distinction between functional recovery
and a clean first-attempt run.

## Required reading

Before any state-changing command, read:

- root `AGENTS.md`;
- `docs/agents/production-l2.md`;
- `docs/agents/state-reset.md`;
- `docs/agents/transaction-finalization.md`; and
- `docs/agents/midgard-node.md`.

Then run the skill currency check from the repository root:

```bash
node .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs
```

If it fails, repair the runbook or use current source help before operating a
live deployment. Never improvise past a stale command, missing evidence gate,
or deployment-identity mismatch.

## Choose one run mode

Record the mode and reason before changing state:

1. **Attach**: a complete matching deployment exists. Do not run `init` or
   reset durable state. Verify manifest, one-shot, reference scripts, provider,
   operator, DB route, `/healthz`, and `/readyz`, then start `listen` or Docker.
2. **Resume**: a fresh deployment was interrupted before `init`, or a submitted
   post-init milestone has been reconciled. Preserve the same manifest,
   run-state, policy, one-shot, and submitted transaction identities.
3. **Post-init diagnosis**: a state-changing command may have submitted. Stop,
   reconcile chain/DB/run-state evidence, and classify the attempt before any
   retry.
4. **Fresh**: intentionally create a new on-chain identity, fresh reference
   scripts and `init`, and matching clean local state.

“Fresh redeploy” is a reason for mode `fresh`, not an
`e2e-finalize-summary --mode` value. The summary CLI accepts `fresh`, `attach`,
`resume`, or `unknown`.

Provider, wallet, DA, projection, scheduler, lease, and evidence failures are
not automatic redeploy triggers. Use a fresh deployment only when requested or
required by `docs/agents/state-reset.md`.

## Route to the relevant reference

- Read [references/live-acceptance.md](references/live-acceptance.md) completely
  for a fresh run or value-submitting attach/resume flow.
- Read [references/recovery.md](references/recovery.md) completely for an
  interruption, ambiguous submission, readiness failure, DA failure, or merge
  failure.
- Read [references/benchmark.md](references/benchmark.md) only when the user
  explicitly requests stress or throughput evidence. Functional E2E does not
  include stress by default.

## Hard rules

- Work from `demo/midgard-node` for operational commands.
- `listen` and Docker startup attach; `init` bootstraps.
- Never wipe local durable state without a full fresh on-chain deployment.
- Never attach value-submitting flows to an old deployment after local state was
  wiped.
- Do not delete `demo/midgard-node/cardano/db` or `cardano/kupo`; they are the
  local Preprod provider state, not the Midgard deployment reset target.
- Use a fresh funded operator UTxO for `HUB_ORACLE_ONE_SHOT_*`. Patch it before
  reference-script publication and keep the same identity through `init`.
- Build `onchain/aiken/plutus.json` with `aiken build --env testnet` before a
  fresh demo/Preprod image build.
- Publish node-runtime reference scripts before `init` and preserve the
  deployment run-state used to create their auth policy.
- Keep `RUN_GENESIS_ON_STARTUP=false`; run explicit `init` through
  `e2e-run-step`.
- Use local Docker Kupmios only. Require `L1_PROVIDER=Kupmios`, local Kupo and
  Ogmios endpoints, and no `L1_PROVIDER_FAILOVER`.
- Pass `--wallet-seed-phrase-env USER_SEED_PHRASE` on user-wallet commands.
  Do not use a default `USER_WALLET` source.
- Run DB-backed host commands with
  `POSTGRES_HOST=127.0.0.1 POSTGRES_PORT=5433`, or run them inside the node
  container.
- `/tx-status` accepts `tx_hash`; the L2 submission response field is `txId`.
- Use the DA committee node and libp2p manifests for acceptance. Do not replace
  it with `attest-state-queue-once`.
- Keep the DA signer and L1 submitter roles explicit. Require a configured,
  funded `DA_L1_SUBMITTER_KEY_SOURCE`; never hardcode a local secret path in the
  runbook.
- Start enough DA committee listeners to meet threshold before the producer
  bind/listen preflight. Generate matching producer and watcher manifests from
  the finalized contract deployment manifest.
- Do not use manual SQL rewrites, manual `/merge`,
  `reconcile merge-complete --repair`, local-only finalization, or disabled
  local UPLC evaluation to make acceptance pass.
- Preserve raw logs. Report compact failure summaries with artifact paths rather
  than pasting secrets or large bodies.

## Lower-layer feedback gate

For transaction builders, wallet/input selection, validity, workers, DA, or
recovery changes, run the relevant layers in
`demo/midgard-node/docs/TX_PREP_FEEDBACK_LADDER.md` before live E2E:

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT/demo"
pnpm run test:tx-prep:sdk
pnpm run test:tx-prep:node
pnpm run test:tx-prep:emulator
```

If live E2E finds a deterministic defect, stop repeated live retries. Add a
targeted local or emulator regression, fix it, rerun the lower layer, then
return to live acceptance.

## Acceptance contract

For a fresh run, use `e2e-run-step` for every required milestone. The current
finalizer source is authoritative for required step IDs and transaction labels;
the runbook validator compares the documentation to that source.

Acceptance is complete only when:

- `e2e-finalize-summary` writes `summary.json` and `summary.md`;
- `functionalVerdict`, `cleanRunVerdict`, and `verdict` are `success`;
- `nextSafeAction` is `none_run_complete`;
- `required_fresh_steps`, `required_transaction_evidence`, and
  `required_fresh_step_attempt_quality` are satisfied for mode `fresh`;
- both baseline L2 transactions are committed;
- every transaction observation is reconciled, with no submitted, unknown,
  timed-out, or signaled attempt left ambiguous;
- DA payload publication, watcher verification, attestation init/add/apply, and
  automatic merge/finality evidence are present for every committed header;
- `/healthz` is healthy, `/readyz` is ready without reasons, and the DA watcher
  is healthy and ready;
- the state queue is empty; pending finalizations are finalized; volatile
  tables and unfinished mutation jobs are empty; and confirmed/immutable state
  reflects the run; and
- the final error scan has no unexplained error, failure, abandonment, crash, or
  hash mismatch.

A recovered run may reach functional success while `cleanRunVerdict` remains
failed or interrupted. Report that honestly as recovery evidence; do not call
it a clean acceptance run.

## Before handoff

Rerun:

```bash
node .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs
python3 /mnt/c/Users/phili/.codex/skills/.system/skill-creator/scripts/quick_validate.py \
  .agents/skills/midgard-e2e-acceptance
```

Also run the narrow Midgard tests for any source behavior changed alongside the
skill. A documentation-only update still requires the runbook validator,
frontmatter validator, formatting check, and review against the current CLI
help/source.
