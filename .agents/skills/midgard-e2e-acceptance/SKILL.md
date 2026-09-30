---
name: midgard-e2e-acceptance
description: Run, attach, resume, or diagnose the Midgard live Preprod acceptance run through the one-command `e2e-stack` harness, and assess what it does and does not prove. Use for fresh or interrupted Preprod deployments with local Kupmios, reference scripts, operator registration, the DA committee and public retained DA, the watcher, deposits, L2 transfers, automatic merge and finality, withdrawals and payouts, stop-message recovery, the release-readiness gates, and opt-in throughput checks.
---

# Midgard E2E Acceptance

Treat this as production L2 acceptance. Preserve deployment identity, durable
state and every saved record. One command, `e2e-stack`, deploys, attaches,
resumes and runs the wallet journeys; there is no second, hand-driven flow.

## Required reading

Before any state-changing command, read:

- root `AGENTS.md`;
- `demo/AGENTS.md`;
- `docs/agents/production-l2.md`;
- `docs/agents/state-reset.md`;
- `docs/agents/transaction-finalization.md`;
- `demo/midgard-node/AGENTS.md`; and
- `demo/midgard-node-tools/docs/PREPROD_STACK.md` (configuration, secrets,
  ports, funding and the saved-record model).

Then run the skill currency check from the repository root:

```bash
node .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs
```

If it fails, repair the runbook or use current source help before operating a
live deployment. Never improvise past a stale command, a missing evidence
gate, or a deployment-identity mismatch. [review]

## Choose one run mode

Every mode is the same command with the same configuration file. Record the
mode and reason before running it:

1. **Fresh**: a new on-chain identity (nonce, reference scripts, `init`) on
   fresh local storage. Use a separate linked worktree; the stack refuses to
   reuse populated storage. [runtime: assertPreservedStorage]
2. **Attach**: a complete deployment exists for this configuration. The stack
   verifies it against Cardano and local storage and skips every confirmed
   step. It never runs `init` again. [runtime: initializationRecovery]
3. **Resume**: an earlier run stopped. The stack reconciles each unfinished
   step against Cardano and its saved records before repeating anything.
4. **Post-init diagnosis**: the stack stopped with a message. Read the message
   and route it through [references/recovery.md](references/recovery.md)
   before rerunning. Never run a state-changing node command by hand to get
   past it. [review]

Provider, wallet, DA, projection, scheduler, lease and evidence failures are
not redeploy triggers. Use a fresh deployment only when requested or required
by `docs/agents/state-reset.md`. [review]

## Route to the relevant reference

- Read [references/live-acceptance.md](references/live-acceptance.md)
  completely before any fresh, attach or resume run.
- Read [references/recovery.md](references/recovery.md) completely when the
  stack stops, a run was interrupted, or a service is unhealthy.
- Read [references/release-readiness.md](references/release-readiness.md)
  when asked whether a run proves release readiness, fault proofs, rollback
  recovery or state correction.
- Read [references/benchmark.md](references/benchmark.md) only when the user
  explicitly requests stress or throughput evidence. The journey does not run
  stress.

## Hard rules

- Drive deployment, operator, DA, deposit, transfer and withdrawal work only
  through `e2e-stack`. Hand-run node commands bypass its journal, locks and
  identity checks. [review]
- Pass `--config` as an absolute path, and keep the same configuration file
  for attach and resume. A changed identity field stops the run.
  [runtime: runStackController]
- The configuration must name Preprod, the `preprod-testing` profile, local
  Kupmios with no failover, `RUN_GENESIS_ON_STARTUP=false`, the exact L2 fees,
  and a Postgres host port other than 5433 and 55433; setup refuses anything
  else. [runtime: loadStackConfig]
- Never wipe local durable state, the run directory or service volumes under a
  deployment. [review]
- Do not delete `demo/midgard-node/cardano/db` or `cardano/kupo`; they are the
  local Preprod provider state, not a deployment reset target. [review]
- Never pass `--fresh-redeploy` to the node yourself unless
  [references/recovery.md](references/recovery.md) routes a dead signed nonce
  there and the owner has chosen to replace the deployment identity. [review]
- Manage the running stack only through `scripts/operator-compose.sh` with the
  generated `<runDirectory>/services/compose.json` override. Do not use the
  node README's plain `docker compose ... up` on a stack node directory.
  [review]
- Do not use manual SQL rewrites, manual `/merge`,
  `reconcile merge-complete --repair`, local-only finalization, or disabled
  local UPLC evaluation to make a run pass. [review]
- Preserve `attempts/`, `stack-journal.json` and every receipt. Report compact
  failure summaries with artifact paths, never secrets or large bodies.
  [review]
- Keep the runbook and the CLIs in step: the runbook validator checks that
  every documented command and `e2e-stack` flag is declared, and CI runs it.
  Blind spot: it checks names, not what a command does.
  [ci: repo-tools-ci/Validate the e2e acceptance runbook]

## Lower-layer feedback gate

For transaction builders, wallet/input selection, validity, workers, DA, or
recovery changes, run the relevant workspace checks before a live run:

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT/demo"
pnpm run test:tx-prep:sdk
pnpm run test:tx-prep:node
pnpm run test:tx-prep:emulator
```

For changes to the stack harness itself, run its focused tests:

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
MIDGARD_SKIP_DB_TESTS=1 pnpm --dir "$REPO_ROOT/demo/midgard-node-tools" \
  exec vitest run tests/full-stack
```

If a live run finds a deterministic defect, stop repeated live runs. Add a
targeted local or emulator regression, fix it, rerun the lower layer, then
return to the live run.

## Acceptance contract

A wallet-journey run is complete only when:

- the command exits 0 and `<runDirectory>/journey-summary.json` has
  `result: "wallet-journey-complete"` and the configured `cycles`;
- every step in `stack-journal.json` is `complete`, including each
  `cycle-N-deposit`, `cycle-N-transfer` and `cycle-N-withdrawal`;
- each cycle's receipts show exact L2 credits and debits including fees, a
  committed transfer retrieved through public libp2p DA, confirmed-ledger
  finality, and the exact L1 payout destination and value; and
- the stack reached readiness: node `/readyz` without reasons, healthy
  automatic settlement, the watcher live and ready with its launch scope
  complete, and every DA committee member ready.

The journey proves the functional deployment, DA, finality and payout path.
It is not release readiness: the fault-proof, state-correction and
crash/rollback gates are described in
[references/release-readiness.md](references/release-readiness.md), and an
`e2e-stack` run does not produce their evidence. Report release readiness as
not run unless that reference was followed. [review]

A run that needed recovery is still recovery evidence. Report the stops, the
diagnosis and the rerun alongside the final result. [review]

## Before handoff

Rerun:

```bash
node .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs
node --test .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.test.mjs
# Locate quick_validate.py in the installed skill-creator skill first.
python3 "$SKILL_CREATOR_DIR/scripts/quick_validate.py" \
  .agents/skills/midgard-e2e-acceptance
```

Also run the narrow Midgard tests for any source behavior changed alongside the
skill. A documentation-only update still requires the runbook validator,
frontmatter validator, formatting check, and review against the current CLI
help and source.
