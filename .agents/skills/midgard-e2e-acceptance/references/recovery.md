# Recovery and Failure Routing

Read this reference completely before retrying an interrupted or ambiguous live
step.

## Contents

1. [Preserve evidence](#preserve-evidence)
2. [Classify the attempt](#classify-the-attempt)
3. [Reconciliation commands](#reconciliation-commands)
4. [Failure routing](#failure-routing)
5. [Recovery completion](#recovery-completion)

## Preserve evidence

Before restarting a process or command, record:

- selected run mode and reason;
- every attempted step ID, status, summary path, and raw log path;
- every observed transaction hash and whether it is prepared, submitted,
  confirmed, committed, rejected, or unknown;
- deployment manifest path, manifest ID/hash, network, hub-oracle one-shot,
  reference-script policy ID and UTxO count;
- deployment run-state path and identity;
- provider and DB routes;
- operator, reference-script, merge, user, DA signer, and L1 submitter addresses
  or hashes, never credentials;
- producer and watcher manifest paths and hashes;
- watcher PID, log, store, health/readiness, and per-header status; and
- state queue, mutation lease, pending finalization, scheduler, and unfinished
  local mutation-job evidence.

Do not delete a raw log because a retry produced a cleaner one. Supply all
attempt summaries to the final dashboard so clean-run quality remains honest.

## Classify the attempt

Choose exactly one classification:

1. **Safe before submit**: the runner and raw log prove no transaction was
   signed or submitted. Fix the local cause and rerun with a new attempt log.
2. **Submitted, reconcile before retry**: a hash exists or submission may have
   occurred. Query provider/chain and use the matching reconciliation command.
3. **Wait for visibility**: provider confirmation, inclusion-time projection,
   finality depth, DA propagation, lease ownership, or a valid protocol window
   has not matured. Wait against authoritative evidence.
4. **Attach/resume**: deployment identity is complete and matches local durable
   state. Continue without `init` or reset.
5. **Fresh required**: local durable state was lost or the one-shot, manifest,
   reference-script policy, and on-chain deployment can no longer be proven to
   match. Follow `docs/agents/state-reset.md` and create a complete fresh
   identity.

A timeout or signal with no transaction observation is ambiguous for a
transaction-bearing step. Do not classify it as safe-before-submit without raw
evidence.

## Reconciliation commands

Run from `demo/midgard-node`. Add `--repair` only where shown and only after the
read-only result proves the repair is appropriate.

```bash
node dist/index.js reconcile phas-registered --json
node dist/index.js reconcile reference-scripts-complete \
  --scope node-runtime \
  --json
node dist/index.js reconcile deployment-manifest \
  --out deploymentInfo/contract-deployment-info.json \
  --init-tx-hash <confirmed-init-tx-hash> \
  --json
node dist/index.js reconcile deposit-projected \
  --event-id <event-id> \
  --json
node dist/index.js reconcile tx-committed \
  --tx-hash <l2-tx-hash> \
  --json
node dist/index.js reconcile da-attested \
  --header-hash <header-hash> \
  --watcher-url http://127.0.0.1:8787 \
  --contract-deployment-info \
  deploymentInfo/contract-deployment-info.json \
  --json
node dist/index.js reconcile block-committed \
  --header-hash <header-hash> \
  --json
node dist/index.js reconcile-deposit-submission \
  --tx-hash <deposit-tx-hash> \
  --json
```

Where a read-only reconciliation result explicitly supports it, rerun that same
command with `--repair`. Never run `reconcile merge-complete --repair` for
acceptance. The running merge fiber must prove liveness.

## Failure routing

| Blocker                                                | Safe next action                                                                                                                                                           |
| ------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Local Kupmios unhealthy or failover configured         | Run `l1-provider-preflight --json`; repair local Cardano node, Ogmios, or Kupo. Do not switch to a remote provider.                                                        |
| Fresh one-shot mismatch                                | Stop. Attach only if the existing initialized identity is complete; otherwise restart the full fresh deployment with a new one-shot.                                       |
| Reference-script publication interrupted before `init` | Preserve run-state/logs and resume only when network, one-shot, manifest, and auth policy match.                                                                           |
| Manifest/reference-script mismatch after `init`        | Stop and diagnose identity drift. Fresh deploy only when state-reset rules require it.                                                                                     |
| Operator registered but not active                     | Prove exactly one matching registered node and no active node; then use `activate-operator`.                                                                               |
| Deposit submission timeout                             | Run `reconcile-deposit-submission`; do not resubmit an `ambiguous` result.                                                                                                 |
| Deposit not projected                                  | Inspect `deposits_utxos.inclusion_time`; wait until due, then run projection once.                                                                                         |
| Watcher wallet preflight fails                         | Fund or correct the distinct configured L1 submitter wallet. Do not substitute the operator wallet silently.                                                               |
| DA watcher below threshold                             | Start or repair enough matching committee listeners. Do not lower threshold.                                                                                               |
| Producer bind/listen preflight fails                   | Stop stale producer instances; regenerate matching host-preflight/runtime manifests; rerun before node startup.                                                            |
| DA payload missing                                     | Inspect producer publication rows, retained payload, pending finalization, and watcher logs. Use `db:backfill-da-payloads --header-hash` only as explicit repair evidence. |
| DA payload root mismatch, malformed data, or conflict  | Stop. Do not merge. Fix payload construction, deployment identity, or endpoint/peer set.                                                                                   |
| State-queue lease blocks merge                         | Inspect `/stateQueueMutationLease` and unfinished mutation jobs; wait or repair the actual owner.                                                                          |
| Scheduler/commit validity expired                      | Rebuild through the worker path with fresh timing; never reuse expired signed transactions or pending rows.                                                                |
| Automatic merge wait times out                         | Inspect readiness, queue, lease, scheduler, finalization, DA status, and merge logs. Fix the merge fiber; do not call `/merge`.                                            |
| Hash mismatch                                          | Preserve raw body/log separately; report command, tx hash, expected/actual hash, script policy/outref, and artifact path.                                                  |
| Local state wiped under an old deployment              | Stop all value-submitting activity and perform a complete fresh on-chain deployment or restore provably matching durable state.                                            |

## Recovery completion

After recovery:

- rerun the affected reconciliation and health/readiness checks;
- add both failed/interrupted and recovery step summaries to
  `e2e-finalize-summary`;
- reconcile every transaction observation;
- use summary mode `resume`, unless this became a complete new deployment, in
  which case use `fresh` and retain all attempt evidence; and
- report functional and clean-run verdicts separately.

Recovery is complete only when the normal acceptance contract is satisfied. A
working endpoint by itself does not close an ambiguous transaction, DA, or
finality failure.
