# Operations alerts

Each alert in the node's and the watcher's Prometheus rules has one section
here: what it means, what to check first, what to run, and when to escalate.
The rules live beside the services they watch:

- [`demo/midgard-node/alert-rules.yml`](../../demo/midgard-node/alert-rules.yml)
  reads the node's metrics. The node exports them only when started with
  `listen --with-monitoring` (port 9464, scraped by
  [`prometheus.yml`](../../demo/midgard-node/prometheus.yml), which loads the
  rules).
- [`demo/midgard-watcher/alert-rules.yml`](../../demo/midgard-watcher/alert-rules.yml)
  reads the watcher's `GET /metrics`. That endpoint is on the watcher's
  loopback operations server (`operationsEndpoint` in the watcher process
  configuration), so the Prometheus that loads these rules runs on the
  watcher's host.

Thresholds and `for` durations are starting values; tune them to the
deployment's block cadence. In the commands below, `$NODE` is the node's HTTP
address and `$OPS` the watcher's `operationsEndpoint` (for example
`http://127.0.0.1:7402`).

An alert names a symptom; the node's `GET /readyz` and the watcher's
`GET /v1/status` name the cause. Read those before acting. On the public
deployment profiles (`preprod-public`, `mainnet`) the node's `/readyz` keeps
each detail's code and drops its free text; the node log carries the rest.

## Node alerts

### MidgardCommitLag

**Meaning.** Work is waiting to be committed (`midgard_commit_pipeline_idle`
is 0), but the end time of the node's latest local block
(`midgard_local_block_end_time_ms`) is more than ten minutes old. Users'
transactions are not reaching blocks.

**First checks.**

```bash
curl -s "$NODE/readyz"
```

Read `reasons`, `livenessReasons`, `blockCommitmentCoordination`,
`commitPlanningProgress`, `commitDaFramePressure` and `l1Follower`. A named
liveness reason or follower hold is the cause; the commit worker waits on it.

**Action.** Resolve the named reason (an L1 provider, Postgres or DA committee
outage clears by itself once the dependency is back). Do not edit the
database.

**Escalate** when `/readyz` names no reason that explains the lag, or the lag
persists after the named dependency has recovered.

### MidgardCommitPlanningCatchUp

**Meaning.** For 30 minutes the commit planner has selected catch-up plans
(`midgard_commit_planning_catch_up` is 1; it reads 0 while the commit pipeline
is idle or once the last plan is older than the age limit `/readyz` applies to
it): the L1 event backlog (deposits,
withdrawals, forced orders) exceeds one block, so each block commits a
window of events that stops short of the block time and includes no ordinary
L2 transactions until the backlog clears.

**First checks.** `commitPlanningProgress.planned` in `GET /readyz` shows the
plan: `mode`, `selectedEndTimeMs`, `eligibleEventCountLowerBounds` and
`selectedEventCounts`. Check that consecutive readings advance
`selectedEndTimeMs`.

**Action.** None while the window advances: catch-up ends by itself. The
node's error log names `catch_up_da_frame_hold` or
`catch_up_instant_exceeds_cap` when a single event instant cannot fit a
block.

**Escalate** when the selected window stops advancing or either log alarm
appears.

### MidgardForcedAdmissionDivergence

**Meaning.** The node's off-chain admission screen refused a forced order the
chain admitted (`forced_admission_divergence_total` increased, labelled
`refusal_code`). The on-chain check and its off-chain twin disagree. The
order is still ingested and classified, and the node stays ready.

**First checks.** `GET /readyz` lists the latest divergent orders as
`forced_admission_divergence:<orderIdHex>:<refusalCode>` details, and the
node logs one `ForcedAdmissionDivergence` error event per order.

**Action.** Record the order id, refusal code and log event. Do not change
node state.

**Escalate** every occurrence to the protocol developers, with the order id,
refusal code and deployment manifest.

### MidgardL1EventRefusals

**Meaning.** The node's L1 follower leaves L1 events out of ingestion
(`midgard_l1_event_refusals`, labelled `reason`):

- `l1_event_undecodable`: an event that does not decode into the node's row.
  The node stays ready; `/readyz` names the count as a detail.
- `l1_event_identity_conflict`: an event whose public id has a local row
  under another admission. It is a follower hold until a chain change lifts
  it.

Both read 0 while the follower is not running; `/readyz` then names the
follower's own state.

**First checks.** `l1Follower.readiness` in `GET /readyz`, and the node log
line `event refused (<reason>): ...` for each event.

**Action.** None in node state: the follower re-reads on every chain change.

**Escalate** any `l1_event_undecodable` (a decoder disagrees with the chain),
and an `l1_event_identity_conflict` that survives past L1 finality.

### MidgardSettlementJobsStuck

**Meaning.** One or more settlement jobs reached the retry cap of consecutive
failures (`midgard_settlement_jobs_stuck`). A stuck job is retried once an
hour, so a long outage still clears without an operator. Block production is
unaffected.

**First checks.** `GET /readyz` details carry
`settlement_jobs_stuck:<count>:<kind>:<eventId>:<phase>:<lastError>` for the
earliest-due job.

**Action.** Fix what `lastError` names (provider, wallet funds, Postgres).
The next hourly retry clears the job; a success resets its count.

**Escalate** when the job fails again after its cause is fixed.

### MidgardSettlementReconcileStuck

**Meaning.** The node cannot read the outcome of a submitted settlement
transaction: its reconcile failed up to the retry cap
(`midgard_settlement_reconcile_stuck` is 1). New settlement work waits behind
it; the reconcile keeps polling.

**First checks.** The `GET /readyz` detail
`settlement_reconcile_stuck:<txHash>:<lastError>`; look the transaction up on
an L1 explorer or the node's own provider.

**Action.** Restore the provider the reconcile reads. Settlement resumes
within five minutes of the provider answering.

**Escalate** when the provider answers but the reconcile keeps failing.

## Watcher alerts

### MidgardWatcherReplayRootMismatch

**Meaning.** The watcher's replayed ledger does not open the root of the L1
confirmed state (or does not start from its parent's root). The
`root_mismatch` alert is active, the watcher holds its replay checkpoint
(`/readyz` names `replayed_ledger_checkpoint_pending` and `active_alert`), and
the watcher logs one `replayed_ledger_root_mismatch` error per confirmed
head. The alert clears
once the ledger advances to the confirmed state again.

**First checks.**

```bash
curl -s "$OPS/v1/status"
curl -s "$OPS/v1/diagnostics?kind=alert"
```

`replayRefusals.rootMismatch` in `/v1/status` names the header hash, the kind
(`root-mismatch` or `parent-mismatch`) and the detail. Check `l1Readiness`
and the L1 source status: a follower still catching up or a rollback in
progress resolves by itself.

**Action.** Keep the watcher running and preserve its state directory. Do not
delete or edit the replay checkpoint.

**Escalate** at once when the alert lasts past the next confirmed-state
advance: the watcher's replay and the confirmed state disagree, and the
watcher cannot classify later blocks until they agree.

### MidgardWatcherConfirmedReplayRefused

**Meaning.** While moving its replay checkpoint to the confirmed state, the
watcher refused to replay a block (`replay-refused`), for example because
its payload could not be read as a block. The checkpoint stays held.

**First checks.** `replayRefusals.recent` in `GET /v1/status` and the
watcher's `replayed_ledger_refused` warning name the header and the detail.
Check retained-DA reachability (`retainedDaTransport`, `da_fetch`
diagnostics).

**Action.** Restore the retained-DA sources if they are failing; the next
advance retries.

**Escalate** when the refusal repeats for a header whose payload is reachable.

### MidgardWatcherBlockReplayMismatch

**Meaning.** A committed block's replay was refused while the watcher
classified it: its replay does not open the root its header claims, or does
not start from its parent's root. This is the path fault proofs exist for;
the counter only records that the watcher saw it.

**First checks.** `replayRefusals.recent` in `GET /v1/status` names the
header. Follow the proof in `GET /v1/diagnostics?kind=proof_step` and in
`supervisor` of `/v1/status`.

**Action.** None while the watcher proves the fault. The fault-proof alerts
below cover a proof that does not land.

**Escalate** when no proof objective appears for the header.

### MidgardWatcherL1EventRefusals

**Meaning.** The watcher's L1 follower refused user orders the on-chain
observer admitted (`l1_user_event_refused`), or could not read its refusals
(`l1_event_refusals_unreadable`). Every refused order was admitted on chain,
so a refusal is a disagreement between the decoder and the chain.

**First checks.** `l1Degradations` in `GET /v1/status` gives the count per
refusal reason and the newest refusal; `/readyz` names it until it is k deep
and pruned.

**Action.** None in watcher state.

**Escalate** every `l1_user_event_refused` to the protocol developers with the
newest refusal's detail.

### MidgardWatcherProofSubmissionFailure

**Meaning.** The watcher could not land a fault-proof transaction
(`proof_submission_failure` alert): a submission failed, an objective was
quarantined, or a funding reservation was orphaned.

**First checks.**

```bash
curl -s "$OPS/v1/diagnostics?kind=alert"
curl -s "$OPS/v1/diagnostics?kind=proof_step"
```

**Action.** Submission is automatic and retried by the watcher; fix the cause
the diagnostics name (prover wallet funds, L1 provider). See the
[challenger runbook](../fault-proofs/challenger-runbook.md#5-when-a-proof-does-not-land).

**Escalate** when the alert stays active after the cause is fixed.

### MidgardWatcherProofHeld

**Meaning.** The watcher holds a fault-proof objective; the `reason` label
names why:

| `reason`                              | What holds it                                                               | What clears it                               |
| ------------------------------------- | --------------------------------------------------------------------------- | -------------------------------------------- |
| `prover_funding_unavailable`          | The prover wallet cannot fund the next transaction                          | The next run after the wallet is topped up   |
| `fault_proof_continuation_stalled`    | A run journaled no progress within its continuation bound                   | A later run that journals progress           |
| `fault_proof_objective_quarantined`   | A run stalled or met an integrity error of its own; retried on a backoff    | The first run that meets something else      |
| `fault_proof_resubmission_required`   | The next action needs a fresh submission after the latest safe start passed | See escalation                               |
| `fault_proof_start_deadline_passed`   | The objective's latest safe start passed before it signed any attempt       | Its header leaving the finalized state queue |
| `prover_funding_orphaned_reservation` | A reservation no workflow will drive holds attempts not yet retired         | Its attempts retired past release finality   |
| `fault_proof_l1_refused:<reason>`     | The L1 source refused a read the proof needs (for example pruned history)   | The next run whose reads the source answers  |

**First checks.** `l1Readiness` and `supervisor.journalDecisionMissing` in
`GET /v1/status` give each held objective as `<category>/<headerHash>` with
its detail.

**Action.** Top up the prover wallet for `prover_funding_unavailable`.
For the others, keep the watcher running: every hold except the two
deadline-bound ones clears by itself.

**Escalate** `fault_proof_resubmission_required` and
`fault_proof_start_deadline_passed` at once: no command at this release grants
fresh submission authority to an objective whose safe start has passed.
Another independent challenger can still prove the same fault while the
challenge window is open (proving is permissionless). Escalate a
`fault_proof_l1_refused:<reason>` hold that persists across observations:
history the L1 source has pruned does not come back.

### MidgardWatcherProofDeadline

**Meaning.** A fault proof's deadline is at risk (`deadline_at_risk`) or can
no longer be met safely (`deadline_unsafe`). The watcher keeps running; a
restart would meet the same deadline.

**First checks.** `supervisor.deadlineHealth` in `GET /v1/status`
(`at_risk` or `unsafe`) and the objectives the supervisor lists.

**Action.** Remove what slows the proof (prover wallet funds, L1 provider
latency, retained-DA reachability).

**Escalate** `deadline_unsafe` at once.
