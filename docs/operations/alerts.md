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

### MidgardCommitSchedulerAlignmentFailing

**Meaning.** Work is waiting to be committed, but for ten minutes every commit
tick failed to align the L1 scheduler before the commit worker
(`midgard_liveness_reason_age_ms{source="commit_scheduler"}`). The node cannot
take its scheduler turn, or push an expired turn on to the next operator, so
its users' transactions are not reaching blocks. The node logs the same
failure at error level after ten minutes.

**First checks.** `GET /readyz` names the reason
`commit_scheduler_alignment_failed`; its `livenessReasons` entry carries the
first line of the last failure. Common causes: an operator wallet with no
plain ADA to pay the scheduler transaction, an L1 provider that cannot read
the scheduler or operator lists, or a registered operator due for activation
that blocks the scheduler's rewind.

**Action.** Fix what the detail names (fund the operator wallet, restore the
provider, or activate the registered operator). The next tick that aligns or
waits clears the reason. Do not edit the database.

**Escalate** when the detail names none of these, or the reason stays raised
after its cause is fixed.

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
`forced_admission_divergence:<orderIdHex>:<refusalCode>` details (a public
profile shows the code alone), and the node logs one
`ForcedAdmissionDivergence` error event per order.

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

### MidgardLocalLedgerDivergence

**Meaning.** The node's ledger entries (`confirmed_ledger` with the processed
landed deltas) do not derive the root it stores for them
(`midgard_local_ledger_divergence_total` increased, labelled `check`). Both
come from the node's own stores, so the fault is this node's, never a
block's. The `check` names where it was found:

- `landed_parent`, `own_parent`: the parent ledger a landed block (foreign or
  this node's own) is adopted on. The block is held as
  `landed_block_local_fault`, not judged invalid. After
  `LOCAL_FAULT_RUN_LIMIT` attempts in a row the node exits non-zero under that
  reason.
- `commit_base_landed`, `commit_base_confirmed_ledger`: the base a commit
  builds on. The commit is refused on every tick, each refusal counted; the
  node keeps running. A commit on a landed tail builds from the frontier at
  which the verification below last passed. It applies each landed delta
  after that frontier and checks it: the delta builds on the root before it,
  names each outref once and none both spent and produced, every output it
  spends is held, no output it produces is held already, and it changes the
  root exactly when it is not empty. A delta that fails is a
  `commit_base_landed` divergence.
- `confirmed_ledger`, `confirmed_ledger_tail`: the node's verification of
  the landed ledger, run off the commit path. It builds one trie from
  `confirmed_ledger` and checks its root against the frontier's stored root
  (`confirmed_ledger`). It then applies each processed landed delta up to
  the queue tail, first checked with the commit's checks, and checks the
  root it reaches against the tail's stored root (`confirmed_ledger_tail`;
  a delta that fails the commit's checks is a `confirmed_ledger_tail`
  divergence too). The two are checked and counted separately: a
  `confirmed_ledger` entry a later delta spends is covered only by the
  frontier's check, a delta byte only by the tail's. It runs at startup,
  after each fold or other write to `confirmed_ledger` (each draws a new
  frontier generation) and every ten minutes, so it also catches a row (of
  `confirmed_ledger` or a landed delta) changed outside a fold. Each check
  counts once per run that finds its divergence. A run reads
  `confirmed_ledger` a page at a time on one database snapshot, so its
  memory does not grow with the ledger. A pass names the frontier
  generation it read, and a clean fold by landed-block processing carries
  it to the folded block when that block's delta is the one the run
  checked. Each fold is logged, and a pass is replayed over the folds
  committed while its run was reading, so it reaches the current frontier
  without a second run. The log holds 256 folds; a fold past that drops it
  and makes a full run due, at most one in flight, which brings the pass to
  the current frontier. Replay keeps the pass current only while fewer than
  256 folds land during one run and each folded block was processed before
  the run read; otherwise commits on landed tails wait for the next run,
  which follows every frontier move. A pass from a run that read an older generation,
  and that its replay does not bring to the recorded generation, is
  ignored; a divergence it finds is counted and logged at warn, but raises
  nothing and leaves the recorded pass alone. While no pass holds at the
  current frontier and generation, commits on landed tails wait as
  `commit_base_ledger_unverified`. The reason clears when a later run
  passes, or when a fold or replay carries the pass there. A run that
  cannot complete raises
  `confirmed_ledger_verification_failed` under the same source instead,
  and counts nothing.

The node exports every `check` series at 0 from startup, so the first
divergence after a start shows as an increase. A fold, or an unfold on a
rollback, landing while a check reads does not fire it: the commit-base checks
read the ledger and its root on one database snapshot. A commit base on a
tail that landed-block processing has not processed yet waits as
`commit_base_pending`, not this alert.

**First checks.** `GET /readyz` names `landed_block_local_fault` (landed
blocks), `commit_base_ledger_divergence:<check>` (commits), or
`commit_base_ledger_divergence:confirmed_ledger` or `:confirmed_ledger_tail`
under `confirmed_ledger_verification` (the verification; when both diverge it
names `confirmed_ledger`, the frontier's, and the log and the counter carry
both). The node log carries
`local ledger divergence (<check>): the ledger at <headerHash> derives root
<derived>, its stored root is <stored>` (or, for a landed delta, `takes a
delta that <fault>` in place of the derived root). It appears under a
`LandedBlockLocalFault` error event or a `Commit refused on its base` error
line, one line per divergence a verification run found. Each verification
run also logs `confirmed_ledger verification at frontier <headerHash>
(generation <n>), tail <headerHash>: verified (entries=<count>, <ms> ms)` (or
`diverged (<check>, ...)` in place of `verified`). A pass replayed over folds
made during its run logs `confirmed_ledger verification at generation <n>
replayed <count> folds made during the run, to <headerHash> (generation
<n>)`, with `; stopped at <headerHash>: <fault>` when a fold did not apply
(at warn unless the run had not checked that fold's delta). A full fold log
logs `confirmed_ledger fold log past 256 folds: dropped, a full verification
run is due`.

**Action.** None in node state: the node neither adopts a block nor commits
on the diverged ledger, and nothing repairs it automatically. No known
transient fires it, so a restart does not clear it: the node reads the same
ledger and fails the same check, and after a landed-block exit it exits
again.

**Escalate** every occurrence to the protocol developers, with the check,
header hash, both roots and the deployment manifest.

### MidgardMergeFailures

**Meaning.** For ten minutes the node's merge attempts have kept failing
(`merge_failure_count` keeps rising). The counter counts an attempt that could
not read or fund from the merge wallet, failed its merge build, or refused the
oldest block's candidate check. Blocks still commit, but the state queue grows
and nothing reaches the confirmed state. A failed submission or confirmation
is not counted here; it shows in the log as `Submit tx error` or
`Confirm tx error`.

**First checks.** The node log around each attempt: an `E_MERGE_` code names
a refused candidate check or build; `No operator wallet inputs available to
fund state_queue merge tx` and `Failed to read the merge wallet view` name the
merge wallet. Read `midgard_wallet_plain_lovelace{role="merge"}` too (see
[MidgardWalletBalanceLow](#midgardwalletbalancelow)).

**Action.** Fund the merge wallet when it is empty, and restore the L1
provider or Postgres when the log names one. The next merge attempt picks the
change up.

**Escalate** any `E_MERGE_` code, and failures that continue once the merge
wallet holds funds and the provider answers.

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

### MidgardWalletBalanceLow

**Meaning.** A node role wallet holds less plain ADA (outputs with no token,
datum or script reference) than its transactions need for five minutes. The
`role` label names the wallet; the balance is
`midgard_wallet_plain_lovelace` and the requirement
`midgard_wallet_required_lovelace`. Both are read from the wallet view the
node builds its transactions from, every five seconds.

| `role`             | Requirement                                                                                                                                                                                      | Short of it                                             |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------- |
| `operator`         | The defense reserve [validation-trace-defense.md](../../demo/midgard-node/docs/validation-trace-defense.md) asks for (2 × 82.4 + 5 + 10 ADA) plus one commit's 5 ADA block node, about 184.8 ADA | Commits, or a validation-trace Defend, cannot be funded |
| `merge`            | No requirement of its own; the 5 ADA collateral a merge presents (the collateral floor)                                                                                                          | Merges fail; see MidgardMergeFailures                   |
| `settlement`       | No requirement of its own; the 5 ADA collateral a settlement transaction presents (the collateral floor). Exported only when the node has a settlement wallet                                    | Settlement jobs fail; see MidgardSettlementJobsStuck    |
| `reference_script` | None: publication tops it up from the operator wallet                                                                                                                                            | No rule                                                 |
| `defense_pocket`   | None: an empty pocket is its normal state, and a Defend pays it its collateral output                                                                                                            | No rule                                                 |

The `reference_script` and `defense_pocket` balances are exported for reading
only; the rule never fires for them.

**First checks.**

```bash
curl -s "$NODE/readyz"
```

A wallet that cannot fund pending work also shows there (for example
`validation-trace-defense-underfunded` with its shortfall).

**Action.** Send plain ADA to the named role's wallet. The alert clears on the
first sample after the funds land in the wallet view.

**Escalate** when the balance keeps falling after a top-up with no matching
commits, merges, settlements or Defends in the node log.

## Watcher alerts

Some alerts in the watcher's `GET /v1/status` (`activeAlerts`) are kept one
subject per state-queue header: `da_fetch_failure` (a retained-DA fetch for the
header failed), `validation_trace_replay_stuck` (a script re-execution in the
header keeps failing the same way), `fault_detector_failed` and
`proof_family_coverage_gap`. Each clears by itself, with no restart or operator
action, when the header gets its own outcome or leaves the observed state
queue, including when an L1 rollback drops the header: each decision pass
clears every one of them for each header that is no longer queued. For
`da_fetch_failure`, a header still counts as queued while availability holds
it pending at release depth: the watcher still fetches its DA after a merge
or removal at the tip, until that reaches release depth.
`unproven_fault_merged` is also one subject per header, but it never clears
([MidgardWatcherUnprovenFaultMerged](#midgardwatcherunprovenfaultmerged)).
`da_fetch_failure` and `validation_trace_replay_stuck` have no rule of their
own: a `da_fetch_failure` holds readiness for 180 seconds after its latest
failed fetch, and `validation_trace_replay_stuck` never holds readiness.

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
quarantined, or a funding reservation was orphaned. A quarantine's alert
clears once the objective leaves quarantine: a run meets another outcome, or
the objective settles, including when its header leaves the observed queue
between retries. A settled objective is not retried.

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

| `reason`                              | What holds it                                                                | What clears it                               |
| ------------------------------------- | ---------------------------------------------------------------------------- | -------------------------------------------- |
| `fault_proof_continuation_stalled`    | A run journaled no progress within its continuation bound                    | A later run that journals progress           |
| `fault_proof_objective_quarantined`   | A run stalled or met an integrity error of its own; retried on a backoff     | Another outcome, or its objective settling   |
| `fault_proof_resubmission_required`   | The next action needs a fresh submission under reconciliation-only authority | See escalation                               |
| `fault_proof_start_deadline_passed`   | The objective's header matured before it signed any attempt                  | Its header leaving the finalized state queue |
| `prover_funding_orphaned_reservation` | A reservation no workflow will drive holds attempts not yet retired          | Its attempts retired past release finality   |
| `fault_proof_l1_refused:<reason>`     | The L1 source refused a read the proof needs (for example pruned history)    | The next run whose reads the source answers  |

**First checks.** `l1Readiness` and `supervisor.journalDecisionMissing` in
`GET /v1/status` give each held objective as `<category>/<headerHash>` with
its detail.

A pause on prover funding pages
[MidgardWatcherProverFundingUnavailable](#midgardwatcherproverfundingunavailable)
instead.

**Action.** Keep the watcher running: every hold except the two
deadline-bound ones clears by itself.

**Escalate** `fault_proof_resubmission_required` at once: a signed objective
holds reconciliation-only authority at or past its header's maturity, and
while it is not the watcher's current proof target, and no command at this
release grants it fresh submission authority. Before maturity the current
target keeps submitting, late. Another independent challenger can
still prove the same fault while the challenge window is open (proving is
permissionless). `fault_proof_start_deadline_passed` means the header matured
unproven: no removal can land any more. If the header merged, its block is in
confirmed state and
[MidgardWatcherUnprovenFaultMerged](#midgardwatcherunprovenfaultmerged) fires;
escalate as that section says. Escalate a
`fault_proof_l1_refused:<reason>` hold that persists across observations:
history the L1 source has pruned does not come back.

### MidgardWatcherProverFundingUnavailable

**Meaning.** A fault-proof objective is paused because the prover wallet
cannot fund its next transaction (`prover_funding_unavailable` alert, one
subject per objective). The watcher keeps observing the objective; the
readiness reason `prover_funding_unavailable` names it as
`<category>/<headerHash>` with the funding error. The alert clears once a run
meets something else, as the first run after a top-up does, or once the
objective settles.

**First checks.** `l1Readiness` in `GET /v1/status`, and the prover wallet's
plain-ADA coins (a coin holding a token is never spent; one reserved pool of
coins funds a transaction and posts its collateral).

**Action.** Top up the prover wallet with plain-ADA coins at once: the proof
still has to land before its header matures, and `supervisor.deadlineHealth`
and `remainingSafeStartMs` count the paused objective.

**Escalate** when the alert stays active after a top-up, or when
[MidgardWatcherProofStartedLate](#midgardwatcherproofstartedlate) fires with
it.

### MidgardWatcherProverCollateralForfeited

**Meaning.** A fault-proof attempt landed on L1 failing phase 2, so the chain
took the collateral it posted (`prover_collateral_forfeited` alert, one
subject per objective). The watcher evaluated the scripts locally before it
submitted, so local evaluation and the chain disagreed. The watcher accounts
for it at the tip: the objective's journal records the attempt and the
collateral taken, the reservation drops those coins and holds the collateral
return instead, and the workflow goes on with a re-signed attempt. The watcher
log has one error line per such attempt, naming the objective, the attempt
and the collateral. A rollback that drops the landing undoes all of it and
clears the alert; so does the objective settling. The alert is informational:
it does not hold readiness.

**First checks.**

```bash
curl -s "$OPS/v1/diagnostics?kind=alert"
```

The `prover_collateral_forfeited` log line names the attempt's transaction;
look it up on an L1 explorer to see which script failed.

**Action.** Keep the watcher running and preserve its state directory. Top up
the prover wallet if the loss leaves it short
([MidgardWatcherProverFundingUnavailable](#midgardwatcherproverfundingunavailable)).

**Escalate** always: a script that passed local evaluation and failed on
chain is a defect in the watcher, its cost model or its protocol parameters.

### MidgardWatcherProverCollateralAccountingFailed

**Meaning.** The watcher could not record, in an objective's prover
reservation, the collateral that an attempt's phase-2-invalid landing took,
or give it back after a rollback dropped that landing
(`prover_collateral_accounting_failed` alert, one subject per objective). The
reservation's pool may disagree with the chain until it succeeds. The
objective's journal records the failure once; the watcher reads the attempt
again later and retries, and the objective goes on meanwhile. The watcher log
has one error line per failure, naming the objective, the attempt and the
error. The alert clears once every such attempt is accounted for, a later
read leaves nothing to account, or the attempt retires or lands as its
action's result; so does the objective settling. It holds readiness while it
stands.

**First checks.**

```bash
curl -s "$OPS/v1/status"
curl -s "$OPS/v1/diagnostics?kind=alert"
```

The `prover_collateral_accounting_failed` log line names the error. A provider
or L1 source that did not answer, a chain that moved under the read and a busy
database never raise this alert: the watcher retries those as it retries any
other read. Any other L1 read failure clears on a later retry once the provider
answers.

**Action.** Keep the watcher running and preserve its state directory.

**Escalate** when the alert stays active across retries: any other error is a
defect in the watcher's reservation accounting.

### MidgardWatcherProofStartedLate

**Meaning.** A fault proof ran after its header's latest safe start
(`maturity_deadline_risk` alert), whether it started then or signed a step
before. The latest safe start is a margin: the state queue accepts a
completed fraud-proof removal until the header matures, so until then the
watcher keeps every right an on-time run has: a fresh submission for each
later step, and a new attempt for a signed step whose validity lapsed. The watcher writes
one error line naming the objective, and the readiness reason
`fault_proof_started_late` names it as `<category>/<headerHash>` with its
latest safe start and its maturity. A restarted watcher raises both again on
the objective's first run, before or after maturity. The alert clears when the correction is
included, another record resolves the objective, or the header leaves the
queue. At maturity an objective with nothing signed is held as
`fault_proof_start_deadline_passed`, and a signed one whose next action needs
a fresh submission as `fault_proof_resubmission_required`
([MidgardWatcherProofHeld](#midgardwatcherproofheld)).

**First checks.**

```bash
curl -s "$OPS/v1/status"
curl -s "$OPS/v1/diagnostics?kind=proof_step"
```

`supervisor.remainingSafeStartMs` is negative; the readiness detail gives the
maturity time.

**Action.** Remove whatever slows the proof at once: prover wallet funds
([MidgardWatcherProverFundingUnavailable](#midgardwatcherproverfundingunavailable)),
L1 provider latency, retained-DA reachability.

**Escalate** at once to the other challengers: proving is permissionless, and
another challenger can prove the same fault before maturity.

### MidgardWatcherProofDeadline

**Meaning.** A fault proof's deadline is at risk (`deadline_at_risk`) or can
no longer be met safely (`deadline_unsafe`). The watcher keeps running; a
restart would meet the same deadline.

**First checks.** `supervisor.deadlineHealth` in `GET /v1/status`
(`at_risk` or `unsafe`) and the objectives the supervisor lists.

**Action.** Remove what slows the proof (prover wallet funds, L1 provider
latency, retained-DA reachability).

**Escalate** `deadline_unsafe` at once.

### MidgardWatcherFaultDetectorFailed

**Meaning.** A fault-proof family's replay of a committed block failed in a
way a retry would only repeat, and the header it failed on is still not
decided. The watcher withholds that family's result and decides the header
from the other families. When they find a provable fault the header is decided
in the same pass, and the `fault_detector_failed` alert (one subject per
header, informational: it does not hold readiness) is raised and cleared
together. It stays active when nothing else decides the header: the header is
recorded `failed` and the bridge proves nothing after it until the follower
moves or rewinds, so a later faulty block can merge unproven. A later replay of
another header, by the same family or any other, does not clear it. It clears
by itself, with no restart or operator action, when the header gets its own
outcome (decided, proven, removed, merged or past its horizon) or leaves the
observed queue, including when an L1 rollback drops the header: each decision
pass clears it for every header that is no longer queued, as it does every
other per-header alert ([Watcher alerts](#watcher-alerts)). A failure the decision pass retries or
defers (an L1 source or the retained DA not answering, a held script
re-execution, a local replay fault, a store connection dropping) never raises
it. Each withheld result writes one error line with `reason`
`fault_detector_failed`, naming the `family`, the `headerHash` and the error
`detail`, whether or not the alert stays active.

**First checks.**

```bash
curl -s "$OPS/v1/diagnostics?kind=alert"
curl -s "$OPS/v1/diagnostics?kind=verification"
```

The watcher log's `fault_detector_failed` lines name the family and the error;
the verification record with outcome `failed` names the header.

**Action.** Keep the watcher running and preserve its state directory. A
restart replays the header with the same detector and fails the same way.

**Escalate** every occurrence at once to the protocol developers with the
header hash, the family and the error detail: a detector that cannot replay a
committed block is a watcher defect, and while the alert stands no later
header is proven.

### MidgardWatcherProofFamilyCoverageGap

**Meaning.** The watcher classified a committed block as unprovable: its
earliest finding is one no installed fault-proof family proves.
The `proof_family_coverage_gap` alert is active, one subject per header, and
holds readiness (`active_alert`). The watcher keeps classifying and proving
the later blocks. The alert clears by itself when the header is decided
otherwise, merges, is removed, or leaves the observed state queue.

**First checks.**

```bash
curl -s "$OPS/v1/diagnostics?kind=alert"
curl -s "$OPS/v1/diagnostics?kind=verification"
```

The verification record with outcome `unprovable_gap` names the header.

**Action.** Keep the watcher running and preserve its state directory.

**Escalate** every occurrence to the protocol developers with the header
hash.

### MidgardWatcherUnprovenFaultMerged

**Meaning.** A block this watcher found faulty merged into confirmed state
before any proof removed it. The invalid block is now part of confirmed state.
The watcher records the merge in its journal database, with one error line
when it first records it, and exports when it saw each recorded merge as
`midgard_watcher_unproven_fault_merge_timestamp_seconds`, one series per
merged block (`header` label). Each time comes from the record, so it survives
restarts: this rule pages for 30 minutes after each new merge as an alert of
its own, including a second merge inside an earlier one's 30 minutes and one
the watcher first sees on its first pass after an outage, and does not page on
a restart that finds none new. The page's `header` label names the block.
The merge itself stays listed as an active `unproven_fault_merged` alert (one
subject per header) in `GET /v1/status`,
raised again after every restart. That alert never clears and nothing
acknowledges it, so it is informational: it does not hold readiness.

**First checks.**

```bash
curl -s "$OPS/v1/diagnostics?kind=alert"
curl -s "$OPS/v1/diagnostics?kind=verification"
```

The watcher log's `unproven_fault_merged` line names the header, its fault
category, its decision digest and the merge transaction.

**Action.** Keep the watcher running and preserve its state directory. Find
why the proof did not land before maturity: the
[MidgardWatcherProverFundingUnavailable](#midgardwatcherproverfundingunavailable),
[MidgardWatcherProofStartedLate](#midgardwatcherproofstartedlate),
[MidgardWatcherProofHeld](#midgardwatcherproofheld) and
[MidgardWatcherProofSubmissionFailure](#midgardwatcherproofsubmissionfailure)
alerts before the merge.

**Escalate** every occurrence at once to the protocol developers with the
header hash and the merge transaction.
