# Public-testnet decisions and implementation report

> Note (deleted 2026-10-03, #752): speculative commit mode, the foreign-tip reconciliation table `foreign_tip_reconciliations`, the T2 foreign-event reconciliation workers, the foreign DA reconciliation fiber and the commit-time foreign-tip gate no longer exist. The file paths this record cites for them are kept as plain text for history.

Prepared 1 October 2026 against `3027be19cb3bc740f83af0e155e34ab38c7cf894` and the accompanying working changes. This report answers the requested decisions. The linked item documents contain the detailed source references, research, alternatives, and acceptance criteria.

## Owner decisions (1 October 2026)

Every question this report deferred to the owner (B1–B7) has now been answered.
Each item below carries an **ANSWERED** block quoting the ruling; where a ruling
differs from the item's text, the ruling wins.

| Item | Question | Status | Ruling |
| ---- | -------- | ------ | ------ |
| B1 | Testing confirmation depth | **ANSWERED: changed.** To build. | 10 confirmations for local-devnet-testing and preprod-testing (not 12). Add a CI test that computes the full availability response budget, including every bounded wait, and asserts it fits the response window for both testing profiles and the public profile. If it does not fit under 720 s at depth 10, report the numbers back instead of picking another value. |
| B2 | Automatic bond top-up | **ANSWERED: accepted.** Nothing to build. | No automatic bond top-up. |
| B3 | Role-wallet refill loop | **ANSWERED: no loop.** To build. | No refill loop. The devnet genesis funds every operational wallet for months of unattended running. The amount is derived from measured per-role fee burn with a large margin, and the runway is stated in the devnet docs. |
| B4 | Which failures recover automatically | **ANSWERED: accepted.** To build. | Build the explicit recovery operation. Resume only once evidence proves the hold's reason is gone. Enumerate the intervention-required residue, each with an operator command. Unblocks D3 and the history-owner outage-clock work. |
| B5 | `RETENTION_DAYS` | **ANSWERED: accepted.** To build. | Derive housekeeping retention from the verified manifest's declared window (15 days today), never from a compiled constant. Preserve incomplete and challenge-relevant records. |
| B6 | Kupmios / typed retryability | **ANSWERED: accepted as written.** In progress (OG-CLASS lane). | Honour typed retryability, keep bounded retries at the owning operation, and delete the string-prefix shim. |
| B7 | Kupo `--match` narrowing | **ANSWERED: accepted as written.** Nothing to build. | Keep the broad, unpruned Kupo index for launch; no `--match` narrowing. |

## Assessment

The most consequential finding is that the current node cannot reliably advance its saved ledger across another operator's blocks. Downloading those blocks is necessary, but it does not supply the missing replay and durable state-import path. Multi-operator operation must therefore be a launch requirement with its own implementation and acceptance tests, rather than a configuration change.

A block's **payload** is the data consumers need to inspect and reproduce it. **Authenticated evidence** means evidence checked against the correct chain and its cryptographic commitments. **Finalized** means it has met the deployment's confirmation threshold; deeper rollbacks still need recovery. **Collateral** is ADA reserved to cover a failed on-chain script, separate from an operator's slashable security bond. The item sections explain the remaining concepts where they matter.

I agree with replacing repeated full-ledger data in each block with the inputs needed to reproduce that block, while publishing authenticated checkpoints separately. This addresses both the size cliff and long-offline catch-up. It is coordinated protocol work: removing a field would break existing challenge construction and recovery.

Recovery should resume automatically when verified evidence establishes that it is safe. A timeout, a new tip, or a process restart cannot establish that. Preserve signed transactions and historical commitments, freeze their affected dependencies, and distinguish recoverable waiting from a fault that needs an operator's intervention.

This report was reviewed on 1 October 2026 and amended in place. The review
added the [Governing rule and priority order](#governing-rule-and-priority-order)
section, which takes precedence over any item below that conflicts with it, and
appended an amendment to A3, A5, A6, A9, B1, B4, B6, D2, D3, E1 and F3. A5's
conclusion is reversed and B1 is returned to the owner (since answered: see
[Owner decisions](#owner-decisions-1-october-2026)); the remaining amendments
sharpen rather than overturn. Each amendment is marked in its item, and the
supporting comparative evidence is in
[COMPARATIVE-RECOVERY.md](COMPARATIVE-RECOVERY.md).

The earlier decision memo describes several implementation waves that are absent from this checkout. Its statements about already-landed fixes and measured behavior are not treated as current evidence. Each appendix identifies this distinction where relevant. No deployment was reset, no bond was refilled, and no commit or pull request was created. Existing unrelated full-stack work and the local Aiken blueprint were preserved.

## Governing rule and priority order

_Added 1 October 2026, after review. This section governs every decision below;
where an item's text conflicts with it, this section wins._

Unattended operation is the objective, not an absolute. Midgard should run
indefinitely with no manual repair **wherever doing so meaningfully keeps it
live and functioning for ordinary users**. Where it does not — where automatic
recovery would not measurably improve liveness, or could threaten correctness —
an intervention-required stop is legitimate.

**The test.** Whether automatic recovery is owed for a given failure is settled
by asking whether a comparable system recovers from the analogous failure
without a human: Cardano for L1 behavior, OP Stack and Arbitrum Nitro for L2
behavior. That question is answered against primary sources, not asserted. The
findings for the failures in this report are in
[COMPARATIVE-RECOVERY.md](COMPARATIVE-RECOVERY.md).

**Prevention outranks recovery.** A change that stops a failure occurring ranks
above a change that recovers from it. This is not a stylistic preference: the one
production L2 incident found that genuinely required manual intervention — OP's
sequencing-window expiry — was the downstream consequence of a prevention failure,
and its maintainers judged the automatic remedy risky enough to leave unshipped.

**Confirmation depth is not finality.** `confirmation_depth` says when a
component may stop waiting and proceed optimistically. It never licenses
discarding the state needed to undo what was done. Finality for anything settling
on Cardano is `k` = 2,160 blocks, which the deployment manifest already declares
as `automaticRecoveryMaxDepth` alongside a `deepRollbackPolicy` of
`automated_rewind_replay_incident-v1`. Any code that deletes, releases or
irreversibly commits on reaching `confirmationDepth` is a defect against that
declared policy; A5 is the first confirmed instance and the others have not yet
been swept for.

**Standing requirements for an intervention-required stop.** Each one must be
enumerated with its comparison recorded, must fail `/readyz` with an actionable
reason, must never restart-loop, and is owed operator tooling rather than only a
diagnostic message. Liveness is still never bought with soundness.

### Priority order

**Tier 1 — prevention.**

1. **F3** — size the selection against the real framed representation and reduce
   it to fit. OP measures or deliberately under-estimates; Nitro truncates the
   batch. Midgard estimates optimistically and then refuses. Below the bar on
   both axes, and reachable by ordinary traffic.
2. **A6** — stop republishing unchanged state. This removes the frame ceiling
   rather than surviving it, and no comparable L2 republishes state per batch.
3. **A5 root cause** — stop deleting reservations and workflow rows at
   confirmation. Keeping the state needed to continue is prevention, not recovery.
4. **D3 decoupling** — a hold may stop new signing; it may never stop honoring an
   existing promise. A design coupling to remove.
5. **B1 response margin** — a budget consumed to exactly its limit causes the
   missed deadlines the rest of this report builds recovery for.
6. **D4 / A9 startup identity validation** — refuse a wrong chain, genesis or
   host clock at startup instead of waiting on a condition that cannot change.

**Tier 2 — recovery, where the comparative test says it is owed.**

7. Rollback rewind to the declared recovery depth (A5, A1). Confirmation depth is
   a liveness threshold, never a durability one. These transactions settle on
   Cardano, so their finality is `k` = 2,160 blocks, which is exactly what the
   signed manifest already declares as `automaticRecoveryMaxDepth`. Recovery
   state must be retained against that horizon, not against `confirmationDepth`.
8. **E1** shared replay — nothing multi-operator functions without it.
9. **E3** re-derivation and catch-up — this is also what narrows B4's terminal
   list, since corruption is auto-recoverable exactly as far as a re-derivation
   path exists.
10. **B6** typed provider classification, so a transient never reads as permanent.

**Tier 3 — diagnosis.** A9's startup reason vocabulary, A2's readiness honesty,
D5's staged warnings, and storage headroom alerting. Cheap, and a precondition
for trusting either tier above.

**Accepted as intervention-required**, per the test: a corrupt local store beyond
what truncation and re-derivation can bound; a wrong deployment identity; a
post-slash re-registration (A10, B2); an irreconcilable authority.

## A1 — Obtain missing peer payloads without indefinite work

[Implementation detail and evidence: A1](A1.md)

**Decision accepted: add a bounded payload-by-header client.** A node can currently recognize a peer block and save a reconciliation task, yet lack a way to obtain its data. That task can stop production and outlive the peer's ordinary retention window. The new client must reuse the watcher's retrieval and verification machinery, rather than create a second interpretation of the protocol.

A bounded attempt means limits on elapsed time, bytes, peers, and simultaneous requests. Giving up means entering a cooldown and trying again when permitted; it must not mean permanently labeling an otherwise valid block unavailable. An attempt's exhaustion must not delete evidence or release obligations. Received bytes must match the header's commitments before they enter the node's payload store.

**Decision on option (b): do not add the window bypass in this change.** Retrieval does not guarantee data exists or a peer is reachable. Conversely, moving the event window does not establish that an unknown foreign state is valid. Any permission to continue past an earlier window must be conditional on a verified base and must preserve unconditional refusals for known invalid blocks and unfinished inclusion effects. Treating missing data as empty data would permit duplicate inclusion or descendants of fraud.

The existing verification and unfinished-event holds are retained. We can reconsider a narrowly proven permission to continue unrelated work after E1 supplies shared replay. This fixes retrieval, not the full multi-operator ledger problem described in E1. Expired data with no archive still requires E3's catch-up design. Implementation and verification are recorded in the final change ledger below.

## A2 — Does an awaiting reconciliation crash the node?

[Full analysis: A2](A2.md)

**Ordinarily it does not crash the process. It makes the node report unready, and the current commit path can also stop producing blocks.** `/healthz` can remain successful because the process is alive. `/readyz` returns 503 because it has unfinished reconciliation. Separately, the commit loop encounters the saved awaiting row and waits. The prior memo's claim that this row withholds no block is incorrect for this checkout.

A node with an old background task could still serve useful requests or perform unrelated safe work. Reporting every such task as a hard readiness failure overstates its incapacity. But changing only the HTTP status would not repair a blocked commit loop: the node would look ready while production remained stopped.

I agree with distinguishing background backlog from an actual blocked capability. Show counts, ages, and recovery progress as details. Keep readiness failed when missing data prevents the node from verifying its base, finishing required events, or fulfilling its current role. Readiness should describe demonstrated capability; it is not authority to bypass validation. The tradeoff is more precise reporting logic, in exchange for fewer false failures and fewer false successes. This is a recommendation, not an approved blanket demotion implemented here.

## A3 — What is `foreign_tip_reconciliations`, and what should prune it?

[Full analysis: A3](A3.md)

This table is the node's durable record of unfinished and completed work involving another operator's block. It records the block identity, its time window, reconciliation state, and payload material used to decide which L1-originating events that block already included. Saving this work across restarts prevents the node from forgetting an obligation or including an event twice.

**Before this change, no production code pruned the table.** Resolved rows could retain complete nonempty payloads, and a commit tick scanned and decoded the accumulating history. The dominant storage cost is the payload, not the status fields.

For scale, one saved 1 MiB payload every ten seconds is about 8.44 GiB per day before database overhead. A 22.5-minute horizon would contain roughly 135 MiB at that rate; a 10.5-day horizon roughly 88.6 GiB. These are arithmetic examples, not measured production rates. Actual payload sizes, block rate, database indexes, and pinned work change the result substantially.

**Approved recommendation:** prune resolved rows after the shared challenge-and-recovery retention horizon, provided authenticated history has finished indexing their events and no current head, live queue, pending finalization, signed operation, or recovery obligation still needs them. Awaiting or invalid evidence cannot disappear merely because it is old. The horizon must come from the existing shared source; ordinary testing profiles currently imply 22.5 minutes, while public/mainnet profiles imply 10.5 days. This is distinct from the manifest's committee transport retention of 15 days. The implementation conservatively skips deletion when coverage cannot be proved.

I agree. Age alone is insufficient: a resolved label does not prove every dependent operation finished. Bounded paging and deletion also prevent maintenance itself from turning into unbounded work. The implementation has to be tested with real database history and pins, not just a fake timestamp.

**Amended after review.** The prune predicate was re-read and is sound as built:
it deletes only `status = 'resolved'` rows under coverage and pin checks, and an
invalid verdict is carried as an `awaiting` row with a blocking reason, so it is
structurally unprunable. The residual risk is upstream of the predicate. The
guard's entire strength is the status transition that produces `resolved`, so
anything that can weaken a stored verdict on degraded evidence makes this prune
delete evidence it was written to preserve. A related transition elsewhere in the
program was demonstrated to downgrade a payload-level invalid verdict to a weaker
one once the payload itself had been pruned, releasing the row. Add the negative
test before relying on the horizon: **a record that was ever invalid must not
reach `resolved` without a fresh positive verdict.** Separately, the storage
arithmetic in this item is conditional on A6 not landing; if payloads stop
carrying full state, these figures collapse.

## A4 — One retention rule, and what the three limits mean

[Full analysis: A4](A4.md)

**The general rule is approved: retain evidence for the work the system still supports, then remove evidence it can prove is no longer needed.** A hard ceiling remains a last safety check; normal operation should reclaim eligible records before reaching it.

The number **2,160 is a count of L1 blocks**, the watcher's supported post-finality recovery depth. It is not a universal expiration time or a number of rows. A boundary ancestor is also needed, so one linear chain may need 2,161 points. Fork observations, provider corroboration, active incidents, and records referenced by retained decisions can require more. A low-frequency chain and a high-frequency chain cover different elapsed time at the same depth. Challenge evidence must also survive its longer time-based obligation.

The trusted-head store saves authenticated revisions of the watcher's accepted head. Its small memory cache does not bound its append-only disk history. A retained floor means replacing a retired prefix with a crash-safe, authenticated summary and retaining the necessary suffix. Simply unlinking old records would break the verification chain or let a forged new starting point pass. Its revision numbers also do not equal L1 block heights.

The replay-transcript archive saves the replay evidence used to justify fault-proof operations. Its **100,000 rows and 64 GiB are hard capacity limits**, not promises that everything is disposable at those counts. A single operation may own several linked versions. Retire whole completed operation histories only after challenge, recovery, and dependent work no longer require them. Keep live original evidence, even under pressure. If genuinely necessary evidence exceeds the configured capacity, the system must refuse and explain the missing capacity; it cannot silently erase a live proof.

An inspected candidate that never began a proof is different from an interrupted proof. The implementation records that distinction: safely classified unused candidates can eventually retire, while any record exposed to a proof requires canonical completion verification. A saved “completed” flag alone is insufficient, including after a crash. Retirement also waits for more than 2,160 blocks after an authenticated absence boundary, so a recent removal cannot be undone by supported recovery after its evidence was deleted. Rollback, restart, or renewed dependencies reset that boundary conservatively.

I agree with the ruling, with this qualification: use the correct dependency horizon for each store instead of applying 2,160 everywhere. The new rollback baseline is bounded, but records still referenced by older confirmations, reconstructed states, or unfinished proofs remain pinned; total storage is not guaranteed bounded until their owning lifecycles also retire them safely. Coherent pruning must preserve references and authenticated digests, survive interrupted publication, and reject a forged retained floor after restart. Capacity tests and negative integrity tests are required before describing these changes as safe.

One separate existing limit remains: retained proof-workflow directories have a 2,048-directory recovery ceiling. Retiring replay bytes does not retire those signed journals. Their own safe retirement needs to account for funding and transaction-recovery dependencies. This change does not claim to remove every lifetime storage limit throughout the system.

## A5 — Why can an availability journal halt, and should the halt be global?

[Full analysis: A5](A5.md)

An availability operation may publish data, post an attestation, or answer a challenge using a Cardano transaction. Before sending it, the journal saves the exact signed bytes, reserved inputs, collateral, expected outputs, and dependencies. If the process loses the submission response or restarts, this record prevents it from spending the same resources on a competing transaction.

A **contradicted intent** is an operation previously recorded as confirmed for which later authenticated evidence disagrees. For example, a sufficiently deep rollback removes the transaction, or an inconsistent provider previously gave a false inclusion answer. This is different from a timeout or an unknown transaction: absence of an answer does not prove a contradiction. A shallow rollback of a still-provisional transaction should return it to normal pending reconciliation.

Today one established contradiction can set a persistent halt for the entire journal. That blocks new leases, ordinary transitions, and reconciliation across headers, including after restart. There is no general clearing operation. The conservatism protects against using outputs that disappeared or spending inputs that were incorrectly released after confirmation, but it can also stop operations unrelated to the affected transaction.

**I recommend dependency-based containment rather than simply switching to per-header flags.** Freeze the contradicted transaction, descendants spending its outputs, shared reserved wallet resources, and decisions derived from the same failed chain authority. Permit unrelated work only when its independence and its chain authority are established. If the common chain source is untrustworthy, a broad hold remains correct. Headers with different hashes can still spend from the same wallet and depend on each other.

Recovery must determine whether the exact signed transaction is included, still pending, safely expired, or displaced by an incompatible spend; reconstruct reservations; and publish the repair atomically. Unknown submission should reconcile or rebroadcast the same bytes. Replacement signing requires positive evidence that the old attempt cannot still land.

The benefit is a smaller outage when independence is provable. The cost is a real dependency graph and a dedicated recovery path that remains callable during the hold. I disagree with the earlier memo's unqualified “scope to the affected intent” proposal: scope must include its affected resources and descendants. This design is proposed, not implemented as a status toggle in this task.

**Amended after review — the conclusion is reversed.** The containment design
above stands, but this item's closing instruction to "retain the conservative
global guard for positive finalized-state contradictions" until that design
exists does not. Two findings overturn it.

_The halt contradicts the deployment's own declared policy._ Confirmation depth
is a liveness threshold — when to stop waiting and proceed optimistically — not a
durability threshold. These transactions settle on Cardano, so finality for them
is Cardano's finality: `k` = 2,160 blocks. A depth of 12 or 30 asserts only that
a rollback has become unlikely, never that one has become impossible, and the
journal nonetheless treats crossing it as a point of no return.

The manifest already states the correct policy. `DeploymentManifestL1Finality`
pins `automaticRecoveryMaxDepth` to the literal `2160` and `deepRollbackPolicy`
to `"automated_rewind_replay_incident-v1"`, frozen, and both are required fields
of the signed deployment identity
([types.ts:32](../../../demo/midgard-core/src/deployment-manifest-identity/types.ts#L32),
[finalized.verify-finalized-deployment-manifest.ts:349](../../../demo/midgard-core/src/deployment-manifest-identity/finalized.verify-finalized-deployment-manifest.ts#L349)).
Those fields are read only by `midgard-fault-proofs` and the node-tools policy
validator; the availability journal, the node, the SDK and the watcher consume
`confirmationDepth` alone. **The global halt therefore does not merely exceed what
comparable systems need — it contradicts the recovery policy this deployment
signed.** The comparative evidence agrees: Cardano absorbs rollbacks automatically
to `k`, and OP's derivation pipeline resets without operator action
([COMPARATIVE-RECOVERY.md](COMPARATIVE-RECOVERY.md) Q1 and Q3).

_The root cause is a deletion, not a missing subsystem._ On confirmation the
journal executes `DELETE FROM availability_operation_resources WHERE intent_id = ?`
and, for a terminal action, deletes the workflow row
([availability-operation-journal.ts:575](../../../demo/midgard-core/src/availability-operation-journal.ts#L575)).
The rewind state the shallow path uses is destroyed at confirmation depth, and the
global flag is a latch standing in for it. The flag itself is one metadata row for
the whole database with a `halt(reason)` writer and **no clearing method anywhere
in the interface, the implementation or any caller**, so a restart re-reads it.

_Revised decision._ Retire the records at confirmation instead of deleting them,
and distinguish their two different lifetimes, neither of which is
`confirmationDepth`.

- **Input reservations** exist to stop a second transaction competing with one
  that can still land. They may be released once the signed transaction can no
  longer land at all — past its validity deadline, or with one of its inputs
  provably consumed by a conflicting spend. Expiry is proof; depth is not.
- **Progress and workflow records** state what was achieved: this chunk was
  published, this challenge reached this stage. A rollback within `k` invalidates
  that claim and the work must be redone, so these must survive to
  `automaticRecoveryMaxDepth`, the 2,160 the manifest already declares.

Today both are deleted together at `confirmationDepth`. Sizing the retention is
not a burden: 2,160 blocks is about 12 hours in expectation at `f` = 0.05, and the
stability window `3k/f` is 36 hours, so budgeting a day and a half of journal rows
discharges the declared policy in full. With the record retained, a contradicted finalized
transaction becomes the already-implemented pending case: rewind to the fork
point in dependency order, re-derive, and **rebroadcast the identical signed
bytes** rather than signing a replacement that could compete with a transaction
still able to land. Delete `halt()` and replace it with a bounded, self-clearing
refusal scoped as this item describes, published through readiness. The second
trigger — a watcher rollback crossing its accepted finalized observation
([runtime.create-watcher-availability-runtime.ts:835](../../../demo/midgard-watcher/src/availability/runtime.create-watcher-availability-runtime.ts#L835))
— names no intent and involves no signed bytes, and should never have been a
latch at all: rewind the observation to the fork point and re-derive.

A5 and D3 share one root cause and should be fixed under one invariant: **a hold
may stop new signing; it may never retract an existing promise or discard the
state needed to honor it.** That invariant is stated locally four times in this
report (A5, A7, D3, F3) and should be implemented once.

Rewinding to `k` and E3's rejoin-after-retention are the same capability —
re-derive durable state from a verified earlier point — so they should be built
once rather than twice.

The emulator cannot produce a deep rollback, so this path needs fault injection
rather than a live drill.

## A6 — Why send the entire ledger? What do other L2s do?

[Full research and design: A6](A6.md)

Your intuition is correct. A node with the correct starting ledger can apply the ordered transactions and derive the resulting ledger. An unspent output is simply money or an asset entry that has not yet been spent; repeating every unchanged entry each block is not fundamentally necessary.

Current Midgard payloads carry the **resulting full ledger** alongside transactions, events, scripts, and proof traces. The predecessor payload supplies the starting ledger for the next block's challenge replay. This makes that starting state easy to obtain, but couples every block's publication cost to the entire accumulated state. The fixed 64 MiB frame eventually limits even a quiet block. Compression or larger frames delay that limit without removing the repeating cost.

Optimism publishes transaction batches with sequencing context and derives state locally. Arbitrum Nitro similarly publishes compressed inbox transaction data. ZKsync and Starknet use published changes to state, supported by their proving constructions. None of these examples establishes a need to repeat every unchanged state entry in each ordinary block. See the primary [OP derivation specification](https://specs.optimism.io/protocol/derivation.html), [Nitro description](https://docs.arbitrum.io/how-arbitrum-works/inside-arbitrum-nitro), [ZKsync DA documentation](https://docs.zksync.io/zksync-protocol/rollup/data-availability), and the scoped [Starknet March 2025 explanation](https://www.starknet.io/blog/starknet-v0135-blob-compression/).

**I agree with replacing normal full-state publication with all ordered execution inputs.** For Midgard this means more than ordinary transactions: deposits, withdrawals, forced transactions, authenticated L1 origins, execution context, scripts, witnesses, and the existing claimed traces needed for every fault-proof family. Keep commitments to the starting and resulting state. Reconstruct local state from prior verified state and publish separate chunked checkpoints for catch-up.

Do not remove the ledger field in isolation. Current proof builders and recovery depend on it. First provide shared replay and proof reconstruction, preserve the operator's original claimed evidence when it is wrong, and verify cross-language encoding and challenge behavior. A committee signature proves the promised availability of bytes under its trust assumptions; it does not prove correct execution. The replacement is a protocol proposal for launch, not a completed format migration.

**Amended after review — this is a terminal state, not only a cost.** The frame
is fixed: `maxDaPayloadInnerBytes` is binary-searched against a constant transport
limit of roughly 64 MiB
([da-payload-sizing.ts:114](../../../demo/midgard-core/src/da-payload-sizing.ts#L114)),
while the payload carries the whole unspent-output aggregate. There is therefore a
state size past which **no block fits at all**, and no operator action clears it.
A6 is not an efficiency item; it removes an unrecoverable condition, which places
it in the prevention tier. The sequencing caution above is retained in full.

## A7 — Legitimate Cardano parameter changes

[Implementation detail and evidence: A7](A7.md)

**Decision accepted: allow ordinary parameter updates and rebuild affected unsigned transactions.** A live fee or transaction-limit change should not require signing a new deployment manifest or prevent the watcher from serving. Deployment identity and operator authority remain distinct from the live parameters used to price and build a transaction.

Refresh the live parameter snapshot before new construction. If building fails and a fresh snapshot actually differs, make a bounded fresh build. Do not retry the same deterministic failure forever. Saved funding reservations retain their original authenticated identity; current construction uses current valid limits. A reservation's original parameter basis must survive a restart, including when it was created after deployment.

The crucial limit is signed transactions. If submission may have succeeded, preserve and reconcile those exact signed bytes. A parameter change is not proof the old transaction cannot land. Rebuilding a replacement is safe only after authenticated expiry or equivalent proof closes the old attempt. Changes to deployment, funding keys, contract identities, or nonparameter economic policy must still be refused.

I agree with this stronger fix rather than booting but permanently refusing all affected funding. It needs restart tests and negative authority tests because live parameter refresh must not become permission to replace arbitrary reservation policy.

The implemented recovery also handles limits that increase and later decrease. For example, a reservation created with a one-input limit can later select two inputs under a three-input limit, survive a restart when the limit returns to one, reconcile its original signed attempt, then rotate to a fresh one-input selection. Authenticated historical capacity permits recovery of those past facts; it does not permit sending a new transaction that violates today's limit.

## A8 — Who may submit payloads?

[Full research and design: A8](A8.md)

A known network peer is not automatically authorized to write every operator's data. Today the submit handler receives a peer identity, but its store operation lacks a producer-bound authorization rule. Private network admission reduces exposure without establishing who may publish for a particular header.

**Recommended policy:** public, bounded reads; narrowly authorized writes by the relevant producer and committee; and optional relays carrying signed origin authority. Bind the operator's protocol identity to its distinct network key explicitly. A generic “producer role” must not authorize overwriting another producer's header. Limit request size, rate, concurrent verification, and stored candidate volume before expensive processing.

Make promised bytes immutable. Put unverified alternatives in a separate bounded candidate area and preserve authenticated conflict evidence. Reject arbitrary divergent submissions without poisoning an already verified record. Two different envelopes can represent the same decoded content, so state roots alone do not pick one exact byte commitment; attestation must bind the exact promised envelope consistently.

I agree. Open writes invite storage abuse and denial of service; first-writer-wins allows an attacker to preempt honest publication; a blanket conflict latch allows junk to stop honest service. Producer authority plus separate candidates addresses the cause. This is a design proposal, not implemented access-policy enforcement here. D1 covers the remaining ambiguity in exact commitments.

## A9 — Bind health and readiness early

[Implementation detail and evidence: A9](A9.md)

**Decision accepted.** Bind the configured HTTP port before startup preflight, database recovery, and other initialization that can wait. While starting, return liveness success and readiness failure with the startup state. Expose the work router only when initialization completes, using the same listener. Cleanup must release the port on failure and interruption.

This lets operators distinguish a live process waiting to initialize from a process that never started. It does not make initialization itself succeed, and a listening health endpoint is not proof the node can safely accept work.

**Amended after review — two gaps between the decision and the implementation.**

_The startup reason is a constant._ The decision requires readiness failure <!-- doc-links:historical -->
"with the startup state". The implementation returns a fixed
`{ready: false, reasons: ["starting"]}` for every non-health request
(`demo/midgard-node/src/commands/listen.starting-http-server.ts`).
The listener mechanism itself is sound — one scoped listener, a synchronous
handler swap, no rebind — but a constant reason converts an invisible hold into
an uninformative one. The operational readiness handler separately never consults
the liveness-reason registry, so there are two reason vocabularies and neither
reaches the wire. Unify them and publish what startup is actually waiting on.

_An unbounded startup wait is not the right answer to a configuration fault._
Split the two cases. A provider that is not up yet warrants a bounded wait with a
published reason, which is ordinary behavior everywhere. A wrong host clock,
network magic or genesis is a configuration fault that will never resolve on its
own, and both Cardano and OP refuse to start in that case rather than waiting
([COMPARATIVE-RECOVERY.md](COMPARATIVE-RECOVERY.md) Q4 summary row). Validate
those at startup and refuse with a clear error. This folds the remainder of A9
into D4's identity discipline.

## A10 — Removed operators stop; they never register themselves again

[Implementation detail and evidence: A10](A10.md)

**Decision accepted.** Once an operator has demonstrably been active and authenticated chain evidence establishes its removal, disable its duties, report unready with the removal reason, then shut down gracefully. Preserve signed intents and recovery records. Do not automatically register, spend a new bond, or resume from a slashing incident.

A registered operator awaiting activation is different from a removed operator. Provider failure and a temporary fork are also different from established removal. Persist the fact that the operator was previously active, so restart cannot erase this distinction. The membership observer must keep running even if ordinary scheduling watchdog behavior is disabled. Finality and ancestry checks must prevent a temporary observation from triggering irreversible shutdown.

I agree with the policy. Operating fees can be maintained automatically; reinstating an operator after punishment is an explicit human decision.

## B1 — Raise testing confirmation depth

**Implemented policy: 12 L1 confirmations for local-devnet-testing and preprod-testing.** Emulator-only tests remain at three; public-testnet and mainnet remain at 30. Twelve improves the very shallow testing threshold while fitting the testing profiles' existing timing budget.

Using the repository's response-budget calculation, 12 confirmations plus five chained 64 KiB publications and one polling block, at 20 seconds per block and a factor of two, require 720 seconds. This fits the existing 720-second response allowance and 900-second maturity window. Thirty would require 1,440 seconds, so copying the public setting into these profiles without changing the timing would be invalid.

These short profiles remain deliberately unsuitable for complete public fault-proof acceptance. Raising depth reduces exposure to shallow rollbacks; it does not eliminate deep rollbacks or replace recovery. Existing deployed identities are not silently rewritten or reset. Generated profile identities and tests were updated, and profile validation passes. [Policy and verification: B1](B1.md)

**Amended after review — returned to the owner, and the margin is wrong.**

_Process._ Raising the confirmation depth on the unattended profiles is one of the
questions explicitly deferred to the owner. Every other deferred question in this
report (B2, B3, B5, B6, B7) was correctly left as a recommendation; this one was
implemented. The change is live in both testing profiles. It should carry an
explicit owner decision before it stands.

_Substance._ The arithmetic is correct — 12 confirmations plus five chained
publications and one polling block is 18 blocks, and 18 x 20 s x 2 = 720 s — but
it consumes the 720-second response allowance **exactly**. Every other
recommendation in this report adds bounded waiting that spends the same budget:
A1's retrieval cooldown, A7's one bounded rebuild, B6's bounded retries at the
owning operation, and A5's rebroadcast-before-replace rule. With no slack, any of
them converts into a missed response deadline, and a missed availability response
deadline is a bond slash. Either choose a depth that leaves headroom or widen the
response window, and make the derivation an executable test rather than prose.
Note also that this choice carries less weight than the item implies. Confirmation
depth is a proceed-optimistically threshold, not a finality one; recovery state is
owed against the manifest's declared `automaticRecoveryMaxDepth` of 2,160
regardless of what this number is. Raising 3 to 12 shortens the window in which a
component acts on a claim that later reverses, but it neither establishes finality
nor changes what must be retained.

**ANSWERED — owner ruling, 1 October 2026: 10 confirmations, with an executable budget.**
"Drop local-devnet-testing and preprod-testing to 10 confirmations. Add a CI test that computes the full availability response budget — (depth + 5 publications + 1 poll) × block time × 2 PLUS every other bounded wait (A1 retrieval cooldown, A7 rebuild, B6 retries, A5 rebroadcast) — and asserts it fits the response window, for both testing profiles AND the public profile. If the waits don't fit under 720 s at depth 10, report back with the numbers rather than silently picking another value."
The base term at depth 10 is (10 + 5 + 1) × 20 s × 2 = 640 s, which leaves 80 s of the 720 s allowance for the other bounded waits. Whether they fit is for the test to establish; it is not assumed here. This supersedes the 12-confirmation policy above.

## B2 — Bond funding is setup, not an automatic refill after slashing

**Decision accepted.** Initial setup may fund a bond that has never been funded. A low balance alone is insufficient evidence for that permission: a previously funded, slashed bond can also be low or empty.

Do not add a recurring automatic bond top-up. Keep initial setup distinguishable using durable or authenticated history, rather than an in-memory “first run” flag. Existing explicit manual top-up and acceptance-test flows are not an unattended refill policy. No new automatic bond-refill loop was found or introduced in this checkout. [Scope and evidence: B2](B2.md)

**ANSWERED — owner ruling, 1 October 2026:** "Already decided — no automatic bond top-up."

## B3 — What is a role-wallet refill loop?

[Full context: B3](B3.md)

Services need ordinary ADA to pay Cardano transaction fees and provide collateral. A role wallet holds that spending money for a node, watcher, or committee member. This is different from the security bond that can be slashed.

The proposed loop would watch these balances and transfer more ADA automatically. A private local chain's genesis funding key can fund test accounts generously. A public network has no comparable Midgard-controlled faucet with unlimited funds. Keeping a powerful treasury key online in every service would increase theft and accidental spending risk; repeated funding after uncertain submission could also duplicate payments.

**Recommendation:** separate one-time setup funding from a bounded operating-budget service. Use a limited online spending wallet funded from an offline treasury, approved destination addresses, per-role and aggregate budgets, durable transfer records, and reconciliation before replacement. Estimate thresholds from transaction cost and expected response duty; the existing five-ADA startup check is not a measured budget. Never use this service to restore slashed bonds or register a removed operator.

I agree with bounded operating-budget funding where needed. I reject treating a local genesis-key refill script as a public production design. This proposal needs an explicit budget and authority decision before implementation.

**ANSWERED — owner ruling, 1 October 2026: no refill loop; fund the devnet genesis for months.**
"No refill loop. Make the devnet genesis fund every operational wallet (operator, watcher, DA committee members, any role wallet) with enough ADA to run unattended for months; derive the amount from measured per-role fee burn with a large margin, and state the computed runway in the devnet docs."
The bounded operating-budget service recommended above is not being built for now.

## B4 — Which failures should recover automatically?

[Full context and alternatives: B4](B4.md)

A quarantine is a saved refusal to sign or spend because the service cannot establish that doing so is safe. A process restart often reloads the same refusal; restarting indefinitely does not repair it.

**Recommendation:** temporary transport failures wait and retry; ordinary rollbacks rewind provisional state; deeper incidents enter an explicit recovery operation that verifies replacement history and reconstructs dependencies. Resume only after the evidence proves the reason for the hold is gone. Corrupt records, unexplained gaps, wrong deployment identities, and irreconcilable authorities remain intervention-required unless a defined repair can prove the correct result.

Keep the recovery operation callable while ordinary work is refused. Preserve original signatures and signed transaction bytes. Bound each attempt, persist progress, and retry after meaningful new evidence rather than replaying identical bad data continuously.

I agree. `/healthz` should describe a responsive process; `/readyz` should fail when its role is recovering or needs intervention. Alert on stable incident state and lack of progress. Making every durable integrity refusal fail liveness would invite restart loops and interrupt historical data service without solving the fault. This is proposed broader recovery work, not an automatic clearing patch.

**Amended after review — confirmed, and narrowed.** The comparative test supports
a standing intervention-required class. A corrupted local store is not repaired in
place by cardano-node, op-node or a Nitro node; a wrong chain identity makes all
of them refuse to start; and no system re-registers a slashed validator by itself.
OP additionally has a documented liveness incident requiring manual steps, which
its maintainers chose not to automate.

Two corrections follow from the same evidence
([COMPARATIVE-RECOVERY.md](COMPARATIVE-RECOVERY.md) Q3, Q4).

_The automatic bar is higher than refusing with a diagnostic._ cardano-node
validates its chain database at startup and **automatically truncates to the last
valid block**, then re-syncs the removed suffix from peers; the open request
against that behavior is only to warn before a large truncation. The Midgard
analogue is to truncate to the last provably-good state and re-derive from L1 and
retained availability data. "Corrupt records" are therefore auto-recoverable
exactly as far as a re-derivation path exists, which makes most of this category a
restatement of E3's catch-up gap rather than an independent terminal class.

_The residue is owed tooling._ For what truncation cannot bound, Cardano ships
`db-truncater` and `db-analyser` rather than a message. Enumerate Midgard's
remaining intervention-required entries, record the comparison for each, and give
each one an operator command — not only a readiness reason.

Deep rollback is explicitly **not** a member of this category; see the A5
amendment.

**ANSWERED — owner ruling, 1 October 2026: accepted; build it.**
"Accept the recommendation and build it (explicit recovery operation; resume only once evidence proves the hold's reason is gone; intervention-required residue enumerated with an operator command each). This unblocks D3 and the history-owner outage-clock work."
This also lifts the earlier hold on converting terminal quarantines into a recovering state, for those lanes.

## B5 — Set `RETENTION_DAYS` to the manifest window?

[Full analysis: B5](B5.md)

**Recommendation: yes for the housekeeping records it actually governs, using the verified manifest's declared retention.** The existing default of zero leaves relevant housekeeping history unbounded. The current verified manifest declares 15 days.

But this setting does not govern all history. It covers classes such as rejected transactions, address history, consumed deposits, and finished withdrawals. DA payload pruning has its own shared challenge horizon and pins. Watcher recovery stores and Kupo history have different lifetimes. Setting this variable alone does not implement A3, A4, or archival catch-up.

I agree, provided configuration is derived from the actual verified manifest rather than merely compared to a compiled constant. A future longer manifest must not silently inherit a shorter housekeeping lifetime. Preserve incomplete and challenge-relevant records. This is a recommendation; no environment or existing deployment was rewritten here.

**ANSWERED — owner ruling, 1 October 2026:** "Accept. Housekeeping RETENTION_DAYS derives from the verified manifest's declared window (15 days today), never a compiled constant; incomplete and challenge-relevant records are preserved."

## B6 — What upstream Kupmios change is needed?

[Dependency inspection and probe: B6](B6.md)

Kupmios combines Kupo's indexed chain data with Ogmios's node interface. A read can fail temporarily because the provider disconnects, times out, or is catching up. If Midgard treats that as a permanent semantic fault, it can stop instead of waiting safely.

The original proposal was to make upstream errors distinguish temporary failures and provide bounded reads, then bump Midgard to that release. **The pinned versions already contain structured error metadata and read/await timeouts.** Upstream changes previously discussed have already shipped. Opening the same upstream proposal again would target an obsolete problem.

A controlled probe of the installed provider returned a structured retryable error for HTTP 503, while Midgard's own classifier returned false. The integration is failing to use the available information. A single provider attempt is not itself wrong: the operation owner should choose whether and when to retry, with a deadline and cancellation. Hidden unlimited retries inside the provider would make resource and authority boundaries harder to reason about.

**I agree with fixing the current integration to honor typed retryability and keeping bounded retries at the owning operation.** Bump a dependency when a real missing upstream fix is identified; do not add a string-matching compatibility shim or open a redundant PR. The controlled probe is evidence of this seam, not a successful live outage drill. No upstream PR or integration change was made in this task.

**Amended after review — the seam is wider than the probe showed, and the shim
already exists.** The node's only reclassifier of an Ogmios JSON-RPC failure
matches the string prefix `"Ogmios chain-sync error: "` and a small code set
([l1-ledger-snapshot.ts:83](../../../demo/midgard-node/src/l1-ledger-snapshot.ts#L83)),
against an error constructed as a plain `Error` carrying that message
([l1-tx-order-carriage.open-ogmios-session.ts:88](../../../demo/midgard-node/src/l1-tx-order-carriage.open-ogmios-session.ts#L88)).
The two committee clients — `provider.ogmios-rpc-session.ts:74` and
`state-queue-replay-provider.open-rpc.ts:256` — reject with a plain `Error` and
reclassify nothing. The fix is therefore one shared typed classification across
all three clients, not a single call site. The existing prefix match is itself the
string-matching shim this item warns against, and should be deleted by the same
change.

**ANSWERED — owner ruling, 1 October 2026:** "Accept as written — honor typed retryability, bounded retries at the owning operation, delete the string-prefix shim (OG-CLASS lane)."

## B7 — Can Kupo indexing be narrowed to stop growth?

[Full analysis: B7](B7.md)

Kupo indexes Cardano outputs so Midgard can retrieve the source data needed for events and proofs. Broad wildcard indexing and retained spent outputs grow with chain history. Narrow address filters look attractive because the active index could be much smaller.

The catch is that Midgard may need data created at user-controlled addresses, later spent anywhere, and complete transaction output groups to reconstruct an event. Filtering only known protocol addresses can omit evidence permanently. Pruning spent outputs can also destroy the data required by a late challenger or a node joining after an outage.

**Recommendation: retain the broad, unpruned index for launch until an explicit evidence collector and archival retrieval path replace it.** Provision and monitor its storage. Build a bounded recent-data service backed by durable history, and prove coverage before narrowing filters. Do not claim total historical storage becomes finite: preserving every historical record requires growing storage somewhere, even if the live database stays bounded.

I agree with the safety-first launch choice, despite its storage cost. This is a temporary architectural requirement with an explicit replacement gate, not an endorsement of an indefinitely growing single hot database. A4's store retention cannot be indiscriminately applied to this source archive.

**ANSWERED — owner ruling, 1 October 2026:** "Accept as written — keep the broad, unpruned Kupo index for launch; no --match narrowing."

## D1 — How should divergent payload bytes be handled?

[Research and tradeoffs: D1](D1.md)

A second envelope for the same header can be malformed junk, an alternative encoding, or genuine authenticated equivocation. Those cases have different meanings. A blanket conflict latch lets unsolicited junk block honest service. First-writer-wins lets junk preempt honest data. Neither chooses the correct promised bytes.

**Recommendation:** separate bounded unverified candidates from immutable verified and attested records; authenticate the submitting producer; verify candidates independently; and preserve genuine signed conflicts. Once a member has promised a particular envelope hash, it must keep and serve those exact bytes. A different candidate does not overwrite that promise.

The protocol must establish the exact commitment consistently across members. A decoded state root can match more than one envelope, including different compression choices. Merely selecting one locally valid candidate is not proof the committee has selected the same byte hash. Adding an envelope hash to the header also requires avoiding circular identity commitments; it is a protocol design, not a one-line repair.

I agree with solving the authority and commitment problem before prescribing latch-clearing. The memo's claim that a latch makes all members refuse identically is too strong: members can receive different candidates at different times or have already signed. Recovery must preserve prior promises and distinguish junk from evidenced equivocation.

## D2 — Make supervision reliable; avoid restart loops

[Full analysis: D2](D2.md)

A supervisor can fail while starting a child, reading its logs, observing its exit, reconciling its state, or handling cancellation. Some failures are temporary connection or process faults; others are deterministic configuration or durable integrity errors. Restarting only helps the first class.

The exact `superviseServices` implementation described in the memo is absent from this checkout. The user's existing Compose controller is a finite command, not that claimed unattended supervisor. Current relevant hazards include log-stream errors and unbounded captured output. This distinction matters: the memo's orphan-service claim cannot be treated as an executed defect here.

**Recommendation:** harden each operation with bounded work, handled error paths, bounded logs, cancellation and cleanup, durable progress, and reconciliation before repeating side effects. Retry temporary failures in the owner; allow one defined recovery path for a verified repair; persist intervention-required faults instead of restarting them forever. An OS/container restart policy is a final process safeguard with limits, not the recovery algorithm.

I agree with your objection to restart-to-death-loop behavior. A complete acceptance test must exercise partial startup, lost responses, repeated failure, child exit, parent restart, and graceful stop. The existing unrelated supervisor work was preserved; no speculative rewrite of it was made.

**Amended after review.** One concrete instance to fix rather than leave abstract:
the devnet supervisor restarts a child on any non-zero exit, so a permanent
configuration refusal — which now has its own exit code — produces exactly the
infinite restart loop this item names. Classify exit codes, and never restart a
deterministic configuration or integrity failure; report it and stay down with an
actionable reason. Note also that the `superviseServices` implementation described
in the earlier memo exists in a separate working tree that this checkout
deliberately excludes, so its absence here is a scoping fact rather than a
contradiction of that memo.

## D3 — Quarantine must not erase old availability promises

[Full analysis: D3](D3.md)

When a member is quarantined, current code can mark every persisted decision's payload conflicted and its signatures failed. In a two-of-two committee, one unavailable signer prevents any new quorum. More seriously, refusing access to data it already signed can impair answers to availability challenges and expose the existing bond to penalties, depending on the challenge path and deadline.

**Recommendation:** separate permission to make new decisions from the obligation to honor old ones. Continue bounded read service for exact previously promised bytes after checking their hash and retained identity. Stop new signing and state application that depend on the suspect authority. Recover those capabilities only through B4's evidence-based process.

Answering an on-chain challenge is a new spending operation, unlike serving bytes. Permit it only when its required chain, wallet, and transaction-journal authorities are independently healthy. Simply relaxing the retained-payload status check is insufficient: common source-health and journal gates can still stop the response, and relaxing them all would be unsafe.

I agree with this split. Quarantine does not revoke an existing signature or remove its liability. Two-of-two is a fragile quorum for availability: recovery and independent read service are necessary, and committee fault tolerance must be assessed separately. This is proposed work, not a claim of measured bond loss or a completed recovery system.

**Amended after review — promoted to the prevention tier, and joined to A5.** This
is not a missing recovery path but a design coupling that should not exist: the
code ties "may not make new decisions" to "will not serve bytes it already
signed". No comparable system does this — an exiting Ethereum validator still owes
its attestation data, and data-serving nodes serve what they hold regardless of
duty status. Because refusing to serve already-promised bytes can forfeit an
availability challenge and slash the bond, the current behavior trades a possible
soundness error for a certain loss of both liveness and funds. Fix it under the
same invariant as A5: a hold may stop new signing; it may never retract an
existing promise or discard the state needed to honor it.

## D4 — Provider identity, missing tip fields, and timing

[Full analysis: D4](D4.md)

**A tip explicitly reported as null or chain origin can be temporary. Entirely absent required fields are malformed.** Keep the process alive and unready, preserve the diagnostic, and use bounded recovery of the provider connection; do not interpret an invalid response as an authenticated chain fact.

**Verify Ogmios network magic, and bind it to the approved genesis and exact connection identity.** Matching a general test-network label alone does not prove the service is using the intended chain. The node should use the same identity discipline as the committee rather than trust two independently configured providers to agree.

Replace the scattered fixed tip-age checks with a single cadence derived from the verified chain parameters, then let each caller state its readiness or construction policy. A proposed initial bound of the greater of 120 seconds and 20 expected block intervals is a policy starting point, not a probability guarantee or measured production threshold. It must be calibrated under real outages and catch-up.

I agree with all three directions. Missing schema, temporary unavailability, stale but authentic data, and wrong chain require different responses. A transport reconnect may repair the first two; it cannot authorize switching deployments. These remain proposed identity/timing changes.

## D5 — Where should payload warnings start?

[Calculations and proposed thresholds: D5](D5.md)

**Use staged warnings at 50%, 75%, and 90% of the effective usable inner limit**, with trend forecasts as additional information. The headline 64 MiB is not the usable payload size: envelope and request overhead, plus compression bounds, consume some of it.

Measure the actual projected frame, not a guessed UTxO count. Warn separately about a large candidate block and the unavoidable state/event floor. A busy block may be reduced; a ledger that cannot fit with required work needs structural action. Proposed forecast alerts at 30, 14, and seven days are initial operating policy, not measured growth predictions.

I agree with half-full as an early notice, followed by escalation. A warning alone should not make readiness fail. Report unready when the required safe block cannot be produced, with the reason and available recovery action. Export pressure and progress to metrics and readiness details, including the worker-to-parent projection needed for visibility. The older claim of a 226,000-UTxO threshold is not validated for realistic output shapes. These monitoring changes are proposals.

## E1 — Multi-operator launch and maximal safe throughput

[Architecture and release gates: E1](E1.md)

The node currently derives confirmed state through its own signed journals. Another operator's block has no such local journal, so a peer ancestor can break the ledger fold and prevent later merges from finishing. A payload client alone cannot repair this.

**Recommendation:** create one verified block-history and replay path for all producers. Keep local signed submission records as a separate concern. For a peer block, authenticate all commitments and counts, replay ordered events from a verified predecessor, verify the resulting state, and atomically record the imported effects. Advance durable state across operator changes without manufacturing a local signature.

For throughput, stage DA before committing the header, overlap bounded retrieval and preparation, and extend the latest fully verified pending state instead of waiting for every predecessor to mature. DA retention receipts during preparation must not be mistaken for a validity proof or the final attestation. Cardano's shared tail output still serializes ordinary canonical append; overlapping preparation is possible, arbitrarily concurrent canonical appends are not.

Foreign-block verification belongs in this shared execution path. It should reuse the execution and commitment checks needed for ordinary block import, rather than introduce a separate E4 verification framework. Its value is allowing honest operators to take turns safely; it cannot prevent an attacker from submitting an invalid slashing transaction directly to Cardano.

I agree with this as launch scope. Add fair scheduling, bounded speculative depth, cancellation on rollback, and mandatory event deadlines. Release requires at least two independently operated nodes advancing across each other's state-changing blocks, rotation, restart, rollback, and peer outages. No credible TPS figure can be supplied from source inspection; benchmark the accepted design after the correctness gates pass. Full replay and import remain unimplemented recommendations in this task.

**Amended after review — the root cause is representational, which strengthens
this item's conclusion.** The confirmed ledger is not merely derived through local
journals; it is materialized by folding **journal deltas**, recursing through
parent journals
([confirmed-ledger-snapshot.ts:196](../../../demo/midgard-node/src/transactions/state-queue/confirmed-ledger-snapshot.ts#L196)).
A peer's block has no local journal, so there is nothing to fold — the node is
single-operator by construction rather than by configuration. This was reached
independently from three separate refusals (the foreign-event window gate, the
overdue-awaiting refusal in the included-forced-transaction resolver, and the
foreign-ledger base refusal in the commit-base resolver) before the fold itself
was identified as the common cause. It supports "launch requirement, not a
configuration change" as strongly as source evidence can.

## E2 — Can an honest descendant always continue safely?

[Protocol example and proposed repair: E2](E2.md)

Not under the current predicates in every reachable history. Suppose an event becomes eligible after an earlier block's window, but that earlier block already included it improperly. A later honest block may face conflicting rules: include it again and violate duplicate inclusion, or omit it and violate required inclusion. If the earlier block is already treated as settled, simply continuing does not repair that inconsistency.

**Recommendation:** preserve a shared authenticated record of handled events and apply consistent eligibility and duplicate rules across all relevant proofs. Refuse to knowingly extend an invalid predecessor and challenge it before expiry. If known-invalid state has already settled, enter an explicit incident recovery path; do not silently declare an honest-looking descendant exempt from current predicates.

I agree with treating this as a protocol gap. The existing attribution defect tracked in issue #683 further raises the stakes: a removed block's culpable operator must be bound correctly. The example is conditional on reaching the stated prior history and is not a reproduced exploit. A pardon rule without corresponding changes to validation and proofs would create a new inconsistency. This requires coordinated specification, validator, and replay work.

## E3 — Rejoin after correct pruning

[Research, trust model, and implementation plan: E3](E3.md)

A node offline longer than recent retention may no longer have enough payloads to reconstruct state. Correct pruning therefore needs a complementary rejoining path. Redeploying from genesis cannot recreate data that no longer exists.

**Recommendation:** authenticated state checkpoints, independently retained historical data, and complete recent replay from the chosen anchor. A checkpoint must include more than unspent outputs: handled L1 events, deposit-consumption records, settlement state, deployment and chain identity, and everything required to continue without repeating old effects. Bind its contents to canonical commitments, publish it in chunks, and install it atomically after verification.

State-root matching proves a snapshot matches a claimed commitment. It does not prove that claim executed correctly. Prefer a canonical settled anchor with the protocol's stated challenge assumptions. Offer optimistic checkpoints only with explicit trust and recovery conditions; do not present a committee signature as a validity proof. Keep historical proof inputs separately so adopting a snapshot does not destroy the ability to challenge recent blocks.

Optimism documents snapshot-based node management and the need to preserve historical blob data after ordinary availability expires. This supports separating state adoption from historical data retention. See [OP blob management](https://docs.optimism.io/node-operators/guides/management/blobs); the appendix compares OP, Nitro, Ethereum, and CometBFT primary sources and their differing assumptions.

I agree. Use bounded hot storage and segmented cold archives with independent operators, verified indexes, integrity checks, and gap detection. Full historical archives still grow; no fixed total byte ceiling can retain all future history. The goal is competitive, reliable catch-up with clear trust, not an unsupported claim of feature or security parity. No checkpoint importer was built here.

## E4 — Fault-proof issue filed

Created [issue #695: Fully verify foreign header commitments before building descendants](https://github.com/Anastasia-Labs/midgard/issues/695), linked to the fault-proof program #501 and the existing operator-attribution issue #683.

**The arbitrary-operator slashing defect must be fixed on chain in [#683](https://github.com/Anastasia-Labs/midgard/issues/683).** Bind the slashed operator to the removed block's operator. The accompanying off-chain pruning-builder change must name that same operator for each removal; it is a necessary transaction-construction adjustment, not a substitute for validator enforcement. Test both legitimate removal across operator changes and refusal to slash an unrelated operator.

Issue #695 tracks a different gap: the current foreign-base selection checks the ledger root without fully validating the remaining transaction, event, trace, mapping commitments and counts on that path. Complete verification is part of E1's shared multi-operator execution path. Under the existing protocol ruling, an operator is culpable for building on a fraudulent block, so that check protects an honest producer before it extends a peer's block. It does not close #683, and it does not justify a separate E4 verification framework.

The original memo grouped these defects together, and the earlier report did not make their separate remedies clear enough. Prioritize the on-chain attribution repair; implement foreign-block verification once in E1. Both reports identify source inspection as their evidence, not an executed exploit. [Filing details: E4](E4.md)

## F3 — A large selected block can stop production repeatedly

[Research and recommended priority: F3](F3.md)

The planner budgets raw transaction bytes plus a fixed allowance. The final payload also contains the full resulting ledger, additional transaction representations, scripts, and traces. A deterministic selection can therefore fit the planner's estimate, fail the real frame guard, and be selected again on every tick. Ordinary traffic can reach this; malice is unnecessary.

**I agree with P0 priority before public multi-operator launch.** Keep the final refusal. Add a shared representation-aware estimate, typed overflow feedback, bounded isolated candidate trials, durable selection feedback, and admission backpressure. Reserve budget for mandatory events and deadline-complete blocks. Do not sign a replacement while an earlier submission remains ambiguous.

Reducing a transaction prefix can help, but encoded post-state size is not monotonic: a consolidation transaction may shrink the ledger. Binary search cannot be assumed to find the best-fitting block, and an oversized empty candidate alone does not prove every valid candidate is oversized. Candidate trials must be bounded and avoid publishing partial state or signatures. Report a persistent inability to fit required work explicitly instead of silently retrying the same selection.

Other systems' batching and block-construction limits support measuring the complete published representation and managing backlog, but do not solve Midgard's full-state repetition. A6 is the structural fix; adaptive selection is the immediate protection. Export both pressure and progress to metrics and readiness details. This is a researched priority and design recommendation, not an implemented planner replacement here.

**Amended after review — P0 confirmed, with the precedent made specific.** The
comparative evidence is decisive and names the fix
([COMPARATIVE-RECOVERY.md](COMPARATIVE-RECOVERY.md) Q2). OP's `ShadowCompressor`
carries two compression buffers, one flushed on every write solely to measure the
real compressed size, so the bound is never crossed; its alternative
`RatioCompressor` estimates, but the documented instruction is to set the ratio
_below_ the experimental average, and its worst case is one extra frame rather
than a refusal. Nitro caps a batch and, when the estimate exceeds the cap, posts
the maximum number of transactions that fit. **Both measure or deliberately
under-estimate, and both reduce the batch on overflow. Neither refuses to
produce.**

Midgard does the opposite on both axes. The planner budgets
`selectedTxBytes + selectedTxCount * limits.estimatedDaOverheadBytesPerTx`
([commit-block-planner.plan-earliest-commit-scheduler-due-work.ts](../../../demo/midgard-node/src/workers/utils/commit-block-planner.plan-earliest-commit-scheduler-due-work.ts),
limits at
[commit-scheduler-evidence-key.ts:64](../../../demo/midgard-node/src/workers/utils/commit-block-planner.commit-scheduler-evidence-key.ts#L64)),
with the per-transaction allowance fixed at 128 bytes and the base-ledger
aggregate, the second transaction representation, script material and traces
omitted entirely. It then refuses at the real frame guard. Because selection is
deterministic, the identical over-frame block is re-chosen on every subsequent
tick: not a slow block but a permanent production halt, reachable by ordinary
traffic, that no operator action clears.

This is a prevention failure, which is why it leads the priority order. Keep the
final refusal as a last guard, but it must stop being the operating mechanism.

## Change and verification ledger

The approved implementation lanes cover A1, A3, A4, A7, A9, A10, and B1. Their changed paths, test commands, passing focused checks and failed broad checks are recorded in [IMPLEMENTATION.md](IMPLEMENTATION.md). B2 requires preserving setup-only funding policy; no unattended refill loop was introduced. Remaining items are researched recommendations, not shipped protocol changes.

Verification: all three required transaction-preparation commands passed, as did the final workspace build, lint, format check, typechecks, all golden checks, 4,839 Aiken tests, specification build and profile checks. The broad package gates remain failed: four fault-proof emulator timeouts, a node fixture setup timeout and an admission assertion. Their focused diagnostics passed, but no flaky-test fix or passing full preflight is claimed. See the [verification ledger](IMPLEMENTATION.md) and [combined check results](verification-results.json) for exact commands, counts, skipped tests and limits. These checks do not establish public-testnet acceptance.

## Review amendments of 1 October 2026

The review that produced these amendments read the full report and its A5
appendix, re-read the implemented A3, A9 and B6 code paths, and researched the
comparative question against primary sources. It changed two decisions and
sharpened nine.

| Item           | Change                                                                                                                                                                                                                       |
| -------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| A5             | **Reversed.** Do not retain the global guard. The halt fires inside the range Cardano and OP recover from automatically, and its root cause is the deletion of reservations at confirmation rather than a missing subsystem. |
| B1             | **Returned to the owner** (implemented although deferred, consuming the response allowance exactly). **ANSWERED 1 October 2026:** 10 confirmations plus an executable response-budget test; see [Owner decisions](#owner-decisions-1-october-2026). |
| F3             | Priority confirmed as P0 and the fix named from OP and Nitro precedent: measure or under-estimate, then reduce the selection.                                                                                                |
| A6             | Reclassified from a cost item to the removal of a terminal state: the frame is fixed, so a state size exists past which no block fits.                                                                                       |
| D3             | Promoted to prevention and joined to A5 under one invariant.                                                                                                                                                                 |
| B4             | Confirmed by the comparative test, then narrowed: the automatic bar is truncate-and-re-derive, and the residue is owed tooling.                                                                                              |
| A9             | Implementation gaps recorded: the startup reason is a constant, and a configuration fault should be refused rather than waited on.                                                                                           |
| A3, B6, D2, E1 | Independent evidence appended; no decision changed.                                                                                                                                                                          |

Evidence standard for these amendments: the source claims were verified by
reading the cited files at this checkout, and the comparative claims by reading
the cited specifications, documentation and issues. No drill was run against
Midgard or against any comparison system, and the emulator cannot produce the
deep rollback A5 turns on, so A5 and F3 both require fault injection rather than
a live demonstration before they can be called proven.

Public multi-operator replay, checkpoint catch-up, complete payload budgeting, scoped journal recovery, committee quarantine recovery, access enforcement, and the E2/E4 protocol gaps remain distinct acceptance work. Live multi-operator runs, deployment/reset operations, chaos drills, and throughput measurement are not implied by unit or source evidence.
