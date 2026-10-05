# Owner decisions needed — devnet unattended-lifecycle program, 2026-10-01

*Revised later the same day: both review waves finished and added five more
(Part D), and I took eleven decisions myself rather than ask — those are listed
at the end so you can overrule any of them.*

Twenty-two open questions. **Nothing is blocked waiting for you except one lane
(LS3-AJ).** Everything else already runs under a stated default, listed below —
so silence is a decision, and I would rather you overrule a default than have a
lane idle.

Each item: what the code does today, what is at stake, the default in force,
and my recommendation.

---

## Part A — new from sweep 3 (read-only sweep of node, watcher, committee, SDK)

### A1. Foreign-tip latch: DA client, or narrow the window?  **(P0, defaulted)**
An ordinary L2 transfer writes an `awaiting` row that can only clear once a
**foreign** block's DA payload is locally available — but `midgard-node` has no
`payload-by-header` client **at all** (it implements only the server side).
`commit-block-header.database-operations-program.ts:567` returns
`AwaitingForeignDaOutput` before doing any work, so block production stops; and
`daPayloads.pruneBeyondRetention` exempts only confirmed-head, live-queue and
finality-held payloads, so after 22.5 minutes the row is **unresolvable
forever**. This is the same self-amplifying shape as the watcher brick, in the
node.

- **(a)** Give the node a bounded payload-by-header client (reuse the watcher's
  `WatcherPublicDaClientV1`), with bounded retry and a self-clearing give-up.
- **(b)** Narrow the T2 refusal to the foreign block's own `(startTime, endTime]`
  window and keep committing over `(endTime, now]` — the event-horizon reasoning
  the same file already applies 12 lines later at `:579-580`.

**Default in force: (b)**, being implemented now. **Recommendation: (b) now,
(a) before any genuinely multi-operator network**, because under (b) alone this
node can never obtain a peer's payload — (b) bounds the damage, it does not make
foreign payloads reachable. Neither option may bypass the `needsPayload` check:
committing on a foreign base whose non-empty event roots were never verified is
the double-inclusion hazard.

### A2. Is `foreign_tip_reconciliation_awaiting` a *hard* readiness reason?
An unresolved row withholds no block — the 503 is pure liveness loss — but
`tests/readiness.test.ts:68-77` **pins** the current behaviour, so demoting it
to a readiness *detail* (the pattern `pendingFinalizationAgeDetail` already
uses) is a ruling, not a bug fix. **Recommendation: demote to a detail.**
LV-ND1 needs this.

### A3. What prunes `foreign_tip_reconciliations`, at what horizon?
Confirmed by grep: **nothing does.** No `DELETE` anywhere; the full-table scan
plus per-entry decode runs on every commit tick and grows without bound, unlike
`da_payloads` which has `pruneBeyondRetention` and a sweeper.
**Recommendation: resolved rows past the challengeability horizon**, deriving
the horizon from the same `MIDGARD_RETENTION_WINDOW` source the committee prunes
by — never a second copy of the arithmetic. Being implemented under that
assumption; confirm the horizon.

### A4. One general retention ruling instead of four patches
Proposed rule: **every unbounded durable collection gets horizon-based retention
tied to the recovery depth, and hard ceilings become assertions that retention
works rather than fuses on uptime.** Concretely, is
`WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth` (2,160) the right floor for
both the authenticated consistency history and the durable observation store, or
should it be the challenge window? The argument for 2,160: `planRewind`
provably cannot rewind deeper, so evidence below it is unreadable by
construction. This one ruling also covers the trusted-head record floor and the
replay-transcript archive's 100,000-row / 64 GiB ceilings.
**Recommendation: adopt the rule, floor at 2,160.**

### A5. Availability-journal halt — journal-wide, or per-intent?  **(BLOCKING)**
Today one contradicted intent disables **every** lease, transition and reconcile
for **every** header, across restarts, and there is **no clearing code in the
repo**. Scoping the refusal to the affected intent would let the rest of the
availability machinery run while that one is re-derived. This changes the blast
radius of a finality violation, so I am not defaulting it: **LS3-AJ is held and
is the only lane waiting on you.** Distinct from B4 — that is about
auto-clearing, this is about blast radius.
**Recommendation: scope to the affected intent.**

### A6. DA payload size cliff — confirm the scope
Every block's DA payload carries the **entire L2 UTxO set** against a fixed
64 MiB frame (`cek-proof.encode-midgard-cek-term-node.ts:82`). Not shipping the
whole set, or chunking the frame, is a protocol decision I will not take.
LS3-CS is scoped to making the limit **observable well before it bites** and the
refusal **non-terminal**. **Recommendation: confirm that scope**, and yes —
somebody should measure how far a realistic block is from the limit and the
growth rate per L2 UTxO. I have asked LS3-CS for a projection that does not
require the live devnet.

### A7. Watcher refuses to boot after a legitimate protocol-parameter update
`watcher/src/funding/prover-funding.ts:148-150` refuses to boot whenever live <!-- doc-links:historical -->
protocol parameters differ from the signed deployment manifest — so a routine L1
parameter update bricks the watcher until someone re-signs the manifest.
**Recommendation: boot, serve, and refuse only the funding operations that
actually depend on the changed parameters, alerting loudly.** LS3-WIO leaves
this arm alone for now.

### A8. Access policy on payload-submit
`createDaLibp2pPayloadRequestHandlers` takes only
`{deploymentFingerprint, store, limits}` while the proof handlers take an
`accessPolicy` — so any registry peer can write a payload record for a header it
did not produce. A policy change, not a bug fix.
**Default in force: LS3-DAP implements only the non-destructive half.**
**Recommendation: narrow submit to the producer/committee roles**, separately.

### A9. Bind `/healthz` + `/readyz` before the router?
A boot wedge currently exits the node **37 lines before the port is bound**, so
a probe sees nothing and the operator has to read logs to learn why. The
committee already does it the other way (`listenStartingServer`). Purely
additive — the surface accepts no work, signs nothing, grants no authority — but
it is a visible interface change. **Recommendation: yes, bind it first.**
LV-HO1 and LV-WA1 need this before they wire it; both are proceeding without it.

### A10. Auto re-register, spending a 900 ADA bond?
A node that finds its own operator key absent from the active set is today
permanently inert **while reporting healthy**. Adjacent to B3 but distinct: B3
refills a role wallet from the genesis key, this **spends a fresh bond**.
- *Report unready and wait* → a readiness reason in LV-FB1 plus LV-ND1.
- *Auto re-register* → an idempotent reconciliation gated on the observed
  active/registered sets, with backoff.

**Recommendation: report unready now** (it is the honest signal and it is
cheap), and treat auto re-registration as a later decision — spending a bond
without a human is the one case in this program where I think "never requires
manual intervention" should bend.

---

## Part B — the seven still deferred from the earlier waves

Restated for completeness; all seven are still open, and every lane that touches
one is implementing everything that does *not* depend on it.

| # | Question | Status |
|---|---|---|
| B1 | Raise `confirmation_depth` / finality depth above 3 on the unattended profiles (and do `da_attestation_timeout_ms` and the maturity/challenge window move with it)? | Open. Independent of the answer, **no component may exit the process over a deep reorg** — that part is being fixed now. |
| B2 | An automatic DA bond-pool top-up maintainer? | Open. |
| B3 | An automatic role-wallet refill loop from the genesis key? | Open. |
| B4 | Convert terminal quarantines into a `RECOVERING` state that re-derives and auto-clears; should the remaining terminal cases fail `/healthz`? | Open. Independent of the answer, **no new terminal states**, and every new refusal is bounded and self-clearing. |
| B5 | Set `RETENTION_DAYS` on the devnet to the manifest window? | Open. Related to A3/A4. |
| B6 | lucid-evolution Kupmios single-attempt reads: upstream PR + version bump, or a local normalisation seam? | Open. Note the standing rule *prefer dependency updates over compat shims* argues for the upstream PR. |
| B7 | Kupo `--match` narrowing vs unbounded growth? | Open. Related to A4. |

---

## Part C — earlier backlog

The accumulated question list from waves 5b/5c (torn-final drop, reserve
fragmentation and float sizing, the blank `L1_SETTLEMENT_SEED_PHRASE` default,
the 60 s history lease, `prepareSignedHeaderRecovery` still being a live
coverage-proof path contrary to the 2026-09-26 ruling, F2's abandoned-publication
double-landing, the watcher-journey evidence format) is unchanged and recorded in
the program memory. I am not re-raising it here; say the word and I will fold it
into this format too.

---

## What is running while you read this

- **Recovery wave** (8 lanes): provider/slot transients, history owner, DA
  committee, node readiness/router, node fibers, watcher degraded mode,
  devnet-stack endurance maintainer. Six implementers done, reviews in flight.
- **Wave 4** (6 lanes): the sweep-3 lanes above — A1, the boot
  ledger-snapshot misclassification (a transient Ogmios error gets wrapped in
  `DatabaseError`, the classifier rejects it, the history owner latches failed,
  and the process exits), plus the committee attestation/payload protocols, the
  SDK provider seams, and A6.

Still owed after these land, in order: the four never-executed chaos drills
(`restart-cardano-node`, `stop-kupo`, `stop-ogmios`, `kill-public-retained-da`),
a pre-commit formatting pass over the untracked devnet-stack surface, and then
the real proof — a **fresh** deploy via the default `up`, a full journey, and the
drills, on a dist that contains all of this.


---

## Part D — added after the two review waves finished

### D1. Divergent bytes for an *unverified* ("fetched") payload record — latch, or first bytes win?
A divergent submit over a fetched record currently latches `conflicted`.
First-bytes-win looks no worse for liveness (if hostile bytes arrive first the
outcome is terminal either way; if honest bytes arrive first the member keeps
verifiable bytes). It is still a policy call because of **equivocation**: under
first-bytes-win, committee members that received different envelopes for one
header could hold and attest *different* payload hashes, where the latch makes
every member refuse identically.
**Recommendation: keep the latch**, and make the latch clearable by
re-derivation instead — which is B4.

### D2. The supervisor is itself unsupervised
If `superviseServices` throws mid-run, the devnet services are left orphaned and
unsupervised until somebody runs `up` by hand. Neither of the two in-process
alternatives (forced exit after 5 s, or keep a partly working supervisor alive)
recovers without a human.
**Recommendation: this one cannot be fixed inside the supervisor.** Run it under
an OS/container restart policy and make `up` idempotent and re-entrant. That is
the only shape that satisfies "no manual intervention", and it is a deployment
decision, so it is yours.

### D3. B4 (terminal quarantine → RECOVERING) now has a measured price — please rule it
Two findings landed on it this wave:
- `quarantineL1Decisions` marks **every** header that has a persisted decision,
  so the payload becomes `conflicted` and the signatures `post_failed`. As long
  as quarantine stays terminal, **a 2-of-2 committee stays halted no matter what
  else we fix.**
- `availability/retained-payload.ts:20-29` throws for any payload that is not
  `verified`, so a quarantined member **cannot answer availability challenges
  for headers it already signed** — which may expose the DA bond pool.

**Recommendation: rule B4 now.** In the meantime I am implementing only the
narrow, clearly-safe half — serving what you already signed is pure liveness,
because the signature exists and was valid when it was made — and leaving the
general auto-clearing quarantine alone.

### D4. Three provider-identity questions from the slot/provider lane
- A `/health` answer whose tip fields are **entirely absent** (not null, not
  `origin`): transient, so wait, or malformed, so fail closed at startup? The
  code currently waits.
- Should `midgard-node` verify the Ogmios **network magic** the way
  `da-committee-node` already does? **Recommendation: yes.**
- Four call sites still use the old hardcoded tip-age bound rather than the new
  genesis-derived one (the readiness refresher, the merge-gate fallback,
  l1-provider-preflight, the da-bond CLI). Which lane moves them?

### D5. Where should the DA payload warning threshold sit?
LS3-CS warns at **0.5 of the frame**, which on lc1 means roughly 226k plain
UTxOs (sooner if blocks are large). Is half the frame early enough notice?

### Bookkeeping correction
The brief I gave LS3-CS cited "owner question 6" for the structural DA-payload
question, but old Q6 is the lucid/Kupmios one. The structural question is **A6**
above and is genuinely with you now — it was briefly with nobody.

---

## Decisions I took myself rather than ask

Overrule any of these and I will redo the work.

1. **Accepted LV-FB2's deviation** from its brief: it kept the wait and bounded
   the two worker cancelers (30 s) instead of using `Effect.disconnect`. Under
   `disconnect`, a timed-out commitment or merge would keep mutating globals,
   the database and wallet UTxOs while another scope took the single
   L1-control-plane permit — breaking the serialization the permit exists for.
   The cost is that `maxHoldMs` is now a hard deadline only for those two
   cancelers; any other unbounded uninterruptible region under the permit is
   detected by the holder watchdog but not bounded.
2. **Accepted** LV-DA1's two out-of-list file edits (`store/factory.ts`,
   `store/postgres.assert-postgres-decision-retry.ts`) as in-scope.
3. **Accepted** LV-ND1's two semantic changes: expired exact HubOracle evidence
   now leaves `/readyz` at 200 with a `provider_query_degraded` detail for up to
   max(5 min, derived tip bound) instead of going 503 at once; and a lease whose
   release keeps failing is retried at the next acquisition in-process instead
   of being held to its 10-minute TTL. Soundness is unchanged in both.
4. **Accepted LS3-BR's partial retraction** of its own finding: the brief's
   premise (a permanent crash loop on a checkpoint the node can no longer
   acquire) does not match the code — every exact-point capture is at the node's
   current tip, so a wrong-point answer can only come from a race and the next
   attempt captures at a newer head. The classifier half of the finding stands.
5. **Accepted LS3-DAQ's ledger correction**: its first finding is restated as
   "serve gate redundant" and its fifth is retracted, because the real amplifier
   is the `quarantineL1Decisions` marking (D3 above).
6. **Chose option (b) for the foreign-tip latch** without waiting for A1, and
   then, when the reviewer showed (b) does not fix the multi-operator case,
   **launched a read-only adversarial adjudication rather than guessing again**
   — four lenses trying to refute the claim that committing past an unverified
   foreign block is sound, which also has to deliver the spec for (a). I will
   bring you its verdict rather than a third default.

---

## Part E — added 2026-10-01 after the foreign-tip adjudication and sweep 4

### E0. Report first, because it changes the priority order: `signed_intent_undecided` can never clear

**Correction to narrow this, made by the triage pass after I first wrote it:**
the precondition is not the ordinary TTL-miss replacement. Three of the five
finders assumed the replacement sits in journal status
`SubmittedUnconfirmed`; the triage established by reading the code that **no
production path writes that status** — `markLocalFinalizationComplete` has no
caller, and local finalization is deferred until L1 confirmation. A replacement
that was submitted but never landed sits in `SubmittedLocalFinalizationPending`,
which **passes** the arm and reaches revival. So lc1's three TTL-miss
replacements in 40 minutes are **already handled**, and I have retracted those
three findings in their literal form.

What survives is one reachable shape, and it still ends in a permanent brick:
**E is replaced by E'; E' lands and is locally finalized; a short L1 rollback
then removes E' and lands E instead.** That needs a reorg, not just a missed
commit — rarer, but a reorg is exactly the expected transient the directive
names, and `local finalization runs at first observation with no depth gate`,
so a shallow reorg suffices. Nothing is verified by execution yet; wave 5's
first step is an emulator reproduction, and I will retract anything that does
not reproduce.

In that shape:

- `canonical-journal-recovery.ts:171-317` raises
  `SignedIntentReplacementIntegrityError` on every confirmation tick.
- `history-expired-intent-release.decide.ts:184-203` returns the same
  `undecided` wait on every evaluation. The decision is a pure function of state
  that nothing in the system ever changes — no journal row moves, the queue
  still names the sibling, the observer does not change. The 30-L1-block
  escalation emits one log line and nothing else.
- No code can rewind a `Finalized` sibling that did not land:
  `reincludeStateQueueCorrectedBlocks` kind `unlanded` and the correction
  `UNLANDED_STATUSES` **both exclude `ObservedWaitingStability` and
  `Finalized`**. Only a second L1 rollback that removes the sibling clears it.
- On the history side the same error is a `DatabaseError`, which the recoverable
  allowlist rejects, so the history owner fails terminally and the process
  exits — and every restart re-derives the same state. Permanent crash loop.

This is the direct inverse of your 2026-09-26 ruling (*"when the observed tail
is an abandoned own block, revive that block and abandon the other one"*). The
code records the ruling in its comments and then holds instead of acting on it.
**I am treating implementing your ruling as the top P0 of wave 5** — no new
question there. Two sub-points where I do want you:

- **E0a.** A finder argues the comment at
  `state-queue-correction-recovery.ts:434`, *"The native rewind has no
  inverse"*, is **factually wrong**: the abandoned journal keeps its signed
  content and its `nativeMpfReplay`, so the block can be revived exactly the way
  a replaced block already is. If you agree, the fix is a revival rather than a
  new rewind primitive, and it is much smaller. Do you agree?
- **E0b.** `globals.liveness-reasons.ts:36` `currentLivenessReasons` **has no
  reader anywhere**. `/readyz` never surfaces `signed_intent_undecided` or any
  other liveness reason; `/healthz` stays green. Your NB-03 ruling said to
  *"mark the node unready with `signed_intent_undecided`"*. I intend to wire
  `LIVENESS_REASONS` into `/readyz` as soft reasons. Confirm — this is the
  difference between a brick and a *silent* brick, and it affects every hold in
  the program.

### E1. Multi-operator interim throughput  **(the only genuinely blocking one here)**

The foreign-tip gate I adjudicated is sound **only for a single operator**. The
sound multi-operator guard is "do not commit past an unverified foreign block
until it merges or is removed", which costs **one maturity window per foreign
block** — with N operators interleaving, that is a throughput floor, not a
corner case. Options: (a) accept the stall and ship single-operator-only
throughput for the public testnet; (b) require operators to publish payloads
before their header lands, so a peer can verify rather than wait; (c) something
else. This gates the shape of W5-1.

Related and smaller: I verified myself that `SPECULATIVE_COMMIT_BUILD` defaults
**false** (`config.make-config.ts:202-204`), and nothing in the devnet-stack or
lc1 sets it, so the whole foreign-tip surface is **inert on lc1 today**. I have
therefore moved it behind E0 in the ordering. Say so if you want it first
anyway.

### E2. A protocol gap, not a code bug: an honest descendant can have no safe move

Found while adjudicating. Suppose an ancestor block includes an event **early**
— before the event's inclusion window opens — and then that ancestor **merges
unproven** (nobody filed the fault proof in time). An honest descendant now has
two choices and both are punishable:

- **Include** the event: `CrossBlockDuplicateEvent` rule 1 fires, because the
  ancestor already included it.
- **Omit** it: `OmittedDueL1Event` fires, because the omitted predicate in
  `transition-trace/timed-history.ak` computes omission from the L1 event and
  the window **and ignores ancestor inclusion entirely**.

So the honest operator is slashable either way, through no fault of its own.
This is a specification question, not something I should patch: either the
omitted predicate must account for ancestor inclusion, or early inclusion must
itself be the provable fault. Which?

### E3. Catch-up after *correct* pruning

A node that was down long enough comes back to a merged foreign block whose
payload has been pruned everywhere — correctly, by every retention rule. It then
cannot derive the confirmed post-state, and there is no path that lets it. This
is the same shape as the W3 watcher wedge, one level up: retention working as
designed makes rejoining impossible. Do you want (a) a bounded "archival" role
that retains past the manifest window, (b) a state-snapshot sync path, or
(c) an accepted operational rule that a node down longer than the window must be
redeployed from genesis?

### E4. A pre-existing soundness hole, for your awareness

Independent of any lane: `select-authenticated-foreign-base-candidate` checks
**only `utxosRoot`**. `transactionsRoot`, the three event roots,
`validationTracesRoot`, `transitionTraceRoot`, `eventToStepRoot` and the counts
are compared against nothing on the build path. Combined with the SQ5/#683 hole
(the link arm does not bind `removed_node.operator_vkey`), a fraud token for a
foreign block lets the slash name **any** operator. Both are at HEAD, neither
was introduced by this program. Do you want them in this program's scope or
filed for the fault-proof program?

### E5. Two more defaults I am taking — override if you disagree

- **E5a.** A `Failed` local-finalization job whose journal is already
  `Finalized` is currently classified `refuse` and blocks startup forever. The
  2026-09-30 note had this as an owner question; I am treating the liveness-first
  directive as deciding it. A `Finalized` journal proves the last durable step
  committed, so wave 5 classifies it `complete`. It also gets the real fix,
  which is in steady state, not at startup: when the journal is already
  `Finalized`, skip the re-finalization and close the job, so the node stops
  re-arming the job to `Running`, failing, and marking it `Failed` once per
  commit tick. (The restart refusal is only the second symptom of that loop.)
- **E5b.** Reversing the local finalization of an own `Observed`/`Finalized`
  block, plus its unmerged descendants, when the authenticated queue shows a
  replaced sibling holding its base slot. I read this as already ruled by
  whichever-lands-wins plus rollback-redesign D2 ("statuses below settled are
  reversible"), so I am proceeding. A **merged** block is never reversed, and
  two landed siblings stay a refusal — held, with the process up, not an exit.

### E6. Settled by reading after I first wrote E1: a node cannot follow a chain that contains another operator's block

I went looking for a path that applies a merged **foreign** block's effects to
the confirmed ledger. There is none, and the reason is structural rather than a
missing branch:

- `landedUnfinalizedMerges` walks back from the confirmed state and **breaks**
  at a header with no local journal
  (`merge-to-confirmed-state.landed-unfinalized-merges.ts:82`), so a merged
  foreign block is skipped, not finalized.
- The confirmed ledger is not rebuilt from payloads. It is rebuilt by folding
  **journal deltas**: `materializeFromBase`
  (`transactions/state-queue/confirmed-ledger-snapshot.ts:196-262`) recurses
  through each block's *parent journal*, and when the parent journal is absent
  it fails with *"Pending-finalization ledger delta base is not confirmed and
  its parent journal is missing"*. A foreign ancestor has no journal, so the
  fold cannot cross it.
- `finalizeMergesLandedThrough` logs *"merges stay blocked until it succeeds"*
  on that failure, and the same rows re-derive on every restart.

So the moment a second operator's block merges, this node's confirmed ledger
stops advancing and **every later merge is blocked permanently**. That is not a
throughput trade-off; it means multi-operator operation is **not implemented at
the ledger layer**, and it changes E1: option (a) is not "accept a stall", it is
"single operator only, by construction". Building a journal-equivalent from a
foreign block's retained payload is the missing piece, and it is a design
question, not a patch.

Caveats, stated plainly: this is established by **reading**, not by running, and
it is inert on single-operator lc1. I have not checked whether any
state-reconciliation verb can repair it out of band — and if one can, that is a
manual repair, which the directive rules out anyway.

## Part F — added 2026-10-01 after the LS3 and recovery waves finished their fix passes

### F1 (no decision needed, for the record) — the foreign-tip soundness blocker was caught and fixed
The LS3-FT reviewer refused the first implementation with a blocker, in its words:
*"The window gate sends every unresolved verdict through the same check, including
`invalid`. The node already has positive evidence that such a foreign header is
malformed, and it now commits on top of that block whenever the block's window holds
none of its pending events. Before the fix it refused. Under the descendant-culpability
ruling of 2026-09-26, an honest operator never builds on a fraudulent block, so this
buys liveness by weakening a soundness refusal."*

That is exactly the trade the brief forbids, and it is now closed: `invalid` and
`foreign_event_present_requires_finalization` are unconditional refusals that the
window gate never lifts and the time-based prune never deletes, and the
root/count consistency check moved ahead of the "payload missing" return so a
non-empty root with a zero count is classified `invalid` from the header alone.
Two mutations (ignore `unconditional` in `firstBlocking`; let the prune ignore the
verdict) each fail two tests. 64/64 across four files.

**I am commissioning an independent verification pass on it anyway** — the wave's
pipeline runs two reviews and then one fix, so no reviewer has read the fixed
source. Same for the other four landed fixes.

### F2 — E6 is now confirmed three times over, from three different files
My own read found one refusal. The LS3-FT reviewer and then its fixer found two more.
A peer operator's block cannot be settled by this node in any shape:
1. the T2 foreign-event window gate (a pending event in the peer block's window);
2. the overdue-Awaiting refusal in `src/mpf/event-window.resolve-included-forced-transaction-entries-for-window.ts`;
3. the foreign-ledger base refusal in `resolve-commit-base-ledger-entries.ts`;
and underneath all three, the confirmed ledger is rebuilt by folding *journal* deltas
through parent journals (`confirmed-ledger-snapshot.ts:196-262`), and a peer's block
has no journal here, so the fold fails permanently.

This is not three bugs. It is one deliberate design boundary stated three times.
It does not change E1, it settles it: **the current code is single-operator by
construction.** Multi-operator liveness needs a DA client plus authenticated
foreign-ledger finalization — new subsystems, not a patch. E1 is still the one
blocking question, but it is now a scoping question, not a diagnosis question.

### F3 (new finding, needs a decision only on priority) — a user can halt block production permanently by filling the mempool
The LS3-CS fixer escalated this rather than fix it, correctly: the fix is outside its files.

- The commit planner's DA budget counts only transaction bytes plus 128 bytes per
  transaction (`commit-block-planner.plan-earliest-commit-scheduler-due-work.ts`
  around lines 228 and 254). It never sees the base-ledger UTxO list, the second
  copy of each transaction, CEK program material or traces.
- The pre-submit frame check sees all of it and refuses.
- Selection is deterministic. So a selection that passes the planner and overflows
  the frame is picked again, and refused again, on every tick — forever.

A user does not need to be malicious to reach it, only to submit enough traffic while
the ledger is large. There is no operator action that clears it short of waiting for
the ledger to shrink, which it does not. This is a permanent block-production halt
triggered from outside the operator's control, so I am treating it as P0 and
commissioning the fix now (bounded deterministic step-down with a floor at an
empty block, the existing refusal kept as the last guard). **Tell me if you want it
ranked below the signed-intent latch.**

Two parts of it I am *not* doing without an answer: the commit worker runs in its
own thread and the Prometheus exporter is registered only on the main thread, so a
gauge for frame pressure needs a projection field on the worker output. Do you want
frame pressure on `/metrics` and in the `/readyz` detail, or is the step-down enough?

### F4 — the readiness blindness is now confirmed by an independent reviewer
I reported this as HOLD-03 from my own read. The LV-FB1 reviewer found it separately
and listed what it hides: `retention_l1_view_stale`, the state-queue correction
rewind conflict, `signed_intent_undecided`, `history_signed_intent_release`,
`operator_watchdog_manifest_unverified/mismatch`, and the native MPF owner's
restart-exhausted and recovery-pending reasons. Its scenario, verbatim:
*"heldSchedule/heldRestart then hold block commitment, merge, settlement and the
speculative builder/submitter, possibly for good. /readyz still reports ready=true,
so an unattended private testnet stalls with only a counter and an escalation log
line as evidence."*

The one-line fix lands in a file another lane is editing right now, so it is queued
behind it rather than open. No decision needed; recorded because it is the single
highest-value line of code in the program and I do not want it to look optional.

### F5 — a hard ceiling on how long the devnet can run, which I need a number from you on
This falls out of F3 and I had not seen it before today.

Every DA payload carries the **whole base-ledger UTxO aggregate**, not a delta. The frame limit is
fixed (about 64 MiB inner, a little less in the zstd mode). So there is a number of L2 UTxOs at
which **no block fits at all** — not even an empty one. Past that point block production stops
permanently and there is no repair: the ledger does not shrink by itself, and the node cannot
commit the block that would shrink it.

The directive says "running indefinitely". With continuous deposits and few withdrawals, the UTxO
set only grows. I have asked the DA-FRAME lane to measure the per-entry cost and state the exact
UTxO count at which an empty block stops fitting, so we are arguing about a number rather than a
feeling. Expect it in this wave.

Two questions, and I am not guessing at either:
1. Is a payload that carries the full UTxO set per block the intended design, or is a delta or a
   sharded encoding in scope? This is architecture, not a liveness patch, so I will not touch it
   without you.
2. For the private testnet specifically: do you want a guard that goes **unready** as the ledger
   approaches the ceiling, so the halt is announced before it happens rather than discovered
   after? That is cheap and I can do it in the same lane if you say yes.

### F6 — a cardano-node restart may be terminal for us, and we still cannot say whether it is
I went looking for this myself while the wave ran, because it is one of the four chaos drills we
have never executed. What I found by reading is worse than I expected, and what I could not find
is the part that decides it.

**Established by reading the code.** There are three independent Ogmios JSON-RPC clients in the
tree, and all three reject an error envelope as a plain `Error`:
- `demo/midgard-node/src/l1-tx-order-carriage.open-ogmios-session.ts:90`
- `demo/da-committee-node/src/l1/provider.ogmios-rpc-session.ts:74`
- `demo/da-committee-node/src/l1/state-queue-replay-provider.open-rpc.ts:256`

A plain `Error` is not recoverable: `isRecoverableHistorySourceFailure` admits only
`L1SourceUnavailable`, `HistoryRecoverySuperseded`, a transient `SqlError` and `TimeoutError`.
Exactly one consumer reclassifies, `l1-ledger-snapshot.ts:85-106`, and only for local-state-query
acquisition codes 2000, 2001 and 2003. Its comment is explicit that this is deliberate:
*"Any other code, the JSON-RPC protocol ones included, refuses this request itself and stays a
refusal."*

By contrast every *socket* shape is already transient — failure, close, open timeout, send
failure all become `L1SourceUnavailable`.

**So the whole question is which shape a cardano-node restart produces.** If Ogmios closes the
client socket, we already recover. If Ogmios stays up and answers with a JSON-RPC error, we treat
a routine restart as a genuine fault. A cardano-node restart is not exotic on an unattended box:
OOM, host reboot, a docker restart, an upgrade.

**I cannot answer it from what we have, and I am not going to guess.** lc1's cardano-node never
restarted, so its logs hold no instance. Ogmios is pinned at v7.0.0 (b3a830a1) and the
documentation page the code itself cites does not state the behaviour. The `restart-cardano-node`
drill, which would settle it in one run, is one of the four we have never executed.

**What I am doing about it, which needs no answer from you:** making the classification
shape-independent, so it is correct either way — any Ogmios answer that means "the node is not
available right now" becomes transient in all three clients, while genuine semantic refusals
(malformed request, no intersection, era mismatch) stay terminal, tested in both polarities with
injected error envelopes. Then the drill confirms it on a live stack rather than deciding it.

One file in that set, `l1-tx-order-carriage.open-ogmios-session.ts`, belongs to a lane whose fix
is in flight, so this is queued behind it rather than started. Flagging it because it is the kind
of thing that looks like a detail and is actually the difference between a devnet that survives a
host reboot and one that does not.

### F7 — one deliberate deviation from a brief, which the implementer flagged rather than hid
In lane LV-ND1 I asked for the commit skip to fire on **any** active pending-finalization record.
It was implemented to fire only on a `pending_submission` journal that carries an
`intended_tx_hash` — the same condition as `assertNoUnreconciledSignedSubmission`.

The narrowing is defensible and the lc1 evidence backs it: all three refusal windows we have
(journals f8fa296f, a534b306, 712d7e7c) were signed intents still in `pending_submission`. A wider
skip would also hold commits while a journal sits in a status that cannot conflict.

I am accepting it, because a narrower skip means *fewer* holds and the signed-intent case is the
one that can actually double-submit. Recording it because it is a deviation from what I asked for
and you should see it rather than find it later. Say the word if you want the wider condition.

### What is running right now
- **Wave 6** (`wf_7e2fbc61-31c`): SI-RELAND (the signed-intent latch), DA-FRAME (the F3 frame
  halt), and four read-only verification lanes over the fixes no reviewer had read.
- **Wave 7** (`wf_ad968ffa-a0f`): READY-VIS — publishing liveness holds on `/readyz`, with the
  stale-incident clear in the same change so a published reason can never become a permanent
  false red, and with an explicit check that an unready node still accepts user deposits,
  withdrawals and L2 submissions.
- Still in flight from earlier waves: `fix:LV-HO1`, `fix:LV-WA1`, `review:LV-WA1`, and wave 5a's
  LF-JOB and HIST-WAIT.
- Queued behind them: the Ogmios classification lane (F6), OBS-BOUND, the four never-executed
  chaos drills, one formatting pass, and the final proof — a fresh deploy, a full journey and the
  drills on a dist that contains all of this.
