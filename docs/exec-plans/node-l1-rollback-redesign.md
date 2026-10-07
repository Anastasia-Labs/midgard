# Midgard node: L1 rollback redesign

Status: PROPOSED, revision 4, 2026-09-26. Owner rulings D1–D3 in. P0
verifications done (§7). §9 readiness gates closed by three code audits; the
ticket breakdown is in §11. Nothing implemented. Tickets NOT yet published.

Audit sources, copied to `docs/exec-plans/node-l1-rollback-audits/`:
`p0-deletion-audit.md`, `p0b-mempool-reinjection.md`,
`p0a-mpf-retained-roots.md`, `schema-draft.md` (full DDL),
`projection-acceptance.md` (per-projection criteria tables),
`recovery-callers-and-chaining.md`.

Path convention below: unprefixed paths are `demo/midgard-node/src/`.

## 1. Problem

The node acts on L1 tip data, as the protocol requires (§3), and pays for it
with ~20k lines of bespoke rollback code: per-block undo records, per-tx ledger
before/after receipts, orphan/incarnation bookkeeping, recovery plans, a
six-state pending-finalization machine, signed-header recovery with a coverage
proof, and a state-queue correction rewind. Every feature
adds state a rollback must undo by hand.

Live holes today:

- Tx orders have no rollback handling: Kupo poll, insert-only
  (`fibers/fetch-and-insert-tx-order-utxos.ts`, `database/forcedTransactions.ts`).
- Own commit counts as landed at one Kupo sighting at depth 0; deposits marked
  projected and DA published from that sighting (`fibers/block-confirmation.ts`).
- Merge local finalization has no inverse (`transactions/state-queue/merge-to-confirmed-state.ts:1232-1349`).
- User deposit/withdrawal submissions confirm at one `transactionStatus`
  sighting, no rollback path (`transactions/event-history-submission.ts`).
- `fibers/project-deposits-to-mempool-ledger.ts:158-160` still uses `new Date()`
  as the cutoff on the CLI and `reconcile` paths (the production path already
  uses the tip slot, `services/event-history-runtime.ts:196-197`).
- `deepRollbackPolicy` is validated, never implemented.
- `l1-event-history-initialization.ts` (141 lines) is dead.
- `/readyz` does not consult state-queue topology; the address-wide summary
  misses orphan nodes (`services/state-queue-topology.ts:85-110` vs `:149-224`).
- **Latent wedge under the default MPF engine.** Every commit records a signed
  intent (`workers/commit-block-header/submission.ts:687-703`), but the only
  path that releases an expired signed commit,
  `services/history-expired-intent-release.ts`, runs only under
  `MPF_ENGINE=architecture_g` (`services/event-history-runtime.ts:79-90,155-168`).
  The default is `legacy` (`services/config.ts:1088`), and no tracked config or
  compose file sets the engine. Under `legacy` an expired signed commit stays
  active forever and the active-journal guard
  (`database/pendingBlockFinalizations.ts:1996-2012`) refuses every later
  commit. Which engine the preprod runs used is unverified. This is a hotfix
  candidate independent of the redesign (§11, ticket 0).
- Two recovery paths coexist and both run in production from
  `commands/listen.ts:238`: expired-intent release (implements the
  "whichever lands wins" ruling, TTL-gated, unlanded rows only) and the older
  signed-header coverage path (`services/history-signed-header-recovery.ts`,
  runs on every pending reconciliation with no engine gate). Issue #759 removes
  its uncapped journal retention hold while preserving recovery itself.
  They never act on the same row. The coverage path is the only thing today
  that handles a *landed* own block that is later rolled back together with
  its deposits' funding. It can be deleted only once the reconcile loop (§4.E)
  handles orphaned deposit funding for landed or abandoned own blocks of any
  shape. Delete `prepareSignedHeaderRecovery` and its tests in the same change
  that lands that universal replacement handler; the two must never run side
  by side.
  The retained coverage path still requires the signed validity-start boundary:
  once ordinary pruning passes it, automatic recovery can fail closed and leave
  the node not-ready. Removing the hold does not close this recovery gap; ticket
  14 must replace the coverage requirement for the full any-shape matrix.

## 2. What mature systems do

| Principle | Optimism | Nitro | Kupo / db-sync / Djed | Hydra |
|---|---|---|---|---|
| L1 facts keyed by L1 point; rollback = truncate | engine labels + reset | message log, batch delete | `created_at`/`spent_at`; DELETE > point | chain-state history |
| Own signed material never rolled back | (unsafe blocks dropped, not bonded) | sequencer feed | n/a | "L2 state never rolls back" |
| One reset path for restart / reorg / inconsistency | `sync/start.go` | reorg to message count | resume = rollback to intersection | |
| Reorg detected by commitment comparison | parent hash, L1 origin hash | inbox accumulator | block hash at target | |
| Own-tx resubmission is a stateless reconcile loop | batcher `sync_actions.go` | nonce-keyed queue | wallet retries exact bytes | re-posts from snapshot |
| Inputs you commit to are read with a lag | seq conf depth 4 | delayed inbox at "safe" | | `depositActivation` |

## 3. Protocol and platform facts that constrain the design (verified)

1. `validate_commit_block_header_v1` (`onchain/aiken/validators/state-queue.ak:1030-1235`)
   spends the tail + the operator's active-operators node; references
   scheduler, correction lock, hub oracle, confirmed-state root and head node.
   Never spends/references any deposit, withdrawal or tx-order UTxO.
   - A commit landing on a fork where an included deposit never re-lands is a
     provable `fabricated-deposit` against us; a deposit appearing only on the
     new fork inside an already-committed window is a provable `OmittedDueDeposit`.
   - Reference inputs also kill a commit: a merge re-creates the root, a DA
     attestation re-creates the head node. Our own merge/attestation invalidates
     our own in-flight commit (`workers/commit-block-header/state-queue.ts:118,161`).
2. `inclusion_time = valid_to + event_wait_duration` (`user-events.ak:197`; 60 s
   everywhere). `end_time` = commit tx upper validity bound. Node caps
   `end_time ≤ includedThroughMs + W − 1` (`services/history-commit-window.ts:13-25`).
3. A commit spends the tail, so a commit built on a rolled-back parent can
   never land. The tail is the nonce.
4. History-list `InsertOrder` spends the predecessor node; continuations
   preserve the Value. A deposit's value lives in a list node that is respent
   by ordinary insert traffic → reference-input pinning is not viable.
5. Event identity is not derivable from UTxOs alone: event id = consumed nonce
   outref; `inclusion_time` = admitting tx's `invalidAfter`; retirement reason
   is redeemer-only. The follower must keep the **decoded transaction** (inputs,
   reference inputs, collaterals, mint, withdrawals, redeemers with purpose and
   index, validity interval, tx index, validity flag) for every tx touching a
   tracked address. Raw body bytes are needed only for creating-body
   verification of untracked txs, which go through fetch-by-hash anyway.
6. Forced-tx carriage lives at the **order creator's wallet** (non-enumerable),
   CEK program material is matched by **payment credential** only, and
   reference inputs may predate the follower. These need a by-hash content
   fetch, not a tracked-address store.
7. cardano-node **does not re-inject** rolled-back txs into its mempool
   (`ouroboros-consensus@7d630e8e5 Mempool/Init.hs:64-83`, `Update.hs:579-588`).
   Caveat: a peer that never adopted the abandoned block may still hold and
   forge the tx. cardano-wallet resubmits client-side for the same reason.
8. Neither ledger MPF store has disk GC (`mpf/root-view-store.ts:~596-615`
   node-key `del` is a no-op). `restoreRetainedRoot`
   (`services/mpf-native-owner/service.ts:1567-1650`) is a full index walk +
   child restart. Native resident cap: 2M records / 512 MiB
   (`service.ts:35-36`). Only `architecture_g` has a native owner; `legacy`,
   `overlay` and `event_flat` have none (`services/config.ts:255,1083-1088`).
9. **One active own commit is already the invariant.** Six guard layers
   enforce it (see `recovery-callers-and-chaining.md` Q2a). The optional
   speculative builder (`fibers/speculative-commit-builder.ts:670-800,996-1182`,
   off by default) builds the next block before the parent lands and
   **submits** only after. Retaining the invariant regresses nothing.

## 4. Target architecture

| Kind | Store | On L1 rollback |
|---|---|---|
| A. Facts observed on L1 | slot-tagged, insert-only | one truncation |
| B. Operator's own signed material | append-only | never touched |
| C. Content-addressed immutable data | cache | never touched |
| D. Everything else | projection of A+B+C | recomputed |

### A. One L1 follower feeding a truncatable fact store

- Reuse `l1-event-history-chain.ts` (Ogmios ChainSync; continuity checks;
  intersection must be a requested point; fail closed past retained path;
  persistence-ack backpressure; tip-height bracketing) and
  `l1-event-history-source.ts` session authentication. k = 2160 retained points.
- Bootstrap: keep `l1-ledger-snapshot.ts` (LSQ `acquireLedgerState` at an
  exact point, tip checked before and after, 15-min deadline). **Its decoder
  drops reference-script bytes (`l1-ledger-snapshot.ts:114`); fix before it
  feeds the store.** CEK material at bootstrap: Kupo credential-pattern match
  as the index, each hit verified by LSQ utxo-by-outref at the bootstrap
  point. Bootstrap point must be ≥ protocol activation (completeness
  assertion; the correction observer relies on it).
- Tracked set (14 items, `schema-draft.md` §E): hub oracle; deposit and
  withdrawal lists + retention; tx-order address + policy; state queue and
  `stateQueueAuthValidator` address when distinct; scheduler; registered /
  active / retired operators; correction lock; settlement, reserve, payout;
  fraud-proof; DA params + attestation; reference-script publication; own
  wallets (main, merging, reference-scripts); CEK material **payment
  credential**. Test-only always-succeeds addresses excluded on public deploys.
- Tx qualification: a tx (valid or phase-2-failed) is stored iff it (a)
  creates a tracked output (failed: collateral return only), (b) spends a
  tracked outref (collaterals if failed), (c) mints/burns under a tracked
  policy, or (d) **references** a tracked outref. (d) is stricter than Kupo
  and is what lets the observer and carriage see commit/reference-only txs.
  All four are decidable at follow time from `l1_outputs` plus the current
  block stage.
- Tables (full DDL, indexes, rollback SQL and six invariant queries in
  `schema-draft.md` §B–C; the schema is a design decision, summarised here):
  - `l1_blocks(slot PK, hash, height, parent_hash)`.
  - `l1_txs(tx_hash PK, block_slot, block_tx_index, is_valid, inputs[],
    reference_inputs[], collaterals[], output_count, has_collateral_return,
    mint jsonb, withdrawals jsonb, redeemers jsonb{purpose,index,cbor},
    invalid_before, invalid_after)` + `l1_tx_mint_policies` side table.
    Decoded columns only; no fee, signers, certs, metadata, witness scripts.
  - `l1_outputs(tx_hash, output_index PK, address, payment_cred,
    payment_cred_is_script, lovelace, assets jsonb, datum_hash, datum,
    script_ref_type, script_ref, created_slot, created_tx_index, spent_slot,
    spent_tx, bootstrap_slot)` + `l1_output_assets` side table for
    unit/policy lookups. Bootstrap rows have `created_slot NULL`.
  - `l1_follower_cursor(slot, hash, origin_slot)` replaces Kupo `/checkpoints`.
  - `l1_tx_bodies(tx_hash PK, cbor)` insert-only, filled only by fetch-by-hash (kind C).
  - Rollback to p: null `spent_*` after p first, then delete outputs/txs/blocks
    after p, in one transaction. Resume = same op. Any future GC must respect
    the same ordering (SET NULL foreign keys).
  - Live-at-slot predicate: `(created_slot IS NULL OR created_slot ≤ s) AND
    (spent_slot IS NULL OR spent_slot > s)`.
  - Spent rows are read by the observer (`*@txHash`), depth checks and the
    never-reuse rule; retain them for ≥ k blocks. Prune older than k behind
    tip once derived tables (key set) hold what they need. Never unbounded.
- Kupo stays as a **non-authoritative content service** (by-hash tx bodies,
  credential-pattern bootstrap index). Every "what is live now" read moves to
  the fact store (179 provider-read sites, 140 outside `commands/`).
- Fact-store-backed Lucid `Provider` for the UTxO-query subset (`utxosAt`,
  `utxosAtWithUnit`, `utxoByUnit`, `utxosByOutRef`, datum, checkpoints
  equivalent); Ogmios for submit/evaluate/params; `native-ledger.ts` reward
  reads unchanged.
- Kept as validators over A: `l1-event-history-transition.ts`,
  `-transaction.ts`, `-list-replay.ts`, `-reference.ts`, `-activation.ts`,
  `-provenance.ts`. They become pure functions from `l1_txs` rows to typed
  list transitions. Correction observer: with a complete tracked scope, an
  input absent from `l1_outputs` is soundly "not a lock or proof" (today a
  missing Kupo match throws, `services/state-queue-correction-observer.ts:1000-1020`).

### B. Append-only operator intent store

- Own block = everything `pending_block_finalizations` carries today: members
  (tx, deposit, withdrawal, forced), transition trace, event_to_step,
  validation traces + witnesses, CEK sidecars, withdrawal classification,
  forced-tx verdicts, ledger delta, native MPF replay log + owner binary sha,
  deployment marker, **L1 origin point**, window, signed roots.
- Signed L1 tx = bytes, hash, purpose, spent inputs, **reference inputs**,
  **`valid_from`/`valid_to`**, wallet, and **dependency edges** to intents
  whose outputs it spends or references.
- **One active own commit at a time** (today's invariant, §3.9). A child is
  **signed and submitted** only after its parent has landed; speculative
  *building* before that is preserved.
- Never truncated. Pruned when (a) merged and past the settlement horizon, or
  (b) dead and max(`valid_to` slot, landed slot) is k-deep.
- User deposit/withdrawal submissions are a second intent family in B.

### C. Content-addressed immutable data

Foreign DA payloads by header hash (verified against all 8 roots + counts,
write-once, `VerifiedEmpty` never upgraded) and tx CBOR by hash
(`l1_tx_bodies`). A projection lacking one **defers**, never guesses.

### D. Projections

Acceptance-criteria tables (inputs, output, today's code, invariants,
rollback behaviour, test, dependencies) for every item are in
`projection-acceptance.md`; the ticket in §11 that owns each one carries its
criteria. Heads: local / landed / settled (depth ≥ `confirmationDepth`: 30
preprod-public/mainnet, 3 devnet) / confirmed (merged, at depth). Ruling D2:
every status below `settled` is reversible; the API exposes the head level
(today `commands/tx-status.ts` has no head field).

Decisions resolved by the audits (were open questions):

- **Orphaned-origin ids are readmitted.** A deposit whose origin was orphaned
  by a fork re-lands with the *same* nonce outref and therefore the same id.
  Refusing it would strand the user's legitimate deposit. Never-reuse applies
  to canonical (live or retired) origins only, which is what
  `l1-event-history-provenance.ts:157-197,264-268` does today. Rev-3 text
  reversed.
- **Foreign inclusion of our candidates**: an event in a verified foreign
  block that has landed becomes `included(foreign, h)`, reversible below
  `settled` per D2. Today it blocks as
  `foreign_event_present_requires_finalization`
  (`workers/t2-foreign-event-reconciliation.ts:270-282`).
- **Correction-removed tail refusal** must key on correction removals only.
  Today `workers/commit-block-header.ts:986-1003` also refuses
  replacement-abandoned journals, which conflicts with revive (§4.E-1).
- **Landed-block local effects** live in `workers/utils/commit-submission.ts:175-380`
  (not merge-to-confirmed-state); undo is spread over
  `services/state-queue-correction-recovery.ts:593-660`,
  `-ledger-restore.ts` and `native-mpf-local-finalization.ts`.

Defaults adopted pending owner objection (§6):

- Post-admission correction rollback = **integrity halt** on every engine
  (today: record + restore, `state-queue-correction-observer.ts:551-585`;
  halt only under architecture_g after a rewind).
- Unhealthy topology (orphan, malformed policy UTxO, cycle, > 10 000 nodes)
  ⇒ `/readyz` not-ready **and** no proposals; reads keep serving.
- MPF root tiers: tier 1 (`SwitchRoot`, resident set, mark/sweep) for
  `architecture_g` only; tier 2 (restore confirmed root + replay saved
  per-block event logs) engine-uniform, because it is the only path for
  corrections at maturity depth and for the three ownerless engines.

### E. One reconcile loop

Runs on every L1 event and on a timer; serialized; the single L1-facing
scheduler. Fibers become pure proposers. **Retained:** the DB-level
cross-process single-writer lock (`database/stateQueueMutationLeases.ts`, 476
lines, also held by `mpf-audit`, `reconcile` CLI, timeout correction) and one
owner of the Lucid wallet / UTxO override view. The in-process control-plane
semaphore goes. Wallet view = fact store − inputs of live intents + their
predicted change; recomputed every pass; override cleared before every build.

1. Landed chain from A, at every depth (not only after TTL, not only at tip
   depth). If one of our blocks landed, follow it even if abandoned by
   replacement; a correction-abandoned journal is never revived. Revive
   requires member txs absent from immutable and the MPF marker CAS.
2. For each live intent, can it still land? All of: spent inputs unspent on
   the canonical chain; reference inputs still present; dependency intents
   landed or live; **wall-clock-extrapolated slot** (`local-ledger-slot.ts`)
   inside [`valid_from`, `valid_to`]; for a commit also the append fence,
   no expired unattested suffix, scheduler still names us, and signed event
   roots equal roots recomputed from the new chain (origin still canonical ⇒
   equal).
3. Can land → resubmit exact bytes (nothing in the node does this today).
   Cannot → dead; never resubmit. Undecodable / TTL-less → never replaced,
   reported once (present today).
4. Landed tip ≠ derived → replacement spending the same tail via the full
   build path. Today's release hands off to the commit fiber, which builds on
   whatever the tail is now; pinning to the abandoned tail D is not required
   (if a foreign block took D, building on the new tail is correct).
5. Working state = ledger at landed base + the one live own commit + mempool
   revalidated; rejection transitive, batch-atomic, terminal.
6. Multi-tx workflows with on-chain locks stay sub-state-machines driven by
   the loop.
7. Backoff, attempts/last_error per step, stuck-intent alarm, circuit breaker.
8. Effects keyed by header hash, idempotent: DA publish, gossip. A revived
   block must produce its payload before the attestation timeout.

Gap of `history-expired-intent-release.ts` against steps 1–4: step 1 PARTIAL
(TTL-gated, unlanded rows, tip depth), step 2 MINIMAL (checkpoint slot ≥ TTL
only), step 3 ABSENT, step 4 PARTIAL (delegated to the commit fiber). Keep
`eventHistoryRecoveryPlans.ts`, `history-dependent-recovery.ts` and
`assertNoUnreconciledSignedSubmission`; they are shared.

## 5. Reorg hazards the protocol allows, and what mitigates them

1. **Don't resubmit dead intents** — viable (§3.7). Residual: non-adopting
   peers; bounded by commit TTL.
2. **Reference-input pinning** — not viable (§3.4). Dropped.
3. **`event_wait_duration` raise + horizon lag d** — D1 ACCEPTED. Cap must be
   `(tip − d) + W − 1`; d ≈ W / 20 s (W = 5 min → d ≈ 13).

## 6. Owner rulings and open defaults

Ruled 2026-09-26: **D1** raise W with horizon lag d. **D2** below `settled`
reversible; API exposes head level. **D3** node only.

Defaults adopted in this revision; object before ticket 11/7/22 starts:

- **O1** Post-admission correction rollback = halt on all engines.
- **O2** Unhealthy topology stops proposals and fails `/readyz`.
- **O3** Tier 1 roots are architecture_g only; tier 2 engine-uniform. If the
  three ownerless engines are being retired, tier 2 shrinks to one engine.
- **O4** Spent fact rows pruned past k behind tip; the never-reuse key set is
  a separate append-only table.
- **O5** Legacy-engine wedge hotfix (ticket 0): release expired signed
  commits on every engine, or set `MPF_ENGINE` explicitly in every deployment.
  Which engine the preprod runs used needs checking against the deploy env.

## 7. P0 results

| Item | Result |
|---|---|
| P0-a MPF retained roots | FEASIBLE-CHEAP. No GC exists; need `SwitchRoot` + bounded mark/sweep. |
| P0-b mempool re-injection | DOES NOT RE-INJECT. Caveat: non-adopting peers. |
| P0-c pin target | none. Closed. |
| Deletion audit | 30 whole files, 14,638 lines deletable once all projections exist (P4 4,389 + P3 10,108 + dead 141). `stateQueueMutationLeases.ts` stays. `state-queue-topology.ts`, `state-queue-correction-observer.ts`, `t2-foreign-event-reconciliation.ts`, `operator-wallet-view.ts` rewritten in place. Coverage path alone: 1,056 source + 1,089 test lines. |
| One-active-commit retention | No regression (§3.9). Min inter-commit gap today ≈ parent inclusion (~20 s) + 2 s poll + 1 s tick + build. |
| Devnet fork drill | `t1-recover.sh` has no evidenced passing live run (no attestation, no CI, no readiness entry). Only unit and fixture tests exist. |

## 8. Phases

- **P1** fact store (schema, follower ingestion, bootstrap, fork simulator,
  by-hash content fetch). Shadow run; projection diff vs existing tables.
- **P2** consumers onto the fact store, expand–contract, one batch per PR.
  Contract gate: zero live-state Kupo reads outside the content fetch,
  grep-enforced.
- **P3** intent store; reconcile loop under the simulator; fibers →
  proposers; delete both recovery paths, the finalization machine and the
  semaphore. Gate: first evidenced pass of the devnet fork drill.
- **P4** per-block roots, landed-block effects, `confirmed_ledger` as
  projections; delete receipts/repair/plans/rewind/restore. Gate:
  correction-removal drill.
- **P5** W raise + horizon d.
- **P6** tx-status head level; user-submission `settled`; docs.

## 9. Readiness

All four rev-3 gates are closed: recovery paths mapped (both live; deletion
sequenced after E step 1 covers landed→rolled-back), schema drafted from
consumers, projection acceptance tables written, one-active-commit retention
verified non-regressing. Remaining owner input is the five defaults in §6,
none of which blocks tickets 1–6.

Estimate: **10–14 weeks calendar**, ±40%, at the current agent workflow;
critical path is tickets 1→2→4→7→22→23→24→26. P1–P2 can run alongside preprod
work (shadow, no behaviour change). P3+ must not cut over mid-acceptance.

## 10. Changes from revision 3

- Coverage path is live in production, not superseded; deletion re-sequenced.
- Legacy-engine expired-commit wedge found; hotfix ticket 0 added.
- Orphaned-origin never-reuse reversed (must readmit).
- Landed-block effects, tail refusal, deposit cutoff, tier-2 line refs corrected.
- Schema fixed: decoded columns, four-way tx qualification incl. reference-only
  txs, 14-item tracked set (+ reserve/payout/settlement, auth validator, CEK
  credential), bootstrap decoder ref-script bug, `l1_tx_bodies`, cursor table.
- "Built only after parent landed" → "signed/submitted only after".
- Five owner defaults recorded (O1–O5); foreign-inclusion and observer
  absent-rule decided.
- Ticket breakdown added (§11).

## 11. Ticket breakdown

Thirty tracer-bullet tickets. Each is a vertical slice with its own test;
"Blocked by" lists only genuine gates. Tickets in the same tier can run in
parallel. Acceptance criteria per projection are in `projection-acceptance.md`
and are copied into the ticket body at publish time.

### Tier 0 — independent

| # | Ticket | Blocked by | Delivers / verified by |
|---|---|---|---|
| 0 | Hotfix: release expired signed commits on every MPF engine | none | A `legacy`-engine node whose signed commit expires abandons it and commits again. Emulator test with `MPF_ENGINE=legacy`; today it wedges. |
| 1 | Fact-store schema, migration, rollback SQL, invariant checks | none | Migration creates the six tables; `rollbackTo(p)` passes the six invariant queries; property test: random insert/rollback sequences equal a fresh replay. |
| 13 | Intent store schema (block members, signed tx with validity window, inputs, reference inputs, dependency edges, pruning rule) | none | Records and prunes intents; prune property (TTL k-deep + landed k-deep); never truncated by any rollback call. |
| 27 | Raise `event_wait_duration` + horizon lag d in the commit window | none | Deploy param + cap `(tip − d) + W − 1`; unit test that no due event is < d blocks deep at commit time. Redeploy required; ship with P5. |

### Tier 1 — fact store (P1)

| # | Ticket | Blocked by | Delivers / verified by |
|---|---|---|---|
| 2 | Follower → fact store ingestion (qualification rules a–d, phase-2-failed txs, per-block atomic write + cursor), shadow mode | 1 | Follows devnet alongside the current pipeline; per-block diff of `l1_outputs` vs Kupo live view is empty for tracked addresses. |
| 3 | Bootstrap into the fact store (fix ref-script loss in the snapshot decoder; CEK credential via Kupo index + LSQ verify; activation completeness assertion) | 1 | Bootstrap at point P then follow P+n equals follow-from-activation for the same range (test on emulator + devnet). |
| 4 | Fork simulator + projection-diff harness (fast-check over the rollback transport; re-land / never re-land / changed `valid_to` / new-fork-only / phase-2-failed) | 2 | Harness runs in CI; every later projection ticket adds cases to it. |
| 5 | By-hash content fetch (`l1_tx_bodies`; Kupo then Ogmios point-scan; verified by hash) | 1 | Carriage reference datums, pre-origin creating txs, list-replay bodies and activation location resolve without a live-state Kupo read. |

### Tier 2 — consumers (P2, expand–contract)

| # | Ticket | Blocked by | Delivers / verified by |
|---|---|---|---|
| 6 | Fact-store Lucid Provider (UTxO subset + checkpoints equivalent) | 2, 3 | Wallet coin selection and reference-script publication run against it; Kupmios provider retained for submit/evaluate only. |
| 7 | Heads projection + landed queue + topology health + `/readyz` gate + proposal stop | 2, 4 | I1–I5 of the heads table hold under the simulator; orphan node ⇒ not-ready. |
| 8 | Event set + never-reuse key set from `l1_txs` (list validators as pure functions) | 2, 3, 5 | Equals replay-from-activation on the simulator; orphaned origin readmitted; retired key refused. |
| 9 | Tx orders from the fact store; carriage through the content fetch | 5, 2 | Order ingestion survives a fork that removes and re-lands an order; no `fetch-and-insert-tx-order-utxos` Kupo poll. |
| 10 | Scheduler / operators / correction lock / settlement / DA reads via the fact store | 6 | Grep: zero Kupo reads in those modules; devnet run green. |
| 11 | Correction observer as projection over `l1_txs` (admission at depth, absent-means-untracked, post-admission rollback = halt) | 7 | Removal at depth d−1 not admitted, at d admitted; rolled-back admitted removal halts; non-admitted one does not. |
| 12 | Contract: Kupo demoted to content service; lint forbids live-state Kupo reads | 6, 8, 9, 10, 11 | Lint rule green; compose keeps Kupo with `--match "*"`; node boots and commits on devnet. |

### Tier 3 — intents and the loop (P3)

| # | Ticket | Blocked by | Delivers / verified by |
|---|---|---|---|
| 14 | Reconcile loop core: follow landed chain (incl. revive-except-correction), can-still-land predicate, exact-bytes resubmit | 7, 13 | Simulator: dead intents never resubmitted; live ones resubmitted byte-equal; orphaned deposit funding is handled for landed **or abandoned** own blocks of **any shape**, including deposit-only blocks, deposits with L2 transactions, deposits with withdrawals, and two candidate blocks. Delete `prepareSignedHeaderRecovery` and its tests in the same change that lands this universal replacement handler; the two never run side by side. |
| 15 | Replacement on the same tail; commit fiber → proposer; one active commit with speculative build preserved; backoff, alarm, circuit breaker | 14 | Devnet: abandoned commit replaced within one block; speculative build still submits after parent lands. |
| 16 | Merge, DA attestation, scheduler refresh, attestation-timeout correction as loop-driven intents | 15 | Own merge invalidating own in-flight commit is detected by the reference-input check; timeout correction file journal driven by the loop. |
| 17 | Wallet view projection + single wallet owner; frozen override removed | 6, 13 | Dead intent's inputs reappear; no "Missing vkey witness" after a fork; `operator-wallet-view` tests adapted. |
| 18 | Delete: pending-finalization machine, both recovery paths, canonical-journal recovery, control-plane semaphore (DB lease kept) | 14, 15, 16, 17 | −10k lines; **first evidenced pass of the devnet fork drill**; suites green. |

### Tier 4 — ledger projections (P4)

| # | Ticket | Blocked by | Delivers / verified by |
|---|---|---|---|
| 19 | Event status + T2 carry-forward + foreign inclusion (defers without payload) | 7, 8, 13 | Non-empty root + missing DA ⇒ nothing omitted; verified omission ⇒ carried forward; fork removes block ⇒ back to awaiting. |
| 20 | Deposit spendability projection (cutoff = tip slot; no `new Date()`) | 19 | Fake clock ahead of tip ⇒ still unspendable; dependent tx rejected after fork; lint for `new Date()`. |
| 21 | Withdrawal classification + forced verdicts as signed block content | 13 | Row mutation after signing changes neither payload nor roots; replacement re-derives under its own header. |
| 22 | MPF roots tier 1: `SwitchRoot`, resident retained set, mark/sweep | 7 | Depth-3 fork ⇒ O(1) switch, no restart; sweep then reopen all retained roots. |
| 23 | MPF roots tier 2: confirmed-root restore + per-block replay log, engine-uniform | 22 | Maturity-depth correction reaches the target root; tampered log ⇒ halt; swept target ⇒ tier 2 succeeds. |
| 24 | Landed-block local effects as an idempotent projection keyed by header hash | 19, 21, 22 | Crash mid-effects converges; fork removes h ⇒ rows gone, members back in mempool; revived block publishes payload before timeout. |
| 25 | `confirmed_ledger` as projection at settled depth | 7, 23 | Merge at d−1 leaves it unchanged; at d applies; wrong base root refused. |
| 26 | Delete receipts, repair, recovery plans, correction rewind/restore | 24, 25 | −4.4k lines; **correction-removal drill** passes; suites green. |

### Tier 5 — surface (P5/P6)

| # | Ticket | Blocked by | Delivers / verified by |
|---|---|---|---|
| 28 | tx-status head level (D2) + user-submission `settled` status | 7, 13 | API returns the head level; a status below `settled` reverts after a fork in the simulator. |
| 29 | Docs: readiness entry, node README, decision record; retire superseded rollback docs | 26 | `docs/public_testnet_readiness.md` and `docs/research/cardano-inclusion-and-rollback.md` reconciled. |

Parallel frontiers: {0, 1, 13, 27} → {2, 3, 5} → {4, 6, 8, 9} → {7, 10} →
{11, 14, 17, 21, 22} → {12, 15, 19, 23} → {16, 20, 24, 25, 28} → {18, 26} → {29}.
