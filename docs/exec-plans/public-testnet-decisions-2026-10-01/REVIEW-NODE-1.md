# Independent node review, pass 1

Base: `3027be19cb3bc740f83af0e155e34ab38c7cf894`. Inputs: final working diff and current new files only; no author rationale or earlier findings supplied. Sources were read in the shared working tree on 2026-10-01, including updates visible during review. Line references below identify the reviewed snapshot.

Scope: node foreign payload retriever/store and reconciliation fiber; foreign event evidence verification; shared watcher public DA exports, manifest client configuration, payload client and libp2p transport; operator membership/watchdog, Globals, migration 0003 and acquired ledger snapshots; startup HTTP listener/runtime/registration/readiness; foreign reconciliation retention and paging, retention sweeper, owner coverage; related changed tests. Composition reads included history producer permissions, bound ledger source, retained-tip reconciliation, normal commitment reconciliation, and source-owner producer admission. Applied the reviewing-consensus-changes skill, all twelve lenses, and state-queue, user-event and DA invariants.

## Ranked findings

### [F1] CONFIRMED major — unauthenticated address donations can stop an active operator

Where: `demo/midgard-node/src/fibers/operator-membership.ts:231`, `:247`, `:250`, `:340`, `:349`, `:371`; `demo/midgard-node/src/l1-ledger-snapshot.ts:224`.

Defect: the membership monitor requires historical existence of every indexer output at four public addresses, including irrelevant permissionless donations, then disables all duties if any such output did not exist at its checkpoint.

Trace: an operator remains active at canonical checkpoint H; its Ready history owner is one block behind the indexer/chain tip, a supported state because `event-history-owner.history-owner-change.ts:24` permits five blocks of lag and `event-history-owner.make-event-history-owner.ts:1003` refuses only greater lag. An attacker creates an ordinary ADA-only output D at an active/registered/retired directory address in H+1. `operator-membership.ts:231-252` queries all outputs at the hub and directory addresses and passes D's reference along with the authentic directory references. It acquires H at `:247`. D cannot appear in H, so the strict reference equality at `l1-ledger-snapshot.ts:224-231` throws before the monitor can decode the otherwise-complete authentic active list. The catch publishes `unknown` at `operator-membership.ts:340`. `runOperatorDuties` observes the state change at `:363-374`, interrupts all duties, and waits for activation. Membership checks repeat only every 60 seconds at `:349-350`. Repeating donations while the independent source follower has supported bounded lag can repeatedly prevent duties from restarting; commitment, merge, takeover, reconciliation and retained-payload serving share this duty group (`listen.run-node.ts:343` onward).

Reach: anyone may create a minimum-ADA output at these script addresses; creating an output does not execute the directory spending validator and requires no directory NFT. The candidates are not filtered by hub/directory authentication assets. No malformed datum or forged NFT is needed. The decoder would ignore an ADA-only donation (`operator-membership.ts:71-74`), but reference acquisition fails before reaching it.

Lens: 11, ledger facts versus builder assumptions; 12, replacement guards and exceptional exits. The denial is local liveness, not an incorrect on-chain removal or slash. A single such mismatch already interrupts duties; sustained loss additionally requires the explicitly stated bounded-lag schedule. This is a source-traced finding, not a live attack measurement.

Suggested closure: restrict indexer candidates to the actual authenticated hub and directory assets before the exact-reference query, while retaining root/link completeness checks. Add a monitor-composition regression where H has complete active membership and the indexer includes an ADA-only donation from H+1; duties should continue. Also retain refusal for an authentic directory candidate missing at H.

### [F2] CONFIRMED minor — downloaded foreign payloads cannot be reconciled by their new fiber

Where: `demo/midgard-node/src/fibers/foreign-da-reconciliation.ts:137`; `demo/midgard-node/src/workers/t2-foreign-event-reconciliation.reconcile-retained-foreign-tip-entry.ts:279`; `demo/midgard-node/src/services/event-history-producer.ts:133`.

Defect: the new fiber invokes a history-protected reconciler without acquiring the required history producer permit.

Trace: another operator supplies a foreign header with nonempty user-event commitments; the local node retains an awaiting reconciliation and retrieves valid matching DA. The fiber validates identity, roots and counts, and commits its DA row at `foreign-da-reconciliation.ts:136`. Its following call at `:137` enters the reconciler's outer `withHistoryWrite` at `reconcile-retained-foreign-tip-entry.ts:279`. The production fiber is neither inside an owned history SQL transaction nor supplied `HistoryPreparation`, `HistoryProducer`, or `UnownedHistoryFixture`. `event-history-producer.ts:133-135` therefore fails with `Missing producer permit` before reconciliation runs. The fiber logs the failure. Its next SQL selection excludes this header because the DA row now exists (`foreign-da-reconciliation.ts:96` in the reviewed source).

Reach: a valid foreign header and valid peer DA reach this branch; the production `listen.run-node.ts` duty wiring invokes the fiber directly. Its network/read/store checks do not supply producer authority.

Lens: 12, replacement composition and retained guards. Severity is minor because ordinary commitment reconciliation is already producer-owned and reads the newly stored DA (`commit-block-header.database-operations-program.ts:569`; `block-commitment.build-and-submit-commitment-block-action.ts:545`). I did not establish permanent protocol liveness loss from this defect alone.

Suggested closure: acquire `runHistoryProducer` around the reconciliation step after the download, without holding the producer over network I/O. Test the actual production fiber/owner boundary; current foreign DA tests exercise the retriever and store with a fixture capability but never this call.

## Lens coverage

1. Parameter trust — clean: finalized deployment manifests are verified for read-only clients; this scope changes no on-chain parameter application.
2. Always-succeeds scripts — clean: no validator/script loading or yield handshake changed in this scope.
3. Decoders/compiler — clean for the changed DA path: strict envelope decode and retained header/root/count verification refuse substituted content; no Aiken source changed in this scope. The membership decoder refuses missing/cyclic/disconnected authenticated lists rather than concluding removal.
4. Value conservation — clean: retrieval/retention changes construct no value-moving transaction; watchdog strike/retire submission retains the existing SDK builders and control-plane guard.
5. Anchoring — clean: downloaded payload SHA256 binds envelope bytes; retained deployment/header identity is checked and every reconstructed header root and count is compared before storage or reusable evidence.
6. Reference scripts — clean: no fault-proof submitter or script publication change in this scope.
7. Both polarities — coverage gap attached to F1/F2: changed tests cover root substitution/failover, missing DA holds, incomplete directory decoding, sticky shutdown and drainage, and retention pins, but not the donation/head-lag monitor composition or producer-owned production foreign fiber. No changed validator polarity was in this scope.
8. Gates that cannot fail — clean as to inspected assertions: foreign root substitution selects a second peer; deadline tests use an independent virtual clock; retention tests independently assert deletion and preservation. No mutation/red-check was run by this reviewer, so test adequacy beyond these readings is unverified.
9. Execution/size budgets — no confirmed budget defect: network payload/response limits, fetch deadlines, retry state cardinality, retention batch sizes and query pages are bounded. Full historical directory captures still scan the ledger under a 15-minute deadline; no new execution-unit/transaction-size ledger applies to this read-only scope. No performance claim is made.
10. TypeScript/Aiken twins — clean: verification reuses existing canonical SDK decoding and commitment derivation; this scope edits no canonical codec or Aiken twin.
11. Ledger facts — F1: freely created address outputs are included in the exact historical reference set even though they are not directory witnesses.
12. Replacement guards — F1/F2: new membership interruption composes indexer-tip candidates with a historical checkpoint; new DA reconciliation retains its history-write guard but omits the capability its caller must acquire. Retention otherwise retains deployment, rollback anchor, challengeability, pending-journal, recovery-plan and nonterminal-event pins.

## Verification and gates

Read related test source: `foreign-da-reconciliation.test.ts`, `operator-membership.test.ts`, changed watchdog/readiness tests, acquired ledger snapshot and startup HTTP tests, foreign-tip retention/paging additions to `event-history-journal.test.ts`, and watcher deadline/permit tests.

Relevant implementation gates for the owning author: node typecheck/lint/build; focused node foreign DA, membership, watchdog, readiness, startup HTTP, snapshot, migration, history journal and retention tests; watcher public DA construction/failover/deadline/transport tests. Wider acceptance and preflight remain the author's release evidence.

Executed `node scripts/doctor.mjs --json`: exit 1. Fresh blueprint and installed node modules were reported; compiler execution, Postgres connection and child-process capability probes were denied by this review sandbox (`EPERM`). Node was 24.13.1 versus CI's 22.22.2. No test suite, Aiken gate, live stack, mutation test or red-check was run by this independent reviewer. No implementation changes were made.

## Residual risks and limits

F1 and F2 remain open in the reviewed snapshot. Foreign DA showing a candidate event is present still yields the existing `foreign_event_present_requires_finalization` hold; this scope downloads data without adding remote finalization authority. I found no evidence that this change weakens that safety hold. Known SQ5 slash-operator binding and UE7 exact-fee residuals are outside this scope and unchanged by the reviewed node code.

No live Ogmios historical availability or full indexer/source timing attack was exercised. No fund-stealing, honest-block-removal, fraudulent-block-acceptance or codec-twin finding was established in this scope. Other reviewers own the wider watcher funding/rollback and full-stack changes. Shared-tree edits continued during review; this report is pass-1 evidence and requires final-tree verification after fixes.
