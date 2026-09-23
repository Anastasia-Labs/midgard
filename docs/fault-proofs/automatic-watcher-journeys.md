# Automatic watcher journeys

Status: All 52 family results accepted through preserved and recovered runs.

The acceptance target is a fresh, isolated Cardano devnet with real Cardano
node, Kupo, Ogmios, native chain sync, public libp2p DA retrieval, and the normal
watcher launcher. The source catalogue supplies the family inventory. Each
family must be detected from a committed L1 header and public DA, complete its
installed workflow automatically, retain its permanent proof token, remove the
faulty header with the configured slash/reward, and classify a subsequent honest
commitment as healthy. A family counts only after these observations pass.

The existing `transitionTrace` journey supplies emulator evidence. Its retained
run took 2,896.92 seconds: deployment 19.6s, startup 57.9s, proof/correction
34.0s, and successor catch-up 2,760.1s. These measurements motivate profiling
catch-up; they do not establish the real devnet's performance.

## Retained live status

The September 16 continuation reached **52 accepted families** at 17:05 UTC:
34 preserved results from the earlier deployment and 18 on the replacement
deployment. Withdrawal-mistag's final journal reached depth 31; the final selected
journey process exited successfully. The aggregate verifier rechecked all 18
current stamps, the pinned accepted files, and the historical archive hashes.
No families remain. All earlier acceptance evidence and included transactions
are preserved.

The aggregate receipt is
`/var/tmp/midgard-watcher-completion-20260916/accepted-family-index.json`.
This establishes recovered functional acceptance across two deployments. It is
not a clean first-attempt full release gate; the broad emulator gate was not
rerun. Earlier interrupted sessions and source-change incidents remain recorded.
Concurrent SDK/operator work also changed inputs during the final session; the
receipt records those differences. The watcher and fault-proof compiled artifacts
remained unchanged throughout that selected run.

The resumed mint-item proof and correction succeeded, and its healthy successor
was committed and attested. Continuation was then paused to fix pending finality
not waking on canonical blocks that leave the state queue unchanged. Native
progress now wakes retained objectives without reclassifying unchanged evidence;
duplicate notifications are ignored. All 11 joint integration cases pass.
The retained mint-item recovery mismatch was the custom prepared artifact being
validated as a generic workflow envelope. Recovery now authenticates the
mint-specific artifact against its sealed original decision and signed intents;
funding recovery applies the same identity binding. Mint removal also uses the
shared terminal verification and funding-release handoff.

The subsequent restart exposed a second identity mismatch in the custom mint
runner: terminal continuation selected the fresh authority decision digest
instead of its bound original execution digest. The retained 38-record journal
contains the prepared artifact and terminal checkpoint; no records were lost.
The custom runner now uses the journal's bound execution digest, matching the
generic runner. A regression with that retained journal proves pending-to-final
continuation under refreshed authority without a second journal or transaction.

Fabricated-deposit completion exposed a stale decision-journal reader: the
supervisor had opened its snapshot before the live writer appended that
execution's decision. Bounded recent decision retention and fresh authenticated
reads on genuine cache misses fix that failure. Atomic decision-record
publication also prevents concurrent readers from observing partial writes.
The 12 joint supervisor tests, nine authority/cache tests, and eight decision
journal tests cover these paths; accepted records remain unchanged.

Withdrawal-mistag fixture publication exposed a normal event arriving between
event-address discovery and authenticated history acquisition. Capture now
validates every event, expands missing NFT histories, and retries at most three
final captures. Continued checkpoint movement defers the affected classification
suffix until canonical progress; malformed event data remains fatal. Twenty
event-capture tests and 62 bridge tests passed (two existing skips).

The watcher now admits progress through one supervisor slot per objective.
Pending observations coalesce, journal admission precedes funding, completed
executions receive a signer-free canonical check, and retained attempts reconcile
under fresh authority. Joint tests cover the real supervisor, workflow journal,
SQLite funding admission, canonical rollback observations, and shutdown races.
See [the architecture](architecture.md#watcher-proof-progress). These checks do
not count as new live-family acceptance or a clean full-suite release gate.

Focused regressions reproduced and corrected four integration defects:

- The distinct-asset datum registry omitted its initial datum and duplicated
  its final schema, shifting the intermediate decoder selection.
- Recovery checked absence of every computation thread under the shared policy.
  It now checks the corrected category and header's exact token, allowing other
  proof objectives to remain active.
- Mint-item removal discarded authenticated removal/proof output references and
  used a family-specific action kind that the funding authority correctly
  rejected. It now binds the canonical removal action and exact references,
  records each descendant separately, and reconciles included removals on restart.
  The funding authority's strict transaction and economics checks remain intact.
- Both fabricated-family workflow bindings omitted the published PHAS membership
  reference required by their generic initialization. The watcher supplies it
  through the shared reference helpers, and each workflow authenticates it
  against the manifest before provider access.

Continuation receipts are under
`/var/tmp/midgard-watcher-mint-removal-20260916-ops/`. Read-only reconciliation
confirmed all six mint-item proof transactions, the retained proof token, and
the still-present fault target; no removal transaction had been submitted.

At 2026-09-16 03:22 UTC, **39 of 52 families had accepted finality evidence;
13 remained**. The continuation was paused to fix submission recovery and
canonical category resolution before resuming its retained proof.

The submission failure was a conflict with the harness's own pending attempt.
The node accepted `e77890ca…` at 02:28:08 UTC, but a 138-slot gap between blocks
left it in the mempool after the harness's local expiry wait. At 02:30:05 UTC,
the harness submitted `380e034f…` spending the same three inputs; the node
rejected it. The first attempt left the mempool only after the next block at
02:30:25 UTC. Local time and missing unspent outputs therefore cannot authorize
replacement. Fault, history, and successor staging now reconcile the exact
persisted attempt after an ambiguous submission or local expiry wait. Only
authenticated expiry or invalidation permits wallet refresh and replacement;
inclusion continues the existing attempt, and uncertainty preserves its bytes.

Production workflow reconciliation also retains its signed intent when a typed
canonical-checkpoint change requires another observation. Identity and
authentication failures remain hard errors. Five registered proof actuators
now resolve removal through their canonical catalogue names, preserving the
guard against explicit-category collisions. Focused regressions cover these
recovery and routing boundaries; the historical interrupted runs remain
recovery evidence, not clean first-attempt acceptance.

As of 2026-09-15 at 21:54 UTC, **34 of 52 runnable families have accepted
finality evidence; 18 remain**. This combines 24 finalized stamps with ten
older results whose terminal observations reached depth 30 or greater. The
old remaining-only run is stopped. The publisher-authorized replacement under
`/var/tmp/midgard-watcher-journeys-20260915-publisher-auth` started exactly the
remaining 18 family journeys at 20:33:12 UTC in `session-Dg0eP4`. No new family
is accepted yet. Deployment completed with 446 confirmed publication transactions,
521 reference roles, initialization, and all 76 required reward registrations.
Independent readiness verification passed at slot 2374, before authority expiry
at slot 7878, for deployment fingerprint
`48bc32f03410ea0b2bc411db9300316c69ba42244a221825391359b6723484ce`.
New publication policies require the
publisher's signature and permit an immediate confirmed-publication audit,
without waiting for their minting deadline. Exact policy decoding prevents this
readiness path from accepting the earlier time-only policy. Focused validation,
seven package builds and typechecks passed before publication started at
20:06:32 UTC. Operational receipts are under
`/var/tmp/midgard-watcher-publisher-auth-20260915-ops/`; `workflow.receipt.json`
records deployment and immediate readiness audit. That first journey attempt
stopped at 20:58 UTC after proof/correction and honest-successor publication,
before healthy-processing acceptance. Its watcher queue journal rejected a
transition's wall-clock ordering, then the harness queried the replacement
watcher before its operations listener was ready.
The controller monitors family completions, stops on an unexplained error, and
preserves the existing signed records for reconciliation.

Same-deployment recovery receipts are under
`/var/tmp/midgard-watcher-recovery-20260915-ops/`. The exact 18-family
continuation resumed at 21:51:45 UTC in `session-8PRARB`, with source freeze
`27cd916ee64260d00ca954bfb378bc688c4926559f34a34fc41788e01fceba14`.
Its `workflow.receipt.json` is the authoritative live continuation status.
`transactionOutputNonCanonical` passed healthy-successor verification at
21:54:38 UTC, reusing its original proof, removal, and successor transactions.
The runner advanced to `resolvedOutputNonCanonical`; the recovered family's
depth-30 evidence stamp remained pending, so it is not yet added to the accepted
count. Recovery took approximately three minutes including startup; this is
checkpoint reuse, not a fresh-family performance benchmark.
Independent reconciliation
confirmed 24 submitted transactions, the removed faulty commitment, the retained
proof token, and the existing honest successor. The repaired queue reader
authenticated all 65 existing records without changing their bytes. Queue
transitions now use authenticated revision order and legal predecessor states;
wall-clock timestamps remain observations, not transition authority. Operations
reads wait for authorized process restarts using a bounded monotonic deadline.

Cold sessions launch the observer after retaining their healthy predecessor,
overlapping observer startup with remaining fault preparation. The temporary
transaction-phase timing callbacks and CPU-profile launch hooks have been removed;
the observer startup optimization and ordinary journey stage records remain.
The operator builder and fixture now use the deployed contract's existing
immediate-activation exception: the earliest registered operator can restore an
authenticated empty active set. The transaction consumes that root witness;
ordinary registration maturity and the 120-second submission window remain.
Focused real-validator coverage verifies early success and rejection for a
newer registration or a nonempty active set. No validator changes were needed.

Docker and its original chain containers were recovered after a host restart.
The recreated node socket needed the access mode already declared by the
deployment harness. A subsequent trusted-head HTTP connection failure was
addressed by closing each authority response connection; two new real HTTP
regressions, the watcher build/typecheck, and scoped lint/format checks passed.
CAS remains single-attempt when its response is ambiguous.
The DA fixture now checks expiry before funding init or signatures, after
reconciling retained attempts. Three new focused regressions passed, including
recovery of an already-attested old header; watcher and node-tools typechecks
and scoped lint/format checks also passed. No emulator suite was rerun.

The latest continuation, `session-H40Ws9`, recovered
`transactionOutputNonCanonical`'s completed proof (depth 132) and included DA
signatures for its retained honest successor. Apply then rejected that
successor's immutable deadline, which elapsed at 09:05:09.999 UTC during the
interruption. This is not another accepted family: healthy successor processing
and its finality evidence remain incomplete.

Live queue reconciliation found that successor at the tail behind 38 attested
predecessors. The retained deployment's timeout correction only removes the oldest head. Ordinary merges
must therefore precede its removal; the last predecessor matures on
**2026-09-22 at 07:16:51.999 UTC**, before transaction execution and confirmation
overhead. Re-signing or reinitializing DA cannot change this deadline.
The user has authorized replacing this deployment while preserving the accepted
results and all transaction evidence. Source changes generalize unattested
timeout correction to any pending commitment, preserving its earlier prefix;
they cannot change the validators already deployed here. Remaining families need
a fresh deployment with the new applied scripts and independent acceptance
evidence. The first replacement attempt was created under
`/var/tmp/midgard-watcher-journeys-20260915-timeout-suffix`, with fresh genesis,
wallets, databases, and the locally validated timeout blueprint. Deployment
publication started at 18:41 UTC. Its 446 publication transactions and 521
reference roles are confirmed, along with atomic initialization and all 76
required PHAS/runtime reward roles. That attempt was deliberately stopped at
19:44 UTC to replace its time-only publication policy; no family acceptance is
claimed. Frozen inputs and process receipts are under
`/var/tmp/midgard-watcher-fresh-20260915-ops/`.

The 34 accepted results and their referenced evidence are independently archived
at `/var/tmp/midgard-watcher-accepted-archive-20260915/`. Its verification receipt
records 66,312 verified files, including all accepted-result references, the
original deployment identity, and a Postgres logical dump. The archive directory
occupies 1.81 GiB after deduplication and compression; CPU profiles and
unreferenced superseded deployment runtime data were omitted. Separate complete
container logs occupy another 2.5 GiB: seven complete logs and the last 100,000
Cardano daemon records; the full Cardano debug trace was discarded. At 18:52 UTC,
the old run directory, eight stopped containers, and their dedicated network
were removed after archive and log checksum verification. Measured free disk
space increased by 92,234,555,392 bytes (about 86 GiB); the cleanup receipt is
`/var/tmp/midgard-watcher-fresh-20260915-ops/old-cleanup-receipt.json`.
Historical paths below map through the archive's original-path inventory.
That attempt's publication authority expires at slot 7397, around 20:39 UTC. Its default
publication schedule produced a longer authority lifetime than the runner's
one-hour test timeout. At 19:05 UTC, a continuation with an extended test timeout
started on the same deployment after independent verification of every reference,
initialization, all reward registrations, and an empty mempool across an additional
canonical block. The original process was deliberately stopped with SIGTERM;
this was an operational continuation, not a clean uninterrupted run. The exact
nonce, authority policy, signed publications, initialization, and blueprint were
preserved. The detached supervisor and continuation were subsequently stopped
after all signed publications, initialization, reward registrations, and an empty
mempool were reconciled. That supervisor will not launch families. Its durable state is
`/var/tmp/midgard-watcher-fresh-20260915-ops/deployment-continuation-state.json`.

The old 34 remain evidence for their original deployment; running the remaining
18 on the new deployment does not certify all 52 on the new validators.

Persistent reconciliation, the remaining-family CSV, launch receipts, and
validation logs are under
`/var/tmp/midgard-watcher-journeys-20260911-preprod-ready/work/journeys/recovery-20260915/`.
In particular, `expired-successor-queue-topology.json` records every live queue
link and maturity time. Older `/tmp` diagnostic files referenced below were lost
during the host restart; those references describe historical observations.

### Earlier recovery history

As of 2026-09-14 at 22:55 UTC, the retained deployment has **25 anchored family
passes**, with 27 registered journeys unfinished. `valueNotPreserved` recovered
its retained first fold and progressed through fold 15 plus the next field
publication. Fold 16 stopped at funding reservation, before a durable submission
intent, because its output was outside the governed contract roster. Separately,
the harness exited 1 after its source-pin check detected the user-requested
DA corrected-target merge applied during the run. Owned processes and archives
closed; no stop signal was sent.

Earlier, `committedFieldShape` recovered its existing proof and honest successor,
passed healthy processing, and anchored at 15:19:06.562 UTC. That boundary also
anchored `canonicalDecodability` at 15:19:06.542 UTC and began `l2TxMistag` in the
same running session. The batch then exited 1 at 15:22:33 UTC after a DA
signatures confirmation timeout. Those signatures were already included on
chain. At that interruption, 29 journeys were unfinished, beginning with that staged
target; no `l2TxMistag` proof had started at that first interruption.
The 29-family continuation resumed at 15:31:35 UTC in
`sessions/session-RCMwxA`, using the same target and included signatures. Its
init and two proof steps were included, but DA apply consumed the header output
also used as an ordinary input by the signed removal. The batch was deliberately
stopped around 15:48 UTC with that removal unresolved. No honest successor or
next-family handoff occurred; the same 29 journeys were still unfinished.

The same 29-family selection resumed at 19:39:27 UTC in
`sessions/session-xdgkue`, after the user requested live continuation without
further test reruns. Recovery retired the old removal attempt and included its
replacement at 19:42:34.229 UTC, preserving the existing proof. The journal
recorded `terminal_included` at 19:42:35.571 UTC. After native recorder catch-up,
the honest successor completed in 411.6 seconds and healthy processing in
1.1 seconds. `l2TxMistag` wrote its functional pass at 20:00:20.537 UTC, with
`terminal-included-awaiting-anchor`; `withdrawnInput` staging then began.
That family passed at 20:24:55.535 UTC, after an expired, unminted successor
commitment was rebuilt and healthy-source catch-up completed. The same boundary
anchored `l2TxMistag` at 20:24:55.594 UTC and `withdrawnInput` at 20:24:55.612 UTC,
then began `valueNotPreserved`. `withdrawnInput/session.json` records watcher
reuse in `session-xdgkue`; no process restart or manual intervention was needed.

`valueNotPreserved` then timed out at 20:44:17.160 UTC after 900.3 seconds in
the automatic-decision stage. The batch exited at 20:44:29 UTC with two passing
families and one failed family; later selected families did not run. Durable
source progress ended at block 15981, slot 330217, 45 blocks before its retained
commitment at block 16026, slot 331010. Its DA-attested target remained live at
`b74afc1ed3b22a77e4b418493a1f1bea9127060b543deabca10f2620e7db5cfb#0`.
No workflow journal was created. This is a source catch-up timeout, not a proof
rejection or a completed family. The next selection is
`/tmp/midgard-inclusion-after-withdrawn-input-families.csv`: 27 families starting
with the retained `valueNotPreserved` target, preserving the final three event
families. Exact points, process closure and archive ownership are recorded in
`/tmp/midgard-objective-live-valueNotPreserved-timeout.json`.

The 27-family selection resumed at 20:55:56 UTC in `sessions/session-xSQnmO`
with bounded first-observation prefetch. Publication still advances only through
each requested block, with fresh second-capture authentication. Initialization
`e6148a2167f446c63ec1292eab9f59412dc7506b7de6f106fd5c2b883f74b4ed`
was included at block 16123, slot 333088. Fold transaction
`6144f8971c163e888cab6c1b6f7393182430bce1c8fa307ce4b5f22de7b0dbac`
was included at block 16124, slot 333120; its thread output remains unspent.
The observer then failed closed with `value conservation: live thread omitted
from raw L1 snapshot`. The last pending reconciliation was at 21:02:42.975 UTC;
the failure line has no timestamp, and the harness discovered it after native
recorder catch-up, before restarting at 21:11:04 UTC. The deliberate stop exited
143 at 21:11:36 UTC. This attempt produced no family pass or successor.

The retained archive contains only one newly published first/second observation
pair, at block 15982 (21:00:04.675–21:00:13.821 UTC, tips 16110–16111).
Unpublished prefetched first observations are process-local, so this interrupted
attempt does not establish multi-block prefetch amortization from archived
evidence. Exact journal, canonical points and cleanup facts are retained in
`/tmp/midgard-valueNotPreserved-live-thread-reconciliation.json`; the witness
extraction is `/tmp/midgard-user-event-prefetch-live-witnesses.json`.

The coherent-snapshot continuation began at 22:39:42 UTC in
`sessions/session-WyG81f`. It acknowledged the previously included fold at
22:42:37.819 UTC, then progressed without replaying that transaction. Fold 15
`2712ed41d65a226921f5a3296723d09ba57b234c5b229f0f822c24a448c8aac5`
confirmed at 22:54:11.051 UTC; its thread output remains unspent. Field publication
`707682b55bd5bce0dd4ea8932448b9466e5ad1a77e3d27e9b979be3ff19e630e`
confirmed at 22:54:25.950 UTC. Journal sequence 206 records the funding-roster
failure for fold 16 at 22:54:30.951 UTC, with no subsequent submission intent.
The funding reservation has no pending transition after acknowledging the field
publication. The separate harness source-pin error names `journey-runner.ts`;
its exit at 22:54:51 UTC must not be attributed solely to the watcher failure.
The 207-entry journal, exact outputs and both failure causes are retained in
`/tmp/midgard-valueNotPreserved-funding-roster-reconciliation.json`.

The retained failures remain part of this recovery evidence. The earlier
canonical journey completed functional checks at 12:42:35.158 UTC; that boundary anchored
`withdrawnReferenceInput` and began `committedFieldShape` in the same session.
The second family failed when its signed proof initialization referenced the
queue output being consumed by DA attestation. The batch was stopped with exit
143 around 12:47 UTC, preserving that unresolved intent and the attested target.
That attempt verified shared-session handoff but did not complete the second
journey.
The retained 30-family continuation resumed at 14:28:42 UTC after the required
gate and artifact checks passed. It recovered the proof and included the honest
successor, then failed during healthy processing with a funding reconciliation
error. It was stopped around 14:52 UTC. No new functional or anchored family
result is counted, and `l2TxMistag` was not staged.
After the current-role funding fix and installed artifact checks passed, the
same 30-family selection resumed at 15:00:27 UTC in `sessions/session-4w2Akp`.
It completed the recovery above before the next family's confirmation timeout.
There is no completed batch verdict. The deployment fingerprint is
`e3e004b1a2b2ea8bf8aac849534520af0577c7f3dddc45571c4b2b0fa94fc481`, and the
retained run is `/var/tmp/midgard-watcher-journeys-20260911-preprod-ready`.
All family artifacts and logs below are under its `work/journeys` directory.

The catalogue contains 54 non-interactive families, of which 52 have registered
live fixtures. Of those 52, 27 still need anchored completion, beginning with
`valueNotPreserved`. Completed families are not rerun to collect a
pending stamp. A failed family can leave an attested, unproven header on the
queue; reconcile it before progressing to later families.

After session shutdown, the read-only `readiness-report.test.ts` passed and
independently reported 21 live-complete families. It validated the
`withdrawnReferenceInput` stamp against the closed session's native file.
At that audit, `canonicalDecodability` was incomplete solely because its final stamp
was absent: the report placed that expected missing file in its generic
`invalid_live_evidence` category. No other live evidence issue was reported.
`committedFieldShape` had no result and remained pending. The check took 90.81
seconds of test time, 102.21 seconds overall, and exited zero; its output is
`/tmp/midgard-shared-session-closed-readiness.log`. This is evidence validation,
not an additional live journey or a completed batch.
A second closed-session audit after `session-crQPbh` stopped also passed: 21
live-complete families, only the same missing canonical stamp, and no committed
family result. It took 92.61 seconds of test time, 103.72 seconds overall, and
exited zero; output is `/tmp/midgard-reference-recovery-closed-readiness.log`.

Ten passes used the earlier per-step release-finality waits:
`transitionTrace`, `zeroInput`, `invalidRange`, `invalidSignature`,
`mintAuthorization`, `minFee`, `spendInputSignerMissing`,
`protectedOutputSignerMissing`, `observersForbiddenOnUntaggedNetwork`, and
`inputSetUniqueness`. They establish their tested protocol behavior, not the
new inclusion/recovery behavior. The last completed at 18:15:18 UTC on
2026-09-13 in `live-g2n-20260913T080337Z.log`, with exit zero. The trace
artifacts use the retained directory name `transition-trace`.

The thirteen subsequent anchored passes used inclusion-based progression:

| Family                    | Finalized stamp, UTC | Journey provenance                                                                                     |
| ------------------------- | -------------------- | ------------------------------------------------------------------------------------------------------ |
| `observerOrderInvalid`    | Sep 13, 22:58:16.635 | Recovered after funding-input failure; same staged fault and signed chain                              |
| `doubleSpend`             | Sep 14, 01:35:43.309 | Recovered the already included removal without resubmission                                            |
| `nonExistentInput`        | Sep 14, 02:09:59.948 | Clean functional journey                                                                               |
| `nonExistentInputNoIndex` | Sep 14, 02:41:58.007 | Clean proof; expired, unminted honest successor rebuilt                                                |
| `daHashPreimage`          | Sep 14, 03:13:07.152 | Completed after historical classification catch-up                                                     |
| `noReferenceInput`        | Sep 14, 03:46:58.357 | Clean functional journey                                                                               |
| `referenceInputNoIdx`     | Sep 14, 04:48:23.701 | Clean functional journey; historical catch-up cleared without restart                                  |
| `nativeScriptDecoding`    | Sep 14, 05:56:59.691 | Completed after an earlier fixture prerequisite failure                                                |
| `missingSignature`        | Sep 14, 11:06:04.691 | Recovered the same staged fault after classification timeout                                           |
| `missingNativeScriptTx`   | Sep 14, 11:55:11.532 | Recovered the same prepared artifact after transport, timing and admission failures                    |
| `withdrawnReferenceInput` | Sep 14, 12:42:35.239 | Clean proof; successor scheduler reappointment; stamp authenticated in the next shared session         |
| `canonicalDecodability`   | Sep 14, 15:19:06.542 | Functional checks passed in the first shared session; stamp collected during later recovery            |
| `committedFieldShape`     | Sep 14, 15:19:06.562 | Recovered reference-invalidated init, then resumed existing proof/successor after funding-role failure |
| `l2TxMistag`              | Sep 14, 20:24:55.594 | Retained proof reused; ordinary-input invalidated removal retired and rebuilt automatically            |
| `withdrawnInput`          | Sep 14, 20:24:55.612 | Shared watcher reused; expired, unminted successor rebuilt; healthy-source catch-up completed          |

For inclusion-based families, `result.json` records functional checks, while
`finalized-evidence-stamp.json` establishes anchored completion. A result may
still say `terminal-included-awaiting-anchor` after its separate stamp is written.
Keep `pending-evidence-stamp.json`, `completed-workflow.json`,
`finalized-workflow.json`, `native-chain.ndjson`, and `timings.ndjson` together
with the attempt logs. Registration or local verification is not live acceptance.
The newly registered `fabricatedDeposit`, `fabricatedWithdrawal`, and
`withdrawalMistag` fixtures have no completed live verdict on this deployment.

Two catalogue families remain genuinely blocked: `doubleWithdraw` and
`crossBlockDuplicateEvent` are candidate fixtures whose current constructions
select an earlier fault family in the full classifier. The latter also requires
the genuine settlement prerequisite described under Recovery and maturity.
Neither a fabricated settlement nor a prior queued header satisfies it.

## Measured progression and recovery

The observed proof transitions advance without per-action release-finality
waits, and family handoff does not wait for the final stamp:

| Sample                                                 | Parent inclusion to next submission | Canonical blocks before submission                                    | Evidence context                                                                                                        |
| ------------------------------------------------------ | ----------------------------------- | --------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| `observerOrderInvalid`, initial step 1 and publication | 2.498s, 3.808s                      | 0 each; children in blocks 11829–11830                                | Initial attempt later interrupted                                                                                       |
| `observerOrderInvalid`, resumed steps 3, 4 and removal | 3.749s, 3.518s, 5.006s              | 0 each; children in blocks 12153–12155                                | Completed recovery                                                                                                      |
| `nonExistentInput`, five transitions                   | 2.615–6.405s                        | 0 each; parent/child chain in blocks 12635–12640                      | Clean proof                                                                                                             |
| `missingSignature`, five transitions                   | 2.536–4.706s                        | Four at 0; one crossed 1 block in 4.287s                              | All six submissions attempt 1; journey recovered after classification timeout                                           |
| `missingNativeScriptTx`, nine transitions              | 4.737–15.381s                       | Four at 0; five crossed 1 block; children in the next or second block | All ten submissions attempt 1; recovered the retained prepared artifact                                                 |
| `withdrawnReferenceInput`, four transitions            | 3.770–15.002s                       | First three at 0; removal crossed 1 block                             | All five proof submissions attempt 1; pre-proof decision profiling and successor scheduler recovery recorded separately |
| `canonicalDecodability`, three transitions             | 7.886s, 4.755s, 9.815s              | 0 each; children in blocks 14578–14580                                | Functional pass in the first shared session; watcher process profiling enabled                                          |
| `committedFieldShape`, recovered proof                 | 9.159s, 7.466s, 23.377s             | First two at 0; removal crossed 3 blocks                              | Proof recovered; funding-role failure required a later healthy-processing continuation                                  |
| `l2TxMistag`, interrupted proof                        | 5.028s, 3.700s                      | 0 each; children in blocks 15141–15142                                | Init and two steps included on attempt 1; removal unresolved after competing DA apply                                   |

The interrupted `observerOrderInvalid` step 2 took 6,541.034 seconds and 322
inclusion blocks after publication because the run stopped for repair. Exclude
that gap from normal latency claims. `doubleSpend`'s eight initial submissions
were each on attempt 1; dependent submissions followed their parent's confirmed
journal observation by 0.795–1.535 seconds. These observation-based intervals
are distinct from the native inclusion-based intervals in the table.

`doubleSpend` handed off at 01:10:47.338 UTC with its honest successor only eight
blocks deep (included at 12584, advertised tip 12591); its stamp arrived at a
later family boundary. `nonExistentInput` handed off at 01:35:53.592 UTC with
its successor six blocks deep (12659 at tip 12664). `missingSignature` passed
functional checks at 05:56:59.527 UTC and handed off at 05:57:10.168 UTC while
its stamp remained pending. That boundary anchored `nativeScriptDecoding`
without rerunning its journey. `missingNativeScriptTx` then handed off to
`withdrawnReferenceInput` with its own stamp pending: its successor was included
at 14328 and the next recorder advertised tip 14333, only six blocks deep.
The shared-session handoff captured the next family's workflow baseline at
12:42:35.473 UTC, 0.315 seconds after `canonicalDecodability` passed healthy
processing. The same authority, watcher and native recorder were still running
at this boundary; the canonical family's final stamp remained pending.

Major stage measurements below are seconds. They describe the listed attempt;
recovered attempts retain the earlier failures described below. A dash means
this summary does not quote that stage, not that it took zero time.

| Family/attempt                                |             Startup |              Decision |   Proof/correction | Honest successor | Healthy processing |
| --------------------------------------------- | ------------------: | --------------------: | -----------------: | ---------------: | -----------------: |
| `observerOrderInvalid`, resumed               |               138.3 |                     — |     74.7 remaining |            381.3 |                0.0 |
| `doubleSpend`, resumed                        |                   — |                     — | 5.1 reconciliation |            374.5 |              135.2 |
| `nonExistentInput`                            |                94.5 |                  81.3 |              212.1 |            441.8 |               28.0 |
| `nonExistentInputNoIndex`                     |                   — |                     — |              218.1 |            485.9 |              318.0 |
| `daHashPreimage`                              |                   — |                 682.8 |               66.3 |            324.3 |                7.3 |
| `noReferenceInput`                            |                72.7 |                 142.0 |              151.2 |            525.6 |              195.9 |
| `referenceInputNoIdx`                         |                87.3 |                 695.3 |              215.0 |            281.7 |               34.5 |
| `nativeScriptDecoding`, resumed               |                87.0 |                 824.2 |              194.0 |            378.8 |              119.6 |
| `missingSignature`, resumed                   |                96.9 |                  19.6 |              106.7 |            492.5 |                0.2 |
| `missingNativeScriptTx`, resumed              |                91.2 | 0.0 retained decision |              328.7 |            379.5 |                2.5 |
| `withdrawnReferenceInput`                     |               142.6 |                 884.9 |              241.4 |            442.2 |              169.5 |
| `canonicalDecodability`, first shared session | 0.03 readiness wait |                  42.6 |               66.4 |            410.9 |                0.8 |

The completed `observerOrderInvalid` recovery also spent 566.1 seconds staging,
38.5 seconds in binding preflight, and 673.4 seconds in the separate finalized
stamp stage. Vitest took 1,899.57 seconds overall, including 1,888.65 seconds in
the test, and exited zero at 22:58:28 UTC on September 13.

The clean `nonExistentInput` stages totaled approximately 26 minutes, including
672.2 seconds staging/origin replay and 23.7 seconds binding. The older `minFee`
journey took approximately 84 minutes; `transitionTrace` took 11,153 seconds
with three harness relaunches. These different families and execution policies
are operational comparisons, not a controlled speedup ratio. The proposed
5–8-minute total has not been demonstrated: origin replay, startup/classification,
and honest operator eligibility still consume substantial time.

`missingSignature` recovery staging took 669.0 seconds and binding 38.8 seconds.
Its 19.6-second decision followed an earlier 900-second classification timeout;
different retained cursors and times prevent a controlled benchmark claim.
The proof timings are fresh, but the complete journey is recovery evidence.

## Retained failures and verified repairs

The completed recovery evidence preserves the original failures:

- `observerOrderInvalid` first failed its exact funding-input check after init,
  step 1 and publication were included. The run stopped with exit 143 and
  resumed the same staged fault and signed chain after regression checks.
- `doubleSpend` initially failed with `StateQueueHeaderNotLiveError` after
  removal had already landed. The retained-header recovery fix passed local
  checks and the full emulator gate; recovery reconciled that exact removal
  without resubmission. The first batch's native-script prerequisite had passed.
- `nativeScriptDecoding` first stopped at 03:57:39 UTC before signing a forced
  order or fault: the fixture required an unpublished `txOrderMint` reference.
  The SDK's existing inline-policy fallback fixed the fixture. Four focused
  tests, type/lint and named node checks passed. A read-only build with actual
  retained material passed local UPLC evaluation at 12,584 signed bytes against
  the 16,384-byte limit. Its later live journey used that inline path.
- `missingSignature` first timed out at 05:19:57.910 UTC, with no proof begun;
  finalized replay stopped at 13282 before its retained fault at 13294. The
  watcher now coalesces classification while finalized replay trails an observed
  inclusion, preserving history, availability and execution-recovery updates.
  It adds no persisted execution history or cached healthy verdicts. Thirty-five
  focused runtime/coordinator tests, the installed-manifest journey, watcher
  type/lint, named node checks, rebuilt watcher and both installed CLI smoke
  checks passed before its successful functional recovery.

The recovered `missingNativeScriptTx` journey retains replacement header
`3903c7a90b0377b82fa74f3cb15f52c2cee014697dcd865c26f4de38`, committed by
`282a793c0f67bae1454b1c23d571bc21f4cbf174b0d08796577286e83803d642` at block 13506. The first commitment expired unminted and was rebuilt through the existing
recovery path. Its first binding, startup and decision took 26.3, 121.6 and
266.6 seconds. Subsequent interruptions have not replaced this fault:

**Sep 14, 06:21:47 UTC, exit 143.** Historical provider `192.168.0.7:8443` was
unreachable from the host watcher. No proof was signed. The harness now tunnels
only the two configured HTTPS authorities through an allowlisted CONNECT proxy
to loopback-published archive ports; URLs, keys, TLS checks and roster digest
remain unchanged. Six transport tests, three archive API tests, type/lint, named
node checks and an actual production resolver probe passed.

**Sep 14, 06:55:20 UTC, exit 1.** A retained decision matched immediately while
runtime replay still preceded the fault; the harness's 190-second action timer
expired on the old start record. Retained workflows now get 900 seconds for
first new durable progress, then the normal action allowance. Fresh limits,
reconciliation-only handling, stall checks and the whole correction cap remain.
Twenty-six focused tests, type/lint, named node checks and independent review
passed.

**Sep 14, 07:29:35 UTC, exit 143.** Both archives returned HTTP 200 for
publication and canonicality during preparation and readmission. A prepared
artifact was saved at 07:29:01.989 UTC; admission rejected its corpus/L1 binding
because canonical journal serialization reordered nested object keys. At that
interruption there were no proof submission intents or submitted transactions. The prepared
artifact and original authority bindings are preserved.

The native-script fix compares admitted nested structures independently of
object key insertion order. The fabricated-artifact fixes project nested fields
in their original schema order before hashing, preserving existing digests.
Both retain field and authority checks. Directory-journal round-trip regressions
passed: 22 native-script tests and 41 fabricated-artifact tests. Full fault-proof
TypeScript checking, full lint and independent reviews passed. An additional
field-carriage regression retains a confirmed journal entry, removes its
canonical publication output, and requires publication again while blocking the
dependent proof preflight; all seven tests in that file, lint, formatting and
the final type check passed. The required full emulator gate exited zero:
52 node tests in 391.44 seconds and 434 fault-proof tests across 101 files in
4,667.87 seconds. Both installed CLI smoke checks passed after the staged swap,
and the same 33 unfinished families resumed at 08:58:25 UTC with unchanged
manifest and blueprint hashes. These local checks do not count as live passes.

**Sep 14, 09:14:09 UTC, exit 143.** The continuation passed workflow binding
in 38.5 seconds and watcher startup in 92.6 seconds. Readmission then rejected
the prepared artifact's original historical boundary at block 13713 because it
required that boundary to equal the current observation. This failed locally
before either archive received a new publication or canonicality request. The
original prepared artifact and digest remained unchanged; no proof submission
intent existed at that interruption. The run stopped for a regression and repair
that preserve the artifact while re-authenticating its publication against current chain state.

The boundary repair now preserves the original observation metadata and digest
while every installed provider re-authenticates the exact publication at the
current boundary. It also removes the workflow's frozen-tip guard while keeping
the corpus-digest guard. Twenty-five focused tests, full type/lint, formatting
and independent review passed. The full emulator gate then exited zero:
52 node tests in 395.31 seconds and 434 fault-proof tests across 101 files in
4,686.33 seconds. A read-only probe using the staged build and exact journaled
L1 evidence passed
against both real archives through block 14021, preserving the original
block-13713 evidence digest. It changed no journal or transaction state. Probe
output is `/tmp/midgard-inclusion-moving-boundary-retained-probe.log`.
The staged swap and both installed CLI smoke checks passed before the
10:42:05 UTC continuation, with the same manifest and blueprint hashes.

That continuation recovered the original prepared artifact and completed proof
and correction in 328.7 seconds, reaching `terminal_included` at 11:00:02.476 UTC.
All ten proof transactions used attempt 1: init, six steps, two publications and
removal. Dependent submissions followed their parent's confirmed journal
observation by 1.823–11.592 seconds; the native inclusion intervals are recorded
above. Staged re-observation/origin replay took 660.2 seconds and binding 39.2
seconds. Honest successor preparation took 379.5 seconds and healthy processing
2.5 seconds, completing functional checks at 11:06:04.565 UTC. This is recovery
evidence across the retained failures, not a clean first-attempt journey.
Its finalized stamp anchored at 11:55:11.532 UTC; `missingSignature` had already
anchored at 11:06:04.691 UTC.

`withdrawnReferenceInput` completed functional checks at 11:55:11.457 UTC after
staging a genuine withdrawal between its prerequisite header and fault. The
fault was `654ac347c4ff24ceecc7731d0950217f340e809e6c81373b86fe0d43`, included
at block 14373; its predecessor was
`f6dace098ceba366655e602ece0e906f80e8779219f44ae63b9f7174`. Staging, including
origin replay and the prerequisite, took 1,197.1 seconds, and binding took 23.7
seconds. Automatic classification took 884.9 seconds while replaying retained
history, nearly exhausting its 900-second budget. A bounded 30-second CPU profile
was captured before proof; decision latency is therefore not an uninstrumented
benchmark. The profile was mostly idle/native-helper and historical-checkpoint
work and does not justify a specific classification CPU speedup claim.

The proof completed in 241.4 seconds with all five submissions on attempt 1 and
no watcher restart. Honest successor preparation required reappointing a scheduler
transaction that expired unminted, then completed in 442.2 seconds. Healthy
processing took 169.5 seconds. Preserve that scheduler recovery when describing
the whole journey. The deliberate 11:55:24 UTC stop at the next family boundary
preserved these results while shared-session work proceeded. Its final stamp
was subsequently authenticated at 12:42:35.239 UTC from the shared recorder.

The first shared session, `sessions/session-hV5k98`, retained authority PID
3006406, watcher PID 3007044 and one native stream across the canonical-to-field
handoff. Canonical staging took 885.3 seconds including initial origin replay;
binding preflight took 40.7 seconds once for the session. Watcher startup ran
concurrently with staging, so its later 0.03-second readiness wait is not the
startup duration. The next family's staging took 261.0 seconds without another
origin replay or binding preflight. Different fixture work and enabled watcher
CPU profiling prevent interpreting this pair as a controlled speedup ratio.

During `committedFieldShape` staging, proof init
`4f8985e8ec157921b86fca24eafe875dba7fe75d1455210b13b0c9a9114024ee`
recorded its signed intent at 12:45:44.817 UTC, targeting commitment
`2c7b67650ca2ac4c780c31336803a36df6963310d5a8d3be326901d77ee91e4f#0`.
Submission was ambiguous; its rebroadcast also failed with an unknown-input
error, and the workflow stalled. Native receipts show the valid commitment at
block 14599 and DA apply
`45cb44c9956b51a1ee9712eb12ff6250f4d5980bacced11875d55b4b6e59e32f`
at block 14602 consuming that output. The apply transaction recreated the exact
attested target at output 0, which Kupo still reported unspent after the stop.
The init transaction was absent from retained canonical native evidence through
block 14606. These facts establish the competing input; they do not establish
that ledger inclusion of DA apply preceded signing the init transaction.

The watcher exited failed-closed with code 70 and restarted as PID 3049053 at
12:46:41.644 UTC, keeping the authority and recorder alive. The second family's
`session.json` therefore records `watcherUse: reused` with a replacement watcher
PID. It is not evidence of uninterrupted watcher execution or a completed second
journey. The batch was then stopped for diagnosis. The recovery change described
below is implemented and has focused and read-only node evidence. The required
emulator gate and installed artifact checks subsequently passed, permitting the
14:28:42 UTC continuation.

That continuation appended the exact old init's `not_found` reconciliation at
14:32:32.266 UTC. Replacement init
`22fbec939a046a7575af9fb264e360c22516f90cdd66f6c12ea06e2a99563e7d`
referenced the attested DA output and was confirmed at 14:39:01.394 UTC. Two
steps and removal followed, reaching `terminal_included` at 14:41:15.788 UTC.
All four replacement proof transactions used attempt 1 for their new action
identities; the original abandoned attempt remained in the journal. Initial
recorder replay and staging took 791.8 seconds while the proof ran. The subsequent
19.3-second proof/correction stage therefore measures verification after replay,
not the full proof execution duration. Watcher CPU profiling remained enabled.

Honest successor `ada81de5081092ccb80528b4ddb103415748d842916bfe5022214c43`
was included at block 14961, slot 310551; its preparation stage completed in
388.8 seconds at 14:48:32.454 UTC. Healthy classification was still catching up
when the watcher failed with `prover funding cannot release idle reconciliation
inputs`. The failed-closed process's last output was at 14:51:18.560 UTC;
restart 1 followed at 14:51:33.342 UTC before the batch was stopped. Its 54-entry
workflow journal ends at `terminal_included`; no functional result or final stamp
was produced. Resume must preserve the
included proof and prepared successor, complete healthy processing, and collect
the pending stamps. The unchanged 30-family selection still starts with this
unfinished journey.

The funding fix now reauthenticates the actuation permit and checks its current
role when releasing idle reconciliation inputs, rather than using the role
captured when the funding port was created. This allows a valid permit's
transition to reconciliation after removal while still rejecting revoked
authority. The exact live failure was reproduced before the fix. The final 27
recovery tests and 17 store tests passed, including pending signed-byte
protection and idempotent release. Watcher typecheck, lint, formatting and staged
build passed; the named node check passed again with 174 tests. Fault-proof
source is unchanged, so its preceding full 434-test gate still applies. The
installed swap, CLI, manifest and native checks passed before the 15:00:27 UTC
continuation.

That continuation encountered two transient trusted-head startup socket closures,
then its existing bounded restart policy reached a healthy watcher. Authority
PID 3153090 stayed alive; final watcher PID 3168162 completed the retained
workflow at 15:15:45.259 UTC without resubmitting its proof or successor. Healthy
processing passed in 222.7 seconds at 15:19:06.500 UTC. Both pending stamps were
written, then the next family captured its baseline at 15:19:06.855 UTC: a
0.355-second handoff using the same services. Preserve the two startup retries
and earlier protocol/funding failures when describing this completed recovery.

The `l2TxMistag` staging failure took 209.4 seconds and reported retryable
`kupo awaitTx failed` at the DA signatures stage. Its original fault commitment
`132be0004ad47645da6f4f4d313fd11ff00e242b756a5a9d1e42ffce81adc150`
was included at block 15066. Independent native receipts show signatures
transaction `a74106103767e0b004eed862f46c4b7231b05780353b5ee8a38ee26569a80e86`
validly included at block 15068, slot 312685. Its exact target DA output has two
attestations against threshold two; Kupo reported that output and the original
fault header unspent after the first stop. No DA apply or proof journal existed
then. Recovery reused the included signatures and retained fault. The timeout
does not establish transaction absence or justify another signatures submission.
The journey provider now sets its optional `awaitTxTimeoutMs` to 600,000
milliseconds. Sixteen focused tests cover the configured boundary; watcher and
node-tools typechecks, lint, formatting, the 174-test node check, staged build,
swap and installed identity checks passed before the 15:31:35 UTC continuation.
Fault-proof source remains unchanged from its full 434-test gate. The bounded
confirmation wait changes neither inclusion policy nor transaction validity.

That continuation included init and two proof steps at blocks 15140–15142,
all on attempt 1. Initial native replay and staging took 827.3 seconds while
the proof ran; the subsequent 0.055-second decision stage only matched the
already recorded decision. These measurements do not establish fresh
classification latency or a completed journey.

Signed removal
`5ed6bfc80aa26a2dad56caf214a3bd290fd4ff7f72a613c6ae38987a4a7ad3f8`
recorded its intent at 15:45:18.833 UTC and an ambiguous unknown-input submission
at 15:45:19.108 UTC. Native evidence authenticates DA apply
`e8dfeddc910172890d66986d3e557df22620b940369fccd30aa7c55df5cb83cb`
at block 15143, slot 314081, consuming the original commitment's output 0 and
recreating the same target at its own output 0. Kupo reported the replacement
unspent. Unlike the earlier init conflict, the old header is an ordinary
spending input in this removal, so reference-only invalidation cannot authorize
its replacement. This exposes a protocol-input recovery gap; it does not justify
discarding the signed attempt or adding a DA-attestation gate.

The deliberate stop preserved the 77-entry journal. Its last record, at
15:48:53.067 UTC, reports actuation revoked during reconciliation after a local
history change; no stable-conflict outcome or successful removal was recorded.
The removal is absent from the closed recorder's canonical evidence through
block 15154, slot 314265. Recovery must preserve the included proof and reconcile
the attested target. No successor, functional result or final stamp was
created for this family. An intermediate repair permitted stable invalidation
of authenticated script-owned ordinary inputs while protecting all collateral
and key-owned or unclassifiable ordinary inputs. Independent review and 146
focused tests passed. Its required emulator run was deliberately stopped with
exit 143 after at least 237 tests had passed, when the scope expanded to
recovering ordinary fee-input replacement as well. That interrupted gate is not
a full pass. The later live recovery is recorded below.

A read-only probe at 15:59:35.788 UTC returned `invalidated` for the exact retained
signed removal at canonical block 15183, slot 314914, with release-final block
15154, slot 314265. It authenticated the old header's script credential and
exact DA spender at block 15143. All six ordinary inputs in this transaction are
script-owned; the other five remained unspent. There are no key-owned ordinary
inputs in these signed bytes. Both collateral outputs
`4652ffcca85bacf3222643ab9b44a54401b6ed7967332ae68e16ee1f320a1e70#0`
and `#1` remained unspent. The probe passed one test in 0.894 seconds, 9.98
seconds overall, and invoked no signer, authorizer, rebroadcast or submission.
An initial diagnostic assertion incorrectly required a key-owned ordinary input;
it was corrected to report the authenticated credential counts. Both logs are
retained. This verifies the new recovery observation, not a rebuilt removal or
a completed family.

The current implementation separates the durable proof objective from each
replaceable signed transaction attempt. Canonical absence plus an authenticated
release-final spend of any exact input can retire that attempt, including a fee
or collateral input. A missing input history or uncertain source observation
cannot establish retirement. The funding layer separately re-queries the
wallet and accounts for its current UTxOs before constructing replacement bytes;
retiring an attempt does not declare a spent coin available. All other unresolved
signed attempts retain their reservations and require their own reconciliation.
The same prepared proof and protocol predecessor remain subject to normal
authentication when the next attempt is built.

An authenticated change to an unsigned linear/cursor action now yields to a
later observation before invoking the stale builder. Generic local-UPLC and
evidence errors remain explicit failures. Unknown signed recovery, bounded
submission batches and action-loop exhaustion likewise yield while preserving
the objective and monotonic attempt history. Authorized rebroadcasts retain the
exact signed bytes and leave the inner polling loop after one attempt; they do
not authorize a new transaction. Existing chain callbacks schedule subsequent
work, including historical catch-up callbacks that can share the same live tip.
The recovery update adds a 30-second rebroadcast backoff using the existing
submission/rebroadcast timestamps, so those callbacks cannot repeatedly send
the same bytes immediately. This is a rate limit, not canonical proof or a
one-rebroadcast-per-head guarantee; it does not delay the next dependent action
after inclusion.

Focused validation of the expanded implementation passed: 130 raw-reader and 24
family-state tests; 122 combined orchestration, runtime and terminal tests;
32 funding-recovery, 17 store and three permit tests; 22 adapter and four
supervisor tests. Final focused reruns also passed the two collateral cases and
one foreign-address case; those overlap the suites and are not additional totals.
The supervisor coverage includes prepared-objective rescheduling only after a
later recovery callback. Watcher and fault-proof typechecks, scoped lint,
fault-proof full lint and the 699-test SDK gate passed. Both staged builds passed
and were subsequently refreshed and swapped before the 19:39 UTC continuation.

The replacement emulator gate that started at 17:01 UTC was deliberately stopped
with exit 143 during fault-proof execution to optimize the gate; its 50 passed
fault-proof cases do not constitute a complete pass. Its node stage had already
passed 52 cases in 432.61 seconds. A parallel, separate named node-gate invocation
was blocked by a migration lock before tests ran and still requires its sequential
rerun. After preserving the same 434 fault-proof cases across 103 files, the
four-worker benchmark passed in 23.09 minutes and the eight-worker benchmark
passed in 21.17 minutes, both with all 434 cases. Those wall times exclude the node stage. See the
[authoritative performance record](testing-status.md#emulator-gate-performance)
for the frozen test inventory, benchmark scope and profiling findings.
The subsequent completion-scoped evaluator reuse passed both expensive cases
and 26 focused regressions. The broad checks and staged-build receipts above
predate that dependency patch. Root refreshed and verified the installed builds
and provider health before the user-authorized live continuation; no additional
test rerun was performed for that launch.
The interrupted log is `/tmp/midgard-objective-attempts-emulator-gate.log`.
The orchestration result is retained in
`/tmp/midgard-objective-attempts-orchestration-complete.log`, and the final source
inventory is `/tmp/midgard-objective-attempts-source-freeze-final.json`. Live
services resumed with 23 anchored families and 29 unfinished; the earlier
completed gate does not establish this expanded implementation's acceptance.

The live objective-recovery receipt is journal sequence 77–85 for the retained
`l2TxMistag` target. At 19:42:17.845 UTC, sequence 77 reconciled old signed removal
`5ed6bfc80aa26a2dad56caf214a3bd290fd4ff7f72a613c6ae38987a4a7ad3f8`
as `not_found`. The replacement consumed the attested header
`e8dfeddc910172890d66986d3e557df22620b940369fccd30aa7c55df5cb83cb#0`
and retained the existing proof output
`9306b71394b96ba385f36f764971d4da447c9249233d8a7ee3ca5956b1781971#0`.
Replacement transaction
`afbdac5d2a46346db91bae293931bc6af214305674c9b20704df0bbb0339a814`
passed local-UPLC preflight, recorded its intent at 19:42:23.470 UTC, was submitted
at 19:42:23.896 UTC and confirmed at 19:42:34.229 UTC. Retirement to replacement
submission took 6.051 seconds; submission to confirmation took 10.333 seconds.
The terminal inclusion record reports slot 328303 and depth 2. These are recovery
measurements for an interrupted journey, not a fresh proof or anchored family
pass. The launch receipt is `/tmp/midgard-objective-live-resume-receipt.json`;
current raw evidence is `sessions/session-xdgkue/native-chain.ndjson`. The later
functional result at 20:00:20.537 UTC preserves this recovery provenance; its
final evidence stamp was written at 20:24:55.594 UTC during the next family
boundary. `withdrawnInput` completed its successor stage in 498.7 seconds and
healthy processing in 617.9 seconds, including source catch-up; these timings
include automatic recovery of the expired first successor commitment.

The original attempt logs remain the primary evidence; timestamps in filenames
identify the attempt, not an additional pass:

| Evidence group                                                                                           | Log filenames under `work/journeys`                                                                     |
| -------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| Observer initial/recovery                                                                                | `live-inclusion-baseline-20260913T203154Z.log`; `live-inclusion-baseline-resume-20260913T222755Z.log`   |
| Double-spend interruption and six-family continuation                                                    | `live-inclusion-remaining-20260913T230003Z.log`; `live-inclusion-remaining-resume-20260914T004922Z.log` |
| Inline-order repair and classification timeout                                                           | `live-inclusion-inline-order-resume-20260914T040725Z.log`                                               |
| Classification repair, missing-signature pass and archive failure                                        | `live-inclusion-classification-resume-20260914T053401Z.log`                                             |
| Archive transport repair and resume-clock failure                                                        | `live-inclusion-archive-network-resume-20260914T063712Z.log`                                            |
| Resume-clock repair and artifact admission failure                                                       | `live-inclusion-resume-clock-20260914T070441Z.log`                                                      |
| Artifact admission repair and retained-boundary failure                                                  | `live-inclusion-artifact-order-resume-20260914T085825Z.log`                                             |
| Moving-boundary recovery, two functional passes and deliberate boundary stop                             | `live-inclusion-moving-boundary-resume-20260914T104205Z.log`                                            |
| Shared-session handoff, canonical functional pass and DA/init input conflict                             | `live-inclusion-shared-observer-resume-20260914T121929Z.log`                                            |
| Reference-invalidation proof recovery and later funding reconciliation failure                           | `live-inclusion-reference-recovery-resume-20260914T142842Z.log`                                         |
| Current-role funding recovery, two final stamps and next-family DA confirmation timeout                  | `live-inclusion-current-authority-resume-20260914T150027Z.log`                                          |
| Bounded confirmation-window recovery, included proof steps and unresolved removal/DA input conflict      | `live-inclusion-confirmation-window-resume-20260914T153135Z.log`                                        |
| Durable-objective recovery, two anchored passes, then source catch-up timeout before next proof          | `live-inclusion-objective-attempts-resume-20260914T193927Z.log`                                         |
| First-observation prefetch continuation, included init/fold, then inconsistent live-thread observation   | `live-inclusion-prefetch-resume-20260914T205556Z.log`                                                   |
| Coherent-snapshot recovery through fold 15, funding-roster failure, and separate harness source-pin exit | `live-inclusion-snapshot-resume-20260914T223942Z.log`                                                   |

Temporary analysis reports retain full transaction hashes, canonical points,
inputs and timing provenance: `/tmp/midgard-inclusion-doubleSpend-reconciliation.json`,
`/tmp/midgard-inclusion-nonExistentInput-timing.json`,
`/tmp/midgard-inclusion-missingSignature-reconciliation.json`,
`/tmp/midgard-inclusion-missingSignature-resume-timing.json`,
`/tmp/midgard-inclusion-missingNativeScriptTx-reconciliation.json`,
`/tmp/midgard-inclusion-missingNativeScriptTx-authority-reconciliation.json`,
`/tmp/midgard-inclusion-missingNativeScriptTx-boundary-reconciliation.json`,
`/tmp/midgard-inclusion-missingNativeScriptTx-resume-timing.json`, and
`/tmp/midgard-inclusion-withdrawnReferenceInput-timing.json`. Shared-session
evidence is summarized in
`/tmp/midgard-inclusion-canonicalDecodability-shared-timing.json` and
`/tmp/midgard-inclusion-committedFieldShape-shared-reconciliation.json`. The next
attempt is recorded in
`/tmp/midgard-inclusion-committedFieldShape-reference-recovery-timing.json`.
The initially included DA signatures and output status are retained in
`/tmp/midgard-l2TxMistag-da-signatures-reconciliation.json`. The next attempt's
exact signed removal inputs and DA receipt are in
`/tmp/midgard-l2TxMistag-removal-da-race.json`; its closed-session proof timings
and journal status are in `/tmp/midgard-inclusion-l2TxMistag-interruption-timing.json`.
The actual script-input recovery probe is
`/tmp/midgard-l2-script-input-invalidation-readonly.json`, with its passing run in
`/tmp/midgard-l2-script-input-invalidation-readonly-final.log` and initial diagnostic
assertion failure in `/tmp/midgard-l2-script-input-invalidation-readonly.log`.
The pre-proof
profile is `/tmp/midgard-withdrawn-reference-finalized-replay-30s.cpuprofile`.
The retained journals and native records remain authoritative if these temporary
analysis files are unavailable.

## Work sequence

1. Provision an independent run directory, network magic, genesis, wallets and
   service ports using the repository's pinned Cardano/Kupo/Ogmios images.
2. Admit the custom chain explicitly, binding its network magic and slot clock
   to the actual genesis used by the native node. Retain normal transaction
   limits, local evaluation, 30-block finality and rollback checks; actions are
   gated on authenticated inclusion at fixed depth 1, and the 30-block depth
   anchors finalized evidence.
3. Establish a real-node baseline for the existing journey. Capture stage
   durations and profiles before changing the measured expensive path.
4. Share deployment and fixture construction where their identities permit it;
   retain independent protocol state and evidence for each family. Use the
   production builders and existing canonical fault material, without mocked
   classifier decisions, proof results, chain receipts, DA responses or clocks.
5. Run all 54 non-interactive catalogue entries and repair defects reached by
   the real flow. The installed interactive `validationTraceDispute` family is
   outside this acceptance scope.
6. Retain a per-family result index with source revision/dirty diff, compiler,
   blueprint, deployment identity, timings, workflow journals and independent
   chain observations. Record failures and incomplete cases explicitly.

The run must not reuse or reset the checkout's persistent Preprod services.
Successful lower-layer or emulator tests do not count as this devnet acceptance.

A retained deployment also retains its catalogue and blueprint identity. When
concurrent work adds an uninstalled family, verify the retained run from an
isolated source tree with its recorded catalogue. Preserve the concurrent work
and record the isolated source hashes; do not relax deployment admission to
load incompatible receipts.

Before the watcher starts, a binding preflight runs the production configuration
loaders for every installed family against the finalized manifest and actual
reference UTxOs. Its application exposes readiness checks and resource cleanup
only. `workflow-binding-preflight.test.ts` runs this check independently of the
watcher launcher; its per-family receipts do not imply live proof completion.

For each family, the runner uses the session's actual parsed configuration
and verified deployment identity to retrieve both staged payloads through the
production public DA runtime. Exact bytes and authenticated peer identities must
match. `public-da-preflight.json` records payload hashes, durations, and failed
requests. The standalone `retained-da-live.test.ts` uses this same preflight for
cheap diagnosis without restarting the watcher or submitting transactions.

The watcher application shares one public DA transport across classification
and workflow runtime leases. Each lease can cancel its own requests; the owner
bounds concurrent requests and queued work and closes after the proof supervisor.
This avoids creating a new libp2p connection for each classification, which can
exceed the retained DA server's connection admission rate. Server limits and
authenticated retention-absence checks remain enforced.
`retained-da-lifecycle-live.test.ts` checks this lifecycle with the actual runtime
configuration and retained payloads: 256 exact fetches across 128 leases,
including 330 seconds idle. A passing transport probe does not establish a
completed proof journey.

Public DA retrieval may repeat an identical request once on a fresh stream after
a recognized stream closure, remote reset or connection closure. Both attempts share the original
deadline, cancellation, peer and protocol identity. Partial responses are
discarded, and each complete response still passes normal authentication and
frame checks. Submission protocols and validation failures are never retried.
Set `MIDGARD_WATCHER_DA_CONTINUOUS_SECONDS=720` for the lifecycle test's bounded
continuous-retrieval variant; its evidence remains separate from proof journals.
Set `MIDGARD_WATCHER_DA_CONCURRENT_LEASES=8` to exercise the actual owner's full
request concurrency; the default remains two leases.

Both public DA endpoints serve the standard libp2p ping control protocol on
existing authenticated connections. Two bounded inbound control streams allow
heartbeat and asynchronous stream closure to coexist with the separate DA
request limit. The watcher still refuses inbound connections and exposes no
inbound DA handlers. Connection monitoring stays enabled: a zero inbound Yamux
stream budget resets heartbeats before negotiation and makes the monitor abort
otherwise healthy connections. The retained listener keeps its DA protocol
allowlist, aggregate admission bounds and outbound dialing restrictions.

The read-only `reference-acquisition-live.test.ts` checks every retained
reference output against its canonical bytes through the actual Kupo/Ogmios
configuration. Ogmios acquisition allows four physical sessions per endpoint;
closing connections retain their capacity until closure is observed.
`l1-events-live.test.ts` also captures the real trace event history across an
actual producer head advance. A typed Kupo head change discards the entire
unpublished snapshot and retries from a fresh release-final boundary, at most
three attempts. Partial reads never carry into another attempt. Explicit
canonical disagreement and other acquisition errors remain distinct failures.

`observation-lifecycle-live.test.ts` holds a real finalized observation for 330
seconds before resolving its block. It records native, Kupo and Ogmios transport
liveness separately, covering the connection age that short backfill tests miss.
It does not submit transactions or change the retained watcher database.
Local query authority renews its endpoint connections every 30 seconds while the
old connection and native authority are still live. Each service owns at most
one current connection and one renewal candidate; it waits for physical closure
before another renewal. Lost or revoked authority is never revived. A closed
transport and an endpoint mismatch have separate diagnostics.

`transaction-recovery-live.test.ts` uses the production signed-intent reader to
check a never-submitted expired transaction, a valid transaction absent from the
real mempool, and a recorded transaction already included on chain. It signs
with an independent plain-Ada publisher input but submits nothing. The recorded
inclusion case remains reusable after the workflow clears its pending funding
state. These source checks do not replace full workflow recovery acceptance.

The CLI emits structured startup phase records before its operations endpoint is
available. Pending records report elapsed time every 30 seconds; completed and
failed records retain stage duration. These records always report
`productionReady: false`; they describe startup activity, not a readiness result.
Journey stage timing also retains failures so a failed start does not disappear
from the timing record.

Availability failures and recovery emit bounded `availability_status` records
into the same CLI log. They retain the authenticated observation identity and
reconciliation duration, including failures caught internally while proof
processing continues. Repeated identical failures are deduplicated; these
diagnostics never assert whole-watcher readiness.

## Shared journey session

`journey.test.ts` lazily opens one `JourneySession` from `journey-session.ts` for
the selected sequential families and closes it in the suite's `afterAll` hook.
The session owns the deployment context, independent native recorder, retained
DA server, authenticated history archives, trusted-head authority and installed watcher. Later families reuse these running services.
The native recorder replays from origin once per session; a new process still
authenticates its history and resumes the existing durable runtime. A standalone
runner call owns and closes its own session.

The watcher starts against the retained journey head before staging when one
exists, or against the first staged target otherwise. Its original readiness
header is a startup binding; subsequent classification observes the current
queue. Before staging each family, the runner captures existing workflow journal
prefixes. That baseline distinguishes retained completion from work the live
observer performs during staging and preserves the resumed progress clock.
Fixture wallets keep their existing Base addresses, while prover wallets use
Enterprise addresses. Address-scoped selection isolates their UTxOs even where
they share a payment key; the session requires no wallet pause API.

New session artifacts live under `work/journeys/sessions/session-*`: process
configuration, binding preflight, `authority.log`, `start.log`, restart records,
session `timings.ndjson`, and the shared `native-chain.ndjson`. Persistent watcher,
availability, trusted-head and workflow state remains in `work/journeys/runtime`.
Each family keeps its own staging, correction, successor, result and timing
artifacts. Its `session.json` identifies the session, process, configuration,
binding receipt and native evidence path; `workflow-baseline.json` records the
captured prefixes. Results and finalized stamps point to the actual recorder
path. Historical per-family native files remain evidence for their original
attempts and are not rewritten to resemble shared-session runs.

Session reuse checks pinned configuration, deployment and executable inputs,
including compiled watcher and fault-proof artifacts. A failed-closed watcher
can restart within the existing bounded policy using the same bindings and live
services; unrelated exits or changed identities fail the session. Initialization
and family failures close owned services, and cleanup attempts every owner even
if one close operation fails. A normal family handoff leaves the services alive;
the last selected family still waits for outstanding finalized stamps before
suite cleanup.

Keeping the watcher active exposes a DA fixture race: a proof can consume the
attested queue output before the publisher's post-submit live-UTxO lookup. The
publisher now checks the apply transaction returned by the independent native
recorder. It verifies the exact signed body hash, the ledger's valid branch,
one output at the expected queue address carrying its token, and the expected
header and DA bond in the datum. This authenticated receipt remains checkable
after the output is spent. Locally signed bytes alone do not establish inclusion.
Callers without the native receipt reader retain the existing live-output check.

Validation before the first shared-session attempt: nine session tests, eight DA
receipt tests, two native recorder tests and three address-isolation tests passed,
as did the node-tools typecheck. These establish lifecycle, receipt and wallet
behavior in focused tests. The live attempt above established one functional pass and a
session handoff, then exposed the separate DA/init input conflict.

The recovery change uses the common signed-transaction reader. Before permitting
replacement, it proves the exact old transaction is absent from the canonical
chain and an exact input has been consumed by another authenticated transaction
at the release-final boundary. That yields `invalidated`, which retires only the
signed attempt through the existing abandonment path. Input roles do not change
the fact that those exact bytes can no longer be included. Funding recovery
separately re-observes live wallet inputs and preserves every other unresolved
attempt's reservations before rebuilding. A spend that has not become stable remains pending;
uncertain observations cannot establish absence. If an authorized rebroadcast
RPC fails, the same signed intent remains pending for reconciliation instead of
being treated as absent.

Linear and cursor workflows rebuild the same initialization or current
step/thread after that authenticated outcome. `doubleSpend` now uses the same
signed reconciliation helper. No DA-attestation gate is added: normal actions
still progress at authenticated inclusion depth 1. Stable expiry or spend
evidence is required only when replacing an exceptional unresolved signed
attempt, so ordinary proof steps do not inherit a release-finality wait.

A read-only probe of the retained init returned `invalidated` at canonical
block 14659 (slot 304403), with release-final block 14630 (slot 303907). Its
spent commitment output was reference-only. The signed ordinary input
`6fe85c6d2b4e4974f66c2a417727a3b75cff3cb007915ca0c0b3adba85565dfd#1`
and collateral
`4652ffcca85bacf3222643ab9b44a54401b6ed7967332ae68e16ee1f320a1e70#0`
both remained unspent; the reader authenticated all seven input references.
The probe invoked no signer, authorizer or rebroadcast and submitted nothing.
One test passed in 0.85 seconds, 10.45 seconds overall. Exact points and inputs
are in `/tmp/midgard-committed-field-reference-invalidation-readonly.json`; its
companion `.log` records the run. This proves the recovery observation, not a
completed abandonment, rebuilt submission or recovered live journey.

Recovery validation so far: 133 generic reconciliation tests, three transient
rebroadcast tests, 70 double-spend tests and eight staging/checkpoint tests
passed. Named SDK and node checks passed with 699 and 174 tests respectively.
The full required emulator gate exited zero: its node component passed 52 tests,
and fault proofs passed 434 tests across 101 files in 4,798.46 seconds. Final
fault-proof, watcher and node-tools typechecks, the staged fault-proof build and
staged CLI check passed. After the swap, installed artifact byte comparisons,
CLI/native checks and retained manifest hashes also passed. The harness's final
19 correction-recovery tests passed alongside the unchanged 12 correction and
nine session tests; scoped lint and formatting passed. The 14:28:42 UTC live
continuation used `sessions/session-crQPbh` and retained the stopped attempt's
original journal. Live replacement and proof completion succeeded, but the later
funding failure prevented healthy completion and the next family handoff.

The prepared 14:28 and 15:00 resume selection was
`/tmp/midgard-inclusion-reference-recovery-families.csv`: 30 unique unfinished
families, starting with `committedFieldShape`, with `fabricatedDeposit`,
`fabricatedWithdrawal` and `withdrawalMistag` last. It excluded the then 21 anchored
families and the functional canonical journey. The normal boundary sweep has
since completed the latter's stamp. The first family retained its original
staged commitment and signed init; recovery reconciled those records against the
attested replacement output before constructing new bytes. After its completed
recovery, the next continuation needs only the other 29 selected families.

## Shared configuration and verification

The common Phase 4 generator applies the verified Preprod profile before genesis
hashing. The snapshot records its query time and canonical source tip. Both the
CLI protocol-parameter representation (used to render genesis) and Ogmios
representation (used for an exact live comparison) are retained. Refresh the
profile explicitly after target-network changes and generate a fresh chain.

`devnet/watcher-journeys/configuration.test.ts` checks the running node without
publishing contracts. `deployment.test.ts` runs the same gate before funding or
publication. Local credentials, network magic, start time, initial funds and
initial delegation remain isolated test-network state.

## Devnet consensus timing

The watcher acceptance generator uses verified Preprod consensus timing:
`slotLength=1`, `activeSlotsCoeff=0.05`, `epochLength=432000`, and
`securityParam=2160`. Byron's `protocolConsts.k` is also 2160. All genesis
changes are applied and their configuration hashes recomputed before startup.
The deployment check refuses a run with different values.

Blocks are probabilistic, with approximately 20 seconds between blocks; an epoch
lasts five days. Acceptance retains its existing 30-block confirmation rule,
which now takes approximately ten minutes. This is an acceptance rule, separate
from the chain's consensus security parameter.

Changing timing requires a fresh run directory and chain. Stop the previous
watcher Compose project, generate the replacement, and deploy its protocol state
afresh; never reuse a previous chain's database or deployment receipts.

## Devnet cost models

The watcher devnet pins Cardano node 11.1.0 by image digest. This is a
prerelease used only by this isolated acceptance environment. Node 11.0.1
silently retains the original Plutus V1/V3 cost models when `extraConfig`
supplies replacements ([ledger issue 5896](https://github.com/IntersectMBO/cardano-ledger/issues/5896));
the [11.1.0 release](https://github.com/IntersectMBO/cardano-node/releases/tag/11.1.0)
includes the fix.

The original 251-entry V3 model caused the real DA attestation's
`count_set_bits` call to exhaust both local and node evaluation budgets. The
replacement models in
`demo/midgard-node-tools/devnet/preprod/configuration.json`
were read from the local Preprod node at protocol version 11. They are supplied
through the supported Alonzo `extraConfig.costModels` field; the original
era-specific genesis fields retain their required historical lengths. Before
funding or publication, the deployment test compares every live model entry
against this pinned input, as well as the 16,384-byte transaction limit and
17,500,000-memory / 10,000,000,000-step transaction limits and
77,500,000-memory / 20,000,000,000-step block limits. The complete live Ogmios
protocol-parameter object must equal the recorded Preprod object, including
economic and governance parameters; selected-field checks are insufficient.

The fixture wallets use the same Scalus local evaluator as the watcher availability
runtime. With the corrected live cost models, node evaluation accepts the DA
signature transaction at 318,448 memory and 248,474,366 CPU units; Lucid's
default Aiken evaluator still reports a budget overflow for that transaction.
Local evaluation remains enabled and the actual node validates every submission.

Each run retains its genesis, deployment receipts, checkpoints, native block
stream, watcher journals, and stage timings. A failed run is stopped and retained,
not overwritten with a different genesis. Historical transaction-phase timings
and profiles remain evidence for their original runs; new runs do not generate them.

## Bounded reference publication

Run the builder/emulator checks and the small real-node publication probes before
starting a complete deployment. The shared publisher records exact signed bytes,
inputs, outputs, dependencies and role assignments before submission. Children
spend the designated predecessor change output; confirmed reference outputs and
role tokens remain reserved. Pending transaction count and signed bytes both
apply backpressure. Local evaluation receives the exact unconfirmed inputs.

The run's `work/publication-schedule.json` binds those limits and the measured
preparation rate, chain capacity estimate and bounded confirmation allowance.
Its companion evidence records how those measurements were obtained. The
planned workload determines the minting deadline. Publisher-signed policies
proceed after canonical publication and the role-token audit, trusting the
publisher key until expiry. Historical time-only policies still require canonical
expiry: check the node tip and its exact indexed checkpoint, never just a local
clock or elapsed timer. Report publication duration and readiness audit separately.
On restart, reopen the same authority and signed journal; reconcile canonical
transactions before extending it. An expired or invalid parent requires all
recorded descendants to be resolved; it does not authorize a replacement
publication identity.

The September 11, 2026 isolated-node run published all 517 roles through 444
transactions (4,842,075 signed bytes) in 18m23.151s of recorded journal time,
including an interruption
with eight pending transactions and restart. The peak was eight unconfirmed
transactions and 98,502 signed bytes against bounds of eight and 100,000.
Every transaction confirmed; the journal retained its original signed records,
with no duplicated roles, reused inputs, spent reference outputs or unresolved
outcomes. Publication finished with 34m44.857s remaining in its authority
window. Independently, the first and last publications were included at slots
1590 and 2687: 1,097 one-second slots apart, with 2,085 slots remaining before
authority expiry. Canonical closure and all 517 unique retained roles were
verified at slot 4863, beyond expiry slot 4772. The run retains
`work/full-publication-evidence.json`, `work/publication-closure.json` and
`work/publication-transactions.ndjson` for independent inspection.

Separate small real-node probes cover a lost acknowledgement after node
acceptance, byte-bound backpressure, and expiry of an accepted parent with a
pending child. These publication results establish neither automatic watcher
journeys nor workflow interruption recovery. Initialization, the authority-expiry
wait and final deployment uniqueness verification remain distinct stages.

## Recovery and maturity

Exercise follower outages while the producer continues, then replay the retained
canonical intersection through subsequent live blocks. Native follower recovery
is a lower-layer gate; workflow recovery additionally needs proof initialization,
intermediate-step and lost-acknowledgement interruptions through the common
runner, followed by independent token, correction and economics checks.

Native node-to-client streaming must survive waiting for a block and consumer
backpressure. The pinned muxer's segment deadline starts when the first byte
arrives; incomplete headers and payloads remain bounded. Exact-point queries
retain their absolute operation deadline. Native failures after startup retain
the operation, helper identity and bounded stderr cause, while revoking the
failed helper's authority. The real-node regression pauses the stdout consumer
for 130 seconds, then checks ordered catch-up; the old helper failed this case
with a socket read timeout.

The deterministic single-deposit `transitionTrace` fixture has fifteen dependent
transactions, including its preimages, checkpoint folds, proof token and removal.
The fixture checks the real projected output against the audited plan: one
41-byte output, three scan primitives and two value primitives. Its timing helper
reads the retained genesis and verified deployment manifest, and checks the
runtime's authenticated finality depth, which must still be the manifest's
thirty. Action stages are budgeted from the action depth, not from that depth:
one actual block at one-second slots and `f=0.05` averages twenty seconds per
inclusion gate, where thirty blocks average ten minutes.

The trace harness allows twice the expected inclusion time per dependent
transaction, plus explicit transaction preparation, RPC, startup and successor
allowances, plus one release-finality window for the finalized evidence stamp.
The harness passes `actionDepth: 1` to its timing helper, matching the runtime's
fixed inclusion policy; this gives 47.5 minutes for correction under the captured
configuration. The helper can also calculate the historical depth-30 baseline:
337.5 minutes for correction and 503.5 minutes for the complete retained journey.
Its `actionDepth` argument controls test budgets only and is not a watcher
configuration field. The finalized evidence stamp adds 20.5 minutes in either
calculation: it is one
release-finality window plus its RPC allowance. These are finite test
deadlines, not finality guarantees. They do not change transaction
validity, consensus or confirmation depth.

Every other automatic family uses the same cadence formula over a finite generic
plan of twenty-four dependent transactions, which bounds the largest installed
proof chain with margin; a family that needs more must publish an audited plan
instead of widening the bound. Because that bound is loose, the correction stage
also fails as soon as the workflow journal records no durable progress (anything
other than a per-block reconciliation) for one transaction allowance: twice the
expected confirmation time plus preparation and RPC. Without an unresolved signed
intent, a fresh or progressing family fails within one action allowance if
progress stops. A retained workflow first gets 900 seconds to re-observe its
target and produce new durable progress; repeated pending reconciliation records
cannot reset that initial clock. The whole family
cap remains in force. The journey tolerates journaled stalls for the watcher's
whole preflight retry budget plus one retry delay. Poll deadlines use a monotonic
clock, and a resumed attempt preserves its entire pre-launch journal prefix while treating newly recorded
failures as failures of the current attempt.

An unresolved signed intent receives at least the existing finalized-evidence
window for exceptional reconciliation, rather than expiring the ordinary action
clock while a reference spend becomes stable. Repeated pending observations do
not reset this bounded clock. The first `not_found` outcome matching an exact
action ID and transaction hash counts once as durable progress; replacement
then returns to the ordinary action allowance. Duplicate or mismatched outcomes
cannot extend the deadline, and the whole correction cap remains unchanged.
Final transaction authentication omits only an exact intent durably abandoned
this way, including when its replacement binds a different output reference;
confirmed submissions remain subject to the existing checks.

Fault staging, the proof chain, the honest successor commitment, and healthy
processing advance on authenticated inclusion. The production journal records
`terminal_included` for provisional completion and `completed` only after the
terminal is independently reauthenticated at release depth. Rollback invalidates
cached authority; the worker re-observes current state and reconciles submitted
attempts before retrying or rebuilding. A suitable signed transaction can be
rebroadcast under fresh authority. The existing chain index and submission
journal supply recovery state; no per-family inverse workflow is required.

The harness persists `pending-evidence-stamp.json` before its inclusion-based
`result.json`. The shared watcher continues reconciliation across families and
restart. At each family boundary the harness stamps pending requests whose
journals have reached `completed`; only the last selected family waits for all
remaining stamps before stopping services. `finalized-evidence-stamp.json`
records the release policy digest, actual finalized terminal, stamp time, and
independent native evidence path; `finalized-workflow.json` retains its validated
journal. The recorder must have replayed enough canonical blocks to establish
the proof-token, removal, and honest-successor depths. A reauthenticated terminal
may have a different inclusion point after rollback.
A historical completed proof can resume its unfinished honest-successor stage
against the authenticated current retained head when it has no prepared successor.
The original fault and removal evidence stay unchanged, and a separate
`successor-predecessor.json` records the resumed continuation. Existing prepared
or signed successor attempts keep their predecessor until reconciled.

All families run sequentially in one Vitest process, while anchor observation
overlaps subsequent proof execution. Report active execution and anchoring
latency separately. A provisional result is insufficient for finalized release
evidence until its stamp exists.

Test the authority transition through the actual follower callbacks: history
advance, authenticated queue removal, and subsequent workflow reconciliation.
Removal may restrict authority to checking the exact recorded transaction and
terminal result. It must not authorize another submission. A real rollback or
lost canonical authority still revokes reconciliation. Restart tests must pass
through the supervisor queue as well as the workflow runner; a queue job marked
finished does not establish that its transaction journal completed.

Preparation and completion each cross a SQLite reservation and an immutable
directory journal. Crash tests must close and reopen both stores between those
writes. The durable preparation handoff retains the exact signed transaction,
action and expected journal prefix. The completion handoff retains the verified
terminal result with the funding release, so recovery cannot reopen released
inputs. An expiry/absence result needs the same interruption analysis before
discarding pending transaction context.

The real-node `transaction-recovery-live.test.ts` gate reads a never-submitted
expired transaction, valid transactions with and without expiry absent from the
node's mempool, and an already included recorded workflow transaction. It retains
the exact signed transaction for rechecking after workflow completion and uses the runtime's admitted
source and signs diagnostic intents without submitting them. This establishes
the adapter's expiry and rebroadcast eligibility decisions; actual rebroadcast,
crash recovery and complete proof evidence remain separate gates.

A frozen standalone producer has the actual consensus forecast horizon derived
from `3*k/f` (129,600 one-second slots for this profile). Phase 4 refuses a restore
at or beyond that boundary before replacing durable state. This is distinct from
an ordinary follower outage on a continuing canonical chain. There is no
72-hour snapshot-reuse guarantee.

The `crossBlockDuplicateEvent` journey requires a real prior settlement. Stage
its honest deposit history early by adopting the independently verified
`transitionTrace` successor and its exact genuine deposit. The preparation gate
checks the completed trace result, native commitment, retained payload, deposit
role and inclusion interval before recording the source header's end time plus
seven days. Run `history-preparation.test.ts` with
`MIDGARD_WATCHER_JOURNEY_PREPARE_HISTORY=1` immediately after the trace passes.
Resume the existing SDK merge path when that actual maturity has elapsed and a
later healthy queue head exists.
Keep this family time-gated until the settlement exists; a prior queued header
or a fabricated settlement cannot satisfy it. Other families can advance the
same queue while that prerequisite matures.
