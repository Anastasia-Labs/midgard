# §3.2 Necessity artifact — transaction general-field items above the single-publication maximum

## Binding

- Family / item: `transaction-field-chunk` (`TransactionFieldChunkWitness`) /
  one canonical bounded-field item of any of the nine V1 general fields;
  maximum shapes measured: 16,384-byte ledger-output item and a
  32,768-byte aggregate-field item.
- Applied validator hashes measured (re-measured 2026-08-03, see
  "Re-measurement 2026-08-03" below):
  `983051b4a0c3fe90057a599e77ed44c5ab694014036d49c86373a143`
  (`canonical_decode_item_semantic_v1` applied on the measurement deployment
  with `hub_oracle=11…11`, `catalogue=22…22`),
  `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab`
  (`proof_item_v1`, unparameterized — unchanged); unapplied blueprint hash
  `62501cfe7cf63485a493c902060cd422acdd88757c319345eadb8819`;
  blueprint `onchain/aiken/plutus.json` sha256
  `277b6457197870a9df069ce5c492c166e8d0b4b32fb616294ae12404ecb070b6`.
  Any change invalidates this artifact; re-measure before CG5
  (GOAL_SPEC.md §3.2).
  Superseded pin (2026-07-29): applied
  `925662085ac87eb3cd63221b5184f59fde2c8b46d8db93052e80fc96`, unapplied
  `547cc8b7a136515c85cf51a8a3a32ecae63fb8859cf63ef5f2daa893`, blueprint
  `6d23a25f8cb96f62f3e3aeeecb4e1506e8002ac712ae9bcb8873e42b4136ff1a`.
- Re-verification 2026-08-04 (C21-AUDIT, issue #484): the generated
  `onchain/aiken/plutus.json` is now SHA-256
  `f5ae651e34cf3e1175d928634c002580c4f2af4659a229952007c458945b866b`, so the
  whole-file blueprint pin above is superseded. Both validators this artifact
  binds are byte-identical in that blueprint:
  `fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend`
  is unapplied `62501cfe7cf63485a493c902060cd422acdd88757c319345eadb8819` and
  `fraud_proofs/validation_trace/proof_item_v1.main.else` is
  `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab`. The applied
  hash `983051b4a0c3fe90057a599e77ed44c5ab694014036d49c86373a143` therefore
  still follows from the unchanged unapplied script and the parameter
  snapshot pinned above, and the measurements below remain bound. The
  whole-file digest moved because other validators in the same blueprint
  changed; that was not diffed here, only the two bound script hashes were
  compared. A fresh applied re-measurement is still owed before CG5 release
  closure.
- Parameter snapshot digests: consensus profile
  `midgard-consensus-v1` digest
  `181730d304796b764c8f657b0ae788b87c6aba9f4491dbfa9ce24d99932911b7`;
  capability floor per
  `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`
  (Conway epoch 645: `maxTxSize` 16,384; execution 16,500,000 /
  10,000,000,000; `utxoCostPerByte` 4,310). No standalone snapshot digest
  file exists yet; this artifact binds the decision record plus the profile
  digest above.
- Fixture: deterministic generators in
  `demo/midgard-validation/tests/complete-item-proof-fit-v1.test.ts` and
  `demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts`
  (exact-size canonical output items; regenerable by running the suites).

## Measurements (§3.2 order — stop at the first representation that fits)

Execution reserve applied: 20% below the 16,500,000-memory /
10,000,000,000-CPU floors → 13,200,000 / 8,000,000,000
(docs/consensus-profile-v1.md §10, GOAL_SPEC.md §3.3).

| Representation | Tx bytes / maxTxSize | Mem / limit·0.8 | CPU / limit·0.8 | Fee | Fits §3.3? |
| --- | --- | --- | --- | --- | --- |
| 1. Complete item direct in proof tx | measured frontier: a 13,282-byte item yields exactly 16,384/16,384 (margin 0); 13,283 bytes → 16,385 (over by 1); a 16,384-byte item's `Verify` redeemer alone exceeds the envelope | 205,594 / 13,200,000 | 500,275,649 / 8,000,000,000 | 974,576 | NO above 13,282 bytes |
| 2. Complete item as inline-datum publication + reference consumption | pub fits through 14,396 bytes: 15,256/16,384 (margin 1,128; min-Ada 65,576,650); a 16,384-byte item's complete signed publication measures 18,290/16,384 (over by 1,906); 32,768 bytes → 35,186/16,384 (over by 18,802) | consuming tx 264,106 / 13,200,000 | 552,114,352 / 8,000,000,000 | pub 826,821; consume 376,690 | NO above 14,396 bytes |
| 3. Minimum multi-output publication + complete logical reconstruction | not deployed for this family; the bounded-chunk stream below already reconstructs the complete item from ≤4,095-byte authenticated chunks bound to one item commitment | — | — | — | superseded by 4 |
| 4. Bounded chunk consumption (`TransactionFieldChunkWitness`, ≤4,095-byte chunks) | every chunk reveal ≤ 4,675-byte publication (`MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxFieldPublicationUnsignedTransactionBytes`, pinned by `demo/midgard-sdk/tests/tx-order-v1.test.ts`) | 3,398,228 / 13,200,000 | 1,209,745,039 / 8,000,000,000 | per pinned receipt measurements | YES |

## Exact limiting constraint

`maxTxSize = 16,384` on the complete serialized transaction. Measured with
complete signed constructions against the applied validators: the direct
proof transaction crosses 16,384 at a 13,283-byte item (16,385 bytes,
Plutus-data 64-byte chunk framing costs 2 bytes per 64 plus 2,686 bytes of
transaction/continuation framing), and the single publication transaction
crosses it between 14,396 (15,256 bytes) and 16,384 item bytes
(18,290 bytes, overshoot 1,906). Items up to the 16,384-byte ledger-output
maximum and the 32,768-byte aggregate-field maximum are legal canonical
content, so a bounded fallback is required above 14,396 bytes.

## Why no simpler authenticated representation closes the gap

The item bytes themselves exceed what one L1 transaction can carry: even a
zero-overhead publication of a 16,384-byte item equals the whole envelope
before any datum framing, input, fee, or signature. Splitting the datum
across outputs of one transaction does not reduce the transaction's total
serialized size, and referencing cannot help until the item is published.
The deployed bounded-chunk stream is the minimum additional machinery: it
reuses the same per-item commitment (chunk tree root) that representation 1
and 2 authenticate, so no second commitment scheme is introduced.

## Preserved complete-item path

Items at or below 13,282 measured bytes fit representation 1; items at or
below 14,396 bytes fit representation 2 (`deriveValidationProofItemPublicationV1`
plus `VerifyReference`); the producer keeps the complete-item witness for
every item at or below `maxSinglePublicationCompleteItemBytes` and emits
chunks only above it
(`demo/midgard-validation/src/validation-machine.ts`, single guarded site,
pinned by `demo/midgard-validation/tests/complete-item-carriage-policy-v1.test.ts`).
Both representations authenticate the same bounded-item commitment and the
equivalence and rejection tests live at
`demo/midgard-validation/tests/complete-item-equivalence-v1.test.ts`
(omission, duplication, reorder, substitution, trailing data reject in both)
and `demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts`
(identical terminal state through direct and reference carriage; deployed
validator rejects substituted and trailing-byte published items).

Caveat recorded for the ledger: `MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes = 13,998`
is invalidated by these measurements (a 13,998-byte item's direct proof
transaction measures 17,122 bytes); the measured direct frontier is 13,282
bytes and the automatic direct/reference selector must move to it.

Caveat resolution (2026-07-31, recorded here 2026-08-03 — this paragraph is
historical record, not a live instruction): the selector was NOT moved to
13,282. Commit `92426384` examined the instruction and refused it, rebinding
`maxReliableDirectCompleteItemBytes` to 8,273 (zero-reserve
`maxExactDirectCompleteItemBytes` 8,769) instead. Reason: 13,282 and 13,998
are both by-reference-basis numbers — the transaction sources the validator
from a reference input and embeds no script witness — while the deployed
direct route embeds the applied validator (reference-input count 0) and is
limited by the observation stage. Because the selector is
`itemBytes <= constant`, moving it from 8,273 to 13,282 would have WIDENED
direct-carriage acceptance on a basis the deployed route does not match,
which is recorded as a soundness regression rather than a fix. The frontier
is now pinned in both directions (8,273 direct / 8,274 reference, and
`carriage(13_282)` and `carriage(13_998)` both "reference") by the "pins the
direct complete-item carriage frontier in both directions" case in
`demo/midgard-core/tests/consensus-profile-v1.test.ts`, whose comment
retains 13,282 as this artifact's by-reference frontier and forbids binding
it to the direct constant. Consequence for the "Preserved complete-item
path" paragraph above: items at or below 13,282 bytes fit representation 1
on the by-reference basis measured here, but the deployed selector carries
only items at or below 8,273 bytes directly and routes 8,274–14,396 bytes
through representation 2. The complete-item path is preserved across the
whole band; only the split point between its two forms is lower than this
artifact's text implies.

## Re-measurement 2026-08-03 (task C21-AUDIT)

This is the shared re-measurement basis for all seven §3.2 necessity
artifacts; the other six reference this section rather than repeating it.

**Blueprint provenance.** `sha256sum onchain/aiken/plutus.json` =
`277b6457197870a9df069ce5c492c166e8d0b4b32fb616294ae12404ecb070b6`,
Aiken `v1.1.22+39d6b04`, 376 validators. `onchain/aiken/plutus.json` is
gitignored (`onchain/aiken/.gitignore:8`), so this digest pins a local build
rather than a committed file. This particular build is an OVERLAY build: it
includes four untracked validators
(`script_sources_stage_one_redeemer_{finalize_frame_executor,fold_map_executor,outer_normalizer,traversal_normalizer}_v1`)
and the modified `cek-data-traverse-v1.ak` / `redeemer-item-proof-v1.ak`
libraries. A build from the committed tree produces a different digest and a
smaller validator count, so this pin must be refreshed once the
stage-one-redeemer family and its owner security ruling (RF-021) land.

**Re-verified unchanged.**

- `proof_item_v1` applied hash
  `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab`
  (unparameterized, identical in the new blueprint).
- Consensus profile digest
  `181730d304796b764c8f657b0ae788b87c6aba9f4491dbfa9ce24d99932911b7`
  (`cd demo/midgard-core && node scripts/sync-consensus-profile-doc-v1.mjs
  --check` exits 0 against the current tree).
- Every structural guardrail these artifacts bind:
  `maxSinglePublicationCompleteItemBytes` 14,396,
  `maxTransactionFieldChunkBytes` 4,095,
  `maxTransactionAggregateFieldBytes` 32,768,
  `maxLedgerOutputPreimageBytes` 16,384, `maxMintPreimageBytes` 32,768,
  `maxDistinctAssetCount` 16,384, `maxNativeScriptNodeCount` 16,384,
  `maxNativeScriptDepth` 16,384, `maxCekProgramEnvelopeBytes` 50,
  `maxCekProgramNodeCount` 1,597,819,
  `maxCekProgramMaterialBytes` 67,108,418,
  `minSupportedL1MaxTxBytes` 16,384, and the 9,215-byte direct-constant gate.
- Every `MIDGARD_V1_ENVELOPE_MEASUREMENTS` value these artifacts quote:
  `maxFieldPublicationUnsignedTransactionBytes` 4,675,
  `maxProgramMaterialPublicationDatumBytes` 4,268,
  `maxProgramMaterialPublicationUnsignedTransactionBytes` 4,369,
  `maxFieldChunkReceiptPublicationMemoryUnits` 3,398,228,
  `maxFieldChunkReceiptPublicationCpuUnits` 1,209,745,039.
- Every fixture and test path cited by the seven artifacts still exists.

**Moved.**

| Measurement | 2026-07-29 pin | 2026-08-03 |
| --- | --- | --- |
| blueprint `plutus.json` sha256 | `6d23a25f…36ff1a` | `277b6457…b070b6` |
| `canonical_decode_item_semantic_v1` unapplied | `547cc8b7…a893` | `62501cfe…8819` |
| `canonical_decode_item_semantic_v1` applied | `925662085…fc96` | `983051b4…a143` |

The applied hash was reproduced with the artifacts' own documented
parameters — `buildValidationTraceDisputeFaultProofContracts` over the
current blueprint with `hubOraclePolicyId = "11".repeat(28)` and
`fraudProofCataloguePolicyId = "22".repeat(28)`, reading
`semanticResolvers[1]` (the `canonicalDecodeItem` entry of
`VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics`).

**Fresh by-reference byte series.** Produced by the tree's own unchanged
measurement script (`cd demo/midgard-core && node
scripts/measure-validation-proof-item-envelope.mjs`, whose measurement logic
is byte-identical since it was introduced at `4a4bc660` — `92426384` renamed
its output labels only) against the current blueprint. Every transaction
below is signed (one vkey witness) and sources its validator from a
reference input. Collection proof shape: 434 items, item index 433, 9
frontier peaks, 9 siblings.

| Item bytes | Publication tx | Publication datum | Proof tx (validator by reference) |
| --- | --- | --- | --- |
| 4,095 | 5,249 | 4,995 | 5,659 |
| 13,282 | 14,724 | 14,470 | 15,134 |
| 14,396 | 15,872 | 15,618 | 16,282 |
| 16,384 | 17,922 | 17,668 | 18,332 |
| 32,768 | 34,818 | 34,564 | 35,228 |

Frontiers on that basis: complete-item publication exact maximum 14,892
item bytes (datum 16,130, min-Ada 70,412,470, fee 876,277, transaction
16,384/16,384; 14,893 bytes → 16,385, over by 1), reliable 14,396 (datum
15,618, min-Ada 68,205,750, fee 853,749, transaction 15,872/15,872);
semantic-proof-validator-by-reference exact maximum 14,494 (redeemer 15,954,
transaction 16,384/16,384), reliable 13,998 (redeemer 15,442, transaction
15,872).

**Conclusions still supported: YES, on independent evidence.** The two
family maxima remain byte-impossible in one transaction by a wide margin: a
16,384-byte item's publication measures 17,922/16,384 (over by 1,538) and a
32,768-byte item's measures 34,818/16,384 (over by 18,434), so a bounded
fallback is still required. No threshold that justified the decomposition
moved in the direction that would remove it; the deployed direct/reference
split point moved DOWN (to 8,273), which strengthens rather than weakens the
case.

**Basis mismatch found in the recorded tables — for owner review.** The
recorded figures are not reproducible from the measurement script even
though both are signed by-reference constructions, and they do not agree
among themselves on a single basis. At the two family maxima the recorded
values sit a uniform 368 bytes above the fresh ones (16,384 → recorded
18,290 vs fresh 17,922; 32,768 → recorded 35,186 vs fresh 34,818), which is
consistent with a different collection proof shape. But at the publication
cap the recorded value is 616 bytes BELOW the fresh one (14,396 → recorded
15,256 vs fresh 15,872), so the recorded row cannot share a basis with the
two above it. `demo/midgard-core/src/consensus-profile-v1.ts` attributes the
15,256 figure to "the applied SDK publisher", which is a third construction
again. None of this changes a conclusion — every value on every basis still
overshoots the envelope at the family maxima — but the tables should be
regenerated from one declared construction before CG5 so the artifact states
a single reproducible basis.

**Not re-measurable in this pass — carried forward unverified.** These
figures are pinned by no source constant and by no current test, and are not
reproducible from the measurement script: 15,256, 18,290, 35,186, 16,900,
205,594, 500,275,649, 974,576, 264,106, 552,114,352, 826,821, 376,690,
65,576,650, and the CEK one-shot pair 45,154,331 / 14,905,078,582.
Regenerating them requires re-running
`demo/midgard-validation/tests/complete-item-proof-fit-v1.test.ts`,
`complete-item-proof-fit-emulator-v1.test.ts`, and
`demo/midgard-sdk/tests/tx-order-v1.test.ts`. They are left as recorded
rather than replaced with numbers from a different construction.
