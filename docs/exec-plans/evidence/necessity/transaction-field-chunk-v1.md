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
  `onchain/aiken/plutus.json` of that epoch was SHA-256
  `f5ae651e34cf3e1175d928634c002580c4f2af4659a229952007c458945b866b`
  (380 validators, `aiken v1.1.22+39d6b04`), which superseded the whole-file
  blueprint pin above. Both validators this artifact binds were byte-identical
  in that blueprint:
  `fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend`
  unapplied `62501cfe7cf63485a493c902060cd422acdd88757c319345eadb8819` and
  `fraud_proofs/validation_trace/proof_item_v1.main.else`
  `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab`. That pass
  compared only the two bound script hashes and *inferred* the applied hash
  `983051b4a0c3fe90057a599e77ed44c5ab694014036d49c86373a143` from them, so it
  left a fresh applied re-measurement owed. The `f5ae651e…` digest is retained
  here as measurement-epoch provenance only.
- Re-verification 2026-08-06 (issue #546) — applied re-measurement, not
  inference. A fresh stock `aiken build --env testnet` of the current tree
  (`aiken v1.1.22+39d6b04`) produces `onchain/aiken/plutus.json` SHA-256
  `605c8b8dca1f01e2cde5219138a1f81e69214f9a182c10b73c20341187ddc2dc`
  (391 validators, including the chunked-MPF and harvest additions); that is
  the current whole-file pin and it supersedes both `277b6457…` and
  `f5ae651e…`. Measured against it, unchanged: the two bound scripts are
  byte-identical again (`62501cfe…` unapplied, `22c9a103…` for
  `proof_item_v1`), and the applied hash
  `983051b4a0c3fe90057a599e77ed44c5ab694014036d49c86373a143` was re-derived by
  the production builder `buildValidationTraceDisputeFaultProofContracts` on
  the measurement deployment (`hub_oracle=11…11`, `catalogue=22…22`, semantic
  resolver 1 of 76). Producing run: `pins the applied §3.2 necessity
  identities on the measurement deployment` in
  `demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts`,
  which now gates both identities instead of arguing them. Every hash pinned
  above is therefore current under `605c8b8d…` and the measurement tables
  below stay bound; the C21-AUDIT "fresh applied re-measurement owed before
  CG5" residual is discharged for these two identities.
- **Re-pinned 2026-08-15 (issue #580, Phase-7 confirm-and-publish) — the
  post-cascade identities. This bullet supersedes every pin above it; the
  earlier ones are retained as provenance on this file's own superseded-pin
  discipline.** The §3.2 hash binding fired exactly as designed across #579's
  single blueprint regeneration and #609's applied-hash cascade, and the
  movement was measured rather than inferred.

  Current blueprint: `onchain/aiken/plutus.json` SHA-256
  `91861fac2d0bcafade6d8e1b4872e669cf9e7b52f5ed75d3b9729e4e02d6dd6b`
  (md5 `b20c9a14a8fe445cdddbe5305b3857c1`, **398 validators / 702
  definitions**), built by the pinned fork **`aiken v1.1.23+2a78108`** (binary
  md5 `b3acfdf348235798cb6b921d0f87750a`) under the declared construction
  **`aiken build --env testnet`**. The env flag is load-bearing and was not
  recorded before: a default-env build of the same tree differs in
  `scheduler.spend` alone (`shift_duration` is the only env-divergent constant
  any validator compiles) and does **not** reproduce this digest. Stock
  `v1.1.22` is retired from every role (#579 owner ruling A), so every
  `v1.1.22` figure above is a void stock value rather than a second opinion.

  | bound identity | superseded | current |
  | --- | --- | --- |
  | blueprint SHA-256 | `605c8b8d…` (391 validators, stock v1.1.22) | `91861fac…` (398 validators, fork v1.1.23+2a78108) |
  | `canonical_decode_item_semantic_v1` unapplied | `62501cfe…` | `a8d8f3ac58bce62636725d394cb5953767f927518d6db6de70144b67` |
  | `canonical_decode_item_semantic_v1` applied (`hub_oracle=11…11`, `catalogue=22…22`) | `983051b4…` | `0a42b4c76739fa1a1a391c8a766fdadf58c2692b983e3c455cb5fdb6` |
  | `proof_item_v1` | `22c9a103…` | `22c9a103…` — **unmoved**, still unparameterized |

  Derived by producers on the deployment this file declares, not read off a
  diff. `demo/midgard-core/scripts/measure-validation-proof-item-envelope.mjs`
  applies `["11"×28, "22"×28, proof_item_script_hash]` to the blueprint's own
  `canonical_decode_item_semantic_v1.main.spend` and reports the applied hash;
  that three-argument application is exactly what #609's blueprint-arity guard
  now requires, because the validator declares **3** parameters and did *not*
  gain #592's `field_preimage_certificate_policy_id`. The gate named in the
  bullet above — `pins the applied §3.2 necessity identities on the measurement
  deployment` in
  `demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts` —
  re-derives both identities at runtime rather than hardcoding them, and is
  **green against this blueprint** (suite 5/5, 2026-08-15), so it gates the new
  values as it gated the old. Full record, commands and the
  provisional-versus-confirmed ledger:
  `docs/exec-plans/evidence/canonical-v1-p7-remeasurement-v1.json`.
- **Re-pinned 2026-08-16 (issue #606, the E2 certificate repair carrying the
  #608 empty-sentinel rider) — the post-#606 identities. This bullet supersedes
  every pin above it, the #580 bullet included; the earlier ones are retained as
  provenance on this file's own superseded-pin discipline.** #606 moved the
  blueprint again, so the digest pinned above stopped being a current-tree claim
  the moment that batch landed: the §8.6 repair rewrote
  `lib/midgard/native-tx-field-access-v1.ak` and
  `validators/field-preimage-certificate.ak` (constant asset name, `field_hash`
  welded into the mint-verified datum, the new door equality), and the rider
  rewrote `validators/pexcludes.ak` and re-pointed
  `env.plutarch_pexcludes_validator_hash` in both env files
  (`a9ec251d…` → `03adaadf…`) with its four embedding step validators behind
  it.

  Current blueprint: `onchain/aiken/plutus.json` SHA-256
  `f49cae224f24cfab577f1ed10b5340384b75e541851eb7b77b507a79cb7d5e00`
  (md5 `5e38d7c6ccb7987d0aca710307dcaea7`, **398 validators / 702
  definitions**), built by the same pinned fork **`aiken v1.1.23+2a78108`**
  (binary md5 `b3acfdf348235798cb6b921d0f87750a`) under the same declared
  construction **`aiken build --env testnet`** — the env flag is load-bearing
  exactly as the bullet above records — and verified byte-reproducible by a
  fresh build of this tree.

  | bound identity | superseded (#580) | current (#606) |
  | --- | --- | --- |
  | blueprint SHA-256 | `91861fac…` (md5 `b20c9a14…`, 398 validators) | `f49cae22…` (md5 `5e38d7c6…`, 398 validators / 702 definitions) |
  | `canonical_decode_item_semantic_v1` unapplied | `a8d8f3ac…` | `a8d8f3ac58bce62636725d394cb5953767f927518d6db6de70144b67` — **unmoved** |
  | `canonical_decode_item_semantic_v1` applied (`hub_oracle=11…11`, `catalogue=22…22`) | `0a42b4c7…` | `0a42b4c76739fa1a1a391c8a766fdadf58c2692b983e3c455cb5fdb6` — **unmoved** |
  | `proof_item_v1` | `22c9a103…` | `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab` — **unmoved**, still unparameterized |

  **The blueprint digest moved; this artifact's own bound identities did not.**
  That is the measured outcome, not an assumption: neither of #606's two
  repairs reaches `canonical_decode_item_semantic_v1` or `proof_item_v1`, and
  the resolver still declares **3** parameters — it did not gain
  `field_preimage_certificate_policy_id`, so the three-argument application
  #609's arity guard requires is unchanged in shape *and* in value. Re-derived
  by the same producer the bullet above names,
  `demo/midgard-core/scripts/measure-validation-proof-item-envelope.mjs`, run
  against this tree: it reports `proofItemScriptHash`
  `22c9a103…` and `semanticScriptHash` `0a42b4c7…`, reproducing both pins
  exactly. The §3.2 hash binding therefore did **not** fire for this artifact
  this pass, and every measurement table below stays bound. Only the two
  identities #606 did move — the §8.6 `FieldPreimageCertificateV1` mint policy
  (`c3682abd…` → `f030476f9cddff41d15bdaa7951a9726252c6867901992e2a5f8427e`)
  and, through it and the rewritten field-access door, `cek_v1` — are re-pinned,
  on `cek-program-material-v1.md`.
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

## Disposition of the thirteen, and the declared construction (#580, 2026-08-15)

The Phase-7 confirm-and-publish pass settles both residuals the section above
records — the three-construction basis mismatch and the thirteen
carried-forward-unverified figures. Full record:
`docs/exec-plans/evidence/canonical-v1-p7-remeasurement-v1.json`.

**One declared construction, named.** Every byte figure this artifact publishes
now comes from `demo/midgard-core/scripts/measure-validation-proof-item-envelope.mjs`
— the tree's own measurement script, byte-identical in logic since `4a4bc660`,
already the producer of the 2026-08-03 "fresh by-reference byte series" above.
Its basis is stated once and applies to every row: each transaction is signed
with one vkey witness and sources its validator from a reference input;
collection proof shape 434 items / index 433 / 9 frontier peaks / 9 siblings;
`maxTxBytes` 16,384, reliability reserve 512, `coinsPerUtxoByte` 4,310,
`minFeeCoefficient` 44, `minFeeConstant` 155,381. Re-run against the Phase-6
blueprint it reproduces the fresh series to the byte (complete-item publication
exact maximum 14,892 item bytes → datum 16,130, min-Ada 70,412,470, fee 876,277,
transaction 16,384; reliable 14,396 → 15,618 / 68,205,750 / 853,749 / 15,872;
semantic-by-reference exact maximum 14,494 → redeemer 15,954 / 16,384, reliable
13,998 → 15,442 / 15,872). **The ±368 / −616 basis mismatch is therefore closed
by declaration rather than by reconciliation**: the recorded 15,256 / 18,290 /
35,186 row was a third construction and is superseded, not adjusted.

**The thirteen are regenerated-as-superseded, never migrated.** Every one of them
prices the *counted* mechanism — the direct or reference carriage of a
bounded-collection proof item, with a 434-item collection proof, per-item leaves
and chunk trees. #560 dissolved that mechanism, so none of the thirteen names a
quantity that still exists to be re-taken at the same value, and re-publishing
any of them against a flat construction would be exactly the migration the
evidence rule forbids. What replaces each:

| carried-forward figure | superseded by |
| --- | --- |
| 15,256 / 18,290 / 35,186 (publication and proof transaction bytes) | the declared construction's boundary table above |
| 974,576 / 826,821 / 376,690 (fees) | the same table's `fee` columns |
| 65,576,650 (min-Ada at the publication cap) | 68,205,750 at 14,396 item bytes, same table |
| 16,900 (counted proof transaction at the 16,384-byte item) | 17,220, `maxLedgerOutputPublicationTransactionBytes`, measured by `complete-item-proof-fit-v1.test.ts` |
| 205,594 / 500,275,649 and 264,106 / 552,114,352 (execution pairs) | the applied direct-authentication row: 181,260 mem / 342,607,667 cpu at 8,273 reliable direct item bytes, 14,270 complete signed bytes, margin 2,114, judged at the 13,200,000 / 8,000,000,000 basis |
| the CEK one-shot pair 45,154,331 / 14,905,078,582 | **not regenerated.** It belongs to `cek-program-material-v1.md`, which #560 ruled STAND (re-pin only); the owed work there is a #546-style identity re-pin against the Phase-6 blueprint, not a re-measurement. Carried forward, flagged, and recorded as a residual on #580. |

**The §3.2 binding above has fired, and the headers are re-pinned.** Measured
against the Phase-6 blueprint (`plutus.json` md5
`b20c9a14a8fe445cdddbe5305b3857c1`, SHA-256 `91861fac…`, 398 validators, fork
`aiken v1.1.23+2a78108`, built `--env testnet`), `proof_item_v1` is unmoved at
`22c9a103…` while the applied semantic hash on this artifact's own measurement
deployment is now `0a42b4c76739fa1a1a391c8a766fdadf58c2692b983e3c455cb5fdb6`,
not the `983051b4…` bound above — the hash-binding invalidation working as
designed across the regeneration and #609's applied-hash cascade. The **Binding**
section of this file and of its four re-derived siblings carries a superseding
`Re-pinned 2026-08-15 (issue #580)` bullet with the full old→new table;
`cek-program-material-v1.md` carries the matching #546-style re-pin for the
identities its STAND verdict binds. Earlier pins are retained above rather than
rewritten, on the superseded-pin discipline this file already uses.

**Re-pinned again 2026-08-16 (issue #606).** The Phase-6 blueprint named in the
paragraph above is itself superseded: #606's §8.6 certificate repair and its
#608 empty-sentinel rider moved `plutus.json` to md5
`5e38d7c6ccb7987d0aca710307dcaea7`, SHA-256 `f49cae22…`, still 398 validators /
702 definitions under the same fork `aiken v1.1.23+2a78108`, built
`--env testnet`. This time the §3.2 binding did **not** fire for this artifact:
re-derived against the new blueprint by the same producer, `proof_item_v1` is
still `22c9a103…` and the applied semantic hash is still
`0a42b4c76739fa1a1a391c8a766fdadf58c2692b983e3c455cb5fdb6`, so the paragraph
above stays true of the current tree in everything except the blueprint digest
it names. The identities #606 did move are the §8.6 `FieldPreimageCertificateV1`
mint policy and `cek_v1`; both are re-pinned on `cek-program-material-v1.md`.
