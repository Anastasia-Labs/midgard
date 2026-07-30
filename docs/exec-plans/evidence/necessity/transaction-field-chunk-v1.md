# §3.2 Necessity artifact — transaction general-field items above the single-publication maximum

## Binding

- Family / item: `transaction-field-chunk` (`TransactionFieldChunkWitness`) /
  one canonical bounded-field item of any of the nine V1 general fields;
  maximum shapes measured: 16,384-byte ledger-output item and a
  32,768-byte aggregate-field item.
- Applied validator hashes measured:
  `925662085ac87eb3cd63221b5184f59fde2c8b46d8db93052e80fc96`
  (`canonical_decode_item_semantic_v1` applied on the measurement deployment
  with `hub_oracle=11…11`, `catalogue=22…22`),
  `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab`
  (`proof_item_v1`, unparameterized); unapplied blueprint hash
  `547cc8b7a136515c85cf51a8a3a32ecae63fb8859cf63ef5f2daa893`;
  blueprint `onchain/aiken/plutus.json` sha256
  `6d23a25f8cb96f62f3e3aeeecb4e1506e8002ac712ae9bcb8873e42b4136ff1a`.
  Any change invalidates this artifact; re-measure before CG5
  (GOAL_SPEC.md §3.2).
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
