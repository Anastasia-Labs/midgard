# §3.2 Necessity artifact — ledger-output preimage incremental proof

## Binding

- Family / item: `ledger-output-incremental`
  (`LedgerOutputProofBeginWitness` / `LedgerOutputProofStepWitness` with
  `LedgerOutputProofChunks`, `LedgerOutputProofDatum`,
  `LedgerOutputProofValue`, `LedgerOutputProofNativeFrame` /
  `LedgerOutputProofFinalizeWitness`) / one complete ledger output preimage;
  maximum shape 16,384 bytes (`maxLedgerOutputPreimageBytes`).
- Applied validator hashes measured:
  `925662085ac87eb3cd63221b5184f59fde2c8b46d8db93052e80fc96`
  (`canonical_decode_item_semantic_v1` applied on the measurement
  deployment), `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab`
  (`proof_item_v1`); blueprint sha256
  `6d23a25f8cb96f62f3e3aeeecb4e1506e8002ac712ae9bcb8873e42b4136ff1a`.
  Any change invalidates this artifact (GOAL_SPEC.md §3.2).
- Parameter snapshot digests: consensus profile digest
  `181730d304796b764c8f657b0ae788b87c6aba9f4491dbfa9ce24d99932911b7`;
  capability floor per
  `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.
- Fixture: exact-size canonical output items generated in
  `demo/midgard-validation/tests/complete-item-proof-fit-v1.test.ts`
  (deterministically regenerable).

## Measurements (§3.2 order — stop at the first representation that fits)

| Representation | Tx bytes / maxTxSize | Mem / limit·0.8 | CPU / limit·0.8 | Fee | Fits §3.3? |
| --- | --- | --- | --- | --- | --- |
| 1. Complete output direct in proof tx | 16,384-byte output: `Verify` redeemer alone ≥ 16,900 bytes framed — exceeds 16,384 before framing; measured direct frontier for any complete item is 13,282 bytes (16,384/16,384, margin 0) | 205,594 / 13,200,000 | 500,275,649 / 8,000,000,000 | 974,576 | NO above 13,282 bytes |
| 2. Complete output as inline-datum publication + reference consumption | 16,384-byte output publication measures 18,290/16,384 — over by 1,906; publication fits only through 14,396 bytes (15,256/16,384) | consuming tx 264,106 / 13,200,000 | 552,114,352 / 8,000,000,000 | pub 826,821 | NO above 14,396 bytes |
| 3. Minimum multi-output publication + complete logical reconstruction | not deployed; the incremental route below consumes the same ≤4,095-byte bounded chunks and additionally interleaves Value, datum-traversal, and native-frame sub-proofs that a flat reconstruction would still need | — | — | — | superseded by 4 |
| 4. Incremental begin/step/finalize traversal (chunks + datum actions + native frames + Value proofs) | each step ≤ one bounded chunk reveal (≤4,675-byte publication, pinned) | within pinned per-step receipts (3,398,228 max observed field-chunk receipt) / 13,200,000 | 1,209,745,039 / 8,000,000,000 | per pinned receipts | YES |

## Exact limiting constraint

`maxTxSize = 16,384` on the complete serialized publication or proof
transaction: a maximum 16,384-byte ledger output equals the whole L1
envelope by itself, and its measured complete signed publication overshoots
by 1,906 bytes. The consensus profile therefore retains ledger outputs
"authenticated incrementally" (docs/consensus-profile-v1.md §10), and the
resolve-inputs membership route must be able to traverse output preimages,
inline datums, embedded native scripts, and Values chunk by chunk.

## Why no simpler authenticated representation closes the gap

The complete output cannot enter one transaction above 14,396 bytes
(measured, not inferred from item length). A multi-output flat publication
still requires the datum, Value, and native-script sub-structures to be
verified against the output commitment, which is exactly what the
begin/step/finalize route does while consuming the same bounded chunks; a
flat variant would duplicate the chunk machinery without removing any step.

## Preserved complete-item path

Every output at or below `maxSinglePublicationCompleteItemBytes` (14,396)
keeps its complete-item carriage: the canonical-decode producer emits
`TransactionFieldItemWitness` for it (guard pinned by
`complete-item-carriage-policy-v1.test.ts`), and the scriptSources output
fold always authenticates the complete output item. Both representations
bind the same bounded-item commitment; equivalence and
omission/duplication/reorder/substitution/trailing rejection are exercised
by `demo/midgard-validation/tests/complete-item-equivalence-v1.test.ts` and
the deployed-route rejection tests in
`demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts`.

Measured residual gap recorded for the ledger (not a fallback
justification): the scriptSources stage-4 fold carries the complete output
item with no bounded fallback, and its one-step evidence crosses the
16,384-byte envelope at a measured 14,774-byte output
(`largestProvableCompleteOutputBytes`, pinned by
`complete-item-proof-fit-v1.test.ts`). Outputs in (14,774, 16,384] are legal
canonical content whose stage-4 one-step argument cannot currently be
constructed; closing that gap requires either a bounded fallback with its
own §3.2 artifact on that route or a reference-carried item variant of the
stage-4 semantic validator.
