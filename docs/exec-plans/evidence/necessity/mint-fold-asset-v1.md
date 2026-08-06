# §3.2 Necessity artifact — mint-field asset fold

## Binding

- Family / item: `mint-fold-asset` (`MintFoldAssetWitness` with
  `chunk_proof`/`next_chunk_proof`) / the complete canonical mint field
  folded asset by asset; maximum shape the 32,768-byte mint aggregate field
  with up to 16,384 distinct assets.
- Applied validator hashes measured (re-measured 2026-08-03): shared
  complete-item route
  `983051b4a0c3fe90057a599e77ed44c5ab694014036d49c86373a143` /
  `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab` (the second
  unchanged); blueprint sha256
  `277b6457197870a9df069ce5c492c166e8d0b4b32fb616294ae12404ecb070b6`.
  Any change invalidates this artifact (GOAL_SPEC.md §3.2). Superseded pin
  (2026-07-29): `925662085ac87eb3cd63221b5184f59fde2c8b46d8db93052e80fc96` /
  blueprint `6d23a25f8cb96f62f3e3aeeecb4e1506e8002ac712ae9bcb8873e42b4136ff1a`.
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
- Parameter snapshot digests: profile digest
  `181730d304796b764c8f657b0ae788b87c6aba9f4491dbfa9ce24d99932911b7`;
  capability floor per
  `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.
- Fixture: shared exact-size generators in
  `demo/midgard-validation/tests/complete-item-proof-fit-v1.test.ts`;
  mint boundary corpus in
  `demo/midgard-validation/tests/ordered-collection-mint-boundary-v1.test.ts`.

## Measurements (§3.2 order — stop at the first representation that fits)

| Representation | Tx bytes / maxTxSize | Mem / limit·0.8 | CPU / limit·0.8 | Fee | Fits §3.3? |
| --- | --- | --- | --- | --- | --- |
| 1. Complete mint field direct in proof tx | the 32,768-byte aggregate exceeds the envelope outright; measured publication framing at 32,768 bytes: 35,186/16,384 (over by 18,802); fits only through the 13,282-byte measured frontier | 205,594 / 13,200,000 | 500,275,649 / 8,000,000,000 | 974,576 | NO above 13,282 bytes |
| 2. Complete field as inline-datum publication + reference | fits through 14,396 bytes (15,256/16,384); 16,384 → 18,290; 32,768 → 35,186 | 264,106 / 13,200,000 | 552,114,352 / 8,000,000,000 | pub 826,821 | NO above 14,396 bytes |
| 3. Minimum multi-output publication + complete reconstruction | value semantics still require per-asset conservation deltas against the ledger `Value` commitments — the per-asset fold, not the bytes, is the binding step | — | — | — | superseded by 4 |
| 4. Asset-by-asset fold over ≤4,095-byte chunks (`MintFoldAssetWitness`) | each step ≤ one chunk reveal (≤4,675-byte publication, pinned) | within pinned per-step receipts / 13,200,000 | within pinned receipts / 8,000,000,000 | per pinned receipts | YES |

## Exact limiting constraint

`maxTxSize = 16,384` on the complete serialized transaction: the mint
aggregate field is reserved to 32,768 bytes (measured single-publication
framing 35,186 bytes, over by 18,802), and the distinct-asset guardrail
admits up to 16,384 assets whose per-asset conservation mutations
(`ValueAssetMutationWitnessV1` MPF delta proofs) each carry their own
sibling paths. One transition per asset with one bounded chunk is the
largest step shape that stays inside both the byte envelope and the
reserved execution ceilings for the worst legal field.

## Why no simpler authenticated representation closes the gap

Mint verification is not byte transport: every asset triple must be checked
against the accumulated `Value` delta commitment. A complete-field
representation above the measured publication maximum cannot enter one
transaction, and even below it a one-shot fold across 16,384 assets
concentrates 16,384 MPF mutations in one step. The deployed fold reuses the
same bounded chunk commitment for the field bytes and adds only the
per-asset cursor.

## Preserved complete-item path

Mint fields at or below 14,396 bytes retain complete-item carriage for
byte authentication (direct at or below the measured 13,282-byte frontier;
publication + reference at or below 14,396); small fields fold in a single
chunk whose bytes are the complete field. Chunked and complete
representations bind the identical bounded-item commitment with hostile
omission/duplication/reorder/substitution/trailing rejection proven at
`demo/midgard-validation/tests/complete-item-equivalence-v1.test.ts`.

## Re-measurement 2026-08-03 (task C21-AUDIT)

Basis, blueprint provenance, and the shared by-reference byte series are
recorded once in `transaction-field-chunk-v1.md` §"Re-measurement
2026-08-03"; that section's overlay-build caveat applies to the digests
pinned above.

Re-verified unchanged for this family: `maxMintPreimageBytes` 32,768,
`maxDistinctAssetCount` 16,384, `maxTransactionFieldChunkBytes` 4,095,
`maxSinglePublicationCompleteItemBytes` 14,396,
`maxFieldPublicationUnsignedTransactionBytes` 4,675, and the consensus
profile digest. Both bounds that make this fold necessary — the 32,768-byte
mint aggregate and the 16,384-asset guardrail — are byte-identical to the
2026-07-29 pin.

Re-measured on the fresh basis: the 32,768-byte aggregate's single signed
publication measures 34,818/16,384, over by 18,434 rather than the recorded
18,802 — the same uniform 368-byte difference the other artifacts show (see
the basis-mismatch note in `transaction-field-chunk-v1.md`).

Conclusion still supported: YES. The aggregate remains more than twice the
envelope, and the per-asset conservation argument — which is what actually
forces the fold — is unaffected by any measurement in this pass.

Carried forward unverified: 13,282, 15,256, 18,290, 35,186, 205,594,
500,275,649, 974,576, 264,106, 552,114,352, 826,821 — see the
"not re-measurable" list in `transaction-field-chunk-v1.md`.
