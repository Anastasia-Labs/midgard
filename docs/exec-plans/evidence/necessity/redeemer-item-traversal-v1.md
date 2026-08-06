# §3.2 Necessity artifact — redeemer-item ingestion and nested-data traversal

## Binding

- Family / item: `redeemer-item-traversal`
  (`TransactionRedeemerItemBeginWitness`, `RedeemerItemStepWitness` with
  `RedeemerItemProofControlV1` and `DataTraverseControlV1` actions carrying
  `chunk_proof`/`next_chunk_proof`) / one complete redeemer item including
  its nested Plutus-data payload; maximum shape bounded by the 32,768-byte
  redeemer aggregate field and unbounded nesting depth within it.
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
  nested-data boundary behavior in
  `demo/midgard-validation/tests/nested-redeemer-data-boundary-v1.test.ts`.

## Measurements (§3.2 order — stop at the first representation that fits)

| Representation | Tx bytes / maxTxSize | Mem / limit·0.8 | CPU / limit·0.8 | Fee | Fits §3.3? |
| --- | --- | --- | --- | --- | --- |
| 1. Complete redeemer item direct in proof tx | fits through the measured 13,282-byte frontier (16,384/16,384); redeemer items may legally approach the 32,768-byte aggregate bound → 35,186/16,384 measured publication framing | 205,594 / 13,200,000 | 500,275,649 / 8,000,000,000 | 974,576 | NO above 13,282 bytes |
| 2. Complete item as inline-datum publication + reference | fits through 14,396 bytes (15,256/16,384); 16,384 → 18,290; 32,768 → 35,186 | 264,106 / 13,200,000 | 552,114,352 / 8,000,000,000 | pub 826,821 | NO above 14,396 bytes |
| 3. Minimum multi-output publication + complete reconstruction | a flat reconstruction still has to fold the nested Plutus-data tree into typed summaries (roots, lengths, memory) to bind the CEK context — the traversal, not the bytes, is the binding step | — | — | — | superseded by 4 |
| 4. Staged item ingestion + `DataTraverse` actions over ≤4,095-byte chunks | each step ≤ one chunk reveal (≤4,675-byte publication, pinned) | within pinned per-step receipts / 13,200,000 | within pinned receipts / 8,000,000,000 | per pinned receipts | YES |

## Exact limiting constraint

Byte fit: redeemer items above 14,396 bytes cannot enter one publication
transaction (measured 18,290 at 16,384 item bytes, 35,186 at 32,768).
Execution fit: the CEK context requires typed data summaries (node roots,
CBOR lengths, memory) for the complete nested payload; folding an
arbitrarily deep 32,768-byte data tree in one transition cannot be bounded
under the reserved 13,200,000-memory / 8,000,000,000-CPU ceilings for the
worst legal shape, whereas one `DataTraverse` action per step pins each
transition to one node with its measured cost.

## Why no simpler authenticated representation closes the gap

The complete payload is not merely carried — it must be structurally folded
into the exact summary tree the deployed CEK route consumes. Any
representation that skips the per-node fold either re-derives it in one
unboundable step or trusts an unauthenticated summary. The staged traversal
reuses the same bounded-item chunk commitment as the complete
representations and adds only the frame/control state needed to bound one
step to one node.

## Preserved complete-item path

Redeemer items at or below 14,396 bytes keep complete-item byte carriage
(direct at or below the measured 13,282-byte frontier; publication +
reference at or below 14,396), and small payloads traverse in a single
begin/step pair whose chunk carries the complete item bytes. Chunked and
complete representations bind the identical item commitment
(`demo/midgard-validation/tests/complete-item-equivalence-v1.test.ts`),
with omission/duplication/reorder/substitution/trailing rejection proven
for both.

## Re-measurement 2026-08-03 (task C21-AUDIT)

Basis, blueprint provenance, and the shared by-reference byte series are
recorded once in `transaction-field-chunk-v1.md` §"Re-measurement
2026-08-03"; that section's overlay-build caveat applies to the digests
pinned above.

Re-verified unchanged for this family: `maxTransactionAggregateFieldBytes`
32,768, `maxTransactionFieldChunkBytes` 4,095,
`maxSinglePublicationCompleteItemBytes` 14,396,
`maxFieldPublicationUnsignedTransactionBytes` 4,675, the per-step receipt
ceilings 3,398,228 / 1,209,745,039, and the consensus profile digest.

Re-measured on the fresh basis: 16,384 item bytes → 17,922/16,384; 32,768
item bytes → 34,818/16,384.

Conclusion still supported: YES. The unbounded-nesting argument that forces
one `DataTraverse` action per node is independent of every measurement in this
pass, and byte-impossibility above the publication frontier is re-confirmed.

Scope boundary (unchanged, restated 2026-08-03). This artifact covers the
DATA-level staging only: `TransactionRedeemerItemBeginWitness` /
`RedeemerItemStepWitness` with `RedeemerItemProofControlV1` and
`DataTraverseControlV1`. It does NOT cover the script-level stage-one-redeemer
decomposition — the
`script_sources_stage_one_redeemer_{outer_normalizer,traversal_normalizer,fold_map_executor,finalize_frame_executor}_v1`
validator family present in the blueprint pinned above. That split is a
distinct decomposition of a distinct item and needs its own §3.2 necessity
artifact; it is blocked on the owner security ruling RF-021 and is not
extended into this artifact's scope. Writing that artifact is open future
work, not part of this re-measurement.

Carried forward unverified: 13,282, 15,256, 18,290, 35,186, 205,594,
500,275,649, 974,576, 264,106, 552,114,352, 826,821 — see the
"not re-measurable" list in `transaction-field-chunk-v1.md`.
