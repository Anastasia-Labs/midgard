# §3.2 Necessity artifact — script-source hash-block traversal

## Binding

- Family / item: `script-source-hash-block` (`ScriptSourceHashBlockWitness`
  with `chunk_proof`/`next_chunk_proof` driving the staged Blake2b-256
  trace) / one complete script source revealed for hash authentication;
  maximum shape bounded by the 32,768-byte script-witness aggregate field.
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
  hash-trace semantics covered by the script-sources stage-zero Aiken
  suites.

## Measurements (§3.2 order — stop at the first representation that fits)

| Representation | Tx bytes / maxTxSize | Mem / limit·0.8 | CPU / limit·0.8 | Fee | Fits §3.3? |
| --- | --- | --- | --- | --- | --- |
| 1. Complete source direct in proof tx | fits through the measured 13,282-byte frontier (16,384/16,384); the aggregate bound admits items to 32,768 bytes → 35,186/16,384 measured publication framing | 205,594 / 13,200,000 | 500,275,649 / 8,000,000,000 | 974,576 | NO above 13,282 bytes |
| 2. Complete source as inline-datum publication + reference | fits through 14,396 bytes (15,256/16,384); 16,384 → 18,290; 32,768 → 35,186 | 264,106 / 13,200,000 | 552,114,352 / 8,000,000,000 | pub 826,821 | NO above 14,396 bytes |
| 3. Minimum multi-output publication + complete reconstruction | a flat reconstruction must still hash the complete source on-chain in one step to authenticate the script hash — the hash itself, not the bytes, is the binding step | — | — | — | superseded by 4 |
| 4. Bounded hash blocks (`Blake2b256TraceControlV1` staged compression over ≤4,095-byte chunks) | each step ≤ one chunk reveal (≤4,675-byte publication, pinned) | within pinned per-step receipts / 13,200,000 | within pinned receipts / 8,000,000,000 | per pinned receipts | YES |

## Exact limiting constraint

Two measured constraints stack. Byte fit: sources above 14,396 bytes cannot
enter one publication transaction (18,290 measured at 16,384 item bytes;
35,186 at the 32,768-byte aggregate bound). Execution fit: authenticating a
script hash requires hashing the complete source; a single-step Blake2b-256
over tens of kilobytes concentrates the entire compression cost in one
transition, while the reserved ceilings (13,200,000 memory /
8,000,000,000 CPU) must hold for every individual step. The staged
compression trace bounds each step to one 128-byte-block run over one
revealed chunk.

## Why no simpler authenticated representation closes the gap

The script hash is a strict function of the complete byte stream, so no
representation can avoid feeding every byte through the compression
function; the only degrees of freedom are where the bytes live (bounded by
the measured publication envelope) and how much compression one step
performs (bounded by the reserved execution ceilings). The chunk-driven
trace is minimal in both dimensions and reuses the same bounded-item
commitment as the complete representations.

## Preserved complete-item path

Sources at or below one chunk (≤4,095 bytes) hash in a single block
sequence with the complete bytes revealed at once, and any source at or
below 14,396 bytes retains complete-item carriage for its byte
authentication (direct at or below the measured 13,282-byte frontier,
publication + reference at or below 14,396). Chunk proofs bind the same
item commitment as the complete item
(`demo/midgard-validation/tests/complete-item-equivalence-v1.test.ts`),
and hostile omission/duplication/reorder/substitution/trailing chunks
reject in both representations.

## Re-measurement 2026-08-03 (task C21-AUDIT)

Basis, blueprint provenance, and the shared by-reference byte series are
recorded once in `transaction-field-chunk-v1.md` §"Re-measurement
2026-08-03"; that section's overlay-build caveat applies to the digests
pinned above.

Re-verified unchanged for this family: `maxTransactionAggregateFieldBytes`
32,768, `maxTransactionFieldChunkBytes` 4,095,
`maxSinglePublicationCompleteItemBytes` 14,396,
`maxFieldPublicationUnsignedTransactionBytes` 4,675, and the consensus
profile digest.

Re-measured on the fresh basis: 16,384 item bytes → 17,922/16,384; 32,768
item bytes → 34,818/16,384; one chunk (4,095 bytes) publishes in a
5,249-byte transaction, comfortably inside the envelope.

Conclusion still supported: YES. The execution half of the argument — that a
script hash is a strict function of every source byte and that one step must
therefore be bounded to one block run — is independent of every measurement
in this pass, and the byte half is re-confirmed.

Carried forward unverified: 13,282, 15,256, 18,290, 35,186, 205,594,
500,275,649, 974,576, 264,106, 552,114,352, 826,821 — see the
"not re-measurable" list in `transaction-field-chunk-v1.md`.
