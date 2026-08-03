# §3.2 Necessity artifact — native-script token and frame traversal

## Binding

- Family / item: `native-script-traversal` (`NativeScriptTokenWitness` with
  `chunk_proof`/`next_chunk_proof`, `NativeScriptFrameWitness`) / one
  complete Cardano native script from the script-witness field or an output;
  maximum shape bounded by the 32,768-byte script-witness aggregate field
  and the 16,384-node/depth guardrails (transaction bytes effective).
- Applied validator hashes measured (re-measured 2026-08-03): shared
  complete-item route
  `983051b4a0c3fe90057a599e77ed44c5ab694014036d49c86373a143` /
  `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab` (the second
  unchanged); blueprint sha256
  `277b6457197870a9df069ce5c492c166e8d0b4b32fb616294ae12404ecb070b6`.
  Any change invalidates this artifact (GOAL_SPEC.md §3.2). Superseded pin
  (2026-07-29): `925662085ac87eb3cd63221b5184f59fde2c8b46d8db93052e80fc96` /
  blueprint `6d23a25f8cb96f62f3e3aeeecb4e1506e8002ac712ae9bcb8873e42b4136ff1a`.
- Parameter snapshot digests: profile digest
  `181730d304796b764c8f657b0ae788b87c6aba9f4491dbfa9ce24d99932911b7`;
  capability floor per
  `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.
- Fixture: shared exact-size item generators in
  `demo/midgard-validation/tests/complete-item-proof-fit-v1.test.ts`;
  native evaluation semantics covered by the phase-A Aiken suites.

## Measurements (§3.2 order — stop at the first representation that fits)

A native script is carried as one bounded field item, so its byte fit is the
transaction-field measurement applied to this family's maxima.

| Representation | Tx bytes / maxTxSize | Mem / limit·0.8 | CPU / limit·0.8 | Fee | Fits §3.3? |
| --- | --- | --- | --- | --- | --- |
| 1. Complete script direct in proof tx | fits through the measured 13,282-byte frontier (16,384/16,384, margin 0); a script-witness item may legally reach the 32,768-byte aggregate bound — 32,768-byte publication framing measures 35,186/16,384 | 205,594 / 13,200,000 | 500,275,649 / 8,000,000,000 | 974,576 | NO above 13,282 bytes |
| 2. Complete script as inline-datum publication + reference | fits through 14,396 bytes (15,256/16,384); 16,384 bytes → 18,290 (over by 1,906); 32,768 bytes → 35,186 (over by 18,802) | 264,106 / 13,200,000 | 552,114,352 / 8,000,000,000 | pub 826,821 | NO above 14,396 bytes |
| 3. Minimum multi-output publication + complete reconstruction | not deployed; the token/frame walk below consumes the same bounded chunks while simultaneously evaluating timelock/signature/threshold semantics that flat reconstruction would re-traverse | — | — | — | superseded by 4 |
| 4. Token scan (`chunk_proof`/`next_chunk_proof`) plus stateful frames | each step reveals at most one ≤4,095-byte chunk (≤4,675-byte publication, pinned) | within pinned per-step receipts / 13,200,000 | within pinned receipts / 8,000,000,000 | per pinned receipts | YES |

## Exact limiting constraint

`maxTxSize = 16,384` on the complete serialized transaction: a native
script item above 14,396 bytes cannot enter one publication transaction
(measured 18,290 bytes at 16,384 item bytes, 35,186 at 32,768), yet the
canonical guardrails admit native scripts whose encoding is bounded only by
the aggregate field. Additionally, native evaluation is inherently
recursive: frames bound the on-chain stack so one dispute step never
re-parses an unbounded subtree, keeping every step inside the reserved
execution ceilings.

## Why no simpler authenticated representation closes the gap

Above the publication maximum the bytes cannot fit one transaction at all
(measured, representation 1 and 2). For evaluation, a complete-script
one-shot verifier would re-traverse up to 16,384 nodes in a single step;
the frame representation is the minimum stateful addition that pins each
step to one node family while chunks stay bound to the same item
commitment used by the complete representations.

## Preserved complete-item path

Native scripts whose items fit 14,396 bytes retain complete-item carriage
through the canonical-decode complete-item route (direct at or below the
measured 13,282-byte frontier, publication + reference at or below 14,396),
and single-chunk tokens (`chunk_proof` with chunk 0 of a ≤4,095-byte item)
carry the complete script bytes in one reveal. Chunk proofs and the
complete item bind the identical bounded-item commitment
(`demo/midgard-validation/tests/complete-item-equivalence-v1.test.ts`
proves commitment equality and omission/duplication/reorder/substitution/
trailing rejection for both representations).

## Re-measurement 2026-08-03 (task C21-AUDIT)

Basis, blueprint provenance, and the shared by-reference byte series are
recorded once in `transaction-field-chunk-v1.md` §"Re-measurement
2026-08-03"; that section's overlay-build caveat applies to the digests
pinned above.

Re-verified unchanged for this family: `maxTransactionAggregateFieldBytes`
32,768, `maxNativeScriptNodeCount` 16,384, `maxNativeScriptDepth` 16,384,
`maxTransactionFieldChunkBytes` 4,095,
`maxSinglePublicationCompleteItemBytes` 14,396,
`maxFieldPublicationUnsignedTransactionBytes` 4,675, and the consensus
profile digest. Both guardrails the frame representation is sized against —
16,384 nodes and 16,384 depth — are byte-identical to the 2026-07-29 pin.

Re-measured on the fresh basis: 16,384 item bytes → 17,922/16,384 (over by
1,538, recorded 1,906); 32,768 item bytes → 34,818/16,384 (over by 18,434,
recorded 18,802). Both differ from the recorded values by a uniform 368
bytes (see the basis-mismatch note in `transaction-field-chunk-v1.md`);
direction and magnitude class are unchanged.

Conclusion still supported: YES. Byte-impossibility above the publication
frontier is re-confirmed, and the recursion argument that forces frames
depends only on the node/depth guardrails, which did not move.

Carried forward unverified: 13,282, 15,256, 18,290, 35,186, 205,594,
500,275,649, 974,576, 264,106, 552,114,352, 826,821 — see the
"not re-measurable" list in `transaction-field-chunk-v1.md`.
