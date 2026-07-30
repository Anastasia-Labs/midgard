# §3.2 Necessity artifact — redeemer-item ingestion and nested-data traversal

## Binding

- Family / item: `redeemer-item-traversal`
  (`TransactionRedeemerItemBeginWitness`, `RedeemerItemStepWitness` with
  `RedeemerItemProofControlV1` and `DataTraverseControlV1` actions carrying
  `chunk_proof`/`next_chunk_proof`) / one complete redeemer item including
  its nested Plutus-data payload; maximum shape bounded by the 32,768-byte
  redeemer aggregate field and unbounded nesting depth within it.
- Applied validator hashes measured: shared complete-item route
  `925662085ac87eb3cd63221b5184f59fde2c8b46d8db93052e80fc96` /
  `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab`; blueprint
  sha256 `6d23a25f8cb96f62f3e3aeeecb4e1506e8002ac712ae9bcb8873e42b4136ff1a`.
  Any change invalidates this artifact (GOAL_SPEC.md §3.2).
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
