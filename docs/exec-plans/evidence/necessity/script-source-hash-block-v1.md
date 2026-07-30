# §3.2 Necessity artifact — script-source hash-block traversal

## Binding

- Family / item: `script-source-hash-block` (`ScriptSourceHashBlockWitness`
  with `chunk_proof`/`next_chunk_proof` driving the staged Blake2b-256
  trace) / one complete script source revealed for hash authentication;
  maximum shape bounded by the 32,768-byte script-witness aggregate field.
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
