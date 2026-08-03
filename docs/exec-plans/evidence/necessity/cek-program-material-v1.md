# §3.2 Necessity artifact — CEK program material and source-blob chunking

## Binding

- Family / item: `cek-program-material` (content-addressed CEK material
  nodes, `CekSourceBlobControlV1` blob chunks, incremental script-context
  construction) / one complete canonical CEK program's material graph;
  maximum shape 67,108,418 structural bytes across at most 1,597,819 nodes
  within the 64 MiB DA envelope, blob chunks of at most 4,095 bytes.
- Applied validator hashes measured (re-measured 2026-08-03): the compact
  50-byte program envelope and material commitments are bound by the deployed
  validation-machine bundle in blueprint sha256
  `277b6457197870a9df069ce5c492c166e8d0b4b32fb616294ae12404ecb070b6`
  (superseded pin, 2026-07-29:
  `6d23a25f8cb96f62f3e3aeeecb4e1506e8002ac712ae9bcb8873e42b4136ff1a`);
  publication measurements pinned against
  `MIDGARD_V1_ENVELOPE_MEASUREMENTS` (`maxProgramMaterialPublicationDatumBytes`
  4,268, `maxProgramMaterialPublicationUnsignedTransactionBytes` 4,369) by
  `demo/midgard-sdk/tests/tx-order-v1.test.ts`. Any change invalidates this
  artifact (GOAL_SPEC.md §3.2).
- Parameter snapshot digests: profile digest
  `181730d304796b764c8f657b0ae788b87c6aba9f4491dbfa9ce24d99932911b7`;
  capability floor per
  `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.
- Fixture: `deriveCekProgramMaterialPublicationsV1`
  (`demo/midgard-sdk/src/user-events/tx-order.ts`) with the pinned maximum
  blob-chunk vector in `demo/midgard-sdk/tests/tx-order-v1.test.ts`
  (deterministically regenerable).

## Measurements (§3.2 order — stop at the first representation that fits)

| Representation | Tx bytes / maxTxSize | Mem / limit·0.8 | CPU / limit·0.8 | Fee | Fits §3.3? |
| --- | --- | --- | --- | --- | --- |
| 1. Complete material direct in proof tx | 67,108,418 structural bytes against a 16,384-byte envelope — exceeds it by more than three orders of magnitude; even one 16,384-byte slice measures 18,290/16,384 as a publication | — | — | — | NO |
| 2. Complete material as one inline-datum publication + reference | same bound: no single publication transaction can carry more than the measured 14,396-byte complete-item maximum | — | — | — | NO |
| 3. Minimum multi-output publication + complete logical reconstruction | deployed: one content-addressed node per publication — measured 4,268-byte datum / 4,369-byte one-input-one-output unsigned transaction per node (pinned); at most 1,597,819 node publications for the maximum program | reconstruction cost concentrates in consumption, see 4 | — | per-node fees | bytes YES, execution NO for one-shot consumption |
| 4. Incremental node-at-a-time consumption (compact 50-byte envelope + per-step blob/data controls) | each reveal ≤ 4,369-byte publication; consuming steps bounded per node | pinned worst one-shot comparison: a single-step nine-field maximum verification measured 45,154,331 memory — over the raw 16,500,000 limit — while per-receipt steps measure ≤ 3,398,228 / 13,200,000 | one-shot 14,905,078,582 CPU over the raw 10,000,000,000 limit; per-step ≤ 1,209,745,039 / 8,000,000,000 | per pinned receipts | YES |

## Exact limiting constraint

Byte fit and execution fit are both broken by simpler representations,
measured rather than inferred: the maximum canonical material
(67,108,418 bytes) exceeds any single transaction by construction, and the
repository-pinned one-shot verification of the staged-receipt protocol's
worst case measured 45,154,331 memory / 14,905,078,582 CPU — above the raw
16,500,000 / 10,000,000,000 floors, before applying the 20% reserve
(docs/consensus-profile-v1.md §10). The reserved per-step ceilings
(13,200,000 / 8,000,000,000) hold only for node-at-a-time consumption.

## Why no simpler authenticated representation closes the gap

The material graph is orders of magnitude larger than the L1 envelope, so
representations 1 and 2 are byte-impossible; representation 3 (flat
multi-output publication) is exactly what V1 deploys for carriage, but its
one-shot logical reconstruction measurably exceeds the raw execution limits,
so consumption must also be incremental. Content addressing keeps every
node's publication independently authenticated against the canonical
program hash bound by the 50-byte envelope, so no additional commitment
scheme is introduced.

## Preserved complete-item path

Every individually bounded item in this family that fits the complete-item
envelope keeps the complete path: constants at or below the 9,215-byte
direct-constant gate travel whole, single-chunk blobs (≤4,095 bytes) are
published complete in one 4,268-byte datum, and the 50-byte program
envelope always travels complete. Duplication and root-substitution of
material publications reject
(`demo/midgard-sdk/tests/tx-order-v1.test.ts`), and the chunk commitment
equals the complete-blob commitment by construction
(`commitMidgardCekBlobV1` frontier over the same chunk hashes).

## Re-measurement 2026-08-03 (task C21-AUDIT)

Basis, blueprint provenance, and the shared by-reference byte series are
recorded once in `transaction-field-chunk-v1.md` §"Re-measurement
2026-08-03"; that section's overlay-build caveat applies to the digest pinned
above.

Re-verified unchanged for this family: `maxCekProgramMaterialBytes`
67,108,418, `maxCekProgramNodeCount` 1,597,819, `maxCekProgramEnvelopeBytes`
50, `maxTransactionFieldChunkBytes` 4,095, the 9,215-byte direct-constant
gate (`MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES_V1`),
`maxProgramMaterialPublicationDatumBytes` 4,268,
`maxProgramMaterialPublicationUnsignedTransactionBytes` 4,369, and the
consensus profile digest. Moved: the blueprint digest only.

Conclusion still supported: YES. The limiting constraint is the ratio between
the maximum material graph and the L1 envelope, and both terms are unchanged
(67,108,418 structural bytes against 16,384), as is every per-node
publication measurement the deployed representation depends on.

Carried forward unverified: the one-shot comparison pair 45,154,331 memory /
14,905,078,582 CPU and the per-step ceiling 3,398,228 / 1,209,745,039 as
observed receipts (the two envelope constants themselves are re-verified;
their derivation from a live emulator run is not). Re-running
`demo/midgard-sdk/tests/tx-order-v1.test.ts` regenerates them.
