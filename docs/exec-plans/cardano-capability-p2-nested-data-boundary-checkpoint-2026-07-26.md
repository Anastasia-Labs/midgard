# Cardano capability P2 balanced Data checkpoint — 2026-07-26

Authority:

- `cardano-capability-proof-completion.md`, P2;
- `../midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.

This checkpoint closes the balanced maximum-node-cardinality fixtures and
their bounded retained reconstruction for inline-datum and redeemer Plutus
`Data`. It does not close the whole Data family: broad constructor/list
children, broad map pairs, unary depth, and the active whole-raw challenged
redeemer scanner remain P2 gaps.

## Redeemer bridge normalization

Cardano CML's generic canonical CBOR and Aiken's `cbor.serialise(Data)` are
not the same container representation for nested Data:

- CML emits definite non-empty arrays;
- Aiken emits indefinite non-empty constructor fields and lists;
- Aiken emits definite maps while preserving the Data map's explicit pair
  order;
- Aiken chunks byte strings above 64 bytes into canonical 64-byte chunks.

Canonical Midgard V1 already requires the Aiken representation for datums and
redeemer Data. The bridge now normalizes Cardano redeemer Data to that exact
representation in the Cardano-to-Midgard direction and requires the same
representation in the Midgard-to-Cardano direction. It does not enable CBOR
tags in the generic Midgard codec or change the V1 redeemer schema.

A nested golden vector containing a map, non-empty list, small constructor,
large constructor, integer, and 65-byte chunked byte string proves that:

- CML-definite and Aiken source encodings normalize to identical Midgard
  bytes;
- purpose, index, Data, memory, and steps are exact;
- reverse Cardano construction preserves the exact Aiken Data bytes;
- the second Cardano-to-Midgard conversion is byte exact.

## Balanced maximum-cardinality shape

Both fixtures use the same deterministic balanced Data family. The fixed
outer structure contains:

- a large constructor;
- a definite one-entry map;
- a small-constructor map key;
- a balanced tree of non-empty lists;
- one byte-string leaf; and
- the remaining integer leaves.

For `N` leaves the exact Aiken Data length is `3N + 10` bytes and the semantic
node count is `2N + 2`. Increasing `N` by one adds one real scalar leaf and
one list node, not padding. This is maximum-cardinality/all-node-kind
evidence, not a proof of every Cardano-admitted Data shape: the outer
constructor has arity `1`, the map has one pair, lists are binary, and depth
is logarithmic. Separate broad and iterative unary-depth fixtures are
required before the Data-family row can pass.

### Inline datum

- accepted leaves: `5,387`;
- semantic nodes: `10,776`;
- Data CBOR: `16,171` bytes;
- signed Cardano CBOR: `16,382` bytes, margin `2`;
- adjacent leaves: `5,388`;
- adjacent Data CBOR: `16,174` bytes;
- adjacent signed Cardano CBOR: `16,385` bytes, rejected by `maxTxSize`;
- canonical Midgard transaction: `16,468` bytes;
- output item: `16,220` bytes;
- output-proof steps: `129,324`;
- semantic Data steps: `129,311`;
- maximum Data source span: `14` bytes.

The accepted transaction passes the Lucid emulator and production Midgard
consensus validation, reconstructs byte-exactly through both retained DA
classifications, traverses inside the unchanged ledger-output proof using
bounded chunk witnesses, and round-trips to exact Cardano datum semantics.
It contains neither withdrawals nor mint, has no scripts or redeemers, and
uses no collateral.

### Redeemer

- accepted leaves: `5,324`;
- semantic nodes: `10,650`;
- Data CBOR: `15,982` bytes;
- signed genuine collateralized Cardano CBOR: `16,382` bytes, margin `2`;
- adjacent leaves: `5,325`;
- adjacent Data CBOR: `15,985` bytes;
- adjacent signed Cardano CBOR: `16,385` bytes, rejected by `maxTxSize`;
- collateral-free schema-parallel Cardano CBOR: `16,293` bytes;
- canonical Midgard transaction: `16,349` bytes;
- redeemer field: `15,998` bytes in four bounded chunks;
- semantic Data steps: `127,799`;
- maximum Data source span: `14` bytes.

The genuine transaction has exactly one spend redeemer at index `1`, exact
execution units `1,601` memory and `316,149` steps, one Plutus V3 script, one
vkey witness, one collateral input, and fixed total collateral `5,000,000`.
It contains neither withdrawals nor mint and has no required-signers field.
The emulator accepts it. Production conversion still rejects it exactly with
`E_CONVERSION_UNSUPPORTED_FEATURE` and detail `collateral_inputs`.

The collateral-free parallel preserves spend inputs, outputs, fee,
script-data hash, redeemer purpose/index/Data/ex-units, and script witness.
It alone crosses the canonical Midgard bridge, reconstructs through normal
and forced retained DA, and round-trips semantically to Cardano. No collateral
field is stripped by the production bridge.

## Cross-language applied controls

TypeScript extracts and directly replays real pre/action/source/post controls
selected from the maximum balanced datum trace for HeadLargeConstructor,
HeadSequence, HeadMap, HeadScalar, FoldList, FoldMap, and FinalizeFrame. Aiken
pins and replays those same controls, rejects meaningful mutations for every
nonterminal action kind, applies the exact final-frame transitions for both
datum and redeemer, and rejects substituted terminal roots:

- maximum datum terminal: `2,170,067` memory / `871,272,899` CPU;
- maximum redeemer terminal: `2,165,655` memory / `870,105,005` CPU.
- four real head controls with per-kind mutations: `7,441,306` memory /
  `2,843,553,783` CPU;
- real FoldList, FoldMap, and internal FinalizeFrame controls with mutated
  fold proofs: `8,608,081` memory / `3,527,543,832` CPU.

## Remaining P2 boundary

The P2 evidence uses the new bounded `cek-data-traverse-v1` machine and the
retained item/chunk commitments. The active challenged-transition
redeemer-context scanner still carries whole `raw_cbor` and an obsolete
`9,215`-byte cap. Under P2's requirement to replace whole-field/list decoding
in challenged transitions with individually bounded scan/fold instructions,
that scanner is a P2 gap; it is not deferred to P3.

Before this family can pass, P2 must also prove envelope-limited constructor
and list child breadth, map-pair breadth, and an iteratively built unary chain
near `maxTxSize` through the production CML, bridge, and traversal paths
without host stack overflow. The activation gate remains fail closed.

## Focused evidence

Passing TypeScript:

- `midgard-core/tests/native-cardano-redeemer-bridge.test.ts`;
- `midgard-validation/tests/nested-data-boundary-v1.test.ts`;
- `midgard-validation/tests/nested-redeemer-data-boundary-v1.test.ts`;
- package typechecks and focused lint.

Passing Aiken:

- `aiken check -m maximum_cardano_nested` (`4/4` tests).
