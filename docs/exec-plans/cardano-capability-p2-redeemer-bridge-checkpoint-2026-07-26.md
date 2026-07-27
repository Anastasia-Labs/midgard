# Cardano capability P2 canonical redeemer bridge checkpoint — 2026-07-26

## Scope and authority

This checkpoint closes the canonical redeemer representation mismatch at the
Cardano/Midgard bridge and proves one genuine Plutus redeemer through the
field-8 ordered-collection path. It is the first hard checkpoint toward P2,
not a whole-P2 or activation claim.

The governing sources remain:

- `cardano-capability-proof-completion.md`, P2;
- `../midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`;
- the unchanged fail-closed canonical V1 schema and release gate.

P3–P6, live deployment, Docker, release evidence, compatibility, profile-limit
changes, and unrelated cleanup are outside this checkpoint.

## Representation boundary

Cardano CML redeemers encode the Plutus Data item inline. The genuine unit
fixture therefore contains the constructor tag bytes `d87980` directly in its
Cardano redeemer:

```text
81840001d87980821906411a0004d2f5
```

Canonical Midgard and the Aiken `MidgardRedeemerWitness` codec encode a list of
`[purpose, index, redeemer_data_cbor_as_definite_bytes, [memory, steps]]`.
The same semantic witness is therefore:

```text
8184000143d87980821906411a0004d2f5
```

The prior Cardano-to-Midgard bridge copied
`Redeemers.to_cbor_bytes()` directly. That passed Cardano's inline Data tag to
the generic Midgard CBOR scanner, which correctly rejects CBOR tags. The
reverse bridge likewise tried to parse the Midgard bytes-wrapped list directly
as CML `Redeemers`.

The bridge now performs explicit semantic normalization in both directions:

- retain supported purpose/tag, index, canonical Plutus Data CBOR, memory, and
  steps;
- sort deterministically by numeric purpose/tag and index;
- encode the Midgard Data CBOR as definite bytes;
- reconstruct a canonical Cardano redeemer map;
- reject duplicate pointers, unsupported purposes, malformed/noncanonical
  Data, and negative fields;
- leave the generic Midgard CBOR codec unchanged.

## Genuine N=1 dual fixture

The fixture first builds and successfully submits a genuine emulator-evaluated
Plutus V3 spending transaction. That Cardano transaction has:

| Measurement | Result |
| --- | ---: |
| Signed bytes | 431 |
| Margin to 16,384 bytes | 15,953 |
| Spend inputs | 2 |
| Outputs | 2 |
| Vkey witnesses | 1 |
| Plutus V3 scripts | 1 |
| Redeemers | 1 |
| Redeemer pointer | spend index 1 |
| Redeemer Data | `d87980` |
| Execution memory | 1,601 |
| Execution steps | 316,149 |
| Fee | 174,637 lovelace |

The genuine Cardano transaction requires one collateral input, a collateral
return, and total collateral of 5,000,000 lovelace. Production conversion
continues to reject it exactly as before:

```text
message = Cardano tx cannot be converted to Midgard native format without dropping fields
code    = E_CONVERSION_UNSUPPORTED_FEATURE
detail  = collateral_inputs
```

The fixture then constructs a collateral-free parallel transaction solely to
exercise the unchanged Midgard schema. It retains the same spend inputs,
outputs, fee, validity interval, withdrawal contents, required signer hashes,
mint policy/asset quantities, hashes, network, scripts, and exact Cardano
redeemer CBOR. The present N=1 fixture explicitly has no withdrawals and no
mint. Its shared-field comparator nevertheless compares complete
reward-account/amount entries and complete policy/asset/quantity entries when
those fields are populated; it does not reduce them to counts.

The collateral-free transaction is 346 signed bytes. Its normalized field-8
evidence is:

| Measurement | Result |
| --- | ---: |
| Normalized field bytes | 17 |
| Items | 1 |
| Item reveal steps | 1 |
| Maximum item chunk | 16 bytes |
| Maximum reveal | 157 bytes |
| Complete nine-field fold steps | 7 |

The normalized Midgard witness reconstructs a canonical Cardano redeemer map
with the same purpose, index, `d87980` Data, memory, and steps.

## Cross-language golden vector

TypeScript and Aiken both pin this two-witness canonical Midgard preimage:

```text
8284000043d8798082050784030142182a820b0d
```

It represents:

1. spend index 0, Data `d87980`, memory 5, steps 7;
2. reward index 1, Data `182a`, memory 11, steps 13.

The TypeScript bridge proves that Cardano legacy-array and map containers
normalize to the same bytes, reconstructs the canonical Cardano map, and
rejects duplicate/unsupported pointers. The Aiken native transaction codec
decodes and re-encodes the identical bytes.

## Verification

```sh
cd demo/midgard-core
pnpm typecheck
pnpm exec eslint \
  src/codec/native-redeemer.ts \
  src/codec/native-cardano-conversion.ts \
  src/codec/native.ts \
  tests/native-cardano-redeemer-bridge.test.ts \
  --max-warnings=0
pnpm test
```

Result: **PASS** (`35` files, `245` tests).

```sh
cd demo/midgard-validation
pnpm typecheck
pnpm exec eslint \
  tests/helpers/ordered-collection-boundary-v1.ts \
  tests/redeemer-collateral-schema-feasibility-v1.test.ts \
  --max-warnings=0
pnpm exec vitest run \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  tests/ordered-collection-reference-inputs-boundary-v1.test.ts \
  tests/ordered-collection-observer-native-script-boundary-v1.test.ts \
  tests/ordered-collection-mint-boundary-v1.test.ts \
  tests/redeemer-collateral-schema-feasibility-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (`7` files, `7` tests).

```sh
cd onchain/aiken
aiken fmt --check lib/midgard/fraud-proofs/native-tx.test.ak
aiken check -m canonical_redeemer_bridge_golden_vector_is_stable
```

Result: **PASS** (`1` focused unit test; memory `164,089`, CPU `65,959,547`).

`git diff --check` is clean.

## Remaining P2 gate

This checkpoint proves semantic bridge normalization and one real field-8
item. It does not establish the maximum Cardano-capable redeemer cardinality
or Data shape.

Before P2 can pass, the remaining work must:

1. derive and exercise maximum applicable redeemer cardinality and Data
   content from Cardano transaction-size and execution limits, with adjacent
   rejection evidence;
2. reconstruct maximum-shape normal and forced transactions from retained DA;
3. reveal every dynamic family through individually bounded typed item/chunk
   steps;
4. prove TypeScript/Aiken terminal-fold agreement for those maximum shapes;
5. retain the fail-closed activation gate until every P2 family passes.

No cardinality search was started in this checkpoint.
