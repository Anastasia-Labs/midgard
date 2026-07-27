# Cardano capability P2 redeemer boundary checkpoint — 2026-07-26

## Scope

This checkpoint derives the canonical V1 field-8 (`redeemer_tx_wits`)
cardinality boundary from exact, fully signed Cardano transaction bytes under
the preserved preprod epoch-303 parameter snapshot. It extends the genuine
N=1 dual fixture committed in `6b720047`; it does not change canonical V1,
Cardano ingress, consensus limits, or the generic tag-rejecting Midgard CBOR
decoder.

The governing source remains
`cardano-capability-proof-completion.md`, P2. This is one ordered-field
checkpoint, not a whole-P2 or release claim.

## Exact boundary

The fixture uses one real key funding input, one distinct key collateral
input, and real genesis Plutus V3 script UTxOs with inline unit datums. A
genuine locally evaluated N=1 transaction supplies the script, canonical
redeemer Data, and measured execution units. Each larger transaction is
fee-stabilized, signed, and serialized with CML.

| Measurement | Accepted | Adjacent |
| --- | ---: | ---: |
| Spend redeemers | 296 | 297 |
| Spend inputs, including the key funding input | 297 | 298 |
| Plutus V3 script witnesses | 1 | 1 |
| Vkey witnesses | 1 | 1 |
| Signed Cardano bytes | 16,377 | 16,433 |
| Margin to `maxTxSize = 16,384` | +7 | -49 |
| Stabilized fee | 910,060 lovelace | 912,640 lovelace |
| Execution memory | 473,896 | 475,497 |
| Memory margin to 16,500,000 | 16,026,104 | 16,024,503 |
| Execution steps | 93,580,104 | 93,896,253 |
| Step margin to 10,000,000,000 | 9,906,419,896 | 9,906,103,747 |

One redeemer uses 1,601 memory units and 316,149 steps. The corresponding
execution-only ceilings are 10,306 redeemers by memory and 31,630 by steps,
so the adjacent transaction remains comfortably within both execution
ceilings. The preserved Cardano `maxTxSize` is the binding constraint.

The accepted collateralized transaction is submitted to the same bounded
emulator that owns its inputs and is accepted. Production Cardano-to-Midgard
conversion continues to reject that complete transaction without dropping
collateral:

```text
message = Cardano tx cannot be converted to Midgard native format without dropping fields
code    = E_CONVERSION_UNSUPPORTED_FEATURE
detail  = collateral_inputs
```

This spend-only fixture has neither withdrawals nor mint. Its populated
semantics are checked exactly: all 296 spend pointers are the ordered sequence
1 through 296, all purposes are spend, every Data item is canonical `d87980`,
and every execution-unit pair is `[1601, 316149]`.

The test constructs the explicit collateral-free common-schema parallel
transaction introduced by `6b720047`. It retains the shared spend inputs,
output, fee, script-data hash, Plutus V3 script, exact redeemer preimages and
execution units, and regenerates the consumed key witness for the changed
body hash. Its exact field-8 evidence is:

| Measurement | Result |
| --- | ---: |
| Canonical Midgard transaction bytes | 16,933 |
| Field preimage bytes | 5,053 |
| Field preimage Blake2b-256 | `680079f9aebb6ab20240bf0a4b46a9b607181843413e0cdfbb293942aebe3d0a` |
| Field collection commitment | `07da3c8aea4dd252510b18f872268ea7b7d752fe9d6874f3321286ec6d8c4133` |
| Committed items | 296 |
| Item reveal steps | 296 |
| Maximum item chunk | 18 bytes |
| Maximum reveal | 514 bytes |
| Complete nine-field fold steps | 596 |

Every item is revealed and the exact terminal fold completes. Converting the
parallel transaction through the production Cardano-to-Midgard bridge and
back reconstructs the accepted transaction's redeemer purposes, indexes,
canonical Data, memory, and steps exactly.

The focused Aiken maximum vector independently constructs the same 296
ordered witnesses, encodes and decodes their canonical preimage, and reaches
both hashes above through the Aiken codec and
`bounded_collection_v1.from_items`. It checks exact pointers `1..296`,
purpose, inline `d87980` Data bytes, and per-item execution units.

A second Aiken vector verifies the genuine fixture's final field-8 item/chunk
proof. Item 295 is pointer 296; the terminal fold advances from canonical
decode at encoded length 5,035 to compact binding at the exact committed
length 5,053. TypeScript and Aiken agree on pre/post work roots
`759a815cdb2475e891089fa279d80fbe44ce6fe5e4552183e144a5ca85a602ed`
and
`79855e6bc07c2c112afdee7d7a1255d3f451bf94c4b025b6ec289bf21a5df1f2`.
This binds the TypeScript terminal field commitment and state transition to
the Aiken implementation without using a whole-field witness in the
production path. The aggregate Aiken unit test is diagnostic; the applicable
production shape remains 296 individually bounded reveal steps.

## Verification

```sh
cd demo/midgard-validation
NODE_OPTIONS=--max-old-space-size=2048 \
MIDGARD_PRINT_PROOF_FIT=1 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  tests/ordered-collection-reference-inputs-boundary-v1.test.ts \
  tests/ordered-collection-observer-native-script-boundary-v1.test.ts \
  tests/ordered-collection-mint-boundary-v1.test.ts \
  tests/redeemer-collateral-schema-feasibility-v1.test.ts \
  tests/ordered-collection-redeemer-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (`8` files, `8` tests, one fork, 17.02 seconds).

```sh
cd demo/midgard-validation
NODE_OPTIONS=--max-old-space-size=2048 \
  ./node_modules/.bin/tsc --noEmit
NODE_OPTIONS=--max-old-space-size=2048 \
  ../node_modules/.bin/eslint \
  tests/helpers/ordered-collection-boundary-v1.ts \
  tests/ordered-collection-redeemer-boundary-v1.test.ts \
  --max-warnings=0
```

Result: **PASS**.

```sh
cd onchain/aiken
aiken fmt --check \
  lib/midgard/fraud-proofs/native-tx.max-redeemers.test.ak
aiken check -m maximum_cardano_spend_redeemer
```

Result: **PASS** (`2` focused unit tests). The canonical field vector used
`124,272,878` memory and `50,949,000,952` CPU in aggregate Aiken fixture
evaluation; the bounded final transition used `2,068,700` memory and
`856,755,810` CPU. These are test evaluation totals, not a release
publication-size or live-transaction budget claim.

The checkpoint scope contains exactly the shared ordered-collection helper,
one field-8 boundary test, one focused Aiken agreement test, and this
checkpoint document. `git diff --cached --check` is clean.

## Remaining P2 gate

The TypeScript ordered-field boundary cells now have concrete Cardano-envelope
evidence for fields 0 through 8, and field 8 has an exact TypeScript/Aiken
terminal-fold vector. P2 remains open: retained normal/forced DA
reconstruction, corresponding lifecycle agreement across all dynamic
families, and maximum applicable nested Data/content shape evidence are not
established by this checkpoint. No activation or release-evidence digest may
rely on this result alone.
