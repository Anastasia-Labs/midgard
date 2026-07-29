# Cardano capability P2 signer/witness boundary checkpoint — 2026-07-26

Branch: `codex/tx-validation-capability-checkpoint`
Clean base: `23aff475`

This checkpoint extends the reusable ordered-collection boundary harness with
one real Cardano coupling: field 4 (`required_signers`) and field 7
(`address_witnesses`) are derived from the same exact fully signed
transaction. It is one coupled P2 boundary slice, not a complete
ordered-collection or whole-P2 claim.

No consensus limit, retained-DA path, applied Aiken lifecycle, resolver,
P3/P4 behavior, deployment, Docker topology, live-network submission, P5
digest, P6 work, or unrelated surface changed.

## Cardano construction

For requested cardinality `N`, the candidate contains:

- one deterministic enterprise-key funding input;
- one balanced plain-Ada output;
- `N` deterministic required-signer key hashes; and
- the corresponding `N` valid vkey witnesses.

Signer zero is also the funding input's spending credential. Consequently
every witness is required, every required signer has a witness, and the
transaction contains neither a missing nor extraneous witness.

The complete no-script minimum fee is stabilized using the preserved preprod
epoch-303 values:

- `maxTxSize = 16,384`;
- `maxValueSize = 5,000`;
- `minFeeA = 44`;
- `minFeeB = 155,381`; and
- `minFeeRefScriptCostPerByte = 15`.

The reusable search compares exact signed Cardano CBOR bytes with the snapshot
`maxTxSize`. No Midgard signer or witness count participates in the boundary.

## Exact coupled boundary

| Measurement                   |                   Accepting shape |                      Adjacent shape |
| ----------------------------- | --------------------------------: | ----------------------------------: |
| Requested required signers    |                               124 |                                 125 |
| Actual required signers       |                               124 |                                 125 |
| Actual vkey witnesses         |                               124 |                                 125 |
| Cardano outputs               |                                 1 |                                   1 |
| Complete signed Cardano bytes |                            16,351 |                              16,482 |
| Margin against `maxTxSize`    |                               +33 |                                 -98 |
| Exact minimum fee             |                  874,825 lovelace |                    880,589 lovelace |
| Result                        | submitted to one bounded emulator | rejected by exact snapshot envelope |

The accepting transaction was submitted to one in-memory emulator configured
with the preserved 16,384-byte and 5,000-byte Value parameters. It passed all
emulator input, required-signer, signature, missing-witness, and
extraneous-witness checks. The adjacent rejection is the direct
protocol-parameter comparison `16,482 > 16,384`; provider-side size
enforcement is not assumed.

## Exact Midgard receipt/fold paths

The accepting signed Cardano bytes feed directly into
`cardanoTxBytesToMidgardNativeTxCanonicalCborV1`. The same resulting
16,685-byte canonical Midgard transaction feeds both field measurements.

| Measurement                                 | Required signers, field 4 | Address witnesses, field 7 |
| ------------------------------------------- | ------------------------: | -------------------------: |
| Canonical field bytes                       |                     3,722 |                     12,774 |
| Committed item count                        |                       124 |                        124 |
| Reveal steps                                |                       124 |                        124 |
| Largest revealed chunk                      |                  28 bytes |                  101 bytes |
| Largest serialized auxiliary reveal witness |                 536 bytes |                  613 bytes |

Every reveal in both fields verifies against the exact converted transaction
commitment. For each field measurement, the complete 250-step field-major
receipt sequence reconstructs the original canonical Midgard transaction byte
for byte and terminates at the exact committed field length.

## Verification

Run from `demo/midgard-validation/` with one Vitest fork, one emulator, and a
2 GiB V8 heap ceiling:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
MIDGARD_PRINT_PROOF_FIT=1 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (1 test; measured test 1.135 s, total 2.22 s).

The centralized snapshot-parameter refactor was checked against the preceding
outputs boundary:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (1 test; measured test 1.340 s, total 2.31 s).

Additional checks:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
./node_modules/.bin/tsc --noEmit

../node_modules/.bin/eslint \
  tests/helpers/ordered-collection-boundary-v1.ts \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  --max-warnings=0
```

Both pass.

## Remaining gate cells

This checkpoint closes the TypeScript boundary and terminal-fold fixtures for
fields 4 and 7. The earliest ordered field without equivalent boundary
evidence is field 0 (`spend_inputs`). Fields 1, 3, 5, 6, and 8 also remain
open, as do maximum-shape normal/forced retained-DA reconstruction and applied
Aiken lifecycle evidence.

P2 remains incomplete and unsupported activation remains fail closed.
