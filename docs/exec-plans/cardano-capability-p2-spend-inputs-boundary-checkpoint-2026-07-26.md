# Cardano capability P2 spend-inputs boundary checkpoint — 2026-07-26

Branch: `codex/tx-validation-capability-checkpoint`
Clean base: `559198bb`

This checkpoint adds the exact ordered-collection boundary fixture for field 0
(`spend_inputs`). Every measured input is a real UTxO in the same bounded
emulator that accepts the boundary transaction. This is one P2 field slice,
not a complete ordered-collection or whole-P2 claim.

No reference-input, retained-DA, applied Aiken lifecycle, resolver, P3/P4,
deployment, Docker, live-network, P5, consensus-limit, or unrelated surface
changed.

## Real input supply

The emulator is seeded with 514 genesis UTxOs at one deterministic enterprise
address. Lucid Emulator maps account-array position `i` to the real UTxO
`00...00#i`, and the test checks every returned transaction ID and index before
constructing candidates.

Supply is derived from the Cardano byte envelope:

```text
floor(16,384 maxTxSize / 32 transaction-ID bytes per input)
+ 2 adjacent-candidate reserve
= 514 real UTxOs
```

This is deliberately conservative: every serialized input necessarily carries
the 32-byte transaction ID in addition to its index and CBOR framing, while
the transaction also requires fixed fields and a witness. The supply therefore
cannot become a second input-count limit before the byte envelope is reached.

Each UTxO contains 10,000,000 lovelace and uses the same payment credential.
For requested cardinality `N`, the candidate consumes real genesis indices
`0..N-1`, balances their complete supply into one plain-Ada output after fee,
and provides exactly one valid common-key vkey witness.

## Preserved envelope and exact boundary

The complete no-script minimum fee is stabilized using the preserved preprod
epoch-303 parameters, including `maxTxSize = 16,384`,
`maxValueSize = 5,000`, `minFeeA = 44`, and `minFeeB = 155,381`.

| Measurement                   |                          Accepting shape |                      Adjacent shape |
| ----------------------------- | ---------------------------------------: | ----------------------------------: |
| Requested and actual inputs   |                                      434 |                                 435 |
| Real genesis indices          |                                 `0..433` |                            `0..434` |
| Vkey witnesses                |                                        1 |                                   1 |
| Plain-Ada outputs             |                                        1 |                                   1 |
| Complete signed Cardano bytes |                                   16,379 |                              16,417 |
| Margin against `maxTxSize`    |                                       +5 |                                 -33 |
| Exact minimum fee             |                         876,057 lovelace |                    877,729 lovelace |
| Result                        | submitted to the seeded bounded emulator | rejected by exact snapshot envelope |

The accepting transaction passed the emulator's existence, unspent-input,
common spending-credential, signature, missing-witness, and
extraneous-witness checks for all 434 inputs. The adjacent rejection is the
direct parameter comparison `16,417 > 16,384`; neither provider-side size
enforcement nor a Midgard input-count constant participates.

The 435-input adjacent candidate remains well within the 514 real-UTxO supply,
so the Cardano byte envelope is the measured boundary.

## Exact Midgard receipt/fold path

The accepting signed Cardano bytes feed directly into the production
`cardanoTxBytesToMidgardNativeTxCanonicalCborV1` bridge and then into the
existing typed collection/item/chunk constructors, per-reveal verifier, and
complete terminal reconstruction fold.

| Measurement                                 |     Value |
| ------------------------------------------- | --------: |
| Canonical Midgard transaction bytes         |    17,336 |
| Canonical spend-inputs field bytes          |    17,083 |
| Committed input item count                  |       434 |
| Field-0 reveal steps                        |       434 |
| Largest revealed chunk                      |  38 bytes |
| Largest serialized auxiliary reveal witness | 614 bytes |
| Complete transaction fold steps             |       436 |

Every field-0 reveal verifies against the exact converted transaction
commitment. The complete 436-step field-major receipt sequence reconstructs
the original canonical Midgard transaction byte for byte and terminates field
0 at the committed 17,083-byte length.

## Verification

Run from `demo/midgard-validation/` with one Vitest fork, one emulator, and a
2 GiB V8 heap ceiling:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
MIDGARD_PRINT_PROOF_FIT=1 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (1 test; measured test 1.201 s, total 2.15 s).

The two preceding ordered-field controls were rerun together:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (2 tests; total 3.41 s).

Additional checks:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
./node_modules/.bin/tsc --noEmit

../node_modules/.bin/eslint \
  tests/helpers/ordered-collection-boundary-v1.ts \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  --max-warnings=0
```

Both pass.

## Remaining gate cells

This checkpoint closes the TypeScript Cardano boundary and terminal-fold
fixture for field 0. The next ordered field without equivalent evidence is
field 1 (`reference_inputs`). Fields 3, 5, 6, and 8 also remain open, as do
maximum-shape normal/forced retained-DA reconstruction and applied Aiken
lifecycle evidence.

P2 remains incomplete and unsupported activation remains fail closed.
