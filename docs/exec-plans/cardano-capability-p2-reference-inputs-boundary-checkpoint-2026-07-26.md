# Cardano capability P2 reference-inputs boundary checkpoint — 2026-07-26

Branch: `codex/tx-validation-capability-checkpoint`
Clean base: `341c5d98`

This checkpoint adds the exact ordered-collection boundary fixture for field 1
(`reference_inputs`). Every spending and reference input is a distinct real
UTxO in the same bounded emulator that accepts the boundary transaction. This
is one P2 field slice, not a complete ordered-collection or whole-P2 claim.

No retained-DA, applied Aiken lifecycle, script/redeemer, resolver, P3/P4,
deployment, Docker, live-network, P5, consensus-limit, or unrelated surface
changed.

## Real input supply and disjoint roles

The emulator is seeded with 514 genesis UTxOs at one deterministic enterprise
address. Lucid Emulator maps account-array position `i` to the real UTxO
`00...00#i`, and the test checks every returned transaction ID and index before
constructing candidates.

Supply reuses the conservative Cardano byte-envelope derivation:

```text
floor(16,384 maxTxSize / 32 transaction-ID bytes per input)
+ 2 adjacent-candidate reserve
= 514 real UTxOs
```

Each UTxO contains 10,000,000 lovelace and uses the same payment credential.
Genesis index 0 is reserved as the sole funding spend input. For requested
reference cardinality `N`, distinct real genesis indices `1..N` are added as
reference inputs. The complete funding supply, less the stabilized fee, is
balanced into one plain-Ada output, and exactly one valid vkey witness signs
the funding spend.

## Preserved envelope and exact boundary

The complete no-script minimum fee is stabilized using the preserved preprod
epoch-303 parameters, including `maxTxSize = 16,384`,
`maxValueSize = 5,000`, `minFeeA = 44`, and `minFeeB = 155,381`.

| Measurement | Accepting shape | Adjacent shape |
| --- | ---: | ---: |
| Requested and actual reference inputs | 433 | 434 |
| Funding spend input | `00...00#0` | `00...00#0` |
| Concrete reference indices | `1..433` | `1..434` |
| Spending inputs | 1 | 1 |
| Vkey witnesses | 1 | 1 |
| Plain-Ada outputs | 1 | 1 |
| Complete signed Cardano bytes | 16,380 | 16,418 |
| Margin against `maxTxSize` | +4 | -34 |
| Exact minimum fee | 876,101 lovelace | 877,773 lovelace |
| Result | submitted to the seeded bounded emulator | rejected by exact snapshot envelope |

The accepting transaction passed the emulator's existence, disjoint
spending/reference-input, unspent-input, funding-signature, and structural
checks. The adjacent rejection is the direct parameter comparison
`16,418 > 16,384`; neither provider-side size enforcement nor a Midgard
reference-input-count constant participates.

The adjacent shape requires 435 total real UTxOs, which remains within the 514
seeded UTxOs. Real-input supply therefore does not become an independent
boundary before the Cardano transaction-byte envelope.

## Exact Midgard receipt/fold path

The accepting signed Cardano bytes feed directly into the production
`cardanoTxBytesToMidgardNativeTxCanonicalCborV1` bridge and then into the
existing typed collection/item/chunk constructors, per-reveal verifier, and
complete terminal reconstruction fold.

| Measurement | Value |
| --- | ---: |
| Canonical Midgard transaction bytes | 17,333 |
| Canonical reference-inputs field bytes | 17,045 |
| Committed reference-input item count | 433 |
| Field-1 reveal steps | 433 |
| Largest revealed chunk | 38 bytes |
| Largest serialized auxiliary reveal witness | 615 bytes |
| Complete transaction fold steps | 436 |

Every field-1 reveal verifies against the exact converted transaction
commitment. The complete 436-step field-major receipt sequence reconstructs
the original canonical Midgard transaction byte for byte and terminates field
1 at the committed 17,045-byte length.

## Verification

Run from `demo/midgard-validation/` with one Vitest fork, one emulator, and a
2 GiB V8 heap ceiling:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
MIDGARD_PRINT_PROOF_FIT=1 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-reference-inputs-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (1 test; measured test 1.189 s, total 2.13 s).

The three preceding ordered-field controls were rerun serially in one fork:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (3 tests; total 4.71 s).

Additional checks:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
./node_modules/.bin/tsc --noEmit

../node_modules/.bin/eslint \
  tests/helpers/ordered-collection-boundary-v1.ts \
  tests/ordered-collection-reference-inputs-boundary-v1.test.ts \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  --max-warnings=0
```

Both pass.

## Remaining gate cells

This checkpoint closes the TypeScript Cardano boundary and terminal-fold
fixture for field 1. Fields 0, 1, 2, 4, and 6 now have this evidence. The next
ordered field without equivalent evidence is field 3 (`required_observers`);
fields 5, 6, and 8 also remain open, as do maximum-shape normal/forced
retained-DA reconstruction and applied Aiken lifecycle evidence.

P2 remains incomplete and unsupported activation remains fail closed.
