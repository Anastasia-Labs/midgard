# Cardano capability P2 outputs boundary checkpoint — 2026-07-26

Branch: `codex/tx-validation-capability-checkpoint`
Clean base: `5593d44c`

This checkpoint implements the smallest ordered-collection P2 boundary
vertical slice: reusable boundary-search and exact receipt/fold measurement
infrastructure, exercised first for field 2 (`outputs`). It does not claim
that the complete nine-field P2 gate passes.

No consensus limit, resolver, P3/P4 behavior, deployment, Docker topology,
live-network submission, release-evidence digest, P6 work, or stress surface
changed.

## Preserved Cardano envelope

The fixture uses the preprod epoch-303 boundary-relevant parameter values
preserved by the preceding proof-fit checkpoints:

- `maxTxSize = 16,384`;
- `maxValueSize = 5,000`;
- `minFeeA = 44`;
- `minFeeB = 155,381`;
- `coinsPerUtxoByte = 4,310`;
- `maxTxExMem = 16,500,000`;
- `maxTxExSteps = 10,000,000,000`; and
- `minFeeRefScriptCostPerByte = 15`.

The Cardano transaction uses one deterministic enterprise-key funding input,
plain-Ada outputs, an explicit change output, a minimum fee stabilized through
Cardano's `min_no_script_fee`, and one valid completed vkey signature.

The reusable search derives cardinality solely from exact signed Cardano CBOR
bytes. No Midgard output-count constant participates in acceptance or
rejection.

## Exact boundary

| Measurement | Accepting shape | Adjacent shape |
| --- | ---: | ---: |
| Requested payment outputs | 437 | 438 |
| Total Cardano outputs, including change | 438 | 439 |
| Complete signed Cardano bytes | 16,372 | 16,409 |
| Margin against `maxTxSize` | +12 | -25 |
| Minimum fee | 875,749 lovelace | 877,377 lovelace |
| Result | submitted to one bounded emulator | rejected by exact snapshot envelope |

The accepting signed transaction was submitted to one in-memory emulator
configured with the preserved 16,384-byte and 5,000-byte Value parameters.
Submission proves that its input, output, and signature structure is accepted
by the emulator. The adjacent classification is the direct protocol-parameter
comparison `16,409 > 16,384`; provider-side size enforcement is not assumed.

## Exact Midgard receipt/fold path

The accepting signed Cardano bytes feed directly into
`cardanoTxBytesToMidgardNativeTxCanonicalCborV1`. The resulting canonical
Midgard bytes then feed the existing typed collection/item/chunk constructors,
per-reveal verifier, and complete terminal reconstruction fold.

| Measurement | Value |
| --- | ---: |
| Canonical Midgard transaction bytes | 19,085 |
| Canonical outputs field bytes | 18,841 |
| Committed output item count | 438 |
| Output reveal steps | 438 |
| Complete transaction fold steps | 440 |
| Largest revealed chunk | 45 bytes |
| Largest serialized auxiliary reveal witness | 657 bytes |

Every output reveal is authenticated against the exact converted transaction
commitment. The complete 440-step field-major chunk sequence reconstructs the
original canonical Midgard transaction byte for byte and terminates field 2 at
the committed 18,841-byte length.

## Verification

Run from `demo/midgard-validation/` with one Vitest fork and a 2 GiB V8 heap
ceiling:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
MIDGARD_PRINT_PROOF_FIT=1 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (1 test; measured test 1.359 s, total 2.35 s).

Additional checks:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
./node_modules/.bin/tsc --noEmit

../node_modules/.bin/eslint \
  tests/helpers/ordered-collection-boundary-v1.ts \
  tests/ordered-collection-boundary-v1.test.ts \
  --max-warnings=0
```

Both pass.

## Remaining gate cells

This slice establishes the Cardano-derived accepting and adjacent outputs
boundary plus the exact TypeScript receipt/fold path. It leaves these cells
open:

- boundary fixtures for the other eight typed transaction fields;
- normal and forced retained-DA reconstruction using this maximum shape;
- corresponding applied on-chain semantic-transition evidence; and
- aggregation into later P5 target-network parity/release evidence.

P2 therefore remains incomplete and unsupported activation remains fail
closed.
