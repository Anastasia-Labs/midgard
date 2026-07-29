# Cardano capability P2 mint boundary checkpoint — 2026-07-26

Branch: `codex/tx-validation-capability-checkpoint`
Clean base: `194bcffe`

This checkpoint adds the exact Cardano boundary fixture for field 5 (`mint`),
with field 6 (`script_witnesses`) exercised only as its native-policy
authorization control. Every minted policy has exactly one matching, consumed
native-script witness, and every minted asset is placed in an exact real output
Value. This is one focused P2 field slice, not a whole-P2 claim.

No retained-DA, applied Aiken lifecycle, Plutus/redeemer, resolver, P3/P4,
deployment, Docker, live-network, P5, consensus-limit, release-digest,
configuration, tooling, or unrelated surface changed.

## Exact native-policy mint shape

One bounded emulator is seeded with one real genesis funding UTxO containing
1,000,000,000,000 lovelace at a deterministic enterprise address. The
transaction spends that one input and provides one valid common spending-key
vkey witness.

For requested cardinality `N`, the fixture generates `N` unique native policy
scripts using the already-proven all-of shape:

1. the common spending-key signer; and
2. a distinct invalid-hereafter expiry `20,000 + policy_index`.

The transaction has no lower validity bound and has fixed TTL 10,000, safely
below every policy expiry. Each script hash is one distinct minting policy.
Each policy mints exactly one positive unit of the fixed valid asset name
`MidgardV1` (`4d6964676172645631`), and exactly those `N` native scripts are
included as witnesses.

The test proves set equality among the unique mint policy hashes, native-script
witness hashes, and output policy hashes. It also enumerates every mint and
output asset and checks the fixed asset name and quantity one. Emulator
submission independently enforces exact input-plus-mint/output value balance
and rejects missing or extraneous policy witnesses.

There are no withdrawals, Plutus scripts, redeemers, datums, collateral
inputs, or extraneous witnesses.

## Exact maxValueSize packing

The preserved `maxValueSize = 5,000` is an independent ledger envelope, not a
Midgard asset or policy cap. Policies are sorted by hash and greedily packed
into the minimum number of output Values. Every attempted insertion measures
exact `CML.Value.to_cbor_bytes().length`.

The first output receives all remaining funding Ada after fee and deposits for
later outputs. Every non-first output receives a safe 100,000,000-lovelace
deposit. Packing is rebuilt during fee stabilization and must converge with
the resulting output count and first-output coin.

| Measurement | Accepting shape | Adjacent shape |
| --- | ---: | ---: |
| Mint policies / assets / native scripts | 130 / 130 / 130 | 131 / 131 / 131 |
| Output count | 2 | 2 |
| Policies per output | 118 / 12 | 118 / 13 |
| Exact output Value bytes | 4,968 / 511 | 4,968 / 553 |
| Value margins against 5,000 | +32 / +4,489 | +32 / +4,447 |

Both sides of the transaction-byte boundary remain fully packed within
`maxValueSize`. The adjacent failure therefore is not caused by an oversized
Value. Candidate generation is on demand and terminates solely when exact
signed transaction bytes cross `maxTxSize`; no Midgard policy or asset count
participates.

## Preserved transaction envelope and exact boundary

The complete no-script minimum fee is stabilized using the preserved preprod
epoch-303 parameters, including `maxTxSize = 16,384`,
`maxValueSize = 5,000`, `minFeeA = 44`, and `minFeeB = 155,381`.

| Measurement | Accepting shape | Adjacent shape |
| --- | ---: | ---: |
| Mint policies | 130 | 131 |
| Minted assets at quantity one | 130 | 131 |
| Native-policy witnesses | 130 | 131 |
| Spending inputs | 1 | 1 |
| Vkey witnesses | 1 | 1 |
| Balanced outputs | 2 | 2 |
| Validity interval | lower unset, TTL 10,000 | lower unset, TTL 10,000 |
| Distinct policy expiries | `20,000..20,129` | `20,000..20,130` |
| Complete signed Cardano bytes | 16,376 | 16,500 |
| Margin against `maxTxSize` | +8 | -116 |
| Exact minimum fee | 875,925 lovelace | 881,381 lovelace |
| Result | submitted to the seeded bounded emulator | rejected by exact snapshot envelope |

The accepting transaction passed native-policy evaluation, exact
mint/output/input balance, funding-signature validation, UTxO existence, and
missing/extraneous witness checks.

The adjacent rejection is the direct snapshot comparison
`16,500 > 16,384`. Its output Values remain valid at 4,968 and 553 bytes, so
the exact signed Cardano transaction envelope is the measured binder.

## Exact Midgard field-5 receipt/fold path

The accepting signed Cardano bytes feed directly into the production
`cardanoTxBytesToMidgardNativeTxCanonicalCborV1` bridge and then into the
existing typed collection/item/chunk constructors, every per-reveal verifier,
and the exact complete terminal reconstruction fold.

| Measurement | Value |
| --- | ---: |
| Canonical Midgard transaction bytes | 16,986 |
| Canonical mint field bytes | 5,462 |
| Committed mint policy items | 130 |
| Field-5 reveal steps | 130 |
| Largest revealed chunk | 43 bytes |
| Largest serialized auxiliary reveal witness | 468 bytes |
| Complete transaction fold steps | 265 |

Every field-5 reveal verifies against the exact converted transaction
commitment. The complete 265-step field-major receipt sequence reconstructs
the original canonical Midgard transaction byte for byte. Field 7 also
completes all 130 vkey-witness reveals and the same terminal fold. Field 6
completes all 130 native-script reveals and the same terminal fold, but it is
only an authorization-coupling control for this checkpoint.

The test first constructs `N = 1` and proves the CML, production bridge, and
both field-5 and field-6 paths before starting generic exact-byte boundary
search.

## Verification

Run from `demo/midgard-validation/` with one Vitest fork, one emulator, and a
2 GiB V8 heap ceiling:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
MIDGARD_PRINT_PROOF_FIT=1 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-mint-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (1 test; measured test 3.239 s, total 4.51 s).

The five preceding ordered-field controls were rerun serially in one fork:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  tests/ordered-collection-reference-inputs-boundary-v1.test.ts \
  tests/ordered-collection-observer-native-script-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (5 tests; total 7.49 s).

Additional checks:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
./node_modules/.bin/tsc --noEmit

../node_modules/.bin/eslint \
  tests/helpers/ordered-collection-boundary-v1.ts \
  tests/ordered-collection-mint-boundary-v1.test.ts \
  tests/ordered-collection-observer-native-script-boundary-v1.test.ts \
  tests/ordered-collection-reference-inputs-boundary-v1.test.ts \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  --max-warnings=0
```

Both pass.

## Remaining gate cells

This checkpoint closes the TypeScript exact Cardano boundary and terminal-fold
fixture for field 5. Fields 0 through 7 now have this evidence, with field 7
covered by its real vkey-witness boundary and field 6 covered by native-script
observer and policy authorization controls.
The remaining transaction dynamic field without equivalent evidence is field
8 (`redeemers`).

Maximum-shape normal/forced retained-DA reconstruction, applied Aiken lifecycle
evidence, Plutus program-material scanning, redeemer/data scanning, and the
other P2 dynamic-content families remain open. P2 remains incomplete and
unsupported activation remains fail closed.
