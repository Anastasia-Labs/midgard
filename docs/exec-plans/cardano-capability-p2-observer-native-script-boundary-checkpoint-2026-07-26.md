# Cardano capability P2 observer/native-script boundary checkpoint — 2026-07-26

Branch: `codex/tx-validation-capability-checkpoint`
Clean base: `ff829619`

This checkpoint adds the exact coupled Cardano boundary fixture for field 3
(`required_observers`) and native-script entries in field 6
(`script_witnesses`). Every observer is represented by the production
converter's supported zero-lovelace script reward withdrawal and has exactly
one matching, consumed native-script witness. This is a focused P2 slice, not
a whole-P2 claim.

No retained-DA, applied Aiken lifecycle, Plutus/redeemer, resolver, P3/P4,
deployment, Docker, live-network, P5, consensus-limit, release-digest, or
unrelated surface changed.

## Exact coupled Cardano shape

One bounded emulator is seeded with one real genesis funding UTxO at a
deterministic enterprise address. The transaction spends that one input,
balances it after fee into one plain-Ada output, and provides one valid common
spending-key vkey witness.

For requested cardinality `N`, the fixture generates `N` unique native scripts.
Each script is an all-of containing:

1. the common spending-key signer; and
2. a distinct invalid-hereafter expiry `20,000 + script_index`.

The transaction has no lower validity bound and has fixed TTL 10,000, safely
below every script expiry. Each native-script hash is used as one unique
network-0 script reward credential with an exact zero-lovelace withdrawal. The
same `N` scripts are included as native-script witnesses.

Candidate generation is on demand and terminates solely when exact signed
bytes cross the preserved `maxTxSize`. There is no pre-generated observer
supply and no Midgard observer or script count participates.

Immediately before submitting the accepting transaction, the test initializes
the emulator's public reward-account state for the exact measured withdrawal
keys:

```text
registeredStake = true
poolId = null
rewards = 0
```

The test proves set equality between all unique observer script hashes and all
unique native-script witness hashes. There are no Plutus scripts, redeemers,
datums, collateral inputs, or extraneous witnesses.

## Preserved envelope and exact boundary

The complete no-script minimum fee is stabilized using the preserved preprod
epoch-303 parameters, including `maxTxSize = 16,384`,
`maxValueSize = 5,000`, `minFeeA = 44`, and `minFeeB = 155,381`.

| Measurement                           |                          Accepting shape |                      Adjacent shape |
| ------------------------------------- | ---------------------------------------: | ----------------------------------: |
| Required observers / zero withdrawals |                                      224 |                                 225 |
| Native-script witnesses               |                                      224 |                                 225 |
| Spending inputs                       |                                        1 |                                   1 |
| Vkey witnesses                        |                                        1 |                                   1 |
| Plain-Ada outputs                     |                                        1 |                                   1 |
| Validity interval                     |                  lower unset, TTL 10,000 |             lower unset, TTL 10,000 |
| Distinct script expiries              |                         `20,000..20,223` |                    `20,000..20,224` |
| Complete signed Cardano bytes         |                                   16,338 |                              16,410 |
| Margin against `maxTxSize`            |                                      +46 |                                 -26 |
| Exact minimum fee                     |                         874,253 lovelace |                    877,421 lovelace |
| Result                                | submitted to the seeded bounded emulator | rejected by exact snapshot envelope |

The accepting transaction passed native-script evaluation, every withdrawal
credential check, zero-reward balance matching, funding-signature validation,
and missing/extraneous witness checks. Each withdrawal consumed one native
script hash, and every supplied native script was consumed.

The adjacent rejection is the direct snapshot comparison
`16,410 > 16,384`; neither provider-side size enforcement nor a Midgard count
constant participates.

## Exact Midgard receipt/fold paths

The accepting signed Cardano bytes feed directly into the production
`cardanoTxBytesToMidgardNativeTxCanonicalCborV1` bridge. Both fields then pass
through the existing typed collection/item/chunk constructors, every
per-reveal verifier, and the same exact complete terminal reconstruction fold.

| Measurement                                 | Field 3 observers | Field 6 native scripts |
| ------------------------------------------- | ----------------: | ---------------------: |
| Canonical field bytes                       |             6,722 |                  9,858 |
| Committed items                             |               224 |                    224 |
| Reveal steps                                |               224 |                    224 |
| Largest revealed chunk                      |          28 bytes |               44 bytes |
| Largest serialized auxiliary reveal witness |         492 bytes |              508 bytes |

The canonical Midgard transaction is 16,871 bytes. Every reveal verifies
against its exact converted transaction commitment. The shared 451-step
field-major receipt sequence reconstructs that original canonical transaction
byte for byte.

The test first constructs `N = 1` and proves both bridge/fold paths before
starting generic exact-byte boundary search.

## Verification

Run from `demo/midgard-validation/` with one Vitest fork, one emulator, and a
2 GiB V8 heap ceiling:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
MIDGARD_PRINT_PROOF_FIT=1 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-observer-native-script-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (1 test; measured test 1.544 s, total 2.50 s).

The four preceding ordered-field controls were rerun serially in one fork:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  tests/ordered-collection-reference-inputs-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (4 tests; total 5.97 s).

Additional checks:

```sh
NODE_OPTIONS=--max-old-space-size=2048 \
./node_modules/.bin/tsc --noEmit

../node_modules/.bin/eslint \
  tests/helpers/ordered-collection-boundary-v1.ts \
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
fixture for field 3 and the coupled NativeCardano form of field 6. Fields 0, 1,
2, 3, 4, 6, and 7 now have this evidence. The next dynamic field without
equivalent evidence is field 5 (`mint`); field 8 (`redeemers`) also remains
open.

Maximum-shape normal/forced retained-DA reconstruction, applied Aiken lifecycle
evidence, Plutus program-material scanning, and the other P2 dynamic-content
families remain open. P2 remains incomplete and unsupported activation remains
fail closed.
