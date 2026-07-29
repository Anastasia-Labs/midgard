# Cardano capability P2 retained-DA boundary checkpoint — 2026-07-26

Branch: `codex/tx-validation-capability-checkpoint`
Clean base: `ac495b17`

This checkpoint proves byte-exact reconstruction from both canonical V1
normal and forced retained-DA classifications for every established
Cardano-derived maximum transaction fixture. It also exercises one populated
all-nine-field canonical transaction through the same path and restores its
stale TypeScript/Aiken compact-commitment vector.

> Evidence correction: this historical checkpoint's helper exercised the SDK
> codec and mandatory envelope, then reconstructed directly from its decoded
> synthetic preimages. Its empty committed roots and zero validation traces
> mean it did **not** pass production `reconstructDaPayloadV1`. The corrected
> production integration and strict evidence are recorded in
> `cardano-capability-p2-production-retained-da-checkpoint-2026-07-26.md`.

This is a retained-DA and cross-language codec checkpoint, not a whole-P2
claim. It does not change consensus limits, collateral policy, the canonical
V1 schema, resolver behavior, activation, P3/P4/P5/P6, deployment, Docker,
release evidence, or compatibility behavior.

## Production retention shape

The boundary harness mirrors the production DA representation:

- `transactions` retains the exact compact proof source under the 32-byte L2
  transaction ID;
- `transaction_preimages` retains the complete canonical transaction under
  that same ID;
- `forced_transactions` retains the exact source plus
  `operator_validity = TxIsValid` under a canonical tx-order
  `OutputReference`;
- `forced_transaction_preimages` retains the same complete canonical
  transaction under that same tx-order key; and
- source values use the same Aiken-compatible Plutus-Data serialization as
  the node's production builders.

The forced tx-order key is deliberately distinct from the L2 transaction ID.
The test uses the canonical output reference `(transaction_id, 0)` and
requires exact same-key source/preimage coverage after SDK encoding, mandatory
DA envelope wrapping, unwrapping, and decoding.

For each classification independently, the harness:

1. decodes the retained canonical full transaction;
2. recomputes its transaction ID, proof source, and transaction commitment;
3. compares every compact source component exactly;
4. derives all nine fields' typed item/chunk proofs;
5. verifies every bounded chunk against the retained source; and
6. executes the terminal reconstruction fold and requires byte equality with
   the retained canonical preimage.

The harness is intentionally not a replacement for production DA
reconstruction. P2 remains fail closed while the remaining dynamic-content
cells are open.

## Cardano-derived maximum fixtures

Every previously established accepting Cardano boundary now passes through
both retained classifications. The collateralized redeemer fixture remains
rejected by the production bridge; only its exact collateral-free
shared-schema parallel enters Midgard DA.

| Maximum shape | Canonical bytes | Bounded reveal steps | Inner paired DA bytes | Stored paired DA bytes |
| --- | ---: | ---: | ---: | ---: |
| Spend inputs, field 0 | 17,336 | 436 | 37,470 | 37,513 |
| Reference inputs, field 1 | 17,333 | 436 | 37,466 | 37,509 |
| Outputs, field 2 | 19,085 | 440 | 41,078 | 41,121 |
| Observers/native scripts, fields 3/6 | 16,871 | 451 | 36,526 | 36,569 |
| Required signers/address witnesses, fields 4/7 | 16,685 | 250 | 36,133 | 36,176 |
| Mint/native policies, fields 5/6 | 16,986 | 265 | 36,766 | 36,809 |
| Spend redeemers, field 8 | 16,933 | 596 | 36,645 | 36,688 |

Each DA size includes two full retained preimages: one normal and one forced.
Both classifications execute the same number of individually verified reveal
steps and reconstruct the same canonical transaction bytes.

The real maximum redeemer transaction still has:

```text
E_CONVERSION_UNSUPPORTED_FEATURE
detail = collateral_inputs
```

The test neither strips collateral nor weakens that rejection. Its
collateral-free parallel preserves all 296 redeemers exactly and has no
withdrawals or mint. The earlier semantic comparisons for populated
withdrawal reward accounts/amounts and mint policy/asset quantities remain
unchanged.

## Populated all-field vector

`native-size-balanced-15_5k.json` supplies one 16,126-byte canonical
transaction with all nine dynamic fields populated:

| Field | Items |
| --- | ---: |
| Spend inputs | 48 |
| Reference inputs | 32 |
| Outputs | 48 |
| Required observers | 18 |
| Required signers | 17 |
| Mint policies | 24 |
| Address witnesses | 17 |
| Script witnesses | 68 |
| Redeemer witnesses | 68 |

The complete 340-step reveal sequence reconstructs the exact 16,126 bytes
from both normal and forced retention. The paired inner payload is 35,004
bytes and its stored identity envelope is 35,047 bytes.

The retained-DA test now also binds the recomputed transaction ID to the JSON
fixture. That exposed stale generated compact artifacts: the fixture still
contained raw whole-preimage hashes and a pre-domain-separated transaction ID
from before typed collection commitments became canonical.

The full canonical transaction and every field preimage were left unchanged.
The JSON compact body, compact transaction, nine field commitment hashes,
witness-set hash, and transaction ID were regenerated from the current
canonical V1 codec. The Aiken vector was updated to the same values and now
recomputes:

- all six body-field typed collection commitments;
- all three witness-field typed collection commitments;
- the aggregate witness-set hash; and
- the domain-separated native transaction ID.

This preserves exact semantic checks rather than replacing them with counts.

## Verification

From `demo/midgard-validation/`:

```sh
MIDGARD_PRINT_RETAINED_DA=1 \
./node_modules/.bin/vitest run \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  tests/ordered-collection-reference-inputs-boundary-v1.test.ts \
  tests/ordered-collection-observer-native-script-boundary-v1.test.ts \
  tests/ordered-collection-mint-boundary-v1.test.ts \
  tests/ordered-collection-redeemer-boundary-v1.test.ts \
  tests/redeemer-collateral-schema-feasibility-v1.test.ts \
  tests/retained-da-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true
```

Result: **PASS** (`9` files, `9` tests; total 25.28 s).

```sh
./node_modules/.bin/tsc --noEmit

../node_modules/.bin/eslint \
  tests/helpers/retained-da-boundary-v1.ts \
  tests/retained-da-boundary-v1.test.ts \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-mint-boundary-v1.test.ts \
  tests/ordered-collection-observer-native-script-boundary-v1.test.ts \
  tests/ordered-collection-reference-inputs-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  tests/ordered-collection-redeemer-boundary-v1.test.ts
```

Both pass.

From `onchain/aiken/`:

```sh
aiken fmt --check \
  lib/midgard/fraud-proofs/native-tx.size-balanced.test.ak

aiken check -m size_balanced_lucid_midgard_native_tx_decodes
```

Result: **PASS** (`1` focused unit test; memory `194,080,666`, CPU
`77,818,051,296`).

That whole-fixture Aiken evaluation is cross-language diagnostic evidence,
not an L1 proof-fit claim. L1 operation remains decomposed into the bounded
item/chunk transitions exercised above.

## Remaining P2 gate

This checkpoint closes retained normal/forced reconstruction for the seven
Cardano-derived ordered-field maxima and restores one populated all-field
TypeScript/Aiken codec vector.

P2 still requires exact maximum-shape evidence for the nested dynamic-content
families called out by the plan: output Value policy/assets, inline datum and
redeemer Data traversal, and script-envelope/program-material traversal.
Cross-language terminal-transition agreement must remain explicit for every
remaining maximum shape. Unsupported activation remains fail closed until
those cells pass.
