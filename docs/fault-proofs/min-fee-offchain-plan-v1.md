# Min-fee standalone fault proof: offchain implementation plan (v1)

Plan date: 2026-08-26. Task: Q20 (`min-fee`). This plan covers the
single-party standalone family only. It does not register a production
catalogue category, change the interactive validation dispute, regenerate the
blueprint, or change fee parameters.

## 1. Rule and exact on-chain evidence

The disputed block header supplies the non-negative schedule
`min_fee_a`/`min_fee_b`. For the committed native-V1 transaction, let
`canonical_tx_size` be the byte length of the exact canonical full transaction
derived from the compact scalar fields and all nine authenticated §5.1 field
preimage lengths. The ledger rule is:

```text
fee >= min_fee_a * canonical_tx_size + min_fee_b
```

The standalone fault is the strict complement:

```text
fee < min_fee_a * canonical_tx_size + min_fee_b
```

`onchain/aiken/lib/midgard/fraud-proofs/native-tx/compact.ak` is the sole
formula authority. `min_fee_lovelace_v1` computes the minimum and
`native_tx_canonical_size_v1` computes the exact CBOR size. Both
`validation-machine-v1.ak`'s `reject_min_fee` branch and standalone min-fee
step-02 call that helper, so the two adjudication paths cannot drift.

The evidence chain is:

1. Init mints the category-id-plus-header-hash computation-thread token from
   the immutable catalogue proof and binds the prover.
2. Step-01 uses the shared `pass_native_tx_to_next_step` path. It authenticates
   the state-queue header, re-derives its counted `transactions_root`, proves
   membership of the raw compact transaction CBOR, and writes only values read
   from that authenticated evidence into step-02 state: the compact
   transaction, inline fee, transaction id, and the header's `min_fee_a` and
   `min_fee_b`.
3. Step-02 re-derives the body opening against the thread-carried transaction
   id and the witness opening against both that id and the thread-carried
   `witness_set_hash`. It opens every field through the §8.8 door using exactly
   nine carriages in §2.5 wire order: spend inputs, reference inputs, outputs,
   required observers, required signers, mint, script witnesses, address
   witnesses, redeemers. Each length comes from `field_total_length` after
   authentication, then feeds `native_tx_canonical_size_v1`.
4. Step-02 finalizes only for the strict `<` predicate, burns the computation
   thread, and mints the permanent fraud-proof token. A fee exactly equal to
   the minimum is honest and must be refused at this comparison on-chain.

This shape prevents both size-inflation attacks. A padded body-field preimage
does not match the compact body commitment. A forged witness set and internally
matching witness-field preimages do not match the `witness_set_hash` anchored
by step-01.

## 2. Offchain design

### 2.1 SDK wire twin and rule helpers

`demo/midgard-sdk/src/fraud-proof/min-fee.ts` mirrors all four on-chain data
surfaces positionally:

- step-01 datum/redeemer using the shared native inclusion schema;
- step-02 state `{ bad_tx, bad_tx_body_fee, bad_tx_id, min_fee_a, min_fee_b }`;
- step-02 args `{ input_index, output_index,
fraud_proof_mint_redeemer_index, native_tx_compact_cbor, witness_set,
field_carriages }` with exactly nine `FieldCarriageV1` values; and
- the shared cancellation constructor.

The SDK exposes strict helpers for the canonical minimum and the violation
predicate. They reject negative parameters and non-exact sizes, use bigint
arithmetic, and delegate canonical-size derivation to
`@al-ft/midgard-core`'s
`computeMidgardNativeTxCanonicalSizeFromProofSourceV1` rather than rebuilding
CBOR-size arithmetic.

### 2.2 Preparation

`demo/midgard-fault-proofs/src/prepare-min-fee.ts` takes security-grade
canonical block evidence, the disputed transaction proof source and all nine
field preimages. It must:

- pass `assertSecurityGradeEvidenceV1` and
  `assertNativeInclusionRootAuthenticatedV1`;
- prove the compact transaction/proof-source/transaction-id agreement;
- require every preimage to match the committed field at its literal index;
- compute the exact canonical size and minimum locally;
- refuse honest transactions (`fee >= minimum`) before any submission;
- preserve the header's fee parameters without caller overrides; and
- plan each of the nine field carriages through the existing §8 tier planner.

The result contains the inclusion plan, exact fee evidence, witness compact,
ordered field-opening plans, and the category-id-parameterized thread token
asset name. No automatic cancellation or registration lookup is permitted.

### 2.3 Submission chain

The family adds explicit pre-registration modules:

- `submit-min-fee-init.ts` — generic init semantics with explicit contracts
  and category proof;
- `submit-min-fee-step-01.ts` — native inclusion binding and exact step-02
  datum;
- `submit-min-fee-step-02.ts` — publish/reference the planned field carriage,
  resolve all input/output/redeemer indices from the built transaction,
  finalize and mint the permanent proof token;
- `submit-min-fee-cancel.ts` — prover-signed explicit cancellation from either
  step, burning the thread NFT; and
- barrels in the fault-proof package and SDK.

Every step validator is published as a reference script and consumed by
reference. Inline validator attachment is forbidden. A submitter must compare
the reference UTxO's script hash with the exact applied validator hash before
building. Transaction completion keeps `localUPLCEval: true`.

The two-step core is resumable: callers inspect the thread NFT and decode the
datum to resume at step-01 or step-02. An absent thread after a submitted
transaction is reconciled as either permanent proof-token success or explicit
cancellation; it is never silently restarted.

## 3. Registration posture and reserved id

Production registration is deferred to the catalogue registration wave. This
work must not edit `demo/midgard-sdk/src/fraud-proof/catalogue.ts`, the canonical
deployment manifest, `submit-init.ts`'s registered union, or CLI category
parsing.

Standing pre-registration assignments are:

- `0000000b`: fabricated-deposit;
- `0000000c`: fabricated-withdrawal;
- `0000000d`: native-script-decoding test id;
- `0000000e`: missing-signature expected id, assigned by
  `missing-signature-offchain-plan-v1.md`;
- `0000000f`: missing-native-script-tx;
- `00000010`: withdrawn-reference-input;
- `00000011`: canonical-decodability; and
- `00000012`: committed-field-shape.

Therefore min-fee's expected-but-not-promised harness id is **`00000013`**
(index 19). The emulator records that literal as
`MIN_FEE_TEST_CATEGORY_ID_V1`. Removal uses an explicit category record and
`buildExplicitCategoryRemovalContracts`; it must not pass the unregistered id
through `parseFraudProofCatalogueDeploymentInfo`. The registration wave must
re-scan reservations, allocate the then-next-free id, append all immutable
catalogue/deployment surfaces together, rebuild reference scripts, and perform
a fresh genesis-level deployment. The same wave reserves `00000014` through
`00000018` respectively for withdrawal-mistag, double-withdraw,
cross-block-duplicate-event, l2-tx-mistag, and withdrawn-input; min-fee must not
consume any of those later slots either.

## 4. Emulator acceptance

One pre-registration harness publishes both applied min-fee validators as
reference scripts, builds explicit contracts, and extends the test catalogue
under `00000013` without changing production catalogue code.

The lifecycle suites must prove:

1. **Fault polarity:** init → step-01 → step-02 → permanent proof-token mint →
   fraudulent state-queue commitment removal. The node NFT burns, the proof
   token remains at the same out-ref, and a second removal claim is refused.
2. **Honest polarity:** the same authentic transaction with a schedule whose
   exact minimum equals its fee reaches step-02, then local guards are bypassed
   deliberately and the submitted transaction is refused by the compiled
   validator at the exact `<` comparison. No proof token is minted and the
   commitment remains.
3. **Adversarial negatives:** forged step-01 fee schedule, padded body field,
   forged witness set plus matching witness preimages, wrong reference script,
   permuted/missing field carriage, and non-prover cancellation all fail
   closed.
4. **Cancel/resume:** explicit cancel succeeds from both step states; a journey
   interrupted after step-01 resumes the same NFT at step-02 without reminting
   init or changing the header/category identity.

Single-party semantics are preserved throughout: the evidence is public block
and transaction material; there is no operator turn or interactive fallback in
the standalone path.

## 5. Verification gates

All counts must be non-zero and reported, not inferred from exit status.

```bash
cd onchain/aiken
node scripts/run-focused-check.mjs \
  fraud_proofs/min_fee/step_01 \
  min_fee_step_01_binds_native_v1_block_fixture \
  min_fee_step_01_forwards_the_header_fee_schedule \
  min_fee_step_01_rejects_forged_transactions_root \
  min_fee_step_01_rejects_a_forged_fee_schedule_in_state
node scripts/run-focused-check.mjs \
  fraud_proofs/min_fee/step_02 \
  min_fee_step_02_accepts_a_fee_below_the_flat_minimum \
  min_fee_step_02_accepts_a_fee_below_the_sized_minimum \
  min_fee_step_02_rejects_a_fee_exactly_at_the_minimum \
  min_fee_step_02_rejects_a_fee_exactly_at_the_sized_minimum \
  min_fee_step_02_rejects_a_fee_above_the_minimum \
  min_fee_step_02_rejects_an_inflated_body_field_preimage \
  min_fee_step_02_rejects_a_forged_witness_set_inflation
aiken check -m 'midgard/validation_machine_v1.{..}'
node scripts/verify-normalized-format.mjs \
  validators/fraud-proofs/min-fee/step-01.ak \
  validators/fraud-proofs/min-fee/step-02.ak \
  lib/midgard/fraud-proofs/min-fee/step-01.ak \
  lib/midgard/fraud-proofs/min-fee/step-02.ak \
  lib/midgard/fraud-proofs/native-binding-fixture-v1.ak \
  lib/midgard/fraud-proofs/native-tx/compact.ak \
  lib/midgard/validation-machine-v1.ak
aiken check --skip-tests
```

```bash
pnpm --dir demo/midgard-fault-proofs exec vitest run \
  tests/canonical-evidence-source-v1.test.ts \
  tests/prepare-min-fee.test.ts \
  tests/submit-init-emulator-min-fee-v1.test.ts
pnpm --dir demo/midgard-core exec vitest run \
  tests/capability-parity-v1.test.ts
pnpm --dir demo/midgard-fault-proofs run typecheck
pnpm --dir demo exec prettier --check \
  midgard-sdk/src/fraud-proof/min-fee.ts \
  midgard-fault-proofs/src/prepare-min-fee.ts \
  midgard-fault-proofs/src/submit-min-fee-init.ts \
  midgard-fault-proofs/src/submit-min-fee-step-01.ts \
  midgard-fault-proofs/src/submit-min-fee-step-02.ts \
  midgard-fault-proofs/src/submit-min-fee-cancel.ts \
  midgard-fault-proofs/tests/prepare-min-fee.test.ts \
  midgard-fault-proofs/tests/submit-init-emulator-min-fee-v1.test.ts \
  midgard-fault-proofs/tests/canonical-evidence-source-v1.test.ts \
  midgard-fault-proofs/tests/helpers/canonical-block-evidence-fixture-v1.ts \
  midgard-fault-proofs/tests/support/emulator/contracts.ts \
  midgard-fault-proofs/tests/support/emulator/harness.ts \
  midgard-fault-proofs/tests/support/emulator/removal-deployment.ts \
  ../docs/fault-proofs/min-fee-offchain-plan-v1.md
pnpm --dir demo exec eslint \
  midgard-sdk/src/fraud-proof/min-fee.ts \
  midgard-fault-proofs/src/prepare-min-fee.ts \
  midgard-fault-proofs/src/submit-min-fee-init.ts \
  midgard-fault-proofs/src/submit-min-fee-step-01.ts \
  midgard-fault-proofs/src/submit-min-fee-step-02.ts \
  midgard-fault-proofs/src/submit-min-fee-cancel.ts \
  midgard-fault-proofs/tests/prepare-min-fee.test.ts \
  midgard-fault-proofs/tests/submit-init-emulator-min-fee-v1.test.ts \
  midgard-fault-proofs/tests/canonical-evidence-source-v1.test.ts \
  midgard-fault-proofs/tests/helpers/canonical-block-evidence-fixture-v1.ts \
  midgard-fault-proofs/tests/support/emulator/contracts.ts \
  midgard-fault-proofs/tests/support/emulator/harness.ts \
  midgard-fault-proofs/tests/support/emulator/removal-deployment.ts
```

The TypeScript envelope suite must also assert the exact Data CBOR for
step-02 state and args, the applied parameter order, distinct reference-script
hashes, exactly nine ordered carriages, and the adjacent fee boundary. The
target closure is 11/11 family Aiken selectors, the full non-zero
`validation_machine_v1` suite, at least four prepare/envelope cases, both
emulator polarities through the decisive on-chain result, removal, negatives,
cancel and resume, with zero failures.
