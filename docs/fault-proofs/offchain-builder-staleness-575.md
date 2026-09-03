# Fault-proof field-opening builder contract

This file is the standing compatibility reference for source comments which
point here. Current schema authority is the Aiken source, the generated testnet
blueprint, and the strict TypeScript twins named below.

## 1. Current field-opening model

Fault-proof families open one of the nine committed native-transaction fields
through the shared authenticated door:

- `onchain/aiken/lib/midgard/fraud-proofs/field-opening-v1.ak`
- `onchain/aiken/lib/midgard/native-tx-field-access-v1.ak`
- `demo/midgard-sdk/src/fraud-proof/field-opening-v1.ts`
- `demo/midgard-fault-proofs/src/field-opening-v1.ts`

The transaction id anchors body fields 0–5. The transaction's authenticated
`witness_set_hash` additionally anchors witness fields 6–8. A field opening
uses direct bytes, a raw reference UTxO, or a certified published chunk vector;
every route must authenticate the exact preimage against the positional
commitment carried by the anchored transaction.

## 2. Builder invariants

Thread state carries the transaction anchor required by the next step, not a
caller-asserted field hash. Redeemers carry `FieldOpening`, not a reproduced
legacy list. Field ordinals, positional reference-input indices, commitment
hashes, witness-set identity, and carriage tier are checked before submission
and again on-chain.

**The validators take a new parameter.** Any validator which opens certified
field carriage declares `field_preimage_certificate_policy_id`. Builders must
apply every blueprint-declared parameter in declared order and must derive the
resulting script hash/address from the applied script. A bare or partially
applied validator is not deployable.

## 3. Shared publication rules

Certified carriage uses the single field-preimage certificate policy and the
mint-welded field hash checked by the door. Family-specific publication datums
or computation-thread-coupled publication tokens are not valid alternatives.
The retired input-no-idx fold redeemer is not part of the current ABI.

## 4. Type discipline

The Lucid schemas are hand-written protocol twins, so successful TypeScript
compilation alone is not evidence of ABI parity. Builders must round-trip the
schema and exercise the real validator in focused emulator tests. Values with
the same primitive TypeScript type but different protocol meaning—especially a
transaction id and a field commitment—must be derived and checked at their use
site.

## 5. Affected families and modules

The shared door is consumed by the native-transaction families under
`onchain/aiken/lib/midgard/fraud-proofs/` and their matching SDK/submitter
modules. In particular, the following current families rely on this contract:

- `double-spend`, `no-input`, `zero-input`, and `no-reference-input`;
- `input-no-idx` and `reference-input-no-idx`;
- `invalid-signature`, `missing-signature`, and
  `missing-native-script-tx`;
- `withdrawn-reference-input`, `withdrawn-input`, `canonical-decodability`,
  `committed-field-shape`, `min-fee`, `l2-tx-mistag`,
  `value-not-preserved`, `input-set-uniqueness`, `mint-authorization`, and
  `network-id`;
- `native-script-decoding`, `missing-native-script-utxo`,
  `native-script-invalid`, and `min-ada`.

The current source and blueprint determine each family's exact step state and
declared parameter list. This document intentionally carries no copied script
hashes, byte sizes, branch names, or deployment snapshots.

## 6. Verification

Run the focused `field-opening-v1` package tests, the relevant family emulator
lifecycle, deployment inspection, and the semantic-resolver arity test after
changing a field schema, carriage rule, or validator parameter. Regenerate the
testnet blueprint and deployment identity for any validator or parameter
change.
