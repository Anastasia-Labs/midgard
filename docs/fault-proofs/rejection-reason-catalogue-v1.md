# RejectionReasonV1 — current protocol reference

`RejectionReasonV1` is the typed operator-rejection reason carried only by a
forced-inclusion verdict. The authoritative constructor order, fields, and
legacy-code bridge are implemented in:

- `onchain/aiken/lib/midgard/rejection-reason-v1.ak`
- `demo/midgard-sdk/src/rejection-reason-v1.ts`
- `docs/spec/midgard-tx.md` §13

This document records the stable design rules without copying the complete
constructor table from source.

## 1. Verdict shape

`OperatorVerdictV1` has two arms:

- `ForcedTxValid`
- `ForcedTxInvalid { reason: RejectionReasonV1 }`

Normal `transactions_root` leaves do not carry a rejection reason; they are
acceptance claims and must use validity code zero. A malformed non-zero normal
leaf is covered by `l2TxMistag`.

## 2. Reason space

`RejectionReasonV1` currently has 47 constructors. They name the smallest
authenticated subject needed to adjudicate the fault, covering:

- field size/type and transaction structure;
- input-set, validity, network, fee, and signature failures;
- witness, native-script, observer, and script-integrity failures;
- resolved input/reference-script and output failures;
- purpose/source/redeemer and unused-material failures;
- native execution and Plutus/CEK failures; and
- asset-accumulation, min-Ada, and value-preservation failures.

Constructor order is wire identity. Additions, removals, or reorderings require
a new protocol/deployment identity.

## 3. Subject discipline

A reason carries subject coordinates, not proof arguments. Examples include a
field/item ordinal, input source kind and ordinal, purpose kind and ordinal, or
execution index. The verifier reopens the committed subject and derives the
predicate itself; the operator cannot supply an unbound byte offset, hash, or
verdict witness.

Transaction-global reasons carry no fake coordinate. `ValueNotPreserved` is
transaction-global; its standalone single-asset proof claim is selected by the
challenger and independently authenticated by the registered
`valueNotPreserved` family.

## 4. Legacy-code bridge

`rejection_code_of` is a total, intentionally non-injective map from the 47
typed reasons to the 19 frozen `E_*` descriptor codes. The typed constructor is
the protocol reason; the coarse code exists for descriptor compatibility and
observability. Consumers must not infer a unique typed reason from a coarse
code.

## 5. Interaction boundary

Deterministic statements over retained authenticated data are single-party,
even when they require a multi-transaction computation thread. Native-script
decoding, value preservation, input uniqueness, mint authorization, and other
registered families follow this rule.

`PlutusExecutionFailed` remains on the interactive CEK/validation-dispute path
because resolving it requires authenticated execution traces. Static selection
or malformed-data facts that share a coarse Plutus code do not inherit that
interactive classification.

## 6. Verification obligations

- TypeScript and Aiken encodings must remain byte-identical.
- Every constructor must map through `rejection_code_of`.
- Subject indices and source/purpose kinds must be range-checked before use.
- A reason against an honest accepted/rejected transaction must fail at the
  decisive on-chain predicate.
- Any compiler, schema, constructor-order, or validator change must regenerate
  the deployment identity and rerun the ABI/semantic resolver tests.
