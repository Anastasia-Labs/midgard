# RejectionReasonV1 — current protocol reference

`RejectionReason` is the typed operator-rejection reason carried only by a
forced-inclusion verdict. The authoritative constructor order and fields are
implemented in:

- `onchain/aiken/lib/midgard/rejection-reason-v1.ak`
- `demo/midgard-sdk/src/rejection-reason.ts`
- `docs/spec/midgard-tx.md` §13

This document records the stable design rules without copying the complete
constructor table from source.

## 1. Verdict shape

`OperatorVerdict` has two arms:

- `ForcedTxValid`
- `ForcedTxInvalid { reason: RejectionReason }`

Forced submitted full and compact encodings contain no validity scalar. The
verdict above is the sole committed decision. Any machine scalar is derived
only temporarily; replay determines the actual result independently.

Normal `transactions_root` leaves do not carry a rejection reason; they are
acceptance claims and must use validity code zero. A malformed non-zero normal
leaf is covered by `l2TxMistag`.

## 2. Reason space

`RejectionReason` has 49 constructors. They name the smallest
authenticated subject needed to adjudicate the fault, covering:

- field size/type and transaction structure;
- input-set, validity, network, fee, and signature failures;
- witness, native-script, observer, and script-integrity failures;
- resolved input/reference-script and output failures;
- purpose/source/redeemer and unused-material failures;
- native execution and Plutus/CEK failures; and
- asset-accumulation, min-Ada, and value-preservation failures.

Constructor order is wire identity. An addition, removal, or reordering is a
leaf-schema format revision (`docs/spec/midgard-tx.md` §13.1) and a new
deployment identity.

## 3. Subject discipline

A reason carries subject coordinates, not proof arguments. Examples include a
field/item ordinal, input source kind and ordinal, purpose kind and ordinal, or
execution index. The verifier reopens the committed subject and derives the
predicate itself; the operator cannot supply an unbound byte offset, hash, or
verdict witness.

Transaction-global reasons carry no fake coordinate. `ValueNotPreserved` is
transaction-global; accepted-invalid proofs can select an authenticated unbalanced asset, while
wrongful rejection requires the registered `valueNotPreserved` family to prove
universal conservation across the complete authenticated asset domain.

## 4. Rejection hash

The reason itself is the rejection preimage: the machine's terminal rejection
binds `hash_rejection_code(reason)` in
`onchain/aiken/lib/midgard/validation-trace-v1.ak`, the domain-separated
blake2b-256 of the reason's Plutus Data CBOR. There is no coarse code.

## 5. Interaction boundary

Deterministic statements over retained authenticated data are single-party,
even when they require a multi-transaction computation thread. Native-script
decoding, value preservation, input uniqueness, mint authorization, and other
registered families follow this rule.

`PlutusExecutionFailed` remains on the interactive CEK/validation-dispute path
because resolving it requires authenticated execution traces. Static selection
or malformed-data facts about Plutus material have their own reasons and do
not inherit that interactive classification. One case needs no trace: a forced
`PlutusExecutionFailed` rejection of a transaction whose redeemer field 8 has
an authenticated count of zero runs no Plutus script, so `redeemerCanonicity`
(`00000028`) contradicts it single-party. That family therefore answers two
wrongful forced rejections: `RedeemerMalformed`, and `PlutusExecutionFailed` of
a transaction with no redeemers. A forced `PlutusExecutionFailed` of a
transaction with redeemers stays unanswered until forced leaves are bound to
their execution order.

A wrongful forced rejection whose reason names a coordinate the transaction
does not have (a negative ordinal, one at or past the authenticated item count
of the field the reason names, or a source or item kind outside the reason's
domain) is contradicted by the family that owns the reason, from the
authenticated count alone.

`MintItemMalformed` has no emission site in the validation machine, so
`mintItemNonCanonical` (`00000036`) contradicts every forced rejection carrying
it without opening the mint field. That branch is sound only while no machine
step emits the reason: the change that adds an emission site must, in the same
change, replace the branch with one that proves the claimed coordinate absent
or the item well-formed.

## 6. Verification obligations

- TypeScript and Aiken encodings must remain byte-identical.
- Subject indices and source/purpose kinds must be range-checked before use.
- A reason against an honest accepted/rejected transaction must fail at the
  decisive on-chain predicate.
- Any compiler, schema, constructor-order, or validator change must regenerate
  the deployment identity and rerun the ABI/semantic resolver tests.
