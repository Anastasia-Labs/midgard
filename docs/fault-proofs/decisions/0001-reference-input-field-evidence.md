# 0001 — Authenticated field-preimage carriage

- Status: Implemented in canonical V1
- Scope: fault-proof openings of the nine committed native-transaction fields

## Decision

A fault-proof step opens a transaction field through one shared authenticated
door. The transaction id anchors body fields 0–5; the transaction's committed
`witness_set_hash` anchors witness fields 6–8. The opening must match the
positional field commitment derived from those anchored structures.

The shared planner initially selects carriage from the complete §5.1
field-preimage length using these thresholds:

| Tier      |      Preimage length | Carriage                                                        |
| --------- | -------------------: | --------------------------------------------------------------- |
| Inline    | at most 14,336 bytes | bytes in the step transaction                                   |
| Raw UTxO  |  14,337–15,148 bytes | one permissionless publication UTxO                             |
| Certified |  15,149–32,768 bytes | up to three deterministic 15,148-byte chunks plus a certificate |

The 14,336-byte inline boundary is a planning threshold, not a guarantee that
the complete consuming transaction fits. A builder may need a raw publication
for a smaller field when transaction framing or other evidence consumes its
redeemer budget. Tiers 1 and 2 overlap below `K` at the on-chain door; certified
carriage alone requires a strict lower bound above `K`.
The inline threshold remains release-gating for each consuming shape. The 15,148-byte publication frontier
is measured to leave the protocol's 512-byte reliability reserve under a
16,384-byte `maxTxSize`. The aggregate field bound is 32,768 bytes.

Publication and certification are permissionless. Raw publications need no
special validator: the consuming door authenticates their exact datum bytes.
Certified carriage uses the single field-preimage certificate policy; its
datum contains the mint-welded field hash and ordered chunk commitments. A
family-specific unauthenticated datum, off-chain hash assertion, or
partially applied certificate policy is not an alternative protocol.

## Current authorities

- `docs/spec/midgard-tx.md` §§2.5, 5, and 8
- `onchain/aiken/lib/midgard/fraud-proofs/field-opening-v1.ak`
- `onchain/aiken/lib/midgard/native-tx-field-access-v1.ak`
- `onchain/aiken/validators/field-preimage-certificate.ak`
- `demo/midgard-core/src/codec/native-tx-field-access.ts`
- `demo/midgard-core/src/codec/native-tx-carriage.ts`
- `demo/midgard-sdk/src/fraud-proof/field-preimage-carriage.ts`
- `demo/midgard-fault-proofs/src/field-opening.ts`

These sources own the live wire shapes, bounds, transaction layout, and
authentication rules. This decision intentionally carries no network epoch,
branch, commit, script hash, or catalogue-root snapshot.

## Release obligations

- Measure every consuming step at its worst admissible field and carriage tier
  against the compiled validator and deployment protocol parameters.
- Keep publication, certification, reference-input ordering, and on-chain
  decoding twins covered by exact-boundary and adversarial tests.
- Keep every proof path bounded and comfortably inside the challenge window.
- Regenerate the blueprint and deployment identity after any validator,
  parameter, field schema, or carriage-rule change.
- Fail closed until the inline boundary and all family-specific worst cases
  have release evidence.

## Builder contract

Carry the transaction anchor required by the next step and use the shared
`FieldOpening` schema rather than a caller-asserted hash or legacy list. Apply
all blueprint-declared parameters in their declared order, including the
certificate policy where required, and derive the deployed hash/address from
the fully applied script.

TypeScript schemas are handwritten protocol twins. Typechecking alone cannot
establish Aiken ABI parity: round-trip schemas and run the real validator's
happy-path and refusal emulator scenarios. Reapply parameters and regenerate
identity together when the schema or parameter list changes. A transaction id
and a field commitment may share a primitive type but are not interchangeable.
