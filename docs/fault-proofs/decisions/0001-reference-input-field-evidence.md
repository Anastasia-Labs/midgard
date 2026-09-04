# 0001 — Authenticated field-preimage carriage

- Status: Implemented in canonical V1
- Scope: fault-proof openings of the nine committed native-transaction fields

## Decision

A fault-proof step opens a transaction field through one shared authenticated
door. The transaction id anchors body fields 0–5; the transaction's committed
`witness_set_hash` anchors witness fields 6–8. The opening must match the
positional field commitment derived from those anchored structures.

Carriage is selected deterministically from the complete §5.1 field-preimage
length:

| Tier      |      Preimage length | Carriage                                                        |
| --------- | -------------------: | --------------------------------------------------------------- |
| Inline    | at most 14,336 bytes | bytes in the step transaction                                   |
| Raw UTxO  |  14,337–15,148 bytes | one permissionless publication UTxO                             |
| Certified |  15,149–32,768 bytes | up to three deterministic 15,148-byte chunks plus a certificate |

The 14,336-byte inline boundary remains a provisional execution-envelope
bound and therefore stays release-gating. The 15,148-byte publication frontier
is measured to leave the protocol's 512-byte reliability reserve under a
16,384-byte `maxTxSize`. The aggregate field bound is 32,768 bytes.

Publication and certification are permissionless. Raw publications need no
special validator: the consuming door authenticates their exact datum bytes.
Certified carriage uses the single field-preimage certificate policy; its
datum contains the mint-welded field hash and ordered chunk commitments. A
family-specific datum, caller-selected tier, off-chain hash assertion, or
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
