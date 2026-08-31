# Canonical-decodability fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `canonicalDecodability` (`00000011`). Generic Init, deployment
inspection/identity, and both mandatory reference scripts are wired.
Family-specific CLI verbs, autonomous watcher actuation, and live/preprod
evidence remain open.

## Fault statement

The family proves that a committed native-transaction field has the wrong
canonical-decodability verdict. It authenticates the exact field preimage and
compares its actual envelope verdict with the operator's accepted/rejected
claim. This covers both an accepted undecodable field and a rejected decodable
field; malformed evidence or a matching verdict cannot convict.

## On-chain chain

The two-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/canonical-decodability/`
- `onchain/aiken/lib/midgard/fraud-proofs/canonical-decodability/`

Step 01 binds the challenged transaction through the shared counted
`transactions_root` path, opens the named body or witness field through the
authenticated field-access door, and carries the recomputed verdict forward.
Step 02 accepts only a canonical-decodability mismatch, burns the computation
thread, and mints the permanent fraud-proof token. Cancellation is explicit.

## Off-chain surfaces

- SDK schema:
  `demo/midgard-sdk/src/fraud-proof/canonical-decodability-v1.ts`
- family package:
  `demo/midgard-fault-proofs/src/canonical-decodability/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

Preparation authenticates the compact transaction and field preimage before
building direct or published carriage. Submitters use authenticated reference
scripts and preserve the on-chain step state exactly.

## Verification status

Focused package tests cover field/envelope preparation. Emulator suites cover
accepted-undecodable and rejected-decodable cases, witness fields, adversarial
evidence, cancellation/resume, permanent mint, and faulty-block removal.

## Remaining work

- expose the family through the operational CLI/workflow surface;
- mount watcher detection and proving;
- publish live/preprod proof-through-removal evidence.
