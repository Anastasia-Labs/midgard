# Withdrawn-reference-input fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `withdrawnReferenceInput` (`00000010`). Generic Init, deployment
inspection/identity, and all three mandatory reference scripts are wired.
Autonomous watcher actuation and live/preprod evidence remain open.

## Fault statement

The family proves that an operator-accepted transaction references an L2 output
which a payable withdrawal committed by the same block consumes. The
transaction and withdrawal memberships are authenticated against the same
challenged header. An invalid/non-payable withdrawal or a different output
reference cannot convict.

Actual spending of the withdrawn output is handled by `withdrawnInput`; a
reference input absent from the ledger is handled by
`referenceInputNoIdx`/the input-validity machinery.

## On-chain chain

The three-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/withdrawn-reference-input/`
- `onchain/aiken/lib/midgard/fraud-proofs/withdrawn-reference-input/`

The chain binds the accepted transaction, opens the selected reference input,
and authenticates a payable withdrawal under the same header's counted
withdrawal root with the same L2 output reference. The terminal step burns the
computation thread and mints the permanent fraud-proof token. Cancellation is
explicit.

## Off-chain surfaces

- SDK schema:
  `demo/midgard-sdk/src/fraud-proof/withdrawn-reference-input-v1.ts`
- family package:
  `demo/midgard-fault-proofs/src/withdrawn-reference-input/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

## Verification status

Focused preparation tests and emulator suites cover conviction/removal, honest
and invalid-withdrawal refusals, adversarial evidence, and published carriage.

## Remaining work

- mount watcher detection and proving;
- publish live/preprod proof-through-removal evidence.
