# Cross-block duplicate-event fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `crossBlockDuplicateEvent` (`00000016`). Generic Init, deployment
inspection/identity, and both mandatory reference scripts are wired.
Autonomous watcher actuation, durable settlement-history evidence, and
live/preprod proof-through-removal remain open.

## Fault statement

The family proves that the same authenticated L1 deposit or withdrawal event
identity was applied by two different L2 blocks. It compares a live challenged
state-queue block with a distinct confirmed settlement block and requires the
same event key in the same counted-root domain.

Two distinct withdrawal ids spending the same L2 output are handled by
`doubleWithdraw`; duplicate still-live events and due-window violations remain
transition-trace concerns. This family does not treat an unauthenticated
off-chain archive as evidence.

## On-chain chain

The two-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/cross-block-duplicate-event/`
- `onchain/aiken/lib/midgard/fraud-proofs/cross-block-duplicate-event/`

Step 01 authenticates the challenged header and its event membership. Step 02
authenticates a different settlement NFT/datum and the same event membership,
then burns the computation thread and mints the permanent fraud-proof token.
The challenged live header remains the removal target. Cancellation is
explicit.

## Off-chain surfaces

- SDK schema:
  `demo/midgard-sdk/src/fraud-proof/cross-block-duplicate-event-v1.ts`
- family package:
  `demo/midgard-fault-proofs/src/cross-block-duplicate-event/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

Preparation and resume support construct authenticated proofs for both root
domains and preserve enough state for deterministic continuation.

## Verification status

Focused tests cover preparation and root-domain binding. The emulator lifecycle
suite proves a duplicate through permanent mint and faulty-block removal while
refusing mismatched event, domain, or historical evidence.

## Remaining work

- retain sufficient confirmed settlement-root history for the full challenge
  horizon;
- mount watcher detection and proving;
- publish live/preprod proof-through-removal evidence.
