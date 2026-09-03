# Withdrawal-mistag fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `withdrawalMistag` (`00000014`). Generic Init, deployment
inspection/identity, and all five mandatory reference scripts are wired.
Autonomous watcher actuation and live/preprod evidence remain open.

## Fault statement

The family proves that a committed withdrawal's payable/refund tag disagrees
with the validity recomputed from authenticated evidence:

```text
claimed_valid != actual_valid
```

It covers both a valid withdrawal marked invalid and an invalid withdrawal
marked valid, including owner, value, signature, native-asset, and exact payout
feasibility checks. It does not convict merely because a different invalid
reason label could also apply; all invalid labels select the same refund/no-op
semantics.

This is a standalone single-party family. Transition trace proves consistency
with the committed tag, not the truth of the tag itself.

## On-chain chain

The five-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/withdrawal-mistag/`
- `onchain/aiken/lib/midgard/fraud-proofs/withdrawal-mistag/`

The chain authenticates the source withdrawal, transition coordinate,
pre-state UTxO evidence, signed withdrawal body, and payout conditions before
comparing the claimed and actual validity. The terminal step burns the
computation thread and mints the permanent fraud-proof token. Cancellation is
explicit at every step.

## Off-chain surfaces

- SDK schema: `demo/midgard-sdk/src/fraud-proof/withdrawal-mistag.ts`
- family package: `demo/midgard-fault-proofs/src/withdrawal-mistag/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

## Verification status

Focused tests cover validity recomputation and strict evidence preparation.
Emulator suites cover both fault directions through permanent mint and removal,
plus cancellation/resume and honest/refusal boundaries.

## Remaining work

- mount watcher detection and proving;
- publish live/preprod proof-through-removal evidence.
