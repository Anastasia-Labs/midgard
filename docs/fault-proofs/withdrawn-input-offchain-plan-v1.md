# Withdrawn-input fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `withdrawnInput` (`00000018`). Generic Init, deployment
inspection/identity, all three mandatory reference scripts, a production
runner factory, and watcher installation are wired. Live/preprod acceptance
remains open.

## Fault statement

The family proves that an operator-accepted transaction spends an L2 output
which a payable withdrawal committed by the same block already consumes. The
transaction and withdrawal memberships are authenticated against the same
challenged header. An invalid/non-payable withdrawal cannot convict.

This is distinct from an input absent from the prior UTxO state (`no-input`), a
cross-transaction in-block double spend (`double-spend`), an intra-transaction
duplicate input (`inputSetUniqueness`), and use as a reference input
(`withdrawnReferenceInput`).

## On-chain chain

The three-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/withdrawn-input/`
- `onchain/aiken/lib/midgard/fraud-proofs/withdrawn-input/`

The chain binds the accepted transaction, opens the selected spend input, and
authenticates a payable withdrawal under the same header's counted withdrawal
root with the same L2 output reference. The terminal step burns the computation
thread and mints the permanent fraud-proof token. Cancellation is explicit.

## Off-chain surfaces

- SDK schema: `demo/midgard-sdk/src/fraud-proof/withdrawn-input-v1.ts`
- family package: `demo/midgard-fault-proofs/src/withdrawn-input/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

## Verification status

Emulator suites cover conviction/removal, honest refusal, invalid-withdrawal
refusal, cancellation/resume, and published carriage.

## Remaining work

- accept the installed watcher detection/proving path end to end;
- publish live/preprod proof-through-removal evidence.
