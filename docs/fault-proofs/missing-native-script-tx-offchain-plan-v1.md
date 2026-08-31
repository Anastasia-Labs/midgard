# Missing-native-script transaction fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `missingNativeScriptTx` (`0000000f`). Generic Init, deployment
inspection/identity, and all six mandatory reference scripts are wired.
Family-specific CLI verbs, autonomous watcher actuation, and live/preprod
evidence remain open.

## Fault statement

The family proves that an operator-accepted transaction spends an output locked
by a Cardano native-script credential while the corresponding native script is
absent from the transaction's authenticated script-witness collection. A
present matching script, a non-native script credential, or unauthenticated
transaction/script bytes cannot convict.

## On-chain chain

The six-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/missing-native-script-tx/`
- `onchain/aiken/lib/midgard/fraud-proofs/missing-native-script-tx/`

The chain binds the challenged transaction, opens the selected spend input,
authenticates its resolved output and credential, classifies the credential as
native-script locked, opens the script-witness field, and proves absence of the
required script hash. The terminal step burns the computation thread and mints
the permanent fraud-proof token. Cancellation is explicit at every step.

## Off-chain surfaces

- SDK schema:
  `demo/midgard-sdk/src/fraud-proof/missing-native-script-tx-v1.ts`
- family package:
  `demo/midgard-fault-proofs/src/missing-native-script-tx/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

Preparation derives the accused input, resolved output, script hash, and field
carriage from canonical evidence. Submitters preserve the exact multi-step
state and consume all validators as authenticated reference scripts.

## Verification status

Focused tests cover evidence encoding and envelope preparation. Emulator suites
cover conviction/removal, present-script refusal, mismatched credentials,
adversarial evidence, and published carriage.

## Remaining work

- expose the family through the operational CLI/workflow surface;
- mount watcher detection and proving;
- publish live/preprod proof-through-removal evidence.
