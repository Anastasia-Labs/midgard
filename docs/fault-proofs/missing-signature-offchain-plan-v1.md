# Missing-signature fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `missingSignature` (`0000000e`). Generic Init, deployment
inspection/identity, all four mandatory reference scripts, a production runner
factory, and watcher installation are wired. Live/preprod acceptance remains
open.

## Fault statement

The family proves that an operator-accepted transaction spends an authenticated
key-credential output without a matching valid transaction signature. It binds
the exact transaction, selected input, resolved address credential, and witness
set. A matching valid signature, a script credential, an invalid transaction
binding, or caller-asserted witness data cannot convict.

## On-chain chain

The four-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/missing-signature/`
- `onchain/aiken/lib/midgard/fraud-proofs/missing-signature/`

The chain binds the challenged transaction, authenticates the selected spend
input and resolved key credential, opens the signature witness field, and
proves that no valid witness authorizes that credential for the transaction id.
The terminal step burns the computation thread and mints the permanent
fraud-proof token. Cancellation is explicit at every step.

## Off-chain surfaces

- SDK schema: `demo/midgard-sdk/src/fraud-proof/missing-signature-v1.ts`
- family package: `demo/midgard-fault-proofs/src/missing-signature/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

The package includes strict finding/evidence codecs, preparation, resumable
proving, submitters, cancellation, and reference-script contract application.

## Verification status

Focused tests cover findings, evidence, and authenticated envelopes. Emulator
suites cover conviction/removal, honest-signature refusal, malformed and
substituted witnesses, cancellation/resume, and other negative boundaries.

## Remaining work

- expose the family through the operational CLI/workflow surface;
- accept the installed watcher detection/proving path end to end;
- publish live/preprod proof-through-removal evidence.
