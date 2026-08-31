# Committed-field-shape fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `committedFieldShape` (`00000012`). Generic Init, deployment
inspection/identity, and both mandatory reference scripts are wired.
Family-specific CLI verbs, autonomous watcher actuation, and live/preprod
evidence remain open.

## Fault statement

The family proves that an authenticated committed field violates the canonical
fixed-stride, item-count, or aggregate-size shape required by the native-V1
transaction format. It operates on the exact committed field bytes and does
not accept caller-asserted lengths or verdicts.

## On-chain chain

The two-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/committed-field-shape/`
- `onchain/aiken/lib/midgard/fraud-proofs/committed-field-shape/`

Step 01 binds the challenged transaction through the shared counted
`transactions_root` path, opens the selected body or witness field through the
authenticated field-access door, and recomputes its shape verdict. Step 02
accepts only a non-zero violation verdict, burns the computation thread, and
mints the permanent fraud-proof token. Cancellation is explicit.

## Off-chain surfaces

- SDK schema: `demo/midgard-sdk/src/fraud-proof/committed-field-shape-v1.ts`
- family package: `demo/midgard-fault-proofs/src/committed-field-shape/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

Preparation derives the field claim from canonical evidence. Submitters use
the shared direct/published carriage machinery and authenticated reference
scripts.

## Verification status

Focused tests cover strict schema and shape classification. Emulator suites
cover conviction/removal, an honest-field refusal, adversarial carriage, and
the supported field-carriage paths.

## Remaining work

- expose the family through the operational CLI/workflow surface;
- mount DA-first watcher detection and proving;
- publish live/preprod proof-through-removal evidence.
