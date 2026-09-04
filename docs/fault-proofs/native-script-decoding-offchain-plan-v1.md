# Native-script-decoding fault — off-chain implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `nativeScriptDecoding` (`0000000d`). The on-chain security contract
is documented in
[`native-script-decoding-fault-thread-design-v1.md`](native-script-decoding-fault-thread-design-v1.md).

## Off-chain surfaces

- SDK and deployment builders:
  `demo/midgard-sdk/src/fraud-proof/`
- family package:
  `demo/midgard-fault-proofs/src/native-script-decoding/`
- canonical category order:
  `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

The family package contains strict finding/evidence codecs, scan planning, a
consumer-independent proving core, Init/step/cancel submitters, contract
application, and adapters. All physical validators are consumed as
authenticated reference scripts.

## Workflow

1. Detect an accepted-undecodable or rejected-decodable claim from canonical
   retained evidence.
2. Bind the exact transaction/source and script-bearing subject.
3. Build direct or published field carriage without changing the authenticated
   bytes.
4. Submit Init and steps 01/02.
5. Drive the resumable step-03 scan until it closes.
6. Submit step 04 to burn the computation thread and mint the permanent proof.
7. Invoke the shared faulty-block removal workflow.

The proving core persists enough state for deterministic resume and never
cancels implicitly. Cancellation is an explicit prover action.

## Verification

Focused package tests cover evidence/finding codecs, scan planning, and
envelope selection. Emulator suites cover both claim directions, adversarial
polarity, malformed/substituted evidence, resume/cancel behavior, permanent
mint, and faulty-block removal:

- `submit-init-emulator-native-script-decoding-direction-a.test.ts`
- `submit-init-emulator-native-script-decoding-direction-b.test.ts`
- `submit-init-emulator-native-script-decoding-adversarial.test.ts`
- `submit-init-emulator-native-script-decoding-negatives.test.ts`
- `submit-init-emulator-native-script-decoding-tier2.test.ts`

## Remaining work

- add family-specific CLI/workflow verbs;
- mount the detector/prover in the watcher;
- publish live/preprod proof-through-removal evidence;
- refresh blueprint hashes and execution ledgers whenever the compiler,
  validator source, field limits, or target protocol parameters change.
