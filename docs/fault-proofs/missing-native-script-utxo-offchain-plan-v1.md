# Missing-native-script UTxO fault — implementation reference

Current status: implemented and registered as `missingNativeScriptUtxo`
(`0000001d`). Seven mandatory reference scripts, retained-DA/history evidence,
prepare/submit/cancel modules, a manifest-bound production runner factory, and
catalogue/deployment identity are wired, and the watcher installs the runner.
A dedicated direct-path Lucid Evolution lifecycle exists and passes the shared
state-queue setup. It currently reaches proof submission and fails on-chain
validation before completing cancellation/resume, permanent mint, and removal.

## Fault statement

The family proves that an operator-accepted transaction spends a predecessor
output whose script credential names a Cardano native script that cannot be
authenticated from the predecessor ledger history/material required by
canonical V1. Forged predecessor roots, keys, credentials, or script preimages
cannot convict.

## Implementation

- Aiken validators:
  `onchain/aiken/validators/fraud-proofs/missing-native-script-utxo/`
- off-chain family:
  `demo/midgard-fault-proofs/src/missing-native-script-utxo/`
- production runner factory:
  `createMissingNativeScriptUtxoProductionWorkflowRunnerV1`
- Aiken tests: `staged-v1.test.ak`
- retained-history/evidence tests:
  `demo/midgard-fault-proofs/tests/native-script-family-evidence-v1.test.ts`

The seven-step path binds the transaction and selected input, authenticates
predecessor UTxO membership and the native credential, proves the relevant
script material, and uses bounded staged grammar/semantic evaluation when the
direct path cannot fit.

## Remaining work

- diagnose and correct the current direct-path proof-submission validation
  failure;
- extend the real-blueprint lifecycle to cover the staged maximum path and
  valid-block refusal; the current direct path covers cancellation/resume,
  permanent mint, and removal after the submission failure is corrected;
- keep the complete lifecycle green under the shared Van Rossem emulator
  limits and run the corresponding preprod acceptance.
