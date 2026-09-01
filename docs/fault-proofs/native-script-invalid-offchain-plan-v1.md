# Native-script-invalid fault — implementation reference

Current status: implemented and registered as `nativeScriptInvalid`
(`0000001e`). Five mandatory reference scripts, evidence/submit/cancel modules,
a manifest-bound production runner factory, watcher installation, and
catalogue/deployment identity are wired. The dedicated direct-path Lucid
Evolution lifecycle passes under the shared Van Rossem limits, including
permanent-token minting and faulty-header removal.

## Fault statement

The family proves that an operator accepted a transaction containing a selected
native script that evaluates false under the authenticated validity interval
and address-witness signer set. A satisfied script, a non-native witness, or a
mutated signer/evaluator checkpoint cannot convict.

## Implementation

- Aiken validators:
  `onchain/aiken/validators/fraud-proofs/native-script-invalid/`
- off-chain family:
  `demo/midgard-fault-proofs/src/native-script-invalid/`
- production runner factory:
  `createNativeScriptInvalidProductionWorkflowRunnerV1`
- Aiken tests: `staged-v1.test.ak`
- evidence-machine tests:
  `demo/midgard-fault-proofs/tests/native-script-invalid-evidence-machine-v1.test.ts`

The five-step path binds the transaction and selected script, scans the bounded
signer frontier, carries an authenticated resumable evaluator cursor/stack,
and finalizes only at an evaluation-false terminal state.

## Remaining work

- extend the real-blueprint lifecycle beyond its 28-signer direct path: the
  attempted 29-signer direct and 33-signer staged frontiers exceed the current
  transaction ExUnit budget, and satisfied-script refusal plus cancel/resume
  remain to be exercised there;
- accept the installed watcher runner end to end;
- keep the complete lifecycle green under the shared Van Rossem emulator
  limits and run the corresponding preprod acceptance.
