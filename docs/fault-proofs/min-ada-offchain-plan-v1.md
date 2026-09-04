# Minimum-Ada fault — implementation reference

Current status: implemented and registered as `minAda` (`0000001f`). Five
mandatory reference scripts, strict production artifacts, transaction and
post-UTxO preparation, submit/cancel modules, a manifest-bound production
runner factory, and catalogue/deployment identity are wired. The watcher
installs the runner. Standalone transaction-output and post-UTxO Lucid
Evolution lifecycles exist and now pass shared state-queue setup. Both currently
stop at publication of the family-specific 28,658-byte
`fraudProofMinAdaStep02` script; its signed publication transaction is 28,727
bytes against the 16,384-byte Van Rossem limit.

## Fault statement

The family proves either that an accepted transaction creates an output below
the canonical minimum-Ada floor or that a newly introduced post-state UTxO is
below that floor. Exact-floor and above-floor outputs do not convict, and the
post-state route rejects inherited underfunding and forged predecessor proofs.

## Implementation

- Aiken validators: `onchain/aiken/validators/fraud-proofs/min-ada/`
- off-chain family: `demo/midgard-fault-proofs/src/min-ada/`
- production runner factory: `createMinAdaWorkflowRunner`
- Aiken tests: `family-v1.test.ak`

The family uses the same canonical minimum-Ada formula and production parameter
snapshot as the validation machine. Its five steps authenticate transaction or
post-UTxO evidence, apply exact field/descriptor carriage, prove predecessor
non-membership where required, and finalize only an adjudicated violation.

The existing `submit-init-emulator-min-ada.test.ts` tests the interactive
`validationTraceDispute` route to `E_MIN_ADA`; it does not test this standalone
category.

## Remaining work

- split or reduce `fraudProofMinAdaStep02` so it can be published under the Van
  Rossem limit;
- extend the lifecycles with exact-floor negatives and maximum carriage where
  those shapes are not already exercised;
- keep both complete lifecycles green under the shared Van Rossem emulator
  limits and run the corresponding preprod acceptance.
