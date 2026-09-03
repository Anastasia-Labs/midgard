# Fault-Proof Completion Plan

Active work only. Implemented catalogue families and retired historical waves
are intentionally omitted.

## Goal

For every enabled catalogue category, an independent watcher can obtain the
authentic proof inputs, construct and submit the complete proof within L1 limits,
resume after interruption, remove the fraudulent commitment, and verify the
result before block maturity.

## P0 — Make every lifecycle fit Van Rossem limits

The standalone Lucid Evolution journeys for `missingNativeScriptUtxo`,
`nativeScriptInvalid`, and both `minAda` polarities exist and pass under the
shared limits. Fabricated deposit/withdrawal removal and value/mint
cancellation/resume are also covered.

The shared state-queue publication blocker is resolved. `InitV1` and `Deinit`
remain in the minting policy; commit, unattested-timeout removal,
unavailable-timeout removal, fraud removal, and merge dispatch to five
authenticated rewarding scripts through exact zero withdrawals. The applied
minting policy is 5,222 bytes and publishes in a 5,498-byte signed transaction.
The rewarding scripts are 5,652–8,347 bytes and publish in signed transactions
of 6,161–8,842 bytes, all below the 16,384-byte limit.

The former min-ADA blocker is resolved. Step 02 is a 3,319-byte authenticated
dispatcher (`onchain/aiken/validators/fraud-proofs/min-ada/step-02.ak`) whose
transaction and UTxO branches are separate withdraw-zero rewarding validators
(`step-02-yields.ak`, 15,522 and 6,571 bytes in the generated blueprint) with
their own reference-script role NFTs, and the standalone suite asserts that all
three signed publications fit within 16,384 bytes. The native-script-invalid
29-signer and 33-signer frontiers pass through the deterministic staged route
with Van Rossem headroom, and both the direct and the staged
missing-native-script-UTxO paths submit.

Remaining P0 work:

- Split or redesign the 51 compiled scripts whose raw bodies exceed the
  16,384-byte publication limit: 47 `validationTraceDispute` resolver bodies
  (CEK step, script-sources, resolve-inputs, phase-A, and value-and-mint
  semantics), the two `transitionTrace` finals, `withdrawalMistag` step 03,
  and the availability challenge. Their Lucid lifecycles currently pass only
  through the harness's raised-limit oversized publication path. One plan
  per script, with the shared primer, is indexed in
  [size-plans/README.md](size-plans/README.md).
- Repair the fixture drift that keeps `inspect-contracts.test.ts` and
  `submit-init-emulator-min-ada-v1.test.ts` red after the reference-script
  role-NFT change, so the catalogue-root pin and the validation-dispute
  min-ADA journey are verified again.
- Drive every other category's maximum supported shape through the same
  limits (P2); the three final families are the only ones with explicit
  maximum-frontier assertions today.

Completion: every catalogue lifecycle passes using the shared Van Rossem
limits, including its maximum supported shape, and finishes with the target
header absent and the permanent proof token retained.

## P1 — Production watcher coverage

Install manifest-bound runners for the seven categories absent from
`WATCHER_INSTALLED_WORKFLOW_CATEGORIES`:

- `transitionTrace`
- `validationTraceDispute`
- `nativeScriptDecoding`
- `withdrawalMistag`
- `crossBlockDuplicateEvent`
- `valueNotPreserved`
- `mintAuthorization`

Each installation must use public retained-DA/L1 authorities, the exact
deployment manifest, durable action-specific funding reservations, restart-
safe journals, and final-state reconciliation.

Completion: the watcher application requires and installs all enabled launch
categories and fails readiness if any runner or reference-script identity is
missing.

## P2 — Van Rossem emulator closure

Lucid Evolution lifecycle tests are the transaction-fit acceptance surface. The
shared emulator harness must use Van Rossem's complete-transaction limits:
`maxTxSize = 16,384`, transaction memory `16,500,000`, and transaction CPU
`10,000,000,000`. Tests may not raise those limits for a positive lifecycle.

For every category, submit the complete init, proof, cancel/resume where
applicable, permanent-token, and removal journey using the real testnet
blueprint. Maximum supported inputs, outputs, assets, signatures,
native-script nodes/depth, and field-preimage shapes must pass through those
same transactions. If a direct transaction does not fit, use the protocol's
deterministic staged route; do not relax the emulator.

Completion: all catalogue lifecycle tests pass with the shared Van Rossem
limits and no category-specific size or ExUnit override.

## P3 — Correction and economics

- Set non-zero deployment values for bonds, penalties, prover rewards, and
  inactivity penalties.
- Enforce duplicate-claim/idempotency behavior under concurrent challengers.
- Run target and descendant removal across operator rotation against a real
  node.
- Verify transaction and event re-inclusion, lease fencing, retry, rollback,
  and final balance conservation.

Completion: one command corrects any faulty queue position, pays the authenticated
prover exactly once, and leaves the node and L1 views reconciled.

## P4 — DA and public challenger surface

- Expose stable retained payload, proof artifact, membership witness, field
  opening, and verifier-version schemas to an unprivileged challenger.
- Specify and accept the remedy for data withheld after attestation. The
  implemented availability-challenge validator applies one 20,017-byte script
  (19,927-byte raw blueprint body) to both its spending and minting roles, so
  it cannot be published under the 16,384-byte limit; an authenticated
  split/withdraw-zero redesign, new manifest roles, and matching builder ABI
  changes are required before this remedy exists on chain.
- Keep all proof evidence available for at least maturity plus execution/retry
  margin.
- Publish watcher and manual recovery runbooks.

Completion: a challenger with no node database access can reproduce every
proof input and complete a supported challenge.

## P5 — Acceptance

1. Run watcher-driven detect → prove → remove across independent local
   processes and real sockets.
2. Repeat at least one representative family from each proof-input shape on
   preprod.
3. Run the same blueprint and deployment identity accepted by the Van Rossem-
   limited Lucid Evolution suite.
4. Make the acceptance run a required release gate.

## Definition of done

- 32/32 catalogue categories have complete local emulator lifecycle coverage or an
  explicitly approved launch exclusion.
- 32/32 enabled categories are installed in the production watcher.
- Every family has positive, valid-block negative, resume/cancel, and
  correction emulator coverage appropriate to its shape under Van Rossem
  transaction limits.
- Non-zero economics and idempotency pass.
- Public retained proof inputs survive the challenge window.
- Real-node and preprod acceptance artifacts are reproducible from a clean
  checkout.
