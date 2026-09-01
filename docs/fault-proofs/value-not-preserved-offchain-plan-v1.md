# Value-not-preserved fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `valueNotPreserved` (`00000019`). Generic Init, deployment
inspection/identity, and all four mandatory reference scripts are wired.
Typed family modules exist; family-specific CLI verbs, autonomous watcher
actuation, and live/preprod evidence remain open.

## Fault statement

An operator-accepted committed transaction fails value preservation for at
least one asset. The family uses a bounded single-asset claim: the prover names
one asset and the imbalance direction, and the chain verifies only that asset's
equation. The prover finds an unbalanced asset off-chain.

- ADA: `sum(inputs) - sum(outputs) - fee == 0`
- non-ADA asset `u`: `sum(inputs_u) + mint_u - sum(outputs_u) == 0`

The claim convicts only when the final delta is non-zero and its sign matches
the claimed inflation/deflation direction. ADA minting and negative output
quantities are structurally unrepresentable in canonical V1 and are covered by
the Q24/Q25 executable N/A controls.

## On-chain chain

The four-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/value-not-preserved/`
- `onchain/aiken/lib/midgard/fraud-proofs/value-not-preserved/`

1. Bind the operator-accepted native transaction through the authenticated
   counted `transactions_root`; validate the asset/direction claim.
2. Fold each spend input through authenticated ledger membership, accumulating
   the claimed asset.
3. Fold outputs and mint entries, and subtract the fee for an ADA claim.
4. Prove the final signed imbalance, burn the thread token, and mint the
   permanent fault-proof token.

Every step supports explicit prover cancellation. The state carries the
transaction identity, claim, cursors, prior ledger root, and accumulated delta;
later redeemers cannot substitute those values.

## Off-chain surfaces

- wire schemas:
  `demo/midgard-fault-proofs/src/value-not-preserved/schemas-v1.ts`
- family implementation: `demo/midgard-fault-proofs/src/value-not-preserved/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

The package provides evidence/finding records, a proving core, Init/step/cancel
submitters, and direct/published carriage support. Registration is part of the
canonical deployment identity.

## Verification status

Focused Aiken selectors cover transaction binding, asset claim shape, input
membership, output/mint folds, ADA fee treatment, inequality direction, and
honest-transaction refusal. Emulator suites cover ADA and token convictions,
both imbalance directions, adversarial carriage mutation, permanent mint, and
faulty-block removal:

- `submit-init-emulator-value-not-preserved-ada.test.ts`
- `submit-init-emulator-value-not-preserved-token.test.ts`
- `submit-init-emulator-value-not-preserved-adversarial.test.ts`
- `submit-init-emulator-value-not-preserved-negatives.test.ts`

## Remaining work

- exercise the cancel submitter in the emulator;
- close or explicitly route same-block-created spend inputs if the current
  prior-ledger-only fold cannot authenticate them;
- expose operational CLI/workflow commands;
- mount watcher detection/proving and publish preprod/live evidence;
- rerun maximum-shape lifecycles under the shared Van Rossem emulator limits
  whenever compiler, blueprint, field bounds, or Cardano protocol limits
  change.
