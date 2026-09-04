# L2-transaction-mistag fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `l2TxMistag` (`00000017`). Generic Init, deployment
inspection/identity, both mandatory reference scripts, a production runner
factory, and watcher installation are wired. Typed family modules exist;
family-specific CLI verbs and live/preprod acceptance remain open.

## Fault statement

A normal transaction under `transactions_root` is an acceptance verdict in
canonical V1. Its compact `validity_code` must be zero. A non-zero value marks
the transaction as an invalid no-op and censors its state transition; the
committed scalar itself is the fault.

This is distinct from forced transactions. Forced leaves carry an explicit
operator verdict and use the validation-dispute/transition paths to prove
whether the transaction should execute or remain a no-op. D-S9 is resolved for
canonical V1: valid forced transactions apply the authenticated accepted ledger
delta, invalid forced transactions are exact no-ops, and either wrong verdict
is challengeable.

## On-chain chain

The two-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/l2-tx-mistag/`
- `onchain/aiken/lib/midgard/fraud-proofs/l2-tx-mistag/`

Step 01 binds the exact native compact transaction through the authenticated
counted `transactions_root`. Step 02 requires the normal-leaf validity scalar
to be non-zero, burns the computation thread, and mints the permanent
fault-proof token. Both steps support explicit prover cancellation.

The family does not replay full transaction validity: canonical V1 gives normal
leaves no rejection channel. Replaying validity here would duplicate the
validation-dispute machine and create a different claim than the malformed
normal-leaf verdict this family proves.

## Off-chain surfaces

- wire schemas: `demo/midgard-fault-proofs/src/l2-tx-mistag/schemas.ts`
- family implementation: `demo/midgard-fault-proofs/src/l2-tx-mistag/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

The package provides canonical preparation, Init/step/cancel submitters, and
the registered deployment contracts. Registration is part of the immutable
canonical deployment identity.

## Verification status

Aiken and emulator controls cover a non-zero mistag through permanent mint and
faulty-block removal; zero-code honest leaves, substituted scalars, malformed
evidence, cancellation, and resume are refused or handled at the expected
boundary:

- `prepare-l2-tx-mistag.test.ts`
- `submit-init-emulator-l2-tx-mistag.test.ts`
- `submit-init-emulator-l2-tx-mistag-adversarial.test.ts`
- `submit-init-emulator-l2-tx-mistag-negatives.test.ts`

## Remaining work

- expose operational CLI/workflow commands;
- accept the installed watcher detection/proving path end to end;
- publish live/preprod proof-through-removal evidence;
- keep forced-transaction validation-dispute lifecycles green under the shared
  Van Rossem emulator limits.
