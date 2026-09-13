# Fault-Proof Completion Plan

Status: Active

Last reviewed: 2026-09-07 (remaining acceptance scope).

Boundary: complete publication and independent-challenger acceptance for the
current catalogue. Existing split/yield implementations and runner installation
are not pending implementation tasks. This plan does not authorize protocol
shortcuts or treat source inventory as passing release evidence.

## Goal

For every enabled catalogue category, an independent watcher can obtain the
authentic proof inputs, construct and submit the complete proof within L1 limits,
resume after interruption, remove the fraudulent commitment, and verify the
result before block maturity.

## P0 — Publication closure

Complete the [availability-challenge publication work](size-plans/availability-challenge.md).
Closure requires a current pinned build, applied scripts, complete signed
publication transactions, and matching lifecycle scenarios under L1 limits.

For the split validation/transition resolvers, use the
[installed validation-dispute workflow](validation-trace-dispute-installed-workflow.md)
and [transition replay](transition-trace-installed-replay.md). Run their publication and lifecycle gates on the release identity.

## P1 — Preserve complete watcher coverage

The watcher source currently installs the complete catalogue. Keep that invariant
as categories change, and verify deployment-bound admission, public retained-DA/L1
authorities, action-specific funding, durable recovery, and reconciliation in
acceptance. A source installation count is not evidence of a live deployment.

Completion: every enabled category is admitted with the release manifest and
reference scripts; startup fails closed for missing identity or readiness.

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

Include complete deterministic reason classification in both applicable
directions, exact coordinates and honest refusal, maximum field/certificate,
descriptor/MPF, native signer/evaluator, and CEK/ScriptSources/ResolveInputs
continuation frontiers. Publication-only and atomic maximum rows do not replace
registered provenance through mint/removal. Cancellation and recovery must cover
each reachable continuation, including union folds and same-block-created inputs.
The [fit evidence index](size-plans/README.md) identifies family-specific producers
and verifiers; retain their required shape sets when regenerating evidence.

Completion: all catalogue lifecycle tests pass with the shared Van Rossem
limits and no category-specific size or ExUnit override. Complete the frozen-tree classification sweep and independent review below.

## P3 — Correction and economics

- Reconcile the technical specification's DOUBLE-WITHDRAW formula with the
  implemented requirement that both withdrawal leaves are payable; see the
  [family reference](family-reference.md#double-withdraw-fault).

- Verify the configured non-zero bonds, penalties, prover rewards, and
  inactivity penalties against the release manifest and balance-conservation tests.
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
- Accept the remedy for data withheld after attestation, including publication
  of the availability-challenge scripts and matching deployed manifest roles.
  Use P0 for publication acceptance.

- Keep all proof evidence available for at least maturity plus execution/retry
  margin.
- Exercise the watcher and manual recovery runbooks during independent acceptance.

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

- Every enabled catalogue category has complete local emulator lifecycle coverage.
- Every enabled category is admitted in the production watcher on the release identity.
- Every family has positive, valid-block negative, resume/cancel, and
  correction emulator coverage appropriate to its shape under Van Rossem
  transaction limits.
- Non-zero economics and idempotency pass.
- Public retained proof inputs survive the challenge window.
- Real-node and preprod acceptance artifacts are reproducible from a clean
  checkout.

## Combined verification and independent review

Freeze the intended source revision for combined verification. Rebuild the
normal testnet blueprint, derive the catalogue/manifest/applied-script identity,
and run contract inspection, every family's maximum registered lifecycle,
durable watcher detect → prove → remove journeys, and the complete typed-reason
classification sweep in both applicable directions. Only `PlutusExecutionFailed`
may select the interactive dispute. Reconcile all raised-limit, oversized, and
disabled-evaluation test routes; none establishes a positive release result.

Review the completed on-chain and deployment diff independently after these
gates pass. The original program review boundary is commit
`815b703a99c26161cb735ab2f298bf0cbce4524d`; record the frozen final commit and
verify that the chosen range covers every intended change. Review exact
source/reason/coordinate binding, decisive predicates, honest non-convictability,
field/item/MPF/checkpoint authentication, unique successors, cancellation,
permanent mint and removal, malformed-byte behavior, substitution/replay, ABI
parity, and complete signed byte/ExUnit evidence. Include shared libraries and
parameter application, not only family validator files.

Resolve findings and verify their fixes within the review. Rerun affected
focused checks and combined closure after remediation. Completion requires no
unresolved correctness, soundness, publishability, or evidence finding; source
installation counts and historical green snapshots cannot close this gate.
