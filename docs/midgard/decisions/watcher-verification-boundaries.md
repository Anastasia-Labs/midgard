# Keep watcher verification independent and recoverable

Status: Accepted

Last reviewed: 2026-10-08 (L1 follower migration; not deployment acceptance).

## Context

Cardano validates Midgard's L1 transactions but does not re-execute every L2
block. Detecting an incorrect L2 commitment is useful only when the watcher
can obtain the public evidence, construct a supported proof, and finish its
on-chain lifecycle before the applicable deadline. Operator-local databases
and successful single-family demonstrations cannot establish that assurance.

## Decision

The independent watcher owns L2 reconstruction, verification, and challenge
supervision. The DA committee owns retention, retrieval, and attestation.
Neither operator-local SQL/MPF state nor operator admin endpoints are trusted
substitutes for authenticated L1 observations and committee-retained DA.

The production chain authority is the configured local Cardano node, read by
the watcher's L1 follower over native chain sync. Every L1 read is a pure read
of the follower's facts and projections at one view point. The Kupo/Ogmios
cross-check this decision first described was deleted with the follower
migration. A pathname or endpoint alone is not authenticated authority. Historical native-script retrieval is a
separate external-provider quorum with exact-byte agreement across independently
identified providers; it does not replace the local chain authority or provide
actuation failover. Accepted L1 transaction bytes are indexed deterministically;
the watcher does not become a second implementation of Cardano's validator rules.

Durable replay needs integrity. Authenticate the watcher's block progress
with a stable external rollback key and commit state atomically. A rollback
rewinds the follower's facts and recomputes every projection in-process; no
state is quarantined and nothing needs a restart. A rollback deeper than the
security parameter k leaves the watcher up and unready (`rollback_beyond_k`).
The independent trusted-head authority this decision first required was
deleted with the follower migration.

Bind L2 verification to the deployed format, source roots, event placement,
transition trace, and validation material. Retain intermediate evidence needed
by the installed proof family. The phase order is withdrawals, forced
transactions, normal transactions, then deposits; same-block deposit spending
is invalid. See [transition commitments](../../../demo/midgard-node/docs/TRANSITION_TRACE_COMMITMENTS.md).

Challenge execution is a durable workflow through confirmed removal/correction,
including restart, contention, descendant handling, and deadline escalation.
The bond-consuming slash transaction pays the configured prover reward to the
prover named in the proof token and requires that prover's signature. The
residual bond goes to the transaction fee; removing another header after the
operator was slashed does not pay another reward.

## Consequences and acceptance

Runtime readiness is not public launch approval. For every enabled feature,
acceptance must establish deployed proof coverage, rejection of invalid
transitions, non-challengeability of valid transitions, public evidence
retrieval/retention, and reproducible end-to-end execution. Deadline budgets
include retrieval, proof steps, L1 confirmation, retry/congestion, and rollback
margin. Release identity, key custody, economics, and committee accountability
remain explicit parts of the [readiness assessment](../../public_testnet_readiness.md).

Use the [catalogue status](../../fault-proofs/catalogue-status.md) and
[challenger runbook](../../fault-proofs/challenger-runbook.md) for current
installation, evidence, and operating requirements. The retired watcher plan
and review are not acceptance artifacts.

## Implementation anchors

- [Runtime composition](../../../demo/midgard-watcher/src/runtime/watcher-runtime.ts)
  and [configuration](../../../demo/midgard-watcher/src/runtime/config.ts).
- [L1 follower runtime](../../../demo/midgard-watcher/src/l1-follower/follower-runtime.ts);
  it replaced the trusted-head authority and the rollback engine, both deleted.
- [Workflow installation](../../../demo/midgard-watcher/src/fault-proofs/fault-proof-application.ts).
- [Slash/reward checks](../../../onchain/aiken/lib/midgard/operator-directory.ak).
