# Keep watcher verification independent and recoverable

Status: Accepted

Last reviewed: 2026-09-07 (implemented boundaries; not deployment acceptance).

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

The production chain authority is the configured local Cardano node, admitted
through native chain sync and cross-checked against local Kupo/Ogmios at the
same chain point with exact transaction-byte agreement. A pathname or endpoint
alone is not authenticated authority. Historical native-script retrieval is a
separate external-provider quorum with exact-byte agreement across independently
identified providers; it does not replace the local chain authority or provide
actuation failover. Accepted L1 transaction bytes are indexed deterministically;
the watcher does not become a second implementation of Cardano's validator rules.

Durable replay needs both integrity and freshness. Authenticate recovery state
with a stable external key, commit state/revision atomically, and publish through
compare-and-swap to an independently protected monotonic trusted head. A digest
stored beside rollbackable data cannot prove freshness. Startup and recovery
must reject mismatched state/head evidence before emitting actionable results.
Transient provider disagreement quarantines the observation; an authenticated
canonical replacement uses the bounded rollback/replay path.

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
- [Trusted-head authority](../../../demo/midgard-watcher/src/runtime/trusted-head-authority.ts)
  and [rollback engine](../../../demo/midgard-watcher/src/l1/rollback-engine.ts).
- [Workflow installation](../../../demo/midgard-watcher/src/fault-proofs/fault-proof-application.ts).
- [Slash/reward checks](../../../onchain/aiken/lib/midgard/operator-directory.ak).
