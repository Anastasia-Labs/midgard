# Midgard Fault-Proof System

Status: Active

Last reviewed: 2026-09-12 (source and documentation reconciliation).

## Implementation and identity

The SDK catalogue and watcher application cover the same source category set.
The [catalogue status](catalogue-status.md) owns the checked inventory and points
to the exact category-ID and installation authorities. Aiken validators, SDK
parameter application, manifest-bound workflows, and emulator scenarios exist
across that set; their presence is not a deployment or release acceptance claim.

The blueprint is generated, not checked in. Build it with the pinned compiler
and explicit `testnet` environment for demo/preprod/e2e. Bind acceptance to its
hash and the applied deployment manifest rather than a hash copied into prose.
Catalogue identity is immutable after genesis; changing it requires a fresh
development deployment.

## Readiness boundary

Release completion still requires reproducible acceptance of publication and
maximum-shape lifecycles, independent retained proof-data retrieval, durable
recovery, economics, real-node execution, and preprod challenges. The availability
challenge publication work is tracked in its [size plan](size-plans/availability-challenge.md).
[Public-testnet readiness](../public_testnet_readiness.md) owns the launch decision.

## Document map

| Document                                                   | Authority                                                                   |
| ---------------------------------------------------------- | --------------------------------------------------------------------------- |
| [`catalogue-status.md`](catalogue-status.md)               | Source category/installation inventory and acceptance boundaries            |
| [`remaining-gaps.md`](remaining-gaps.md)                   | Open non-interactive protocol/category gaps and required fixes              |
| [`event-history-design.md`](event-history-design.md)       | Proposed NIFP-01–03 architecture, lifecycle, alternatives, and acceptance   |
| [`coverage-matrix.md`](coverage-matrix.md)                 | Ledger-rule coverage and remaining proof/release gaps                       |
| [`architecture.md`](architecture.md)                       | Catalogue, computation-thread, token, removal, DA, and watcher architecture |
| [`onchain-reference.md`](onchain-reference.md)             | Aiken module map and compiled identity                                      |
| [`offchain-reference.md`](offchain-reference.md)           | SDK, fault-proof package, workflow runtime, watcher, and node map           |
| [`testing-status.md`](testing-status.md)                   | Test fidelity, current gaps, and exact commands                             |
| [`execution-plan.md`](execution-plan.md)                   | Remaining work only                                                         |
| [`challenger-runbook.md`](challenger-runbook.md)           | Supported autonomous challenger procedure and fail-closed boundaries        |
| [`manual-recovery-runbook.md`](manual-recovery-runbook.md) | Recovery, rollback, and escalation procedure                                |

Delivered family and size plans have been consolidated into the
[family semantics reference](family-reference.md), [architecture decisions](decisions/README.md),
and [fit evidence index](size-plans/README.md). Only unfinished acceptance and
availability work remains a plan.

## Maintenance rule

Update the catalogue status, coverage matrix, testing status, execution plan,
and public-testnet readiness checklist in the same change whenever a category,
runner installation, emulator lifecycle, or release gate changes. Derive
counts, IDs, blueprint identity, and catalogue root from code and generated
artifacts; never carry them forward from older documentation.
