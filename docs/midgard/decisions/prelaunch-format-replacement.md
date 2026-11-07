# Replace undeployed formats in place

Status: Accepted; records the delivered canonical consolidation.

Recorded: 2026-09-07. Original decision: 2026-07-24.

## Context

The repository accumulated multiple development generations of transaction,
DA, deployment, and persistence formats before any production version shipped.
Keeping decoders and migrations for those abandoned generations enlarged the
trusted surface without preserving a deployed contract.

## Decision

Keep one interpretation of each undeployed format. An approved prelaunch
semantic change replaces it in place and requires a fresh development deployment
and state reset. Remove obsolete decoders, aliases, fallback branches, and
removal-only fixtures. A missing or unsupported schema discriminator fails at
the boundary; it is never inferred from a record's shape.

Version values belong in wire, storage, and manifest identities. Source naming
follows the [naming policy](../../agents/naming-and-versioning.md), which
supersedes the consolidation plan's requirement to suffix every exported type.
Nested records do not need redundant version fields when their authenticated
outer envelope fixes their interpretation. Semantic constructor tags, sentinels,
Cardano/Plutus versions, and dependency versions retain their meanings.

Once a version actually ships, its compatibility and upgrade obligations require
a separate protocol decision. Development history is not a shipped version.

## Consequences

A format reset changes canonical bytes, commitments, validator hashes, deployment
identity, and persisted state. Follow the [state-reset rules](../../agents/state-reset.md)
and deployment acceptance procedure. This decision does not authorize deleting
unrelated data, selecting another MPF engine, reducing Cardano capability, or
claiming release acceptance from successful consolidation alone.

Operational rollback, finalization recovery, provider failover, leases, and
idempotent replay remain necessary. They are recovery mechanisms, not obsolete
format compatibility. The Cardano capability floor remains governed by
[decision 0001](0001-cardano-l1-transaction-capability-floor.md); activation
still requires [proof and deployment acceptance](../../exec-plans/GOAL_SPEC.md).

The delivered consolidation and format inventory are recoverable from Git at
`81a511e15:docs/exec-plans/canonical-v1-consolidation.md` and
`81a511e15:docs/exec-plans/canonical-v1-format-registry.md`. Current format
authority is [the component specification](../../spec/README.md), source, and
cross-language tests, rather than that completed migration checklist.
