# Transition Trace Commitments

Last reviewed: 2026-09-07 (commitment source map and DA transport).

This reference explains how block commitments bind source events to ledger
transitions. The [transaction specification](../../../docs/spec/midgard-tx.md)
and [consensus profile](../../../docs/consensus-profile-v1.md) own the concrete
format and limits. The [fault-proof audit](../../../docs/fault-proofs/README.md)
owns implemented challenge coverage; the architecture alone does not establish
that every permitted transition is publicly challengeable.

## Commitment boundaries

A queued `StateQueueNode` contains a header and typed DA-availability state
(`Unattested`, `Attested`, `Challenged`, or `Published`). Its
header binds previous and resulting ledger roots, source roots, the transition
trace, event-to-step placement, validation traces, counts, and validation
context. These are on-chain commitments, not merely local database metadata.

Source identity and execution order are separate:

- Withdrawals and deposits use the corresponding L1 event identity.
- Forced transactions use the L1 order identity. Two orders carrying the same
  L2 transaction remain distinct events.
- Normal transactions use their native transaction identity.
- The event-to-step map binds an event key to its trace position.
- Each transition leaf binds its event, phase, and pre/post ledger roots.

The phase order is withdrawals, forced transactions, normal L2 transactions,
then deposits. A transaction cannot spend a deposit first introduced in the
same block's later deposit phase. Within-phase ordering and exact encodings
must be read from the active specification and construction code.

Count, boundary, adjacency, source-membership, and event-placement checks compose
with phase-specific transition checks. A correct final root alone does not
provide the intermediate evidence needed to isolate an invalid transition.

## Data availability

DA protocol traffic uses libp2p only. A challenger retrieves committee-retained
payloads through deployment-bound protocols and validates them against the L1
header. HTTP DA payload/metadata endpoints are retired and must not be used as
fallbacks. See the [DA transport decision](../../../docs-site/content/docs/watchers/da-transport.mdx)
and the [committee architecture](../../da-committee-node/docs/da-committee-node-architecture.md).

Retained payloads provide source entries, trace entries, event placement,
validation material, and ledger members needed to reconstruct the commitments
and supported witnesses. Signatures, retention, permissionless retrieval, and
proof coverage are separate assurance boundaries; see the current
[readiness assessment](../../../docs/public_testnet_readiness.md).

## Implementation map

| Boundary                                      | Source                                                                                                                             |
| --------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| Shared transition types and encodings         | [`transition-trace.ts`](../../midgard-sdk/src/transition-trace.ts)                                                                 |
| Node root construction and consistency checks | [`mpf/transition-trace.ts`](../src/mpf/transition-trace.ts)                                                                        |
| Producer payload assembly                     | [`da-payload.ts`](../src/workers/commit-block-header/da-payload.ts)                                                                |
| Payload envelope                              | [`da-payload-envelope.ts`](../../midgard-core/src/da-payload-envelope.ts)                                                          |
| Public retained-DA service                    | [`public-retained-da-runtime.ts`](../../da-committee-node/src/public-retained-da-runtime.ts)                                       |
| SDK proof types and helpers                   | [`fraud-proof/transition-trace.ts`](../../midgard-sdk/src/fraud-proof/transition-trace.ts)                                         |
| Current proof installation and evidence       | [Catalogue status](../../../docs/fault-proofs/catalogue-status.md), [testing status](../../../docs/fault-proofs/testing-status.md) |

Do not copy a header field inventory or generated script hashes into this page.
Header ABI checks and proof acceptance artifacts must refer to the same source
revision, compiler, deployment parameters, and wire identity before their results
can substantiate a deployment claim.
