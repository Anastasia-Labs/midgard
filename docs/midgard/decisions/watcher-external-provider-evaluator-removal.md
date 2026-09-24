# Remove the unwired watcher external-provider indexer evaluators

Status: Accepted

Recorded: 2026-09-23.

## Context

The watcher carried four indexer evaluators:
`evaluateWatcherStateQueueIndexer` (W14),
`evaluateWatcherUserEventIndexer` (W15),
`evaluateWatcherSettlementIndexer` (W16) and
`evaluateWatcherProofThreadIndexer` (W17). They were built for the
external-provider evidence model: each one checked provider-attested L1 block
observations, multi-provider consistency, finality and rollback results, and
then derived an index from them.

That is not how production chain authority works.
[Watcher verification boundaries](watcher-verification-boundaries.md) makes the
configured local Cardano node the production chain authority. The node is
admitted through native chain sync and cross-checked against local Kupo/Ogmios.
The watcher runtime (`state-queue-runtime.ts`, `user-event-runtime.ts`)
observes the state queue through
`authenticated-state-queue-observation.ts` and user events through the local
user-event history. The four evaluators were never wired into the runtime.
Their only consumers were their own tests, the W25 `parser_replay` event
authority path, and the W26 event-classification verifier, whose only authority
input was a W15 evaluator result.

## Decision

Remove the four evaluators and everything that existed only to serve them:

- the W14, W16 and W17 evaluator modules;
- the W15 evaluator section of `user-event-indexer.ts`;
- the W26 event-classification verifier;
- the W25 `parser_replay` event-origin branch, which had no production writer;
- their tests and test-only scenario harnesses;
- the exports that no longer have a consumer.

Keep only the parts that do not depend on the evaluators:

- The structural state-queue header and snapshot parsers, now in
  `demo/midgard-watcher/src/indexers/state-queue-snapshot.ts`. They are used by
  W22 header-root reconstruction and attestation-timeout observation.
- User-event decoding and the local user-event history in
  `demo/midgard-watcher/src/indexers/user-event-indexer.ts`.
- Configuration and runtime support for `external_providers`: config
  parsing, the finality policy, multi-provider consistency and the
  external-provider transport.

W25 block replay now accepts only locally published user-event authorities. Its
durable result still carries the existing `downstreamPrerequisite` and
verified-contract text, which name W26 and W29. That text was left unchanged in
this removal because it is part of the durable replay-transcript format.

## Consequences

- The watcher does not provide W16 settlement, reserve or payout tracking.
- The watcher does not provide W17 reconciliation of proof or computation
  threads with the local proof journal.
- The watcher does not provide W26 due, omitted, fabricated or duplicate event
  classification.
- Any future need for these must build on the local-node observation model. It
  must not revive the external-provider evaluator model.
- `docs/exec-plans/GOAL_SPEC.md` still lists W14 to W17 as deliverables, and
  W26 as depending on them. Amending the specification is a separate decision.
- The W25 result's W26/W29 contract text names a verifier that no longer
  exists. Changing it changes the durable format and the result digests.
  Before launch this is an in-place replacement under
  [prelaunch format replacement](prelaunch-format-replacement.md). It is not
  part of this removal.

## Links

- [Watcher verification boundaries](watcher-verification-boundaries.md)
- [Watcher persistence](watcher-persistence.md)
- [Prelaunch format replacement](prelaunch-format-replacement.md)
- [State-queue observation](../../../demo/midgard-watcher/src/indexers/authenticated-state-queue-observation.ts)
- [State-queue snapshot parsers](../../../demo/midgard-watcher/src/indexers/state-queue-snapshot.ts)
- [W25 block replay](../../../demo/midgard-watcher/src/verification/block-replay.ts)
