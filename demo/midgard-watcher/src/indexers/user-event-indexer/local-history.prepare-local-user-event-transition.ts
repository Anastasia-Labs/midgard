import { encodeWatcherNormalizedL1Block } from "../../l1/l1-adapter.js";
import { watcherDeploymentAppliedScriptHashes } from "../../runtime/deployment-identity.js";
import {
  journalWatcherProtocolUtxoTransition,
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
} from "../../storage/durable-store.js";
import {
  makeWatcherUserEventCheckpoint,
  WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
} from "../../storage/user-event-checkpoint.js";
import {
  localArchiveBudgets,
  localArchiveEvidence,
  localArchiveObject,
  localCoverageHead,
  localHistoryAnchorDescriptor,
  localOwner,
  type LocalPair,
  localRefuse,
  localTransitionBrand,
  localTransitions,
  type WatcherLocalUserEventHistory,
  type WatcherLocalUserEventTransition,
} from "./local-history.local-history-owner.js";
import { localLivePair } from "./local-history.restore-watcher-local-user-event-coverage.js";
import { immutableWireValue, same, sha256Canonical } from "./policy.js";
import {
  deriveLocalBlockEventSnapshot,
  makeObservation,
  protocolRole,
  storeDigest,
  storeTransitionMatches,
} from "./snapshot.js";
import {
  WATCHER_USER_EVENT_INDEXER_BOUNDS,
  WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION,
} from "./types.js";

export const prepareLocalUserEventTransition = (
  history: WatcherLocalUserEventHistory,
  pair: LocalPair,
): WatcherLocalUserEventTransition => {
  const owner = localOwner(history);
  if (owner.anchorCandidate !== null)
    return localRefuse("anchor publication is unresolved");
  const live = localLivePair(owner, pair);
  if (owner.candidate !== null) {
    const candidate = localTransitions.get(owner.candidate)!;
    if (
      candidate.pair.finality === pair.finality &&
      candidate.pair.observation === pair.observation &&
      candidate.pair.referenceAuthority === pair.referenceAuthority
    )
      return owner.candidate;
    return localRefuse("a different publication is unresolved");
  }
  if (owner.lastAccepted !== null) {
    const accepted = localTransitions.get(owner.lastAccepted)!;
    if (
      accepted.pair.finality === pair.finality &&
      accepted.pair.observation === pair.observation &&
      accepted.pair.referenceAuthority === pair.referenceAuthority
    )
      return owner.lastAccepted;
  }
  if (
    owner.entries.length >= Number(owner.policy.maximumActiveHistoryEntries) ||
    owner.entries.length >= Number(owner.policy.maximumAuditHistoryEntries)
  ) {
    return localRefuse(
      "history bound reached; semantic anchor rotation required",
    );
  }
  const { witness, referenceEvidence } = live;
  const { native: block, capture } = witness.current.observation;
  const predecessor = owner.entries.at(-1);
  if (predecessor === undefined) {
    if (
      pair.finality !== owner.activationPair.finality ||
      pair.observation !== owner.activationPair.observation ||
      block !== owner.origin.block
    )
      return localRefuse("first block is not the exact activation pair");
  } else {
    // The block must be the direct child of the covered head: the last entry
    // itself, or the quiet stretch admitted above it block by block.
    const covered = localCoverageHead(owner);
    if (
      !same(capture.predecessorPoint, covered) ||
      block.chainPoint.parentBlockHash !== covered.blockHash ||
      BigInt(capture.point.blockNo) !== BigInt(covered.blockNo) + 1n ||
      BigInt(capture.point.slot) <= BigInt(covered.slot)
    ) {
      return localRefuse("block is not the strict full-point successor");
    }
  }
  const derivedSnapshot = deriveLocalBlockEventSnapshot(
    owner.policy,
    owner.snapshot,
    block,
    referenceEvidence,
    {
      appliedScriptHashes: watcherDeploymentAppliedScriptHashes(
        owner.deploymentIdentity,
      ),
    },
  );
  if (derivedSnapshot === null)
    return localRefuse("whole-block event semantics differ");
  const snapshot = immutableWireValue(derivedSnapshot);
  const sourceStore = owner.store;
  const chainPoints = [
    ...sourceStore.chainPoints,
    {
      chainPointId: block.chainPoint.chainPointId,
      providerId: block.provider.providerId,
      blockHash: block.chainPoint.blockHash,
      slot: block.chainPoint.slot,
      blockNo: block.chainPoint.blockNo,
      depth: block.chainPoint.depth,
    },
  ];
  const journal = journalWatcherProtocolUtxoTransition({
    sourceStore,
    nextChainPoints: chainPoints,
    spentAtChainPointId: block.chainPoint.chainPointId,
    nextProtocolUtxos: [
      ...sourceStore.protocolUtxos.filter(
        ({ role }) =>
          !["deposit", "withdrawal", "forced_transaction"].includes(role),
      ),
      ...snapshot.activeEvents.map((event) => ({
        outRef: event.outRef,
        role: protocolRole(event.kind),
        chainPointId:
          sourceStore.protocolUtxos.find(
            ({ outRef }) => outRef === event.outRef,
          )?.chainPointId ?? block.chainPoint.chainPointId,
        output: makeWatcherDurablePayload(event.outputCborHex),
      })),
    ],
  });
  const nextStore = immutableWireValue(
    makeWatcherDurableStore({
      deploymentMarker: sourceStore.deploymentMarker,
      revision: (BigInt(sourceStore.revision) + 1n).toString(),
      records: {
        ...sourceStore,
        chainPoints,
        ...journal,
        l1Observations: [
          ...sourceStore.l1Observations,
          {
            observationId: block.observationDigest,
            providerId: block.provider.providerId,
            chainPointId: block.chainPoint.chainPointId,
            payload: makeWatcherDurablePayload(
              encodeWatcherNormalizedL1Block(block).toString("hex"),
            ),
          },
        ],
      },
    }),
  );
  if (!storeTransitionMatches(sourceStore, nextStore, block, snapshot))
    return localRefuse("event view journal differs");
  const observation = makeObservation({
    schemaVersion: WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION,
    policyDigest: owner.policy.policyDigest,
    network: owner.policy.network,
    blueprintHash: owner.policy.blueprintHash,
    deploymentMarker: owner.policy.deploymentMarker,
    transitionKind: "apply_block",
    pointDigest: block.chainPoint.pointDigest,
    blockHash: block.chainPoint.blockHash,
    slot: block.chainPoint.slot,
    blockNo: block.chainPoint.blockNo,
    sourceObservationDigest: block.observationDigest,
    chainPointId: block.chainPoint.chainPointId,
    sourceDurableStoreDigest: storeDigest(sourceStore),
    sourceDurableStoreRevision: sourceStore.revision,
    durableStoreDigest: storeDigest(nextStore),
    durableStoreRevision: nextStore.revision,
    rollbackTargetEntryDigest: null,
    snapshot,
  });
  const evidence = localArchiveObject({
    schemaVersion: "midgard-watcher-local-user-event-block-evidence-v1",
    numericEncoding: "exact-decimal-strings",
    witnesses: localArchiveEvidence(witness),
    referenceEvidence,
  });
  const entryFields = {
    schemaVersion: "midgard-watcher-local-user-event-entry-v1" as const,
    sequence:
      predecessor === undefined
        ? "0"
        : (BigInt(predecessor.sequence) + 1n).toString(),
    originDigest: owner.originDigest,
    policyDigest: owner.policy.policyDigest,
    predecessorEntryDigest: predecessor?.entryDigest ?? null,
    predecessorStateDigest: owner.checkpoint?.payloadDigest ?? null,
    cursor: capture.point,
    parent: capture.predecessorPoint,
    sourceStoreDigest: observation.sourceDurableStoreDigest,
    nextStoreDigest: observation.durableStoreDigest,
    sourceStoreRevision: sourceStore.revision,
    nextStoreRevision: nextStore.revision,
    observationDigest: observation.observationDigest,
    snapshotDigest: snapshot.snapshotDigest,
    evidenceDigest: evidence.digest,
  };
  const entry = Object.freeze({
    ...entryFields,
    entryDigest: sha256Canonical(entryFields),
  });
  const entryArchive = localArchiveObject({ entry, observation });
  const storeArchive = localArchiveObject(nextStore);
  const payload = localArchiveObject({
    schemaVersion: "midgard-watcher-local-user-event-checkpoint-payload-v1",
    originArchiveDigest: owner.originArchive.digest,
    originDigest: owner.originDigest,
    policy: owner.policy,
    anchor: localHistoryAnchorDescriptor(owner),
    head: entry,
    storeArchiveDigest: storeArchive.digest,
    snapshot,
    retainedEntries: [...owner.entries, entry],
    requiredSemanticResume:
      "authenticated_origin_replay_or_semantic_publication_receipt",
  });
  // Retained objects are identified by digest: an entry whose evidence or
  // store archive repeats an earlier object must not be budgeted twice.
  const archiveObjects = Object.freeze([
    ...new Map(
      [
        ...owner.archiveObjects,
        evidence,
        entryArchive,
        storeArchive,
        payload,
      ].map((object) => [object.digest, object] as const),
    ).values(),
  ]);
  const retainedNodes = archiveObjects.reduce(
    (nodes, object) => nodes + localArchiveBudgets.get(object)!.nodes,
    0,
  );
  const retainedBytes = archiveObjects.reduce(
    (bytes, object) => bytes + object.bytesHex.length / 2,
    0,
  );
  if (
    retainedNodes > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes ||
    archiveObjects.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries ||
    retainedBytes > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes
  )
    return localRefuse(
      `retained archive bound reached (objects ${archiveObjects.length.toString()}/${WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries.toString()}, bytes ${retainedBytes.toString()}/${WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes.toString()}, nodes ${retainedNodes.toString()}/${WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes.toString()}; new evidence ${localArchiveBudgets.get(evidence)!.nodes.toString()} nodes ${(evidence.bytesHex.length / 2).toString()} bytes)`,
    );
  const nextCheckpoint = makeWatcherUserEventCheckpoint({
    schemaVersion: WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
    deploymentMarker: owner.policy.deploymentMarker,
    network: owner.policy.network,
    blueprintHash: owner.policy.blueprintHash,
    finalityPolicyDigest: owner.finalityPolicy.policyDigest,
    userEventPolicyDigest: owner.policy.policyDigest,
    checkpointSequence:
      owner.checkpoint === null
        ? "0"
        : (BigInt(owner.checkpoint.checkpointSequence) + 1n).toString(),
    predecessorCheckpointDigest: owner.checkpoint?.checkpointDigest ?? null,
    rollbackGeneration: owner.checkpoint?.rollbackGeneration ?? "0",
    payloadDigest: payload.digest,
    requiredArchiveDigests: [
      ...new Set(archiveObjects.map(({ digest }) => digest)),
    ].sort(),
  });
  const rechecked = localLivePair(owner, pair);
  if (
    rechecked.witness.first !== witness.first ||
    rechecked.witness.current !== witness.current ||
    rechecked.referenceEvidence !== referenceEvidence
  )
    return localRefuse("candidate evidence changed");
  const transition = Object.freeze({ [localTransitionBrand]: true as const });
  const value = Object.freeze({
    sourceStore,
    nextStore,
    observation,
    snapshot,
    entry,
    archiveObjects,
    nextCheckpoint,
    expectedCheckpointDigest: owner.checkpoint?.checkpointDigest ?? null,
    expectedCheckpointSequence: owner.checkpoint?.checkpointSequence ?? null,
  });
  localTransitions.set(transition, {
    history: history,
    generation: owner.generation,
    pair,
    witness,
    referenceEvidence,
    value,
    accepted: false,
  });
  owner.candidate = transition;
  return transition;
};
