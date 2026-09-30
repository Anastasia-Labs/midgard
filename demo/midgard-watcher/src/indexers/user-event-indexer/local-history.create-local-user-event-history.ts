import { type WatcherLocalBackfillFinalityReceipt } from "../../l1/finality-engine.js";
import { type WatcherLocalBackfillObservationReceipt } from "../../l1/l1-adapter.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  type WatcherUserEventScriptBinding,
} from "../../runtime/deployment-identity.js";
import {
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherProtectedUserEventCheckpoint,
} from "../../storage/durable-runtime.js";
import { makeEmptyWatcherDurableStore } from "../../storage/durable-store.js";
import {
  readWatcherUserEventOrigin,
  type WatcherUserEventOriginReceipt,
} from ".././user-event-origin.js";
import {
  localArchiveBudgets,
  localArchiveEvidence,
  localArchiveObject,
  localHistories,
  localHistoryBrand,
  type LocalHistoryOwner,
  localOwner,
  localRefuse,
  type WatcherLocalUserEventHistory,
} from "./local-history.local-history-owner.js";
import {
  immutableWireValue,
  makeWatcherUserEventIndexerPolicy,
  same,
} from "./policy.js";
import { makeSnapshot, storeDigest } from "./snapshot.js";
import { WATCHER_USER_EVENT_INDEXER_BOUNDS } from "./types.js";

/** Empty initialization is available only at the authenticated whole activation block. */
export const createLocalUserEventHistory = (
  input: Readonly<{
    origin: WatcherUserEventOriginReceipt;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    scriptBinding: WatcherUserEventScriptBinding;
    finality: WatcherLocalBackfillFinalityReceipt;
    observation: WatcherLocalBackfillObservationReceipt;
    publication: WatcherProtectedUserEventCheckpoint;
    semanticReplay: boolean;
  }>,
): WatcherLocalUserEventHistory => {
  const {
    origin: originReceipt,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
    publication: publicationReceipt,
  } = input;
  const origin = readWatcherUserEventOrigin({
    origin: originReceipt,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
  });
  const publication =
    readWatcherProtectedUserEventCheckpointReceipt(publicationReceipt);
  const finalityPolicy = origin.originalWitness.current.finality.policy;
  if (
    (!input.semanticReplay &&
      (publication.checkpoint !== null || publication.payload !== null)) ||
    !same(
      publication.trustedHead.deploymentMarker,
      deploymentIdentity.durableMarker,
    ) ||
    !same(finalityPolicy.deploymentMarker, deploymentIdentity.durableMarker) ||
    finalityPolicy.blueprintHash !== origin.blueprintHash ||
    finalityPolicy.network !== origin.network
  ) {
    return localRefuse(
      "empty origin requires an absent matching protected checkpoint",
    );
  }
  const store = immutableWireValue(
    makeEmptyWatcherDurableStore(deploymentIdentity.durableMarker),
  );
  const parsedPolicy = makeWatcherUserEventIndexerPolicy({
    network: origin.network,
    ...(finalityPolicy.customNetwork === undefined
      ? {}
      : { customNetwork: finalityPolicy.customNetwork }),
    blueprintHash: origin.blueprintHash,
    deploymentMarker: deploymentIdentity.durableMarker,
    deposit: origin.scripts.deposit,
    withdrawal: origin.scripts.withdrawal,
    forcedOrder: origin.scripts.forcedOrder,
    bootstrapStoreDigest: storeDigest(store),
    deploymentTrustRootId: deploymentIdentity.trustRootId,
    requiredFinalityDepth: finalityPolicy.confirmationDepth,
    maximumActiveHistoryEntries:
      WATCHER_USER_EVENT_INDEXER_BOUNDS.activeHistoryEntries.toString(),
    maximumAuditHistoryEntries:
      WATCHER_USER_EVENT_INDEXER_BOUNDS.auditHistoryEntries.toString(),
  });
  if (parsedPolicy === null)
    return localRefuse("origin cannot establish the strict event policy");
  const policy = immutableWireValue(parsedPolicy);
  const snapshot = makeSnapshot([], []);
  if (snapshot === null)
    return localRefuse("empty snapshot construction failed");
  const originArchive = localArchiveObject({
    schemaVersion: "midgard-watcher-local-user-event-origin-archive-v1",
    numericEncoding: "exact-decimal-strings",
    facts: localArchiveEvidence(origin),
    policy,
    bootstrapStore: store,
  });
  const history = Object.freeze({ [localHistoryBrand]: true as const });
  localHistories.set(history, {
    origin,
    originDigest: origin.originDigest,
    activationPair: Object.freeze({
      finality: finality,
      observation: observation,
    }),
    deploymentIdentity: deploymentIdentity,
    scriptBinding: scriptBinding,
    policy,
    finalityPolicy,
    originArchive,
    store,
    snapshot: immutableWireValue(snapshot),
    entries: Object.freeze([]),
    acceptedEvidence: Object.freeze([]),
    pinnedEvidence: Object.freeze([]),
    archiveIndex: null,
    anchorCandidate: null,
    lastAccepted: null,
    archiveObjects: Object.freeze([originArchive]),
    checkpoint: null,
    candidate: null,
    coverage: null,
    generation: 0,
    acceptedAtMonotonicMs: null,
    closed: false,
    suspendedAt: null,
    semanticReplay: input.semanticReplay,
  });
  return history;
};

/** Empty initialization remains unavailable over a published checkpoint. */
export const createWatcherLocalUserEventHistory = (
  input: Omit<
    Parameters<typeof createLocalUserEventHistory>[0],
    "semanticReplay"
  >,
): WatcherLocalUserEventHistory =>
  createLocalUserEventHistory({ ...input, semanticReplay: false });

/** Retained checkpoint closure measured the way the publish bound measures
 * it: unique objects by digest, canonical bytes, evidence nodes. */
const localRetainedArchive = (owner: LocalHistoryOwner) =>
  Object.freeze({
    objects: owner.archiveObjects.length,
    bytes: owner.archiveObjects.reduce(
      (total, object) => total + object.bytesHex.length / 2,
      0,
    ),
    nodes: owner.archiveObjects.reduce(
      (total, object) => total + localArchiveBudgets.get(object)!.nodes,
      0,
    ),
  });

/** Every retained entry pins its own durable-store snapshot, so the closure
 * grows with the store as well as with the entry count. An anchor is due once
 * more than the 64-entry suffix is retained and either the entry count reaches
 * the active bound or any closure dimension has used half its bound; waiting
 * for the entry count alone lets the publish bound refuse first. */
export const localAnchorDue = (owner: LocalHistoryOwner): boolean => {
  if (owner.entries.length <= 64) return false;
  if (
    owner.entries.length >=
    WATCHER_USER_EVENT_INDEXER_BOUNDS.activeHistoryEntries
  )
    return true;
  const retained = localRetainedArchive(owner);
  return (
    retained.objects * 2 >=
      WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries ||
    retained.bytes * 2 >=
      WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes ||
    retained.nodes * 2 >=
      WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes
  );
};

export const readWatcherLocalUserEventHistory = (
  history: WatcherLocalUserEventHistory,
) => {
  const owner = localOwner(history);
  return Object.freeze({
    policy: owner.policy,
    store: owner.store,
    snapshot: owner.snapshot,
    cursor: owner.entries.at(-1)?.cursor ?? null,
    entryDigest: owner.entries.at(-1)?.entryDigest ?? null,
    checkpoint: owner.checkpoint,
    retainedEntries: owner.entries.length,
    retainedArchive: localRetainedArchive(owner),
    anchorDue: localAnchorDue(owner),
    status:
      owner.candidate !== null || owner.anchorCandidate !== null
        ? ("publication_pending" as const)
        : owner.entries.length >=
            Number(owner.policy.maximumActiveHistoryEntries)
          ? ("history_bound_hold" as const)
          : ("ready" as const),
  });
};
