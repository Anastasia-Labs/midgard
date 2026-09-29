import {
  admitFraudProofRawL1Point,
  computeFraudProofRawL1PointId,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  readWatcherLocalBackfillFinalityObservation,
  readWatcherLocalBackfillFinalityOriginalWitness,
  type WatcherFinalityPolicy,
  type WatcherLocalBackfillFinalityReceipt,
} from "../../l1/finality-engine.js";
import {
  encodeWatcherNormalizedL1Block,
  type WatcherLocalBackfillObservationReceipt,
} from "../../l1/l1-adapter.js";
import {
  readWatcherUserEventScriptBinding,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentAppliedScriptHashes,
  type WatcherUserEventScriptBinding,
} from "../../runtime/deployment-identity.js";
import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherDurableRuntime,
  type WatcherProtectedUserEventCheckpoint,
} from "../../storage/durable-runtime.js";
import {
  journalWatcherProtocolUtxoTransition,
  makeEmptyWatcherDurableStore,
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  parseWatcherDurableStore,
  watcherCanonicalJson,
  type WatcherDurableStore,
} from "../../storage/durable-store.js";
import {
  makeWatcherUserEventCheckpoint,
  WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
  WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION,
  type WatcherUserEventArchive,
  type WatcherUserEventCheckpoint,
  type WatcherUserEventValidation,
} from "../../storage/user-event-checkpoint.js";
import {
  assertWatcherStateQueueHeaderObservation,
  type WatcherStateQueueHeaderObservation,
} from ".././authenticated-state-queue-observation.js";
import {
  findWatcherUserEventArchiveIndex,
  makeWatcherUserEventArchiveIndex,
  readWatcherUserEventArchiveIndex,
  type WatcherUserEventArchiveIndexRead,
} from ".././user-event-history-archive.js";
import {
  readWatcherUserEventOrigin,
  type WatcherUserEventOriginFacts,
  type WatcherUserEventOriginReceipt,
} from ".././user-event-origin.js";
import {
  admitWatcherLocalBackfillUserEventReferenceEvidence,
  readWatcherUserEventReferenceEvidence,
  type WatcherUserEventReferenceAuthority,
  type WatcherUserEventReferenceEvidence,
} from ".././user-event-reference-authority.js";
import { outputReference } from "./decode.js";
import {
  evidenceWithinBounds,
  exactRecord,
  immutableWireValue,
  isHex28,
  isHex32,
  isHexBytes,
  isNatural,
  makeWatcherUserEventIndexerPolicy,
  same,
  sha256Bytes,
  sha256Canonical,
} from "./policy.js";
import {
  deriveLocalBlockEventSnapshot,
  makeObservation,
  makeSnapshot,
  parseObservationStructural,
  protocolRole,
  storeDigest,
  storeTransitionMatches,
  topologyMatches,
} from "./snapshot.js";
import {
  type EvidenceGraphBudget,
  type PlainRecord,
  WATCHER_USER_EVENT_INDEXER_BOUNDS,
  WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION,
  WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION,
  type WatcherIndexedUserEvent,
  type WatcherTerminalUserEvent,
  type WatcherUserEventIndexerPolicy,
  type WatcherUserEventKind,
  type WatcherUserEventObservation,
  type WatcherUserEventSnapshot,
} from "./types.js";

const localHistoryBrand = Symbol("watcher-local-user-event-history");
const localTransitionBrand = Symbol("watcher-local-user-event-transition");
export type WatcherLocalUserEventHistory = Readonly<{
  [localHistoryBrand]: true;
}>;
export type WatcherLocalUserEventTransition = Readonly<{
  [localTransitionBrand]: true;
}>;
type LocalPair = Readonly<{
  finality: WatcherLocalBackfillFinalityReceipt;
  observation: WatcherLocalBackfillObservationReceipt;
  referenceAuthority: WatcherUserEventReferenceAuthority;
}>;
type LocalWitness = ReturnType<
  typeof readWatcherLocalBackfillFinalityOriginalWitness
>;
type LocalArchiveObject = Readonly<{ digest: string; bytesHex: string }>;
export type WatcherLocalUserEventEntry = Readonly<{
  schemaVersion: "midgard-watcher-local-user-event-entry-v1";
  sequence: string;
  originDigest: string;
  policyDigest: string;
  predecessorEntryDigest: string | null;
  predecessorStateDigest: string | null;
  cursor: WatcherUserEventOriginFacts["parentPoint"];
  parent: WatcherUserEventOriginFacts["parentPoint"];
  sourceStoreDigest: string;
  nextStoreDigest: string;
  sourceStoreRevision: string;
  nextStoreRevision: string;
  observationDigest: string;
  snapshotDigest: string;
  evidenceDigest: string;
  entryDigest: string;
}>;
type LocalPreparedRead = Readonly<{
  sourceStore: WatcherDurableStore;
  nextStore: WatcherDurableStore;
  observation: WatcherUserEventObservation;
  snapshot: WatcherUserEventSnapshot;
  entry: WatcherLocalUserEventEntry;
  archiveObjects: readonly LocalArchiveObject[];
  nextCheckpoint: WatcherUserEventCheckpoint;
  expectedCheckpointDigest: string | null;
  expectedCheckpointSequence: string | null;
}>;
type LocalRetainedEvidence = Readonly<{
  entry: WatcherLocalUserEventEntry;
  entryArchiveDigest: string;
  rawBlockCbor: string;
  pointDigest: string;
  chainPointId: string;
}>;
type LocalHistoryOwner = {
  readonly origin: WatcherUserEventOriginFacts;
  readonly originDigest: string;
  readonly activationPair: Readonly<{
    finality: WatcherLocalBackfillFinalityReceipt;
    observation: WatcherLocalBackfillObservationReceipt;
  }>;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly scriptBinding: WatcherUserEventScriptBinding;
  readonly policy: WatcherUserEventIndexerPolicy;
  readonly finalityPolicy: WatcherFinalityPolicy;
  readonly originArchive: LocalArchiveObject;
  store: WatcherDurableStore;
  snapshot: WatcherUserEventSnapshot;
  entries: readonly WatcherLocalUserEventEntry[];
  acceptedEvidence: readonly LocalRetainedEvidence[];
  pinnedEvidence: readonly LocalRetainedEvidence[];
  archiveIndex: WatcherUserEventArchiveIndexRead | null;
  anchorCandidate: WatcherLocalUserEventAnchor | null;
  lastAccepted: WatcherLocalUserEventTransition | null;
  archiveObjects: readonly LocalArchiveObject[];
  checkpoint: WatcherUserEventCheckpoint | null;
  candidate: WatcherLocalUserEventTransition | null;
  /** Coverage sits on the head entry and extends over quiet blocks. */
  coverage: WatcherLocalUserEventCoverage | null;
  generation: number;
  acceptedAtMonotonicMs: number | null;
  closed: boolean;
  suspendedAt: number | null;
  semanticReplay: boolean;
};
/**
 * The moving coverage checkpoint: every block from the head entry's cursor
 * through `point` has been admitted from the native stream with its parent
 * link checked, and none of them carried anything the event fold tracks.
 * Point coverage is a lookup against it, never a walk.
 */
export type WatcherLocalUserEventCoverage = Readonly<{
  point: WatcherUserEventOriginFacts["parentPoint"];
  headEntryDigest: string;
}>;
type LocalTransitionOwner = {
  readonly history: WatcherLocalUserEventHistory;
  readonly generation: number;
  readonly pair: LocalPair;
  readonly witness: LocalWitness;
  readonly referenceEvidence: WatcherUserEventReferenceEvidence;
  readonly value: LocalPreparedRead;
  accepted: boolean;
};
const localHistories = new WeakMap<
  WatcherLocalUserEventHistory,
  LocalHistoryOwner
>();
const localTransitions = new WeakMap<
  WatcherLocalUserEventTransition,
  LocalTransitionOwner
>();
const localRefuse = (reason: string): never => {
  throw new Error(`Local user-event history refused: ${reason}`);
};
const localOwner = (
  history: WatcherLocalUserEventHistory,
): LocalHistoryOwner => {
  const owner =
    localHistories.get(history) ??
    localRefuse("history is not privately admitted");
  if (owner.closed) return localRefuse("history is closed");
  if (owner.suspendedAt !== null) return localRefuse("history is suspended");
  return owner;
};

/** Archive schema encodes every number as its exact finite decimal string.
 * Numeric field types belong to the evidence schema, never to a revived clock.
 * These bytes are descriptive past-process facts, not reissued receipt authority.
 */
const localArchiveEvidence = (value: unknown): unknown => {
  if (typeof value === "bigint") return value.toString();
  if (typeof value === "number") {
    if (!Number.isFinite(value))
      return localRefuse("non-finite archive number");
    const decimal = Object.is(value, -0) ? "-0" : value.toString();
    if (!Object.is(Number(decimal), value))
      return localRefuse("inexact archive number");
    return decimal;
  }
  if (Array.isArray(value)) return value.map(localArchiveEvidence);
  if (typeof value === "object" && value !== null) {
    return Object.fromEntries(
      Object.entries(value).map(([key, child]) => [
        key,
        localArchiveEvidence(child),
      ]),
    );
  }
  return value;
};
const localArchiveBudgets = new WeakMap<
  LocalArchiveObject,
  EvidenceGraphBudget
>();
const localArchiveObject = (value: unknown): LocalArchiveObject => {
  const budget = { nodes: 0, bytes: 0 };
  if (!evidenceWithinBounds(value, budget))
    return localRefuse("archive evidence bound exceeded");
  const bytes = Buffer.from(watcherCanonicalJson(value), "utf8");
  if (
    bytes.byteLength > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes
  )
    return localRefuse("archive byte bound exceeded");
  const object = Object.freeze({
    digest: sha256Bytes(bytes),
    bytesHex: bytes.toString("hex"),
  });
  localArchiveBudgets.set(object, Object.freeze(budget));
  return object;
};
const localHistoryAnchorDescriptor = (owner: LocalHistoryOwner) =>
  owner.archiveIndex === null
    ? {
        kind: "activation_origin",
        parent: owner.origin.parentPoint,
        bootstrapStoreDigest: owner.policy.bootstrapStoreDigest,
      }
    : {
        kind: "materialized_history",
        indexDigest: owner.archiveIndex.digest,
        indexSequence: owner.archiveIndex.index.indexSequence,
        retainedSuffixEntries: "64",
      };
const localRetainedEvidence = (
  owner: LocalHistoryOwner,
): readonly LocalRetainedEvidence[] =>
  [...owner.pinnedEvidence, ...owner.acceptedEvidence].sort((left, right) =>
    BigInt(left.entry.sequence) < BigInt(right.entry.sequence)
      ? -1
      : BigInt(left.entry.sequence) > BigInt(right.entry.sequence)
        ? 1
        : 0,
  );

const localCoverageOnHead = (
  head: WatcherLocalUserEventEntry,
): WatcherLocalUserEventCoverage =>
  Object.freeze({
    point: Object.freeze({ ...head.cursor }),
    headEntryDigest: head.entryDigest,
  });

/** The covered head: the coverage point when it references the head entry,
 * otherwise the head entry's own cursor. The activation block has no entry
 * yet, so before it the covered head is the origin's parent. */
const localCoverageHead = (
  owner: LocalHistoryOwner,
): WatcherUserEventOriginFacts["parentPoint"] => {
  const head = owner.entries.at(-1);
  if (head === undefined) return owner.origin.parentPoint;
  const coverage = owner.coverage;
  if (coverage === null || coverage.headEntryDigest !== head.entryDigest)
    return localRefuse("coverage does not sit on the head entry");
  return coverage.point;
};

export const readWatcherLocalUserEventCoverage = (
  history: WatcherLocalUserEventHistory,
): WatcherLocalUserEventCoverage | null => {
  const owner = localOwner(history);
  const head = owner.entries.at(-1);
  if (head === undefined) return null;
  return Object.freeze({
    point: Object.freeze({ ...localCoverageHead(owner) }),
    headEntryDigest: head.entryDigest,
  });
};

/**
 * Admits one quiet native block above the covered head. The link is checked
 * locally from the header the native stream delivered: parent hash, block
 * number and slot. No request, no observation, no digest-chain change.
 */
export const advanceWatcherLocalUserEventCoverage = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    header: Readonly<{
      blockHash: string;
      parentBlockHash: string;
      blockNo: string;
      slot: string;
    }>;
  }>,
): WatcherLocalUserEventCoverage => {
  const owner = localOwner(input.history);
  const head = owner.entries.at(-1);
  if (
    head === undefined ||
    owner.checkpoint === null ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    owner.semanticReplay
  )
    return localRefuse("coverage requires a settled publication");
  const covered = localCoverageHead(owner);
  const { header } = input;
  if (
    !isHex32(header.blockHash) ||
    !isHex32(header.parentBlockHash) ||
    !isNatural(header.blockNo) ||
    !isNatural(header.slot) ||
    header.parentBlockHash !== covered.blockHash ||
    BigInt(header.blockNo) !== BigInt(covered.blockNo) + 1n ||
    BigInt(header.slot) <= BigInt(covered.slot)
  )
    return localRefuse(
      "quiet block is not the direct child of the covered head",
    );
  const point = admitFraudProofRawL1Point({
    blockHash: header.blockHash,
    blockNo: header.blockNo,
    slot: header.slot,
    pointId: computeFraudProofRawL1PointId({
      blockHash: header.blockHash,
      blockNo: header.blockNo,
      slot: header.slot,
    }),
  });
  owner.coverage = Object.freeze({
    point: Object.freeze({ ...point }),
    headEntryDigest: head.entryDigest,
  });
  return owner.coverage;
};

/**
 * Moves coverage back to a point at or above the head entry after a native
 * rollback whose fork lies inside the quiet stretch. The caller resolved the
 * point's block number and hash from the headers it admitted; a fork below
 * the head entry is not a coverage matter and goes through rollback recovery.
 */
export const rewindWatcherLocalUserEventCoverage = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    point: WatcherUserEventOriginFacts["parentPoint"];
  }>,
): WatcherLocalUserEventCoverage => {
  const owner = localOwner(input.history);
  const head = owner.entries.at(-1);
  if (
    head === undefined ||
    owner.checkpoint === null ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    owner.semanticReplay
  )
    return localRefuse("coverage requires a settled publication");
  const covered = localCoverageHead(owner);
  const point = admitFraudProofRawL1Point(input.point);
  if (
    BigInt(point.blockNo) > BigInt(covered.blockNo) ||
    BigInt(point.blockNo) < BigInt(head.cursor.blockNo) ||
    (point.blockNo === head.cursor.blockNo && !same(point, head.cursor))
  )
    return localRefuse("coverage rewind target is outside the covered stretch");
  owner.coverage = Object.freeze({
    point: Object.freeze({ ...point }),
    headEntryDigest: head.entryDigest,
  });
  return owner.coverage;
};

/**
 * Restores saved coverage over a restored head. A record that references an
 * older entry is a torn write between the head publication and the coverage
 * update; it is discarded and coverage restarts at the head. A record on the
 * current head that lies below it is a bug and fails loudly.
 */
export const restoreWatcherLocalUserEventCoverage = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    saved: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      headEntryDigest: string;
      checkpointDigest: string;
    }> | null;
  }>,
): Readonly<{
  coverage: WatcherLocalUserEventCoverage;
  disposition: "restored" | "head" | "discarded_stale";
}> => {
  const owner = localOwner(input.history);
  const head = owner.entries.at(-1);
  if (head === undefined || owner.checkpoint === null)
    return localRefuse("coverage requires a settled publication");
  const onHead = localCoverageOnHead(head);
  const saved = input.saved;
  if (saved === null) {
    owner.coverage = onHead;
    return Object.freeze({ coverage: onHead, disposition: "head" });
  }
  if (
    saved.headEntryDigest !== head.entryDigest ||
    saved.checkpointDigest !== owner.checkpoint.checkpointDigest
  ) {
    owner.coverage = onHead;
    return Object.freeze({ coverage: onHead, disposition: "discarded_stale" });
  }
  if (
    !isHex32(saved.blockHash) ||
    !isNatural(saved.blockNo) ||
    !isNatural(saved.slot) ||
    BigInt(saved.blockNo) < BigInt(head.cursor.blockNo) ||
    BigInt(saved.slot) < BigInt(head.cursor.slot) ||
    (saved.blockNo === head.cursor.blockNo &&
      saved.blockHash !== head.cursor.blockHash)
  )
    return localRefuse(
      "saved coverage lies below the head entry it references",
    );
  const point = admitFraudProofRawL1Point({
    blockHash: saved.blockHash,
    blockNo: saved.blockNo,
    slot: saved.slot,
    pointId: computeFraudProofRawL1PointId({
      blockHash: saved.blockHash,
      blockNo: saved.blockNo,
      slot: saved.slot,
    }),
  });
  owner.coverage = Object.freeze({
    point: Object.freeze({ ...point }),
    headEntryDigest: head.entryDigest,
  });
  return Object.freeze({ coverage: owner.coverage, disposition: "restored" });
};

const localLivePair = (owner: LocalHistoryOwner, pair: LocalPair) => {
  const scripts = readWatcherUserEventScriptBinding({
    binding: owner.scriptBinding,
    deploymentIdentity: owner.deploymentIdentity,
  });
  const current = readWatcherLocalBackfillFinalityObservation(pair);
  const witness = readWatcherLocalBackfillFinalityOriginalWitness(pair);
  const evidence = readWatcherUserEventReferenceEvidence(
    pair.referenceAuthority,
  );
  const referenceEvidence = admitWatcherLocalBackfillUserEventReferenceEvidence(
    {
      ...pair,
      evidence,
      deploymentIdentity: owner.deploymentIdentity,
    },
  );
  if (
    scripts !== owner.origin.scripts ||
    referenceEvidence !== evidence ||
    witness.current.finality !== current.finality ||
    witness.current.observation !== current.observation ||
    !same(current.finality.policy, owner.finalityPolicy) ||
    !same(
      current.observation.capture.sourceBinding,
      owner.origin.originalWitness.current.observation.capture.sourceBinding,
    ) ||
    current.observation.sourceIdentityDigest !==
      owner.origin.originalWitness.current.observation.sourceIdentityDigest ||
    witness.first.observation.capture.nativeBlock.rawBlockCbor !==
      current.observation.capture.nativeBlock.rawBlockCbor ||
    !same(
      witness.first.observation.capture.predecessorPoint,
      current.observation.capture.predecessorPoint,
    )
  ) {
    return localRefuse("live finality/reference/source binding differs");
  }
  return { witness, referenceEvidence };
};

/** Empty initialization is available only at the authenticated whole activation block. */
const createLocalUserEventHistory = (
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
const localAnchorDue = (owner: LocalHistoryOwner): boolean => {
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

export const prepareWatcherLocalUserEventTransition = (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      publication: WatcherProtectedUserEventCheckpoint;
    }>,
): WatcherLocalUserEventTransition => {
  const {
    history,
    publication: receipt,
    finality,
    observation,
    referenceAuthority,
  } = input;
  const owner = localOwner(history);
  if (owner.semanticReplay)
    return localRefuse("semantic replay has not been published");
  const publication = readWatcherProtectedUserEventCheckpointReceipt(receipt);
  if (!same(publication.checkpoint, owner.checkpoint))
    return localRefuse(
      "protected predecessor differs; semantic reconciliation required",
    );
  const transition = prepareLocalUserEventTransition(
    history,
    Object.freeze({ finality, observation, referenceAuthority }),
  );
  readWatcherProtectedUserEventCheckpointReceipt(receipt);
  return transition;
};

const prepareLocalUserEventTransition = (
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

export const readWatcherLocalUserEventTransition = (
  transition: WatcherLocalUserEventTransition,
): LocalPreparedRead => {
  const prepared =
    localTransitions.get(transition) ??
    localRefuse("transition is not privately admitted");
  const owner = localOwner(prepared.history);
  if (prepared.accepted) {
    if (
      owner.lastAccepted !== transition ||
      owner.checkpoint?.checkpointDigest !==
        prepared.value.nextCheckpoint.checkpointDigest
    )
      return localRefuse("accepted transition is no longer the head");
  } else if (
    owner.generation !== prepared.generation ||
    owner.candidate !== transition
  )
    return localRefuse("transition is no longer pending");
  const live = localLivePair(owner, prepared.pair);
  if (
    live.witness.first !== prepared.witness.first ||
    live.witness.current !== prepared.witness.current ||
    live.referenceEvidence !== prepared.referenceEvidence
  )
    return localRefuse("prepared evidence differs");
  return prepared.value;
};

/** Exact protected publication advances the private cursor once, never preparation. */
export const acceptWatcherLocalUserEventPublication = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    transition: WatcherLocalUserEventTransition;
    publication: WatcherProtectedUserEventCheckpoint;
  }>,
) => {
  const { history, transition, publication: publicationReceipt } = input;
  const owner = localOwner(history);
  const prepared =
    localTransitions.get(transition) ??
    localRefuse("transition is not privately admitted");
  const publication =
    readWatcherProtectedUserEventCheckpointReceipt(publicationReceipt);
  if (
    prepared.history !== history ||
    !same(publication.checkpoint, prepared.value.nextCheckpoint) ||
    publication.payload === null ||
    sha256Bytes(publication.payload) !==
      prepared.value.nextCheckpoint.payloadDigest
  )
    return localRefuse(
      "publication does not match the exact prepared frame and payload",
    );
  if (prepared.accepted) {
    if (
      owner.checkpoint?.checkpointDigest !==
      prepared.value.nextCheckpoint.checkpointDigest
    )
      return localRefuse("accepted publication is no longer the head");
    return Object.freeze({
      entryDigest: prepared.value.entry.entryDigest,
      cursor: prepared.value.entry.cursor,
    });
  }
  if (owner.semanticReplay)
    return localRefuse("semantic replay requires readmission publication");
  return commitLocalUserEventTransition(history, transition);
};

const commitLocalUserEventTransition = (
  history: WatcherLocalUserEventHistory,
  transition: WatcherLocalUserEventTransition,
) => {
  const owner = localOwner(history);
  const prepared =
    localTransitions.get(transition) ??
    localRefuse("transition is not privately admitted");
  if (prepared.history !== history || prepared.accepted)
    return localRefuse("transition is not a pending step of this owner");
  readWatcherLocalUserEventTransition(transition);
  owner.store = prepared.value.nextStore;
  owner.snapshot = prepared.value.snapshot;
  owner.entries = Object.freeze([...owner.entries, prepared.value.entry]);
  owner.coverage = localCoverageOnHead(prepared.value.entry);
  owner.acceptedEvidence = Object.freeze([
    ...owner.acceptedEvidence,
    Object.freeze({
      entry: prepared.value.entry,
      entryArchiveDigest: localArchiveObject({
        entry: prepared.value.entry,
        observation: prepared.value.observation,
      }).digest,
      rawBlockCbor:
        prepared.witness.current.observation.capture.nativeBlock.rawBlockCbor,
      pointDigest:
        prepared.witness.current.observation.native.chainPoint.pointDigest,
      chainPointId:
        prepared.witness.current.observation.native.chainPoint.chainPointId,
    }),
  ]);
  owner.lastAccepted = transition;
  owner.archiveObjects = prepared.value.archiveObjects;
  owner.checkpoint = prepared.value.nextCheckpoint;
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  owner.candidate = null;
  prepared.accepted = true;
  return Object.freeze({
    entryDigest: prepared.value.entry.entryDigest,
    cursor: prepared.value.entry.cursor,
  });
};

/** Closing an owner revokes every transition and event capability it issued. */
export const closeWatcherLocalUserEventHistory = (
  history: WatcherLocalUserEventHistory,
): void => {
  const owner =
    localHistories.get(history) ??
    localRefuse("history is not privately admitted");
  owner.closed = true;
};

/** Retire every issued capability synchronously before rollback recovery awaits. */
export const suspendWatcherLocalUserEventHistory = (
  history: WatcherLocalUserEventHistory,
): void => {
  const owner = localHistories.get(history) ?? localRefuse("unknown history");
  if (owner.closed) return localRefuse("history is closed");
  owner.generation += 1;
  owner.suspendedAt = performance.now();
};

/** Same-process recovery preserves the accepted fold only when new native W12
 * evidence and a genuine protected read still identify that exact publication. */
export const resumeWatcherLocalUserEventHistory = (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      publication: WatcherProtectedUserEventCheckpoint;
    }>,
): void => {
  const owner =
    localHistories.get(input.history) ?? localRefuse("unknown history");
  const head = owner.entries.at(-1);
  const accepted = owner.acceptedEvidence.at(-1);
  const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
    input.publication,
  );
  if (
    owner.closed ||
    owner.suspendedAt === null ||
    owner.semanticReplay ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    head === undefined ||
    accepted === undefined ||
    owner.checkpoint === null ||
    owner.snapshot.quarantined ||
    !same(protectedHead.checkpoint, owner.checkpoint) ||
    protectedHead.payload === null ||
    sha256Bytes(protectedHead.payload) !== owner.checkpoint.payloadDigest
  )
    return localRefuse("suspended history requires restart reconciliation");
  const { witness } = localLivePair(owner, input);
  if (
    witness.first.observation.capture.startedAtMonotonicMs <
      owner.suspendedAt ||
    !same(witness.current.observation.capture.point, head.cursor) ||
    witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      accepted.rawBlockCbor
  )
    return localRefuse(
      "rollback recovery does not freshly corroborate the accepted head",
    );
  readWatcherProtectedUserEventCheckpointReceipt(input.publication);
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  owner.suspendedAt = null;
};

const localUnavailableErrors = new WeakSet<Error>();
const localAuthorityUnavailable = (reason: string): never => {
  const error = new Error(`Local user-event authority unavailable: ${reason}`);
  localUnavailableErrors.add(error);
  throw error;
};
/** Only an ordinary candidate's unavailable event/header membership is recoverable.
 * Callers must still freshly fence protected-head, native lease and generation. */
export const isWatcherLocalUserEventAuthorityUnavailable = (
  error: unknown,
): error is Error =>
  error instanceof Error && localUnavailableErrors.has(error);

/** Refresh the protected checkpoint without requiring any particular event. */
export const assertWatcherLocalUserEventHeadCurrent = async (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      runtime: WatcherDurableRuntime;
    }>,
): Promise<void> => {
  const owner = localOwner(input.history);
  const generation = owner.generation;
  const checkpoint = owner.checkpoint;
  const assertCurrent = () => {
    const head = owner.entries.at(-1);
    const accepted = owner.acceptedEvidence.at(-1);
    const finality = input.runtime.read().currentFinalityState;
    if (
      localOwner(input.history) !== owner ||
      owner.generation !== generation ||
      owner.checkpoint !== checkpoint ||
      checkpoint === null ||
      owner.semanticReplay ||
      owner.candidate !== null ||
      owner.anchorCandidate !== null ||
      owner.snapshot.quarantined ||
      owner.acceptedAtMonotonicMs === null ||
      head === undefined ||
      accepted === undefined ||
      finality.phase === "quarantined" ||
      finality.incident !== null
    )
      return localRefuse("user-event protected head is no longer current");
    const { witness } = localLivePair(owner, input);
    if (
      witness.first.observation.capture.startedAtMonotonicMs <
        owner.acceptedAtMonotonicMs ||
      !same(witness.current.observation.capture.point, head.cursor) ||
      witness.current.observation.capture.nativeBlock.rawBlockCbor !==
        accepted.rawBlockCbor
    )
      return localRefuse(
        "user-event head lease is not fresh exact W12 evidence",
      );
  };
  assertCurrent();
  const receipt = await readWatcherProtectedUserEventCheckpoint(input.runtime);
  assertCurrent();
  const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(receipt);
  if (
    checkpoint === null ||
    !same(protectedHead.checkpoint, checkpoint) ||
    protectedHead.payload === null ||
    sha256Bytes(protectedHead.payload) !== checkpoint.payloadDigest
  )
    return localRefuse(
      "user-event protected head differs after candidate refusal",
    );
};

const localEventAuthorityBrand = Symbol("watcher-local-user-event-authority");
export type WatcherLocalUserEventAuthority = Readonly<{
  [localEventAuthorityBrand]: true;
}>;
export type WatcherLocalUserEventAuthorityRead = Readonly<{
  deploymentManifestId: string;
  blueprintHash: string;
  network: WatcherUserEventIndexerPolicy["network"];
  event: WatcherIndexedUserEvent | WatcherTerminalUserEvent;
  throughHeader: WatcherLocalUserEventHeaderCutoff | null;
  checkpointDigest: string;
  checkpointPayloadDigest: string;
  snapshotDigest: string;
  headEntryDigest: string;
  historyEntryDigests: readonly string[];
}>;
type LocalEventAuthorityOwner = Readonly<{
  history: WatcherLocalUserEventHistory;
  header: WatcherStateQueueHeaderObservation | null;
  runtime: WatcherDurableRuntime;
  pair: LocalPair;
  generation: number;
  value: WatcherLocalUserEventAuthorityRead;
  protectedRead: { receipt: WatcherProtectedUserEventCheckpoint | null };
}>;
const localEventAuthorities = new WeakMap<
  WatcherLocalUserEventAuthority,
  LocalEventAuthorityOwner
>();

export type WatcherLocalUserEventHeaderCutoff = Readonly<{
  headerHash: string;
  headerCborHex: string;
  queueOutRef: string;
  observedTransactionHash: string;
  observedBlockHash: string;
  observedSlot: string;
  observedBlockNo: string;
  transactionIndex: string;
  historyEntryDigest: string;
}>;

const localHeaderFields = (header: WatcherStateQueueHeaderObservation) => {
  assertWatcherStateQueueHeaderObservation(header);
  return Object.freeze({
    headerHash: header.headerHash,
    headerCborHex: header.headerCborHex,
    queueOutRef: header.queueOutRef,
    observedTransactionHash: header.observedTransactionHash,
    observedBlockHash: header.observedBlockHash,
    observedSlot: header.observedSlot,
    observedBlockNo: header.observedBlockNo,
  });
};

const localReadArchivedValue = async (
  archive: WatcherUserEventArchive,
  digest: string,
): Promise<unknown> => {
  if (!isHex32(digest))
    return localRefuse("historical cutoff archive digest differs");
  const bytes = await archive.read(digest);
  if (
    bytes === null ||
    bytes.byteLength >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes ||
    sha256Bytes(bytes) !== digest
  )
    return localRefuse("historical cutoff archive is absent or corrupt");
  let value: unknown;
  try {
    value = JSON.parse(new TextDecoder("utf-8", { fatal: true }).decode(bytes));
  } catch {
    return localRefuse("historical cutoff archive is not JSON");
  }
  if (
    !evidenceWithinBounds(value, { nodes: 0, bytes: 0 }) ||
    localArchiveObject(value).digest !== digest
  )
    return localRefuse("historical cutoff archive encoding or bounds differ");
  return value;
};

/** Parses and validates one sealed segment's retained-entry payload. */
const localArchivedSegmentEntries = async (
  owner: LocalHistoryOwner,
  archive: WatcherUserEventArchive,
  segment: WatcherUserEventArchiveIndexRead,
): Promise<readonly WatcherLocalUserEventEntry[]> => {
  const payload = await localReadArchivedValue(
    archive,
    segment.index.sourcePayloadDigest,
  );
  const entriesValue = localArchiveField(payload, ["retainedEntries"]);
  if (
    !Array.isArray(entriesValue) ||
    entriesValue.length === 0 ||
    entriesValue.length > Number(owner.policy.maximumActiveHistoryEntries) ||
    localArchiveField(payload, ["originDigest"]) !== owner.originDigest ||
    !same(localArchiveField(payload, ["policy"]), owner.policy)
  )
    return localRefuse("historical cutoff segment payload differs");
  const entries = entriesValue.map(localArchivedEntry);
  if (!same(localArchiveField(payload, ["head"]), entries.at(-1)))
    return localRefuse("historical cutoff segment head differs");
  return entries;
};

/**
 * Finds the sealed entry observed at `blockNo`, or null when no event block
 * was observed there. Entries are no longer dense in block numbers (quiet
 * blocks publish nothing), so sealed segments are bisected by the block
 * numbers of their first and last retained entries.
 */
const localArchivedEntryAtBlock = async (
  owner: LocalHistoryOwner,
  archive: WatcherUserEventArchive,
  root: WatcherUserEventArchiveIndexRead,
  blockNo: bigint,
): Promise<Readonly<{
  segment: WatcherUserEventArchiveIndexRead;
  entry: WatcherLocalUserEventEntry;
}> | null> => {
  let first = 0n;
  let last = BigInt(root.index.indexSequence);
  for (let iteration = 0; first <= last && iteration < 65; iteration += 1) {
    const middle = (first + last) / 2n;
    const segment = await findWatcherUserEventArchiveIndex(
      archive,
      root,
      middle.toString(),
    );
    // The sealed payload retains the suffix carried over from the previous
    // segment as well; only this segment's own entry range orders the search.
    const entries = (
      await localArchivedSegmentEntries(owner, archive, segment)
    ).filter(
      (candidate) =>
        BigInt(candidate.sequence) >=
          BigInt(segment.index.firstEntrySequence) &&
        BigInt(candidate.sequence) <= BigInt(segment.index.lastEntrySequence),
    );
    if (entries.length === 0)
      return localRefuse("historical cutoff segment range is absent");
    if (blockNo < BigInt(entries[0]!.cursor.blockNo)) last = middle - 1n;
    else if (blockNo > BigInt(entries.at(-1)!.cursor.blockNo))
      first = middle + 1n;
    else {
      const matches = entries.filter(
        (candidate) => BigInt(candidate.cursor.blockNo) === blockNo,
      );
      if (matches.length > 1)
        return localRefuse("historical cutoff entry is not uniquely archived");
      return matches.length === 0
        ? null
        : Object.freeze({ segment, entry: matches[0]! });
    }
  }
  return null;
};

/**
 * Looks up the accepted event block at a block number: the retained suffix
 * first, then the sealed archive. Null means the block lies inside the
 * published range but was quiet, so nothing was observed there.
 */
const localEntryAtBlock = async (
  owner: LocalHistoryOwner,
  blockNo: bigint,
  archive: WatcherUserEventArchive,
): Promise<Readonly<{
  entry: WatcherLocalUserEventEntry;
  rawBlockCbor: unknown;
}> | null> => {
  const head = owner.entries.at(-1)!;
  if (
    blockNo < BigInt(owner.origin.block.chainPoint.blockNo) ||
    blockNo > BigInt(head.cursor.blockNo)
  )
    return localRefuse(
      "header cutoff lies outside the published event history",
    );
  const retained = localRetainedEvidence(owner).find(
    ({ entry }) => BigInt(entry.cursor.blockNo) === blockNo,
  );
  if (retained !== undefined)
    return Object.freeze({
      entry: retained.entry,
      rawBlockCbor: retained.rawBlockCbor,
    });
  // Pinned evidence reaches below the retained suffix, so only the suffix's
  // own oldest entry bounds the range where an absent entry means quiet.
  if (blockNo > BigInt(owner.entries[0]!.cursor.blockNo)) return null;
  const root = owner.archiveIndex;
  if (root === null) return null;
  const found = await localArchivedEntryAtBlock(owner, archive, root, blockNo);
  if (found === null) return null;
  const { segment, entry } = found;
  if (!segment.index.sourceArchiveDigests.includes(entry.evidenceDigest))
    return localRefuse(
      "historical cutoff evidence is not in the sealed closure",
    );
  const evidence = await localReadArchivedValue(archive, entry.evidenceDigest);
  if (
    localArchiveField(evidence, ["schemaVersion"]) !==
      "midgard-watcher-local-user-event-block-evidence-v1" ||
    localArchiveField(evidence, ["numericEncoding"]) !== "exact-decimal-strings"
  )
    return localRefuse("historical cutoff evidence framing differs");
  const rawBlockCbor = localArchiveField(evidence, [
    "witnesses",
    "current",
    "observation",
    "capture",
    "nativeBlock",
    "rawBlockCbor",
  ]);
  for (const step of ["first", "current"] as const) {
    if (
      localArchiveField(evidence, [
        "witnesses",
        step,
        "observation",
        "capture",
        "nativeBlock",
        "rawBlockCbor",
      ]) !== rawBlockCbor ||
      !same(
        localArchiveField(evidence, [
          "witnesses",
          step,
          "observation",
          "capture",
          "point",
        ]),
        entry.cursor,
      ) ||
      !same(
        localArchiveField(evidence, [
          "witnesses",
          step,
          "observation",
          "capture",
          "predecessorPoint",
        ]),
        entry.parent,
      )
    )
      return localRefuse("historical cutoff original witness binding differs");
  }
  return Object.freeze({ entry, rawBlockCbor });
};

const localHeaderBlock = async (
  owner: LocalHistoryOwner,
  header: Pick<
    ReturnType<typeof localHeaderFields>,
    "observedBlockHash" | "observedBlockNo" | "observedSlot"
  >,
  archive: WatcherUserEventArchive,
): Promise<
  Readonly<{ entry: WatcherLocalUserEventEntry; rawBlockCbor: string }>
> => {
  const originDigest = owner.originDigest;
  const policyDigest = owner.policy.policyDigest;
  const found = await localEntryAtBlock(
    owner,
    BigInt(header.observedBlockNo),
    archive,
  );
  if (found === null)
    return localRefuse("header cutoff block carried no observed event");
  const { entry, rawBlockCbor } = found;
  if (
    entry.originDigest !== originDigest ||
    entry.policyDigest !== policyDigest ||
    entry.cursor.blockHash !== header.observedBlockHash ||
    entry.cursor.slot !== header.observedSlot ||
    entry.cursor.blockNo !== header.observedBlockNo ||
    typeof rawBlockCbor !== "string" ||
    !isHexBytes(rawBlockCbor)
  )
    return localRefuse("header cutoff is not the exact accepted block");
  return Object.freeze({ entry, rawBlockCbor });
};

/** Pure decoding of the already admitted lineage's original bytes. This creates
 * neither a native acquisition receipt nor fresh W12 finality authority.
 */
const localCutoffTransactionOrder = (
  block: Readonly<{ entry: WatcherLocalUserEventEntry; rawBlockCbor: string }>,
) => {
  const decoded = CML.Block.from_cbor_hex(block.rawBlockCbor);
  const header = decoded.header();
  const body = header.header_body();
  if (
    decoded.to_cbor_hex() !== block.rawBlockCbor ||
    Buffer.from(
      blake2b(Buffer.from(header.to_cbor_hex(), "hex"), { dkLen: 32 }),
    ).toString("hex") !== block.entry.cursor.blockHash ||
    body.slot().toString() !== block.entry.cursor.slot ||
    body.block_number().toString() !== block.entry.cursor.blockNo ||
    body.prev_hash()?.to_hex() !== block.entry.parent.blockHash
  )
    return localRefuse("historical cutoff raw header differs");
  const bodies = decoded.transaction_bodies();
  const transactionIds = Array.from({ length: bodies.len() }, (_, index) =>
    CML.hash_transaction(bodies.get(index)).to_hex(),
  );
  if (new Set(transactionIds).size !== transactionIds.length)
    return localRefuse("historical cutoff transaction order is ambiguous");
  return {
    bodies,
    transactionIds,
    invalidTransactions: new Set(decoded.invalid_transactions()),
  };
};

const localEventAtHeaderCutoff = async (
  owner: LocalHistoryOwner,
  event: WatcherIndexedUserEvent,
  terminal: WatcherTerminalUserEvent | null,
  originEvidence: LocalRetainedEvidence,
  terminalEvidence: LocalRetainedEvidence,
  header: WatcherStateQueueHeaderObservation,
  archive: WatcherUserEventArchive,
) => {
  const fields = localHeaderFields(header);
  const block = await localHeaderBlock(owner, fields, archive);
  const ordered = localCutoffTransactionOrder(block);
  const transactionIndex = ordered.transactionIds.indexOf(
    fields.observedTransactionHash,
  );
  if (transactionIndex < 0 || ordered.invalidTransactions.has(transactionIndex))
    return localRefuse("header cutoff transaction is not validly included");
  const outRef = fields.queueOutRef.split("#");
  if (
    outRef.length !== 2 ||
    outRef[0] !== fields.observedTransactionHash ||
    !isNatural(outRef[1]) ||
    BigInt(outRef[1]) >=
      BigInt(ordered.bodies.get(transactionIndex).outputs().len())
  )
    return localRefuse("header cutoff output reference differs");
  const output = ordered.bodies
    .get(transactionIndex)
    .outputs()
    .get(Number(outRef[1]));
  if (
    output.datum()?.as_datum()?.to_canonical_cbor_hex() !==
      header.linkedListDatumCborHex ||
    Buffer.from(
      blake2b(Buffer.from(fields.headerCborHex, "hex"), { dkLen: 28 }),
    ).toString("hex") !== fields.headerHash
  )
    return localRefuse("header cutoff output or header bytes differ");
  const occursThroughHeader = (
    evidence: LocalRetainedEvidence,
    transactionHash: string,
  ): boolean => {
    const entrySequence = BigInt(evidence.entry.sequence);
    if (entrySequence !== BigInt(block.entry.sequence))
      return entrySequence < BigInt(block.entry.sequence);
    if (!same(evidence.entry, block.entry))
      return localRefuse("event cutoff entry membership differs");
    const index = ordered.transactionIds.indexOf(transactionHash);
    if (index < 0 || ordered.invalidTransactions.has(index))
      return localRefuse("event cutoff transaction is not validly included");
    return index <= transactionIndex;
  };
  // Pointer continuations replace transactionHash/outRef, but admission remains
  // the unique valid transaction that consumed the immutable event nonce in
  // the authenticated origin block. Archived bytes describe this live owner's
  // accepted lineage; they do not establish a new origin or fresh authority.
  const originOrder = localCutoffTransactionOrder(originEvidence);
  const admissions = originOrder.transactionIds.filter((_, index) => {
    if (originOrder.invalidTransactions.has(index)) return false;
    const inputs = originOrder.bodies.get(index).inputs();
    for (let inputIndex = 0; inputIndex < inputs.len(); inputIndex += 1) {
      if (outputReference(inputs.get(inputIndex)) === event.nonceOutRef)
        return true;
    }
    return false;
  });
  if (admissions.length !== 1)
    return localRefuse("event origin nonce is not uniquely consumed");
  if (!occursThroughHeader(originEvidence, admissions[0]!))
    return localAuthorityUnavailable(
      "event origin occurs after the challenged header",
    );
  let selected: WatcherIndexedUserEvent | WatcherTerminalUserEvent = event;
  let includeTerminal = false;
  if (terminal !== null) {
    includeTerminal = occursThroughHeader(
      terminalEvidence,
      terminal.terminalTransactionHash,
    );
    if (includeTerminal) selected = terminal;
    else {
      const {
        terminalStatus: _status,
        terminalTransactionHash: _tx,
        terminalPointDigest: _point,
        terminalBlockHash: _hash,
        terminalSlot: _slot,
        terminalBlockNo: _number,
        terminalFinalityStatus: _finality,
        terminalClassification: _classification,
        ...origin
      } = terminal;
      selected = Object.freeze(origin);
    }
  }
  if (!same(localHeaderFields(header), fields))
    return localRefuse("header cutoff changed during its archive read");
  return Object.freeze({
    event: selected,
    includeTerminal,
    throughHeader: Object.freeze({
      ...fields,
      transactionIndex: transactionIndex.toString(),
      historyEntryDigest: block.entry.entryDigest,
    }),
  });
};

const localEventAuthorityCurrent = (
  authority: LocalEventAuthorityOwner,
): LocalHistoryOwner => {
  const owner = localOwner(authority.history);
  if (authority.header !== null) {
    const cutoff = authority.value.throughHeader;
    if (cutoff === null) return localRefuse("event header cutoff is absent");
    const {
      transactionIndex: _index,
      historyEntryDigest: _entry,
      ...fields
    } = cutoff;
    if (!same(localHeaderFields(authority.header), fields))
      return localRefuse("event header cutoff is no longer identical");
  }
  const head = owner.entries.at(-1);
  const accepted = owner.acceptedEvidence.at(-1);
  if (
    owner.semanticReplay ||
    owner.generation !== authority.generation ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    owner.checkpoint === null ||
    owner.acceptedAtMonotonicMs === null ||
    owner.snapshot.quarantined ||
    head === undefined ||
    accepted === undefined ||
    owner.checkpoint.checkpointDigest !== authority.value.checkpointDigest ||
    head.entryDigest !== authority.value.headEntryDigest
  )
    return localRefuse(
      "event authority no longer matches the published semantic head",
    );
  // This is a new corroboration, acquired after publication. Original archived
  // W12 observations and depths remain unchanged and are never revived from JSON.
  const { witness } = localLivePair(owner, authority.pair);
  if (
    witness.first.observation.capture.startedAtMonotonicMs <
      owner.acceptedAtMonotonicMs ||
    !same(witness.current.observation.capture.point, head.cursor) ||
    witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      accepted.rawBlockCbor
  )
    return localRefuse(
      "event authority requires a fresh post-publication capture of the exact head",
    );
  return owner;
};

/** Final synchronous fence after all asynchronous authority reads. This checks
 * same-runtime protected-head changes as well as closure, source liveness and
 * private owner generation. The async reader remains necessary for disk freshness.
 */
export const assertWatcherLocalUserEventAuthorityCurrent = (
  receipt: WatcherLocalUserEventAuthority,
): void => {
  const authority =
    localEventAuthorities.get(receipt) ??
    localRefuse("event authority is not privately admitted");
  const owner = localEventAuthorityCurrent(authority);
  const publication = authority.protectedRead.receipt;
  const finality = authority.runtime.read().currentFinalityState;
  if (
    publication === null ||
    finality.phase === "quarantined" ||
    finality.incident !== null ||
    !same(
      readWatcherProtectedUserEventCheckpointReceipt(publication).checkpoint,
      owner.checkpoint,
    )
  )
    return localRefuse(
      "event authority protected checkpoint is no longer current",
    );
};

/** Descriptive output is never accepted as authority. Each read refreshes the
 * protected head and checks the still-live post-publication capture after await.
 * The runtime owner must close this history on a source rollback or shutdown.
 */
export const readWatcherLocalUserEventAuthority = async (
  receipt: WatcherLocalUserEventAuthority,
): Promise<WatcherLocalUserEventAuthorityRead> => {
  const authority =
    localEventAuthorities.get(receipt) ??
    localRefuse("event authority is not privately admitted");
  localEventAuthorityCurrent(authority);
  const publication = await readWatcherProtectedUserEventCheckpoint(
    authority.runtime,
  );
  const owner = localEventAuthorityCurrent(authority);
  const protectedHead =
    readWatcherProtectedUserEventCheckpointReceipt(publication);
  const finalityState = authority.runtime.read().currentFinalityState;
  if (
    finalityState.phase === "quarantined" ||
    finalityState.incident !== null ||
    !same(protectedHead.checkpoint, owner.checkpoint) ||
    protectedHead.payload === null ||
    sha256Bytes(protectedHead.payload) !==
      authority.value.checkpointPayloadDigest
  )
    return localRefuse(
      "event authority protected checkpoint is no longer current",
    );
  authority.protectedRead.receipt = publication;
  return authority.value;
};

/** Verify an older stream intersection against the accepted private/archive
 * lineage. A matching height alone never permits skipping native blocks. */
export const assertWatcherLocalUserEventPointCovered = async (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    point: WatcherUserEventOriginFacts["parentPoint"];
    runtime: WatcherDurableRuntime;
    archive: WatcherUserEventArchive;
  }>,
): Promise<WatcherLocalUserEventPointCoverage> => {
  const owner = localOwner(input.history);
  const generation = owner.generation;
  const checkpoint = owner.checkpoint;
  if (
    checkpoint === null ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null
  )
    return localRefuse("coverage requires a settled publication");
  const point = admitFraudProofRawL1Point(input.point);
  const head = owner.entries.at(-1)!;
  if (BigInt(point.blockNo) > BigInt(head.cursor.blockNo))
    return localRefuse(
      "point lies above the head entry; coverage of the quiet stretch is a runtime lookup",
    );
  const found = await localEntryAtBlock(
    owner,
    BigInt(point.blockNo),
    input.archive,
  );
  if (found !== null) {
    // An event block: the point must be exactly the accepted block.
    const block = await localHeaderBlock(
      owner,
      {
        observedBlockHash: point.blockHash,
        observedBlockNo: point.blockNo,
        observedSlot: point.slot,
      },
      input.archive,
    );
    localCutoffTransactionOrder(block);
  }
  // A quiet block at or below the head entry lies inside the strict successor
  // chain the head's lineage established. Callers below the release-final
  // boundary need no hash check; the head entry's canonical corroboration
  // already fixes every ancestor by construction.
  const publication = readWatcherProtectedUserEventCheckpointReceipt(
    await readWatcherProtectedUserEventCheckpoint(input.runtime),
  );
  const finality = input.runtime.read().currentFinalityState;
  if (
    localOwner(input.history) !== owner ||
    owner.generation !== generation ||
    owner.checkpoint !== checkpoint ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    finality.phase === "quarantined" ||
    finality.incident !== null ||
    !same(publication.checkpoint, checkpoint) ||
    publication.payload === null ||
    sha256Bytes(publication.payload) !== checkpoint.payloadDigest
  )
    return localRefuse("coverage changed during protected archive read");
  return found === null ? "quiet" : "event";
};

/**
 * How a point at or below the head entry is covered: "event" when the exact
 * accepted block was observed there, "quiet" when the block lies inside the
 * linked stretch between observations. A quiet point above the release-final
 * boundary still needs its hash confirmed against the canonical chain, which
 * the runtime resolves with one node lookup on demand.
 */
export type WatcherLocalUserEventPointCoverage = "event" | "quiet";

/** Issues one retained event from the privately published whole-block fold. */
export const admitWatcherLocalUserEventAuthority = async (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      runtime: WatcherDurableRuntime;
      eventId: string;
      kind: WatcherUserEventKind;
      throughHeader?: WatcherStateQueueHeaderObservation;
      archive?: WatcherUserEventArchive;
    }>,
): Promise<WatcherLocalUserEventAuthority> => {
  const {
    history,
    runtime,
    eventId,
    kind,
    throughHeader,
    archive,
    finality,
    observation,
    referenceAuthority,
  } = input;
  const owner = localOwner(history);
  const generation = owner.generation;
  const checkpoint = owner.checkpoint;
  const head = owner.entries.at(-1);
  if (checkpoint === null || head === undefined)
    return localRefuse("event authority requires a published history");
  const matches = [
    ...owner.snapshot.activeEvents,
    ...owner.snapshot.terminalEvents,
  ].filter((event) => event.eventId === eventId && event.kind === kind);
  if (matches.length === 0)
    return localAuthorityUnavailable("event is not retained");
  if (matches.length !== 1)
    return localRefuse("event is not uniquely retained");
  const event = matches[0]!;
  const retainedEvidence = localRetainedEvidence(owner);
  const originIndex = retainedEvidence.findIndex(
    ({ pointDigest }) => pointDigest === event.originPointDigest,
  );
  const terminalIndex =
    "terminalPointDigest" in event
      ? retainedEvidence.findIndex(
          ({ pointDigest }) => pointDigest === event.terminalPointDigest,
        )
      : originIndex;
  if (
    originIndex < 0 ||
    terminalIndex < originIndex ||
    event.finalityStatus !== "final" ||
    ("terminalFinalityStatus" in event &&
      event.terminalFinalityStatus !== "final")
  )
    return localRefuse(
      "event origin or terminal membership is absent from finalized history",
    );
  const terminal =
    owner.snapshot.terminalEvents.find(
      (candidate) => candidate.eventId === eventId && candidate.kind === kind,
    ) ?? null;
  const scoped =
    throughHeader === undefined
      ? null
      : await localEventAtHeaderCutoff(
          owner,
          event,
          terminal,
          retainedEvidence[originIndex]!,
          retainedEvidence[terminalIndex]!,
          throughHeader,
          archive ?? localRefuse("header cutoff requires the history archive"),
        );
  if (
    localOwner(history) !== owner ||
    owner.generation !== generation ||
    owner.checkpoint !== checkpoint ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null
  )
    return localRefuse("history changed during header cutoff acquisition");
  const receipt = Object.freeze({ [localEventAuthorityBrand]: true as const });
  localEventAuthorities.set(
    receipt,
    Object.freeze({
      history,
      header: throughHeader ?? null,
      runtime,
      pair: Object.freeze({ finality, observation, referenceAuthority }),
      generation,
      protectedRead: { receipt: null },
      value: Object.freeze({
        deploymentManifestId: owner.origin.deploymentFingerprint,
        blueprintHash: owner.policy.blueprintHash,
        network: owner.policy.network,
        event: scoped?.event ?? event,
        throughHeader: scoped?.throughHeader ?? null,
        checkpointDigest: checkpoint.checkpointDigest,
        checkpointPayloadDigest: checkpoint.payloadDigest,
        snapshotDigest: owner.snapshot.snapshotDigest,
        headEntryDigest: head.entryDigest,
        historyEntryDigests: Object.freeze([
          ...new Set([
            retainedEvidence[originIndex]!.entry.entryDigest,
            ...(scoped === null || scoped.includeTerminal
              ? [retainedEvidence[terminalIndex]!.entry.entryDigest]
              : []),
            ...(scoped === null
              ? []
              : [scoped.throughHeader.historyEntryDigest]),
          ]),
        ]),
      }),
    }),
  );
  await readWatcherLocalUserEventAuthority(receipt);
  return receipt;
};

const localReadmissionBrand = Symbol("watcher-local-user-event-readmission");
export type WatcherLocalUserEventReadmission = Readonly<{
  [localReadmissionBrand]: true;
}>;
export type WatcherLocalUserEventReplaySource = (
  point: WatcherUserEventOriginFacts["parentPoint"],
) => Promise<LocalPair & Readonly<{ close(): Promise<void> }>>;
type LocalReadmissionOwner = {
  readonly runtime: WatcherDurableRuntime;
  readonly history: WatcherLocalUserEventHistory;
  readonly pair: LocalPair;
  readonly generation: number;
  readonly previousCheckpoint: WatcherUserEventCheckpoint;
  readonly nextCheckpoint: WatcherUserEventCheckpoint;
  readonly archiveObjects: readonly LocalArchiveObject[];
  readonly release: () => Promise<void>;
  accepted: boolean;
};
const localReadmissions = new WeakMap<
  WatcherLocalUserEventReadmission,
  LocalReadmissionOwner
>();

const localArchiveField = (
  value: unknown,
  keys: readonly string[],
): unknown => {
  let current = value;
  for (const key of keys) {
    if (
      typeof current !== "object" ||
      current === null ||
      Array.isArray(current)
    )
      return localRefuse("archive field is absent");
    const descriptor = Object.getOwnPropertyDescriptor(current, key);
    if (descriptor === undefined || !("value" in descriptor))
      return localRefuse("archive field is absent");
    current = descriptor.value;
  }
  return current;
};

const localArchivedEntry = (value: unknown): WatcherLocalUserEventEntry => {
  const record = exactRecord(value, [
    "schemaVersion",
    "sequence",
    "originDigest",
    "policyDigest",
    "predecessorEntryDigest",
    "predecessorStateDigest",
    "cursor",
    "parent",
    "sourceStoreDigest",
    "nextStoreDigest",
    "sourceStoreRevision",
    "nextStoreRevision",
    "observationDigest",
    "snapshotDigest",
    "evidenceDigest",
    "entryDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== "midgard-watcher-local-user-event-entry-v1" ||
    !isNatural(record.sequence) ||
    record.sequence.length > 20 ||
    !isNatural(record.sourceStoreRevision) ||
    record.sourceStoreRevision.length > 20 ||
    !isNatural(record.nextStoreRevision) ||
    record.nextStoreRevision.length > 20 ||
    !isHex32(record.originDigest) ||
    !isHex32(record.policyDigest) ||
    !(
      record.predecessorEntryDigest === null ||
      isHex32(record.predecessorEntryDigest)
    ) ||
    !(
      record.predecessorStateDigest === null ||
      isHex32(record.predecessorStateDigest)
    ) ||
    !isHex32(record.sourceStoreDigest) ||
    !isHex32(record.nextStoreDigest) ||
    !isHex32(record.observationDigest) ||
    !isHex32(record.snapshotDigest) ||
    !isHex32(record.evidenceDigest) ||
    !isHex32(record.entryDigest)
  )
    return localRefuse("archive entry framing differs");
  const entry = Object.freeze({
    schemaVersion: record.schemaVersion,
    sequence: record.sequence,
    originDigest: record.originDigest,
    policyDigest: record.policyDigest,
    predecessorEntryDigest: record.predecessorEntryDigest,
    predecessorStateDigest: record.predecessorStateDigest,
    cursor: Object.freeze(admitFraudProofRawL1Point(record.cursor)),
    parent: Object.freeze(admitFraudProofRawL1Point(record.parent)),
    sourceStoreDigest: record.sourceStoreDigest,
    nextStoreDigest: record.nextStoreDigest,
    sourceStoreRevision: record.sourceStoreRevision,
    nextStoreRevision: record.nextStoreRevision,
    observationDigest: record.observationDigest,
    snapshotDigest: record.snapshotDigest,
    evidenceDigest: record.evidenceDigest,
    entryDigest: record.entryDigest,
  });
  const { entryDigest, ...fields } = entry;
  if (sha256Canonical(fields) !== entryDigest)
    return localRefuse("archive entry digest differs");
  return entry;
};

/** Compare only replay-stable semantics. The three original acquisition-derived
 * point commitments remain checked/retained as archived values, and are never
 * relabelled as commitments produced by the fresh W12 observations.
 */
const localStableSnapshot = (value: unknown): unknown => {
  const snapshot = exactRecord(value, [
    "schemaVersion",
    "activeEvents",
    "terminalEvents",
    "quarantined",
    "snapshotDigest",
  ]);
  if (
    snapshot === null ||
    snapshot.schemaVersion !== WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION ||
    snapshot.quarantined !== false ||
    !Array.isArray(snapshot.activeEvents) ||
    !Array.isArray(snapshot.terminalEvents) ||
    snapshot.activeEvents.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.activeEvents ||
    snapshot.terminalEvents.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.terminalEvents ||
    !isHex32(snapshot.snapshotDigest)
  )
    return localRefuse("archive snapshot framing differs");
  const { snapshotDigest, ...fields } = snapshot;
  if (sha256Canonical(fields) !== snapshotDigest)
    return localRefuse("archive snapshot digest differs");
  const stableEvent = (value: unknown, terminal: boolean) => {
    const baseKeys = [
      "kind",
      "eventId",
      "outRef",
      "transactionHash",
      "outputIndex",
      "nonceOutRef",
      "policyId",
      "spendScriptHash",
      "addressHex",
      "assetNameHex",
      ...(typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, "witnessScriptHash")
        ? ["witnessScriptHash"]
        : []),
      ...(typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, "historyPayloadCborHex")
        ? ["historyPayloadCborHex"]
        : []),
      "inclusionTime",
      "eventCborHex",
      "datumCborHex",
      "outputCborHex",
      "eventContentDigest",
      "datumDigest",
      "outputDigest",
      "originPointDigest",
      "originChainPointId",
      "originBlockHash",
      "originSlot",
      "originBlockNo",
      "finalityStatus",
    ];
    const classification =
      typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, "terminalClassification");
    const event = exactRecord(value, [
      ...baseKeys,
      ...(terminal
        ? [
            "terminalStatus",
            "terminalTransactionHash",
            "terminalPointDigest",
            "terminalBlockHash",
            "terminalSlot",
            "terminalBlockNo",
            "terminalFinalityStatus",
            ...(classification ? ["terminalClassification"] : []),
          ]
        : []),
    ]);
    if (
      event === null ||
      (event.kind === "forced_order"
        ? !isHex28(event.witnessScriptHash) ||
          Object.hasOwn(event, "historyPayloadCborHex")
        : (event.kind !== "deposit" && event.kind !== "withdrawal") ||
          !isHexBytes(event.historyPayloadCborHex) ||
          Object.hasOwn(event, "witnessScriptHash")) ||
      !isHex32(event.originPointDigest) ||
      !isHex32(event.originChainPointId) ||
      event.finalityStatus !== "final" ||
      (terminal &&
        (!isHex32(event.terminalPointDigest) ||
          event.terminalFinalityStatus !== "final"))
    )
      return localRefuse("archive event framing differs");
    const {
      originPointDigest: _originPointDigest,
      originChainPointId: _originChainPointId,
      terminalPointDigest: _terminalPointDigest,
      terminalClassification,
      ...stable
    } = event;
    if (classification) {
      const decoded = exactRecord(terminalClassification, [
        "schemaVersion",
        "operatorValidity",
        "terminalTransactionHash",
        "terminalPointDigest",
      ]);
      if (
        decoded === null ||
        decoded.terminalPointDigest !== event.terminalPointDigest
      )
        return localRefuse("archive terminal classification differs");
      const {
        terminalPointDigest: _classificationPoint,
        ...stableClassification
      } = decoded;
      return { ...stable, terminalClassification: stableClassification };
    }
    return stable;
  };
  return {
    schemaVersion: snapshot.schemaVersion,
    quarantined: false,
    activeEvents: snapshot.activeEvents.map((event) =>
      stableEvent(event, false),
    ),
    terminalEvents: snapshot.terminalEvents.map((event) =>
      stableEvent(event, true),
    ),
  };
};

const localStableEventStore = (store: WatcherDurableStore): unknown => {
  const points = new Map(
    store.chainPoints.map((point) => [point.chainPointId, point]),
  );
  const stablePoint = (id: string) => {
    const point =
      points.get(id) ?? localRefuse("event store point dependency is absent");
    return {
      providerId: point.providerId,
      blockHash: point.blockHash,
      slot: point.slot,
      blockNo: point.blockNo,
    };
  };
  const {
    chainPoints,
    l1Observations,
    protocolUtxos,
    spentProtocolUtxos,
    caches: _caches,
    ...rest
  } = store;
  const order = (values: readonly unknown[]) =>
    [...values].sort((a, b) =>
      watcherCanonicalJson(a).localeCompare(watcherCanonicalJson(b)),
    );
  return {
    ...rest,
    chainPoints: order(
      chainPoints.map((point) => stablePoint(point.chainPointId)),
    ),
    l1Observations: order(
      l1Observations.map((row) => ({
        providerId: row.providerId,
        point: stablePoint(row.chainPointId),
      })),
    ),
    protocolUtxos: protocolUtxos.map(({ chainPointId, ...utxo }) => ({
      ...utxo,
      point: stablePoint(chainPointId),
    })),
    spentProtocolUtxos: spentProtocolUtxos.map(
      ({ chainPointId, spentAtChainPointId, ...utxo }) => ({
        ...utxo,
        point: stablePoint(chainPointId),
        spentAt: stablePoint(spentAtChainPointId),
      }),
    ),
  };
};

const localStableOrigin = (value: unknown): unknown => ({
  schemaVersion: localArchiveField(value, ["schemaVersion"]),
  deploymentFingerprint: localArchiveField(value, ["deploymentFingerprint"]),
  blueprintHash: localArchiveField(value, ["blueprintHash"]),
  blueprintSha256: localArchiveField(value, ["blueprintSha256"]),
  network: localArchiveField(value, ["network"]),
  canonicalOneShotOutRef: localArchiveField(value, ["canonicalOneShotOutRef"]),
  scripts: localArchiveField(value, ["scripts"]),
  parentPoint: localArchiveField(value, ["parentPoint"]),
  activation: localArchiveField(value, ["activation"]),
});

/** A protected archive is descriptive input. Only the actual fresh W12 pairs
 * drive this private fold; archived observations never become W12 receipts.
 * Indexed sealed segments are replayed chronologically with bounded live state.
 */
export const prepareWatcherLocalUserEventReadmission = async (
  input: Omit<
    Parameters<typeof createWatcherLocalUserEventHistory>[0],
    "publication"
  > &
    Readonly<{
      referenceAuthority: WatcherUserEventReferenceAuthority;
      runtime: WatcherDurableRuntime;
      archive: WatcherUserEventArchive;
      replayBlock: WatcherLocalUserEventReplaySource;
    }>,
): Promise<WatcherLocalUserEventReadmission> => {
  const {
    origin,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
    referenceAuthority,
    runtime,
    archive,
    replayBlock,
  } = input;
  const firstPair = Object.freeze({
    finality,
    observation,
    referenceAuthority,
  });
  const publication = await readWatcherProtectedUserEventCheckpoint(runtime);
  const protectedHead =
    readWatcherProtectedUserEventCheckpointReceipt(publication);
  const previousCheckpoint = protectedHead.checkpoint;
  const runtimeFinality = runtime.read().currentFinalityState;
  if (
    runtimeFinality.phase === "quarantined" ||
    runtimeFinality.incident !== null
  )
    return localRefuse("semantic readmission runtime is quarantined");
  if (previousCheckpoint === null || protectedHead.payload === null)
    return localRefuse("semantic readmission requires a published checkpoint");
  const history = createLocalUserEventHistory({
    origin,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
    publication,
    semanticReplay: true,
  });
  const owner = localOwner(history);
  const replayState: {
    retainedSource: Awaited<
      ReturnType<WatcherLocalUserEventReplaySource>
    > | null;
    priorIndex: WatcherUserEventArchiveIndexRead | null;
  } = { retainedSource: null, priorIndex: null };
  try {
    if (
      previousCheckpoint.userEventPolicyDigest !== owner.policy.policyDigest ||
      previousCheckpoint.finalityPolicyDigest !==
        owner.finalityPolicy.policyDigest ||
      previousCheckpoint.blueprintHash !== owner.policy.blueprintHash ||
      previousCheckpoint.network !== owner.policy.network
    )
      return localRefuse("archived policy differs from the fresh deployment");
    const bootstrapStore = owner.store;
    const readClosure = async (requiredDigests: readonly string[]) => {
      const objects = new Map<
        string,
        Readonly<{ object: LocalArchiveObject; value: unknown }>
      >();
      const budget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
      let bytesRead = 0;
      for (const digest of requiredDigests) {
        const bytes = await archive.read(digest);
        if (bytes === null || sha256Bytes(bytes) !== digest)
          return localRefuse("archived closure is absent or corrupt");
        bytesRead += bytes.byteLength;
        if (
          bytesRead > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes
        )
          return localRefuse("archived closure byte bound exceeded");
        let value: unknown;
        try {
          value = JSON.parse(
            new TextDecoder("utf-8", { fatal: true }).decode(bytes),
          );
        } catch {
          return localRefuse("archived closure is not canonical JSON");
        }
        if (!evidenceWithinBounds(value, budget))
          return localRefuse("archived closure evidence bound exceeded");
        const object = localArchiveObject(value);
        if (object.digest !== digest)
          return localRefuse("archived closure JSON encoding differs");
        objects.set(digest, Object.freeze({ object, value }));
      }
      return objects;
    };
    const readValue = async (digest: string): Promise<unknown> => {
      const objects = await readClosure([digest]);
      return objects.get(digest)!.value;
    };
    const parsePayload = (payloadValue: unknown) => {
      const hasReadmission =
        typeof payloadValue === "object" &&
        payloadValue !== null &&
        Object.hasOwn(payloadValue, "readmission");
      const payload = exactRecord(payloadValue, [
        "schemaVersion",
        "originArchiveDigest",
        "originDigest",
        "policy",
        "anchor",
        "head",
        "storeArchiveDigest",
        "snapshot",
        "retainedEntries",
        "requiredSemanticResume",
        ...(hasReadmission ? ["readmission"] : []),
      ]);
      if (
        payload === null ||
        payload.schemaVersion !==
          "midgard-watcher-local-user-event-checkpoint-payload-v1" ||
        payload.requiredSemanticResume !==
          "authenticated_origin_replay_or_semantic_publication_receipt" ||
        !isHex32(payload.originArchiveDigest) ||
        !isHex32(payload.originDigest) ||
        !isHex32(payload.storeArchiveDigest) ||
        !same(payload.policy, owner.policy) ||
        !Array.isArray(payload.retainedEntries) ||
        payload.retainedEntries.length === 0 ||
        payload.retainedEntries.length >
          Number(owner.policy.maximumActiveHistoryEntries)
      )
        return localRefuse("archived semantic payload framing differs");
      return {
        payload: {
          schemaVersion: payload.schemaVersion,
          originArchiveDigest: payload.originArchiveDigest,
          originDigest: payload.originDigest,
          policy: payload.policy,
          anchor: payload.anchor,
          head: payload.head,
          storeArchiveDigest: payload.storeArchiveDigest,
          snapshot: payload.snapshot,
          retainedEntries: payload.retainedEntries,
          requiredSemanticResume: payload.requiredSemanticResume,
          readmission: payload.readmission,
        },
        hasReadmission,
      };
    };
    const currentObjects = await readClosure(
      previousCheckpoint.requiredArchiveDigests,
    );
    const currentPayloadValue =
      currentObjects.get(previousCheckpoint.payloadDigest)?.value ??
      localRefuse("current payload is absent");
    const { payload } = parsePayload(currentPayloadValue);
    const anchorDescriptor = exactRecord(payload.anchor, [
      "kind",
      "indexDigest",
      "indexSequence",
      "retainedSuffixEntries",
    ]);
    let rootIndex: WatcherUserEventArchiveIndexRead | null = null;
    if (anchorDescriptor !== null) {
      if (
        anchorDescriptor.kind !== "materialized_history" ||
        !isHex32(anchorDescriptor.indexDigest) ||
        anchorDescriptor.retainedSuffixEntries !== "64"
      )
        return localRefuse("archive anchor descriptor differs");
      rootIndex = await readWatcherUserEventArchiveIndex(
        archive,
        anchorDescriptor.indexDigest,
      );
      if (rootIndex.index.indexSequence !== anchorDescriptor.indexSequence)
        return localRefuse("archive root sequence differs");
    }
    let lastProcessedSequence = -1n;
    let lastProcessedEntry: WatcherLocalUserEventEntry | null = null;
    let oldRetainedEntries: readonly WatcherLocalUserEventEntry[] = [];
    let archivedSourceStore = bootstrapStore;
    const writePrivateObjects = async (
      objects: readonly LocalArchiveObject[],
    ) => {
      for (const object of objects) {
        const digest = await archive.put(Buffer.from(object.bytesHex, "hex"));
        if (digest !== object.digest)
          return localRefuse("semantic replay archive write differs");
      }
    };
    const replaySegment = async (
      objects: Awaited<ReturnType<typeof readClosure>>,
      payloadDigest: string,
      sealedIndex: WatcherUserEventArchiveIndexRead | null,
    ) => {
      const archived = (digest: string): unknown =>
        objects.get(digest)?.value ??
        localRefuse("archive dependency is absent");
      const { payload, hasReadmission } = parsePayload(archived(payloadDigest));
      const expectedAnchor =
        replayState.priorIndex === null
          ? {
              kind: "activation_origin",
              parent: owner.origin.parentPoint,
              bootstrapStoreDigest: owner.policy.bootstrapStoreDigest,
            }
          : {
              kind: "materialized_history",
              indexDigest: replayState.priorIndex.digest,
              indexSequence: replayState.priorIndex.index.indexSequence,
              retainedSuffixEntries: "64",
            };
      const oldOrigin = exactRecord(archived(payload.originArchiveDigest), [
        "schemaVersion",
        "numericEncoding",
        "facts",
        "policy",
        "bootstrapStore",
      ]);
      if (
        payload.originDigest !==
          localArchiveField(currentPayloadValue, ["originDigest"]) ||
        oldOrigin === null ||
        oldOrigin.schemaVersion !==
          "midgard-watcher-local-user-event-origin-archive-v1" ||
        oldOrigin.numericEncoding !== "exact-decimal-strings" ||
        !same(oldOrigin.policy, owner.policy) ||
        !same(
          localStableOrigin(oldOrigin.facts),
          localArchiveEvidence(localStableOrigin(owner.origin)),
        ) ||
        localArchiveField(oldOrigin.facts, ["originDigest"]) !==
          payload.originDigest ||
        !same(oldOrigin.bootstrapStore, bootstrapStore) ||
        !same(payload.anchor, expectedAnchor)
      )
        return localRefuse(
          "archived activation origin differs from the fresh authenticated activation",
        );
      if (hasReadmission) {
        const priorReadmission = exactRecord(payload.readmission, [
          "kind",
          "previousCheckpointDigest",
          "previousPayloadDigest",
          "archivedOriginDigest",
          "freshOriginDigest",
          "stableSnapshotDigest",
        ]);
        if (
          priorReadmission === null ||
          priorReadmission.kind !== "fresh_authenticated_origin_replay" ||
          !isHex32(priorReadmission.previousCheckpointDigest) ||
          !isHex32(priorReadmission.previousPayloadDigest) ||
          !isHex32(priorReadmission.archivedOriginDigest) ||
          priorReadmission.freshOriginDigest !== payload.originDigest ||
          priorReadmission.stableSnapshotDigest !==
            sha256Canonical(localStableSnapshot(payload.snapshot)) ||
          localArchiveField(
            await readValue(priorReadmission.previousPayloadDigest),
            ["originDigest"],
          ) !== priorReadmission.archivedOriginDigest
        )
          return localRefuse("archived semantic readmission linkage differs");
      }
      const entries = payload.retainedEntries.map(localArchivedEntry);
      if (
        entries.some(
          (entry, index) =>
            BigInt(entry.sequence) !==
            BigInt(entries[0]!.sequence) + BigInt(index),
        ) ||
        entries[0]!.sequence !== (oldRetainedEntries[0]?.sequence ?? "0")
      )
        return localRefuse(
          "archived retained entries are not the exact consecutive suffix",
        );
      if (
        !same(payload.head, entries.at(-1)) ||
        (replayState.priorIndex === null &&
          payload.storeArchiveDigest !== entries.at(-1)!.nextStoreDigest)
      )
        return localRefuse("archived head differs");
      const entryObservations = new Map<string, WatcherUserEventObservation>();
      for (const { value } of objects.values()) {
        const entryArchive = exactRecord(value, ["entry", "observation"]);
        if (entryArchive === null) continue;
        const entry = localArchivedEntry(entryArchive.entry);
        localStableSnapshot(
          localArchiveField(entryArchive.observation, ["snapshot"]),
        );
        const observed = parseObservationStructural(entryArchive.observation);
        if (
          observed === null ||
          observed.observationDigest !== entry.observationDigest ||
          entryObservations.has(entry.entryDigest)
        )
          return localRefuse("archived entry observation differs");
        entryObservations.set(entry.entryDigest, observed);
      }
      const headStore = parseWatcherDurableStore(
        archived(payload.storeArchiveDigest),
      );
      if (storeDigest(headStore) !== payload.storeArchiveDigest)
        return localRefuse("archived head store digest differs");
      for (const entry of entries) {
        if (BigInt(entry.sequence) <= lastProcessedSequence) {
          if (!oldRetainedEntries.some((retained) => same(retained, entry)))
            return localRefuse(
              "archived retained suffix differs from its materialization",
            );
          continue;
        }
        const previous: WatcherLocalUserEventEntry | null = lastProcessedEntry;

        const oldObservation = entryObservations.get(entry.entryDigest);
        if (
          BigInt(entry.sequence) !== lastProcessedSequence + 1n ||
          entry.originDigest !== payload.originDigest ||
          entry.policyDigest !== owner.policy.policyDigest ||
          entry.predecessorEntryDigest !== (previous?.entryDigest ?? null) ||
          entry.sourceStoreDigest !== storeDigest(archivedSourceStore) ||
          BigInt(entry.nextStoreRevision) !==
            BigInt(entry.sourceStoreRevision) + 1n ||
          entry.sourceStoreRevision !== archivedSourceStore.revision ||
          oldObservation === undefined ||
          oldObservation.transitionKind !== "apply_block" ||
          oldObservation.policyDigest !== owner.policy.policyDigest ||
          oldObservation.network !== owner.policy.network ||
          oldObservation.blueprintHash !== owner.policy.blueprintHash ||
          !same(
            oldObservation.deploymentMarker,
            owner.policy.deploymentMarker,
          ) ||
          oldObservation.snapshot.snapshotDigest !== entry.snapshotDigest ||
          oldObservation.sourceDurableStoreDigest !== entry.sourceStoreDigest ||
          oldObservation.durableStoreDigest !== entry.nextStoreDigest ||
          oldObservation.sourceDurableStoreRevision !==
            entry.sourceStoreRevision ||
          oldObservation.durableStoreRevision !== entry.nextStoreRevision ||
          oldObservation.blockHash !== entry.cursor.blockHash ||
          oldObservation.blockNo !== entry.cursor.blockNo ||
          oldObservation.slot !== entry.cursor.slot ||
          (lastProcessedSequence === -1n
            ? entry.predecessorStateDigest !== null
            : entry.predecessorStateDigest === null)
        )
          return localRefuse("archived semantic entry chain differs");
        if (
          previous !== null &&
          (entry.predecessorStateDigest === null ||
            !same(
              localArchiveField(archived(entry.predecessorStateDigest), [
                "head",
              ]),
              previous,
            ))
        )
          return localRefuse("archived predecessor state differs");
        if (replayState.retainedSource !== null) {
          await replayState.retainedSource.close();
          replayState.retainedSource = null;
        }
        const source =
          lastProcessedSequence === -1n
            ? null
            : await replayBlock(entry.cursor);
        if (source !== null) replayState.retainedSource = source;
        const pair =
          source === null
            ? firstPair
            : Object.freeze({
                finality: source.finality,
                observation: source.observation,
                referenceAuthority: source.referenceAuthority,
              });
        const live = localLivePair(owner, pair);
        const oldEvidence = exactRecord(archived(entry.evidenceDigest), [
          "schemaVersion",
          "numericEncoding",
          "witnesses",
          "referenceEvidence",
        ]);
        if (
          oldEvidence === null ||
          oldEvidence.schemaVersion !==
            "midgard-watcher-local-user-event-block-evidence-v1" ||
          oldEvidence.numericEncoding !== "exact-decimal-strings" ||
          !same(live.witness.current.observation.capture.point, entry.cursor) ||
          !same(
            live.witness.current.observation.capture.predecessorPoint,
            entry.parent,
          )
        )
          return localRefuse(
            "fresh replay does not match the archived whole block",
          );
        for (const step of ["first", "current"] as const) {
          if (
            localArchiveField(oldEvidence.witnesses, [
              step,
              "observation",
              "capture",
              "nativeBlock",
              "rawBlockCbor",
            ]) !==
              live.witness.current.observation.capture.nativeBlock
                .rawBlockCbor ||
            !same(
              localArchiveField(oldEvidence.witnesses, [
                step,
                "observation",
                "capture",
                "point",
              ]),
              entry.cursor,
            ) ||
            !same(
              localArchiveField(oldEvidence.witnesses, [
                step,
                "observation",
                "capture",
                "predecessorPoint",
              ]),
              entry.parent,
            )
          )
            return localRefuse(
              "archived original block bytes differ from fresh canonical replay",
            );
        }
        // The fresh W12 capture just corroborated `entry.parent` as this
        // block's canonical predecessor; the archived quiet stretch between
        // the previous entry and that parent lies on the same linear chain.
        if (previous !== null)
          owner.coverage = Object.freeze({
            point: Object.freeze({ ...entry.parent }),
            headEntryDigest: owner.entries.at(-1)!.entryDigest,
          });
        const transition = prepareLocalUserEventTransition(history, pair);
        const fresh = readWatcherLocalUserEventTransition(transition);
        if (
          !same(
            localStableSnapshot(oldObservation.snapshot),
            localStableSnapshot(fresh.snapshot),
          )
        )
          return localRefuse(
            "fresh semantic replay differs from the archived event fold",
          );
        const oldStore = parseWatcherDurableStore(
          archived(entry.nextStoreDigest),
        );
        if (
          storeDigest(oldStore) !== entry.nextStoreDigest ||
          !topologyMatches(oldStore, oldObservation.snapshot)
        )
          return localRefuse("archived event store topology differs");
        const oldNative = localArchiveField(oldEvidence.witnesses, [
          "current",
          "observation",
          "native",
        ]);
        const oldPoint = oldStore.chainPoints.find(
          (point) => point.chainPointId === oldObservation.chainPointId,
        );
        const oldRow = oldStore.l1Observations.find(
          (row) => row.observationId === oldObservation.sourceObservationDigest,
        );
        if (
          oldPoint === undefined ||
          oldRow === undefined ||
          oldRow.chainPointId !== oldPoint.chainPointId ||
          oldPoint.blockHash !== entry.cursor.blockHash ||
          oldPoint.blockNo !== entry.cursor.blockNo ||
          oldPoint.slot !== entry.cursor.slot ||
          oldPoint.chainPointId !==
            localArchiveField(oldNative, ["chainPoint", "chainPointId"]) ||
          oldPoint.depth !==
            localArchiveField(oldNative, ["chainPoint", "depth"]) ||
          oldPoint.providerId !==
            localArchiveField(oldNative, ["provider", "providerId"]) ||
          oldObservation.pointDigest !==
            localArchiveField(oldNative, ["chainPoint", "pointDigest"]) ||
          oldRow.observationId !==
            localArchiveField(oldNative, ["observationDigest"]) ||
          !same(
            localArchiveEvidence(
              JSON.parse(
                Buffer.from(oldRow.payload.cborHex, "hex").toString("utf8"),
              ),
            ),
            oldNative,
          )
        )
          return localRefuse(
            "archived original observation/store binding differs",
          );
        const oldPoints = [...archivedSourceStore.chainPoints, oldPoint];
        const oldJournal = journalWatcherProtocolUtxoTransition({
          sourceStore: archivedSourceStore,
          nextChainPoints: oldPoints,
          spentAtChainPointId: oldPoint.chainPointId,
          nextProtocolUtxos: oldObservation.snapshot.activeEvents.map(
            (event) => ({
              outRef: event.outRef,
              role: protocolRole(event.kind),
              chainPointId:
                archivedSourceStore.protocolUtxos.find(
                  ({ outRef }) => outRef === event.outRef,
                )?.chainPointId ?? oldPoint.chainPointId,
              output: makeWatcherDurablePayload(event.outputCborHex),
            }),
          ),
        });
        const rebuiltOldStore = makeWatcherDurableStore({
          deploymentMarker: owner.policy.deploymentMarker,
          revision: entry.nextStoreRevision,
          records: {
            ...archivedSourceStore,
            chainPoints: oldPoints,
            ...oldJournal,
            l1Observations: [...archivedSourceStore.l1Observations, oldRow],
          },
        });
        if (
          !same(oldStore, rebuiltOldStore) ||
          !same(
            localStableEventStore(oldStore),
            localStableEventStore(fresh.nextStore),
          )
        )
          return localRefuse(
            "archived event journal differs from fresh semantic replay",
          );
        archivedSourceStore = oldStore;
        lastProcessedSequence = BigInt(entry.sequence);
        lastProcessedEntry = entry;
        oldRetainedEntries = [...oldRetainedEntries, entry];
        commitLocalUserEventTransition(history, transition);
      }
      if (
        !same(
          payload.snapshot,
          entryObservations.get(entries.at(-1)!.entryDigest)!.snapshot,
        ) ||
        !same(
          localStableSnapshot(payload.snapshot),
          localStableSnapshot(owner.snapshot),
        )
      )
        return localRefuse(
          "fresh semantic head differs from archived snapshot",
        );
      if (!same(archivedSourceStore, headStore))
        return localRefuse(
          "archived payload materialization differs from the replayed head",
        );
      if (sealedIndex !== null) {
        if (
          sealedIndex.index.lastEntrySequence !==
            lastProcessedEntry!.sequence ||
          !same(
            sealedIndex.index.retainedEntryDigests,
            oldRetainedEntries.slice(-64).map((entry) => entry.entryDigest),
          )
        )
          return localRefuse(
            "archive segment does not seal the exact retained suffix",
          );
        const requiredPoints = new Set(
          oldRetainedEntries.slice(-64).map((entry) => {
            const observation = entryObservations.get(entry.entryDigest);
            if (observation === undefined || observation.chainPointId === null)
              return localRefuse("materialized suffix observation is absent");
            return observation.chainPointId;
          }),
        );
        const retainPoint = (
          blockHash: string,
          slot: string,
          blockNo: string,
        ) => {
          const points = archivedSourceStore.chainPoints.filter(
            (point) =>
              point.blockHash === blockHash &&
              point.slot === slot &&
              point.blockNo === blockNo,
          );
          if (points.length !== 1)
            return localRefuse(
              "materialized event point is not uniquely retained",
            );
          requiredPoints.add(points[0]!.chainPointId);
        };
        for (const event of owner.snapshot.activeEvents)
          retainPoint(
            event.originBlockHash,
            event.originSlot,
            event.originBlockNo,
          );
        for (const event of owner.snapshot.terminalEvents) {
          retainPoint(
            event.originBlockHash,
            event.originSlot,
            event.originBlockNo,
          );
          retainPoint(
            event.terminalBlockHash,
            event.terminalSlot,
            event.terminalBlockNo,
          );
        }
        const materialized = localMaterializedStoreFromPoints(
          archivedSourceStore,
          requiredPoints,
        );
        const archivedMaterialized = parseWatcherDurableStore(
          await readValue(sealedIndex.index.materializedStoreDigest),
        );
        if (!same(materialized, archivedMaterialized))
          return localRefuse(
            "archived materialization is not the exact dependency-preserving projection",
          );
        const pair =
          replayState.retainedSource === null
            ? firstPair
            : replayState.retainedSource;
        await writePrivateObjects(owner.archiveObjects);
        const anchor = await prepareLocalUserEventAnchor(
          history,
          pair,
          archive,
        );
        const freshMaterialized = localAnchors.get(anchor)!.value.nextStore;
        if (
          !same(
            localStableEventStore(materialized),
            localStableEventStore(freshMaterialized),
          )
        )
          return localRefuse(
            "fresh materialization differs from archived semantics",
          );
        await writePrivateObjects(
          readWatcherLocalUserEventAnchor(anchor).archiveObjects,
        );
        commitLocalUserEventAnchor(anchor);
        archivedSourceStore = materialized;
        oldRetainedEntries = oldRetainedEntries.slice(-64);
        replayState.priorIndex = sealedIndex;
      }
    };
    if (rootIndex !== null) {
      for (
        let sequence = 0n;
        sequence <= BigInt(rootIndex.index.indexSequence);
        sequence += 1n
      ) {
        const segment = await findWatcherUserEventArchiveIndex(
          archive,
          rootIndex,
          sequence.toString(),
        );
        if (
          segment.index.previousIndexDigest !==
            replayState.priorIndex?.digest &&
          !(
            replayState.priorIndex === null &&
            segment.index.previousIndexDigest === null
          )
        )
          return localRefuse("archive segment immediate predecessor differs");
        if (
          BigInt(segment.index.firstEntrySequence) !==
          lastProcessedSequence + 1n
        )
          return localRefuse("archive segment entry boundary differs");
        await replaySegment(
          await readClosure(segment.index.sourceArchiveDigests),
          segment.index.sourcePayloadDigest,
          segment,
        );
      }
      if (replayState.priorIndex?.digest !== rootIndex.digest)
        return localRefuse("archive root is not the replayed segment head");
    }
    await replaySegment(currentObjects, previousCheckpoint.payloadDigest, null);
    const replayHead = owner.entries.at(-1)!;
    const replayPayload = owner.archiveObjects.find(
      (object) => object.digest === owner.checkpoint!.payloadDigest,
    )!;
    const replayPayloadFields = exactRecord(
      JSON.parse(Buffer.from(replayPayload.bytesHex, "hex").toString("utf8")),
      [
        "schemaVersion",
        "originArchiveDigest",
        "originDigest",
        "policy",
        "anchor",
        "head",
        "storeArchiveDigest",
        "snapshot",
        "retainedEntries",
        "requiredSemanticResume",
      ],
    );
    if (replayPayloadFields === null)
      return localRefuse("private replay payload differs");
    const readmissionPayload = localArchiveObject({
      ...replayPayloadFields,
      readmission: {
        kind: "fresh_authenticated_origin_replay",
        previousCheckpointDigest: previousCheckpoint.checkpointDigest,
        previousPayloadDigest: previousCheckpoint.payloadDigest,
        archivedOriginDigest: payload.originDigest,
        freshOriginDigest: owner.originDigest,
        stableSnapshotDigest: sha256Canonical(
          localStableSnapshot(owner.snapshot),
        ),
      },
    });
    const archiveObjects = Object.freeze([
      ...new Map([
        ...[...currentObjects.values()].map(
          ({ object }) => [object.digest, object] as const,
        ),
        ...owner.archiveObjects.map(
          (object) => [object.digest, object] as const,
        ),
        [readmissionPayload.digest, readmissionPayload] as const,
      ]).values(),
    ]);
    if (
      archiveObjects.length >
        WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries ||
      archiveObjects.reduce(
        (total, object) => total + object.bytesHex.length / 2,
        0,
      ) > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes ||
      archiveObjects.reduce(
        (total, object) => total + localArchiveBudgets.get(object)!.nodes,
        0,
      ) > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes
    )
      return localRefuse("semantic readmission archive bound exceeded");
    const nextCheckpoint = makeWatcherUserEventCheckpoint({
      ...previousCheckpoint,
      checkpointSequence: (
        BigInt(previousCheckpoint.checkpointSequence) + 1n
      ).toString(),
      predecessorCheckpointDigest: previousCheckpoint.checkpointDigest,
      payloadDigest: readmissionPayload.digest,
      requiredArchiveDigests: archiveObjects.map(({ digest }) => digest).sort(),
    });
    const refreshed = readWatcherProtectedUserEventCheckpointReceipt(
      await readWatcherProtectedUserEventCheckpoint(runtime),
    );
    if (!same(refreshed.checkpoint, previousCheckpoint))
      return localRefuse("protected head changed during semantic replay");
    const pair =
      replayState.retainedSource === null
        ? firstPair
        : Object.freeze({
            finality: replayState.retainedSource.finality,
            observation: replayState.retainedSource.observation,
            referenceAuthority: replayState.retainedSource.referenceAuthority,
          });
    if (
      !same(
        localLivePair(owner, pair).witness.current.observation.capture.point,
        replayHead.cursor,
      )
    )
      return localRefuse("semantic replay head is no longer live");
    const retained = replayState.retainedSource;
    const readmission = Object.freeze({
      [localReadmissionBrand]: true as const,
    });
    localReadmissions.set(readmission, {
      runtime,
      history,
      pair,
      generation: owner.generation,
      previousCheckpoint,
      nextCheckpoint,
      archiveObjects,
      release: async () => {
        await retained?.close();
      },
      accepted: false,
    });
    replayState.retainedSource = null;
    return readmission;
  } catch (error) {
    closeWatcherLocalUserEventHistory(history);
    await replayState.retainedSource?.close();
    throw error;
  }
};

/** Rebuild a replacement branch from fresh native evidence. Unlike archived
 * readmission, this requires a positive conflicting block at the saved head's
 * height; an unavailable Order, missing block, or transport error proves nothing. */
export const prepareWatcherLocalUserEventCanonicalReplay = async (
  input: Omit<
    Parameters<typeof prepareWatcherLocalUserEventReadmission>[0],
    "replayBlock"
  > &
    Readonly<{
      replayCanonical: (
        savedHead: WatcherUserEventOriginFacts["parentPoint"],
      ) => AsyncIterable<
        Awaited<ReturnType<WatcherLocalUserEventReplaySource>>
      >;
    }>,
): Promise<WatcherLocalUserEventReadmission> => {
  const publication = await readWatcherProtectedUserEventCheckpoint(
    input.runtime,
  );
  const published = readWatcherProtectedUserEventCheckpointReceipt(publication);
  const previous = published.checkpoint;
  if (
    previous === null ||
    published.payload === null ||
    published.validation?.checkpointDigest !== previous.checkpointDigest ||
    published.validation.payloadDigest !== previous.payloadDigest
  )
    return localRefuse(
      "canonical replay requires a validated protected checkpoint",
    );
  const payload = objectForLocalRestart(published.payload);
  const savedHead = localArchivedEntry(payload.head);
  const history = createLocalUserEventHistory({
    ...input,
    publication,
    semanticReplay: true,
  });
  const owner = localOwner(history);
  let retained: Awaited<ReturnType<WatcherLocalUserEventReplaySource>> | null =
    null;
  try {
    if (
      !same(payload.policy, owner.policy) ||
      previous.userEventPolicyDigest !== owner.policy.policyDigest ||
      previous.finalityPolicyDigest !== owner.finalityPolicy.policyDigest ||
      !isHex32(payload.originArchiveDigest)
    )
      return localRefuse("canonical replay deployment or policy differs");
    const oldOriginBytes = await input.archive.read(
      payload.originArchiveDigest,
    );
    if (
      oldOriginBytes === null ||
      sha256Bytes(oldOriginBytes) !== payload.originArchiveDigest
    )
      return localRefuse("canonical replay original provenance is absent");
    const oldOrigin = objectForLocalRestart(oldOriginBytes);
    if (
      !same(
        localStableOrigin(oldOrigin.facts),
        localArchiveEvidence(localStableOrigin(owner.origin)),
      )
    )
      return localRefuse("canonical replay activation differs");
    const firstPair = {
      finality: input.finality,
      observation: input.observation,
      referenceAuthority: input.referenceAuthority,
    };
    commitLocalUserEventTransition(
      history,
      prepareLocalUserEventTransition(history, firstPair),
    );
    let replacement: WatcherUserEventOriginFacts["parentPoint"] | null = null;
    const writeObjects = async () => {
      for (const object of owner.archiveObjects) {
        if (
          (await input.archive.put(Buffer.from(object.bytesHex, "hex"))) !==
          object.digest
        )
          return localRefuse("canonical replay archive write differs");
      }
    };
    for await (const pair of input.replayCanonical(savedHead.cursor)) {
      try {
        const current = localLivePair(owner, pair).witness.current.observation
          .capture.point;
        if (BigInt(current.blockNo) > BigInt(savedHead.cursor.blockNo))
          return localRefuse("canonical replay skipped the saved head height");
        commitLocalUserEventTransition(
          history,
          prepareLocalUserEventTransition(history, pair),
        );
        await retained?.close();
        retained = pair;
        if (localAnchorDue(owner)) {
          await writeObjects();
          const anchor = await prepareLocalUserEventAnchor(
            history,
            pair,
            input.archive,
          );
          commitLocalUserEventAnchor(anchor);
        }
        if (current.blockNo === savedHead.cursor.blockNo) {
          if (current.blockHash === savedHead.cursor.blockHash)
            return localRefuse(
              "canonical replay has no conflicting head block",
            );
          replacement = current;
          break;
        }
      } finally {
        if (retained !== pair) await pair.close();
      }
    }
    if (replacement === null || retained === null)
      return localRefuse(
        "canonical replacement has not reached the saved head height",
      );
    // Historical predecessor evidence remains inspectable, but only this fresh
    // contiguous fold and its live final head authorize the replacement.
    const provenance = localArchiveObject({
      kind: "canonical_branch_replacement",
      previousCheckpoint: previous,
      previousHead: savedHead,
      replacementHead: replacement,
    });
    const archiveObjects = localArchiveClosure([
      ...owner.archiveObjects,
      provenance,
    ]);
    const nextCheckpoint = makeWatcherUserEventCheckpoint({
      ...previous,
      checkpointSequence: (BigInt(previous.checkpointSequence) + 1n).toString(),
      predecessorCheckpointDigest: previous.checkpointDigest,
      rollbackGeneration: (BigInt(previous.rollbackGeneration) + 1n).toString(),
      payloadDigest: owner.checkpoint!.payloadDigest,
      requiredArchiveDigests: archiveObjects.map(({ digest }) => digest).sort(),
    });
    const refreshed = readWatcherProtectedUserEventCheckpointReceipt(
      await readWatcherProtectedUserEventCheckpoint(input.runtime),
    );
    if (!same(refreshed.checkpoint, previous))
      return localRefuse("protected head changed during canonical replay");
    const receipt = Object.freeze({ [localReadmissionBrand]: true as const });
    const headPair = retained;
    localReadmissions.set(receipt, {
      runtime: input.runtime,
      history,
      pair: headPair,
      generation: owner.generation,
      previousCheckpoint: previous,
      nextCheckpoint,
      archiveObjects,
      release: () => headPair.close(),
      accepted: false,
    });
    readWatcherLocalUserEventReadmission(receipt);
    retained = null;
    return receipt;
  } catch (error) {
    closeWatcherLocalUserEventHistory(history);
    throw error;
  } finally {
    await retained?.close();
  }
};

export const readWatcherLocalUserEventReadmission = (
  receipt: WatcherLocalUserEventReadmission,
) => {
  const readmission =
    localReadmissions.get(receipt) ??
    localRefuse("readmission is not privately admitted");
  const owner = localOwner(readmission.history);
  if (
    readmission.accepted ||
    !owner.semanticReplay ||
    owner.generation !== readmission.generation ||
    owner.candidate !== null
  )
    return localRefuse("readmission is no longer pending");
  const live = localLivePair(owner, readmission.pair);
  const finality = readmission.runtime.read().currentFinalityState;
  if (
    finality.phase === "quarantined" ||
    finality.incident !== null ||
    !same(
      live.witness.current.observation.capture.point,
      owner.entries.at(-1)!.cursor,
    ) ||
    live.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      owner.acceptedEvidence.at(-1)!.rawBlockCbor
  )
    return localRefuse("semantic readmission head is no longer current");
  return Object.freeze({
    archiveObjects: readmission.archiveObjects,
    nextCheckpoint: readmission.nextCheckpoint,
    expectedCheckpointDigest: readmission.previousCheckpoint.checkpointDigest,
    expectedCheckpointSequence:
      readmission.previousCheckpoint.checkpointSequence,
  });
};

export const acceptWatcherLocalUserEventReadmission = (
  receipt: WatcherLocalUserEventReadmission,
  publication: WatcherProtectedUserEventCheckpoint,
): WatcherLocalUserEventHistory => {
  const prepared = readWatcherLocalUserEventReadmission(receipt);
  const observed = readWatcherProtectedUserEventCheckpointReceipt(publication);
  if (
    !same(observed.checkpoint, prepared.nextCheckpoint) ||
    observed.payload === null ||
    sha256Bytes(observed.payload) !== prepared.nextCheckpoint.payloadDigest
  )
    return localRefuse("semantic readmission publication differs");
  const readmission = localReadmissions.get(receipt)!;
  const owner = localOwner(readmission.history);
  owner.checkpoint = readmission.nextCheckpoint;
  owner.archiveObjects = readmission.archiveObjects;
  owner.lastAccepted = null;
  owner.semanticReplay = false;
  owner.coverage = localCoverageOnHead(owner.entries.at(-1)!);
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  readmission.accepted = true;
  return readmission.history;
};

export const closeWatcherLocalUserEventReadmission = async (
  receipt: WatcherLocalUserEventReadmission,
): Promise<void> => {
  const readmission =
    localReadmissions.get(receipt) ??
    localRefuse("readmission is not privately admitted");
  if (!readmission.accepted)
    closeWatcherLocalUserEventHistory(readmission.history);
  await readmission.release();
};

const localAnchorBrand = Symbol("watcher-local-user-event-anchor");
export type WatcherLocalUserEventAnchor = Readonly<{
  [localAnchorBrand]: true;
}>;
type LocalAnchorValue = Readonly<{
  sourceStore: WatcherDurableStore;
  nextStore: WatcherDurableStore;
  archiveIndex: WatcherUserEventArchiveIndexRead;
  archiveObjects: readonly LocalArchiveObject[];
  retainedEntries: readonly WatcherLocalUserEventEntry[];
  retainedEvidence: readonly LocalRetainedEvidence[];
  pinnedEvidence: readonly LocalRetainedEvidence[];
  nextCheckpoint: WatcherUserEventCheckpoint;
  expectedCheckpointDigest: string;
  expectedCheckpointSequence: string;
}>;
type LocalAnchorOwner = {
  readonly history: WatcherLocalUserEventHistory;
  readonly pair: LocalPair;
  readonly generation: number;
  readonly value: LocalAnchorValue;
  accepted: boolean;
};
const localAnchors = new WeakMap<
  WatcherLocalUserEventAnchor,
  LocalAnchorOwner
>();

const localMaterializedStore = (
  source: WatcherDurableStore,
  retainedEvidence: readonly LocalRetainedEvidence[],
): WatcherDurableStore => {
  return localMaterializedStoreFromPoints(
    source,
    new Set(retainedEvidence.map(({ chainPointId }) => chainPointId)),
  );
};

const localMaterializedStoreFromPoints = (
  source: WatcherDurableStore,
  requiredPoints: Set<string>,
): WatcherDurableStore => {
  for (const utxo of source.protocolUtxos)
    requiredPoints.add(utxo.chainPointId);
  for (const utxo of source.spentProtocolUtxos) {
    requiredPoints.add(utxo.chainPointId);
    requiredPoints.add(utxo.spentAtChainPointId);
  }
  const chainPoints = source.chainPoints.filter(({ chainPointId }) =>
    requiredPoints.has(chainPointId),
  );
  if (chainPoints.length !== requiredPoints.size)
    return localRefuse("materialization point dependency is absent");
  return immutableWireValue(
    makeWatcherDurableStore({
      deploymentMarker: source.deploymentMarker,
      revision: (BigInt(source.revision) + 1n).toString(),
      records: {
        ...source,
        chainPoints,
        l1Observations: source.l1Observations.filter(({ chainPointId }) =>
          requiredPoints.has(chainPointId),
        ),
      },
    }),
  );
};

const localArchiveClosure = (
  objects: readonly LocalArchiveObject[],
): readonly LocalArchiveObject[] => {
  const unique = Object.freeze([
    ...new Map(objects.map((object) => [object.digest, object])).values(),
  ]);
  if (
    unique.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries ||
    unique.reduce((total, object) => total + object.bytesHex.length / 2, 0) >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes ||
    unique.reduce(
      (total, object) => total + localArchiveBudgets.get(object)!.nodes,
      0,
    ) > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes
  )
    return localRefuse("materialized archive bound exceeded");
  return unique;
};

const prepareLocalUserEventAnchor = async (
  history: WatcherLocalUserEventHistory,
  pair: LocalPair,
  archive: WatcherUserEventArchive,
  publication?: WatcherProtectedUserEventCheckpoint,
): Promise<WatcherLocalUserEventAnchor> => {
  const owner = localOwner(history);
  const generation = owner.generation;
  const sourceStore = owner.store;
  const sourceCheckpoint = owner.checkpoint;
  const sourceObjects = owner.archiveObjects;
  const priorIndex = owner.archiveIndex;
  const snapshot = owner.snapshot;
  if (
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    sourceCheckpoint === null ||
    owner.entries.length <= 64
  )
    return localRefuse(
      "anchor requires a published head with more than 64 retained blocks and no pending operation",
    );
  const current = localLivePair(owner, pair);
  const head = owner.entries.at(-1)!;
  if (
    !same(current.witness.current.observation.capture.point, head.cursor) ||
    current.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      owner.acceptedEvidence.at(-1)!.rawBlockCbor
  )
    return localRefuse("anchor capture differs from the exact semantic head");
  const retainedEntries = Object.freeze(owner.entries.slice(-64));
  const retainedEvidence = Object.freeze(owner.acceptedEvidence.slice(-64));
  const retainedIds = new Set(
    retainedEntries.map(({ entryDigest }) => entryDigest),
  );
  const eventPoints = new Set(
    [...snapshot.activeEvents, ...snapshot.terminalEvents].flatMap((event) =>
      "terminalPointDigest" in event
        ? [event.originPointDigest, event.terminalPointDigest]
        : [event.originPointDigest],
    ),
  );
  const allEvidence = localRetainedEvidence(owner);
  if (
    [...eventPoints].some(
      (point) => !allEvidence.some(({ pointDigest }) => pointDigest === point),
    )
  )
    return localRefuse("anchor event provenance is absent");
  const pinnedEvidence = Object.freeze(
    allEvidence.filter(
      ({ entry, pointDigest }) =>
        !retainedIds.has(entry.entryDigest) && eventPoints.has(pointDigest),
    ),
  );
  const nextStore = localMaterializedStore(sourceStore, [
    ...pinnedEvidence,
    ...retainedEvidence,
  ]);
  if (!topologyMatches(nextStore, snapshot))
    return localRefuse("anchor changes event topology");
  const storeArchive = localArchiveObject(nextStore);
  const index = await makeWatcherUserEventArchiveIndex(archive, {
    previous: priorIndex,
    firstEntrySequence:
      priorIndex === null
        ? "0"
        : (BigInt(priorIndex.index.lastEntrySequence) + 1n).toString(),
    lastEntrySequence: head.sequence,
    sourcePayloadDigest: sourceCheckpoint.payloadDigest,
    sourceArchiveDigests: sourceCheckpoint.requiredArchiveDigests,
    materializedStoreDigest: storeArchive.digest,
    retainedEntryDigests: retainedEntries.map(({ entryDigest }) => entryDigest),
  });
  const indexArchive = localArchiveObject(index);
  const archiveIndex = Object.freeze({ digest: indexArchive.digest, index });
  const payload = localArchiveObject({
    schemaVersion: "midgard-watcher-local-user-event-checkpoint-payload-v1",
    originArchiveDigest: owner.originArchive.digest,
    originDigest: owner.originDigest,
    policy: owner.policy,
    anchor: {
      kind: "materialized_history",
      indexDigest: archiveIndex.digest,
      indexSequence: index.indexSequence,
      retainedSuffixEntries: "64",
    },
    head,
    storeArchiveDigest: storeArchive.digest,
    snapshot,
    retainedEntries,
    requiredSemanticResume:
      "authenticated_origin_replay_or_semantic_publication_receipt",
  });
  const requiredEvidence = new Set(
    [...pinnedEvidence, ...retainedEvidence].flatMap((record) => [
      record.entry.evidenceDigest,
      record.entryArchiveDigest,
    ]),
  );
  const evidenceObjects = sourceObjects.filter(({ digest }) =>
    requiredEvidence.has(digest),
  );
  if (evidenceObjects.length !== requiredEvidence.size)
    return localRefuse("anchor retained archive dependency is absent");
  const archiveObjects = localArchiveClosure([
    owner.originArchive,
    ...evidenceObjects,
    storeArchive,
    indexArchive,
    payload,
  ]);
  const nextCheckpoint = makeWatcherUserEventCheckpoint({
    ...sourceCheckpoint,
    checkpointSequence: (
      BigInt(sourceCheckpoint.checkpointSequence) + 1n
    ).toString(),
    predecessorCheckpointDigest: sourceCheckpoint.checkpointDigest,
    payloadDigest: payload.digest,
    requiredArchiveDigests: archiveObjects.map(({ digest }) => digest).sort(),
  });
  if (
    owner.generation !== generation ||
    owner.store !== sourceStore ||
    owner.checkpoint !== sourceCheckpoint ||
    owner.archiveObjects !== sourceObjects ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null
  )
    return localRefuse("history changed during anchor preparation");
  if (publication !== undefined)
    readWatcherProtectedUserEventCheckpointReceipt(publication);
  localLivePair(owner, pair);
  const receipt = Object.freeze({ [localAnchorBrand]: true as const });
  const value = Object.freeze({
    sourceStore,
    nextStore,
    archiveIndex,
    archiveObjects,
    retainedEntries,
    retainedEvidence,
    pinnedEvidence,
    nextCheckpoint,
    expectedCheckpointDigest: sourceCheckpoint.checkpointDigest,
    expectedCheckpointSequence: sourceCheckpoint.checkpointSequence,
  });
  localAnchors.set(receipt, {
    history,
    pair,
    generation,
    value,
    accepted: false,
  });
  owner.anchorCandidate = receipt;
  return receipt;
};

export const prepareWatcherLocalUserEventAnchor = async (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      archive: WatcherUserEventArchive;
      publication: WatcherProtectedUserEventCheckpoint;
    }>,
): Promise<WatcherLocalUserEventAnchor> => {
  const {
    history,
    archive,
    publication,
    finality,
    observation,
    referenceAuthority,
  } = input;
  const owner = localOwner(history);
  if (
    owner.semanticReplay ||
    !same(
      readWatcherProtectedUserEventCheckpointReceipt(publication).checkpoint,
      owner.checkpoint,
    )
  )
    return localRefuse("anchor protected predecessor differs");
  return await prepareLocalUserEventAnchor(
    history,
    Object.freeze({ finality, observation, referenceAuthority }),
    archive,
    publication,
  );
};

export const readWatcherLocalUserEventAnchor = (
  receipt: WatcherLocalUserEventAnchor,
) => {
  const anchor =
    localAnchors.get(receipt) ??
    localRefuse("anchor is not privately admitted");
  const owner = localOwner(anchor.history);
  if (
    anchor.accepted ||
    owner.anchorCandidate !== receipt ||
    owner.generation !== anchor.generation ||
    owner.store !== anchor.value.sourceStore ||
    owner.candidate !== null
  )
    return localRefuse("anchor is no longer pending");
  const current = localLivePair(owner, anchor.pair);
  if (
    !same(
      current.witness.current.observation.capture.point,
      owner.entries.at(-1)!.cursor,
    ) ||
    current.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      owner.acceptedEvidence.at(-1)!.rawBlockCbor
  )
    return localRefuse("anchor head is no longer current");
  return Object.freeze({
    archiveObjects: anchor.value.archiveObjects,
    nextCheckpoint: anchor.value.nextCheckpoint,
    expectedCheckpointDigest: anchor.value.expectedCheckpointDigest,
    expectedCheckpointSequence: anchor.value.expectedCheckpointSequence,
  });
};

const commitLocalUserEventAnchor = (
  receipt: WatcherLocalUserEventAnchor,
): WatcherLocalUserEventHistory => {
  readWatcherLocalUserEventAnchor(receipt);
  const anchor = localAnchors.get(receipt)!;
  const owner = localOwner(anchor.history);
  owner.store = anchor.value.nextStore;
  owner.entries = anchor.value.retainedEntries;
  owner.acceptedEvidence = anchor.value.retainedEvidence;
  owner.pinnedEvidence = anchor.value.pinnedEvidence;
  owner.archiveIndex = anchor.value.archiveIndex;
  owner.archiveObjects = anchor.value.archiveObjects;
  owner.checkpoint = anchor.value.nextCheckpoint;
  owner.anchorCandidate = null;
  owner.lastAccepted = null;
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  anchor.accepted = true;
  return anchor.history;
};

export const acceptWatcherLocalUserEventAnchor = (
  receipt: WatcherLocalUserEventAnchor,
  publication: WatcherProtectedUserEventCheckpoint,
): WatcherLocalUserEventHistory => {
  const prepared = readWatcherLocalUserEventAnchor(receipt);
  const anchor = localAnchors.get(receipt)!;
  if (localOwner(anchor.history).semanticReplay)
    return localRefuse("provisional anchor requires semantic readmission");
  const observed = readWatcherProtectedUserEventCheckpointReceipt(publication);
  if (
    !same(observed.checkpoint, prepared.nextCheckpoint) ||
    observed.payload === null ||
    sha256Bytes(observed.payload) !== prepared.nextCheckpoint.payloadDigest
  )
    return localRefuse(
      "anchor publication differs from the exact materialization",
    );
  return commitLocalUserEventAnchor(receipt);
};

/** The durable owner records semantic completion only for a live candidate
 * admitted by this module. Descriptive JSON and copied handles cannot mint it. */
export const readWatcherLocalUserEventValidation = (
  candidate: unknown,
  checkpoint: WatcherUserEventCheckpoint,
): WatcherUserEventValidation => {
  if (typeof candidate !== "object" || candidate === null)
    return localRefuse("semantic validation candidate is absent");
  const next = localTransitions.has(
    candidate as WatcherLocalUserEventTransition,
  )
    ? readWatcherLocalUserEventTransition(
        candidate as WatcherLocalUserEventTransition,
      ).nextCheckpoint
    : localAnchors.has(candidate as WatcherLocalUserEventAnchor)
      ? readWatcherLocalUserEventAnchor(
          candidate as WatcherLocalUserEventAnchor,
        ).nextCheckpoint
      : localReadmissions.has(candidate as WatcherLocalUserEventReadmission)
        ? readWatcherLocalUserEventReadmission(
            candidate as WatcherLocalUserEventReadmission,
          ).nextCheckpoint
        : localRefuse("semantic validation candidate was not admitted");
  if (!same(next, checkpoint))
    return localRefuse("semantic validation candidate checkpoint differs");
  return Object.freeze({
    schemaVersion: WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION,
    checkpointDigest: next.checkpointDigest,
    payloadDigest: next.payloadDigest,
    policyDigest: next.userEventPolicyDigest,
  });
};

/** Restore a completed fold. Original archive facts remain historical data;
 * only the newly acquired head pair supplies current source/finality authority. */
export const restoreWatcherLocalUserEventHistory = async (
  input: Omit<
    Parameters<typeof createWatcherLocalUserEventHistory>[0],
    "publication"
  > &
    Readonly<{
      runtime: WatcherDurableRuntime;
      archive: WatcherUserEventArchive;
      readHead: WatcherLocalUserEventReplaySource;
      referenceAuthority: WatcherUserEventReferenceAuthority;
    }>,
): Promise<WatcherLocalUserEventHistory> => {
  const publication = await readWatcherProtectedUserEventCheckpoint(
    input.runtime,
  );
  const published = readWatcherProtectedUserEventCheckpointReceipt(publication);
  const checkpoint = published.checkpoint;
  if (
    checkpoint === null ||
    published.payload === null ||
    published.validation?.schemaVersion !==
      WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION ||
    published.validation.checkpointDigest !== checkpoint.checkpointDigest ||
    published.validation.payloadDigest !== checkpoint.payloadDigest ||
    published.validation.policyDigest !== checkpoint.userEventPolicyDigest
  )
    return localRefuse(
      "restart requires durable semantic validation; explicit recovery is required",
    );
  const runtimeFinality = input.runtime.readFinality();
  if (
    runtimeFinality.phase === "quarantined" ||
    runtimeFinality.incident !== null
  )
    return localRefuse("restart runtime is quarantined");
  const history = createLocalUserEventHistory({
    ...input,
    publication,
    semanticReplay: true,
  });
  const provisional = localOwner(history);
  let headPair:
    | Awaited<ReturnType<WatcherLocalUserEventReplaySource>>
    | undefined;
  try {
    const payload = objectForLocalRestart(published.payload);
    if (
      payload.schemaVersion !==
        "midgard-watcher-local-user-event-checkpoint-payload-v1" ||
      !same(payload.policy, provisional.policy) ||
      checkpoint.userEventPolicyDigest !== provisional.policy.policyDigest ||
      !isHex32(payload.originArchiveDigest) ||
      !isHex32(payload.originDigest) ||
      !isHex32(payload.storeArchiveDigest) ||
      !Array.isArray(payload.retainedEntries) ||
      payload.retainedEntries.length === 0 ||
      payload.retainedEntries.length >
        Number(provisional.policy.maximumActiveHistoryEntries)
    )
      return localRefuse("saved semantic state dependencies differ");
    const objects = new Map<
      string,
      Readonly<{ object: LocalArchiveObject; value: unknown }>
    >();
    let retainedBytes = 0;
    let retainedNodes = 0;
    // Only the bounded current closure is loaded. Sealed historical segments
    // are left in the archive; no block is replayed or semantically revalidated.
    for (const key of checkpoint.requiredArchiveDigests) {
      const bytes = await input.archive.read(key);
      if (bytes === null || sha256Bytes(bytes) !== key)
        return localRefuse("saved semantic state archive is absent or corrupt");
      retainedBytes += bytes.length;
      if (
        retainedBytes >
        WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes
      )
        return localRefuse("saved semantic state archive exceeds its bound");
      const value: unknown = JSON.parse(
        new TextDecoder("utf-8", { fatal: true }).decode(bytes),
      );
      const archived = localArchiveObject(value);
      retainedNodes += localArchiveBudgets.get(archived)!.nodes;
      if (
        archived.digest !== key ||
        retainedNodes >
          WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes
      )
        return localRefuse("saved semantic state archive framing differs");
      objects.set(key, { object: archived, value });
    }
    const originArchive = objects.get(payload.originArchiveDigest);
    const savedOrigin = originArchive?.value;
    if (
      originArchive === undefined ||
      localArchiveField(savedOrigin, ["schemaVersion"]) !==
        "midgard-watcher-local-user-event-origin-archive-v1" ||
      !same(
        localStableOrigin(localArchiveField(savedOrigin, ["facts"])),
        localArchiveEvidence(localStableOrigin(provisional.origin)),
      ) ||
      localArchiveField(savedOrigin, ["facts", "originDigest"]) !==
        payload.originDigest
    )
      return localRefuse("saved origin differs from the admitted deployment");
    const stored = objects.get(payload.storeArchiveDigest)?.value;
    if (stored === undefined)
      return localRefuse("saved materialized event state is absent");
    const entries = Object.freeze(
      payload.retainedEntries.map(localArchivedEntry),
    );
    const head = entries.at(-1)!;
    if (
      !same(payload.head, head) ||
      entries.some(
        (entry, index) =>
          entry.originDigest !== payload.originDigest ||
          entry.policyDigest !== provisional.policy.policyDigest ||
          (index > 0 &&
            entry.predecessorEntryDigest !== entries[index - 1]!.entryDigest),
      )
    )
      return localRefuse("saved semantic progress marker differs");
    const snapshot = immutableWireValue(
      payload.snapshot,
    ) as WatcherUserEventSnapshot;
    const retainedIds = new Set(entries.map((entry) => entry.entryDigest));
    const pinnedPoints = new Set(
      [...snapshot.activeEvents, ...snapshot.terminalEvents].flatMap((event) =>
        "terminalPointDigest" in event
          ? [event.originPointDigest, event.terminalPointDigest]
          : [event.originPointDigest],
      ),
    );
    const evidence: LocalRetainedEvidence[] = [];
    for (const [entryArchiveDigest, archived] of objects) {
      const candidate = exactRecord(archived.value, ["entry", "observation"]);
      if (candidate === null) continue;
      const entry = localArchivedEntry(candidate.entry);
      if (
        entry.originDigest !== payload.originDigest ||
        entry.policyDigest !== provisional.policy.policyDigest
      )
        continue;
      const original = objects.get(entry.evidenceDigest)?.value;
      if (original === undefined) continue;
      const rawBlockCbor = localArchiveField(original, [
        "witnesses",
        "current",
        "observation",
        "capture",
        "nativeBlock",
        "rawBlockCbor",
      ]);
      const pointDigest = localArchiveField(original, [
        "witnesses",
        "current",
        "observation",
        "native",
        "chainPoint",
        "pointDigest",
      ]);
      const chainPointId = localArchiveField(original, [
        "witnesses",
        "current",
        "observation",
        "native",
        "chainPoint",
        "chainPointId",
      ]);
      if (
        !isHex32(pointDigest) ||
        !isHex32(chainPointId) ||
        !isHexBytes(rawBlockCbor)
      )
        return localRefuse("saved historical block facts are malformed");
      if (retainedIds.has(entry.entryDigest) || pinnedPoints.has(pointDigest))
        evidence.push(
          Object.freeze({
            entry,
            entryArchiveDigest,
            rawBlockCbor,
            pointDigest,
            chainPointId,
          }),
        );
    }
    const acceptedEvidence = Object.freeze(
      entries.map((entry) => {
        const matches = evidence.filter(
          (record) => record.entry.entryDigest === entry.entryDigest,
        );
        if (matches.length !== 1)
          return localRefuse("saved progress evidence is absent or ambiguous");
        return matches[0]!;
      }),
    );
    const pinnedEvidence = Object.freeze(
      evidence.filter(({ entry }) => !retainedIds.has(entry.entryDigest)),
    );
    if (
      [...pinnedPoints].some(
        (point) => !evidence.some((record) => record.pointDigest === point),
      )
    )
      return localRefuse("saved event provenance is absent");
    const anchor = objectForLocalRestart(
      Buffer.from(watcherCanonicalJson(payload.anchor), "utf8"),
    );
    const archiveIndex =
      anchor.kind === "activation_origin"
        ? null
        : anchor.kind === "materialized_history" && isHex32(anchor.indexDigest)
          ? await readWatcherUserEventArchiveIndex(
              input.archive,
              anchor.indexDigest,
            )
          : localRefuse("saved history anchor is invalid");
    if (
      archiveIndex !== null &&
      archiveIndex.index.indexSequence !== anchor.indexSequence
    )
      return localRefuse("saved history anchor sequence differs");
    headPair = await input.readHead(head.cursor);
    const live = localLivePair(provisional, headPair);
    if (
      !same(live.witness.current.observation.capture.point, head.cursor) ||
      live.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
        acceptedEvidence.at(-1)!.rawBlockCbor
    )
      return localRefuse(
        "saved head is no longer canonical; explicit recovery is required",
      );
    const fresh = readWatcherProtectedUserEventCheckpointReceipt(
      await readWatcherProtectedUserEventCheckpoint(input.runtime),
    );
    localLivePair(provisional, headPair);
    if (
      !same(fresh.checkpoint, checkpoint) ||
      !same(fresh.validation, published.validation)
    )
      return localRefuse("saved semantic progress changed during restart");
    localHistories.set(history, {
      ...provisional,
      originDigest: payload.originDigest,
      originArchive: originArchive.object,
      store: immutableWireValue(stored) as WatcherDurableStore,
      snapshot,
      entries,
      acceptedEvidence,
      pinnedEvidence,
      archiveIndex,
      archiveObjects: Object.freeze(
        [...objects.values()].map(({ object }) => object),
      ),
      checkpoint,
      coverage: localCoverageOnHead(head),
      semanticReplay: false,
      acceptedAtMonotonicMs: performance.now(),
    });
    return history;
  } catch (cause) {
    closeWatcherLocalUserEventHistory(history);
    throw cause;
  } finally {
    await headPair?.close();
  }
};

const objectForLocalRestart = (bytes: Uint8Array): PlainRecord => {
  const value: unknown = JSON.parse(
    new TextDecoder("utf-8", { fatal: true }).decode(bytes),
  );
  if (typeof value !== "object" || value === null || Array.isArray(value))
    return localRefuse("saved semantic state is not an object");
  return value as PlainRecord;
};
