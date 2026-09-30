import {
  readWatcherLocalBackfillFinalityOriginalWitness,
  type WatcherFinalityPolicy,
  type WatcherLocalBackfillFinalityReceipt,
} from "../../l1/finality-engine.js";
import { type WatcherLocalBackfillObservationReceipt } from "../../l1/l1-adapter.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  type WatcherUserEventScriptBinding,
} from "../../runtime/deployment-identity.js";
import {
  type WatcherDurableRuntime,
  type WatcherProtectedUserEventCheckpoint,
} from "../../storage/durable-runtime.js";
import {
  watcherCanonicalJson,
  type WatcherDurableStore,
} from "../../storage/durable-store.js";
import { type WatcherUserEventCheckpoint } from "../../storage/user-event-checkpoint.js";
import { type WatcherStateQueueHeaderObservation } from ".././authenticated-state-queue-observation.js";
import { type WatcherUserEventArchiveIndexRead } from ".././user-event-history-archive.js";
import { type WatcherUserEventOriginFacts } from ".././user-event-origin.js";
import {
  type WatcherUserEventReferenceAuthority,
  type WatcherUserEventReferenceEvidence,
} from ".././user-event-reference-authority.js";
import { evidenceWithinBounds, sha256Bytes } from "./policy.js";
import {
  type EvidenceGraphBudget,
  WATCHER_USER_EVENT_INDEXER_BOUNDS,
  type WatcherIndexedUserEvent,
  type WatcherTerminalUserEvent,
  type WatcherUserEventIndexerPolicy,
  type WatcherUserEventObservation,
  type WatcherUserEventSnapshot,
} from "./types.js";

export const localHistoryBrand = Symbol("watcher-local-user-event-history");

export const localTransitionBrand = Symbol(
  "watcher-local-user-event-transition",
);

export type WatcherLocalUserEventHistory = Readonly<{
  [localHistoryBrand]: true;
}>;

export type WatcherLocalUserEventTransition = Readonly<{
  [localTransitionBrand]: true;
}>;

export type LocalPair = Readonly<{
  finality: WatcherLocalBackfillFinalityReceipt;
  observation: WatcherLocalBackfillObservationReceipt;
  referenceAuthority: WatcherUserEventReferenceAuthority;
}>;

type LocalWitness = ReturnType<
  typeof readWatcherLocalBackfillFinalityOriginalWitness
>;

export type LocalArchiveObject = Readonly<{ digest: string; bytesHex: string }>;

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

export type LocalPreparedRead = Readonly<{
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

export type LocalRetainedEvidence = Readonly<{
  entry: WatcherLocalUserEventEntry;
  entryArchiveDigest: string;
  rawBlockCbor: string;
  pointDigest: string;
  chainPointId: string;
}>;

export type LocalHistoryOwner = {
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

export const localHistories = new WeakMap<
  WatcherLocalUserEventHistory,
  LocalHistoryOwner
>();

export const localTransitions = new WeakMap<
  WatcherLocalUserEventTransition,
  LocalTransitionOwner
>();

export const localRefuse = (reason: string): never => {
  throw new Error(`Local user-event history refused: ${reason}`);
};

export const localOwner = (
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
export const localArchiveEvidence = (value: unknown): unknown => {
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

export const localArchiveBudgets = new WeakMap<
  LocalArchiveObject,
  EvidenceGraphBudget
>();

export const localArchiveObject = (value: unknown): LocalArchiveObject => {
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

export const localHistoryAnchorDescriptor = (owner: LocalHistoryOwner) =>
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

export const localRetainedEvidence = (
  owner: LocalHistoryOwner,
): readonly LocalRetainedEvidence[] =>
  [...owner.pinnedEvidence, ...owner.acceptedEvidence].sort((left, right) =>
    BigInt(left.entry.sequence) < BigInt(right.entry.sequence)
      ? -1
      : BigInt(left.entry.sequence) > BigInt(right.entry.sequence)
        ? 1
        : 0,
  );

export const localCoverageOnHead = (
  head: WatcherLocalUserEventEntry,
): WatcherLocalUserEventCoverage =>
  Object.freeze({
    point: Object.freeze({ ...head.cursor }),
    headEntryDigest: head.entryDigest,
  });

/** The covered head: the coverage point when it references the head entry,
 * otherwise the head entry's own cursor. The activation block has no entry
 * yet, so before it the covered head is the origin's parent. */
export const localCoverageHead = (
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

export const localUnavailableErrors = new WeakSet<Error>();

export const localEventAuthorityBrand = Symbol(
  "watcher-local-user-event-authority",
);

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

export type LocalEventAuthorityOwner = Readonly<{
  history: WatcherLocalUserEventHistory;
  header: WatcherStateQueueHeaderObservation | null;
  runtime: WatcherDurableRuntime;
  pair: LocalPair;
  generation: number;
  value: WatcherLocalUserEventAuthorityRead;
  protectedRead: { receipt: WatcherProtectedUserEventCheckpoint | null };
}>;

export const localEventAuthorities = new WeakMap<
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

export const localReadmissionBrand = Symbol(
  "watcher-local-user-event-readmission",
);

export type WatcherLocalUserEventReadmission = Readonly<{
  [localReadmissionBrand]: true;
}>;

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

export const localReadmissions = new WeakMap<
  WatcherLocalUserEventReadmission,
  LocalReadmissionOwner
>();

export const localAnchorBrand = Symbol("watcher-local-user-event-anchor");

export type WatcherLocalUserEventAnchor = Readonly<{
  [localAnchorBrand]: true;
}>;
