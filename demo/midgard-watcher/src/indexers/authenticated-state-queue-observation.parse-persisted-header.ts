import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { watcherSameCanonicalJson } from "../storage/durable-store.js";

export const WATCHER_AUTHENTICATED_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION =
  "midgard-watcher-production-state-queue-observation-v1" as const;

/**
 * The compiled deployment profile's release depth. Signed deployment
 * verification admits only manifests carrying this depth, and each source is
 * checked against it at construction.
 */
export const RELEASE_FINALITY_DEPTH =
  DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export type QueueNode = SDK.StateQueueTransitionNode;

export type DecodedQueueHeader = Readonly<{
  headerHash: string;
  headerCborHex: string;
  stateQueueNodeCborHex: string;
  linkedListDatumCborHex: string;
  daAvailability: SDK.DaAvailabilityStateQueueStatus;
}>;

export type QueueOutput = Readonly<{
  node: QueueNode;
  nextHeaderHash: string | null;
  header: DecodedQueueHeader | null;
}>;

export type LockOutput = Readonly<{
  outRef: string;
  datum: SDK.CorrectionLockDatum;
}>;

export type WatcherAuthenticatedStateQueueObservation = Readonly<{
  schemaVersion: typeof WATCHER_AUTHENTICATED_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION;
  deploymentIdentityDigest: string;
  protocolScriptAuthorityDigest: string;
  stateQueuePolicyId: string;
  hubOraclePolicyId: string;
  nativePoint: Readonly<{
    blockHash: string;
    parentBlockHash: string | null;
    slot: string;
    blockNo: string;
    chainPointId: string;
    finalityDepth: string;
  }>;
  sourceId: string;
  previousObservationDigest: string | null;
  checkpoints: readonly SDK.StateQueueAuthenticatedReplayCheckpoint[];
  finalizedQueue: readonly QueueNode[];
  finalizedHeaders: readonly WatcherStateQueueHeaderObservation[];
  finalizedCorrectionLock: WatcherCorrectionLockObservation | null;
  correctionLockWitnesses: readonly SDK.StateQueueCorrectionLockWitness[];
  observationDigest: string;
}>;

export type WatcherCorrectionLockObservation = Readonly<{
  outRef: string;
  datum: SDK.CorrectionLockDatum;
  observedTransactionHash: string;
  observedBlockHash: string;
  observedSlot: string;
  observedBlockNo: string;
  observedChainPointId: string;
  finalityDepth: string;
}>;

export type WatcherStateQueueHeaderObservation = Readonly<{
  headerHash: string;
  headerCborHex: string;
  stateQueueNodeCborHex: string;
  linkedListDatumCborHex: string;
  daAvailability: SDK.DaAvailabilityStateQueueStatus;
  queueOutRef: string;
  nextHeaderHash: string | null;
  observedTransactionHash: string;
  observedBlockHash: string;
  observedSlot: string;
  observedBlockNo: string;
  observedChainPointId: string;
  finalityDepth: string;
}>;

/** L1 evidence that a queued header was merged into confirmed state. */
export type WatcherMergedHeaderProof = Readonly<{
  headerHash: string;
  mergeTransactionHash: string;
  mergeBlockHash: string;
  mergeSlot: string;
  mergeBlockNo: string;
  confirmationDepth: string;
}>;

/** The state-queue mint redeemers whose burn removes one queued header. */
export const WATCHER_STATE_QUEUE_REMOVAL_KINDS = Object.freeze([
  "RemoveFraudulentBlockHeader",
  "RemoveUnattestedBlockAfterTimeout",
  "RemoveUnavailableBlockAfterTimeout",
] as const);

export type WatcherStateQueueRemovalKind =
  (typeof WATCHER_STATE_QUEUE_REMOVAL_KINDS)[number];

/** L1 evidence that a queued header was removed from the queue. */
export type WatcherRemovedHeaderProof = Readonly<{
  headerHash: string;
  removalTransactionHash: string;
  removalKind: WatcherStateQueueRemovalKind;
  removalBlockHash: string;
  removalSlot: string;
  removalBlockNo: string;
  confirmationDepth: string;
}>;

/** A queued header no longer in the L1 queue at release finality. */
export type WatcherReleasedHeaderProof =
  | WatcherMergedHeaderProof
  | WatcherRemovedHeaderProof;

export const admittedObservations = new WeakSet<object>();

export const admittedHeaders = new WeakSet<object>();

export const assertWatcherStateQueueObservation = (
  observation: WatcherAuthenticatedStateQueueObservation,
): void => {
  if (!admittedObservations.has(observation)) {
    throw new Error(
      "state-queue observation was not admitted by the production source",
    );
  }
};

export const assertWatcherStateQueueHeaderObservation = (
  header: WatcherStateQueueHeaderObservation,
): void => {
  if (!admittedHeaders.has(header)) {
    throw new Error(
      "state-queue HeaderV1 observation was not admitted by the production source",
    );
  }
};

export const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Record<string, unknown> | null => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    return null;
  }
  const actual = Reflect.ownKeys(value);
  const expected = new Set(keys);
  return actual.length === keys.length &&
    actual.every((key) => typeof key === "string" && expected.has(key))
    ? (value as Record<string, unknown>)
    : null;
};

export const parseQueueNodes = (
  value: unknown,
): readonly QueueNode[] | null => {
  if (!Array.isArray(value)) return null;
  const nodes: QueueNode[] = [];
  for (const [index, candidate] of value.entries()) {
    const node = exactRecord(candidate, ["headerHash", "outRef"]);
    if (
      node === null ||
      typeof node.outRef !== "string" ||
      !OUT_REF.test(node.outRef) ||
      (index === 0
        ? node.headerHash !== null
        : typeof node.headerHash !== "string" || !HEX_28.test(node.headerHash))
    ) {
      return null;
    }
    nodes.push(
      Object.freeze({
        headerHash: node.headerHash as string | null,
        outRef: node.outRef,
      }),
    );
  }
  return new Set(nodes.map(({ headerHash }) => headerHash)).size ===
    nodes.length &&
    new Set(nodes.map(({ outRef }) => outRef)).size === nodes.length
    ? Object.freeze(nodes)
    : null;
};

export const parsePersistedHeader = (
  value: unknown,
): WatcherStateQueueHeaderObservation | null => {
  const record = exactRecord(value, [
    "headerHash",
    "headerCborHex",
    "stateQueueNodeCborHex",
    "linkedListDatumCborHex",
    "daAvailability",
    "queueOutRef",
    "nextHeaderHash",
    "observedTransactionHash",
    "observedBlockHash",
    "observedSlot",
    "observedBlockNo",
    "observedChainPointId",
    "finalityDepth",
  ]);
  if (
    record === null ||
    typeof record.headerHash !== "string" ||
    !HEX_28.test(record.headerHash) ||
    typeof record.headerCborHex !== "string" ||
    !EVEN_HEX.test(record.headerCborHex) ||
    typeof record.stateQueueNodeCborHex !== "string" ||
    !EVEN_HEX.test(record.stateQueueNodeCborHex) ||
    typeof record.linkedListDatumCborHex !== "string" ||
    !EVEN_HEX.test(record.linkedListDatumCborHex) ||
    typeof record.queueOutRef !== "string" ||
    !OUT_REF.test(record.queueOutRef) ||
    (record.nextHeaderHash !== null &&
      (typeof record.nextHeaderHash !== "string" ||
        !HEX_28.test(record.nextHeaderHash))) ||
    typeof record.observedTransactionHash !== "string" ||
    !HEX_32.test(record.observedTransactionHash) ||
    typeof record.observedBlockHash !== "string" ||
    !HEX_32.test(record.observedBlockHash) ||
    typeof record.observedSlot !== "string" ||
    !NATURAL.test(record.observedSlot) ||
    typeof record.observedBlockNo !== "string" ||
    !NATURAL.test(record.observedBlockNo) ||
    typeof record.observedChainPointId !== "string" ||
    !HEX_32.test(record.observedChainPointId) ||
    typeof record.finalityDepth !== "string" ||
    !NATURAL.test(record.finalityDepth) ||
    BigInt(record.finalityDepth) < BigInt(RELEASE_FINALITY_DEPTH)
  ) {
    return null;
  }
  try {
    const encoded = Data.to(
      record.daAvailability as SDK.DaAvailabilityStateQueueStatus,
      SDK.DaAvailabilityStateQueueStatus,
    );
    const daAvailability = Data.from(
      encoded,
      SDK.DaAvailabilityStateQueueStatus,
    );
    if (!watcherSameCanonicalJson(record.daAvailability, daAvailability)) {
      return null;
    }
    return Object.freeze({
      ...(record as Omit<WatcherStateQueueHeaderObservation, "daAvailability">),
      daAvailability,
    });
  } catch {
    return null;
  }
};

/**
 * The retained HeaderV1 exists on L1 but none of its authenticated queue
 * outputs carries a public DA attachment yet. The committee attests a block
 * after the operator commits it, so a successor can reach classification
 * before its predecessor's attestation is included. This is a wait
 * condition for the classifier, not a divergence, and it clears on its own
 * once the attestation transaction is included.
 */
export class WatcherRetainedHeaderAttestationPendingError extends Error {
  constructor(readonly headerHash: string) {
    super(
      "retained HeaderV1 lookup requires an authenticated public DA attachment",
    );
    this.name = "WatcherRetainedHeaderAttestationPendingError";
  }
}
