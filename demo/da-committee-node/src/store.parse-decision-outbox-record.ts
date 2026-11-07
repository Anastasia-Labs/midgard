import { parseDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import type { DaStoredPayloadRecord } from "./domain.js";
import {
  type CommitteeDeploymentRecord,
  type DecisionOutboxRecord,
  hasPayloadBytes,
  type StoreData,
} from "./store.committee-store.js";

export const withDerivedPayloadFetchStatus = (
  record: DaStoredPayloadRecord,
): DaStoredPayloadRecord => {
  if (record.payloadFetchStatus !== undefined) {
    return record;
  }
  if (hasPayloadBytes(record)) {
    return { ...record, payloadFetchStatus: "available" };
  }
  if (record.validationStatus === "missing_da") {
    return { ...record, payloadFetchStatus: "missing_da" };
  }
  return record;
};

export const emptyStoreData = (): StoreData => ({
  promiseCapacityEvidence: {},
  stateQueueHeaders: {},
  daPayloads: {},
  daSignatures: {},
  daConflictEvidence: {},
  daAttestationCandidates: {},
  l1Submissions: {},
  peerBroadcasts: {},
  peerHealth: {},
  peerNonces: {},
  decisionOutbox: {},
});

export const parseCommitteeDeploymentRecord = (
  value: unknown,
): CommitteeDeploymentRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(
      "committee node deployment marker record must be an object",
    );
  }
  const record = value as Record<string, unknown>;
  const expected = [
    "marker",
    "manifestSha256",
    "contractDeploymentInfoSha256",
    "manifestRaw",
  ] as const;
  if (
    Object.keys(record).length !== expected.length ||
    expected.some((key) => !Object.prototype.hasOwnProperty.call(record, key))
  ) {
    throw new Error(
      "committee node deployment marker record must contain exactly marker, manifestSha256, contractDeploymentInfoSha256, and manifestRaw",
    );
  }
  const digest = (field: "manifestSha256" | "contractDeploymentInfoSha256") => {
    const entry = record[field];
    if (typeof entry !== "string" || !/^[0-9a-f]{64}$/u.test(entry)) {
      throw new Error(
        `committee node deployment marker record ${field} must be lowercase SHA-256 hex`,
      );
    }
    return entry;
  };
  if (typeof record.manifestRaw !== "string") {
    throw new Error(
      "committee node deployment marker record manifestRaw must be a string",
    );
  }
  return {
    marker: parseDeploymentMarker(record.marker),
    manifestSha256: digest("manifestSha256"),
    contractDeploymentInfoSha256: digest("contractDeploymentInfoSha256"),
    manifestRaw: record.manifestRaw,
  };
};

export const decisionEffectId = (args: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly stateQueueOutRef: string;
  readonly effectKind: DecisionOutboxRecord["effectKind"];
  readonly signerIndex?: number;
}): string =>
  [
    args.deploymentFingerprint,
    args.headerHash,
    args.stateQueueOutRef,
    args.effectKind,
    args.signerIndex === undefined ? "-" : args.signerIndex.toString(),
  ].join(":");

export const parseDecisionOutboxRecord = (
  value: unknown,
): DecisionOutboxRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("decision outbox record must be an object");
  }
  const record = value as Partial<DecisionOutboxRecord>;
  const allowedKeys = new Set([
    "schemaVersion",
    "effectId",
    "deploymentFingerprint",
    "sourceMode",
    "network",
    "effectKind",
    "headerHash",
    "stateQueueOutRef",
    "signerIndex",
    "slot",
    "blockHash",
    "finalized",
    "status",
    "attemptCount",
    "createdAt",
    "updatedAt",
    "lastError",
  ]);
  const signerMatchesKind =
    record.effectKind === "signature_publish"
      ? Number.isInteger(record.signerIndex) &&
        record.signerIndex! >= 0 &&
        record.signerIndex! <= 255
      : record.signerIndex === undefined;
  if (
    Object.keys(record).some((key) => !allowedKeys.has(key)) ||
    record.schemaVersion !== 1 ||
    typeof record.effectId !== "string" ||
    typeof record.deploymentFingerprint !== "string" ||
    record.deploymentFingerprint.length === 0 ||
    (record.sourceMode !== "local_node" &&
      !(
        record.sourceMode === "external_providers" &&
        record.status !== "pending"
      )) ||
    typeof record.network !== "string" ||
    record.network.length === 0 ||
    (record.effectKind !== "signature_publish" &&
      record.effectKind !== "l1_reconcile") ||
    typeof record.headerHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(record.headerHash) ||
    typeof record.stateQueueOutRef !== "string" ||
    !/^[0-9a-f]{64}#[0-9]+$/u.test(record.stateQueueOutRef) ||
    !signerMatchesKind ||
    (record.slot !== undefined &&
      (!Number.isSafeInteger(record.slot) || record.slot < 0)) ||
    (record.blockHash !== undefined &&
      (typeof record.blockHash !== "string" ||
        !/^[0-9a-f]{64}$/u.test(record.blockHash))) ||
    record.finalized !== true ||
    (record.status !== "pending" &&
      record.status !== "published" &&
      record.status !== "failed" &&
      record.status !== "reconciled") ||
    !Number.isSafeInteger(record.attemptCount) ||
    record.attemptCount! < 1 ||
    typeof record.createdAt !== "string" ||
    !isCanonicalIsoTimestamp(record.createdAt) ||
    typeof record.updatedAt !== "string" ||
    !isCanonicalIsoTimestamp(record.updatedAt) ||
    (record.lastError !== undefined &&
      (typeof record.lastError !== "string" || record.lastError.length === 0))
  ) {
    throw new Error("decision outbox record is malformed");
  }
  if (
    record.slot === undefined ||
    record.blockHash === undefined ||
    (record.status === "failed" && record.lastError === undefined) ||
    (record.status !== "failed" && record.lastError !== undefined)
  ) {
    throw new Error(
      "decision outbox record has inconsistent finality or terminal status",
    );
  }
  const canonical: DecisionOutboxRecord = {
    schemaVersion: 1,
    effectId: record.effectId,
    deploymentFingerprint: record.deploymentFingerprint,
    sourceMode: record.sourceMode,
    network: record.network,
    effectKind: record.effectKind,
    headerHash: record.headerHash,
    stateQueueOutRef: record.stateQueueOutRef,
    ...(record.signerIndex === undefined
      ? {}
      : { signerIndex: record.signerIndex }),
    ...(record.slot === undefined ? {} : { slot: record.slot }),
    ...(record.blockHash === undefined ? {} : { blockHash: record.blockHash }),
    finalized: true,
    status: record.status,
    attemptCount: record.attemptCount!,
    createdAt: record.createdAt,
    updatedAt: record.updatedAt,
    ...(record.lastError === undefined ? {} : { lastError: record.lastError }),
  };
  if (
    canonical.effectId !==
    decisionEffectId({
      deploymentFingerprint: canonical.deploymentFingerprint,
      headerHash: canonical.headerHash,
      stateQueueOutRef: canonical.stateQueueOutRef,
      effectKind: canonical.effectKind,
      ...(canonical.signerIndex === undefined
        ? {}
        : { signerIndex: canonical.signerIndex }),
    })
  ) {
    throw new Error("decision outbox effectId does not match record identity");
  }
  return canonical;
};

export const isCanonicalIsoTimestamp = (value: string): boolean => {
  const time = Date.parse(value);
  return Number.isFinite(time) && new Date(time).toISOString() === value;
};
