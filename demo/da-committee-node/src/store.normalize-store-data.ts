import {
  parsePromiseCapacityEvidence,
  promiseCapacityEvidenceKey,
} from "./availability/promise-capacity-evidence.js";
import {
  parseDaSignatureRecord,
  parseDaStoredConflictEvidenceRecord,
  parseDaStoredPayloadRecord,
} from "./domain.js";
import {
  conflictEvidenceKey,
  signatureKey,
  type StoreData,
} from "./store.committee-store.js";
import {
  parseCommitteeDeploymentRecord,
  parseDecisionOutboxRecord,
} from "./store.parse-decision-outbox-record.js";
import { parseL1SourceState } from "./store.parse-l1-source-state.js";
import { parseStoredRecordMap } from "./store.parse-stored-record-map.js";
import { parseRetirementFloor } from "./store/retirement-model.js";

export const normalizeStoreData = (value: unknown): StoreData => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("committee node store data must be an object");
  }
  const record = value as Partial<StoreData>;
  return {
    ...(record.retirementFloor === undefined
      ? {}
      : { retirementFloor: parseRetirementFloor(record.retirementFloor) }),
    promiseCapacityEvidence: parseStoredRecordMap(
      record.promiseCapacityEvidence,
      parsePromiseCapacityEvidence,
      promiseCapacityEvidenceKey,
      "promise capacity evidence",
    ),
    ...(record.deployment === undefined
      ? {}
      : { deployment: parseCommitteeDeploymentRecord(record.deployment) }),
    chainCursor:
      record.chainCursor === undefined
        ? undefined
        : parseL1SourceState(record.chainCursor),
    stateQueueHeaders: record.stateQueueHeaders ?? {},
    daPayloads: parseStoredRecordMap(
      record.daPayloads,
      parseDaStoredPayloadRecord,
      (entry) => entry.headerHash,
      "DA stored payload records V1",
    ),
    daSignatures: parseStoredRecordMap(
      record.daSignatures,
      parseDaSignatureRecord,
      (entry) =>
        signatureKey(
          entry.headerHash,
          entry.availabilityCommitmentDigest,
          entry.signerIndex,
        ),
      "DA signature records V1",
    ),
    daConflictEvidence: parseStoredRecordMap(
      record.daConflictEvidence,
      parseDaStoredConflictEvidenceRecord,
      conflictEvidenceKey,
      "DA conflict evidence records V1",
    ),
    daAttestationCandidates: record.daAttestationCandidates ?? {},
    l1Submissions: record.l1Submissions ?? {},
    peerBroadcasts: record.peerBroadcasts ?? {},
    peerHealth: record.peerHealth ?? {},
    peerNonces: record.peerNonces ?? {},
    decisionOutbox: parseStoredRecordMap(
      record.decisionOutbox,
      parseDecisionOutboxRecord,
      (entry) => entry.effectId,
      "decision outbox records V1",
    ),
  };
};
