import * as SDK from "@al-ft/midgard-sdk";

import type { CommitteeL1ClientConfig } from "../config.js";
import { signatureKey } from "../store.committee-store.js";
import type { CommitteeStore } from "../store.js";
import { decisionEffectId, jsonReplacer } from "../store.js";
import type { CommitteePromiseAdmissionCandidate } from "./promise-admission.js";

/** Upper bound for successful fresh signature/outbox serialization and its
 * source-observation rewrite. Retry/error growth requires its own adopted cap. */
export const promiseSignatureResourceReserve = async (args: {
  candidate: CommitteePromiseAdmissionCandidate;
  config: CommitteeL1ClientConfig;
  store: CommitteeStore;
}): Promise<Readonly<{ storeRecords: number; storeEncodedBytes: number }>> => {
  const { candidate, config, store } = args;
  const verified = candidate.verifiedPayload;
  if (
    verified === undefined ||
    verified.validation.headerHash !== candidate.record.headerHash ||
    config.signerIndex === undefined
  )
    throw new Error(
      "Prospective signature verification metadata is unavailable",
    );
  const source = await store.getL1SourceState();
  const observation = source?.observations.find(
    (item) => item.headerHash === candidate.record.headerHash,
  );
  if (
    source?.status !== "healthy" ||
    observation?.stateQueueOutRef !== candidate.record.stateQueueOutRef
  )
    throw new Error("Prospective source observation is unavailable");
  // ISO extended years are longer than the current-year production timestamps.
  const timestamp = "+999999-12-31T23:59:59.999Z";
  const effectId = decisionEffectId({
    deploymentFingerprint: config.deploymentFingerprint,
    headerHash: candidate.record.headerHash,
    stateQueueOutRef: candidate.record.stateQueueOutRef,
    effectKind: "signature_publish",
    signerIndex: config.signerIndex,
  });
  const effect = {
    schemaVersion: 1,
    effectId,
    deploymentFingerprint: config.deploymentFingerprint,
    sourceMode: source.sourceMode,
    network: config.network,
    effectKind: "signature_publish",
    headerHash: candidate.record.headerHash,
    stateQueueOutRef: candidate.record.stateQueueOutRef,
    signerIndex: config.signerIndex,
    slot: candidate.record.observedChainPoint.slot,
    blockHash: candidate.record.observedChainPoint.blockHash,
    finalized: true,
    status: "pending",
    attemptCount: 1,
    createdAt: timestamp,
    updatedAt: timestamp,
  };
  const signature = {
    deploymentFingerprint: config.deploymentFingerprint,
    headerHash: candidate.record.headerHash,
    signerIndex: config.signerIndex,
    signatureWitness: "00".repeat(65),
    availabilityCommitmentCbor: SDK.encodeDaAvailabilityCommitment(
      candidate.commitment,
    ),
    availabilityCommitmentDigest: candidate.commitmentDigest,
    payloadHash: verified.payloadHash,
    committeeSignersHash: config.daParams.committeeSignersHash,
    signedAt: timestamp,
    broadcastStatus: "post_failed",
    source: "local",
    verifiedAt: timestamp,
    l1ChainPoint: candidate.record.observedChainPoint,
    validation: verified.validation,
  };
  const key = signatureKey(
    candidate.record.headerHash,
    candidate.commitmentDigest,
    config.signerIndex,
  );
  // Pretty JSON and nested container keys exceed either backend's successful
  // row-text delta; reserve a complete source rewrite as additional headroom.
  const projected = {
    daSignatures: { [key]: signature },
    decisionOutbox: { [effectId]: effect },
    l1SourceState: {
      controlledSource: {
        ...source,
        observedAt: timestamp,
        observations: [
          ...source.observations.filter(
            (item) => item.headerHash !== candidate.record.headerHash,
          ),
          { ...observation, hasPersistedDecision: true },
        ],
      },
    },
  };
  const storeEncodedBytes = Buffer.byteLength(
    JSON.stringify(projected, jsonReplacer, 2),
  );
  if (!Number.isSafeInteger(storeEncodedBytes))
    throw new Error("Prospective serialization is out of range");
  return { storeRecords: 2, storeEncodedBytes };
};
