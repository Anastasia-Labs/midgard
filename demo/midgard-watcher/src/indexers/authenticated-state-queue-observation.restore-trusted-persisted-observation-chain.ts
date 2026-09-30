import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Transaction,
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosRawTransaction,
  readAdmittedLocalKupmiosUnitHistoryAtPoint,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { watcherDeploymentProtocolScriptAuthority } from "../runtime/deployment-identity.js";
import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import { sameQueue } from "./authenticated-state-queue-observation.correction-lock-witness.js";
import {
  HEX_28,
  RELEASE_FINALITY_DEPTH,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
  type WatcherStateQueueRecovery,
} from "./authenticated-state-queue-observation.parse-persisted-header.js";
import {
  outputHasUnit,
  parsePersistedObservation,
} from "./authenticated-state-queue-observation.parse-persisted-observation.js";
import { queueOutput } from "./authenticated-state-queue-observation.queue-output.js";
import { WatcherRetainedHeaderAttestationPendingError } from "./authenticated-state-queue-observation.snapshot-observation-at-boundary.js";

export const resolveRetainedHeaderAtBoundary = async ({
  headerHash,
  authority,
  readers,
}: {
  headerHash: string;
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthority>;
  readers: Readonly<{
    readBoundary(): ReturnType<typeof readAdmittedLocalKupmiosBoundary>;
    readHistory(
      unit: string,
      point: Parameters<
        typeof readAdmittedLocalKupmiosUnitHistoryAtPoint
      >[0]["point"],
    ): ReturnType<typeof readAdmittedLocalKupmiosUnitHistoryAtPoint>;
    readTransaction(
      txHash: string,
      point: Parameters<
        typeof readAdmittedLocalKupmiosRawTransaction
      >[0]["expectedInclusionPoint"],
    ): Promise<FraudProofRawL1Transaction>;
  }>;
}): Promise<WatcherStateQueueHeaderObservation> => {
  if (!HEX_28.test(headerHash)) {
    throw new Error("retained HeaderV1 lookup requires a 28-byte header hash");
  }
  const boundary = await readers.readBoundary();
  const stateQueuePolicyId = authority.protocolScriptHashes.stateQueueMint;
  const stateQueueAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
  );
  const unit = `${stateQueuePolicyId}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`;
  const history = await readers.readHistory(unit, boundary.kupoCheckpoint);
  const candidates: WatcherStateQueueHeaderObservation[] = [];
  for (const entry of history.transactions) {
    const raw = await readers.readTransaction(
      entry.txHash,
      entry.inclusionPoint,
    );
    const outputs = CML.TransactionBody.from_cbor_hex(raw.bodyCbor).outputs();
    for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
      const output = outputs.get(outputIndex);
      if (!outputHasUnit(output, unit)) continue;
      const decoded = queueOutput({
        output,
        outRef: `${raw.txHash}#${outputIndex.toString()}`,
        stateQueueAddress,
        stateQueuePolicyId,
      });
      if (
        decoded?.header === null ||
        decoded?.node.headerHash !== headerHash ||
        decoded.header.headerHash !== headerHash
      ) {
        throw new Error(
          "retained HeaderV1 unit history contains a substituted queue output",
        );
      }
      candidates.push(
        Object.freeze({
          ...decoded.header,
          queueOutRef: decoded.node.outRef,
          nextHeaderHash: decoded.nextHeaderHash,
          observedTransactionHash: raw.txHash,
          observedBlockHash: raw.inclusionPoint.blockHash,
          observedSlot: raw.inclusionPoint.slot,
          observedBlockNo: raw.inclusionPoint.blockNo,
          observedChainPointId: raw.inclusionPoint.pointId,
          finalityDepth: raw.confirmationDepth.toString(),
        }),
      );
    }
  }
  if (candidates.length === 0) {
    throw new Error(
      "retained HeaderV1 has no authenticated state-queue output",
    );
  }
  const headerBytes = new Set(
    candidates.map(({ headerCborHex }) => headerCborHex),
  );
  if (headerBytes.size !== 1) {
    throw new Error(
      "retained HeaderV1 unit history changes immutable header bytes",
    );
  }
  const retained = candidates
    .filter(({ daAvailability }) => daAvailability !== "Unattested")
    .sort((left, right) => {
      const blockOrder =
        BigInt(left.observedBlockNo) - BigInt(right.observedBlockNo);
      if (blockOrder !== 0n) return blockOrder < 0n ? -1 : 1;
      const slotOrder = BigInt(left.observedSlot) - BigInt(right.observedSlot);
      if (slotOrder !== 0n) return slotOrder < 0n ? -1 : 1;
      return left.queueOutRef.localeCompare(right.queueOutRef);
    })
    .at(-1);
  if (retained === undefined) {
    throw new WatcherRetainedHeaderAttestationPendingError(headerHash);
  }
  return retained;
};

/**
 * Structural restore of the persisted cursor chain: authority binding, row
 * linkage and checkpoint continuity, with no L1 reads. The last row is both
 * the replay intersection and the catch-up boundary.
 */
export const restoreTrustedPersistedObservationChain = ({
  persistedObservations,
  authority,
  sourceId,
  maximumObservations,
}: {
  persistedObservations: readonly unknown[];
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthority>;
  sourceId: string;
  maximumObservations: number;
}): WatcherStateQueueRecovery => {
  if (
    !Number.isSafeInteger(maximumObservations) ||
    maximumObservations <= 0 ||
    persistedObservations.length === 0 ||
    persistedObservations.length > maximumObservations
  ) {
    throw new Error("state-queue restore input exceeds its release bound");
  }
  const parsed = persistedObservations.map(parsePersistedObservation);
  if (parsed.some((observation) => observation === null)) {
    throw new Error("state-queue restore contains a non-canonical observation");
  }
  const chain = parsed as readonly WatcherAuthenticatedStateQueueObservation[];
  const stateQueuePolicyId = authority.protocolScriptHashes.stateQueueMint;
  const hubOraclePolicyId = authority.protocolScriptHashes.hubOracleMint;
  let prior: WatcherAuthenticatedStateQueueObservation | null = null;
  for (const persisted of chain) {
    if (
      persisted.deploymentIdentityDigest !== authority.deploymentFingerprint ||
      persisted.protocolScriptAuthorityDigest !== authority.authorityDigest ||
      persisted.stateQueuePolicyId !== stateQueuePolicyId ||
      persisted.hubOraclePolicyId !== hubOraclePolicyId ||
      persisted.sourceId !== sourceId ||
      persisted.nativePoint.finalityDepth !==
        RELEASE_FINALITY_DEPTH.toString() ||
      persisted.nativePoint.chainPointId !==
        computeFraudProofRawL1PointId({
          blockHash: persisted.nativePoint.blockHash,
          blockNo: persisted.nativePoint.blockNo,
          slot: persisted.nativePoint.slot,
        }) ||
      (persisted.checkpoints.length === 0 &&
        prior !== null &&
        (persisted.correctionLockWitnesses.length !== 0 ||
          !sameQueue(prior.finalizedQueue, persisted.finalizedQueue) ||
          !watcherSameCanonicalJson(
            prior.finalizedHeaders,
            persisted.finalizedHeaders,
          ) ||
          !watcherSameCanonicalJson(
            prior.finalizedCorrectionLock,
            persisted.finalizedCorrectionLock,
          ))) ||
      (prior !== null &&
        (persisted.previousObservationDigest !== prior.observationDigest ||
          BigInt(persisted.nativePoint.blockNo) <=
            BigInt(prior.nativePoint.blockNo) ||
          BigInt(persisted.nativePoint.slot) <= BigInt(prior.nativePoint.slot)))
    ) {
      throw new Error(
        "state-queue restore chain differs from deployment/source authority",
      );
    }
    let previousCheckpoint: SDK.StateQueueAuthenticatedReplayCheckpoint | null =
      null;
    for (const checkpoint of persisted.checkpoints) {
      if (
        checkpoint.blockHash !== persisted.nativePoint.blockHash ||
        checkpoint.blockNo !== persisted.nativePoint.blockNo ||
        checkpoint.slot !== persisted.nativePoint.slot ||
        checkpoint.chainPointId !== persisted.nativePoint.chainPointId ||
        checkpoint.finalityDepth !== RELEASE_FINALITY_DEPTH.toString() ||
        (prior !== null &&
          previousCheckpoint === null &&
          !sameQueue(prior.finalizedQueue, checkpoint.previousQueue)) ||
        (previousCheckpoint !== null &&
          !sameQueue(previousCheckpoint.nextQueue, checkpoint.previousQueue))
      ) {
        throw new Error(
          "state-queue restore checkpoint chain is discontinuous",
        );
      }
      previousCheckpoint = checkpoint;
    }
    if (
      previousCheckpoint !== null &&
      !sameQueue(previousCheckpoint.nextQueue, persisted.finalizedQueue)
    ) {
      throw new Error(
        "state-queue persisted cursor differs from checkpoint replay",
      );
    }
    prior = persisted;
  }
  const latest = chain[chain.length - 1]!;
  const replayIntersection = Object.freeze({
    blockHash: latest.nativePoint.blockHash,
    blockNo: latest.nativePoint.blockNo,
    slot: latest.nativePoint.slot,
    chainPointId: latest.nativePoint.chainPointId,
  });
  return Object.freeze({
    previous: latest,
    discardedObservationCount: 0,
    replayIntersection,
    catchupBoundary: Object.freeze({
      ...replayIntersection,
      finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
      ogmiosTipBlockNo: latest.nativePoint.blockNo,
    }),
  });
};
