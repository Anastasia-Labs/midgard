import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { watcherDeploymentProtocolScriptAuthority } from "../runtime/deployment-identity.js";
import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import { advanceCurrentLock } from "./authenticated-state-queue-observation.advance-current-lock.js";
import {
  authenticatePersistedBootstrapTopology,
  rawRedeemers,
} from "./authenticated-state-queue-observation.authenticate-persisted-bootstrap-topology.js";
import {
  correctionLockWitness,
  sameQueue,
} from "./authenticated-state-queue-observation.correction-lock-witness.js";
import {
  HEX_32,
  NATURAL,
  RELEASE_FINALITY_DEPTH,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherCorrectionLockObservation,
  type WatcherStateQueueHeaderObservation,
  type WatcherStateQueueRecovery,
} from "./authenticated-state-queue-observation.parse-persisted-header.js";
import {
  mintPolicyIds,
  outputReferences,
  parsePersistedObservation,
} from "./authenticated-state-queue-observation.parse-persisted-observation.js";
import { decodedQueueOutputs } from "./authenticated-state-queue-observation.queue-output.js";
import {
  anchoredHeaderObservation,
  reconstructQueue,
} from "./authenticated-state-queue-observation.reconstruct-queue.js";
import { type PersistedRestoreReaders } from "./authenticated-state-queue-observation.snapshot-observation-at-boundary.js";

export const restorePersistedObservationChain = async ({
  persistedObservations,
  intersection,
  ogmiosTipBlockNo,
  authority,
  sourceId,
  maximumObservations,
  readers,
}: {
  persistedObservations: readonly unknown[];
  intersection: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
  }>;
  ogmiosTipBlockNo: string;
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthority>;
  sourceId: string;
  maximumObservations: number;
  readers: PersistedRestoreReaders;
}): Promise<WatcherStateQueueRecovery> => {
  if (
    !HEX_32.test(intersection.blockHash) ||
    !NATURAL.test(intersection.blockNo) ||
    !NATURAL.test(intersection.slot) ||
    !NATURAL.test(ogmiosTipBlockNo) ||
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
  const atOrBefore = chain.filter(
    ({ nativePoint }) =>
      BigInt(nativePoint.blockNo) < BigInt(intersection.blockNo) ||
      (nativePoint.blockNo === intersection.blockNo &&
        BigInt(nativePoint.slot) <= BigInt(intersection.slot)),
  );
  if (atOrBefore.length === 0 || atOrBefore.length !== chain.length) {
    throw new Error("state-queue restore observations cross the intersection");
  }
  const latestCached = chain[chain.length - 1]!;
  // A deep catch-up is slow, never unsafe: every persisted record is
  // re-authenticated against raw L1 below and every later block is replayed
  // live. Only a cursor ahead of the tip is refused.
  if (BigInt(ogmiosTipBlockNo) < BigInt(latestCached.nativePoint.blockNo)) {
    throw new Error(
      "state-queue restore cursor is ahead of the native chain tip",
    );
  }
  const intersectionPoint = Object.freeze({
    ...intersection,
    pointId: computeFraudProofRawL1PointId(intersection),
  });
  const stateQueuePolicyId = authority.protocolScriptHashes.stateQueueMint;
  const hubOraclePolicyId = authority.protocolScriptHashes.hubOracleMint;
  const stateQueueAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
  );
  const correctionLockAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.correctionLockSpend),
  );
  const fraudProofAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.fraudProofSpend),
  );
  let prior: WatcherAuthenticatedStateQueueObservation | null = null;
  let replayedHeaders = Object.freeze(
    [] as WatcherStateQueueHeaderObservation[],
  );
  let replayedLock: WatcherCorrectionLockObservation | null = null;
  let previousCheckpoint: SDK.StateQueueAuthenticatedReplayCheckpoint | null =
    null;
  for (const persisted of chain) {
    const isAuthenticatedBase = prior === null;
    if (
      persisted.deploymentIdentityDigest !== authority.deploymentFingerprint ||
      persisted.protocolScriptAuthorityDigest !== authority.authorityDigest ||
      persisted.stateQueuePolicyId !== stateQueuePolicyId ||
      persisted.hubOraclePolicyId !== hubOraclePolicyId ||
      persisted.sourceId !== sourceId ||
      // A checkpoint-less record is either the window base (the original
      // bootstrap, or a compacted window that now starts at a progress
      // record; both are authenticated as topology below) or a progress
      // record that carries its predecessor's finalized state unchanged.
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
    const point = Object.freeze({
      blockHash: persisted.nativePoint.blockHash,
      blockNo: persisted.nativePoint.blockNo,
      slot: persisted.nativePoint.slot,
      pointId: computeFraudProofRawL1PointId({
        blockHash: persisted.nativePoint.blockHash,
        blockNo: persisted.nativePoint.blockNo,
        slot: persisted.nativePoint.slot,
      }),
    });
    const rawBlock = await readers.readBlock(point);
    if (
      rawBlock.sourceId !== sourceId ||
      rawBlock.point.pointId !== point.pointId ||
      persisted.nativePoint.chainPointId !== point.pointId ||
      persisted.nativePoint.parentBlockHash !== rawBlock.parentBlockHash ||
      persisted.nativePoint.finalityDepth !== RELEASE_FINALITY_DEPTH.toString()
    ) {
      throw new Error("state-queue restore block metadata was substituted");
    }
    if (isAuthenticatedBase) {
      await authenticatePersistedBootstrapTopology({
        persisted,
        throughPoint: point,
        historyPoint: intersectionPoint,
        authority,
        readers,
      });
      replayedHeaders = persisted.finalizedHeaders;
      replayedLock = persisted.finalizedCorrectionLock;
    }
    for (const checkpoint of persisted.checkpoints) {
      if (
        checkpoint.blockHash !== point.blockHash ||
        checkpoint.blockNo !== point.blockNo ||
        checkpoint.slot !== point.slot ||
        checkpoint.chainPointId !== persisted.nativePoint.chainPointId ||
        checkpoint.finalityDepth !== RELEASE_FINALITY_DEPTH.toString() ||
        (prior !== null &&
          checkpoint === persisted.checkpoints[0] &&
          !sameQueue(prior.finalizedQueue, checkpoint.previousQueue)) ||
        (previousCheckpoint !== null &&
          !sameQueue(previousCheckpoint.nextQueue, checkpoint.previousQueue))
      ) {
        throw new Error(
          "state-queue restore checkpoint chain is discontinuous",
        );
      }
      const blockTransaction =
        rawBlock.transactions[Number(checkpoint.transactionIndex)];
      if (blockTransaction?.txHash !== checkpoint.transactionHash) {
        throw new Error(
          "state-queue restore transaction index was substituted",
        );
      }
      const raw = await readers.readTransaction(
        checkpoint.transactionHash,
        point,
      );
      if (
        raw.inclusionPoint.pointId !== point.pointId ||
        raw.confirmationDepth < Number(checkpoint.finalityDepth)
      ) {
        throw new Error(
          "state-queue restore transaction/finality was substituted",
        );
      }
      const body = CML.TransactionBody.from_cbor_hex(raw.bodyCbor);
      const queueOutputs = decodedQueueOutputs({
        body,
        transactionHash: raw.txHash,
        stateQueueAddress,
        stateQueuePolicyId,
      });
      const nextQueue =
        checkpoint.previousQueue.length === 0 && queueOutputs.length === 1
          ? Object.freeze([queueOutputs[0]!.node])
          : reconstructQueue({
              previousQueue: checkpoint.previousQueue,
              transactionHash: raw.txHash,
              spentInputOutRefs: outputReferences(body.inputs()),
              resolvedInputs: raw.resolvedInputs,
              outputs: queueOutputs,
              stateQueueAddress,
              stateQueuePolicyId,
            });
      const redeemers = rawRedeemers(raw.witnessSetCbor);
      const policies = mintPolicyIds(body);
      const lockWitness = correctionLockWitness({
        raw,
        body,
        mintPolicies: policies,
        redeemers,
        stateQueuePolicyId,
        correctionLockAddress,
        hubOraclePolicyId,
        fraudProofPolicyId: authority.protocolScriptHashes.fraudProofMint,
        fraudProofAddress,
        availabilityChallengePolicyId:
          authority.protocolScriptHashes.availabilityChallengeMint,
      });
      const rederived = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
        deploymentIdentityDigest: authority.deploymentFingerprint,
        stateQueuePolicyId,
        transactionHash: raw.txHash,
        blockHash: point.blockHash,
        slot: point.slot,
        blockNo: point.blockNo,
        transactionIndex: checkpoint.transactionIndex,
        chainPointId: persisted.nativePoint.chainPointId,
        finalityDepth: checkpoint.finalityDepth,
        mintPolicyIds: policies,
        redeemers,
        spentInputOutRefs: outputReferences(body.inputs()),
        referenceInputOutRefs: outputReferences(body.reference_inputs()),
        correctionLockWitness: lockWitness,
        previousQueue: checkpoint.previousQueue,
        nextQueue,
      });
      if (
        rederived === null ||
        !watcherSameCanonicalJson(rederived, checkpoint)
      ) {
        throw new Error(
          "state-queue persisted checkpoint differs from authenticated L1 replay",
        );
      }
      if (!isAuthenticatedBase) {
        replayedLock = advanceCurrentLock({
          current: replayedLock,
          witness: lockWitness,
          transactionHash: raw.txHash,
          point,
          chainPointId: point.pointId,
          finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
        });
        const headersByHash = new Map(
          replayedHeaders.map((header) => [header.headerHash, header]),
        );
        for (const output of queueOutputs) {
          if (output.header === null) continue;
          headersByHash.set(
            output.header.headerHash,
            anchoredHeaderObservation({
              prior: headersByHash.get(output.header.headerHash),
              header: output.header,
              queueOutRef: output.node.outRef,
              nextHeaderHash: output.nextHeaderHash,
              point: {
                transactionHash: raw.txHash,
                blockHash: point.blockHash,
                slot: point.slot,
                blockNo: point.blockNo,
                chainPointId: point.pointId,
              },
            }),
          );
        }
        replayedHeaders = Object.freeze(
          nextQueue.flatMap(({ headerHash }) => {
            if (headerHash === null) return [];
            const header = headersByHash.get(headerHash);
            if (header === undefined) {
              throw new Error(
                "state-queue restore omitted authenticated HeaderV1 bytes",
              );
            }
            return [header];
          }),
        );
      }
      previousCheckpoint = rederived;
    }
    if (
      (isAuthenticatedBase &&
        persisted.checkpoints.length === 0 &&
        (persisted.finalizedQueue.length === 0 ||
          persisted.finalizedCorrectionLock === null)) ||
      (persisted.checkpoints.length > 0 &&
        (previousCheckpoint === null ||
          !sameQueue(
            previousCheckpoint.nextQueue,
            persisted.finalizedQueue,
          ))) ||
      !watcherSameCanonicalJson(replayedHeaders, persisted.finalizedHeaders) ||
      !watcherSameCanonicalJson(replayedLock, persisted.finalizedCorrectionLock)
    ) {
      throw new Error(
        "state-queue persisted cursor differs from checkpoint replay",
      );
    }
    prior = persisted;
  }
  const latest = chain[chain.length - 1]!;
  // The base topology proves its outrefs were unspent at the base point only.
  // A later cursor (a progress record, or the last transition) re-proves the
  // same claim at its own point, so a store that dropped an intervening
  // transition cannot pass off a stale queue as current: every state-queue
  // transition spends at least one node of the queue it replaces.
  if (chain.length > 1) {
    await authenticatePersistedBootstrapTopology({
      persisted: latest,
      throughPoint: Object.freeze({
        blockHash: latest.nativePoint.blockHash,
        blockNo: latest.nativePoint.blockNo,
        slot: latest.nativePoint.slot,
        pointId: computeFraudProofRawL1PointId({
          blockHash: latest.nativePoint.blockHash,
          blockNo: latest.nativePoint.blockNo,
          slot: latest.nativePoint.slot,
        }),
      }),
      historyPoint: intersectionPoint,
      authority,
      readers,
    });
  }
  const intersectionBlock = await readers.readBlock(intersectionPoint);
  if (
    intersectionBlock.sourceId !== sourceId ||
    intersectionBlock.point.pointId !== intersectionPoint.pointId
  ) {
    throw new Error("state-queue restore intersection is not canonical");
  }
  const replayIntersection = Object.freeze({
    blockHash: latest.nativePoint.blockHash,
    blockNo: latest.nativePoint.blockNo,
    slot: latest.nativePoint.slot,
    chainPointId: latest.nativePoint.chainPointId,
  });
  const catchupBoundary = Object.freeze({
    blockHash: intersection.blockHash,
    blockNo: intersection.blockNo,
    slot: intersection.slot,
    chainPointId: intersectionPoint.pointId,
    finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
    ogmiosTipBlockNo,
  });
  return Object.freeze({
    previous: latest,
    discardedObservationCount: 0,
    replayIntersection,
    catchupBoundary,
  });
};
