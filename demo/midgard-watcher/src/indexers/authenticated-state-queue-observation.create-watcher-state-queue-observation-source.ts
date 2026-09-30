import {
  LocalKupmiosCheckpointChangedError,
  type LocalKupmiosFraudProofRawSource,
  localKupmiosHttpOgmiosRawSourceDetails,
  readAdmittedLocalKupmiosAddressUtxosAtPoint,
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosRawTransaction,
  readAdmittedLocalKupmiosUnitHistoryAtPoint,
  withLocalKupmiosSourceCapture,
} from "@al-ft/midgard-fault-proofs";

import {
  assertWatcherLocalKupmiosNativeObservation,
  type WatcherLocalKupmiosNativeObservation,
} from "../l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import {
  createWatcherResolvedBlockObservationSource,
  readWatcherResolvedBlockObservation,
  resolveWatcherBlockObservationTransactions,
} from "../l1/resolved-block-observation.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  assertWatcherDeploymentProtocolScriptAuthority,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentProtocolScriptAuthority,
} from "../runtime/deployment-identity.js";
import { deriveObservation } from "./authenticated-state-queue-observation.derive-observation.js";
import {
  admittedHeaders,
  admittedSources,
  assertWatcherStateQueueObservation,
  RELEASE_FINALITY_DEPTH,
  stateQueueProgressRecordDue,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueObservationSource,
} from "./authenticated-state-queue-observation.parse-persisted-header.js";
import { admitObservation } from "./authenticated-state-queue-observation.parse-persisted-observation.js";
import { candidateRawBlockTransactions } from "./authenticated-state-queue-observation.queue-output.js";
import {
  resolveRetainedHeaderAtBoundary,
  restoreTrustedPersistedObservationChain,
} from "./authenticated-state-queue-observation.restore-trusted-persisted-observation-chain.js";
import {
  type PersistedRestoreReaders,
  snapshotObservationAtBoundary,
} from "./authenticated-state-queue-observation.snapshot-observation-at-boundary.js";

export const createWatcherStateQueueObservationSource = ({
  deploymentIdentity,
  rawSource,
  inclusionRawSource,
}: {
  inclusionRawSource?: LocalKupmiosFraudProofRawSource;
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  rawSource: LocalKupmiosFraudProofRawSource;
}): WatcherStateQueueObservationSource => {
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const authority =
    watcherDeploymentProtocolScriptAuthority(deploymentIdentity);
  assertWatcherDeploymentProtocolScriptAuthority(authority);
  const sourceDetails = localKupmiosHttpOgmiosRawSourceDetails(rawSource);
  if (
    sourceDetails === null ||
    sourceDetails.deploymentIdentityDigest !== deploymentIdentity.manifestId ||
    sourceDetails.blueprintHash !== deploymentIdentity.blueprintHash ||
    sourceDetails.confirmationDepth !== RELEASE_FINALITY_DEPTH
  ) {
    throw new Error(
      "raw state-queue source is not bound to the verified deployment",
    );
  }
  const resolvedBlockSource = createWatcherResolvedBlockObservationSource({
    deploymentIdentity,
    rawSource,
  });
  const includedBlockSource =
    inclusionRawSource === undefined
      ? null
      : createWatcherResolvedBlockObservationSource({
          deploymentIdentity,
          rawSource: inclusionRawSource,
          minimumConfirmationDepth: 1,
        });
  const readers: PersistedRestoreReaders = {
    readBlock: (point) =>
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: rawSource,
        point,
      }),
    readTransaction: (txHash, point) =>
      readAdmittedLocalKupmiosRawTransaction({
        source: rawSource,
        txHash,
        expectedInclusionPoint: point,
        minimumConfirmationDepth: RELEASE_FINALITY_DEPTH,
      }),
    readAddress: (address, point) =>
      readAdmittedLocalKupmiosAddressUtxosAtPoint({
        source: rawSource,
        address,
        point,
      }),
    readUnitHistory: (unit, point) =>
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: rawSource,
        unit,
        point,
      }),
  };
  // All four entry points begin by pinning a new boundary and publish their
  // observation only after acquisition succeeds. A moving provider head can
  // therefore retry the whole read while retaining exclusive source ownership.
  const capture = <T>(
    read: () => Promise<T>,
    captureSource = rawSource,
  ): Promise<T> =>
    withLocalKupmiosSourceCapture(captureSource, async () => {
      for (let attempt = 0; ; attempt += 1) {
        try {
          return await read();
        } catch (error) {
          if (
            !(error instanceof LocalKupmiosCheckpointChangedError) ||
            attempt >= 2
          )
            throw error;
        }
      }
    });
  let latestFinalized: WatcherAuthenticatedStateQueueObservation | null = null;
  const source = Object.freeze({
    ...(inclusionRawSource === undefined || includedBlockSource === null
      ? {}
      : {
          observeIncluded: async ({
            nativeBlock,
            localObservation,
            previous,
          }: {
            nativeBlock: WatcherNativeBlockAdmission;
            localObservation: WatcherLocalKupmiosNativeObservation;
            previous: WatcherAuthenticatedStateQueueObservation;
          }) =>
            capture(async () => {
              assertWatcherStateQueueObservation(previous);
              if (
                previous.deploymentIdentityDigest !==
                  deploymentIdentity.manifestId ||
                previous.protocolScriptAuthorityDigest !==
                  authority.authorityDigest ||
                BigInt(previous.nativePoint.blockNo) >=
                  BigInt(nativeBlock.blockNo)
              )
                throw new Error(
                  "included queue predecessor is foreign or non-monotone",
                );
              await readAdmittedLocalKupmiosBoundary({
                source: inclusionRawSource,
                observationDepth: "inclusion",
              });
              const resolvedBlock = await includedBlockSource.observe({
                nativeBlock,
                localObservation,
              });
              const { rawBlock } =
                readWatcherResolvedBlockObservation(resolvedBlock);
              const candidates = candidateRawBlockTransactions({
                rawBlock,
                queue: previous.finalizedQueue,
                currentLock: previous.finalizedCorrectionLock,
                stateQueuePolicyId:
                  authority.protocolScriptHashes.stateQueueMint,
                hubOraclePolicyId: authority.protocolScriptHashes.hubOracleMint,
              });
              const rawTransactions =
                await resolveWatcherBlockObservationTransactions(
                  resolvedBlock,
                  candidates.map(({ txHash }) => txHash),
                );
              return admitObservation(
                deriveObservation({
                  nativeBlock,
                  localObservation,
                  authority,
                  sourceId: sourceDetails.sourceId,
                  previous,
                  rawTransactions,
                  minimumConfirmationDepth: 1,
                }),
              );
            }, inclusionRawSource),
        }),
    latestFinalizedObservation: () => latestFinalized,
    observe: ({ nativeBlock, localObservation, previous }) =>
      capture(async () => {
        if (!admittedSources.has(source)) {
          throw new Error("state-queue observation source is not admitted");
        }
        assertWatcherLocalKupmiosNativeObservation(
          localObservation,
          nativeBlock,
        );
        if (previous !== null) {
          assertWatcherStateQueueObservation(previous);
          if (
            previous.deploymentIdentityDigest !==
              deploymentIdentity.manifestId ||
            previous.protocolScriptAuthorityDigest !==
              authority.authorityDigest ||
            BigInt(previous.nativePoint.blockNo) >= BigInt(nativeBlock.blockNo)
          ) {
            throw new Error(
              "state-queue observation predecessor is foreign or non-monotone",
            );
          }
        }
        // Each live capture needs a fresh provider tip. Reusing the bootstrap
        // boundary makes later finalized transactions appear under-confirmed.
        // Keep the refreshed boundary pinned until all reads finish.
        await readAdmittedLocalKupmiosBoundary({ source: rawSource });
        const resolvedBlock = await resolvedBlockSource.observe({
          nativeBlock,
          localObservation,
        });
        const { rawBlock } = readWatcherResolvedBlockObservation(resolvedBlock);
        const candidates = candidateRawBlockTransactions({
          rawBlock,
          queue: previous?.finalizedQueue ?? [],
          currentLock: previous?.finalizedCorrectionLock ?? null,
          stateQueuePolicyId: authority.protocolScriptHashes.stateQueueMint,
          hubOraclePolicyId: authority.protocolScriptHashes.hubOracleMint,
        });
        const rawTransactions =
          await resolveWatcherBlockObservationTransactions(
            resolvedBlock,
            candidates.map(({ txHash }) => txHash),
          );
        const result = deriveObservation({
          nativeBlock,
          localObservation,
          authority,
          sourceId: sourceDetails.sourceId,
          previous,
          rawTransactions,
        });
        latestFinalized = admitObservation(result);
        if (result.checkpoints.length > 0) return latestFinalized;
        // A quiet queue still moves the durable cursor forward periodically,
        // so a restart replays a bounded suffix rather than every block since
        // the last queue transaction.
        return previous !== null &&
          stateQueueProgressRecordDue(previous, nativeBlock)
          ? latestFinalized
          : previous;
      }),
    bootstrap: () =>
      capture(async () => {
        if (!admittedSources.has(source)) {
          throw new Error("state-queue observation source is not admitted");
        }
        const admittedBoundary = await readAdmittedLocalKupmiosBoundary({
          source: rawSource,
        });
        const intersection = Object.freeze({
          blockHash: admittedBoundary.kupoCheckpoint.blockHash,
          blockNo: admittedBoundary.kupoCheckpoint.blockNo,
          slot: admittedBoundary.kupoCheckpoint.slot,
        });
        const previous = admitObservation(
          await snapshotObservationAtBoundary({
            intersection,
            authority,
            sourceId: sourceDetails.sourceId,
            readers,
          }),
        );
        return Object.freeze({
          previous,
          discardedObservationCount: 0,
          replayIntersection: Object.freeze({
            blockHash: intersection.blockHash,
            blockNo: intersection.blockNo,
            slot: intersection.slot,
            chainPointId: admittedBoundary.kupoCheckpoint.pointId,
          }),
          catchupBoundary: Object.freeze({
            ...intersection,
            chainPointId: admittedBoundary.kupoCheckpoint.pointId,
            finalityDepth: admittedBoundary.confirmationDepth.toString(),
            ogmiosTipBlockNo: admittedBoundary.ogmiosTip.blockNo,
          }),
        });
      }),
    restore: ({ persistedObservations }) =>
      capture(async () => {
        if (!admittedSources.has(source)) {
          throw new Error("state-queue observation source is not admitted");
        }
        // Recorded final history is fact. A rollback below any row is
        // handled by the dedicated rollback path before restore runs again,
        // so the store is verified structurally and never re-read from Kupo.
        const restored = restoreTrustedPersistedObservationChain({
          persistedObservations,
          authority,
          sourceId: sourceDetails.sourceId,
          maximumObservations: sourceDetails.automaticRecoveryMaxDepth,
        });
        return Object.freeze({
          previous: admitObservation(restored.previous),
          discardedObservationCount: restored.discardedObservationCount,
          replayIntersection: restored.replayIntersection,
          catchupBoundary: restored.catchupBoundary,
        });
      }),
    resolveRetainedHeader: ({ headerHash }) =>
      capture(async () => {
        if (!admittedSources.has(source)) {
          throw new Error("state-queue observation source is not admitted");
        }
        const header = await resolveRetainedHeaderAtBoundary({
          headerHash,
          authority,
          readers: {
            readBoundary: () =>
              readAdmittedLocalKupmiosBoundary({
                source: inclusionRawSource ?? rawSource,
                observationDepth:
                  inclusionRawSource === undefined
                    ? "release_finality"
                    : "inclusion",
              }),
            readHistory: (unit, point) =>
              readAdmittedLocalKupmiosUnitHistoryAtPoint({
                source: inclusionRawSource ?? rawSource,
                unit,
                point,
              }),
            readTransaction: (txHash, point) =>
              readAdmittedLocalKupmiosRawTransaction({
                source: inclusionRawSource ?? rawSource,
                txHash,
                expectedInclusionPoint: point,
                minimumConfirmationDepth:
                  inclusionRawSource === undefined ? RELEASE_FINALITY_DEPTH : 1,
              }),
          },
        });
        admittedHeaders.add(header);
        return header;
      }, inclusionRawSource ?? rawSource),
  } satisfies WatcherStateQueueObservationSource);
  admittedSources.add(source);
  return source;
};
