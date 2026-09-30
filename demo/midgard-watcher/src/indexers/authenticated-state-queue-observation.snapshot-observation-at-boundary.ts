import "./authenticated-state-queue-observation.correction-lock-witness.js";

import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
  type LocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosUnitHistoryAtPoint,
  settleLocalKupmiosReads,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { watcherDeploymentProtocolScriptAuthority } from "../runtime/deployment-identity.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import { deriveObservation } from "./authenticated-state-queue-observation.derive-observation.js";
import {
  type QueueOutput,
  RELEASE_FINALITY_DEPTH,
  WATCHER_AUTHENTICATED_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "./authenticated-state-queue-observation.parse-persisted-header.js";
import {
  lockOutput,
  queueOutput,
} from "./authenticated-state-queue-observation.queue-output.js";

/** Pure semantic test seam. It never admits the returned structural value. */
export const unsafeDeriveWatcherStateQueueObservationForTest =
  deriveObservation;

export type PersistedRestoreReaders = Readonly<{
  readBlock(
    point: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      pointId: string;
    }>,
  ): Promise<LocalKupmiosRawBlockAtPoint>;
  readTransaction(
    txHash: string,
    point: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      pointId: string;
    }>,
  ): Promise<FraudProofRawL1Transaction>;
  readAddress(
    address: string,
    point: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      pointId: string;
    }>,
  ): Promise<readonly FraudProofRawL1Utxo[]>;
  readUnitHistory?(
    unit: string,
    point: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      pointId: string;
    }>,
  ): ReturnType<typeof readAdmittedLocalKupmiosUnitHistoryAtPoint>;
}>;

export const snapshotObservationAtBoundary = async ({
  intersection,
  authority,
  sourceId,
  readers,
}: {
  intersection: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
  }>;
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthority>;
  sourceId: string;
  readers: PersistedRestoreReaders;
}): Promise<WatcherAuthenticatedStateQueueObservation> => {
  const intersectionPoint = Object.freeze({
    ...intersection,
    pointId: computeFraudProofRawL1PointId(intersection),
  });
  const rawBlock = await readers.readBlock(intersectionPoint);
  if (
    rawBlock.sourceId !== sourceId ||
    rawBlock.point.pointId !== intersectionPoint.pointId
  ) {
    throw new Error("state-queue bootstrap boundary is not canonical");
  }
  if (readers.readUnitHistory === undefined) {
    throw new Error("state-queue bootstrap requires exact unit history");
  }
  const provenanceForOutRef = async ({
    unit,
    outRef,
    expectedOutput,
  }: {
    unit: string;
    outRef: string;
    expectedOutput: CML.TransactionOutput;
  }): Promise<FraudProofRawL1Transaction> => {
    const [txHash, outputIndexText] = outRef.split("#") as [string, string];
    const history = await readers.readUnitHistory!(unit, intersectionPoint);
    const creation = history.transactions.find(
      (entry) => entry.txHash === txHash,
    );
    if (creation === undefined) {
      throw new Error(
        "state-queue bootstrap output is absent from unit history",
      );
    }
    const transaction = await readers.readTransaction(
      txHash,
      creation.inclusionPoint,
    );
    const outputIndex = Number(outputIndexText);
    const outputs = CML.TransactionBody.from_cbor_hex(
      transaction.bodyCbor,
    ).outputs();
    if (
      !Number.isSafeInteger(outputIndex) ||
      outputIndex < 0 ||
      outputIndex >= outputs.len() ||
      outputs.get(outputIndex).to_canonical_cbor_hex() !==
        expectedOutput.to_canonical_cbor_hex()
    ) {
      throw new Error(
        "state-queue bootstrap output differs from its creation transaction",
      );
    }
    return transaction;
  };
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
  const [queueUtxos, lockUtxos] = await settleLocalKupmiosReads([
    readers.readAddress(stateQueueAddress, intersectionPoint),
    readers.readAddress(correctionLockAddress, intersectionPoint),
  ]);
  const queueOutputs = queueUtxos.flatMap((utxo) => {
    const decoded = queueOutput({
      output: CML.TransactionOutput.from_cbor_hex(utxo.outputCbor),
      outRef: utxo.outRef,
      stateQueueAddress,
      stateQueuePolicyId,
    });
    return decoded === null ? [] : [decoded];
  });
  const byHeaderHash = new Map(
    queueOutputs.map((output) => [output.node.headerHash, output]),
  );
  const orderedQueue: QueueOutput[] = [];
  let identity: string | null = null;
  while (true) {
    const output = byHeaderHash.get(identity);
    if (output === undefined) break;
    orderedQueue.push(output);
    byHeaderHash.delete(identity);
    if (output.nextHeaderHash === null) break;
    identity = output.nextHeaderHash;
  }
  if (orderedQueue.length === 0 || byHeaderHash.size !== 0) {
    throw new Error(
      "state-queue bootstrap snapshot is not one exact linked queue",
    );
  }
  const liveLock = lockUtxos.flatMap((utxo) => {
    const decoded = lockOutput({
      output: CML.TransactionOutput.from_cbor_hex(utxo.outputCbor),
      outRef: utxo.outRef,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return decoded === null ? [] : [decoded];
  });
  if (liveLock.length !== 1) {
    throw new Error(
      "state-queue bootstrap snapshot has no exact CorrectionLock",
    );
  }
  const finalizedHeaders = Object.freeze(
    (
      await settleLocalKupmiosReads(
        orderedQueue.map(async (output) => {
          if (output.header === null) return null;
          const creation = await provenanceForOutRef({
            unit: `${stateQueuePolicyId}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${output.header.headerHash}`,
            outRef: output.node.outRef,
            expectedOutput: CML.TransactionOutput.from_cbor_hex(
              queueUtxos.find(({ outRef }) => outRef === output.node.outRef)!
                .outputCbor,
            ),
          });
          return Object.freeze({
            ...output.header,
            queueOutRef: output.node.outRef,
            nextHeaderHash: output.nextHeaderHash,
            observedTransactionHash: creation.txHash,
            observedBlockHash: creation.inclusionPoint.blockHash,
            observedSlot: creation.inclusionPoint.slot,
            observedBlockNo: creation.inclusionPoint.blockNo,
            observedChainPointId: creation.inclusionPoint.pointId,
            finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
          });
        }),
      )
    ).filter(
      (header): header is WatcherStateQueueHeaderObservation => header !== null,
    ),
  );
  const liveLockOutput = CML.TransactionOutput.from_cbor_hex(
    lockUtxos.find(({ outRef }) => outRef === liveLock[0]!.outRef)!.outputCbor,
  );
  const lockCreation = await provenanceForOutRef({
    unit: SDK.correctionLockUnit(hubOraclePolicyId),
    outRef: liveLock[0]!.outRef,
    expectedOutput: liveLockOutput,
  });
  const nativePoint = Object.freeze({
    blockHash: intersection.blockHash,
    parentBlockHash: rawBlock.parentBlockHash,
    slot: intersection.slot,
    blockNo: intersection.blockNo,
    chainPointId: intersectionPoint.pointId,
    finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
  });
  const canonical = {
    schemaVersion: WATCHER_AUTHENTICATED_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION,
    deploymentIdentityDigest: authority.deploymentFingerprint,
    protocolScriptAuthorityDigest: authority.authorityDigest,
    stateQueuePolicyId,
    hubOraclePolicyId,
    nativePoint,
    sourceId,
    previousObservationDigest: null,
    checkpoints: Object.freeze([]),
    finalizedQueue: Object.freeze(orderedQueue.map(({ node }) => node)),
    finalizedHeaders,
    finalizedCorrectionLock: Object.freeze({
      outRef: liveLock[0]!.outRef,
      datum: liveLock[0]!.datum,
      observedTransactionHash: lockCreation.txHash,
      observedBlockHash: lockCreation.inclusionPoint.blockHash,
      observedSlot: lockCreation.inclusionPoint.slot,
      observedBlockNo: lockCreation.inclusionPoint.blockNo,
      observedChainPointId: lockCreation.inclusionPoint.pointId,
      finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
    }),
    correctionLockWitnesses: Object.freeze([]),
  };
  return Object.freeze({
    ...canonical,
    observationDigest: watcherSha256CanonicalJson(canonical),
  });
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
