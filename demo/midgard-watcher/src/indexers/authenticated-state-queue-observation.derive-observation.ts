import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Transaction,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { type WatcherLocalKupmiosNativeObservation } from "../l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import { watcherDeploymentProtocolScriptAuthority } from "../runtime/deployment-identity.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import { advanceCurrentLock } from "./authenticated-state-queue-observation.advance-current-lock.js";
import {
  correctionLockWitness,
  sameQueue,
} from "./authenticated-state-queue-observation.correction-lock-witness.js";
import {
  type QueueNode,
  RELEASE_FINALITY_DEPTH,
  WATCHER_AUTHENTICATED_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION,
  type WatcherAuthenticatedStateQueueObservation,
} from "./authenticated-state-queue-observation.parse-persisted-header.js";
import {
  mintPolicyIds,
  outputHasPolicy,
  outputReferences,
  rawOutputHasPolicy,
} from "./authenticated-state-queue-observation.parse-persisted-observation.js";
import {
  decodedQueueOutputs,
  lockOutput,
} from "./authenticated-state-queue-observation.queue-output.js";
import {
  anchoredHeaderObservation,
  decodeLockOutputs,
  reconstructQueue,
} from "./authenticated-state-queue-observation.reconstruct-queue.js";

export const deriveObservation = ({
  nativeBlock,
  localObservation,
  authority,
  sourceId,
  previous,
  rawTransactions,
  minimumConfirmationDepth = RELEASE_FINALITY_DEPTH,
}: {
  minimumConfirmationDepth?: number;
  nativeBlock: WatcherNativeBlockAdmission;
  localObservation: WatcherLocalKupmiosNativeObservation;
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthority>;
  sourceId: string;
  previous: WatcherAuthenticatedStateQueueObservation | null;
  rawTransactions: readonly FraudProofRawL1Transaction[];
}): WatcherAuthenticatedStateQueueObservation => {
  if (
    localObservation.block.chainPoint.blockHash !== nativeBlock.blockHash ||
    localObservation.block.chainPoint.slot !== nativeBlock.slot ||
    localObservation.block.chainPoint.blockNo !== nativeBlock.blockNo ||
    BigInt(localObservation.block.chainPoint.depth) <
      BigInt(minimumConfirmationDepth) ||
    rawTransactions.length > nativeBlock.transactionIds.length
  ) {
    throw new Error(
      "state-queue source chain point/finality differs from native admission",
    );
  }
  const authenticatedChainPointId = computeFraudProofRawL1PointId({
    blockHash: nativeBlock.blockHash,
    blockNo: nativeBlock.blockNo,
    slot: nativeBlock.slot,
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
  const fraudProofPolicyId = authority.protocolScriptHashes.fraudProofMint;
  const fraudProofAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.fraudProofSpend),
  );
  let queue = previous?.finalizedQueue ?? Object.freeze([] as QueueNode[]);
  let finalizedHeaders = previous?.finalizedHeaders ?? Object.freeze([]);
  let finalizedCorrectionLock = previous?.finalizedCorrectionLock ?? null;
  const checkpoints: SDK.StateQueueAuthenticatedReplayCheckpoint[] = [];
  const seenTransactionIndexes = new Set<number>();
  for (const raw of rawTransactions) {
    const transactionIndex = nativeBlock.transactionIds.indexOf(raw.txHash);
    if (
      transactionIndex < 0 ||
      seenTransactionIndexes.has(transactionIndex) ||
      raw.inclusionPoint.blockHash !== nativeBlock.blockHash ||
      raw.inclusionPoint.slot !== nativeBlock.slot ||
      raw.inclusionPoint.blockNo !== nativeBlock.blockNo ||
      raw.confirmationDepth < minimumConfirmationDepth
    ) {
      throw new Error(
        "resolved transaction was substituted across the native chain point",
      );
    }
    seenTransactionIndexes.add(transactionIndex);
    const normalized = localObservation.block.transactions[transactionIndex];
    if (
      normalized === undefined ||
      normalized.txHash !== raw.txHash ||
      normalized.body.bytesHex !== raw.bodyCbor ||
      normalized.witnessSet.bytesHex !== raw.witnessSetCbor
    ) {
      throw new Error(
        "resolved transaction bytes differ from the admitted watcher block",
      );
    }
    const body = CML.TransactionBody.from_cbor_hex(raw.bodyCbor);
    const policies = mintPolicyIds(body);
    const outputs = body.outputs();
    let outputTouchesQueue = false;
    for (let index = 0; index < outputs.len(); index += 1) {
      outputTouchesQueue ||= outputHasPolicy(
        outputs.get(index),
        stateQueuePolicyId,
      );
    }
    const touchesQueue =
      policies.includes(stateQueuePolicyId) ||
      outputTouchesQueue ||
      raw.resolvedInputs.some(({ outputCbor }) =>
        rawOutputHasPolicy(outputCbor, stateQueuePolicyId),
      );
    if (!touchesQueue) {
      const consumesLock = raw.resolvedInputs.some(
        ({ outRef, outputCbor }) =>
          lockOutput({
            output: CML.TransactionOutput.from_cbor_hex(outputCbor),
            outRef,
            correctionLockAddress,
            hubOraclePolicyId,
          }) !== null,
      );
      const producesLock =
        decodeLockOutputs({
          body,
          transactionHash: raw.txHash,
          correctionLockAddress,
          hubOraclePolicyId,
        }).length > 0;
      if (consumesLock || producesLock) {
        throw new Error(
          "CorrectionLock changed without an authenticated state-queue transition",
        );
      }
      continue;
    }
    const spentInputOutRefs = outputReferences(body.inputs());
    const referenceInputOutRefs = outputReferences(body.reference_inputs());
    const queueOutputs = decodedQueueOutputs({
      body,
      transactionHash: raw.txHash,
      stateQueueAddress,
      stateQueuePolicyId,
    });
    const nextQueue =
      queue.length === 0 && queueOutputs.length === 1
        ? Object.freeze([queueOutputs[0]!.node])
        : reconstructQueue({
            previousQueue: queue,
            transactionHash: raw.txHash,
            spentInputOutRefs,
            resolvedInputs: raw.resolvedInputs,
            outputs: queueOutputs,
            stateQueueAddress,
            stateQueuePolicyId,
          });
    const redeemers = normalized.redeemers.map((redeemer) => ({
      purpose: redeemer.purpose,
      index: redeemer.index,
      cborHex: redeemer.bytes.bytesHex,
    }));
    const lockWitness = correctionLockWitness({
      raw,
      body,
      mintPolicies: policies,
      redeemers,
      stateQueuePolicyId,
      correctionLockAddress,
      hubOraclePolicyId,
      fraudProofPolicyId,
      fraudProofAddress,
      availabilityChallengePolicyId:
        authority.protocolScriptHashes.availabilityChallengeMint,
    });
    const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
      deploymentIdentityDigest: authority.deploymentFingerprint,
      stateQueuePolicyId,
      transactionHash: raw.txHash,
      blockHash: nativeBlock.blockHash,
      slot: nativeBlock.slot,
      blockNo: nativeBlock.blockNo,
      transactionIndex: transactionIndex.toString(),
      chainPointId: authenticatedChainPointId,
      finalityDepth: minimumConfirmationDepth.toString(),
      mintPolicyIds: policies,
      redeemers,
      spentInputOutRefs,
      referenceInputOutRefs,
      correctionLockWitness: lockWitness,
      previousQueue: queue,
      nextQueue,
    });
    if (checkpoint === null) {
      throw new Error(
        "state-queue transaction failed authenticated checkpoint derivation",
      );
    }
    checkpoints.push(checkpoint);
    queue = nextQueue;
    finalizedCorrectionLock = advanceCurrentLock({
      current: finalizedCorrectionLock,
      witness: lockWitness,
      transactionHash: raw.txHash,
      point: nativeBlock,
      chainPointId: authenticatedChainPointId,
      finalityDepth: minimumConfirmationDepth.toString(),
    });
    const byHeaderHash = new Map(
      finalizedHeaders.map((header) => [header.headerHash, header]),
    );
    for (const output of queueOutputs) {
      if (output.header === null) continue;
      byHeaderHash.set(
        output.header.headerHash,
        anchoredHeaderObservation({
          minimumConfirmationDepth,
          prior: byHeaderHash.get(output.header.headerHash),
          header: output.header,
          queueOutRef: output.node.outRef,
          nextHeaderHash: output.nextHeaderHash,
          point: {
            transactionHash: raw.txHash,
            blockHash: nativeBlock.blockHash,
            slot: nativeBlock.slot,
            blockNo: nativeBlock.blockNo,
            chainPointId: authenticatedChainPointId,
          },
        }),
      );
    }
    finalizedHeaders = Object.freeze(
      nextQueue.flatMap(({ headerHash }) => {
        if (headerHash === null) return [];
        const header = byHeaderHash.get(headerHash);
        if (header === undefined) {
          throw new Error(
            "state-queue cursor omitted authenticated HeaderV1 bytes",
          );
        }
        return [header];
      }),
    );
  }
  if (
    previous !== null &&
    checkpoints.length > 0 &&
    !sameQueue(checkpoints[0]!.previousQueue, previous.finalizedQueue)
  ) {
    throw new Error(
      "state-queue checkpoint does not extend its admitted predecessor",
    );
  }
  const nativePoint = Object.freeze({
    blockHash: nativeBlock.blockHash,
    parentBlockHash:
      nativeBlock.prevHash.length === 0 ? null : nativeBlock.prevHash,
    slot: nativeBlock.slot,
    blockNo: nativeBlock.blockNo,
    chainPointId: authenticatedChainPointId,
    finalityDepth: minimumConfirmationDepth.toString(),
  });
  const canonical = {
    schemaVersion: WATCHER_AUTHENTICATED_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION,
    deploymentIdentityDigest: authority.deploymentFingerprint,
    protocolScriptAuthorityDigest: authority.authorityDigest,
    stateQueuePolicyId,
    hubOraclePolicyId,
    nativePoint,
    sourceId,
    previousObservationDigest: previous?.observationDigest ?? null,
    checkpoints: Object.freeze(checkpoints),
    finalizedQueue: Object.freeze([...queue]),
    finalizedHeaders,
    finalizedCorrectionLock,
    correctionLockWitnesses: Object.freeze(
      checkpoints.map(({ correctionLockWitness: witness }) => witness),
    ),
  };
  return Object.freeze({
    ...canonical,
    observationDigest: watcherSha256CanonicalJson(canonical),
  });
};
