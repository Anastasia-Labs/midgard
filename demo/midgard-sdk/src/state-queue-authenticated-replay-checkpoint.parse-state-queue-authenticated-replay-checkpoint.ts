import {
  canonicalQueue,
  exactRecord,
  HEX_28,
  HEX_32,
  type Json,
  NATURAL,
  stableJson,
  STATE_QUEUE_AUTHENTICATED_REPLAY_CHECKPOINT_SCHEMA_VERSION,
  type StateQueueAuthenticatedReplayCheckpoint,
} from "./state-queue-authenticated-replay-checkpoint.canonical-redeemer.js";
import {
  deriveStateQueueAuthenticatedReplayCheckpoint,
  parseNodes,
} from "./state-queue-authenticated-replay-checkpoint.derive-state-queue-authenticated-replay-checkpoint.js";
import {
  parseStateQueueAuthenticatedTransition,
  parseStateQueueCorrectionLockWitness,
  type StateQueueAuthenticatedTransition,
  type StateQueueTransitionNode,
} from "./state-queue-correction-transition.js";

export const parseStateQueueAuthenticatedReplayCheckpoint = (
  input: unknown,
): StateQueueAuthenticatedReplayCheckpoint | null => {
  const record = exactRecord(input, [
    "schemaVersion",
    "deploymentIdentityDigest",
    "stateQueuePolicyId",
    "transactionHash",
    "blockHash",
    "slot",
    "blockNo",
    "transactionIndex",
    "chainPointId",
    "finalityDepth",
    "checkpointKind",
    "mintPolicyIds",
    "stateQueueMintRedeemer",
    "spentInputOutRefs",
    "referenceInputOutRefs",
    "correctionLockWitness",
    "previousQueue",
    "nextQueue",
    "terminalTransition",
    "checkpointDigest",
  ]);
  const mint =
    record?.stateQueueMintRedeemer === null
      ? null
      : exactRecord(record?.stateQueueMintRedeemer, [
          "purpose",
          "index",
          "cborHex",
        ]);
  const previousQueue = parseNodes(record?.previousQueue);
  const nextQueue = parseNodes(record?.nextQueue);
  const terminal =
    record?.terminalTransition === null
      ? null
      : parseStateQueueAuthenticatedTransition(record?.terminalTransition);
  const correctionLockWitness = parseStateQueueCorrectionLockWitness(
    record?.correctionLockWitness,
  );
  if (
    record === null ||
    record.schemaVersion !==
      STATE_QUEUE_AUTHENTICATED_REPLAY_CHECKPOINT_SCHEMA_VERSION ||
    typeof record.deploymentIdentityDigest !== "string" ||
    typeof record.stateQueuePolicyId !== "string" ||
    typeof record.transactionHash !== "string" ||
    typeof record.blockHash !== "string" ||
    typeof record.slot !== "string" ||
    typeof record.blockNo !== "string" ||
    typeof record.transactionIndex !== "string" ||
    typeof record.chainPointId !== "string" ||
    typeof record.finalityDepth !== "string" ||
    !Array.isArray(record.mintPolicyIds) ||
    record.mintPolicyIds.some((value) => typeof value !== "string") ||
    !Array.isArray(record.spentInputOutRefs) ||
    record.spentInputOutRefs.some((value) => typeof value !== "string") ||
    !Array.isArray(record.referenceInputOutRefs) ||
    record.referenceInputOutRefs.some((value) => typeof value !== "string") ||
    correctionLockWitness === null ||
    previousQueue === null ||
    nextQueue === null ||
    (mint !== null &&
      (typeof mint.purpose !== "string" ||
        typeof mint.index !== "string" ||
        typeof mint.cborHex !== "string")) ||
    typeof record.checkpointDigest !== "string" ||
    !HEX_32.test(record.checkpointDigest)
  ) {
    return null;
  }
  const derived = deriveStateQueueAuthenticatedReplayCheckpoint({
    deploymentIdentityDigest: record.deploymentIdentityDigest,
    stateQueuePolicyId: record.stateQueuePolicyId,
    transactionHash: record.transactionHash,
    blockHash: record.blockHash,
    slot: record.slot,
    blockNo: record.blockNo,
    transactionIndex: record.transactionIndex,
    chainPointId: record.chainPointId,
    finalityDepth: record.finalityDepth,
    mintPolicyIds: record.mintPolicyIds as string[],
    redeemers:
      mint === null
        ? []
        : [
            {
              purpose: mint.purpose as string,
              index: mint.index as string,
              cborHex: mint.cborHex as string,
            },
          ],
    spentInputOutRefs: record.spentInputOutRefs as string[],
    referenceInputOutRefs: record.referenceInputOutRefs as string[],
    correctionLockWitness,
    previousQueue,
    nextQueue,
  });
  return derived !== null &&
    derived.checkpointKind === record.checkpointKind &&
    derived.checkpointDigest === record.checkpointDigest &&
    stableJson(derived.terminalTransition as unknown as Json) ===
      stableJson(terminal as unknown as Json)
    ? derived
    : null;
};

export const replayStateQueueAuthenticatedCheckpoints = ({
  deploymentIdentityDigest,
  stateQueuePolicyId,
  minimumFinalityDepth,
  anchor,
  checkpoints: checkpointInputs,
}: {
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly minimumFinalityDepth: bigint;
  readonly anchor: Readonly<{
    queue: readonly StateQueueTransitionNode[];
    blockNo: string;
    transactionIndex: string;
  }>;
  readonly checkpoints: readonly unknown[];
}): Readonly<{
  queue: readonly StateQueueTransitionNode[];
  lastBlockNo: string;
  lastTransactionIndex: string;
  terminals: readonly StateQueueAuthenticatedTransition[];
}> | null => {
  if (
    !HEX_32.test(deploymentIdentityDigest) ||
    !HEX_28.test(stateQueuePolicyId) ||
    minimumFinalityDepth <= 0n ||
    !canonicalQueue(anchor.queue, true) ||
    !NATURAL.test(anchor.blockNo) ||
    !NATURAL.test(anchor.transactionIndex)
  ) {
    return null;
  }
  let queue = anchor.queue;
  let blockNo = anchor.blockNo;
  let transactionIndex = anchor.transactionIndex;
  const terminals: StateQueueAuthenticatedTransition[] = [];
  for (const input of checkpointInputs) {
    const checkpoint = parseStateQueueAuthenticatedReplayCheckpoint(input);
    const ordered =
      checkpoint !== null &&
      (BigInt(checkpoint.blockNo) > BigInt(blockNo) ||
        (checkpoint.blockNo === blockNo &&
          BigInt(checkpoint.transactionIndex) > BigInt(transactionIndex)));
    if (
      checkpoint === null ||
      !ordered ||
      checkpoint.deploymentIdentityDigest !== deploymentIdentityDigest ||
      checkpoint.stateQueuePolicyId !== stateQueuePolicyId ||
      BigInt(checkpoint.finalityDepth) < minimumFinalityDepth ||
      stableJson(checkpoint.previousQueue as unknown as Json) !==
        stableJson(queue as unknown as Json)
    ) {
      return null;
    }
    queue = checkpoint.nextQueue;
    blockNo = checkpoint.blockNo;
    transactionIndex = checkpoint.transactionIndex;
    if (checkpoint.terminalTransition !== null) {
      terminals.push(checkpoint.terminalTransition);
    }
  }
  return Object.freeze({
    queue,
    lastBlockNo: blockNo,
    lastTransactionIndex: transactionIndex,
    terminals: Object.freeze(terminals),
  });
};
