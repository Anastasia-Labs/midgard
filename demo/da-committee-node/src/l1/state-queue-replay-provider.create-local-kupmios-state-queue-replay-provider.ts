import * as SDK from "@al-ft/midgard-sdk";

import { joinNativeReads } from "./provider.join-native-reads.js";
import {
  L1SourceIntegrityError,
  StateQueueHistoryNotExtendingAnchorError,
} from "./source-integrity.js";
import {
  correctionLockWitness,
  reconstruct,
} from "./state-queue-replay-provider.correction-lock-witness.js";
import { fetchCorrectionLockOutputs } from "./state-queue-replay-provider.decode-correction-lock-output.js";
import {
  digest,
  fetchAncestor,
  fetchSpend,
  HEX_28,
  HEX_32,
  httpUrl,
  json,
  type Point,
  point,
  type Queue,
  sameQueue,
  type Spend,
  STATE_QUEUE_REPLAY_REQUEST_TIMEOUT_MS,
  type StateQueueReplayFetch,
  type StateQueueReplayWebSocket,
  type StateQueueReplayWebSocketFactory,
} from "./state-queue-replay-provider.open-rpc.js";
import {
  fetchOutputs,
  readTransaction,
} from "./state-queue-replay-provider.parse-transaction.js";

/**
 * Operational snapshot tip height. Replay retirement depth uses the selected
 * tip on its raw block response; this height is only a conservative cap.
 */
export const fetchOgmiosTipBlockNo = async (
  ogmiosUrl: string,
  fetchImpl: StateQueueReplayFetch,
): Promise<number> => {
  const body = (await json(fetchImpl, httpUrl(ogmiosUrl), {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method: "queryNetwork/blockHeight",
      id: "midgard-committee-state-queue-tip-height-v1",
    }),
  })) as { result?: unknown };
  const height = body.result;
  if (
    typeof height !== "number" ||
    !Number.isSafeInteger(height) ||
    height < 0
  ) {
    throw new Error("Ogmios tip height is invalid");
  }
  return height;
};

/**
 * Whether Kupo's chain still holds `target`: its checkpoint at that slot is
 * the block `target` names. False when the block was rolled back, and when
 * Kupo keeps no checkpoint at that exact slot.
 */
export const kupoHoldsChainPoint = async (
  kupoUrl: string,
  target: Point,
  fetchImpl: StateQueueReplayFetch,
  timeoutMs = STATE_QUEUE_REPLAY_REQUEST_TIMEOUT_MS,
): Promise<boolean> => {
  const body = await json(
    fetchImpl,
    `${httpUrl(kupoUrl)}/checkpoints/${target.slot.toString()}`,
    undefined,
    timeoutMs,
  );
  if (body === null) return false;
  const held = point(body, "Kupo checkpoint");
  return (
    held.slot === target.slot &&
    held.blockHash === target.blockHash.toLowerCase()
  );
};

/** Independent committee-side local Kupmios ordered state-queue replay. */
export const createLocalKupmiosStateQueueReplayProvider = ({
  deploymentIdentityDigest,
  stateQueuePolicyId,
  stateQueueAddress,
  hubOraclePolicyId,
  correctionLockAddress,
  fraudProofPolicyId,
  fraudProofAddress,
  kupoUrl,
  ogmiosUrl,
  fetchImpl = fetch,
  webSocketFactory = (url) =>
    new WebSocket(url) as unknown as StateQueueReplayWebSocket,
}: {
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly hubOraclePolicyId: string;
  readonly correctionLockAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAddress: string;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly fetchImpl?: StateQueueReplayFetch;
  readonly webSocketFactory?: StateQueueReplayWebSocketFactory;
}): ((
  previousQueue: Queue,
  currentQueue: Queue,
  tipBlockNo: number,
  limit: number,
) => Promise<readonly SDK.StateQueueAuthenticatedReplayCheckpoint[]>) => {
  if (
    !HEX_32.test(deploymentIdentityDigest) ||
    !HEX_28.test(stateQueuePolicyId) ||
    !HEX_28.test(hubOraclePolicyId) ||
    !HEX_28.test(fraudProofPolicyId) ||
    correctionLockAddress.trim() === "" ||
    fraudProofAddress.trim() === ""
  ) {
    throw new Error("committee state-queue replay release identity is invalid");
  }
  // Walks history in order from `previousQueue` toward `currentQueue`, and
  // stops after `limit` checkpoints: the caller resumes from the final part
  // of what it was given.
  return async (previousQueue, currentQueue, tipBlockNo, limit) => {
    if (!Number.isSafeInteger(tipBlockNo) || tipBlockNo < 0) {
      throw new Error("state-queue replay tip height is invalid");
    }
    if (!Number.isSafeInteger(limit) || limit < 1) {
      throw new Error("state-queue replay checkpoint limit is invalid");
    }
    let queue = previousQueue;
    const checkpoints: SDK.StateQueueAuthenticatedReplayCheckpoint[] = [];
    for (;;) {
      if (sameQueue(queue, currentQueue) || checkpoints.length >= limit)
        return checkpoints;
      const spends = await joinNativeReads(
        queue.map(({ outRef }) => fetchSpend(kupoUrl, outRef, fetchImpl)),
      );
      const unique = new Map<string, Spend>();
      for (const spend of spends) {
        if (spend === null) continue;
        const prior = unique.get(spend.transactionHash);
        if (
          prior !== undefined &&
          (prior.point.slot !== spend.point.slot ||
            prior.point.blockHash !== spend.point.blockHash)
        ) {
          throw new L1SourceIntegrityError(
            "Kupo replay assigned one transaction to competing points",
          );
        }
        unique.set(spend.transactionHash, spend);
      }
      if (unique.size === 0)
        throw new StateQueueHistoryNotExtendingAnchorError(
          "committee replay cannot advance its durable queue",
        );
      const transactions = await joinNativeReads(
        [...unique.values()].map(
          async (spend) =>
            await readTransaction(
              ogmiosUrl,
              await fetchAncestor(kupoUrl, spend.point.slot, fetchImpl),
              spend,
              webSocketFactory,
            ),
        ),
      );
      transactions.sort(
        (left, right) =>
          left.blockNo - right.blockNo ||
          left.transactionIndex - right.transactionIndex ||
          left.transactionHash.localeCompare(right.transactionHash),
      );
      const transaction = transactions[0]!;
      if (tipBlockNo < transaction.blockNo) {
        throw new Error(
          "state-queue snapshot tip precedes a transaction of its history",
        );
      }
      // Bound age to the selected chain that supplied this exact raw block.
      // The earlier snapshot height only caps age; it cannot manufacture depth
      // after a rollback or a contradictory HTTP/chain-sync response.
      const observedTipHeight = Math.min(
        tipBlockNo,
        transaction.selectedChainTip.height,
      );
      const outputs = await fetchOutputs(
        kupoUrl,
        transaction.transactionHash,
        stateQueueAddress,
        stateQueuePolicyId,
        fetchImpl,
      );
      const nextQueue = reconstruct(queue, transaction, outputs);
      const lockOutputs = await fetchCorrectionLockOutputs(
        kupoUrl,
        transaction.transactionHash,
        correctionLockAddress,
        hubOraclePolicyId,
        fetchImpl,
      );
      const lockWitness = await correctionLockWitness({
        transaction,
        outputs: lockOutputs,
        stateQueuePolicyId,
        hubOraclePolicyId,
        correctionLockAddress,
        fraudProofPolicyId,
        fraudProofAddress,
        kupoUrl,
        fetchImpl,
      });
      const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
        deploymentIdentityDigest,
        stateQueuePolicyId,
        transactionHash: transaction.transactionHash,
        blockHash: transaction.blockHash,
        slot: transaction.slot.toString(),
        blockNo: transaction.blockNo.toString(),
        transactionIndex: transaction.transactionIndex.toString(),
        chainPointId: digest({
          source: "da-committee-local-kupmios-state-queue-replay-v1",
          blockHash: transaction.blockHash,
          slot: transaction.slot,
          blockNo: transaction.blockNo,
          transactionIndex: transaction.transactionIndex,
        }),
        finalityDepth: (observedTipHeight - transaction.blockNo + 1).toString(),
        mintPolicyIds: transaction.mintPolicyIds,
        redeemers: transaction.redeemers,
        spentInputOutRefs: transaction.spentInputOutRefs,
        referenceInputOutRefs: transaction.referenceInputOutRefs,
        correctionLockWitness: lockWitness,
        previousQueue: queue,
        nextQueue,
      });
      if (checkpoint === null) {
        throw new L1SourceIntegrityError(
          "committee state-queue transaction failed exact checkpoint derivation",
        );
      }
      checkpoints.push(checkpoint);
      queue = nextQueue;
    }
  };
};
