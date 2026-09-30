import {
  deriveStateQueueAuthenticatedReplayCheckpoint,
  type StateQueueAuthenticatedReplayCheckpoint,
  type StateQueueAuthenticatedTransition,
  type StateQueueTransitionNode,
} from "@al-ft/midgard-sdk";

import {
  fetchKupoAncestorPoint,
  fetchKupoSpend,
  type FetchLike,
  type KupoSpend,
  readOgmiosBlockTransaction,
  type WebSocketFactory,
} from "../l1-tx-order-carriage.js";
import { sameQueue } from "./state-queue-correction-observer.create-database-state-queue-correction-observer-store.js";
import {
  fetchKupoTransactionCorrectionLockOutputs,
  fetchTip,
  sameSpend,
  STATE_QUEUE_CORRECTION_REQUEST_TIMEOUT_MS,
  withRequestTimeout,
} from "./state-queue-correction-observer.decode-kupo-correction-lock-match.js";
import { deriveCorrectionLockWitnessFromRaw } from "./state-queue-correction-observer.derive-correction-lock-witness-from-raw.js";
import {
  fetchKupoTransactionQueueOutputs,
  reconstructQueueAfterTransaction,
} from "./state-queue-correction-observer.fetch-kupo-transaction-queue-outputs.js";
import {
  digest,
  type StateQueueCorrectionObserverSource,
} from "./state-queue-correction-observer.parse-state-queue-correction-observer-state.js";
import { outRef } from "./state-queue-correction-observer.reconcile-state-queue-correction-observer.js";

/** Node-owned local Kupmios/Ogmios source; no watcher process is consulted. */
export const makeLocalKupmiosStateQueueCorrectionSource = ({
  deploymentIdentityDigest,
  stateQueuePolicyId,
  stateQueueAddress,
  hubOraclePolicyId,
  correctionLockAddress,
  fraudProofPolicyId,
  fraudProofAddress,
  kupoUrl,
  ogmiosUrl,
  readQueue,
  fetchImpl: unboundedFetch = fetch,
  requestTimeoutMs = STATE_QUEUE_CORRECTION_REQUEST_TIMEOUT_MS,
  webSocketFactory,
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
  readonly readQueue: () => Promise<readonly StateQueueTransitionNode[]>;
  readonly fetchImpl?: FetchLike;
  readonly requestTimeoutMs?: number;
  readonly webSocketFactory?: WebSocketFactory;
}): StateQueueCorrectionObserverSource => {
  const fetchImpl = withRequestTimeout(unboundedFetch, requestTimeoutMs);
  const canonicalDepth = async (
    transition: StateQueueAuthenticatedTransition,
  ): Promise<bigint | null> => {
    const spends = await Promise.all(
      transition.consumedQueueOutRefs.map((label) =>
        fetchKupoSpend({ kupoUrl, outRef: outRef(label), fetchImpl }),
      ),
    );
    if (
      spends.some(
        (spend) =>
          spend === null ||
          spend.transactionId !== transition.transactionHash ||
          spend.point.headerHash !== transition.blockHash,
      )
    ) {
      return null;
    }
    const tip = await fetchTip(ogmiosUrl, fetchImpl);
    const blockNo = BigInt(transition.blockNo);
    return BigInt(tip.blockNo) < blockNo
      ? null
      : BigInt(tip.blockNo) - blockNo + 1n;
  };
  return {
    readQueue,
    canonicalDepth,
    observeTransitions: async (previousQueue, nextQueue) => {
      let workingQueue = previousQueue;
      const observations: StateQueueAuthenticatedReplayCheckpoint[] = [];
      const tip = await fetchTip(ogmiosUrl, fetchImpl);
      for (let replayed = 0; replayed < 1_000; replayed += 1) {
        if (sameQueue(workingQueue, nextQueue)) return observations;
        const spends = await Promise.all(
          workingQueue.map(({ outRef: label }) =>
            fetchKupoSpend({ kupoUrl, outRef: outRef(label), fetchImpl }),
          ),
        );
        const uniqueSpends = new Map<string, KupoSpend>();
        for (const spend of spends) {
          if (spend === null) continue;
          const prior = uniqueSpends.get(spend.transactionId);
          if (prior !== undefined && !sameSpend(prior, spend)) {
            throw new Error(
              "State-queue Kupo history attached one transaction to competing chain points",
            );
          }
          uniqueSpends.set(spend.transactionId, spend);
        }
        if (uniqueSpends.size === 0) {
          throw new Error(
            "State-queue ordered replay cannot advance from its durable cursor",
          );
        }
        const transactions = await Promise.all(
          [...uniqueSpends.values()].map(async (spend) => {
            const ancestor = await fetchKupoAncestorPoint({
              kupoUrl,
              slot: spend.point.slot,
              fetchImpl,
            });
            const transaction = await readOgmiosBlockTransaction({
              ogmiosUrl,
              intersection: ancestor,
              blockPoint: spend.point,
              txHash: spend.transactionId,
              webSocketFactory,
            });
            return { spend, transaction };
          }),
        );
        transactions.sort(
          (left, right) =>
            left.transaction.blockPoint.blockNo -
              right.transaction.blockPoint.blockNo ||
            left.transaction.transactionIndex -
              right.transaction.transactionIndex ||
            left.transaction.txHash.localeCompare(right.transaction.txHash),
        );
        const { spend, transaction } = transactions[0]!;
        if (
          transaction.blockPoint.headerHash !== spend.point.headerHash ||
          transaction.blockPoint.slot !== spend.point.slot
        ) {
          throw new Error("Kupo/Ogmios state-queue chain points disagree");
        }
        const spentInputOutRefs = (transaction.spentInputs ?? []).map(
          ({ txHash, outputIndex }) => `${txHash}#${outputIndex.toString()}`,
        );
        const historicalOutputs = await fetchKupoTransactionQueueOutputs({
          kupoUrl,
          transactionHash: transaction.txHash,
          stateQueueAddress,
          stateQueuePolicyId,
          fetchImpl,
        });
        const correctionLockOutputs =
          await fetchKupoTransactionCorrectionLockOutputs({
            kupoUrl,
            transactionHash: transaction.txHash,
            correctionLockAddress,
            hubOraclePolicyId,
            fetchImpl,
          });
        const intermediateQueue = reconstructQueueAfterTransaction({
          previousQueue: workingQueue,
          transactionHash: transaction.txHash,
          spentInputOutRefs,
          outputs: historicalOutputs,
        });
        if (tip.blockNo < transaction.blockPoint.blockNo) {
          throw new Error(
            "Ogmios tip precedes an authenticated state-queue transaction",
          );
        }
        const observedDepth = tip.blockNo - transaction.blockPoint.blockNo + 1;
        const localChainPointId = digest({
          source: "midgard-node-local-kupmios-ordered-v1",
          blockHash: transaction.blockPoint.headerHash,
          slot: transaction.blockPoint.slot,
          blockNo: transaction.blockPoint.blockNo,
          transactionIndex: transaction.transactionIndex,
        });
        const correctionLockWitness = await deriveCorrectionLockWitnessFromRaw({
          transaction,
          transactionOutputs: correctionLockOutputs,
          stateQueuePolicyId,
          correctionLockAddress,
          hubOraclePolicyId,
          fraudProofPolicyId,
          fraudProofAddress,
          kupoUrl,
          fetchImpl,
        });
        const transitionInput = {
          deploymentIdentityDigest,
          stateQueuePolicyId,
          transactionHash: transaction.txHash,
          blockHash: transaction.blockPoint.headerHash,
          slot: transaction.blockPoint.slot.toString(),
          blockNo: transaction.blockPoint.blockNo.toString(),
          transactionIndex: transaction.transactionIndex.toString(),
          chainPointId: localChainPointId,
          finalityDepth: observedDepth.toString(),
          mintPolicyIds: transaction.mintPolicyIds,
          redeemers: transaction.redeemers.map((redeemer) => ({
            purpose: redeemer.purpose,
            index: redeemer.index.toString(),
            cborHex: redeemer.redeemer,
          })),
          spentInputOutRefs,
          referenceInputOutRefs: transaction.referenceInputs.map(
            ({ txHash, outputIndex }) => `${txHash}#${outputIndex.toString()}`,
          ),
          correctionLockWitness,
          previousQueue: workingQueue,
          nextQueue: intermediateQueue,
        } as const;
        const checkpoint =
          deriveStateQueueAuthenticatedReplayCheckpoint(transitionInput);
        if (checkpoint === null) {
          throw new Error(
            "State-queue transaction failed exact authenticated checkpoint derivation",
          );
        }
        observations.push(checkpoint);
        workingQueue = intermediateQueue;
      }
      throw new Error(
        "State-queue ordered replay exceeded its 1000-transition safety bound",
      );
    },
  };
};
