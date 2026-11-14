import { type FraudProofRawL1Point } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";

import {
  refusalError,
  required,
  WatcherFaultProofL1RefusedError,
} from "../l1-follower/fault-proof-l1-source.chain.js";
import { watcherRawTransactionCbor } from "./commitment-source.js";
import { type WatcherAvailabilityL1 } from "./follower-reads.js";

/**
 * How a workflow release check reads a header's node chain once the
 * follower has pruned part of it. The follower keeps every fact only to its
 * retained depth, but a release settles only once its terminal step is past
 * automatic rollback recovery, which is deeper, and each check walks from
 * the Open's node output. Two retained facts stand in for pruned rows: the
 * header's queue history, kept while the header is queued, and the block
 * holding a terminal step the journal recorded, kept by its pin.
 */

/** True for a refusal that only says the follower no longer holds a fact. */
export const beyondRetention = (cause: unknown): boolean =>
  cause instanceof WatcherFaultProofL1RefusedError &&
  cause.reason === "beyond_retention";

/**
 * The transaction that spent `outRef`, a pruned output holding the node of
 * the header whose queue history is `unit`, from that history at `point`.
 * The history records every transaction that spent or created the header's
 * node, so it holds the spend whenever it holds the header at all. Each
 * candidate is checked against its own bytes. Throws a `beyond_retention`
 * refusal when the history is not retained or holds no spend: the spend may
 * exist, so nothing is concluded from its absence.
 */
export const queueHistorySpend = async (
  reads: WatcherAvailabilityL1["reads"],
  unit: string,
  outRef: string,
  point: FraudProofRawL1Point,
): Promise<Readonly<{ txHash: string; point: FraudProofRawL1Point }>> => {
  const unretained = (why: string) =>
    refusalError(
      "beyond_retention",
      `the spend of ${outRef} may have been pruned, and ${why}`,
    );
  const history = await reads.unitHistoryAtPoint(unit, point);
  if (
    history.kind === "refused" &&
    (history.reason === "beyond_retention" ||
      history.reason === "unit_not_projected")
  )
    throw unretained(`the queue history of ${unit} is not retained`);
  for (const { txHash, inclusionPoint } of required(history).transactions) {
    const { transaction } = required(
      await reads.rawTransaction(txHash, inclusionPoint),
    );
    if (
      SDK.transactionConsumesOutRef({
        transactionCbor: watcherRawTransactionCbor(transaction),
        transactionId: txHash,
        outRef,
      })
    )
      return { txHash, point: inclusionPoint };
  }
  throw unretained(`the queue history of ${unit} holds no spend of it`);
};

/** The terminal step a released workflow row recorded. */
export type WatcherRecordedWorkflowRelease = Readonly<{
  reason: SDK.DaAvailabilityWorkflowRelease["reason"];
  txHash: string;
  spendPoint: string;
}>;

/**
 * The recorded release re-read from the block it was verified in, when the
 * walk from the Open can no longer be read. A block fixes its transactions,
 * and a canonical terminal step fixes, through the outputs it spends, the
 * whole node chain back to the Open, so the release stands while that block
 * is canonical; its depth is counted to `boundaryBlockNo`. Undefined, which
 * revives the row, once that block left the chain or is shallower than
 * `minimumConfirmationDepth`; a `beyond_retention` refusal when the store no
 * longer holds it.
 */
export const recordedWorkflowRelease = async (
  l1: WatcherAvailabilityL1,
  recorded: WatcherRecordedWorkflowRelease,
  point: FraudProofRawL1Point,
  boundaryBlockNo: number,
  minimumConfirmationDepth: number,
): Promise<SDK.DaAvailabilityWorkflowRelease | undefined> => {
  const [slot, blockHash] = recorded.spendPoint.split(":");
  const status = await l1.store.pointStatus({
    slot: Number(slot),
    hash: Buffer.from(blockHash!, "hex"),
  });
  if (status.kind === "point_not_canonical") return undefined;
  if (status.kind !== "canonical")
    throw refusalError(
      "beyond_retention",
      `the block of terminal transaction ${recorded.txHash} is not retained: ${status.detail}`,
    );
  if (status.height > Number(point.blockNo))
    throw new Error(
      "Availability workflow release lies above the canonical boundary",
    );
  const confirmationDepth = boundaryBlockNo - status.height;
  return confirmationDepth < minimumConfirmationDepth
    ? undefined
    : { ...recorded, confirmationDepth };
};
