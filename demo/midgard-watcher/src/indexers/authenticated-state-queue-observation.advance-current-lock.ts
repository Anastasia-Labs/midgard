import * as SDK from "@al-ft/midgard-sdk";

import { sameLockDatum } from "./authenticated-state-queue-observation.correction-lock-witness.js";
import { type WatcherCorrectionLockObservation } from "./authenticated-state-queue-observation.parse-persisted-header.js";

export const advanceCurrentLock = ({
  current,
  witness,
  transactionHash,
  point,
  chainPointId,
  finalityDepth,
}: {
  current: WatcherCorrectionLockObservation | null;
  witness: SDK.StateQueueCorrectionLockWitness;
  transactionHash: string;
  point: Readonly<{ blockHash: string; blockNo: string; slot: string }>;
  chainPointId: string;
  finalityDepth: string;
}): WatcherCorrectionLockObservation | null => {
  if (witness.kind === "none") return current;
  if (witness.kind === "genesis") {
    if (current !== null)
      throw new Error("CorrectionLock genesis duplicated the singleton");
    return Object.freeze({
      outRef: witness.producedOutRef,
      datum: witness.nextDatum,
      observedTransactionHash: transactionHash,
      observedBlockHash: point.blockHash,
      observedSlot: point.slot,
      observedBlockNo: point.blockNo,
      observedChainPointId: chainPointId,
      finalityDepth,
    });
  }
  if (witness.kind === "idle_reference") {
    if (
      current === null ||
      current.outRef !== witness.referenceOutRef ||
      !sameLockDatum(current.datum, witness.datum)
    ) {
      throw new Error(
        "CorrectionLock reference differs from the authenticated cursor",
      );
    }
    return current;
  }
  if (
    current === null ||
    current.outRef !== witness.consumedOutRef ||
    !sameLockDatum(current.datum, witness.previousDatum)
  ) {
    throw new Error(
      "CorrectionLock spend differs from the authenticated cursor",
    );
  }
  if (witness.kind === "deinit") return null;
  return Object.freeze({
    outRef: witness.continuedOutRef,
    datum: witness.nextDatum,
    observedTransactionHash: transactionHash,
    observedBlockHash: point.blockHash,
    observedSlot: point.slot,
    observedBlockNo: point.blockNo,
    observedChainPointId: chainPointId,
    finalityDepth,
  });
};
