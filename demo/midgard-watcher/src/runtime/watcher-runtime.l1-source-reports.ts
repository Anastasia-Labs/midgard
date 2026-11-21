import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherOperationsSink } from "./operations-observability.js";

/**
 * What the decision driver reports to operations about the local L1
 * source: each decided tip records the source as consistent at that point
 * and clears `chain_rollback`, then `refresh` runs; each new follower
 * generation (a rewind or reset, heard or pulled from the rollback log)
 * raises `chain_rollback`.
 */
export const watcherL1SourceReports = ({
  operationsSink,
  sourceIdentityDigest,
  refresh,
}: {
  readonly operationsSink: Pick<
    WatcherOperationsSink,
    "recordL1Source" | "setAlert"
  >;
  readonly sourceIdentityDigest: string;
  readonly refresh: () => void;
}): Readonly<{
  onDecided: (tip: WatcherAuthenticatedStateQueueObservation) => void;
  onRewind: () => void;
}> =>
  Object.freeze({
    onDecided: (tip) => {
      const observedAtMs = BigInt(Date.now()).toString();
      operationsSink.recordL1Source({
        sourceIdentityDigest,
        sourceMode: "local_node",
        status: "consistent",
        blockHash: tip.nativePoint.blockHash,
        blockNo: tip.nativePoint.blockNo,
        slot: tip.nativePoint.slot,
        observedAtMs,
      });
      operationsSink.setAlert({
        code: "chain_rollback",
        subjectDigest: sourceIdentityDigest,
        active: false,
        observedAtMs,
      });
      refresh();
    },
    onRewind: () => {
      operationsSink.setAlert({
        code: "chain_rollback",
        subjectDigest: sourceIdentityDigest,
        active: true,
        observedAtMs: BigInt(Date.now()).toString(),
      });
    },
  });
