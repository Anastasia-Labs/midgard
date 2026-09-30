import {
  computeFraudProofRawL1PointId,
  LocalKupmiosExactPointNotCanonicalError,
} from "@al-ft/midgard-fault-proofs";

import { type WatcherStateQueueRecovery } from "./authenticated-state-queue-observation.parse-persisted-header.js";
import { parsePersistedObservation } from "./authenticated-state-queue-observation.parse-persisted-observation.js";
import { restorePersistedObservationChain } from "./authenticated-state-queue-observation.restore-persisted-observation-chain.js";
import { resolveRetainedHeaderAtBoundary } from "./authenticated-state-queue-observation.restore-trusted-persisted-observation-chain.js";
import { snapshotObservationAtBoundary } from "./authenticated-state-queue-observation.snapshot-observation-at-boundary.js";

const restoreLongestPersistedObservationChain = async (
  input: Parameters<typeof restorePersistedObservationChain>[0],
): Promise<WatcherStateQueueRecovery> => {
  if (
    input.persistedObservations.length === 0 ||
    input.persistedObservations.length > input.maximumObservations
  ) {
    return await restorePersistedObservationChain(input);
  }
  try {
    return await restorePersistedObservationChain(input);
  } catch (fullError) {
    if (!(fullError instanceof LocalKupmiosExactPointNotCanonicalError)) {
      throw fullError;
    }
    for (
      let retainedCount = input.persistedObservations.length - 1;
      retainedCount > 0;
      retainedCount -= 1
    ) {
      const candidate = parsePersistedObservation(
        input.persistedObservations[retainedCount - 1],
      );
      if (candidate === null) {
        throw new Error(
          "state-queue rollback prefix contains a non-canonical observation",
        );
      }
      const candidatePoint = Object.freeze({
        blockHash: candidate.nativePoint.blockHash,
        blockNo: candidate.nativePoint.blockNo,
        slot: candidate.nativePoint.slot,
        pointId: computeFraudProofRawL1PointId(candidate.nativePoint),
      });
      try {
        await input.readers.readBlock(candidatePoint);
      } catch (error) {
        if (error instanceof LocalKupmiosExactPointNotCanonicalError) {
          continue;
        }
        throw error;
      }
      const restored = await restorePersistedObservationChain({
        ...input,
        persistedObservations: input.persistedObservations.slice(
          0,
          retainedCount,
        ),
      });
      return Object.freeze({
        ...restored,
        discardedObservationCount:
          input.persistedObservations.length - retainedCount,
      });
    }
    throw fullError;
  }
};

/** Pure replay test seam; unlike the production source, it grants no authority. */
export const unsafeRestorePersistedWatcherStateQueueObservationForTest =
  restorePersistedObservationChain;

/** Pure rollback-prefix selection test seam; it grants no source authority. */
export const unsafeRestoreLongestWatcherStateQueuePrefixForTest =
  restoreLongestPersistedObservationChain;

/** Pure bootstrap test seam; it grants no source admission authority. */
export const unsafeSnapshotWatcherStateQueueAtBoundaryForTest =
  snapshotObservationAtBoundary;

/** Pure retained-header test seam; it grants no source admission authority. */
export const unsafeResolveRetainedWatcherStateQueueHeaderForTest =
  resolveRetainedHeaderAtBoundary;
