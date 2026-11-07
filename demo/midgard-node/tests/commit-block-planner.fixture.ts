import {
  Columns as TxColumns,
  EntryWithTimeStamp,
} from "../src/database/utils/tx.js";
import { type CommitSchedulerStateQueueEvidence } from "../src/workers/utils/commit-block-planner.js";

export const mkTxEntry = (
  seed: number,
  timestamp = new Date("2026-01-01T00:00:00.000Z"),
): EntryWithTimeStamp => ({
  [TxColumns.TX_ID]: Buffer.from(seed.toString(16).padStart(64, "0"), "hex"),
  [TxColumns.TX]: Buffer.from(`tx-${seed}`),
  [TxColumns.TIMESTAMPTZ]: timestamp,
});

export const fitInsideSchedulerWindow = ({
  resolvedEndTimeMs,
  maximumEndTimeMs,
}: {
  readonly resolvedEndTimeMs: number;
  readonly maximumEndTimeMs: number;
}) =>
  ({
    status: "fits",
    alignedCandidateEndTime: resolvedEndTimeMs,
    minimumMonotonicEndTime: resolvedEndTimeMs - 2_000,
    minimumCurrentTimeEndTime: resolvedEndTimeMs - 1_000,
    resolvedEndTime: resolvedEndTimeMs,
    maximumEndTimeMs,
  }) as const;

export const fitExceedingSchedulerWindow = ({
  resolvedEndTimeMs,
  maximumEndTimeMs,
}: {
  readonly resolvedEndTimeMs: number;
  readonly maximumEndTimeMs: number;
}) =>
  ({
    status: "exceeds_cap",
    alignedCandidateEndTime: resolvedEndTimeMs - 2_000,
    minimumMonotonicEndTime: resolvedEndTimeMs - 1_000,
    minimumCurrentTimeEndTime: resolvedEndTimeMs,
    resolvedEndTime: resolvedEndTimeMs,
    maximumEndTimeMs,
    reason: `resolved_end_time_ms=${resolvedEndTimeMs.toString()},maximum_end_time_ms=${maximumEndTimeMs.toString()}`,
  }) as const;

export const submitSlotSnapshot = {
  source: "test",
  currentSlot: 10,
  observedAtMs: 50_000,
  slotLengthMs: 1_000,
} as const;

export const stateQueueEvidence: CommitSchedulerStateQueueEvidence = {
  tailCommitBaseOutRef: "stateq#1",
  tailBlockEndTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
  stateQueueHasUnmergedTail: false,
};
