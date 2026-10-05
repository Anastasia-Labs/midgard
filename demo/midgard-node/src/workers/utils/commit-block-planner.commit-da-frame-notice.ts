import {
  type DaPayloadFramePressureStage,
  daPayloadFramePressureStage,
} from "@al-ft/midgard-core/da-payload-sizing";
import { Effect } from "effect";

import type { NothingToCommitOutput } from "./commit-block-header.js";
import type { CommitDaFrameMeasurement } from "./commit-block-planner.commit-scheduler-evidence-key.js";

/** Measured diagnostics for a built candidate; all bytes use the header upper bound. */
export type CommitDaFramePressure = {
  readonly candidateInnerBytesUpperBound: number;
  readonly initialCandidateInnerBytesUpperBound: number;
  readonly baseEmptyBlockInnerBytesUpperBound: number;
  /** Known only after ordinary transactions have been removed. Withdrawals
   * may shrink the base ledger, so its empty-block estimate is not this floor. */
  readonly requiredWorkInnerBytesUpperBound: number | null;
  readonly effectiveInnerLimit: number;
  readonly candidateStagePercent: DaPayloadFramePressureStage;
  readonly initialCandidateStagePercent: DaPayloadFramePressureStage;
  readonly baseLedgerStagePercent: DaPayloadFramePressureStage;
  readonly requiredWorkStagePercent: DaPayloadFramePressureStage | null;
  readonly acceptedTxCount: number;
  readonly passes: number;
};

export type CommitDaFramePressureSnapshot = CommitDaFramePressure & {
  readonly observedAtMs: number;
};

/**
 * Posted by the commit worker once its DA frame step-down has decided, ahead
 * of its output. When a block's conservative size estimate overflows with no
 * transaction left to drop, the block is refused on every tick (by the
 * pre-submit check, or, when only transactions were pending, by committing
 * nothing while they stay in the mempool), so the parent raises a liveness
 * reason. Exact pre-submit admission clears that reason immediately; the
 * estimate can exceed the frame while the final header fits. A tick with no
 * transaction or user event pending posts `COMMIT_DA_FRAME_IDLE_NOTICE`
 * (`nothingToCommitWithNoWork`), since no block is refused. Every other tick
 * that returns before the step-down posts nothing and leaves the reason as it
 * is.
 */
export type CommitDaFrameNotice = {
  readonly type: "CommitDaFrameNotice";
  /** `fits`: the frame admits this tick's measured block, or there is no
   * work to build one.
   * `events_overflow`: the block's events alone overflow a frame whose empty
   * block fits. `ledger_ceiling`: the base ledger's empty block alone
   * exceeds the frame, and this block's withdrawals do not bring it under. */
  readonly status:
    | "fits"
    | "events_overflow"
    | "ledger_ceiling"
    | "exact_check_required"
    | "incomplete";
  /** Goes only to the reason's raise warning log; /readyz carries the
   * reason, its source and age, not this. */
  readonly detail: string;
  /** Omission preserves measured diagnostics on an exact admission acknowledgement.
   * Null explicitly clears diagnostics when there is no candidate to measure. */
  readonly pressure?: CommitDaFramePressure | null;
};

export const COMMIT_DA_FRAME_FITS_NOTICE: CommitDaFrameNotice = {
  type: "CommitDaFrameNotice",
  status: "fits",
  detail: "",
};

/** An idle tick clears diagnostics as well as its DA liveness reason. */
export const COMMIT_DA_FRAME_IDLE_NOTICE: CommitDaFrameNotice = {
  ...COMMIT_DA_FRAME_FITS_NOTICE,
  pressure: null,
};

/** The notice for a step-down's `outcome`; none when the block was left
 * unmeasured, which the pre-submit check decides as before. */
export const commitDaFrameNoticeForOutcome = ({
  outcome,
  passes,
  baseEmptyBlockInnerBytes,
  maxInnerBytes,
  measurement,
  initialCandidateInnerBytesUpperBound,
}: {
  readonly outcome:
    | "fits"
    | "no_transactions_to_drop"
    | "exact_check_required"
    | "incomplete"
    | "unmeasured";
  readonly passes: number;
  readonly baseEmptyBlockInnerBytes: number;
  readonly maxInnerBytes: number;
  readonly measurement?: Pick<
    CommitDaFrameMeasurement,
    "innerBytesUpperBound" | "acceptedTxCount" | "rejectedTxIds"
  >;
  readonly initialCandidateInnerBytesUpperBound?: number;
}): CommitDaFrameNotice | undefined => {
  if (outcome === "unmeasured") return undefined;
  if (outcome === "fits" && measurement === undefined)
    return COMMIT_DA_FRAME_FITS_NOTICE;
  const pressure: CommitDaFramePressure | undefined =
    measurement === undefined
      ? undefined
      : {
          candidateInnerBytesUpperBound: measurement.innerBytesUpperBound,
          initialCandidateInnerBytesUpperBound:
            initialCandidateInnerBytesUpperBound ??
            measurement.innerBytesUpperBound,
          baseEmptyBlockInnerBytesUpperBound: baseEmptyBlockInnerBytes,
          requiredWorkInnerBytesUpperBound:
            measurement.acceptedTxCount === 0
              ? measurement.innerBytesUpperBound
              : null,
          effectiveInnerLimit: maxInnerBytes,
          candidateStagePercent: daPayloadFramePressureStage(
            measurement.innerBytesUpperBound,
            maxInnerBytes,
          ),
          initialCandidateStagePercent: daPayloadFramePressureStage(
            initialCandidateInnerBytesUpperBound ??
              measurement.innerBytesUpperBound,
            maxInnerBytes,
          ),
          baseLedgerStagePercent: daPayloadFramePressureStage(
            baseEmptyBlockInnerBytes,
            maxInnerBytes,
          ),
          requiredWorkStagePercent:
            measurement.acceptedTxCount === 0
              ? daPayloadFramePressureStage(
                  measurement.innerBytesUpperBound,
                  maxInnerBytes,
                )
              : null,
          acceptedTxCount: measurement.acceptedTxCount,
          passes,
        };
  return {
    type: "CommitDaFrameNotice",
    pressure,
    status:
      outcome === "incomplete" || outcome === "exact_check_required"
        ? outcome
        : outcome === "fits"
          ? "fits"
          : baseEmptyBlockInnerBytes > maxInnerBytes
            ? "ledger_ceiling"
            : "events_overflow",
    detail:
      outcome === "fits"
        ? ""
        : `passes=${passes.toString()} base_empty_block_inner_bytes=${baseEmptyBlockInnerBytes.toString()} effective_inner_limit=${maxInnerBytes.toString()}`,
  };
};

/**
 * The output of a tick with no transaction or user event pending. No block is
 * refused then, so it posts `COMMIT_DA_FRAME_IDLE_NOTICE` first, clearing a
 * standing DA frame reason and the frame diagnostics. Only these ticks do: the deposit-only path's
 * "nothing to commit" also ends a step-down that dropped every transaction
 * from an over-frame ledger, and must leave its reason standing.
 */
export const nothingToCommitWithNoWork = (
  notify: ((notice: CommitDaFrameNotice) => Effect.Effect<void>) | undefined,
): Effect.Effect<NothingToCommitOutput> =>
  Effect.as(
    notify === undefined ? Effect.void : notify(COMMIT_DA_FRAME_IDLE_NOTICE),
    { type: "NothingToCommitOutput" } as const,
  );
