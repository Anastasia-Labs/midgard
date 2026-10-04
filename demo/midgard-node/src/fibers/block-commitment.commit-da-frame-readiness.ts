import { Effect, Metric, Ref } from "effect";

import type { Globals } from "../services/globals.js";
import {
  clearLivenessIncident,
  COMMIT_DA_FRAME_EVENTS_OVERFLOW,
  COMMIT_DA_FRAME_LEDGER_CEILING,
  COMMIT_DA_FRAME_SOURCE,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import { type CommitDaFrameNotice } from "../workers/utils/commit-block-planner.commit-da-frame-notice.js";

const commitDaFrameLimitGauge = Metric.gauge(
  "midgard_commit_da_frame_effective_inner_limit_bytes",
);
const commitDaFrameBytesGauge = Metric.gauge(
  "midgard_commit_da_frame_inner_bytes_upper_bound",
);
export const commitDaFrameStageGauge = Metric.gauge(
  "midgard_commit_da_frame_stage_percent",
);
const commitDaFrameObservedAtGauge = Metric.gauge(
  "midgard_commit_da_frame_observed_at_ms",
);
const commitDaFramePassesGauge = Metric.gauge(
  "midgard_commit_da_frame_step_down_passes",
);
// Zero bytes/stage is unavailable when this bounded kind's measured gauge is zero.
const commitDaFrameMeasuredGauge = Metric.gauge(
  "midgard_commit_da_frame_measured",
);

/**
 * Projects a commit worker's DA frame notice onto readiness. A block the frame
 * cannot admit raises a reason under `COMMIT_DA_FRAME_SOURCE`; a notice that
 * the frame admits this tick's block clears it. The worker posts that notice
 * too for a tick with no transaction or user event pending, since no block is
 * refused then. The source holds no fiber, so the commit loop keeps ticking and the tick
 * after the condition goes away clears the reason.
 */
export const applyCommitDaFrameNotice = (
  globals: Pick<Globals, "LIVENESS_REASONS" | "COMMIT_DA_FRAME_PRESSURE">,
  notice: CommitDaFrameNotice,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    const pressure = notice.pressure;
    if (pressure !== undefined) {
      const prior = yield* Ref.get(globals.COMMIT_DA_FRAME_PRESSURE);
      const observedAtMs = Date.now();
      yield* Ref.set(
        globals.COMMIT_DA_FRAME_PRESSURE,
        pressure === null ? null : { ...pressure, observedAtMs },
      );
      yield* Metric.set(
        commitDaFrameLimitGauge,
        pressure?.effectiveInnerLimit ?? 0,
      );
      yield* Metric.set(
        commitDaFrameObservedAtGauge,
        pressure === null ? 0 : observedAtMs,
      );
      yield* Metric.set(commitDaFramePassesGauge, pressure?.passes ?? 0);
      for (const [kind, bytes, stage] of [
        [
          "candidate",
          pressure?.candidateInnerBytesUpperBound,
          pressure?.candidateStagePercent,
        ],
        [
          "initial_candidate",
          pressure?.initialCandidateInnerBytesUpperBound,
          pressure?.initialCandidateStagePercent,
        ],
        [
          "base_ledger",
          pressure?.baseEmptyBlockInnerBytesUpperBound,
          pressure?.baseLedgerStagePercent,
        ],
        [
          "required_work",
          pressure?.requiredWorkInnerBytesUpperBound,
          pressure?.requiredWorkStagePercent,
        ],
      ] as const) {
        yield* Metric.set(
          Metric.tagged(commitDaFrameBytesGauge, "kind", kind),
          bytes ?? 0,
        );
        yield* Metric.set(
          Metric.tagged(commitDaFrameStageGauge, "kind", kind),
          stage ?? 0,
        );
        yield* Metric.set(
          Metric.tagged(commitDaFrameMeasuredGauge, "kind", kind),
          bytes == null ? 0 : 1,
        );
      }
      if (
        pressure !== null &&
        (pressure.candidateStagePercent > 0 ||
          pressure.initialCandidateStagePercent > 0 ||
          pressure.baseLedgerStagePercent > 0 ||
          (pressure.requiredWorkStagePercent ?? 0) > 0) &&
        (prior?.candidateStagePercent !== pressure.candidateStagePercent ||
          prior?.initialCandidateStagePercent !==
            pressure.initialCandidateStagePercent ||
          prior?.baseLedgerStagePercent !== pressure.baseLedgerStagePercent ||
          prior?.requiredWorkStagePercent !== pressure.requiredWorkStagePercent)
      ) {
        yield* Effect.logWarning(
          `commit_da_frame_pressure candidate_stage_percent=${pressure.candidateStagePercent.toString()} initial_candidate_stage_percent=${pressure.initialCandidateStagePercent.toString()} base_ledger_stage_percent=${pressure.baseLedgerStagePercent.toString()} required_work_stage_percent=${pressure.requiredWorkStagePercent?.toString() ?? "unknown"} candidate_inner_bytes_upper_bound=${pressure.candidateInnerBytesUpperBound.toString()} base_empty_block_inner_bytes_upper_bound=${pressure.baseEmptyBlockInnerBytesUpperBound.toString()} required_work_inner_bytes_upper_bound=${pressure.requiredWorkInnerBytesUpperBound?.toString() ?? "unknown"} effective_inner_limit=${pressure.effectiveInnerLimit.toString()} passes=${pressure.passes.toString()}`,
        );
      }
    }
    // Provisional upper-bound overflow and invalidated search cannot clear an
    // existing refusal. An actual failed production cycle owns its worker reason.
    if (
      notice.status === "exact_check_required" ||
      notice.status === "incomplete"
    )
      return;
    yield* notice.status === "fits"
      ? clearLivenessIncident(globals, COMMIT_DA_FRAME_SOURCE)
      : raiseLivenessIncident(
          globals,
          COMMIT_DA_FRAME_SOURCE,
          notice.status === "ledger_ceiling"
            ? COMMIT_DA_FRAME_LEDGER_CEILING
            : COMMIT_DA_FRAME_EVENTS_OVERFLOW,
          notice.detail,
        );
  });
