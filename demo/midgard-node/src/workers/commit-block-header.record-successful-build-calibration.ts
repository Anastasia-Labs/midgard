import { Effect } from "effect";

import * as CommitBuildCalibrationDB from "../database/commitBuildCalibration.js";
import { shouldPreserveCommitMpfRoots } from "./commit-block-header.commit-explicit-block-header-program.js";
import type { WorkerOutput } from "./utils/commit-block-header.js";
import { updateCommitBuildEwma } from "./utils/commit-block-planner.js";

/** Calibration only records the successful final processing pass. */
export const recordSuccessfulBuildCalibration = (input: {
  readonly calibration: CommitBuildCalibrationDB.State | undefined;
  readonly processedTxCount: number;
  readonly measuredBuildMs: number;
  readonly alpha: number;
  readonly output: WorkerOutput;
}) =>
  Effect.gen(function* () {
    const { calibration, processedTxCount, measuredBuildMs, alpha, output } =
      input;
    if (
      calibration === undefined ||
      processedTxCount === 0 ||
      !shouldPreserveCommitMpfRoots(output)
    )
      return;
    const nextEwma = updateCommitBuildEwma({
      previousMsPerTx: calibration.msPerTxEwma,
      measuredBuildMs,
      processedTxCount,
      alpha,
    });
    const updated = yield* CommitBuildCalibrationDB.update(nextEwma);
    yield* Effect.logInfo(
      `commit_build_calibration measured_ms_per_tx=${(measuredBuildMs / processedTxCount).toString()} ewma_ms_per_tx=${updated.msPerTxEwma.toString()} sample_count=${updated.sampleCount.toString()}`,
    );
  });
