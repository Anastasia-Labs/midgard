import { Effect } from "effect";

import * as CommitBuildCalibrationDB from "../database/commitBuildCalibration.js";
import { updateCommitBuildEwma } from "./utils/commit-block-planner.js";

/** Calibration only records the successful final processing pass. */
export const recordSuccessfulBuildCalibration = (
  calibration: CommitBuildCalibrationDB.State,
  processedTxCount: number,
  measuredBuildMs: number,
  alpha: number,
) =>
  Effect.gen(function* () {
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
