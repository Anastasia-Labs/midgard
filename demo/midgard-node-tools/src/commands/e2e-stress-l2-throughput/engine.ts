import { spawn } from "node:child_process";
import { createWriteStream } from "node:fs";
import { join } from "node:path";

import {
  type CanonicalEngineArtifactPaths,
  type CanonicalEngineRunResult,
  type E2EL2StressConfig,
} from "./types.js";

export const canonicalEnginePaths = (
  outDir: string,
): CanonicalEngineArtifactPaths => ({
  engineReportJson: join(outDir, "engine-report.json"),
  engineEventsNdjson: join(outDir, "engine-events.ndjson"),
  submitRecordsNdjson: join(outDir, "submit-records.ndjson"),
  noopCalibrationJson: join(outDir, "noop-calibration.json"),
  stdoutLog: join(outDir, "engine.stdout.log"),
  stderrLog: join(outDir, "engine.stderr.log"),
});

export const runCanonicalEngineProcess = async ({
  config,
  paths,
  signal,
}: {
  readonly config: E2EL2StressConfig;
  readonly paths: CanonicalEngineArtifactPaths;
  readonly signal?: AbortSignal;
}): Promise<CanonicalEngineRunResult> =>
  await new Promise((resolve, reject) => {
    const child = spawn(
      process.execPath,
      ["scripts/throughput-valid-stress.mjs"],
      {
        cwd: process.cwd(),
        env: {
          ...process.env,
          STRESS_SUBMIT_ENDPOINT: config.nodeEndpoint,
          STRESS_MODE: "open",
          STRESS_OPEN_LOOP_RATE_TPS: config.targetRateTps.toString(),
          STRESS_MEASURED_SEC: (config.openLoopDurationMs / 1000).toString(),
          STRESS_SUBMIT_CONCURRENCY: config.openLoopMaxInFlight.toString(),
          STRESS_HTTP_CONNECTIONS: config.openLoopMaxInFlight.toString(),
          STRESS_CORPUS_PATH: config.corpusPath ?? "",
          STRESS_CORPUS_SHAPE: config.corpusShape,
          STRESS_CORPUS_SLICE_ID: config.corpusSliceId,
          STRESS_MAX_CHAINS: "auto",
          STRESS_REPORT_PATH: paths.engineReportJson,
          STRESS_ENGINE_EVENTS_PATH: paths.engineEventsNdjson,
          STRESS_SUBMIT_RECORDS_PATH: paths.submitRecordsNdjson,
          STRESS_NOOP_CALIBRATION_PATH: paths.noopCalibrationJson,
          ...(config.noOpCalibrationEndpoint === undefined
            ? {}
            : { STRESS_NOOP_ENDPOINT: config.noOpCalibrationEndpoint }),
          STRESS_REQUIRE_NOOP_CALIBRATION: config.requireNoOpCalibration
            ? "true"
            : "false",
          STRESS_CALIBRATION_DURATION_SEC: (
            config.noOpCalibrationDurationMs / 1000
          ).toString(),
          STRESS_CLIENT_SELF_CHECK: "false",
          STRESS_REQUIRE_METRIC_PRESENCE: "false",
        },
      },
    );
    const stdout = createWriteStream(paths.stdoutLog, { flags: "w" });
    const stderr = createWriteStream(paths.stderrLog, { flags: "w" });
    child.stdout.pipe(stdout);
    child.stderr.pipe(stderr);
    const abort = (): void => {
      child.kill("SIGTERM");
      setTimeout(() => child.kill("SIGKILL"), 5_000).unref();
    };
    signal?.addEventListener("abort", abort, { once: true });
    child.once("error", reject);
    child.once("close", (exitCode, exitSignal) => {
      signal?.removeEventListener("abort", abort);
      stdout.end();
      stderr.end();
      resolve({
        exitCode: exitCode ?? 1,
        signal: exitSignal,
      });
    });
  });
