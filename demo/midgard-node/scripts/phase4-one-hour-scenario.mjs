#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";

import {
  PHASE4_BLOCK_TX_TARGET,
  PHASE4_MIN_DURATION_SEC,
  PHASE4_ONE_HOUR_SCENARIO,
  verifyPhase4PipelinedReportFile,
} from "./verify-phase4-pipelined-report.mjs";

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const packageRoot = path.resolve(scriptDir, "..");
const timestamp = new Date().toISOString().replace(/[-:.]/gu, "");
const reportPath = path.resolve(
  process.env.STRESS_REPORT_PATH ??
    path.join(
      packageRoot,
      "logs",
      `phase4-one-hour-${timestamp}`,
      "report.json",
    ),
);

const required = [
  "STRESS_CORPUS_PATH",
  "STRESS_CORPUS_INDEX_PATH",
  "STRESS_CORPUS_MANIFEST_PATH",
  "STRESS_CORPUS_SLICE_ID",
  "STRESS_LOAD_GENERATOR_PLACEMENT",
  "STRESS_LOADGEN_COHOSTED",
  "STRESS_CLOCK_OFFSET_MS",
  "STRESS_OBSERVABILITY_PROFILE",
  "STRESS_PHASE4_ENVIRONMENT_FINGERPRINT_PATH",
];
for (const name of required) {
  if (String(process.env[name] ?? "").trim().length === 0) {
    throw new Error(`${name} is required for the canonical Phase 4 gate`);
  }
}
if (Number(process.env.COMMIT_MAX_L2_TX_COUNT) !== PHASE4_BLOCK_TX_TARGET) {
  throw new Error(
    `COMMIT_MAX_L2_TX_COUNT must equal ${PHASE4_BLOCK_TX_TARGET.toString()}`,
  );
}
if (String(process.env.SPECULATIVE_COMMIT_BUILD).toLowerCase() !== "true") {
  throw new Error("SPECULATIVE_COMMIT_BUILD=true is required");
}
if (Number(process.env.STRESS_TARGET_ACCEPTED_TPS) < 2_500) {
  throw new Error("STRESS_TARGET_ACCEPTED_TPS must be at least 2500");
}

fs.mkdirSync(path.dirname(reportPath), { recursive: true });
const childEnv = {
  ...process.env,
  STRESS_SCENARIO_NAME: PHASE4_ONE_HOUR_SCENARIO,
  STRESS_SCENARIO_CLASS: "B",
  STRESS_FORMAL_BENCHMARK: "true",
  STRESS_MODE: "open",
  STRESS_MEASURED_SEC: PHASE4_MIN_DURATION_SEC.toString(),
  STRESS_WAIT_FOR_COMMIT: "true",
  STRESS_WAIT_FOR_MERGE: "true",
  STRESS_PHASE4_BLOCK_TX_TARGET: PHASE4_BLOCK_TX_TARGET.toString(),
  STRESS_REPORT_PATH: reportPath,
};

const exitCode = await new Promise((resolve, reject) => {
  const child = spawn(
    process.execPath,
    [path.join(scriptDir, "throughput-valid-stress.mjs")],
    {
      cwd: packageRoot,
      env: childEnv,
      stdio: "inherit",
    },
  );
  child.once("error", reject);
  child.once("exit", (code, signal) => {
    if (signal !== null) {
      reject(new Error(`Phase 4 workload terminated by ${signal}`));
      return;
    }
    resolve(code ?? 1);
  });
});

if (!fs.existsSync(reportPath)) {
  throw new Error(`Phase 4 workload produced no report at ${reportPath}`);
}
const verification = verifyPhase4PipelinedReportFile(reportPath);
const verificationPath = path.join(
  path.dirname(reportPath),
  "verification.json",
);
fs.writeFileSync(
  verificationPath,
  `${JSON.stringify(verification, null, 2)}\n`,
);
console.log(JSON.stringify(verification, null, 2));
if (exitCode !== 0 || !verification.passed) process.exitCode = 1;
