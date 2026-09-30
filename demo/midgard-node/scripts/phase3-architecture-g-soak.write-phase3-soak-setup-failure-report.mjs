import fs from "node:fs";
import os from "node:os";
import path from "node:path";

import { sha256File } from "./phase3-architecture-g-closure-lib.mjs";
import {
  evaluatePhase3ArchitectureGSoakReport,
  PHASE3_ARCHITECTURE_G_SAMPLE_INTERVAL_MS,
  PHASE3_ARCHITECTURE_G_SOAK_DURATION_SEC,
  PHASE3_ARCHITECTURE_G_SOAK_SCENARIO,
  PHASE3_ARCHITECTURE_G_SOAK_SCHEMA,
} from "./verify-phase3-architecture-g-soak-report.mjs";

export const sleep = (durationMs) =>
  new Promise((resolve) => setTimeout(resolve, Math.max(0, durationMs)));

export const requiredArg = (name) => {
  const index = process.argv.indexOf(name);
  const value = index < 0 ? undefined : process.argv[index + 1];
  if (value === undefined || value.startsWith("--")) {
    throw new Error(`missing required ${name}`);
  }
  return value;
};

export const absoluteArg = (name) => {
  const value = requiredArg(name);
  if (!path.isAbsolute(value)) throw new Error(`${name} must be absolute`);
  return path.resolve(value);
};

export const preparePhase3SoakOutputDirectory = () => {
  let requestedOutDir = null;
  let error = null;
  try {
    requestedOutDir = absoluteArg("--out-dir");
    if (fs.existsSync(requestedOutDir)) {
      throw new Error(`refusing to overwrite ${requestedOutDir}`);
    }
    fs.mkdirSync(requestedOutDir, { recursive: true, mode: 0o700 });
    return { outDir: requestedOutDir, requestedOutDir, error: null };
  } catch (outputError) {
    error = outputError;
  }

  const configuredFallback = String(
    process.env.PHASE3_SOAK_FAILURE_OUT_DIR ?? "",
  ).trim();
  const fallbackCandidate =
    configuredFallback.length > 0 && path.isAbsolute(configuredFallback)
      ? path.resolve(configuredFallback)
      : path.join(
          os.tmpdir(),
          `midgard-phase3-soak-setup-failure-${process.pid.toString()}-${Date.now().toString()}`,
        );
  let outDir = fallbackCandidate;
  if (fs.existsSync(outDir)) {
    outDir = `${fallbackCandidate}-${Date.now().toString()}`;
  }
  fs.mkdirSync(outDir, { recursive: true, mode: 0o700 });
  return { outDir, requestedOutDir, error };
};

export const requiredEnv = (name) => {
  const value = String(process.env[name] ?? "").trim();
  if (value.length === 0) throw new Error(`${name} is required`);
  return value;
};

export const resolvePhase3SoakTiming = (env = process.env) => {
  const durationOverride = env.PHASE3_SOAK_TEST_DURATION_SEC;
  const intervalOverride = env.PHASE3_SOAK_TEST_SAMPLE_INTERVAL_MS;
  const hasOverride =
    durationOverride !== undefined || intervalOverride !== undefined;
  const testOnly = env.NODE_ENV === "test" && env.PHASE3_SOAK_TEST_MODE === "1";
  if (hasOverride && !testOnly) {
    throw new Error(
      "PHASE3_SOAK_TEST_* overrides require NODE_ENV=test and PHASE3_SOAK_TEST_MODE=1",
    );
  }
  if (!testOnly) {
    return {
      durationSec: PHASE3_ARCHITECTURE_G_SOAK_DURATION_SEC,
      sampleIntervalMs: PHASE3_ARCHITECTURE_G_SAMPLE_INTERVAL_MS,
      testOnly: false,
    };
  }
  const durationSec = Number(durationOverride ?? 2);
  const sampleIntervalMs = Number(intervalOverride ?? 500);
  if (
    !Number.isSafeInteger(durationSec) ||
    durationSec <= 0 ||
    durationSec > 300 ||
    !Number.isSafeInteger(sampleIntervalMs) ||
    sampleIntervalMs <= 0 ||
    sampleIntervalMs > 5_000
  ) {
    throw new Error("invalid bounded Phase 3 test-only soak timing");
  }
  return { durationSec, sampleIntervalMs, testOnly: true };
};

export const metricValue = (text, names) => {
  for (const name of names) {
    const escaped = name.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
    const pattern = new RegExp(
      `^${escaped}(?:\\{[^}]*\\})?\\s+([^\\s]+)$`,
      "gmu",
    );
    const values = [];
    let match = pattern.exec(text);
    while (match !== null) {
      const value = Number(match[1]);
      if (!Number.isFinite(value)) {
        throw new Error(`metric ${name} contains a non-finite value`);
      }
      values.push(value);
      match = pattern.exec(text);
    }
    if (values.length > 0) return values.reduce((sum, value) => sum + value, 0);
  }
  throw new Error(`required metric is missing: ${names.join("|")}`);
};

export const readProcessSample = (pid) => {
  const readStartTicks = () => {
    const stat = fs.readFileSync(`/proc/${pid.toString()}/stat`, "utf8");
    const close = stat.lastIndexOf(")");
    const fields = stat
      .slice(close + 2)
      .trim()
      .split(/\s+/u);
    const startTicks = fields[19];
    if (!/^[0-9]+$/u.test(startTicks ?? "")) {
      throw new Error("unable to read stable node process start ticks");
    }
    return startTicks;
  };
  const startTicksBefore = readStartTicks();
  const status = fs.readFileSync(`/proc/${pid.toString()}/status`, "utf8");
  const rssMatch = /^VmRSS:\s+([0-9]+)\s+kB$/mu.exec(status);
  if (rssMatch === null) throw new Error("unable to read node process RSS");
  const startTicksAfter = readStartTicks();
  if (startTicksBefore !== startTicksAfter) {
    throw new Error("node process changed during memory identity capture");
  }
  return {
    pid,
    startTicks: startTicksAfter,
    rssBytes: Number(rssMatch[1]) * 1024,
  };
};

export const writePhase3SoakSetupFailureReport = ({
  reportPath,
  verificationPath,
  timing,
  phase,
  error,
  identity = null,
  preflight = null,
  samples = [],
}) => {
  const completedAtMs = Date.now();
  const message = error instanceof Error ? error.message : String(error);
  const configuredTiming = timing ?? {
    durationSec: null,
    sampleIntervalMs: null,
    testOnly: false,
  };
  const report = {
    schemaVersion: PHASE3_ARCHITECTURE_G_SOAK_SCHEMA,
    scenario: PHASE3_ARCHITECTURE_G_SOAK_SCENARIO,
    testOnly: configuredTiming.testOnly,
    configuredDurationSec: configuredTiming.durationSec,
    sampleIntervalMs: configuredTiming.sampleIntervalMs,
    startedAtMs: preflight?.lifecycleStartedAtMs ?? null,
    completedAtMs,
    preflight:
      preflight === null
        ? null
        : {
            startedAtMs: preflight.startedAtMs,
            completedAtMs: preflight.completedAtMs,
            durationMs: preflight.durationMs,
            lifecycleStartedAtMs: preflight.lifecycleStartedAtMs,
            initialReadiness: preflight.initialReadiness ?? null,
            nodePreLifecycleRevalidation:
              preflight.nodePreLifecycleRevalidation ?? null,
          },
    identity,
    sourceAtCompletion: null,
    observation: {
      workloadSpawnedAtMs: null,
      workloadExitedAtMs: null,
      firstSampleAtMs: samples[0]?.observedAtMs ?? null,
      lastSampleAtMs: samples.at(-1)?.observedAtMs ?? null,
    },
    workload: null,
    termination: {
      completed: false,
      reason: "setup_failure",
      phase,
      workloadExitCode: null,
      workloadSignal: null,
      earlyExit: false,
      error: message,
    },
    samples,
  };
  let evaluation;
  try {
    evaluation = evaluatePhase3ArchitectureGSoakReport(report, {
      allowTestOnlyDuration: configuredTiming.testOnly,
    });
  } catch (evaluationError) {
    evaluation = {
      passed: false,
      reasons: [
        `setup failure evaluation failed closed: ${evaluationError instanceof Error ? evaluationError.message : String(evaluationError)}`,
      ],
    };
  }
  fs.writeFileSync(reportPath, `${JSON.stringify(report, null, 2)}\n`, {
    flag: "wx",
    mode: 0o600,
  });
  const verification = {
    schemaVersion: "midgard-phase3-soak-setup-failure-verification-v1",
    ...evaluation,
    passed: false,
    phase,
    reportPath,
    reportSha256: sha256File(reportPath),
  };
  fs.writeFileSync(
    verificationPath,
    `${JSON.stringify(verification, null, 2)}\n`,
    { flag: "wx", mode: 0o600 },
  );
  return report;
};
