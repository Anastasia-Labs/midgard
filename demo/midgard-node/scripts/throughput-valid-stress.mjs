#!/usr/bin/env node

import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { createHash } from "node:crypto";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import {
  PerformanceObserver,
  monitorEventLoopDelay,
  performance,
} from "node:perf_hooks";
import { fileURLToPath } from "node:url";
import { CML } from "@lucid-evolution/lucid";
import { Pool } from "undici";
import {
  BENCHMARK_WINDOWS_MS,
  acceptedStatuses,
  classifyLikelyBottleneckWithEvidence,
  counterDelta,
  createPhaseRecorder,
  gaugeSlopePerSec,
  isDrainComplete,
  rateBetweenCounters,
  summarizeCounterWindow,
  summarizeHistogramDelta,
  summarizeL1Observation,
  summarizeLatency,
  deriveCalibratedClientCapacity,
  summarizeOpenLoopCheckpointProgress,
  summarizePhase1StageAWindowGate,
  summarizePhase1StarvationGate,
  summarizeSubmitSuccessStatuses,
  summarizeRollingRates,
  terminalStatuses,
} from "./throughput-benchmark-utils.mjs";
import {
  buildNativeSignedOneToOneWithMinFee as buildNativeSignedOneToOne,
  buildNativeSignedSplit,
  decodeCoin,
  makeWalletsFromEnv,
  outputHasMultiAssets,
  parseEnv,
} from "./native-tx-workload-utils.mjs";
import {
  corpusRowsForEntries,
  defaultCorpusIndexPath,
  defaultCorpusManifestPath,
  loadCorpusIndex,
  loadCorpusManifest,
  openStreamingCorpusReader,
  selectCorpusIndexEntries,
  validateCorpusSlice,
  verifyCorpusArtifactIdentity,
} from "./throughput-valid-stress-corpus.mjs";
import { consumePhase3SoakCorpusPreflight } from "./phase3-architecture-g-soak-preflight.mjs";
import { consumePhase3LoadGeneratorIsolation } from "./phase3-architecture-g-load-generator-isolation.mjs";
import {
  loadPhase1FormalBindingSync,
  PHASE1_FORMAL_SCENARIO,
  sha256FileSync,
  validatePhase1BindingEnvironment,
  validatePhase1FormalCorpus,
  verifyPhase1LivePreflight,
} from "./phase1-formal-identity.mjs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const pkgRoot = path.resolve(__dirname, "..");

const envPath = process.env.STRESS_ENV_FILE ?? path.join(pkgRoot, ".env");
const loadedStressEnv = fs.existsSync(envPath) ? parseEnv(envPath) : {};
const stressEnv = { ...loadedStressEnv, ...process.env };
const envValue = (name, fallback = undefined) => stressEnv[name] ?? fallback;
const parsePositiveIntegerSetting = (name, fallback, sentinel = null) => {
  const raw = String(envValue(name, fallback)).trim();
  if (sentinel !== null && raw === sentinel) {
    return { raw, value: Number(fallback), sentinel: true };
  }
  const value = Number.parseInt(raw, 10);
  if (!Number.isFinite(value) || value <= 0) {
    throw new Error(`${name} must be a positive integer`);
  }
  return { raw, value, sentinel: false };
};
const parsePositiveFloatSetting = (name, fallback) => {
  const value = Number.parseFloat(String(envValue(name, fallback)).trim());
  if (!Number.isFinite(value) || value <= 0) {
    throw new Error(`${name} must be a positive number`);
  }
  return value;
};
const submitEndpoint = envValue(
  "STRESS_SUBMIT_ENDPOINT",
  "http://127.0.0.1:3000",
);
const metricsEndpoint = envValue(
  "STRESS_METRICS_ENDPOINT",
  "http://127.0.0.1:9464/metrics",
);
const corpusPath = envValue("STRESS_CORPUS_PATH", null);
const corpusIndexPath =
  envValue("STRESS_CORPUS_INDEX_PATH", null) ??
  (corpusPath === null ? null : defaultCorpusIndexPath(corpusPath));
const corpusManifestPath =
  envValue("STRESS_CORPUS_MANIFEST_PATH", null) ??
  (corpusPath === null ? null : defaultCorpusManifestPath(corpusPath));
const corpusShape = envValue("STRESS_CORPUS_SHAPE", "mixed");
const corpusSliceId = envValue("STRESS_CORPUS_SLICE_ID", null);
const corpusReadAheadRows = Number.parseInt(
  envValue("STRESS_CORPUS_READAHEAD_ROWS", "50"),
  10,
);
const corpusPreflightRequired =
  String(envValue("STRESS_CORPUS_PREFLIGHT_REQUIRED", "false"))
    .trim()
    .toLowerCase() === "true";
const corpusPreflightPath = envValue("STRESS_CORPUS_PREFLIGHT_PATH", null);
const corpusPreflightSha256 = envValue("STRESS_CORPUS_PREFLIGHT_SHA256", null);
const corpusPreflightSourceIdentitySha256 = envValue(
  "STRESS_CORPUS_PREFLIGHT_SOURCE_IDENTITY_SHA256",
  null,
);
const corpusPreflightPhase1BindingSha256 = envValue(
  "STRESS_CORPUS_PREFLIGHT_PHASE1_BINDING_SHA256",
  null,
);
const loadGeneratorIsolationRequired =
  String(envValue("STRESS_LOAD_GENERATOR_ISOLATION_REQUIRED", "false"))
    .trim()
    .toLowerCase() === "true";
const loadGeneratorIsolationPath = envValue(
  "STRESS_LOAD_GENERATOR_ISOLATION_PATH",
  null,
);
const loadGeneratorIsolationSha256 = envValue(
  "STRESS_LOAD_GENERATOR_ISOLATION_SHA256",
  null,
);
const requireNoOpCalibration =
  String(envValue("STRESS_REQUIRE_NOOP_CALIBRATION", "false"))
    .trim()
    .toLowerCase() === "true";
const noOpEndpointValue = String(envValue("STRESS_NOOP_ENDPOINT", "")).trim();
const noOpEndpoint = noOpEndpointValue.length === 0 ? null : noOpEndpointValue;
const calibrationHeadroomMultiplier = parsePositiveFloatSetting(
  "STRESS_CALIBRATION_HEADROOM_MULTIPLIER",
  "2",
);
const calibrationDurationSec = parsePositiveFloatSetting(
  "STRESS_CALIBRATION_DURATION_SEC",
  "5",
);
const nodeSaturationMinRatio = parsePositiveFloatSetting(
  "STRESS_NODE_SATURATION_MIN_RATIO",
  "1.2",
);
const chainLength = Number.parseInt(envValue("STRESS_CHAIN_LENGTH", "500"), 10);
const maxChainsSetting = parsePositiveIntegerSetting(
  "STRESS_MAX_CHAINS",
  "8",
  "auto",
);
let maxChains = maxChainsSetting.sentinel ? null : maxChainsSetting.value;
const utxosPerWallet = Number.parseInt(
  envValue("STRESS_UTXOS_PER_WALLET", "3"),
  10,
);
const minLovelace = BigInt(envValue("STRESS_MIN_LOVELACE", "0"));
const fanoutEnabled =
  String(envValue("STRESS_FANOUT_ENABLED", "true")).trim().toLowerCase() !==
  "false";
const fanoutMaxOutputsPerTx = Number.parseInt(
  envValue("STRESS_FANOUT_MAX_OUTPUTS_PER_TX", "256"),
  10,
);
const fanoutOutputLovelace =
  envValue("STRESS_FANOUT_OUTPUT_LOVELACE") === undefined
    ? null
    : BigInt(envValue("STRESS_FANOUT_OUTPUT_LOVELACE"));
const fanoutStatusTimeoutMs = Number.parseInt(
  envValue("STRESS_FANOUT_STATUS_TIMEOUT_MS", "30000"),
  10,
);
const retry503 = Number.parseInt(envValue("STRESS_RETRY_503", "3"), 10);
const measuredRetry503 = Number.parseInt(
  envValue("STRESS_MEASURED_RETRY_503", "0"),
  10,
);
const retryDelayMs = Number.parseInt(
  envValue("STRESS_RETRY_DELAY_MS", "25"),
  10,
);
const metricsPollMs = Number.parseInt(
  envValue("STRESS_METRICS_POLL_MS", "1000"),
  10,
);
const observeAfterSubmitSec = Number.parseInt(
  envValue("STRESS_OBSERVE_AFTER_SUBMIT_SEC", "15"),
  10,
);
const targetAcceptedTps = Number.parseFloat(
  envValue("STRESS_TARGET_ACCEPTED_TPS", "600"),
);
const requireFreshChains =
  String(envValue("STRESS_REQUIRE_FRESH_CHAINS", "true"))
    .trim()
    .toLowerCase() !== "false";
const txStatusRetries = Number.parseInt(
  envValue("STRESS_TX_STATUS_RETRIES", "5"),
  10,
);
const txStatusRetryDelayMs = Number.parseInt(
  envValue("STRESS_TX_STATUS_RETRY_DELAY_MS", "50"),
  10,
);
const benchmarkMode = String(envValue("STRESS_MODE", "closed"))
  .trim()
  .toLowerCase();
const scenarioClass = String(envValue("STRESS_SCENARIO_CLASS", "A"))
  .trim()
  .toUpperCase();
const scenarioName = String(envValue("STRESS_SCENARIO_NAME", "custom")).trim();
const formalBenchmark =
  String(envValue("STRESS_FORMAL_BENCHMARK", "false")).trim().toLowerCase() ===
  "true";
const phase1FormalBinding =
  formalBenchmark && scenarioName === PHASE1_FORMAL_SCENARIO
    ? (() => {
        const binding = loadPhase1FormalBindingSync(
          envValue("STRESS_PHASE1_BINDING_PATH"),
        );
        return validatePhase1BindingEnvironment({
          binding,
          env: stressEnv,
          scenarioId: sha256FileSync(
            path.join(__dirname, "benchmark-scenario.mjs"),
          ),
          engineId: sha256FileSync(__filename),
        });
      })()
    : null;
const loadGeneratorPlacement = String(
  envValue("STRESS_LOAD_GENERATOR_PLACEMENT", "unspecified"),
).trim();
const loadGeneratorCohostedRaw = String(
  envValue("STRESS_LOADGEN_COHOSTED", "unspecified"),
)
  .trim()
  .toLowerCase();
const loadGeneratorCohosted =
  loadGeneratorCohostedRaw === "true"
    ? true
    : loadGeneratorCohostedRaw === "false"
      ? false
      : null;
const clockOffsetMsRaw = String(envValue("STRESS_CLOCK_OFFSET_MS", "")).trim();
const clockOffsetMs =
  clockOffsetMsRaw.length === 0 ? null : Number(clockOffsetMsRaw);
const observabilityProfile = String(
  envValue("STRESS_OBSERVABILITY_PROFILE", "unspecified"),
)
  .trim()
  .toLowerCase();
const measuredSec = Number.parseFloat(envValue("STRESS_MEASURED_SEC", "30"));
const phase4BlockTxTarget = Number.parseInt(
  envValue("STRESS_PHASE4_BLOCK_TX_TARGET", "0"),
  10,
);
const configuredCommitMaxL2TxCount = Number.parseInt(
  envValue("COMMIT_MAX_L2_TX_COUNT", "0"),
  10,
);
const phase4EnvironmentFingerprintPath = envValue(
  "STRESS_PHASE4_ENVIRONMENT_FINGERPRINT_PATH",
  null,
);
const phase4EnvironmentFingerprint =
  phase4EnvironmentFingerprintPath === null
    ? null
    : (() => {
        const bytes = fs.readFileSync(phase4EnvironmentFingerprintPath);
        const artifact = JSON.parse(bytes.toString("utf8"));
        return {
          path: path.resolve(phase4EnvironmentFingerprintPath),
          sha256: createHash("sha256").update(bytes).digest("hex"),
          artifactSchemaVersion: artifact.schemaVersion,
          documentSha256: artifact.documentSha256,
          document: artifact.document,
        };
      })();
const warmupTxs = Number.parseInt(envValue("STRESS_WARMUP_TXS", "0"), 10);
const warmupSec = Number.parseFloat(envValue("STRESS_WARMUP_SEC", "0"));
const cooldownSec = Number.parseFloat(envValue("STRESS_COOLDOWN_SEC", "3"));
const drainTimeoutSec = Number.parseFloat(
  envValue("STRESS_DRAIN_TIMEOUT_SEC", "60"),
);
const waitForCommit =
  String(envValue("STRESS_WAIT_FOR_COMMIT", "false")).trim().toLowerCase() ===
  "true";
const waitForMerge =
  String(envValue("STRESS_WAIT_FOR_MERGE", "false")).trim().toLowerCase() ===
  "true";
const statusSampleSize = Number.parseInt(
  envValue("STRESS_STATUS_SAMPLE_SIZE", "100"),
  10,
);
const submitConcurrencySetting = parsePositiveIntegerSetting(
  "STRESS_SUBMIT_CONCURRENCY",
  "512",
  "from-calibration",
);
let submitConcurrency = submitConcurrencySetting.value;
const httpConnectionsSetting = parsePositiveIntegerSetting(
  "STRESS_HTTP_CONNECTIONS",
  "256",
  "from-calibration",
);
let httpConnections = httpConnectionsSetting.value;
const httpPipelining = Number.parseInt(
  envValue("STRESS_HTTP_PIPELINING", "1"),
  10,
);
const httpTimeoutMs = Number.parseInt(
  envValue("STRESS_HTTP_TIMEOUT_MS", "30000"),
  10,
);
const openLoopRate = Number.parseFloat(
  envValue("STRESS_OPEN_LOOP_RATE_TPS", String(targetAcceptedTps)),
);
const rampStartTps = Number.parseFloat(
  envValue("STRESS_RAMP_START_TPS", "100"),
);
const rampStepTps = Number.parseFloat(envValue("STRESS_RAMP_STEP_TPS", "100"));
const rampMaxTps = Number.parseFloat(
  envValue(
    "STRESS_RAMP_MAX_TPS",
    String(targetAcceptedTps > 0 ? targetAcceptedTps : 1000),
  ),
);
const rampStageSec = Number.parseFloat(envValue("STRESS_RAMP_STAGE_SEC", "15"));
const rampMinAcceptedRatio = Number.parseFloat(
  envValue("STRESS_RAMP_MIN_ACCEPTED_RATIO", "0.99"),
);
const offeredRateMinRatio = Number.parseFloat(
  envValue("STRESS_OFFERED_RATE_MIN_RATIO", "0.98"),
);
const acceptedRateMinRatio = Number.parseFloat(
  envValue("STRESS_ACCEPTED_RATE_MIN_RATIO", "0.99"),
);
const scheduleLagP95MaxMs = Number.parseFloat(
  envValue("STRESS_SCHEDULE_LAG_P95_MAX_MS", "100"),
);
const scheduleLagP99MaxMs = Number.parseFloat(
  envValue("STRESS_SCHEDULE_LAG_P99_MAX_MS", "250"),
);
const submitLatencyP99MaxMs = Number.parseFloat(
  envValue("STRESS_SUBMIT_LATENCY_P99_MAX_MS", "1000"),
);
const missedStartMaxRatio = Number.parseFloat(
  envValue("STRESS_MISSED_START_MAX_RATIO", "0.001"),
);
const backlogSlopeMaxPerSec = Number.parseFloat(
  envValue("STRESS_BACKLOG_SLOPE_MAX_PER_SEC", "0.1"),
);
const phase1StarvationGateEnabled =
  String(envValue("STRESS_PHASE1_STARVATION_GATE", "false"))
    .trim()
    .toLowerCase() === "true";
const phase1StarvationMinDurationSec = parsePositiveFloatSetting(
  "STRESS_PHASE1_STARVATION_MIN_DURATION_SEC",
  "600",
);
const phase1StarvationMaxAgeMultiplier = parsePositiveFloatSetting(
  "STRESS_PHASE1_STARVATION_MAX_AGE_MULTIPLIER",
  "3",
);
const phase1StarvationMinOverloadRatio = parsePositiveFloatSetting(
  "STRESS_PHASE1_STARVATION_MIN_OVERLOAD_RATIO",
  "2",
);
const phase1StarvationBaselineTps = parsePositiveFloatSetting(
  "STRESS_PHASE1_STARVATION_BASELINE_TPS",
  "2500",
);
const phase1StageAWindowGateEnabled =
  String(envValue("STRESS_PHASE1_STAGE_A_WINDOW_GATE", "false"))
    .trim()
    .toLowerCase() === "true";
const phase1StageAWindowSec = parsePositiveFloatSetting(
  "STRESS_PHASE1_STAGE_A_WINDOW_SEC",
  "300",
);
const phase1StageACheckpointMaxJitterMs = parsePositiveFloatSetting(
  "STRESS_PHASE1_STAGE_A_CHECKPOINT_MAX_JITTER_MS",
  "1000",
);
const candidateCleanTimeoutSec = Number.parseFloat(
  envValue("STRESS_CANDIDATE_CLEAN_TIMEOUT_SEC", "30"),
);
const requireIdleNode =
  String(envValue("STRESS_REQUIRE_IDLE_NODE", "true")).trim().toLowerCase() !==
  "false";
const idleProbeSec = Number.parseFloat(envValue("STRESS_IDLE_PROBE_SEC", "2"));
const requireMetricPresence =
  String(envValue("STRESS_REQUIRE_METRIC_PRESENCE", "true"))
    .trim()
    .toLowerCase() !== "false";
const findMaxBinaryIterations = Number.parseInt(
  envValue("STRESS_FIND_MAX_BINARY_ITERATIONS", "6"),
  10,
);
const findMaxConfirmationSec = Number.parseFloat(
  envValue("STRESS_FIND_MAX_CONFIRMATION_SEC", String(measuredSec)),
);
const findMaxRepeats = Number.parseInt(
  envValue("STRESS_FIND_MAX_REPEATS", "2"),
  10,
);
const findMaxMaxCandidates = Number.parseInt(
  envValue("STRESS_FIND_MAX_MAX_CANDIDATES", "32"),
  10,
);
const clientSelfCheckEnabled =
  String(envValue("STRESS_CLIENT_SELF_CHECK", "true")).trim().toLowerCase() !==
  "false";
const clientSelfCheckRequired =
  String(envValue("STRESS_CLIENT_SELF_CHECK_REQUIRED", "false"))
    .trim()
    .toLowerCase() !== "false";
const clientSelfCheckMultiplier = Number.parseFloat(
  envValue("STRESS_CLIENT_SELF_CHECK_MULTIPLIER", "2"),
);
const clientSelfCheckMinRatio = Number.parseFloat(
  envValue("STRESS_CLIENT_SELF_CHECK_MIN_RATIO", "0.95"),
);
const clientSelfCheckDurationSec = Number.parseFloat(
  envValue("STRESS_CLIENT_SELF_CHECK_DURATION_SEC", "2"),
);
const reportPath =
  envValue("STRESS_REPORT_PATH") ??
  path.join(
    pkgRoot,
    "benchmark-results",
    `l2-throughput-${new Date().toISOString().replace(/[:.]/g, "-")}.json`,
  );
const engineEventsPath =
  envValue("STRESS_ENGINE_EVENTS_PATH", null) ??
  path.join(path.dirname(reportPath), "engine-events.ndjson");
const submitRecordsPath =
  envValue("STRESS_SUBMIT_RECORDS_PATH", null) ??
  path.join(path.dirname(reportPath), "submit-records.ndjson");
const noOpCalibrationPath =
  envValue("STRESS_NOOP_CALIBRATION_PATH", null) ??
  path.join(path.dirname(reportPath), "noop-calibration.json");
const pgStatStatementsEnabled =
  String(envValue("STRESS_PG_STAT_STATEMENTS", "false"))
    .trim()
    .toLowerCase() === "true";
const profileMode =
  String(envValue("STRESS_PROFILE_MODE", "false")).trim().toLowerCase() ===
  "true";
const pyroscopeEnabled =
  String(envValue("STRESS_PYROSCOPE", "false")).trim().toLowerCase() === "true";

const execFileAsync = promisify(execFile);

if (!Number.isFinite(chainLength) || chainLength <= 0) {
  throw new Error("STRESS_CHAIN_LENGTH must be a positive integer");
}
if (maxChains !== null && (!Number.isFinite(maxChains) || maxChains <= 0)) {
  throw new Error("STRESS_MAX_CHAINS must be a positive integer");
}
if (!["fanout", "chain", "mixed"].includes(corpusShape)) {
  throw new Error("STRESS_CORPUS_SHAPE must be fanout, chain, or mixed");
}
if (!Number.isFinite(corpusReadAheadRows) || corpusReadAheadRows <= 0) {
  throw new Error("STRESS_CORPUS_READAHEAD_ROWS must be a positive integer");
}
const corpusPreflightValues = [
  corpusPreflightPath,
  corpusPreflightSha256,
  corpusPreflightSourceIdentitySha256,
  corpusPreflightPhase1BindingSha256,
];
const corpusPreflightEnabled = corpusPreflightValues.every(
  (value) => typeof value === "string" && value.trim().length > 0,
);
if (
  (corpusPreflightRequired && !corpusPreflightEnabled) ||
  (!corpusPreflightEnabled &&
    corpusPreflightValues.some((value) => value !== null))
) {
  throw new Error(
    "full corpus preflight requires path, artifact SHA-256, source-identity SHA-256, and Phase 1 binding SHA-256",
  );
}
if (!Number.isFinite(utxosPerWallet) || utxosPerWallet <= 0) {
  throw new Error("STRESS_UTXOS_PER_WALLET must be a positive integer");
}
if (!Number.isFinite(fanoutMaxOutputsPerTx) || fanoutMaxOutputsPerTx <= 1) {
  throw new Error(
    "STRESS_FANOUT_MAX_OUTPUTS_PER_TX must be an integer greater than 1",
  );
}
if (!Number.isFinite(fanoutStatusTimeoutMs) || fanoutStatusTimeoutMs <= 0) {
  throw new Error("STRESS_FANOUT_STATUS_TIMEOUT_MS must be a positive integer");
}
if (!Number.isFinite(metricsPollMs) || metricsPollMs <= 0) {
  throw new Error("STRESS_METRICS_POLL_MS must be a positive integer");
}
if (!["closed", "open", "ramp", "find-max"].includes(benchmarkMode)) {
  throw new Error("STRESS_MODE must be one of: closed, open, ramp, find-max");
}
if (!["A", "B"].includes(scenarioClass)) {
  throw new Error("STRESS_SCENARIO_CLASS must be A or B");
}
if (formalBenchmark) {
  if (
    !["separate-host", "separate-container", "measured-cgroup"].includes(
      loadGeneratorPlacement,
    )
  ) {
    throw new Error(
      "formal benchmark requires STRESS_LOAD_GENERATOR_PLACEMENT=separate-host, separate-container, or measured-cgroup",
    );
  }
  if (loadGeneratorCohosted === null) {
    throw new Error(
      "formal benchmark requires STRESS_LOADGEN_COHOSTED=true or false",
    );
  }
  if (
    (loadGeneratorPlacement === "separate-host" && loadGeneratorCohosted) ||
    (["separate-container", "measured-cgroup"].includes(
      loadGeneratorPlacement,
    ) &&
      !loadGeneratorCohosted)
  ) {
    throw new Error(
      "STRESS_LOADGEN_COHOSTED is inconsistent with STRESS_LOAD_GENERATOR_PLACEMENT",
    );
  }
  if (clockOffsetMs === null || !Number.isFinite(clockOffsetMs)) {
    throw new Error("formal benchmark requires finite STRESS_CLOCK_OFFSET_MS");
  }
  if (!["on", "off"].includes(observabilityProfile)) {
    throw new Error(
      "formal benchmark requires STRESS_OBSERVABILITY_PROFILE=on or off",
    );
  }
}
const loadGeneratorIsolation = loadGeneratorIsolationRequired
  ? consumePhase3LoadGeneratorIsolation({
      artifactPath: loadGeneratorIsolationPath,
      artifactSha256: loadGeneratorIsolationSha256,
    })
  : null;
if (!Number.isFinite(measuredSec) || measuredSec <= 0) {
  throw new Error("STRESS_MEASURED_SEC must be a positive number");
}
if (!Number.isInteger(phase4BlockTxTarget) || phase4BlockTxTarget < 0) {
  throw new Error(
    "STRESS_PHASE4_BLOCK_TX_TARGET must be a non-negative integer",
  );
}
if (
  phase4BlockTxTarget > 0 &&
  configuredCommitMaxL2TxCount !== phase4BlockTxTarget
) {
  throw new Error(
    `Phase 4 block target ${phase4BlockTxTarget.toString()} does not match COMMIT_MAX_L2_TX_COUNT=${configuredCommitMaxL2TxCount.toString()}`,
  );
}
if (phase4BlockTxTarget > 0 && phase4EnvironmentFingerprint === null) {
  throw new Error(
    "STRESS_PHASE4_ENVIRONMENT_FINGERPRINT_PATH is required for a Phase 4 gate",
  );
}
if (!Number.isFinite(warmupTxs) || warmupTxs < 0) {
  throw new Error("STRESS_WARMUP_TXS must be a non-negative integer");
}
if (!Number.isFinite(warmupSec) || warmupSec < 0) {
  throw new Error("STRESS_WARMUP_SEC must be a non-negative number");
}
if (!Number.isFinite(cooldownSec) || cooldownSec < 0) {
  throw new Error("STRESS_COOLDOWN_SEC must be a non-negative number");
}
if (!Number.isFinite(drainTimeoutSec) || drainTimeoutSec <= 0) {
  throw new Error("STRESS_DRAIN_TIMEOUT_SEC must be a positive number");
}
if (!Number.isFinite(statusSampleSize) || statusSampleSize < 0) {
  throw new Error("STRESS_STATUS_SAMPLE_SIZE must be a non-negative integer");
}
if (!Number.isFinite(submitConcurrency) || submitConcurrency <= 0) {
  throw new Error("STRESS_SUBMIT_CONCURRENCY must be a positive integer");
}
if (!Number.isFinite(httpConnections) || httpConnections <= 0) {
  throw new Error("STRESS_HTTP_CONNECTIONS must be a positive integer");
}
if (!Number.isFinite(httpPipelining) || httpPipelining <= 0) {
  throw new Error("STRESS_HTTP_PIPELINING must be a positive integer");
}
if (!Number.isFinite(httpTimeoutMs) || httpTimeoutMs <= 0) {
  throw new Error("STRESS_HTTP_TIMEOUT_MS must be a positive integer");
}
if (!Number.isFinite(measuredRetry503) || measuredRetry503 < 0) {
  throw new Error("STRESS_MEASURED_RETRY_503 must be a non-negative integer");
}
for (const [name, value] of [
  ["STRESS_OFFERED_RATE_MIN_RATIO", offeredRateMinRatio],
  ["STRESS_ACCEPTED_RATE_MIN_RATIO", acceptedRateMinRatio],
  ["STRESS_RAMP_MIN_ACCEPTED_RATIO", rampMinAcceptedRatio],
]) {
  if (!Number.isFinite(value) || value <= 0 || value > 1) {
    throw new Error(`${name} must be in the range (0, 1]`);
  }
}
for (const [name, value] of [
  ["STRESS_SCHEDULE_LAG_P95_MAX_MS", scheduleLagP95MaxMs],
  ["STRESS_SCHEDULE_LAG_P99_MAX_MS", scheduleLagP99MaxMs],
  ["STRESS_SUBMIT_LATENCY_P99_MAX_MS", submitLatencyP99MaxMs],
  ["STRESS_BACKLOG_SLOPE_MAX_PER_SEC", backlogSlopeMaxPerSec],
  ["STRESS_CANDIDATE_CLEAN_TIMEOUT_SEC", candidateCleanTimeoutSec],
  ["STRESS_IDLE_PROBE_SEC", idleProbeSec],
  ["STRESS_FIND_MAX_CONFIRMATION_SEC", findMaxConfirmationSec],
]) {
  if (!Number.isFinite(value) || value < 0) {
    throw new Error(`${name} must be a non-negative number`);
  }
}
if (!Number.isFinite(missedStartMaxRatio) || missedStartMaxRatio < 0) {
  throw new Error("STRESS_MISSED_START_MAX_RATIO must be non-negative");
}
for (const [name, value] of [
  ["STRESS_FIND_MAX_BINARY_ITERATIONS", findMaxBinaryIterations],
  ["STRESS_FIND_MAX_REPEATS", findMaxRepeats],
  ["STRESS_FIND_MAX_MAX_CANDIDATES", findMaxMaxCandidates],
]) {
  if (!Number.isFinite(value) || value < 1) {
    throw new Error(`${name} must be a positive integer`);
  }
}

/** @typedef {{ outref: string; outputCbor: string }} NodeUtxo */
/** @typedef {{ txHex: string; txIdHex: string }} PrebuiltTx */

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

const makeNdjsonWriter = (filePath) => {
  fs.mkdirSync(path.dirname(filePath), { recursive: true });
  const stream = fs.createWriteStream(filePath, { flags: "w" });
  return {
    path: filePath,
    write(value) {
      stream.write(`${JSON.stringify(value)}\n`);
    },
    async close() {
      await new Promise((resolve, reject) => {
        stream.once("error", reject);
        stream.end(resolve);
      });
    },
  };
};

let engineEventWriter = null;
let submitRecordWriter = null;

const writeEngineEvent = (event, payload = {}) => {
  engineEventWriter?.write({
    event,
    at: new Date().toISOString(),
    ...payload,
  });
};

class BenchmarkHttpClient {
  constructor({ connections, pipelining, timeoutMs }) {
    this.connections = connections;
    this.pipelining = pipelining;
    this.timeoutMs = timeoutMs;
    this.pools = new Map();
  }

  poolFor(url) {
    const parsed = new URL(url);
    let pool = this.pools.get(parsed.origin);
    if (pool === undefined) {
      pool = new Pool(parsed.origin, {
        connections: this.connections,
        pipelining: this.pipelining,
        headersTimeout: this.timeoutMs,
        bodyTimeout: this.timeoutMs,
      });
      this.pools.set(parsed.origin, pool);
    }
    return pool;
  }

  async request(url, options = {}) {
    const parsed = new URL(url);
    const pool = this.poolFor(url);
    const startedAt = performance.now();
    const response = await pool.request({
      method: options.method ?? "GET",
      path: `${parsed.pathname}${parsed.search}`,
      headers: options.headers,
      body: options.body,
      headersTimeout: this.timeoutMs,
      bodyTimeout: this.timeoutMs,
    });
    const text = await response.body.text();
    const latencyMs = performance.now() - startedAt;
    return {
      status: response.statusCode,
      ok: response.statusCode >= 200 && response.statusCode < 300,
      body: text,
      latencyMs,
      json() {
        return text.length === 0 ? {} : JSON.parse(text);
      },
    };
  }

  async close() {
    await Promise.all(Array.from(this.pools.values(), (pool) => pool.close()));
  }
}

let httpClient = new BenchmarkHttpClient({
  connections: httpConnections,
  pipelining: httpPipelining,
  timeoutMs: httpTimeoutMs,
});

const rebuildHttpClient = async ({ connections }) => {
  await httpClient.close();
  httpConnections = connections;
  httpClient = new BenchmarkHttpClient({
    connections: httpConnections,
    pipelining: httpPipelining,
    timeoutMs: httpTimeoutMs,
  });
};

/**
 * Fetches raw Prometheus metrics text from the node.
 */
const fetchMetricsText = async () => {
  const resp = await httpClient.request(metricsEndpoint);
  if (!resp.ok) {
    throw new Error(`metrics endpoint returned ${resp.status}`);
  }
  return resp.body;
};

/**
 * Escapes a string for literal use in a regular expression.
 */
const escapeRegex = (value) => value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

/**
 * Extracts a Prometheus sample value from metrics text.
 */
const extractMetricValue = (text, names) => {
  for (const name of names) {
    const pattern = `^${escapeRegex(name)}(?:\\{[^}]*\\})?\\s+([0-9]+(?:\\.[0-9]+)?)$`;
    const m = text.match(new RegExp(pattern, "m"));
    if (m !== null) {
      return { value: Number(m[1]), name };
    }
  }
  return { value: 0, name: null };
};

const extractMetricSum = (text, names) => {
  for (const name of names) {
    const pattern = new RegExp(
      `^${escapeRegex(name)}(?:\\{[^}]*\\})?\\s+([0-9]+(?:\\.[0-9]+)?)$`,
      "gm",
    );
    const values = [];
    let match = pattern.exec(text);
    while (match !== null) {
      values.push(Number(match[1]));
      match = pattern.exec(text);
    }
    if (values.length > 0) {
      return { value: values.reduce((sum, value) => sum + value, 0), name };
    }
  }
  return { value: 0, name: null };
};

const metricSpecs = {
  submit: ["tx_count_total", "tx_count"],
  accept: ["validation_accept_count_total", "validation_accept_count"],
  reject: ["validation_reject_count_total", "validation_reject_count"],
  mempoolTx: ["mempool_tx_count"],
  validationQueueDepth: ["validation_queue_depth"],
  validationBatchSize: ["validation_batch_size"],
  validationPhaseALatencyMs: ["validation_phase_a_latency_ms"],
  validationPhaseBLatencyMs: ["validation_phase_b_latency_ms"],
  validationBatchDurationCount: ["validation_batch_duration_count"],
  validationBatchDurationSum: ["validation_batch_duration_sum"],
  validationPhaseADurationCount: ["validation_phase_a_duration_count"],
  validationPhaseADurationSum: ["validation_phase_a_duration_sum"],
  validationPhaseBDurationCount: ["validation_phase_b_duration_count"],
  validationPhaseBDurationSum: ["validation_phase_b_duration_sum"],
  submitHandlerLatencyCount: ["submit_handler_latency_count"],
  submitHandlerLatencySum: ["submit_handler_latency_sum"],
  submitQueueOfferFailure: [
    "submit_queue_offer_failure_count_total",
    "submit_queue_offer_failure_count",
  ],
  commitBlock: ["commit_block_count_total", "commit_block_count"],
  commitBlockTx: ["commit_block_tx_count_total", "commit_block_tx_count"],
  commitBlockNumTx: ["commit_block_num_tx_count"],
  mempoolOldestTxAgeMs: ["mempool_oldest_tx_age_ms"],
  commitWorkerDurationCount: ["commit_worker_duration_count"],
  commitWorkerDurationSum: ["commit_worker_duration_sum"],
  mergeBlock: ["merge_block_count_total", "merge_block_count"],
  mergeFailure: ["merge_failure_count_total", "merge_failure_count"],
  mergeDurationCount: ["merge_duration_count"],
  mergeDurationSum: ["merge_duration_sum"],
  blocksInQueue: ["blocks_in_queue"],
  processedUnsubmittedTxs: ["processed_unsubmitted_txs_count"],
  processedUnsubmittedTxsSizeBytes: ["processed_unsubmitted_txs_size_bytes"],
  unconfirmedSubmittedBlockPending: ["unconfirmed_submitted_block_pending"],
  unconfirmedSubmittedBlockAgeMs: ["unconfirmed_submitted_block_age_ms"],
  speculationHit: ["speculation_hit_total", "speculation_hit"],
  speculationInvalidations: [
    "speculation_invalidations_total",
    "speculation_invalidations",
  ],
  speculationOverlapEfficiency: ["speculation_overlap_efficiency"],
  daPublicationBacklog: ["da_publish_reconciler_backlog"],
};

const requiredStageMetricKeys = [
  "submit",
  "accept",
  "reject",
  "validationQueueDepth",
];

const metricMissingKeys = (counters, keys = requiredStageMetricKeys) =>
  keys.filter((key) => counters.metricNames?.[key] === null);

/**
 * Extracts Prometheus histogram series for machine-readable report artifacts.
 */
const extractHistogram = (text, baseName) => {
  const escaped = escapeRegex(baseName);
  const count = extractMetricValue(text, [`${baseName}_count`]);
  const sum = extractMetricValue(text, [`${baseName}_sum`]);
  const buckets = [];
  const re = new RegExp(
    `^${escaped}_bucket\\{([^}]*)\\}\\s+([0-9]+(?:\\.[0-9]+)?)$`,
    "gm",
  );
  let match = re.exec(text);
  while (match !== null) {
    const labels = match[1];
    const le = labels
      .split(",")
      .map((part) => part.trim())
      .find((part) => part.startsWith("le="));
    buckets.push({
      le: le === undefined ? null : le.slice(3).replace(/^"|"$/g, ""),
      value: Number(match[2]),
    });
    match = re.exec(text);
  }
  return {
    count: count.name === null ? null : count.value,
    sum: sum.name === null ? null : sum.value,
    buckets,
  };
};

/**
 * Fetches and parses the counters used by the workload monitor.
 */
const readCounters = async () => {
  const text = await fetchMetricsText();
  const counters = { metricNames: {}, missingMetrics: [], histograms: {} };
  for (const [key, names] of Object.entries(metricSpecs)) {
    const extracted =
      key === "speculationInvalidations"
        ? extractMetricSum(text, names)
        : extractMetricValue(text, names);
    counters[key] = extracted.value;
    counters.metricNames[key] = extracted.name;
    if (extracted.name === null) {
      counters.missingMetrics.push({ key, names });
    }
  }
  counters.histograms.submitHandlerLatency = extractHistogram(
    text,
    "submit_handler_latency",
  );
  counters.histograms.validationBatchDuration = extractHistogram(
    text,
    "validation_batch_duration",
  );
  counters.histograms.validationPhaseADuration = extractHistogram(
    text,
    "validation_phase_a_duration",
  );
  counters.histograms.validationPhaseBDuration = extractHistogram(
    text,
    "validation_phase_b_duration",
  );
  counters.histograms.commitWorkerDuration = extractHistogram(
    text,
    "commit_worker_duration",
  );
  counters.histograms.mergeDuration = extractHistogram(text, "merge_duration");
  counters.histograms.commitCadenceMs = extractHistogram(
    text,
    "commit_cadence_ms",
  );
  counters.histograms.speculativeBuildDurationMs = extractHistogram(
    text,
    "speculative_build_duration_ms",
  );
  counters.histograms.submitAfterConfirmMs = extractHistogram(
    text,
    "submit_after_confirm_ms",
  );
  counters.histograms.confirmationDetectionLagMs = extractHistogram(
    text,
    "confirmation_detection_lag_ms",
  );
  counters.histograms.l1ConfirmationWaitMs = extractHistogram(
    text,
    "l1_confirmation_wait_ms",
  );
  counters.l1TipSlot = null;
  if (scenarioClass === "B") {
    try {
      const readiness = await httpClient.request(`${submitEndpoint}/readyz`);
      if (readiness.ok) {
        const body = readiness.json();
        const slot = Number(body?.localOgmiosSlot?.currentSlot);
        counters.l1TipSlot = Number.isFinite(slot) ? slot : null;
      }
    } catch {
      counters.l1TipSlot = null;
    }
  }
  return counters;
};

/**
 * Fetches spendable UTxOs for a wallet address.
 */
const fetchUtxos = async (address) => {
  const resp = await httpClient.request(
    `${submitEndpoint}/utxos?address=${encodeURIComponent(address)}`,
  );
  if (!resp.ok) {
    throw new Error(`utxos endpoint returned ${resp.status} for ${address}`);
  }
  const body = resp.json();
  if (!Array.isArray(body.utxos)) {
    return [];
  }
  return /** @type {NodeUtxo[]} */ (body.utxos);
};

/**
 * Fetches transaction status information from the node.
 */
const fetchTxStatus = async (txIdHex) => {
  let attempt = 0;
  while (attempt <= txStatusRetries) {
    try {
      const resp = await httpClient.request(
        `${submitEndpoint}/tx-status?tx_hash=${encodeURIComponent(txIdHex)}`,
      );
      if (!resp.ok && resp.status !== 404) {
        if (
          (resp.status === 429 || resp.status === 503) &&
          attempt < txStatusRetries
        ) {
          attempt += 1;
          await sleep(txStatusRetryDelayMs);
          continue;
        }
        throw new Error(
          `tx-status endpoint returned ${resp.status} for ${txIdHex}`,
        );
      }
      let body = {};
      try {
        body = resp.json();
      } catch {
        // Keep fallback behavior below.
      }
      if (typeof body?.status === "string") {
        return body.status;
      }
      return resp.status === 404 ? "not_found" : "unknown";
    } catch (error) {
      if (attempt >= txStatusRetries) {
        throw error;
      }
      attempt += 1;
      await sleep(txStatusRetryDelayMs);
    }
  }
  return "unknown";
};

/**
 * Prebuilds a dependent transaction chain for the stress workload.
 */
const prebuildChain = (chain, length, feeConfig) => {
  /** @type {PrebuiltTx[]} */
  const txs = [];
  let currentOutRef = chain.spendOutRefCbor;
  let currentOutputCbor = chain.outputCbor;
  for (let i = 0; i < length; i++) {
    const tx = buildNativeSignedOneToOne({
      spendOutRefCbor: currentOutRef,
      signer: chain.signer,
      inputOutputCbor: currentOutputCbor,
      minFeeA: feeConfig.minFeeA,
      minFeeB: feeConfig.minFeeB,
    });
    txs.push({
      txHex: tx.txHex,
      txIdHex: tx.txId.toString("hex"),
    });
    currentOutRef = tx.nextOutRef;
    currentOutputCbor = tx.outputCbor;
  }
  return txs;
};

/**
 * Submits a CBOR transaction hex payload to the node.
 */
const submitTxHex = async (
  txHex,
  {
    retryLimit = retry503,
    expectedTxIdHex = null,
    calibrationMode = false,
    endpoint = submitEndpoint,
  } = {},
) => {
  let attempt = 0;
  const attempts = [];
  while (attempt <= retryLimit) {
    const bodyBytes = Buffer.from(txHex, "hex");
    const resp = await httpClient.request(`${endpoint}/submit`, {
      method: "POST",
      headers: { "content-type": "application/cbor" },
      body: bodyBytes,
    });
    attempts.push({
      status: resp.status,
      ok: resp.ok,
      latencyMs: resp.latencyMs,
    });

    let responseTxId = null;
    try {
      const parsed = resp.json();
      responseTxId =
        typeof parsed?.txId === "string" ? parsed.txId.toLowerCase() : null;
    } catch {
      responseTxId = null;
    }

    if (
      resp.ok &&
      !calibrationMode &&
      expectedTxIdHex !== null &&
      responseTxId !== null &&
      responseTxId !== expectedTxIdHex
    ) {
      return {
        ok: false,
        status: resp.status,
        body: resp.body,
        latencyMs: resp.latencyMs,
        attempts,
        physicalAttemptCount: attempts.length,
        responseTxId,
        error: "duplicate_or_mismatched_response",
      };
    }

    if (resp.ok) {
      return {
        ok: true,
        status: resp.status,
        latencyMs: resp.latencyMs,
        attempts,
        physicalAttemptCount: attempts.length,
        responseTxId,
      };
    }

    const body = resp.body;
    if ((resp.status === 503 || resp.status === 429) && attempt < retryLimit) {
      attempt += 1;
      await sleep(retryDelayMs);
      continue;
    }

    return {
      ok: false,
      status: resp.status,
      body,
      latencyMs: resp.latencyMs,
      attempts,
      physicalAttemptCount: attempts.length,
      responseTxId,
    };
  }

  return {
    ok: false,
    status: 0,
    body: "retry loop exhausted",
    latencyMs: 0,
    attempts,
    physicalAttemptCount: attempts.length,
  };
};

/**
 * Waits until a transaction has passed validation or failed.
 */
const waitForAcceptedTx = async (txIdHex, label) => {
  const startedAt = Date.now();
  while (Date.now() - startedAt <= fanoutStatusTimeoutMs) {
    const status = await fetchTxStatus(txIdHex);
    if (
      status === "accepted" ||
      status === "pending_commit" ||
      status === "awaiting_local_recovery" ||
      status === "committed"
    ) {
      return status;
    }
    if (status === "rejected") {
      throw new Error(`${label} ${txIdHex} was rejected`);
    }
    await sleep(txStatusRetryDelayMs);
  }
  throw new Error(
    `${label} ${txIdHex} did not reach accepted status within ${fanoutStatusTimeoutMs}ms`,
  );
};

/**
 * Estimates a safe leaf value for generated fanout UTxOs.
 */
const defaultFanoutOutputLovelace = (minFeeA, minFeeB) => {
  const estimatedTxBytes = 900n;
  const estimatedFee = minFeeA * estimatedTxBytes + minFeeB;
  return estimatedFee * BigInt(chainLength + 2) + 1_000_000n;
};

/**
 * Expands available source UTxOs into enough independent benchmark chains.
 */
const ensureFanoutCandidates = async ({ candidates, minFeeA, minFeeB }) => {
  if (!fanoutEnabled || maxChains === null || candidates.length >= maxChains) {
    return {
      candidates,
      fanoutTxCount: 0,
      fanoutOutputCount: 0,
    };
  }

  const leafLovelace =
    fanoutOutputLovelace ?? defaultFanoutOutputLovelace(minFeeA, minFeeB);
  const result = [...candidates];
  let fanoutTxCount = 0;
  let fanoutOutputCount = 0;

  result.sort((a, b) =>
    a.lovelace === b.lovelace ? 0 : a.lovelace > b.lovelace ? -1 : 1,
  );

  for (
    let sourceIndex = 0;
    result.length < maxChains && sourceIndex < result.length;
    sourceIndex += 1
  ) {
    const source = result[sourceIndex];
    if (outputHasMultiAssets(source.outputCbor)) {
      continue;
    }

    const remainingAfterSpendingSource = result.length - 1;
    const neededOutputs = maxChains - remainingAfterSpendingSource;
    const affordableOutputs = Number(source.lovelace / leafLovelace);
    const outputCount = Math.min(
      fanoutMaxOutputsPerTx,
      neededOutputs,
      affordableOutputs,
    );
    if (outputCount <= 1) {
      continue;
    }

    const split = buildNativeSignedSplit({
      spendOutRefCbor: source.spendOutRefCbor,
      inputOutputCbor: source.outputCbor,
      signer: source.signer,
      outputCount,
      minFeeA,
      minFeeB,
    });
    const txIdHex = split.txId.toString("hex");
    const submittedResult = await submitTxHex(split.txHex);
    if (!submittedResult.ok) {
      throw new Error(
        `fanout submit failed for ${source.outRefHex}: status=${submittedResult.status} body=${submittedResult.body ?? ""}`,
      );
    }
    const status = await waitForAcceptedTx(txIdHex, "fanout tx");
    console.log(
      `fanout tx accepted: tx=${txIdHex} status=${status} outputs=${split.outputs.length} source=${source.outRefHex}`,
    );

    result.splice(
      sourceIndex,
      1,
      ...split.outputs.map((output) => ({
        walletKey: source.walletKey,
        address: source.address,
        signer: source.signer,
        spendOutRefCbor: output.spendOutRefCbor,
        outputCbor: output.outputCbor,
        lovelace: CML.TransactionOutput.from_cbor_bytes(output.outputCbor)
          .amount()
          .coin(),
        outRefHex: output.outRefHex,
      })),
    );
    fanoutTxCount += 1;
    fanoutOutputCount += split.outputs.length;
    sourceIndex += split.outputs.length - 1;
  }

  return {
    candidates: result,
    fanoutTxCount,
    fanoutOutputCount,
  };
};

const makeChainCursors = (chains) =>
  chains.map((chain, chainIndex) => ({
    chain,
    chainIndex,
    nextIndex: 0,
    stopped: false,
    async takeNextTx() {
      if (this.stopped || this.nextIndex >= this.chain.txs.length) {
        return null;
      }
      const tx = this.chain.txs[this.nextIndex];
      const txIndex = this.nextIndex;
      this.nextIndex += 1;
      return {
        ...tx,
        chainIndex: this.chainIndex,
        txIndex,
      };
    },
  }));

const remainingTxCount = (cursors) =>
  cursors.reduce(
    (acc, cursor) =>
      acc +
      (cursor.stopped
        ? 0
        : (cursor.entry?.rowCount ?? cursor.chain.txs.length) -
          cursor.nextIndex),
    0,
  );

const takeNextTx = async (cursor) => await cursor.takeNextTx();

const createStageStats = ({ name, mode, targetRateTps = null }) => ({
  name,
  mode,
  targetRateTps,
  startedAtMs: Date.now(),
  endedAtMs: null,
  counterStart: null,
  counterEnd: null,
  drainCounters: null,
  drain: null,
  logicalSubmitAttempts: 0,
  physicalSubmitAttempts: 0,
  submitted: 0,
  submitErrors: 0,
  submitStatusCounts: {},
  physicalSubmitStatusCounts: {},
  queueFullResponses: 0,
  firstErrors: [],
  submitLatencyMs: [],
  submitAttemptLatencyMs: [],
  statusLatencyMs: [],
  scheduleLagMs: [],
  scheduledStarts: 0,
  sentStarts: 0,
  missedStarts: 0,
  inFlightHighWater: 0,
  bytesSent: 0,
  statusSampleTxIds: [],
  submittedAtByTxId: new Map(),
  cursorPositionsAtStart: null,
  cursorPositionsAtEnd: null,
  phase1StageACheckpoint: null,
});

const snapshotCursorPositions = (cursors) =>
  cursors.map((cursor) => ({
    chainIndex: cursor.chainIndex,
    nextIndex: cursor.nextIndex,
  }));

const cursorPositionDigest = (positions) =>
  createHash("sha256")
    .update(
      positions
        .map(({ chainIndex, nextIndex }) => `${chainIndex}|${nextIndex}`)
        .join("\n"),
    )
    .digest("hex");

const schedulePhase1StageACheckpoint = ({ stage, cursors }) => {
  if (!phase1StageAWindowGateEnabled) {
    return null;
  }
  const deadlineMs = stage.startedAtMs + phase1StageAWindowSec * 1_000;
  let settled = false;
  let resolveCheckpoint;
  const promise = new Promise((resolve) => {
    resolveCheckpoint = resolve;
  });
  const settle = (value) => {
    if (settled) {
      return;
    }
    settled = true;
    resolveCheckpoint(value);
  };
  const timeout = setTimeout(
    () => {
      void (async () => {
        const checkpointRequestedAtMs = Date.now();
        const scheduleProgress = summarizeOpenLoopCheckpointProgress({
          targetRateTps: stage.targetRateTps,
          durationSec: phase1StageAWindowSec,
          dispatchedStarts: stage.scheduledStarts,
        });
        const observedStageState = {
          logicalSubmitAttempts: stage.logicalSubmitAttempts,
          physicalSubmitAttempts: stage.physicalSubmitAttempts,
          submitted: stage.submitted,
          submitErrors: stage.submitErrors,
          submitStatusCounts: { ...stage.submitStatusCounts },
          physicalSubmitStatusCounts: {
            ...stage.physicalSubmitStatusCounts,
          },
          queueFullResponses: stage.queueFullResponses,
          expectedStarts: scheduleProgress.expectedStarts,
          scheduledStarts: scheduleProgress.scheduledStarts,
          sentStarts: stage.sentStarts,
          missedStarts: scheduleProgress.missedStarts,
          submitLatencySampleCount: stage.submitLatencyMs.length,
          submitAttemptLatencySampleCount: stage.submitAttemptLatencyMs.length,
          scheduleLagSampleCount: stage.scheduleLagMs.length,
          cursorPositions: snapshotCursorPositions(cursors),
        };
        try {
          const counters = await readCounters();
          const observedAtMs = Date.now();
          settle({
            checkpointDeadlineMs: deadlineMs,
            checkpointRequestedAtMs,
            observedAtMs,
            counters,
            ...observedStageState,
            error: null,
          });
        } catch (error) {
          settle({
            checkpointDeadlineMs: deadlineMs,
            checkpointRequestedAtMs,
            observedAtMs: Date.now(),
            counters: null,
            ...observedStageState,
            error: error instanceof Error ? error.message : String(error),
          });
        }
      })();
    },
    Math.max(0, deadlineMs - Date.now()),
  );

  return {
    finish: async (stageEndedAtMs) => {
      if (!settled && stageEndedAtMs < deadlineMs) {
        clearTimeout(timeout);
        settle({
          checkpointDeadlineMs: deadlineMs,
          observedAtMs: stageEndedAtMs,
          counters: null,
          error: `stage ended before ${phase1StageAWindowSec}-second checkpoint`,
        });
      }
      return await promise;
    },
  };
};

const recordSubmitResult = (stage, tx, result) => {
  stage.logicalSubmitAttempts += 1;
  stage.physicalSubmitAttempts += result.physicalAttemptCount ?? 1;
  stage.bytesSent += Buffer.byteLength(tx.txHex, "hex");
  const key = String(result.status);
  stage.submitStatusCounts[key] = (stage.submitStatusCounts[key] ?? 0) + 1;
  stage.submitLatencyMs.push(result.latencyMs ?? 0);
  for (const attempt of result.attempts ?? []) {
    const attemptKey = String(attempt.status);
    stage.physicalSubmitStatusCounts[attemptKey] =
      (stage.physicalSubmitStatusCounts[attemptKey] ?? 0) + 1;
    stage.submitAttemptLatencyMs.push(attempt.latencyMs ?? 0);
    if (attempt.status === 429 || attempt.status === 503) {
      stage.queueFullResponses += 1;
    }
  }
  if (result.ok) {
    stage.submitted += 1;
    stage.submittedAtByTxId.set(tx.txIdHex, Date.now());
    if (stage.statusSampleTxIds.length < statusSampleSize) {
      stage.statusSampleTxIds.push(tx.txIdHex);
    }
    return;
  }

  stage.submitErrors += 1;
  if (stage.firstErrors.length < 10) {
    stage.firstErrors.push(
      `chain=${tx.chainIndex} index=${tx.txIndex} status=${result.status} error=${result.error ?? "submit_failed"} body=${result.body ?? ""}`,
    );
  }
};

const submitOne = async (
  cursor,
  stage,
  { scheduledAtPerfMs = null, retryLimit = retry503 } = {},
) => {
  const tx = await takeNextTx(cursor);
  if (tx === null) {
    return false;
  }
  const submittedAtMs = Date.now();
  const scheduledAtMs =
    scheduledAtPerfMs === null
      ? submittedAtMs
      : Math.round(submittedAtMs - (performance.now() - scheduledAtPerfMs));
  if (scheduledAtPerfMs !== null) {
    stage.scheduleLagMs.push(performance.now() - scheduledAtPerfMs);
  }
  stage.sentStarts += 1;
  const submittedResult = await submitTxHex(tx.txHex, {
    retryLimit,
    expectedTxIdHex: tx.txIdHex,
  });
  recordSubmitResult(stage, tx, submittedResult);
  submitRecordWriter?.write({
    txHash: tx.txIdHex,
    scheduledAtMs,
    submittedAtMs,
    scheduleSlipMs:
      scheduledAtPerfMs === null
        ? 0
        : Math.max(0, performance.now() - scheduledAtPerfMs),
    latencyMs: submittedResult.latencyMs ?? 0,
    statusCode: submittedResult.status ?? null,
    responseTxId: submittedResult.responseTxId ?? null,
    error:
      submittedResult.ok === true
        ? null
        : (submittedResult.error ?? `submit_failed_${submittedResult.status}`),
  });
  if (!submittedResult.ok) {
    cursor.stopped = true;
  }
  return true;
};

const runClosedLoopStage = async ({
  name,
  cursors,
  durationSec,
  maxTxs = Number.POSITIVE_INFINITY,
  retryLimit = retry503,
}) => {
  const stage = createStageStats({ name, mode: "closed" });
  writeEngineEvent("stage_started", {
    name,
    mode: "closed",
    durationSec,
    maxTxs: Number.isFinite(maxTxs) ? maxTxs : null,
  });
  stage.counterStart = await readCounters();
  stage.startedAtMs = Date.now();
  const deadlineMs = stage.startedAtMs + durationSec * 1000;
  let started = 0;

  await Promise.all(
    cursors.map(async (cursor) => {
      while (Date.now() < deadlineMs && started < maxTxs) {
        if (remainingTxCount([cursor]) <= 0) {
          break;
        }
        started += 1;
        const hadTx = await submitOne(cursor, stage, { retryLimit });
        if (!hadTx || cursor.stopped) {
          break;
        }
      }
    }),
  );

  stage.endedAtMs = Date.now();
  stage.counterEnd = await readCounters();
  writeEngineEvent("stage_finished", {
    name,
    mode: "closed",
    submitted: stage.submitted,
    submitErrors: stage.submitErrors,
    exhausted: remainingTxCount(cursors) <= 0,
  });
  return stage;
};

const waitForAnyInFlight = async (inFlight) => {
  if (inFlight.size === 0) {
    return;
  }
  await Promise.race(inFlight);
};

const scheduledStartCountDue = ({
  nowPerfMs,
  startedAtPerfMs,
  intervalMs,
  totalStarts,
}) => {
  if (nowPerfMs < startedAtPerfMs) {
    return 0;
  }
  return Math.min(
    totalStarts,
    Math.floor((nowPerfMs - startedAtPerfMs) / intervalMs) + 1,
  );
};

const runDeadlineBatchedSchedule = async ({
  totalStarts,
  startedAtPerfMs,
  deadlinePerfMs,
  intervalMs,
  maxInFlight,
  dispatchStart,
  allowPostDeadlineCatchUp = false,
}) => {
  const inFlight = new Set();
  let nextStartIndex = 0;
  let maxObservedInFlight = 0;
  let lastDispatchedAtPerfMs = null;
  let stoppedWithoutCapacity = false;

  while (nextStartIndex < totalStarts) {
    while (
      inFlight.size >= maxInFlight &&
      (allowPostDeadlineCatchUp || performance.now() < deadlinePerfMs)
    ) {
      await waitForAnyInFlight(inFlight);
    }

    const nowPerfMs = performance.now();
    const deadlineReached = nowPerfMs >= deadlinePerfMs;
    const dueStarts = deadlineReached
      ? totalStarts
      : scheduledStartCountDue({
          nowPerfMs,
          startedAtPerfMs,
          intervalMs,
          totalStarts,
        });
    if (nextStartIndex >= dueStarts) {
      const nextDueAtPerfMs = startedAtPerfMs + nextStartIndex * intervalMs;
      const waitMs = Math.min(
        nextDueAtPerfMs - nowPerfMs,
        deadlinePerfMs - nowPerfMs,
      );
      if (waitMs > 0) {
        // Node timers do not reliably resolve sub-millisecond deadlines. One
        // coarse wake intentionally accumulates every start that becomes due;
        // the next iteration dispatches that whole batch without more timers.
        await sleep(Math.max(1, Math.ceil(waitMs)));
      }
      continue;
    }

    let dispatchedAny = false;
    while (nextStartIndex < dueStarts && inFlight.size < maxInFlight) {
      const startIndex = nextStartIndex;
      const scheduledAtPerfMs = startedAtPerfMs + startIndex * intervalMs;
      const dispatched = dispatchStart({ startIndex, scheduledAtPerfMs });
      if (dispatched === null) {
        break;
      }
      let tracked;
      tracked = Promise.resolve(dispatched).finally(() => {
        inFlight.delete(tracked);
      });
      inFlight.add(tracked);
      nextStartIndex += 1;
      dispatchedAny = true;
      lastDispatchedAtPerfMs = performance.now();
      maxObservedInFlight = Math.max(maxObservedInFlight, inFlight.size);
    }

    // A coarse timer may wake just beyond the hard deadline. Hard-deadline
    // stages dispatch the already-due final batch only into immediately
    // available capacity. Calibration may explicitly catch up instead: its
    // last-dispatch rate and schedule-slip gates expose any real shortfall.
    if (deadlineReached && !allowPostDeadlineCatchUp) {
      break;
    }

    if (nextStartIndex < dueStarts && !dispatchedAny) {
      if (inFlight.size === 0) {
        stoppedWithoutCapacity = true;
        break;
      }
      await waitForAnyInFlight(inFlight);
    }
  }

  await Promise.all(inFlight);
  return {
    scheduledStarts: nextStartIndex,
    missedStarts: totalStarts - nextStartIndex,
    maxObservedInFlight,
    lastDispatchedAtPerfMs,
    stoppedWithoutCapacity,
  };
};

const findAvailableCursor = (cursors, busy, startIndex) => {
  for (let offset = 0; offset < cursors.length; offset += 1) {
    const index = (startIndex + offset) % cursors.length;
    const cursor = cursors[index];
    if (
      !busy.has(cursor.chainIndex) &&
      !cursor.stopped &&
      cursor.nextIndex < (cursor.entry?.rowCount ?? cursor.chain.txs.length)
    ) {
      return { cursor, nextIndex: (index + 1) % cursors.length };
    }
  }
  return null;
};

const runOpenLoopStage = async ({
  name,
  cursors,
  rateTps,
  durationSec,
  maxTxs = Number.POSITIVE_INFINITY,
  retryLimit = retry503,
}) => {
  if (!Number.isFinite(rateTps) || rateTps <= 0) {
    throw new Error("open-loop stages require a positive target rate");
  }
  const stage = createStageStats({
    name,
    mode: "open",
    targetRateTps: rateTps,
  });
  writeEngineEvent("stage_started", {
    name,
    mode: "open",
    targetRateTps: rateTps,
    durationSec,
    maxTxs: Number.isFinite(maxTxs) ? maxTxs : null,
  });
  stage.counterStart = await readCounters();
  stage.startedAtMs = Date.now();
  stage.cursorPositionsAtStart = snapshotCursorPositions(cursors);
  const phase1StageACheckpoint = schedulePhase1StageACheckpoint({
    stage,
    cursors,
  });
  const startedAt = performance.now();
  const deadline = startedAt + durationSec * 1000;
  const intervalMs = 1000 / rateTps;
  const busy = new Set();
  let nextCursorIndex = 0;
  const durationScheduledStarts = Math.ceil((durationSec * 1000) / intervalMs);
  const totalStarts = Number.isFinite(maxTxs)
    ? Math.min(durationScheduledStarts, Math.floor(maxTxs))
    : durationScheduledStarts;
  const schedule = await runDeadlineBatchedSchedule({
    totalStarts,
    startedAtPerfMs: startedAt,
    deadlinePerfMs: deadline,
    intervalMs,
    maxInFlight: submitConcurrency,
    dispatchStart: ({ scheduledAtPerfMs }) => {
      const selected = findAvailableCursor(cursors, busy, nextCursorIndex);
      if (selected === null) {
        return null;
      }
      const { cursor } = selected;
      nextCursorIndex = selected.nextIndex;
      stage.scheduledStarts += 1;
      busy.add(cursor.chainIndex);
      return submitOne(cursor, stage, {
        scheduledAtPerfMs,
        retryLimit,
      }).finally(() => {
        busy.delete(cursor.chainIndex);
      });
    },
  });
  stage.missedStarts += schedule.missedStarts;
  stage.inFlightHighWater = Math.max(
    stage.inFlightHighWater,
    schedule.maxObservedInFlight,
  );
  stage.endedAtMs = Date.now();
  stage.cursorPositionsAtEnd = snapshotCursorPositions(cursors);
  stage.phase1StageACheckpoint =
    phase1StageACheckpoint === null
      ? null
      : await phase1StageACheckpoint.finish(stage.endedAtMs);
  stage.counterEnd = await readCounters();
  const exhausted =
    remainingTxCount(cursors) <= 0 && performance.now() < deadline;
  stage.abortedCorpusExhausted = exhausted;
  writeEngineEvent("stage_finished", {
    name,
    mode: "open",
    targetRateTps: rateTps,
    submitted: stage.submitted,
    submitErrors: stage.submitErrors,
    aborted_corpus_exhausted: exhausted,
  });
  return stage;
};

const collectCalibrationRows = async (cursors, count) => {
  const rows = [];
  const busy = new Set();
  let nextCursorIndex = 0;
  while (rows.length < count) {
    const selected = findAvailableCursor(cursors, busy, nextCursorIndex);
    if (selected === null) {
      break;
    }
    nextCursorIndex = selected.nextIndex;
    const tx = await takeNextTx(selected.cursor);
    if (tx !== null) {
      // Calibration needs only the request body. Retaining parsed corpus rows
      // multiplies heap use by every metadata string and output array.
      rows.push(tx.txHex);
    }
  }
  if (rows.length < count) {
    throw new Error(
      `no-op calibration needs ${count} corpus rows but only ${rows.length} were available`,
    );
  }
  return rows;
};

const summarizeScheduleSlip = (values) => {
  const summary = summarizeLatency(values);
  return {
    p50: summary.p50 ?? 0,
    p95: summary.p95 ?? 0,
    p99: summary.p99 ?? 0,
    max: summary.max ?? 0,
  };
};

const runNoOpCalibrationStage = async ({ cursors, targetRateTps }) => {
  if (noOpEndpoint === null) {
    if (requireNoOpCalibration) {
      throw new Error(
        "STRESS_REQUIRE_NOOP_CALIBRATION=true but STRESS_NOOP_ENDPOINT is unset",
      );
    }
    return { enabled: false, required: false };
  }
  const calibrationRate = targetRateTps * calibrationHeadroomMultiplier;
  const count = Math.max(
    1,
    Math.ceil(calibrationRate * calibrationDurationSec),
  );
  const rows = await collectCalibrationRows(cursors, count);
  const warmupRequestCount = Math.min(
    httpConnections,
    submitConcurrency,
    rows.length,
  );
  const warmupResults = await Promise.all(
    rows.slice(0, warmupRequestCount).map((txHex) =>
      submitTxHex(txHex, {
        retryLimit: 0,
        endpoint: noOpEndpoint,
        calibrationMode: true,
      }),
    ),
  );
  const warmupFailures = warmupResults.filter((result) => !result.ok).length;
  if (warmupFailures > 0) {
    throw new Error(
      `no-op calibration warmup failed: ${warmupFailures.toString()}/${warmupRequestCount.toString()} requests`,
    );
  }
  const startedAtDate = new Date();
  const startedAtMs = Date.now();
  const startedPerfMs = performance.now();
  const intervalMs = 1000 / calibrationRate;
  const cpuBefore = process.cpuUsage();
  const eluBefore = performance.eventLoopUtilization();
  const scheduleSlipMs = [];
  let submittedCount = 0;
  let failedCount = 0;

  writeEngineEvent("calibration_started", {
    endpoint: noOpEndpoint,
    targetRateTps,
    calibrationRateTps: calibrationRate,
    count,
  });

  const schedule = await runDeadlineBatchedSchedule({
    totalStarts: rows.length,
    startedAtPerfMs: startedPerfMs,
    deadlinePerfMs: startedPerfMs + calibrationDurationSec * 1000,
    intervalMs,
    maxInFlight: submitConcurrency,
    allowPostDeadlineCatchUp: true,
    dispatchStart: ({ startIndex, scheduledAtPerfMs }) => {
      const submittedPerfMs = performance.now();
      scheduleSlipMs.push(Math.max(0, submittedPerfMs - scheduledAtPerfMs));
      return submitTxHex(rows[startIndex], {
        retryLimit: 0,
        endpoint: noOpEndpoint,
        calibrationMode: true,
      })
        .then((result) => {
          if (result.ok) {
            submittedCount += 1;
          } else {
            failedCount += 1;
          }
        })
        .catch(() => {
          failedCount += 1;
        });
    },
  });
  const finishedAtDate = new Date();
  const finishedAtMs = Date.now();
  const elapsedMs = Math.max(1, finishedAtMs - startedAtMs);
  const cpuAfter = process.cpuUsage(cpuBefore);
  const eluAfter = performance.eventLoopUtilization(eluBefore);
  const schedulingElapsedMs = Math.max(
    1,
    (schedule.lastDispatchedAtPerfMs ?? startedPerfMs) - startedPerfMs,
  );
  const achievedRateTps = submittedCount / (schedulingElapsedMs / 1000);
  // The hard count and zero-miss checks still require every calibration start.
  // Apply the formal scheduler's 0.1% wall-clock tolerance to the rate only so
  // a final coarse timer wake does not make an otherwise exact 2x run fail.
  const minRequiredRateTps =
    targetRateTps * calibrationHeadroomMultiplier * (1 - missedStartMaxRatio);
  const p95ScheduleSlipLimitMs = scheduleLagP95MaxMs;
  const p99ScheduleSlipLimitMs = scheduleLagP99MaxMs;
  const slip = summarizeScheduleSlip(scheduleSlipMs);
  const passed =
    achievedRateTps >= minRequiredRateTps &&
    slip.p95 <= p95ScheduleSlipLimitMs &&
    slip.p99 <= p99ScheduleSlipLimitMs &&
    schedule.missedStarts === 0 &&
    failedCount === 0;
  const summary = {
    warmupRequestCount,
    warmupFailures,
    offeredCount: rows.length,
    submittedCount,
    failedCount,
    targetRateTps: calibrationRate,
    maxInFlight: submitConcurrency,
    maxObservedInFlight: schedule.maxObservedInFlight,
    scheduledStarts: schedule.scheduledStarts,
    missedStarts: schedule.missedStarts,
    startedAtIso: startedAtDate.toISOString(),
    finishedAtIso: finishedAtDate.toISOString(),
    durationMs: elapsedMs,
    schedulingDurationMs: schedulingElapsedMs,
    achievedRateTps,
    submittedOfferedRatio: rows.length === 0 ? 0 : submittedCount / rows.length,
    scheduleSlipMs: slip,
    endpoint: noOpEndpoint,
    minRequiredRateTps,
    p95ScheduleSlipLimitMs,
    p99ScheduleSlipLimitMs,
    passed,
    eventLoopUtilization: eluAfter.utilization,
    cpuUserMicros: cpuAfter.user,
    cpuSystemMicros: cpuAfter.system,
    notes: passed ? [] : ["no_op_calibration_gate_failed"],
  };
  fs.mkdirSync(path.dirname(noOpCalibrationPath), { recursive: true });
  fs.writeFileSync(
    noOpCalibrationPath,
    `${JSON.stringify(summary, null, 2)}\n`,
  );
  writeEngineEvent("calibration_finished", { calibration: summary });
  if (requireNoOpCalibration && !passed) {
    throw new Error("no-op calibration failed required gate");
  }
  return summary;
};

const waitForStageDrain = async (stage) => {
  const startedAt = Date.now();
  let lastCounters = stage.counterEnd ?? (await readCounters());
  while (Date.now() - startedAt <= drainTimeoutSec * 1000) {
    const acceptedDelta = counterDelta(
      stage.counterStart,
      lastCounters,
      "accept",
    );
    const rejectedDelta = counterDelta(
      stage.counterStart,
      lastCounters,
      "reject",
    );
    const commitTxDelta = counterDelta(
      stage.counterStart,
      lastCounters,
      "commitBlockTx",
    );
    const commitBlockDelta = counterDelta(
      stage.counterStart,
      lastCounters,
      "commitBlock",
    );
    const mergeBlockDelta = counterDelta(
      stage.counterStart,
      lastCounters,
      "mergeBlock",
    );
    const settled = isDrainComplete({
      submitted: stage.submitted,
      acceptedDelta,
      rejectedDelta,
    });
    const committed = !waitForCommit || commitTxDelta >= acceptedDelta;
    const merged =
      !waitForMerge || mergeBlockDelta >= Math.max(1, commitBlockDelta);

    if (settled && committed && merged) {
      stage.drainCounters = lastCounters;
      stage.drain = {
        completed: true,
        elapsedMs: Date.now() - startedAt,
        acceptedDelta,
        rejectedDelta,
        commitTxDelta,
        commitBlockDelta,
        mergeBlockDelta,
      };
      return stage.drain;
    }

    await sleep(metricsPollMs);
    lastCounters = await readCounters();
  }

  const acceptedDelta = counterDelta(
    stage.counterStart,
    lastCounters,
    "accept",
  );
  const rejectedDelta = counterDelta(
    stage.counterStart,
    lastCounters,
    "reject",
  );
  stage.drainCounters = lastCounters;
  stage.drain = {
    completed: false,
    elapsedMs: Date.now() - startedAt,
    acceptedDelta,
    rejectedDelta,
    commitTxDelta: counterDelta(
      stage.counterStart,
      lastCounters,
      "commitBlockTx",
    ),
    commitBlockDelta: counterDelta(
      stage.counterStart,
      lastCounters,
      "commitBlock",
    ),
    mergeBlockDelta: counterDelta(
      stage.counterStart,
      lastCounters,
      "mergeBlock",
    ),
  };
  return stage.drain;
};

const collectStatusLatencies = async (stage) => {
  if (stage.statusSampleTxIds.length === 0) {
    return;
  }
  await Promise.all(
    stage.statusSampleTxIds.map(async (txIdHex) => {
      const submittedAt = stage.submittedAtByTxId.get(txIdHex);
      if (submittedAt === undefined) {
        return;
      }
      const startedAt = Date.now();
      while (Date.now() - startedAt <= drainTimeoutSec * 1000) {
        const status = await fetchTxStatus(txIdHex);
        if (terminalStatuses.has(status)) {
          if (acceptedStatuses.has(status)) {
            stage.statusLatencyMs.push(Date.now() - submittedAt);
          }
          return;
        }
        await sleep(txStatusRetryDelayMs);
      }
    }),
  );
};

const startCounterMonitor = async (phaseNameRef) => {
  const samples = [];
  let stopped = false;
  const firstCounters = await readCounters();
  let prev = firstCounters;
  let prevTs = Date.now();
  samples.push({
    timestampMs: prevTs,
    phase: phaseNameRef.current,
    counters: firstCounters,
  });

  const loop = (async () => {
    while (!stopped) {
      await sleep(metricsPollMs);
      const now = await readCounters();
      const nowTs = Date.now();
      const dt = (nowTs - prevTs) / 1000;
      samples.push({
        timestampMs: nowTs,
        phase: phaseNameRef.current,
        counters: now,
      });
      writeEngineEvent("counter_sample", {
        timestampMs: nowTs,
        phase: phaseNameRef.current,
        counters: now,
      });
      if (dt > 0) {
        const submitRate = (now.submit - prev.submit) / dt;
        const acceptRate = (now.accept - prev.accept) / dt;
        const rejectRate = (now.reject - prev.reject) / dt;
        const commitTxRate = (now.commitBlockTx - prev.commitBlockTx) / dt;
        const mergeRate = (now.mergeBlock - prev.mergeBlock) / dt;
        console.log(
          `phase=${phaseNameRef.current} rate_submit=${submitRate.toFixed(2)} rate_accept=${acceptRate.toFixed(2)} rate_reject=${rejectRate.toFixed(2)} rate_commit_tx=${commitTxRate.toFixed(2)} rate_merge=${mergeRate.toFixed(2)} queue=${now.validationQueueDepth} mempool=${now.mempoolTx}`,
        );
      }
      prev = now;
      prevTs = nowTs;
    }
  })();

  return {
    samples,
    async stop() {
      stopped = true;
      await loop;
    },
  };
};

const createRuntimeSampler = () => {
  const eventLoopDelay = monitorEventLoopDelay({ resolution: 20 });
  const gcDurations = [];
  let observer = null;
  try {
    observer = new PerformanceObserver((list) => {
      for (const entry of list.getEntries()) {
        gcDurations.push(entry.duration);
      }
    });
    observer.observe({ entryTypes: ["gc"] });
  } catch {
    observer = null;
  }
  eventLoopDelay.enable();
  const startElu = performance.eventLoopUtilization();
  const startCpu = process.cpuUsage();
  const startMemory = process.memoryUsage();
  const startedAtMs = Date.now();

  return {
    stop() {
      const endElu = performance.eventLoopUtilization(startElu);
      const endCpu = process.cpuUsage(startCpu);
      const endMemory = process.memoryUsage();
      eventLoopDelay.disable();
      if (observer !== null) {
        observer.disconnect();
      }
      return {
        startedAtMs,
        endedAtMs: Date.now(),
        eventLoopUtilization: endElu.utilization,
        eventLoopDelayMs: {
          min: eventLoopDelay.min / 1e6,
          mean: eventLoopDelay.mean / 1e6,
          p50: eventLoopDelay.percentile(50) / 1e6,
          p95: eventLoopDelay.percentile(95) / 1e6,
          p99: eventLoopDelay.percentile(99) / 1e6,
          max: eventLoopDelay.max / 1e6,
        },
        cpuUsageMicros: endCpu,
        memoryStart: startMemory,
        memoryEnd: endMemory,
        gcPauseMs: summarizeLatency(gcDurations),
      };
    },
  };
};

const maybeStartPyroscope = async () => {
  if (!pyroscopeEnabled) {
    return { enabled: false };
  }
  try {
    const pyroscope = await import("@pyroscope/nodejs");
    const client = pyroscope.default ?? pyroscope;
    client.init({
      serverAddress:
        process.env.PYROSCOPE_SERVER_ADDRESS ?? "http://pyroscope:4040",
      appName:
        process.env.PYROSCOPE_APPLICATION_NAME ?? "midgard-node-benchmark",
      tags: {
        benchmark: "l2-throughput",
        mode: benchmarkMode,
      },
    });
    client.start();
    return { enabled: true, mode: "pyroscope" };
  } catch (error) {
    return {
      enabled: false,
      error: `Pyroscope requested but @pyroscope/nodejs could not be started: ${error instanceof Error ? error.message : String(error)}`,
    };
  }
};

const readPgStatStatements = async (env, label) => {
  if (!pgStatStatementsEnabled) {
    return { enabled: false };
  }
  const host = process.env.POSTGRES_HOST ?? env.POSTGRES_HOST ?? "localhost";
  const port = process.env.POSTGRES_PORT ?? env.POSTGRES_PORT ?? "5432";
  const user = process.env.POSTGRES_USER ?? env.POSTGRES_USER ?? "postgres";
  const database = process.env.POSTGRES_DB ?? env.POSTGRES_DB ?? "midgard";
  const password =
    process.env.POSTGRES_PASSWORD ?? env.POSTGRES_PASSWORD ?? "postgres";
  const query = `
    select queryid,calls,total_exec_time,mean_exec_time,rows,
           left(regexp_replace(query, '\\s+', ' ', 'g'), 240) as query
    from pg_stat_statements
    order by total_exec_time desc
    limit 20
  `;
  try {
    const { stdout } = await execFileAsync(
      "psql",
      [
        "-h",
        host,
        "-p",
        String(port),
        "-U",
        user,
        "-d",
        database,
        "-At",
        "-F",
        "\t",
        "-c",
        query,
      ],
      {
        env: {
          ...process.env,
          PGPASSWORD: password,
        },
        timeout: 10_000,
      },
    );
    return {
      enabled: true,
      label,
      rows: stdout
        .trim()
        .split("\n")
        .filter((line) => line.length > 0)
        .map((line) => {
          const [queryid, calls, totalExecTime, meanExecTime, rows, queryText] =
            line.split("\t");
          return {
            queryid,
            calls: Number(calls),
            totalExecTimeMs: Number(totalExecTime),
            meanExecTimeMs: Number(meanExecTime),
            rows: Number(rows),
            query: queryText,
          };
        }),
    };
  } catch (error) {
    return {
      enabled: true,
      label,
      error: error instanceof Error ? error.message : String(error),
    };
  }
};

const regularFilesUnder = (relativeDirectory) => {
  const root = path.resolve(pkgRoot, relativeDirectory);
  if (!fs.existsSync(root)) return [];
  const files = [];
  const visit = (absoluteDirectory) => {
    for (const entry of fs.readdirSync(absoluteDirectory, {
      withFileTypes: true,
    })) {
      const absolute = path.join(absoluteDirectory, entry.name);
      if (entry.isDirectory()) visit(absolute);
      else if (entry.isFile()) {
        files.push(path.relative(pkgRoot, absolute).replaceAll(path.sep, "/"));
      }
    }
  };
  visit(root);
  return files;
};

const updateFramedHash = (hash, relativePath, bytes) => {
  const pathBytes = Buffer.from(relativePath);
  const lengths = Buffer.allocUnsafe(12);
  lengths.writeUInt32LE(pathBytes.length, 0);
  lengths.writeBigUInt64LE(BigInt(bytes.length), 4);
  hash.update(lengths).update(pathBytes).update(bytes);
};

const readSourceTreeIdentity = () => {
  const sourceFiles = [
    "../pnpm-lock.yaml",
    "../lucid-midgard/package.json",
    "../midgard-core/package.json",
    "../midgard-sdk/package.json",
    "../midgard-validation/package.json",
    ".env.example",
    ".env.benchmark",
    "Dockerfile",
    "docker-compose.benchmark.yaml",
    "docker-compose.kupmios.yaml",
    "docker-compose.yaml",
    "package.json",
    "native/mpf-event-flat-wasm/Cargo.lock",
    "native/mpf-event-flat-wasm/Cargo.toml",
    "tsconfig.json",
    "tsup.config.ts",
    "scripts/stress.benchmark.env",
    ...regularFilesUnder("src"),
    ...regularFilesUnder("scripts"),
    ...regularFilesUnder("native/mpf-event-flat-wasm/src"),
    ...regularFilesUnder("../patches"),
    ...regularFilesUnder("../lucid-midgard/src"),
    ...regularFilesUnder("../midgard-core/src"),
    ...regularFilesUnder("../midgard-sdk/src"),
    ...regularFilesUnder("../midgard-validation/src"),
  ]
    .filter((relativePath) =>
      fs.existsSync(path.resolve(pkgRoot, relativePath)),
    )
    .sort();
  const hash = createHash("sha256");
  for (const relativePath of sourceFiles) {
    updateFramedHash(
      hash,
      relativePath,
      fs.readFileSync(path.resolve(pkgRoot, relativePath)),
    );
  }
  return {
    sourceTreeSha256: hash.digest("hex"),
    sourceTreeFileCount: sourceFiles.length,
  };
};

const readGitMetadata = async () => {
  try {
    const [
      { stdout: commitStdout },
      { stdout: statusStdout },
      { stdout: diffStdout },
    ] = await Promise.all([
      execFileAsync("git", ["rev-parse", "HEAD"], { cwd: pkgRoot }),
      execFileAsync("git", ["status", "--short"], { cwd: pkgRoot }),
      execFileAsync(
        "git",
        ["diff", "--binary", "--no-ext-diff", "HEAD", "--"],
        { cwd: pkgRoot, maxBuffer: 64 * 1024 * 1024 },
      ),
    ]);
    const statusShort = statusStdout.trim().split("\n").filter(Boolean);
    return {
      commit: commitStdout.trim(),
      dirty: statusStdout.trim().length > 0,
      statusShort,
      statusSha256: createHash("sha256")
        .update(`${statusShort.join("\n")}\n`)
        .digest("hex"),
      trackedDiffSha256: createHash("sha256").update(diffStdout).digest("hex"),
      benchmarkScriptSha256: createHash("sha256")
        .update(fs.readFileSync(__filename))
        .digest("hex"),
      ...readSourceTreeIdentity(),
    };
  } catch (error) {
    return {
      commit: null,
      dirty: null,
      error: error instanceof Error ? error.message : String(error),
    };
  }
};

const readRuntimeMetadata = async () => ({
  nodeVersion: process.version,
  platform: process.platform,
  arch: process.arch,
  hostname: os.hostname(),
  cpuModel: os.cpus()[0]?.model ?? null,
  cpuCount: os.cpus().length,
  totalMemoryBytes: os.totalmem(),
  freeMemoryBytes: os.freemem(),
  loadAverage: os.loadavg(),
  pid: process.pid,
  argv: process.argv,
  env: {
    NODE_ENV: process.env.NODE_ENV ?? null,
  },
});

const runClientSelfCheck = async () => {
  if (!clientSelfCheckEnabled) {
    return { enabled: false, required: false };
  }
  const baseRate =
    Number.isFinite(targetAcceptedTps) && targetAcceptedTps > 0
      ? targetAcceptedTps
      : Number.isFinite(openLoopRate) && openLoopRate > 0
        ? openLoopRate
        : 100;
  const targetRate = Math.max(1, baseRate * clientSelfCheckMultiplier);
  const intervalMs = 1000 / targetRate;
  const endpoint =
    noOpEndpoint === null
      ? `${submitEndpoint}/readyz`
      : `${noOpEndpoint}/submit`;
  const requestOptions =
    noOpEndpoint === null
      ? undefined
      : {
          method: "POST",
          headers: { "content-type": "application/cbor" },
          body: Buffer.from([0]),
        };
  const warmupRequestCount = Math.min(httpConnections, submitConcurrency);
  const warmupResponses = await Promise.all(
    Array.from({ length: warmupRequestCount }, () =>
      httpClient.request(endpoint, requestOptions),
    ),
  );
  const warmupFailures = warmupResponses.filter(
    (response) => response.ok !== true,
  ).length;
  if (warmupFailures > 0) {
    throw new Error(
      `benchmark client self-check warmup failed: ${warmupFailures}/${warmupRequestCount} requests`,
    );
  }
  const startedAt = performance.now();
  const deadline = startedAt + clientSelfCheckDurationSec * 1000;
  let ok = 0;
  let failed = 0;
  const latencies = [];
  const totalStarts = Math.ceil(
    (clientSelfCheckDurationSec * 1000) / intervalMs,
  );
  const schedule = await runDeadlineBatchedSchedule({
    totalStarts,
    startedAtPerfMs: startedAt,
    deadlinePerfMs: deadline,
    intervalMs,
    maxInFlight: submitConcurrency,
    dispatchStart: () =>
      httpClient
        .request(endpoint, requestOptions)
        .then((resp) => {
          latencies.push(resp.latencyMs);
          if (resp.ok) {
            ok += 1;
          } else {
            failed += 1;
          }
        })
        .catch(() => {
          failed += 1;
        }),
  });
  const elapsedSec = (performance.now() - startedAt) / 1000;
  const achievedRate = ok / elapsedSec;
  const result = {
    enabled: true,
    required: clientSelfCheckRequired,
    endpoint,
    warmupRequestCount,
    warmupFailures,
    targetRate,
    minRequiredRate: targetRate * clientSelfCheckMinRatio,
    achievedRate,
    scheduled: schedule.scheduledStarts,
    missed: schedule.missedStarts,
    maxObservedInFlight: schedule.maxObservedInFlight,
    ok,
    failed,
    elapsedSec,
    latencyMs: summarizeLatency(latencies),
  };
  if (
    clientSelfCheckRequired &&
    achievedRate < targetRate * clientSelfCheckMinRatio
  ) {
    throw new Error(
      `benchmark client self-check failed: achieved ${achievedRate.toFixed(2)} req/s < required ${(targetRate * clientSelfCheckMinRatio).toFixed(2)} req/s`,
    );
  }
  return result;
};

const summarizeCursorContinuity = (stage, checkpoint) => {
  const start = stage.cursorPositionsAtStart;
  const middle = checkpoint?.cursorPositions;
  const end = stage.cursorPositionsAtEnd;
  if (!Array.isArray(start) || !Array.isArray(middle) || !Array.isArray(end)) {
    return {
      passed: false,
      reason: "cursor position snapshot missing",
    };
  }
  if (start.length !== middle.length || middle.length !== end.length) {
    return {
      passed: false,
      reason: `cursor count changed start=${start.length} checkpoint=${middle.length} end=${end.length}`,
    };
  }
  for (let index = 0; index < start.length; index += 1) {
    const initial = start[index];
    const observed = middle[index];
    const final = end[index];
    if (
      initial.chainIndex !== observed.chainIndex ||
      observed.chainIndex !== final.chainIndex
    ) {
      return {
        passed: false,
        reason: `cursor chain order changed at ordinal=${index}`,
      };
    }
    if (
      initial.nextIndex > observed.nextIndex ||
      observed.nextIndex > final.nextIndex
    ) {
      return {
        passed: false,
        reason: `cursor regressed chain=${initial.chainIndex} start=${initial.nextIndex} checkpoint=${observed.nextIndex} end=${final.nextIndex}`,
      };
    }
  }
  const total = (positions) =>
    positions.reduce((sum, position) => sum + position.nextIndex, 0);
  return {
    passed: true,
    reason: null,
    cursorCount: start.length,
    startConsumedRows: total(start),
    checkpointConsumedRows: total(middle),
    endConsumedRows: total(end),
    startPositionsSha256: cursorPositionDigest(start),
    checkpointPositionsSha256: cursorPositionDigest(middle),
    endPositionsSha256: cursorPositionDigest(end),
    checkpointMode: "observer_only_no_cursor_mutation",
  };
};

const buildPhase1StageAWindowGate = (stage) => {
  if (!phase1StageAWindowGateEnabled) {
    return { enabled: false };
  }
  const checkpoint = stage.phase1StageACheckpoint;
  const checkpointAvailable =
    checkpoint?.counters !== null && checkpoint?.counters !== undefined;
  const measuredDurationSec =
    checkpoint?.observedAtMs === undefined
      ? Number.NaN
      : (checkpoint.observedAtMs - stage.startedAtMs) / 1_000;
  const submitLatencySampleCount = checkpoint?.submitLatencySampleCount ?? 0;
  const scheduleLagSampleCount = checkpoint?.scheduleLagSampleCount ?? 0;
  const statusCounts = checkpoint?.submitStatusCounts ?? {};
  const { durablyAdmitted, duplicateSuccesses, otherSuccesses } =
    summarizeSubmitSuccessStatuses(statusCounts);
  const acceptedDelta = checkpointAvailable
    ? counterDelta(stage.counterStart, checkpoint.counters, "accept")
    : 0;
  const rejectedDelta = checkpointAvailable
    ? counterDelta(stage.counterStart, checkpoint.counters, "reject")
    : 0;
  const missingRequiredMetrics = checkpointAvailable
    ? metricMissingKeys(checkpoint.counters)
    : [];
  const gate = summarizePhase1StageAWindowGate({
    checkpointAvailable,
    checkpointError: checkpoint?.error ?? null,
    checkpointRequestedAfterMs:
      checkpoint?.checkpointRequestedAtMs === undefined
        ? Number.NaN
        : checkpoint.checkpointRequestedAtMs - stage.startedAtMs,
    checkpointObservedAfterMs:
      checkpoint?.observedAtMs === undefined
        ? Number.NaN
        : checkpoint.observedAtMs - stage.startedAtMs,
    checkpointMaxJitterMs: phase1StageACheckpointMaxJitterMs,
    measuredDurationSec,
    minDurationSec: phase1StageAWindowSec,
    targetRateTps: Number(stage.targetRateTps ?? 0),
    durablyAdmitted,
    acceptedDelta,
    rejectedDelta,
    duplicateSuccesses,
    otherSuccesses,
    submitErrors: checkpoint?.submitErrors ?? 0,
    queueFullResponses: checkpoint?.queueFullResponses ?? 0,
    submitLatencyMs: summarizeLatency(
      stage.submitLatencyMs.slice(0, submitLatencySampleCount),
    ),
    scheduleLagMs: summarizeLatency(
      stage.scheduleLagMs.slice(0, scheduleLagSampleCount),
    ),
    scheduledStarts: checkpoint?.scheduledStarts ?? 0,
    missedStarts: checkpoint?.missedStarts ?? 0,
    offeredRateMinRatio,
    acceptedRateMinRatio,
    submitLatencyP99MaxMs,
    scheduleLagP95MaxMs,
    scheduleLagP99MaxMs,
    missedStartMaxRatio,
    missingRequiredMetrics,
    streamContinuity: summarizeCursorContinuity(stage, checkpoint),
  });
  return {
    ...gate,
    stageName: stage.name,
    stageStartedAtIso: new Date(stage.startedAtMs).toISOString(),
    checkpointRequestedAtIso:
      checkpoint?.checkpointRequestedAtMs === undefined
        ? null
        : new Date(checkpoint.checkpointRequestedAtMs).toISOString(),
    checkpointObservedAtIso:
      checkpoint?.observedAtMs === undefined
        ? null
        : new Date(checkpoint.observedAtMs).toISOString(),
    stageEndedAtIso:
      stage.endedAtMs === null ? null : new Date(stage.endedAtMs).toISOString(),
    submitStatusCounts: statusCounts,
    physicalSubmitStatusCounts: checkpoint?.physicalSubmitStatusCounts ?? {},
    logicalSubmitAttempts: checkpoint?.logicalSubmitAttempts ?? 0,
    physicalSubmitAttempts: checkpoint?.physicalSubmitAttempts ?? 0,
    submittedSuccesses: checkpoint?.submitted ?? 0,
    sentStarts: checkpoint?.sentStarts ?? 0,
    checkpointSource:
      "pre-request client/cursor snapshot plus asynchronous Prometheus counter snapshot inside one continuous open-loop stage; elapsed denominator ends at counter response for conservative rates",
  };
};

const buildStageReport = (stage) => {
  const measuredElapsedMs = Math.max(1, stage.endedAtMs - stage.startedAtMs);
  const endCounters = stage.counterEnd ?? stage.counterStart;
  const drainCounters = stage.drainCounters ?? endCounters;
  const acceptedDelta = counterDelta(stage.counterStart, endCounters, "accept");
  const rejectedDelta = counterDelta(stage.counterStart, endCounters, "reject");
  const commitTxDelta = counterDelta(
    stage.counterStart,
    endCounters,
    "commitBlockTx",
  );
  const commitBlockDelta = counterDelta(
    stage.counterStart,
    endCounters,
    "commitBlock",
  );
  const mergeBlockDelta = counterDelta(
    stage.counterStart,
    endCounters,
    "mergeBlock",
  );
  const drainAcceptedDelta = counterDelta(
    stage.counterStart,
    drainCounters,
    "accept",
  );
  const drainRejectedDelta = counterDelta(
    stage.counterStart,
    drainCounters,
    "reject",
  );
  return {
    name: stage.name,
    mode: stage.mode,
    targetRateTps: stage.targetRateTps,
    startedAtIso: new Date(stage.startedAtMs).toISOString(),
    endedAtIso: new Date(stage.endedAtMs).toISOString(),
    measuredElapsedSec: measuredElapsedMs / 1000,
    logicalSubmitAttempts: stage.logicalSubmitAttempts,
    physicalSubmitAttempts: stage.physicalSubmitAttempts,
    submitted: stage.submitted,
    submitErrors: stage.submitErrors,
    submitStatusCounts: stage.submitStatusCounts,
    physicalSubmitStatusCounts: stage.physicalSubmitStatusCounts,
    queueFullResponses: stage.queueFullResponses,
    firstErrors: stage.firstErrors,
    scheduledStarts: stage.scheduledStarts,
    sentStarts: stage.sentStarts,
    missedStarts: stage.missedStarts,
    missedStartRatio:
      stage.scheduledStarts + stage.missedStarts > 0
        ? stage.missedStarts / (stage.scheduledStarts + stage.missedStarts)
        : 0,
    inFlightHighWater: stage.inFlightHighWater,
    bytesSent: stage.bytesSent,
    measuredWindow: summarizeCounterWindow({
      startCounters: stage.counterStart,
      endCounters,
      elapsedMs: measuredElapsedMs,
      counterKeys: [
        "submit",
        "accept",
        "reject",
        "commitBlock",
        "commitBlockTx",
        "mergeBlock",
      ],
    }),
    measuredAcceptedTps: rateBetweenCounters(
      stage.counterStart,
      endCounters,
      "accept",
      measuredElapsedMs,
    ),
    measuredSubmittedTps: rateBetweenCounters(
      stage.counterStart,
      endCounters,
      "submit",
      measuredElapsedMs,
    ),
    physicalSubmitAttemptsPerSec:
      stage.physicalSubmitAttempts / (measuredElapsedMs / 1000),
    queuedSubmitSuccessPerSec: stage.submitted / (measuredElapsedMs / 1000),
    acceptedDelta,
    rejectedDelta,
    commitBlockDelta,
    commitTxDelta,
    mergeBlockDelta,
    drainAcceptedDelta,
    drainRejectedDelta,
    missingRequiredMetrics: requireMetricPresence
      ? metricMissingKeys(endCounters)
      : [],
    drain: stage.drain,
    submitLatencyMs: summarizeLatency(stage.submitLatencyMs),
    submitAttemptLatencyMs: summarizeLatency(stage.submitAttemptLatencyMs),
    scheduleLagMs: summarizeLatency(stage.scheduleLagMs),
    scheduleLagSamplesMs: stage.scheduleLagMs,
    statusLatencyMs: summarizeLatency(stage.statusLatencyMs),
    abortedCorpusExhausted: stage.abortedCorpusExhausted === true,
    phase1StageAWindowGate: buildPhase1StageAWindowGate(stage),
    phase4Metrics: {
      speculationHitDelta: counterDelta(
        stage.counterStart,
        endCounters,
        "speculationHit",
      ),
      speculationInvalidationDelta: counterDelta(
        stage.counterStart,
        endCounters,
        "speculationInvalidations",
      ),
      histograms: {
        commitCadenceMs: summarizeHistogramDelta(
          stage.counterStart.histograms?.commitCadenceMs,
          endCounters.histograms?.commitCadenceMs,
        ),
        speculativeBuildDurationMs: summarizeHistogramDelta(
          stage.counterStart.histograms?.speculativeBuildDurationMs,
          endCounters.histograms?.speculativeBuildDurationMs,
        ),
        submitAfterConfirmMs: summarizeHistogramDelta(
          stage.counterStart.histograms?.submitAfterConfirmMs,
          endCounters.histograms?.submitAfterConfirmMs,
        ),
        confirmationDetectionLagMs: summarizeHistogramDelta(
          stage.counterStart.histograms?.confirmationDetectionLagMs,
          endCounters.histograms?.confirmationDetectionLagMs,
        ),
        l1ConfirmationWaitMs: summarizeHistogramDelta(
          stage.counterStart.histograms?.l1ConfirmationWaitMs,
          endCounters.histograms?.l1ConfirmationWaitMs,
        ),
      },
    },
  };
};

const activeCursorCount = (cursors) =>
  cursors.filter(
    (cursor) =>
      !cursor.stopped &&
      cursor.nextIndex < (cursor.entry?.rowCount ?? cursor.chain.txs.length),
  ).length;

const assertCandidateCapacity = ({
  cursors,
  targetRateTps,
  durationSec,
  priorSubmitLatencyP99Ms,
}) => {
  const availableTxs = remainingTxCount(cursors);
  const requiredTxs = Math.ceil(targetRateTps * durationSec * 1.02);
  if (availableTxs < requiredTxs) {
    throw new Error(
      `candidate requires about ${requiredTxs} txs but only ${availableTxs} prebuilt txs remain`,
    );
  }
  const chainCount = activeCursorCount(cursors);
  if (chainCount <= 0) {
    throw new Error("candidate has no active chains");
  }
  if (priorSubmitLatencyP99Ms !== null && priorSubmitLatencyP99Ms > 0) {
    const chainCapacityTps = chainCount / (priorSubmitLatencyP99Ms / 1000);
    if (chainCapacityTps < targetRateTps * offeredRateMinRatio) {
      throw new Error(
        `candidate target ${targetRateTps} TPS exceeds one-in-flight chain capacity estimate ${chainCapacityTps.toFixed(2)} TPS (chains=${chainCount}, prior_submit_p99_ms=${priorSubmitLatencyP99Ms.toFixed(2)})`,
      );
    }
  }
};

const estimateRequiredCorpusRows = () => {
  const warmupRows = Math.max(0, warmupTxs);
  if (benchmarkMode === "open") {
    return warmupRows + Math.ceil(openLoopRate * measuredSec * 1.02);
  }
  if (benchmarkMode === "ramp") {
    let rows = warmupRows;
    for (
      let targetRate = rampStartTps;
      targetRate <= rampMaxTps;
      targetRate += rampStepTps
    ) {
      rows += Math.ceil(targetRate * rampStageSec * 1.02);
    }
    return rows;
  }
  if (benchmarkMode === "find-max") {
    const exploratoryCandidates =
      Math.floor((rampMaxTps - rampStartTps) / rampStepTps) + 1;
    const candidateCount = Math.min(
      findMaxMaxCandidates,
      Math.max(1, exploratoryCandidates) +
        findMaxBinaryIterations +
        3 * findMaxRepeats,
    );
    const longestDuration = Math.max(rampStageSec, findMaxConfirmationSec);
    return (
      warmupRows +
      Math.ceil(rampMaxTps * longestDuration * candidateCount * 1.02)
    );
  }
  return warmupRows + Math.ceil(targetAcceptedTps * measuredSec * 1.02);
};

const hasCounterActivity = (before, after) =>
  [
    "submit",
    "accept",
    "reject",
    "commitBlock",
    "commitBlockTx",
    "mergeBlock",
  ].some((key) => counterDelta(before, after, key) !== 0);

const waitForCandidateCleanliness = async (label) => {
  const startedAt = Date.now();
  let lastCounters = await readCounters();
  while (Date.now() - startedAt <= candidateCleanTimeoutSec * 1000) {
    const missing = requireMetricPresence
      ? metricMissingKeys(lastCounters, ["validationQueueDepth"])
      : [];
    if (missing.length > 0) {
      throw new Error(
        `required metrics missing before ${label}: ${missing.join(",")}`,
      );
    }
    const queueClean = Number(lastCounters.validationQueueDepth ?? 0) === 0;
    const localFinalizationClean =
      !waitForCommit ||
      Number(lastCounters.unconfirmedSubmittedBlockPending ?? 0) === 0;
    const mergeClean =
      !waitForMerge || Number(lastCounters.blocksInQueue ?? 0) === 0;
    if (queueClean && localFinalizationClean && mergeClean) {
      if (requireIdleNode && idleProbeSec > 0) {
        await sleep(idleProbeSec * 1000);
        const afterIdleProbe = await readCounters();
        if (hasCounterActivity(lastCounters, afterIdleProbe)) {
          throw new Error(
            `node was not idle before ${label}; global Prometheus counters changed during idle probe`,
          );
        }
        return afterIdleProbe;
      }
      return lastCounters;
    }
    await sleep(metricsPollMs);
    lastCounters = await readCounters();
  }
  throw new Error(
    `node did not reach a clean candidate window before ${label} within ${candidateCleanTimeoutSec}s`,
  );
};

const evaluateStagePass = ({ stage, stageReport, monitorSamples }) => {
  const reasons = [];
  const submitSuccessGate = summarizeSubmitSuccessStatuses(
    stageReport.submitStatusCounts,
  );
  const measuredSamples = monitorSamples.filter(
    (sample) =>
      sample.timestampMs >= stage.startedAtMs &&
      sample.timestampMs <= stage.endedAtMs,
  );
  const backlogSlope = gaugeSlopePerSec(
    measuredSamples.length >= 2 ? measuredSamples : monitorSamples,
    "validationQueueDepth",
  );
  const targetRate = Number(stageReport.targetRateTps ?? 0);
  const offeredRate = stageReport.queuedSubmitSuccessPerSec;
  const acceptedRate = stageReport.measuredAcceptedTps;
  const nodeSaturationRatio =
    acceptedRate > 0 ? offeredRate / acceptedRate : null;
  if (stageReport.missingRequiredMetrics.length > 0) {
    reasons.push(
      `missing_required_metrics=${stageReport.missingRequiredMetrics.join(",")}`,
    );
  }
  if (targetRate > 0 && offeredRate < targetRate * offeredRateMinRatio) {
    reasons.push(
      `offered_rate ${offeredRate.toFixed(2)} < ${(targetRate * offeredRateMinRatio).toFixed(2)}`,
    );
  }
  if (targetRate > 0 && acceptedRate < targetRate * acceptedRateMinRatio) {
    reasons.push(
      `accepted_rate ${acceptedRate.toFixed(2)} < ${(targetRate * acceptedRateMinRatio).toFixed(2)}`,
    );
  }
  if (stageReport.submitErrors > 0) {
    reasons.push(`submit_errors=${stageReport.submitErrors}`);
  }
  if (stageReport.queueFullResponses > 0) {
    reasons.push(`queue_full_responses=${stageReport.queueFullResponses}`);
  }
  reasons.push(...submitSuccessGate.reasons);
  if (
    targetRate > 0 &&
    nodeSaturationRatio !== null &&
    nodeSaturationRatio < nodeSaturationMinRatio
  ) {
    reasons.push(
      `node_saturation_ratio ${nodeSaturationRatio.toFixed(4)} < ${nodeSaturationMinRatio}`,
    );
  }
  if (stageReport.rejectedDelta > 0 || stageReport.drainRejectedDelta > 0) {
    reasons.push(
      `unexpected_rejections measured=${stageReport.rejectedDelta} drain=${stageReport.drainRejectedDelta}`,
    );
  }
  if (stageReport.drain === null || stageReport.drain.completed !== true) {
    reasons.push("drain_not_completed");
  }
  if (
    stageReport.scheduleLagMs.p95 !== null &&
    stageReport.scheduleLagMs.p95 > scheduleLagP95MaxMs
  ) {
    reasons.push(
      `schedule_lag_p95_ms ${stageReport.scheduleLagMs.p95.toFixed(2)} > ${scheduleLagP95MaxMs}`,
    );
  }
  if (
    stageReport.scheduleLagMs.p99 !== null &&
    stageReport.scheduleLagMs.p99 > scheduleLagP99MaxMs
  ) {
    reasons.push(
      `schedule_lag_p99_ms ${stageReport.scheduleLagMs.p99.toFixed(2)} > ${scheduleLagP99MaxMs}`,
    );
  }
  if (stageReport.submitLatencyMs.p99 === null) {
    reasons.push("submit_latency_p99_ms missing");
  } else if (stageReport.submitLatencyMs.p99 > submitLatencyP99MaxMs) {
    reasons.push(
      `submit_latency_p99_ms ${stageReport.submitLatencyMs.p99.toFixed(2)} > ${submitLatencyP99MaxMs}`,
    );
  }
  if (stageReport.missedStartRatio > missedStartMaxRatio) {
    reasons.push(
      `missed_start_ratio ${stageReport.missedStartRatio.toFixed(6)} > ${missedStartMaxRatio}`,
    );
  }
  if (backlogSlope > backlogSlopeMaxPerSec) {
    reasons.push(
      `validation_queue_slope_per_sec ${backlogSlope.toFixed(4)} > ${backlogSlopeMaxPerSec}`,
    );
  }
  const phase1StarvationGate = phase1StarvationGateEnabled
    ? summarizePhase1StarvationGate({
        samples: monitorSamples,
        stageStartedAtMs: stage.startedAtMs,
        stageEndedAtMs: stage.endedAtMs,
        targetRateTps: Number(stageReport.targetRateTps ?? 0),
        overloadBaselineTps: phase1StarvationBaselineTps,
        commitTxDelta: stageReport.commitTxDelta,
        commitBlockDelta: stageReport.commitBlockDelta,
        maxAgeMultiplier: phase1StarvationMaxAgeMultiplier,
        minOverloadRatio: phase1StarvationMinOverloadRatio,
        minDurationSec: phase1StarvationMinDurationSec,
      })
    : { enabled: false };
  if (phase1StarvationGate.enabled && !phase1StarvationGate.passed) {
    reasons.push(
      ...phase1StarvationGate.reasons.map(
        (reason) => `phase1_starvation_gate: ${reason}`,
      ),
    );
  }
  const phase1StageAWindowGate = stageReport.phase1StageAWindowGate ?? {
    enabled: false,
  };
  if (phase1StageAWindowGate.enabled && !phase1StageAWindowGate.passed) {
    reasons.push(
      ...phase1StageAWindowGate.reasons.map(
        (reason) => `phase1_stage_a_window_gate: ${reason}`,
      ),
    );
  }
  const l1Observation = summarizeL1Observation(measuredSamples);
  const overlapEfficiency = summarizeLatency(
    measuredSamples
      .filter(
        (sample) =>
          sample.counters?.metricNames?.speculationOverlapEfficiency !== null,
      )
      .map((sample) => Number(sample.counters?.speculationOverlapEfficiency))
      .filter(Number.isFinite),
  );
  const speculationDenominator =
    stageReport.phase4Metrics.speculationHitDelta +
    stageReport.phase4Metrics.speculationInvalidationDelta;
  stageReport.phase4Metrics = {
    ...stageReport.phase4Metrics,
    overlapEfficiency,
    hitRate:
      speculationDenominator > 0
        ? stageReport.phase4Metrics.speculationHitDelta / speculationDenominator
        : null,
    observedBlockTxCount: (() => {
      const values = measuredSamples
        .filter(
          (sample) => sample.counters?.metricNames?.commitBlockNumTx !== null,
        )
        .map((sample) => Number(sample.counters?.commitBlockNumTx))
        .filter((value) => Number.isFinite(value) && value > 0);
      return {
        sampleCount: values.length,
        min: values.length === 0 ? null : Math.min(...values),
        max: values.length === 0 ? null : Math.max(...values),
        last: values.at(-1) ?? null,
      };
    })(),
    queueSlopesPerSec: {
      stateQueueBlocks: gaugeSlopePerSec(measuredSamples, "blocksInQueue"),
      daPublicationBacklog: gaugeSlopePerSec(
        measuredSamples,
        "daPublicationBacklog",
      ),
    },
    queueMetricPresence: {
      stateQueueBlocks: measuredSamples.some(
        (sample) => sample.counters?.metricNames?.blocksInQueue !== null,
      ),
      daPublicationBacklog: measuredSamples.some(
        (sample) => sample.counters?.metricNames?.daPublicationBacklog !== null,
      ),
    },
  };
  if (scenarioClass === "B") {
    if (
      l1Observation.startTipSlot === null ||
      l1Observation.endTipSlot === null
    ) {
      reasons.push("class_b_l1_observation: tip slots missing");
    }
    if (l1Observation.observedPreprodBlockCount < 2) {
      reasons.push(
        `class_b_l1_observation: observed block count ${l1Observation.observedPreprodBlockCount} < 2`,
      );
    }
    if (l1Observation.interBlockTimeMs.p95 === null) {
      reasons.push("class_b_l1_observation: inter-block p95 missing");
    }
  }
  const bottleneck = classifyLikelyBottleneckWithEvidence({
    submitted: stageReport.submitted,
    submitErrors: stageReport.submitErrors,
    queueFullResponses: stageReport.queueFullResponses,
    acceptedDelta: stageReport.acceptedDelta,
    rejectedDelta: stageReport.rejectedDelta,
    commitTxDelta: stageReport.commitTxDelta,
    mergeBlockDelta: stageReport.mergeBlockDelta,
    targetAcceptedTps: targetRate,
    avgAcceptedTps: stageReport.measuredAcceptedTps,
    clientSelfCheck: null,
    endCounters: stage.counterEnd ?? {},
    waitForCommit,
    waitForMerge,
    scheduleLagMs: stageReport.scheduleLagMs,
    missedStarts: stageReport.missedStarts,
    inFlightHighWater: stageReport.inFlightHighWater,
    submitConcurrency,
    backlogSlopePerSec: backlogSlope,
    requiredMetricsMissing: stageReport.missingRequiredMetrics,
  });
  return {
    passed: reasons.length === 0,
    reasons,
    backlogSlopePerSec: backlogSlope,
    nodeSaturation: {
      offeredRatePerSec: offeredRate,
      acceptedRatePerSec: acceptedRate,
      ratio: nodeSaturationRatio,
      minRatio: nodeSaturationMinRatio,
      passed:
        targetRate <= 0 ||
        nodeSaturationRatio === null ||
        nodeSaturationRatio >= nodeSaturationMinRatio,
    },
    submitSuccessGate,
    phase1StageAWindowGate,
    phase1StarvationGate,
    l1Observation,
    bottleneck,
  };
};

const writeReport = (report) => {
  fs.mkdirSync(path.dirname(reportPath), { recursive: true });
  fs.writeFileSync(reportPath, `${JSON.stringify(report, null, 2)}\n`);
};

/**
 * Runs the valid-stress throughput benchmark workload.
 */
const main = async () => {
  const runtimeSampler = createRuntimeSampler();
  const phaseNameRef = { current: "setup" };
  const phaseRecorder = createPhaseRecorder();
  const setPhase = (name) => {
    phaseNameRef.current = name;
    phaseRecorder.start(name);
  };

  setPhase("setup");
  let monitor = null;
  const corpusReaders = [];
  const openCorpusReader = (options) => {
    const reader = openStreamingCorpusReader(options);
    corpusReaders.push(reader);
    return reader;
  };

  try {
    const env = loadedStressEnv;
    const usingCorpus = corpusPath !== null;
    const wallets = usingCorpus ? [] : makeWalletsFromEnv(env);
    const minFeeA = BigInt(envValue("STRESS_MIN_FEE_A", env.MIN_FEE_A ?? "0"));
    const minFeeB = BigInt(envValue("STRESS_MIN_FEE_B", env.MIN_FEE_B ?? "0"));
    const pyroscope = await maybeStartPyroscope();
    const git = await readGitMetadata();
    const runtimeMetadata = await readRuntimeMetadata();

    engineEventWriter = makeNdjsonWriter(engineEventsPath);
    submitRecordWriter = makeNdjsonWriter(submitRecordsPath);
    writeEngineEvent("engine_started", {
      benchmarkMode,
      submitEndpoint,
      metricsEndpoint,
      corpusPath,
      corpusSliceId,
      corpusShape,
    });

    if (!usingCorpus && wallets.length === 0) {
      throw new Error(`No genesis wallet seeds found in ${envPath}`);
    }

    const configForReport = {
      envPath,
      submitEndpoint,
      metricsEndpoint,
      benchmarkMode,
      workload: usingCorpus ? "prebuilt-corpus" : "valid-native-chain",
      chainLength,
      scenarioName,
      scenarioClass,
      formalBenchmark,
      phase1FormalIdentity: null,
      loadGenerator: {
        placement: loadGeneratorPlacement,
        cohosted: loadGeneratorCohosted,
        clockOffsetMs,
        isolation: loadGeneratorIsolation,
      },
      observabilityProfile,
      maxChains: maxChains ?? "auto",
      maxChainsSetting: maxChainsSetting.raw,
      utxosPerWallet,
      minLovelace: minLovelace.toString(),
      corpus: {
        enabled: usingCorpus,
        path: corpusPath,
        indexPath: corpusIndexPath,
        manifestPath: corpusManifestPath,
        shape: corpusShape,
        sliceId: corpusSliceId,
        readAheadRows: corpusReadAheadRows,
      },
      calibration: {
        requireNoOpCalibration,
        noOpEndpoint,
        headroomMultiplier: calibrationHeadroomMultiplier,
        durationSec: calibrationDurationSec,
        noopCalibrationPath: noOpCalibrationPath,
      },
      nodeSaturationMinRatio,
      fanoutEnabled,
      fanoutMaxOutputsPerTx,
      fanoutOutputLovelace:
        fanoutOutputLovelace === null ? null : fanoutOutputLovelace.toString(),
      fanoutStatusTimeoutMs,
      retry503,
      measuredRetry503,
      retryDelayMs,
      metricsPollMs,
      observeAfterSubmitSec,
      targetAcceptedTps,
      requireFreshChains,
      measuredSec,
      phase4: {
        blockTxTarget: phase4BlockTxTarget,
        configuredCommitMaxL2TxCount,
        speculativeCommitBuild:
          String(envValue("SPECULATIVE_COMMIT_BUILD", "false"))
            .trim()
            .toLowerCase() === "true",
        environmentFingerprint: phase4EnvironmentFingerprint,
      },
      warmupTxs,
      warmupSec,
      cooldownSec,
      drainTimeoutSec,
      waitForCommit,
      waitForMerge,
      statusSampleSize,
      submitConcurrency,
      httpConnections,
      httpPipelining,
      httpTimeoutMs,
      openLoopRate,
      rampStartTps,
      rampStepTps,
      rampMaxTps,
      rampStageSec,
      rampMinAcceptedRatio,
      offeredRateMinRatio,
      acceptedRateMinRatio,
      scheduleLagP95MaxMs,
      scheduleLagP99MaxMs,
      submitLatencyP99MaxMs,
      missedStartMaxRatio,
      backlogSlopeMaxPerSec,
      phase1StageAWindowGateEnabled,
      phase1StageAWindowSec,
      phase1StageACheckpointMaxJitterMs,
      phase1StarvationGateEnabled,
      phase1StarvationMinDurationSec,
      phase1StarvationMaxAgeMultiplier,
      phase1StarvationMinOverloadRatio,
      phase1StarvationBaselineTps,
      candidateCleanTimeoutSec,
      requireIdleNode,
      idleProbeSec,
      requireMetricPresence,
      findMaxBinaryIterations,
      findMaxConfirmationSec,
      findMaxRepeats,
      findMaxMaxCandidates,
      clientSelfCheckEnabled,
      clientSelfCheckRequired,
      clientSelfCheckMultiplier,
      clientSelfCheckMinRatio,
      clientSelfCheckDurationSec,
      profileMode,
      pyroscopeEnabled,
      pgStatStatementsEnabled,
      minFeeA: minFeeA.toString(),
      minFeeB: minFeeB.toString(),
      reportPath,
      engineEventsPath,
      submitRecordsPath,
    };

    console.log("Starting Midgard L2 throughput benchmark with config:");
    console.log(JSON.stringify(configForReport, null, 2));

    setPhase("client-self-check");
    const clientSelfCheck = await runClientSelfCheck();
    if (clientSelfCheck.enabled) {
      console.log("Client self-check:");
      console.log(JSON.stringify(clientSelfCheck, null, 2));
    }

    setPhase("setup");
    const pgBefore = await readPgStatStatements(env, "before");
    /** @type {any[]} */
    let candidates = [];
    let fanout = {
      candidates: [],
      fanoutTxCount: 0,
      fanoutOutputCount: 0,
    };
    let effectiveCandidates = [];
    /** @type {{ outRefHex: string; txs: any; signer?: any; outputCbor?: Buffer; spendOutRefCbor?: Buffer; walletKey?: string; address?: string; lovelace?: bigint; }[]} */
    let selectedChains = [];
    let uniquePrebuiltTxIdCount = 0;
    let replaySkipped = 0;
    let duplicateSkipped = 0;
    let chainBuildSkipped = 0;
    let cursors;
    let corpusManifest = null;
    let corpusArtifactIdentity = null;
    let corpusIndexEntries = [];
    let corpusValidation = null;
    let corpusPreflightIdentity = null;
    let phase1FormalIdentity = null;
    let phase1FormalLivePreflight = null;
    let noOpCalibration = { enabled: false, required: requireNoOpCalibration };

    if (usingCorpus) {
      if (corpusIndexPath === null || corpusManifestPath === null) {
        throw new Error(
          "STRESS_CORPUS_PATH requires corpus index and manifest paths",
        );
      }
      if (corpusSliceId === null || String(corpusSliceId).trim().length === 0) {
        throw new Error("STRESS_CORPUS_SLICE_ID is required in corpus mode");
      }
      setPhase("corpus-preflight");
      corpusManifest = await loadCorpusManifest(corpusManifestPath);
      const fullIndex = await loadCorpusIndex(corpusIndexPath);
      if (
        maxChains === null &&
        Number.isSafeInteger(corpusManifest.chainCount) &&
        corpusManifest.chainCount > 0
      ) {
        maxChains = corpusManifest.chainCount;
      }
      corpusIndexEntries = selectCorpusIndexEntries({
        index: fullIndex,
        corpusSliceId,
        corpusShape,
        maxChains,
      });
      if (corpusPreflightEnabled) {
        const consumed = consumePhase3SoakCorpusPreflight({
          artifactPath: corpusPreflightPath,
          artifactSha256: corpusPreflightSha256,
          expectedSourceIdentitySha256: corpusPreflightSourceIdentitySha256,
          expectedPhase1BindingSha256: corpusPreflightPhase1BindingSha256,
          corpusPath,
          indexPath: corpusIndexPath,
          manifestPath: corpusManifestPath,
          manifest: corpusManifest,
          fullIndex,
          selectedEntries: corpusIndexEntries,
          corpusSliceId,
          corpusShape,
        });
        corpusArtifactIdentity = consumed.corpusArtifactIdentity;
        corpusValidation = consumed.validation;
        corpusPreflightIdentity = consumed.artifactIdentity;
      } else {
        corpusArtifactIdentity = await verifyCorpusArtifactIdentity({
          corpusPath,
          indexPath: corpusIndexPath,
          manifestPath: corpusManifestPath,
          manifest: corpusManifest,
        });
        corpusValidation = await validateCorpusSlice({
          corpusPath,
          indexEntries: corpusIndexEntries,
        });
      }
      configForReport.corpus.artifactIdentity = corpusArtifactIdentity;
      configForReport.corpus.preflight = corpusPreflightIdentity;
      if (phase1FormalBinding !== null) {
        phase1FormalIdentity = validatePhase1FormalCorpus({
          binding: phase1FormalBinding,
          corpusManifest,
          corpusArtifactIdentity,
          selectedIndexEntries: corpusIndexEntries,
        });
        configForReport.phase1FormalIdentity = phase1FormalIdentity;
      }
      const availableRows = corpusRowsForEntries(corpusIndexEntries);
      const requiredRows = estimateRequiredCorpusRows();
      if (availableRows < requiredRows) {
        throw new Error(
          `corpus_exhausted_preflight: selected slice has ${availableRows} rows, estimated worst-case need is ${requiredRows}`,
        );
      }
      if (phase1FormalIdentity !== null) {
        setPhase("phase1-live-preflight");
        phase1FormalLivePreflight = await verifyPhase1LivePreflight({
          expected: phase1FormalIdentity.livePreflight,
          fetchUtxos,
        });
        configForReport.phase1FormalLivePreflight = phase1FormalLivePreflight;
      }
      uniquePrebuiltTxIdCount = corpusValidation.uniqueTxHashes;
      selectedChains = corpusIndexEntries.map((entry) => ({
        outRefHex: entry.chainId,
        txs: { length: entry.rowCount },
      }));
      effectiveCandidates = selectedChains;
      fanout = {
        candidates: effectiveCandidates,
        fanoutTxCount: 0,
        fanoutOutputCount: 0,
      };
      console.log(
        `Using corpus slice ${corpusSliceId} with ${corpusIndexEntries.length} chains and ${availableRows} rows`,
      );

      setPhase("noop-calibration");
      const calibrationSliceId = envValue(
        "STRESS_CALIBRATION_CORPUS_SLICE_ID",
        corpusSliceId,
      );
      let calibrationEntries = corpusIndexEntries;
      if (calibrationSliceId !== corpusSliceId) {
        calibrationEntries = selectCorpusIndexEntries({
          index: fullIndex,
          corpusSliceId: calibrationSliceId,
          corpusShape,
          maxChains,
        });
      }
      noOpCalibration = await runNoOpCalibrationStage({
        cursors: openCorpusReader({
          corpusPath,
          indexEntries: calibrationEntries,
          // Calibration touches every chain round-robin. Keeping the measured
          // reader's 50-row buffer here would retain 204,800 parsed rows for
          // the formal 4,096-chain corpus before any node traffic begins.
          readAheadRows: 1,
        }),
        targetRateTps:
          benchmarkMode === "open"
            ? openLoopRate
            : benchmarkMode === "closed"
              ? targetAcceptedTps
              : rampMaxTps,
      });
      if ("maxObservedInFlight" in noOpCalibration) {
        const calibratedCapacity = deriveCalibratedClientCapacity({
          observedMaxInFlight: noOpCalibration.maxObservedInFlight,
          targetRateTps:
            benchmarkMode === "open"
              ? openLoopRate
              : benchmarkMode === "closed"
                ? targetAcceptedTps
                : rampMaxTps,
          assumedAcceptanceLatencyMs: corpusManifest.assumedAcceptanceLatencyMs,
          activeChainCount: corpusIndexEntries.length,
          httpPipelining,
        });
        noOpCalibration.effectiveClientCapacity = calibratedCapacity;
        configForReport.calibration.effectiveClientCapacity =
          calibratedCapacity;
        fs.writeFileSync(
          noOpCalibrationPath,
          `${JSON.stringify(noOpCalibration, null, 2)}\n`,
        );
        writeEngineEvent("calibration_capacity_selected", {
          effectiveClientCapacity: calibratedCapacity,
        });
        if (submitConcurrencySetting.sentinel) {
          submitConcurrency = calibratedCapacity.submitConcurrency;
        }
        if (httpConnectionsSetting.sentinel) {
          await rebuildHttpClient({
            connections: calibratedCapacity.httpConnections,
          });
        }
      }
      cursors = openCorpusReader({
        corpusPath,
        indexEntries: corpusIndexEntries,
        readAheadRows: corpusReadAheadRows,
      });
    } else {
      const seenOutRefs = new Set();
      for (const wallet of wallets) {
        const utxos = await fetchUtxos(wallet.address);
        let selected = 0;
        for (const utxo of utxos) {
          if (selected >= utxosPerWallet) {
            break;
          }
          const outRefHex = utxo.outref.toLowerCase();
          if (seenOutRefs.has(outRefHex)) {
            continue;
          }
          try {
            const coin = decodeCoin(utxo.outputCbor);
            if (coin < minLovelace) {
              continue;
            }
            candidates.push({
              walletKey: wallet.key,
              address: wallet.address,
              signer: wallet.signer,
              spendOutRefCbor: Buffer.from(utxo.outref, "hex"),
              outputCbor: Buffer.from(utxo.outputCbor, "hex"),
              lovelace: coin,
              outRefHex,
            });
            seenOutRefs.add(outRefHex);
            selected += 1;
          } catch {
            // Skip malformed or non-decodable UTxOs.
          }
        }
        console.log(
          `wallet ${wallet.key} address=${wallet.address} selected_utxos=${selected}`,
        );
      }

      if (candidates.length === 0) {
        throw new Error("No spendable UTxOs found for configured wallets");
      }

      setPhase("fanout");
      fanout = await ensureFanoutCandidates({
        candidates,
        minFeeA,
        minFeeB,
      });
      effectiveCandidates = fanout.candidates;
      console.log(
        `Available benchmark source UTxOs after fanout: ${effectiveCandidates.length} (fanout_txs=${fanout.fanoutTxCount}, fanout_outputs=${fanout.fanoutOutputCount})`,
      );

      setPhase("prebuild");
      const generatedTxIds = new Set();
      for (const candidate of effectiveCandidates) {
        if (maxChains !== null && selectedChains.length >= maxChains) {
          break;
        }
        let txs;
        try {
          txs = prebuildChain(candidate, chainLength, { minFeeA, minFeeB });
        } catch (error) {
          chainBuildSkipped += 1;
          console.log(
            `Skipping source UTxO that cannot fund requested chain: outref=${candidate.outRefHex} lovelace=${candidate.lovelace.toString()} reason=${error instanceof Error ? error.message : String(error)}`,
          );
          continue;
        }
        if (txs.length === 0) {
          continue;
        }
        if (requireFreshChains) {
          const firstStatus = await fetchTxStatus(txs[0].txIdHex);
          if (firstStatus !== "not_found") {
            replaySkipped += 1;
            continue;
          }
        }
        let hasDuplicate = false;
        for (const tx of txs) {
          if (generatedTxIds.has(tx.txIdHex)) {
            hasDuplicate = true;
            break;
          }
        }
        if (hasDuplicate) {
          duplicateSkipped += 1;
          continue;
        }
        for (const tx of txs) {
          generatedTxIds.add(tx.txIdHex);
        }
        selectedChains.push({
          ...candidate,
          txs,
        });
      }
      uniquePrebuiltTxIdCount = generatedTxIds.size;

      if (selectedChains.length === 0) {
        throw new Error(
          "No eligible fresh chains found. Increase STRESS_UTXOS_PER_WALLET or disable STRESS_REQUIRE_FRESH_CHAINS=false for diagnostics.",
        );
      }

      console.log(
        `Using ${selectedChains.length} independent prebuilt chains (replay_skipped=${replaySkipped}, duplicate_skipped=${duplicateSkipped}, chain_build_skipped=${chainBuildSkipped})`,
      );

      cursors = makeChainCursors(selectedChains);
    }
    const stageReports = [];
    monitor = await startCounterMonitor(phaseNameRef);

    if (warmupTxs > 0 || warmupSec > 0) {
      setPhase("warmup");
      const warmupStage = await runClosedLoopStage({
        name: "warmup",
        cursors,
        durationSec: warmupSec > 0 ? warmupSec : Number.MAX_SAFE_INTEGER / 1000,
        maxTxs: warmupTxs > 0 ? warmupTxs : Number.POSITIVE_INFINITY,
      });
      await waitForStageDrain(warmupStage);
      await collectStatusLatencies(warmupStage);
      stageReports.push(buildStageReport(warmupStage));
    }

    if (remainingTxCount(cursors) <= 0) {
      throw new Error("No benchmark transactions remain after warmup");
    }

    setPhase("measured");
    const measuredStages = [];
    const candidateEvaluations = [];
    let priorSubmitLatencyP99Ms = null;
    let findMaxResult = null;
    const runOpenCandidate = async ({ name, targetRate, durationSec }) => {
      await waitForCandidateCleanliness(name);
      assertCandidateCapacity({
        cursors,
        targetRateTps: targetRate,
        durationSec,
        priorSubmitLatencyP99Ms,
      });
      const stage = await runOpenLoopStage({
        name,
        cursors,
        rateTps: targetRate,
        durationSec,
        retryLimit: measuredRetry503,
      });
      await waitForStageDrain(stage);
      await collectStatusLatencies(stage);
      const stageReport = buildStageReport(stage);
      const evaluation = evaluateStagePass({
        stage,
        stageReport,
        monitorSamples: monitor.samples,
      });
      stageReport.evaluation = evaluation;
      priorSubmitLatencyP99Ms =
        stageReport.submitLatencyMs.p99 ?? priorSubmitLatencyP99Ms;
      measuredStages.push(stageReport);
      stageReports.push(stageReport);
      candidateEvaluations.push({
        name,
        targetRateTps: targetRate,
        durationSec,
        passed: evaluation.passed,
        reasons: evaluation.reasons,
        measuredAcceptedTps: stageReport.measuredAcceptedTps,
        queuedSubmitSuccessPerSec: stageReport.queuedSubmitSuccessPerSec,
        backlogSlopePerSec: evaluation.backlogSlopePerSec,
        bottleneck: evaluation.bottleneck,
      });
      return { stage, stageReport, evaluation };
    };
    const runSearchCandidate = async ({ name, targetRate, durationSec }) => {
      try {
        return await runOpenCandidate({ name, targetRate, durationSec });
      } catch (error) {
        const reason = `preflight_failed: ${error instanceof Error ? error.message : String(error)}`;
        const evaluation = {
          passed: false,
          reasons: [reason],
          backlogSlopePerSec: null,
          bottleneck: {
            label: "benchmark-client limited",
            rule: "candidate preflight failed before measured submission",
            evidence: {
              targetRateTps: targetRate,
              durationSec,
              reason,
            },
          },
        };
        const stageReport = {
          name,
          targetRateTps: targetRate,
          measuredAcceptedTps: 0,
          queuedSubmitSuccessPerSec: 0,
          evaluation,
        };
        candidateEvaluations.push({
          name,
          targetRateTps: targetRate,
          durationSec,
          passed: false,
          reasons: evaluation.reasons,
          measuredAcceptedTps: 0,
          queuedSubmitSuccessPerSec: 0,
          backlogSlopePerSec: null,
          bottleneck: evaluation.bottleneck,
        });
        return { stage: null, stageReport, evaluation };
      }
    };

    if (benchmarkMode === "ramp" || benchmarkMode === "find-max") {
      if (benchmarkMode === "ramp") {
        for (
          let targetRate = rampStartTps;
          targetRate <= rampMaxTps && remainingTxCount(cursors) > 0;
          targetRate += rampStepTps
        ) {
          setPhase(`measured-ramp-${targetRate}`);
          const { stageReport, evaluation } = await runSearchCandidate({
            name: `ramp-${targetRate}`,
            targetRate,
            durationSec: rampStageSec,
          });
          if (!evaluation.passed) {
            console.log(
              `Stopping ramp at target=${targetRate}: reasons=${evaluation.reasons.join("; ")} measured_accepted_tps=${stageReport.measuredAcceptedTps.toFixed(2)}`,
            );
            break;
          }
        }
      } else {
        const exploratoryPassed = [];
        let low = 0;
        let high = null;
        let candidateCount = 0;
        for (
          let targetRate = rampStartTps;
          targetRate <= rampMaxTps &&
          remainingTxCount(cursors) > 0 &&
          candidateCount < findMaxMaxCandidates;
          targetRate += rampStepTps
        ) {
          setPhase(`measured-find-max-ramp-${targetRate}`);
          const result = await runSearchCandidate({
            name: `find-max-ramp-${targetRate}`,
            targetRate,
            durationSec: rampStageSec,
          });
          candidateCount += 1;
          if (result.evaluation.passed) {
            low = targetRate;
            exploratoryPassed.push(targetRate);
          } else {
            high = targetRate;
            break;
          }
        }

        if (high !== null && low > 0) {
          for (
            let iteration = 0;
            iteration < findMaxBinaryIterations &&
            remainingTxCount(cursors) > 0 &&
            candidateCount < findMaxMaxCandidates;
            iteration += 1
          ) {
            const targetRate = Math.round(((low + high) / 2) * 100) / 100;
            if (targetRate <= low || targetRate >= high) {
              break;
            }
            setPhase(`measured-find-max-binary-${iteration}-${targetRate}`);
            const result = await runSearchCandidate({
              name: `find-max-binary-${iteration}-${targetRate}`,
              targetRate,
              durationSec: rampStageSec,
            });
            candidateCount += 1;
            if (result.evaluation.passed) {
              low = targetRate;
              exploratoryPassed.push(targetRate);
            } else {
              high = targetRate;
            }
          }
        }

        const confirmationTargets = [...new Set(exploratoryPassed)]
          .sort((a, b) => b - a)
          .slice(0, 3);
        for (const targetRate of confirmationTargets) {
          const confirmationReports = [];
          let confirmed = true;
          for (
            let repeat = 1;
            repeat <= findMaxRepeats &&
            remainingTxCount(cursors) > 0 &&
            candidateCount < findMaxMaxCandidates;
            repeat += 1
          ) {
            setPhase(`measured-find-max-confirm-${targetRate}-${repeat}`);
            const result = await runSearchCandidate({
              name: `find-max-confirm-${targetRate}-${repeat}`,
              targetRate,
              durationSec: findMaxConfirmationSec,
            });
            candidateCount += 1;
            confirmationReports.push(result.stageReport);
            if (!result.evaluation.passed) {
              confirmed = false;
              break;
            }
          }
          if (confirmed && confirmationReports.length === findMaxRepeats) {
            findMaxResult = {
              maxSustainableAcceptedTxPerSec: targetRate,
              confirmationRepeats: findMaxRepeats,
              confirmationDurationSec: findMaxConfirmationSec,
              confirmationStageNames: confirmationReports.map(
                (report) => report.name,
              ),
              confirmationAcceptedTps: confirmationReports.map(
                (report) => report.measuredAcceptedTps,
              ),
            };
            break;
          }
        }

        if (findMaxResult === null) {
          console.log(
            "find-max did not confirm a sustainable candidate; see candidate evaluations for failure reasons",
          );
        }
      }
    } else if (benchmarkMode === "open") {
      await runOpenCandidate({
        name: "measured-open",
        targetRate: openLoopRate,
        durationSec: measuredSec,
      });
    } else {
      await waitForCandidateCleanliness("measured-closed");
      const stage = await runClosedLoopStage({
        name: "measured-closed",
        cursors,
        durationSec: measuredSec,
        retryLimit: measuredRetry503,
      });
      await waitForStageDrain(stage);
      await collectStatusLatencies(stage);
      const stageReport = buildStageReport(stage);
      stageReport.evaluation = evaluateStagePass({
        stage,
        stageReport,
        monitorSamples: monitor.samples,
      });
      measuredStages.push(stageReport);
      stageReports.push(stageReport);
    }

    setPhase("cooldown");
    if (cooldownSec > 0) {
      await sleep(cooldownSec * 1000);
    }
    const monitorSamples = monitor.samples;
    await monitor.stop();
    monitor = null;
    const finalCounterSnapshot = await readCounters();

    const pgAfter = await readPgStatStatements(env, "after");
    phaseRecorder.end();
    const runtime = runtimeSampler.stop();

    const primaryMeasuredStages =
      benchmarkMode === "find-max" && findMaxResult !== null
        ? measuredStages.filter((stage) =>
            findMaxResult.confirmationStageNames.includes(stage.name),
          )
        : measuredStages;

    const measuredElapsedSec = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.measuredElapsedSec,
      0,
    );
    const measuredSubmitted = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.submitted,
      0,
    );
    const measuredPhysicalSubmitAttempts = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.physicalSubmitAttempts,
      0,
    );
    const measuredQueueFullResponses = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.queueFullResponses,
      0,
    );
    const measuredSubmitErrors = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.submitErrors,
      0,
    );
    const measuredAcceptedDelta = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.acceptedDelta,
      0,
    );
    const measuredRejectedDelta = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.rejectedDelta,
      0,
    );
    const measuredCommitTxDelta = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.commitTxDelta,
      0,
    );
    const measuredMergeBlockDelta = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.mergeBlockDelta,
      0,
    );
    const measuredMonitorSamples = monitorSamples.filter((sample) =>
      String(sample.phase).startsWith("measured"),
    );
    const rollingRateSamples =
      measuredMonitorSamples.length >= 2
        ? measuredMonitorSamples
        : monitorSamples;
    const rollingRates = summarizeRollingRates(
      rollingRateSamples,
      ["submit", "accept", "reject", "commitBlockTx", "mergeBlock"],
      BENCHMARK_WINDOWS_MS,
    );
    const combinedScheduleLag = summarizeLatency(
      primaryMeasuredStages.flatMap(
        (stage) => stage.scheduleLagSamplesMs ?? [],
      ),
    );
    const missingRequiredMetrics = [
      ...new Set(
        primaryMeasuredStages.flatMap(
          (stage) => stage.missingRequiredMetrics ?? [],
        ),
      ),
    ];
    const measuredBacklogSlope = gaugeSlopePerSec(
      measuredMonitorSamples.length >= 2
        ? measuredMonitorSamples
        : monitorSamples,
      "validationQueueDepth",
    );
    const l1Observation = summarizeL1Observation(measuredMonitorSamples);
    const bottleneck = classifyLikelyBottleneckWithEvidence({
      submitted: measuredSubmitted,
      submitErrors: measuredSubmitErrors,
      queueFullResponses: measuredQueueFullResponses,
      acceptedDelta: measuredAcceptedDelta,
      rejectedDelta: measuredRejectedDelta,
      commitTxDelta: measuredCommitTxDelta,
      mergeBlockDelta: measuredMergeBlockDelta,
      targetAcceptedTps:
        benchmarkMode === "find-max" && findMaxResult !== null
          ? findMaxResult.maxSustainableAcceptedTxPerSec
          : targetAcceptedTps,
      avgAcceptedTps:
        measuredElapsedSec > 0 ? measuredAcceptedDelta / measuredElapsedSec : 0,
      clientSelfCheck,
      endCounters: finalCounterSnapshot,
      waitForCommit,
      waitForMerge,
      scheduleLagMs: combinedScheduleLag,
      submitConcurrency,
      backlogSlopePerSec: measuredBacklogSlope,
      requiredMetricsMissing: missingRequiredMetrics,
    });

    const corpusConsumption =
      usingCorpus && typeof cursors.consumptionSnapshot === "function"
        ? cursors.consumptionSnapshot()
        : null;
    const summary = {
      scenario: scenarioName,
      scenarioClass,
      chainCount: selectedChains.length,
      chainLength,
      attempted: selectedChains.reduce(
        (acc, chain) => acc + chain.txs.length,
        0,
      ),
      remaining: remainingTxCount(cursors),
      replaySkipped,
      duplicateSkipped,
      chainBuildSkipped,
      fanoutTxCount: fanout.fanoutTxCount,
      fanoutOutputCount: fanout.fanoutOutputCount,
      uniquePrebuiltTxIds: uniquePrebuiltTxIdCount,
      corpus: usingCorpus
        ? {
            path: corpusPath,
            indexPath: corpusIndexPath,
            manifestPath: corpusManifestPath,
            sliceId: corpusSliceId,
            shape: corpusShape,
            validation: corpusValidation,
            artifactIdentity: corpusArtifactIdentity,
            preflight: corpusPreflightIdentity,
            consumption: corpusConsumption,
          }
        : null,
      phase1FormalIdentity,
      phase1FormalLivePreflight,
      calibration:
        "enabled" in noOpCalibration && noOpCalibration.enabled === false
          ? null
          : noOpCalibration,
      nodeSaturationProof: primaryMeasuredStages.map((stage) => ({
        stageName: stage.name,
        offeredRatePerSec:
          stage.evaluation?.nodeSaturation?.offeredRatePerSec ?? null,
        acceptedRatePerSec:
          stage.evaluation?.nodeSaturation?.acceptedRatePerSec ?? null,
        ratio: stage.evaluation?.nodeSaturation?.ratio ?? null,
        minRatio: nodeSaturationMinRatio,
        passed: stage.evaluation?.nodeSaturation?.passed ?? false,
      })),
      primaryStageNames: primaryMeasuredStages.map((stage) => stage.name),
      submitted: measuredSubmitted,
      physicalSubmitAttempts: measuredPhysicalSubmitAttempts,
      submitErrors: measuredSubmitErrors,
      queueFullResponses: measuredQueueFullResponses,
      acceptDelta: measuredAcceptedDelta,
      rejectDelta: measuredRejectedDelta,
      commitTxDelta: measuredCommitTxDelta,
      mergeBlockDelta: measuredMergeBlockDelta,
      physicalSubmitAttemptsPerSec:
        measuredElapsedSec > 0
          ? measuredPhysicalSubmitAttempts / measuredElapsedSec
          : 0,
      queuedSubmitSuccessPerSec:
        measuredElapsedSec > 0 ? measuredSubmitted / measuredElapsedSec : 0,
      durablyAdmittedPerSecond:
        measuredElapsedSec > 0 ? measuredSubmitted / measuredElapsedSec : 0,
      avgAcceptedTps:
        measuredElapsedSec > 0 ? measuredAcceptedDelta / measuredElapsedSec : 0,
      acceptedPerSecond:
        measuredElapsedSec > 0 ? measuredAcceptedDelta / measuredElapsedSec : 0,
      committedTxPerSec:
        measuredElapsedSec > 0 ? measuredCommitTxDelta / measuredElapsedSec : 0,
      mergeBlocksPerSec:
        measuredElapsedSec > 0
          ? measuredMergeBlockDelta / measuredElapsedSec
          : 0,
      maxAcceptRate1s: rollingRates.accept?.["1s"] ?? 0,
      maxSubmitRate1s: rollingRates.submit?.["1s"] ?? 0,
      maxRejectRate1s: rollingRates.reject?.["1s"] ?? 0,
      maxCommitTxRate1s: rollingRates.commitBlockTx?.["1s"] ?? 0,
      maxMergeBlockRate1s: rollingRates.mergeBlock?.["1s"] ?? 0,
      targetAcceptedTps,
      maxSustainableAcceptedTxPerSec:
        findMaxResult?.maxSustainableAcceptedTxPerSec ?? null,
      measuredElapsedSec,
      bottleneck: bottleneck.label,
      bottleneckEvidence: bottleneck,
      missingRequiredMetrics,
      measuredBacklogSlopePerSec: measuredBacklogSlope,
      l1Observation,
      phase1StageAWindowGates: primaryMeasuredStages.map((stage) => ({
        stageName: stage.name,
        ...stage.evaluation?.phase1StageAWindowGate,
      })),
      phase1StarvationGates: primaryMeasuredStages.map((stage) => ({
        stageName: stage.name,
        ...stage.evaluation?.phase1StarvationGate,
      })),
      reportPath,
      firstErrors: primaryMeasuredStages
        .flatMap((stage) => stage.firstErrors)
        .slice(0, 10),
    };

    const report = {
      benchmark: "midgard-l2-throughput",
      version: 2,
      scenario: scenarioName,
      scenarioClass,
      generatedAtIso: new Date().toISOString(),
      metadata: {
        git,
        runtime: runtimeMetadata,
      },
      runIdentity: phase1FormalIdentity,
      livePreflight: phase1FormalLivePreflight,
      config: configForReport,
      clientSelfCheck,
      calibration: {
        noOp:
          "enabled" in noOpCalibration && noOpCalibration.enabled === false
            ? null
            : noOpCalibration,
        nodeSaturationByStage: primaryMeasuredStages.map((stage) => ({
          stageName: stage.name,
          offeredRatePerSec:
            stage.evaluation?.nodeSaturation?.offeredRatePerSec ?? null,
          acceptedRatePerSec:
            stage.evaluation?.nodeSaturation?.acceptedRatePerSec ?? null,
          ratio: stage.evaluation?.nodeSaturation?.ratio ?? null,
          passed: stage.evaluation?.nodeSaturation?.passed ?? false,
        })),
      },
      sourceUtxos: {
        selectedBeforeFanout: candidates.length,
        selectedAfterFanout: effectiveCandidates.length,
        fanoutTxCount: fanout.fanoutTxCount,
        fanoutOutputCount: fanout.fanoutOutputCount,
      },
      workload: {
        name: usingCorpus ? "prebuilt-corpus" : "valid-native-chain",
        chainCount: selectedChains.length,
        chainLength,
        corpusManifest,
        corpusArtifactIdentity,
        uniquePrebuiltTxIds: uniquePrebuiltTxIdCount,
      },
      phases: phaseRecorder.list(),
      stages: stageReports,
      candidateEvaluations,
      findMax: findMaxResult,
      rollingRateSampleScope:
        measuredMonitorSamples.length >= 2 ? "measured" : "all",
      rollingRates,
      runtime,
      profiling: {
        nodeProfileMode: profileMode,
        pyroscope,
      },
      postgres: {
        before: pgBefore,
        after: pgAfter,
      },
      summary,
      l1Observation,
    };

    writeReport(report);
    writeEngineEvent("engine_finished", {
      reportPath,
      submitted: measuredSubmitted,
      accepted: measuredAcceptedDelta,
      submitErrors: measuredSubmitErrors,
      findMax: findMaxResult,
    });
    console.log("Benchmark summary:");
    console.log(JSON.stringify(summary, null, 2));

    const failedDrain = measuredStages.some(
      (stage) =>
        primaryMeasuredStages.includes(stage) &&
        (stage.drain === null || stage.drain.completed !== true),
    );
    const failedEvaluation = primaryMeasuredStages.some(
      (stage) => stage.evaluation?.passed !== true,
    );
    const corpusExhausted = measuredStages.some(
      (stage) => stage.abortedCorpusExhausted === true,
    );
    if (
      measuredAcceptedDelta <= 0 ||
      measuredSubmitErrors > 0 ||
      corpusExhausted ||
      failedDrain ||
      failedEvaluation ||
      (benchmarkMode === "find-max" && findMaxResult === null)
    ) {
      process.exitCode = 1;
    }
  } finally {
    if (monitor !== null) {
      await monitor.stop();
    }
    if (submitRecordWriter !== null) {
      await submitRecordWriter.close();
      submitRecordWriter = null;
    }
    if (engineEventWriter !== null) {
      await engineEventWriter.close();
      engineEventWriter = null;
    }
    await Promise.allSettled(corpusReaders.map((reader) => reader.close()));
    await httpClient.close();
  }
};

main().catch((error) => {
  console.error("throughput-valid-stress failed:", error);
  process.exitCode = 1;
});
