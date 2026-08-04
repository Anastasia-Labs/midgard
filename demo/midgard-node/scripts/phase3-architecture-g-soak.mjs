#!/usr/bin/env node

import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Redacted } from "effect";
import { spawn } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  assertRegularFile,
  captureClosureIdentity,
  readJson,
  sameSourceIdentity,
  scanSubmitRecords,
  sha256File,
  sourceIdentity,
  summarizePhase3WorkloadReport,
} from "./phase3-architecture-g-closure-lib.mjs";
import {
  assertPhase3SoakCorpusPreflightCurrent,
  createPhase3SoakCorpusPreflight,
  establishPhase3SoakPreflight,
} from "./phase3-architecture-g-soak-preflight.mjs";
import {
  canonicalPhase3NodeEndpoint,
  createPhase3LoadGeneratorIsolation,
  createPhase3NodePreLifecycleRevalidation,
} from "./phase3-architecture-g-load-generator-isolation.mjs";
import {
  evaluatePhase3ArchitectureGSoakReport,
  PHASE3_ACCEPTED_RATE_MIN_RATIO,
  PHASE3_ARCHITECTURE_G_SAMPLE_INTERVAL_MS,
  PHASE3_ARCHITECTURE_G_SOAK_DURATION_SEC,
  PHASE3_ARCHITECTURE_G_SOAK_SCENARIO,
  PHASE3_ARCHITECTURE_G_SOAK_SCHEMA,
  PHASE3_DRAIN_TIMEOUT_SEC,
  PHASE3_NODE_SATURATION_MIN_RATIO,
  PHASE3_OFFERED_RATE_MIN_RATIO,
  PHASE3_WORKLOAD_LIFECYCLE_GRACE_MS,
} from "./verify-phase3-architecture-g-soak-report.mjs";

const scriptPath = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(scriptPath);
const packageRoot = path.resolve(scriptDir, "..");
const FETCH_TIMEOUT_MS = 10_000;
const MAX_AUDIT_INTERVAL_MS = 6 * 60 * 60_000;
const PHASE3_TARGET_TPS = 5_000;

const sleep = (durationMs) =>
  new Promise((resolve) => setTimeout(resolve, Math.max(0, durationMs)));

const requiredArg = (name) => {
  const index = process.argv.indexOf(name);
  const value = index < 0 ? undefined : process.argv[index + 1];
  if (value === undefined || value.startsWith("--")) {
    throw new Error(`missing required ${name}`);
  }
  return value;
};

const absoluteArg = (name) => {
  const value = requiredArg(name);
  if (!path.isAbsolute(value)) throw new Error(`${name} must be absolute`);
  return path.resolve(value);
};

const preparePhase3SoakOutputDirectory = () => {
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

const requiredEnv = (name) => {
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

const metricValue = (text, names) => {
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

const fetchText = async (url) => {
  const response = await fetch(url, {
    signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
  });
  return { status: response.status, body: await response.text() };
};

const readProcessSample = (pid) => {
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

const auditState = async () => {
  const layer = PgClient.layer({
    host: requiredEnv("POSTGRES_HOST"),
    port: Number(requiredEnv("POSTGRES_PORT")),
    username: requiredEnv("POSTGRES_USER"),
    password: Redacted.make(requiredEnv("POSTGRES_PASSWORD")),
    database: requiredEnv("POSTGRES_DB"),
    maxConnections: 1,
    connectTimeout: 10_000,
    applicationName: "midgard-phase3-soak-audit-probe",
  });
  const rows = await Effect.runPromise(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql`
        SELECT audit_diverged, last_audit_diverged, last_audit_at,
               EXTRACT(EPOCH FROM (CURRENT_TIMESTAMP - last_audit_at)) * 1000 AS audit_age_ms
        FROM mpf_engine_state WHERE store_name = 'ledger'`;
    }).pipe(Effect.provide(layer), Effect.scoped),
  );
  const row = rows[0];
  const auditAgeMs = Number(row?.audit_age_ms);
  const auditCompletedAtMs = new Date(row?.last_audit_at).getTime();
  if (
    row === undefined ||
    row.audit_diverged !== false ||
    row.last_audit_diverged !== false ||
    !Number.isFinite(auditAgeMs) ||
    !Number.isFinite(auditCompletedAtMs) ||
    auditAgeMs < 0 ||
    auditAgeMs > MAX_AUDIT_INTERVAL_MS + 90_000
  ) {
    throw new Error("MPF audit state is divergent, missing, or stale");
  }
  return { auditDivergence: 0, auditAgeMs, auditCompletedAtMs };
};

const captureSample = async ({
  startedAtMs,
  nodePid,
  readyUrl,
  metricsUrl,
}) => {
  const observedAtMs = Date.now();
  const [readyResponse, metricsResponse, audit] = await Promise.all([
    fetchText(readyUrl),
    fetchText(metricsUrl),
    auditState(),
  ]);
  let readiness;
  try {
    readiness = JSON.parse(readyResponse.body);
  } catch {
    throw new Error("readiness endpoint returned invalid JSON");
  }
  if (
    readyResponse.status !== 200 ||
    readiness?.ready !== true ||
    !Array.isArray(readiness?.reasons) ||
    readiness.reasons.length !== 0
  ) {
    throw new Error(
      `node readiness failed with HTTP ${readyResponse.status.toString()}`,
    );
  }
  if (metricsResponse.status !== 200) {
    throw new Error(
      `metrics endpoint failed with HTTP ${metricsResponse.status.toString()}`,
    );
  }
  const owner = readiness.nativeMpfOwner;
  if (owner?.healthy !== true) {
    throw new Error(
      "readiness did not return healthy Architecture G owner diagnostics",
    );
  }
  const metrics = metricsResponse.body;
  const validationWorkerTimeoutTotal = metricValue(metrics, [
    "validation_worker_job_timeout_count_total",
    "validation_worker_job_timeout_count",
  ]);
  const l1ControlPlaneTimeoutTotal = metricValue(metrics, [
    "l1_control_plane_timeouts_total",
  ]);
  return {
    observedAtMs,
    elapsedMs:
      startedAtMs === null ? null : Math.max(0, observedAtMs - startedAtMs),
    readiness: {
      httpStatus: readyResponse.status,
      ready: readiness.ready,
      reasons: readiness.reasons,
    },
    metrics: {
      ...audit,
      confirmedLedgerFullScanTotal: metricValue(metrics, [
        "confirmed_ledger_full_scan_total",
      ]),
      validationWorkerTimeoutTotal,
      l1ControlPlaneTimeoutTotal,
      timeoutInsteadOfBackpressureTotal:
        validationWorkerTimeoutTotal + l1ControlPlaneTimeoutTotal,
      daPublicationBacklog: metricValue(metrics, [
        "da_publish_reconciler_backlog",
      ]),
      mergeQueueDepth: metricValue(metrics, ["blocks_in_queue"]),
    },
    owner: {
      durableRoot: owner.durableRoot,
      residentNodes: owner.residentNodes,
      residentBytes: owner.residentBytes,
      activeGenerations: owner.activeGenerations,
      generatedNodes: owner.generatedNodes,
      generatedBytes: owner.generatedBytes,
      rssBytes: owner.rssBytes,
      peakRssBytes: owner.peakRssBytes,
      childRestarts: owner.childRestarts,
    },
    process: readProcessSample(nodePid),
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
  fs.writeFileSync(reportPath, `${JSON.stringify(report, null, 2)}\n`, {
    flag: "wx",
    mode: 0o600,
  });
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

const run = async () => {
  let setupPhase = "output-directory";
  const preparedOutput = preparePhase3SoakOutputDirectory();
  const outDir = preparedOutput.outDir;
  const reportPath = path.join(outDir, "report.json");
  const verificationPath = path.join(outDir, "verification.json");
  const workloadReportPath = path.join(outDir, "workload-report.json");
  const engineEventsPath = path.join(outDir, "workload-events.ndjson");
  const submitRecordsPath = path.join(outDir, "submit-records.ndjson");
  let timing = null;
  let workloadScript;
  let closureIdentity = null;
  let corpusShape;
  let preflight = null;
  let identity = null;
  let readyUrl;
  let metricsUrl;
  let startedAtMs;
  let samples = [];
  try {
    if (preparedOutput.error !== null) throw preparedOutput.error;
    setupPhase = "timing";
    timing = resolvePhase3SoakTiming();
    setupPhase = "runtime";
    if (!timing.testOnly && process.version !== "v22.22.2") {
      throw new Error("the production Phase 3 soak requires Node v22.22.2");
    }
    setupPhase = "endpoint-arguments";
    readyUrl = canonicalPhase3NodeEndpoint(
      requiredArg("--ready-url"),
      "/readyz",
      "readiness endpoint",
    );
    metricsUrl = canonicalPhase3NodeEndpoint(
      requiredArg("--metrics-url"),
      "/metrics",
      "metrics endpoint",
    );
    setupPhase = "arguments";
    workloadScript = absoluteArg("--workload-script");
    assertRegularFile(workloadScript, "workload script");
    setupPhase = "closure-identity";
    closureIdentity = await captureClosureIdentity({
      packageRoot,
      runtimePath: absoluteArg("--runtime-fingerprint"),
      deploymentPath: absoluteArg("--deployment-manifest"),
      phase1Path: absoluteArg("--phase1-binding"),
      ownerBinaryPath: absoluteArg("--owner-binary"),
      ownerSha256ManifestPath: absoluteArg("--owner-binary-sha256-manifest"),
      runnerPath: scriptPath,
      verifierPath: path.join(
        scriptDir,
        "verify-phase3-architecture-g-soak-report.mjs",
      ),
    });
    const phase1Binding = readJson(closureIdentity.phase1.path);
    corpusShape = String(
      phase1Binding?.stressCorpusEnv?.STRESS_CORPUS_SHAPE ?? "",
    ).trim();
    if (!new Set(["fanout", "chain", "mixed"]).has(corpusShape)) {
      throw new Error("Phase 1 binding has no valid corpus shape");
    }
    let loadGeneratorIsolation = null;
    let initialReadiness = null;
    let nodePreLifecycleRevalidation = null;
    setupPhase = "corpus-preflight";
    const establishedPreflight = await establishPhase3SoakPreflight({
      runPreflight: async () => {
        const artifact = await createPhase3SoakCorpusPreflight({
          outPath: path.join(outDir, "corpus-preflight.json"),
          phase1Binding,
          phase1BindingPath: closureIdentity.phase1.path,
          phase1BindingSha256: closureIdentity.phase1.sha256,
          sourceIdentity: closureIdentity.source,
          corpusIdentity: closureIdentity.phase1.corpus,
          corpusSliceId: closureIdentity.phase1.corpus.sliceId,
          corpusShape,
        });
        setupPhase = "source-revalidation";
        const sourceAfterPreflight = await sourceIdentity(packageRoot);
        if (!sameSourceIdentity(sourceAfterPreflight, closureIdentity.source)) {
          throw new Error("source identity changed during corpus preflight");
        }
        setupPhase = "load-generator-isolation";
        loadGeneratorIsolation = await createPhase3LoadGeneratorIsolation({
          outPath: path.join(outDir, "load-generator-isolation.json"),
          phase1NodeContainerId: closureIdentity.phase1.nodeContainerId,
          phase1NodeImageId: closureIdentity.phase1.nodeImageId,
          readyUrl,
          metricsUrl,
        });
        identity = {
          ...closureIdentity,
          corpusPreflight: artifact,
          loadGeneratorIsolation,
        };
        setupPhase = "initial-readiness";
        initialReadiness = await captureSample({
          startedAtMs: null,
          nodePid: loadGeneratorIsolation.nodeHostPid,
          readyUrl,
          metricsUrl,
        });
        setupPhase = "node-pre-lifecycle-reinspection";
        nodePreLifecycleRevalidation =
          await createPhase3NodePreLifecycleRevalidation({
            outPath: path.join(outDir, "node-pre-lifecycle-revalidation.json"),
            isolationArtifactPath: loadGeneratorIsolation.path,
            isolationArtifactSha256: loadGeneratorIsolation.sha256,
          });
        identity = {
          ...identity,
          nodePreLifecycleRevalidation,
        };
        return artifact;
      },
    });
    preflight = {
      ...establishedPreflight,
      initialReadiness,
      nodePreLifecycleRevalidation,
    };
    startedAtMs = preflight.lifecycleStartedAtMs;
    samples = [{ ...initialReadiness, elapsedMs: 0 }];
  } catch (error) {
    try {
      writePhase3SoakSetupFailureReport({
        reportPath,
        verificationPath,
        timing,
        phase: setupPhase,
        error,
        identity,
        preflight,
        samples,
      });
      console.error(
        `retained failed Phase 3 setup evidence in ${outDir} (phase ${setupPhase})`,
      );
    } catch (evidenceError) {
      console.error(
        `unable to retain setup failure evidence: ${evidenceError instanceof Error ? evidenceError.message : String(evidenceError)}`,
      );
    }
    throw error;
  }
  const nodePid = identity.loadGeneratorIsolation.nodeHostPid;
  const stdout = fs.createWriteStream(
    path.join(outDir, "workload.stdout.log"),
    {
      flags: "wx",
      mode: 0o600,
    },
  );
  const stderr = fs.createWriteStream(
    path.join(outDir, "workload.stderr.log"),
    {
      flags: "wx",
      mode: 0o600,
    },
  );
  const child = spawn(process.execPath, [workloadScript], {
    cwd: packageRoot,
    env: {
      ...process.env,
      STRESS_SCENARIO_NAME: PHASE3_ARCHITECTURE_G_SOAK_SCENARIO,
      STRESS_SCENARIO_CLASS: "B",
      STRESS_FORMAL_BENCHMARK: "true",
      STRESS_MODE: "open",
      STRESS_TARGET_ACCEPTED_TPS: PHASE3_TARGET_TPS.toString(),
      STRESS_OPEN_LOOP_RATE_TPS: PHASE3_TARGET_TPS.toString(),
      STRESS_MEASURED_SEC: timing.durationSec.toString(),
      STRESS_WARMUP_TXS: "0",
      STRESS_WARMUP_SEC: "0",
      STRESS_COOLDOWN_SEC: "0",
      STRESS_DRAIN_TIMEOUT_SEC: PHASE3_DRAIN_TIMEOUT_SEC.toString(),
      STRESS_OFFERED_RATE_MIN_RATIO: PHASE3_OFFERED_RATE_MIN_RATIO.toString(),
      STRESS_ACCEPTED_RATE_MIN_RATIO: PHASE3_ACCEPTED_RATE_MIN_RATIO.toString(),
      STRESS_NODE_SATURATION_MIN_RATIO:
        PHASE3_NODE_SATURATION_MIN_RATIO.toString(),
      STRESS_WAIT_FOR_COMMIT: "true",
      STRESS_WAIT_FOR_MERGE: "true",
      STRESS_REPORT_PATH: workloadReportPath,
      STRESS_ENGINE_EVENTS_PATH: engineEventsPath,
      STRESS_SUBMIT_RECORDS_PATH: submitRecordsPath,
      STRESS_CORPUS_PATH: identity.phase1.corpus.path,
      STRESS_CORPUS_INDEX_PATH: identity.phase1.corpus.indexPath,
      STRESS_CORPUS_MANIFEST_PATH: identity.phase1.corpus.manifestPath,
      STRESS_CORPUS_SLICE_ID: identity.phase1.corpus.sliceId,
      STRESS_CORPUS_SHAPE: corpusShape,
      STRESS_CORPUS_PREFLIGHT_REQUIRED: "true",
      STRESS_CORPUS_PREFLIGHT_PATH: identity.corpusPreflight.path,
      STRESS_CORPUS_PREFLIGHT_SHA256: identity.corpusPreflight.sha256,
      STRESS_CORPUS_PREFLIGHT_SOURCE_IDENTITY_SHA256:
        identity.corpusPreflight.sourceIdentitySha256,
      STRESS_CORPUS_PREFLIGHT_PHASE1_BINDING_SHA256:
        identity.corpusPreflight.phase1BindingSha256,
      STRESS_LOAD_GENERATOR_PLACEMENT: "measured-cgroup",
      STRESS_LOADGEN_COHOSTED: "true",
      STRESS_CLOCK_OFFSET_MS: "0",
      STRESS_LOAD_GENERATOR_ISOLATION_REQUIRED: "true",
      STRESS_LOAD_GENERATOR_ISOLATION_PATH:
        identity.loadGeneratorIsolation.path,
      STRESS_LOAD_GENERATOR_ISOLATION_SHA256:
        identity.loadGeneratorIsolation.sha256,
      STRESS_REQUIRE_NOOP_CALIBRATION: "false",
      STRESS_NOOP_ENDPOINT: "",
    },
    stdio: ["ignore", stdout, stderr],
  });
  const workloadSpawnedAtMs = Date.now();
  let childResult = null;
  const childExit = new Promise((resolve) => {
    child.once("error", () => {
      childResult = {
        code: 1,
        signal: null,
        spawnError: true,
        exitedAtMs: Date.now(),
      };
      resolve(childResult);
    });
    child.once("close", (code, signal) => {
      childResult = {
        code: code ?? 1,
        signal,
        spawnError: false,
        exitedAtMs: Date.now(),
      };
      resolve(childResult);
    });
  });
  let failure = null;
  const durationMs = timing.durationSec * 1_000;
  try {
    let nextSampleAtMs = startedAtMs + timing.sampleIntervalMs;
    const workloadDeadlineMs =
      workloadSpawnedAtMs + durationMs + PHASE3_WORKLOAD_LIFECYCLE_GRACE_MS;
    while (childResult === null) {
      const outcome = await Promise.race([
        childExit.then(() => "exited"),
        sleep(nextSampleAtMs - Date.now()).then(() => "sample"),
      ]);
      if (outcome === "exited") break;
      samples.push(
        await captureSample({ startedAtMs, nodePid, readyUrl, metricsUrl }),
      );
      nextSampleAtMs += timing.sampleIntervalMs;
      if (Date.now() > workloadDeadlineMs) {
        throw new Error(
          "workload did not exit within the measured duration plus bounded drain grace",
        );
      }
    }
    samples.push(
      await captureSample({ startedAtMs, nodePid, readyUrl, metricsUrl }),
    );
  } catch (error) {
    failure = error instanceof Error ? error.message : String(error);
    if (childResult === null) child.kill("SIGTERM");
    await Promise.race([childExit.catch(() => undefined), sleep(10_000)]);
    try {
      if (samples.at(-1)?.observedAtMs < Date.now()) {
        samples.push(
          await captureSample({ startedAtMs, nodePid, readyUrl, metricsUrl }),
        );
      }
    } catch {
      // The original fail-closed sampling error is retained.
    }
  } finally {
    stdout.end();
    stderr.end();
  }

  const completedAtMs = Date.now();
  const workloadExitedAtMs = childResult?.exitedAtMs ?? null;
  let workload = {
    scriptPath: workloadScript,
    scriptSha256: sha256File(workloadScript),
    reportPath: workloadReportPath,
    reportSha256: null,
    reportBytes: null,
    reportSummary: null,
    submitRecords: null,
  };
  try {
    assertPhase3SoakCorpusPreflightCurrent({
      artifactPath: identity.corpusPreflight.path,
      artifactSha256: identity.corpusPreflight.sha256,
      expectedSourceIdentitySha256:
        identity.corpusPreflight.sourceIdentitySha256,
      expectedPhase1BindingSha256: identity.corpusPreflight.phase1BindingSha256,
      corpusPath: identity.phase1.corpus.path,
      indexPath: identity.phase1.corpus.indexPath,
      manifestPath: identity.phase1.corpus.manifestPath,
    });
  } catch (error) {
    failure ??=
      error instanceof Error
        ? `unable to revalidate corpus preflight: ${error.message}`
        : `unable to revalidate corpus preflight: ${String(error)}`;
  }
  if (fs.existsSync(workloadReportPath) && fs.existsSync(submitRecordsPath)) {
    try {
      assertRegularFile(workloadReportPath, "workload report evidence");
      workload = {
        ...workload,
        reportSha256: sha256File(workloadReportPath),
        reportBytes: fs.statSync(workloadReportPath).size,
        reportSummary: summarizePhase3WorkloadReport(
          readJson(workloadReportPath),
        ),
        submitRecords: await scanSubmitRecords(submitRecordsPath),
      };
    } catch (error) {
      failure ??=
        error instanceof Error
          ? `workload evidence invalid: ${error.message}`
          : `workload evidence invalid: ${String(error)}`;
    }
  }
  let sourceAtCompletion = null;
  try {
    sourceAtCompletion = await sourceIdentity(packageRoot);
    if (!sameSourceIdentity(sourceAtCompletion, identity.source)) {
      failure ??= "source identity changed during the live soak";
    }
  } catch (error) {
    failure ??=
      error instanceof Error
        ? `unable to revalidate source identity: ${error.message}`
        : `unable to revalidate source identity: ${String(error)}`;
  }
  const fullDuration =
    samples.at(-1)?.elapsedMs >= durationMs &&
    completedAtMs - startedAtMs >= durationMs &&
    Number(workload?.reportSummary?.measuredElapsedSec) >= timing.durationSec;
  const report = {
    schemaVersion: PHASE3_ARCHITECTURE_G_SOAK_SCHEMA,
    scenario: PHASE3_ARCHITECTURE_G_SOAK_SCENARIO,
    testOnly: timing.testOnly,
    configuredDurationSec: timing.durationSec,
    sampleIntervalMs: timing.sampleIntervalMs,
    startedAtMs,
    completedAtMs,
    preflight: {
      startedAtMs: preflight.startedAtMs,
      completedAtMs: preflight.completedAtMs,
      durationMs: preflight.durationMs,
      lifecycleStartedAtMs: preflight.lifecycleStartedAtMs,
      initialReadiness: preflight.initialReadiness,
      nodePreLifecycleRevalidation: preflight.nodePreLifecycleRevalidation,
    },
    identity,
    sourceAtCompletion,
    observation: {
      workloadSpawnedAtMs,
      workloadExitedAtMs,
      firstSampleAtMs: samples[0]?.observedAtMs ?? null,
      lastSampleAtMs: samples.at(-1)?.observedAtMs ?? null,
    },
    workload,
    termination: {
      completed:
        failure === null &&
        fullDuration &&
        childResult?.code === 0 &&
        childResult.signal === null,
      reason:
        failure === null && fullDuration
          ? "duration_completed"
          : "failed_closed",
      workloadExitCode: childResult?.code ?? null,
      workloadSignal: childResult?.signal ?? null,
      earlyExit: childResult !== null && !fullDuration,
      error: failure,
    },
    samples,
  };
  fs.writeFileSync(reportPath, `${JSON.stringify(report, null, 2)}\n`, {
    flag: "wx",
    mode: 0o600,
  });
  const verification = evaluatePhase3ArchitectureGSoakReport(report, {
    allowTestOnlyDuration: timing.testOnly,
  });
  fs.writeFileSync(
    verificationPath,
    `${JSON.stringify(verification, null, 2)}\n`,
    {
      flag: "wx",
      mode: 0o600,
    },
  );
  console.log(
    JSON.stringify({ reportPath, verificationPath, ...verification }, null, 2),
  );
  if (!verification.passed) process.exitCode = 1;
};

const isMain = process.argv[1] === scriptPath;
if (isMain) {
  run().catch((error) => {
    console.error(error instanceof Error ? error.message : String(error));
    process.exitCode = 1;
  });
}
