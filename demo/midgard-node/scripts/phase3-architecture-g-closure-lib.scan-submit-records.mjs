import { createHash } from "node:crypto";
import fs from "node:fs";

import {
  assertRegularFile,
  MAX_SUBMIT_RECORD_BYTES,
  validateSubmitRecord,
} from "./phase3-architecture-g-closure-lib.evaluate-exact-closure-identity-shape.mjs";

export const scanSubmitRecords = async (filePath) => {
  assertRegularFile(filePath, "submit-record evidence");
  const before = fs.lstatSync(filePath);
  const hash = createHash("sha256");
  let pending = Buffer.alloc(0);
  let bytes = 0;
  let recordCount = 0;
  let successCount = 0;
  let errorCount = 0;
  let timeoutCount = 0;
  const attemptSequence = createHash("sha256");
  const parseRecord = (line) => {
    recordCount += 1;
    if (line.byteLength === 0) {
      throw new Error(`empty submit-record at line ${recordCount.toString()}`);
    }
    if (line.byteLength > MAX_SUBMIT_RECORD_BYTES) {
      throw new Error(
        `submit-record line ${recordCount.toString()} exceeds ${MAX_SUBMIT_RECORD_BYTES.toString()} bytes`,
      );
    }
    let record;
    try {
      record = JSON.parse(line.toString("utf8"));
    } catch {
      throw new Error(
        `invalid submit-record JSON at line ${recordCount.toString()}`,
      );
    }
    validateSubmitRecord(record, recordCount);
    attemptSequence.update(`${recordCount.toString()}\0${record.txHash}\n`);
    if (record.error === null) successCount += 1;
    else errorCount += 1;
    if (
      record.error !== null &&
      /timeout|timed out/iu.test(String(record.error))
    ) {
      timeoutCount += 1;
    }
  };
  for await (const chunk of fs.createReadStream(filePath)) {
    hash.update(chunk);
    bytes += chunk.byteLength;
    pending =
      pending.byteLength === 0
        ? chunk
        : Buffer.concat(
            [pending, chunk],
            pending.byteLength + chunk.byteLength,
          );
    let newline = pending.indexOf(0x0a);
    while (newline >= 0) {
      let line = pending.subarray(0, newline);
      if (line.at(-1) === 0x0d) line = line.subarray(0, -1);
      parseRecord(line);
      pending = pending.subarray(newline + 1);
      newline = pending.indexOf(0x0a);
    }
    if (pending.byteLength > MAX_SUBMIT_RECORD_BYTES) {
      throw new Error(
        `submit-record line ${(recordCount + 1).toString()} exceeds ${MAX_SUBMIT_RECORD_BYTES.toString()} bytes`,
      );
    }
  }
  if (pending.byteLength > 0) {
    if (pending.at(-1) === 0x0d) pending = pending.subarray(0, -1);
    parseRecord(pending);
  }
  const after = fs.lstatSync(filePath);
  if (
    !after.isFile() ||
    after.isSymbolicLink() ||
    after.dev !== before.dev ||
    after.ino !== before.ino ||
    after.size !== before.size ||
    after.mtimeMs !== before.mtimeMs ||
    bytes !== after.size
  ) {
    throw new Error("submit-record evidence changed while it was scanned");
  }
  if (recordCount === 0) {
    throw new Error("submit-record evidence contains no records");
  }
  return {
    path: filePath,
    sha256: hash.digest("hex"),
    bytes,
    recordCount,
    successCount,
    errorCount,
    timeoutCount,
    attemptSequenceSha256: attemptSequence.digest("hex"),
  };
};

export const summarizePhase3WorkloadReport = (report) => {
  if (report?.benchmark !== "midgard-l2-throughput" || report?.version !== 1) {
    throw new Error("workload emitted an unexpected report schema");
  }
  const primary = new Set(report?.summary?.primaryStageNames ?? []);
  const primaryStages = (report?.stages ?? []).filter((stage) =>
    primary.has(stage.name),
  );
  const logicalSubmitAttempts = primaryStages.reduce(
    (sum, stage) => sum + Number(stage?.logicalSubmitAttempts ?? 0),
    0,
  );
  return {
    scenario: report?.scenario,
    scenarioClass: report?.scenarioClass,
    benchmarkMode: report?.config?.benchmarkMode,
    formalBenchmark: report?.config?.formalBenchmark,
    targetAcceptedTps: report?.summary?.targetAcceptedTps,
    openLoopRateTps: report?.config?.openLoopRate,
    measuredDurationSec: report?.config?.measuredSec,
    warmupTxs: report?.config?.warmupTxs,
    warmupSec: report?.config?.warmupSec,
    cooldownSec: report?.config?.cooldownSec,
    drainTimeoutSec: report?.config?.drainTimeoutSec,
    offeredRateMinRatio: report?.config?.offeredRateMinRatio,
    acceptedRateMinRatio: report?.config?.acceptedRateMinRatio,
    nodeSaturationMinRatio: report?.config?.nodeSaturationMinRatio,
    loadGenerator: report?.config?.loadGenerator,
    calibration: report?.summary?.calibration,
    corpus: report?.summary?.corpus,
    measuredElapsedSec: report?.summary?.measuredElapsedSec,
    offeredRatePerSec: report?.summary?.queuedSubmitSuccessPerSec,
    acceptedRatePerSec: report?.summary?.acceptedPerSecond,
    submitted: report?.summary?.submitted,
    logicalSubmitAttempts,
    physicalSubmitAttempts: report?.summary?.physicalSubmitAttempts,
    submitErrors: report?.summary?.submitErrors,
    rejectedDelta: report?.summary?.rejectDelta,
    missingRequiredMetrics: report?.summary?.missingRequiredMetrics,
    allPrimaryStagesPassed:
      primaryStages.length > 0 &&
      primaryStages.every((stage) => stage?.evaluation?.passed === true),
    allPrimaryDrainsCompleted:
      primaryStages.length > 0 &&
      primaryStages.every((stage) => stage?.drain?.completed === true),
    primaryStageMeasurements: primaryStages.map((stage) => ({
      name: stage.name,
      targetRateTps: stage.targetRateTps,
      startedAtMs: Date.parse(stage.startedAtIso),
      endedAtMs: Date.parse(stage.endedAtIso),
      measuredElapsedSec: stage.measuredElapsedSec,
      logicalSubmitAttempts: stage.logicalSubmitAttempts,
      physicalSubmitAttempts: stage.physicalSubmitAttempts,
      submitted: stage.submitted,
      submitErrors: stage.submitErrors,
      offeredRatePerSec: stage.queuedSubmitSuccessPerSec,
      acceptedRatePerSec: stage.measuredAcceptedTps,
      nodeSaturationRatio: stage?.evaluation?.nodeSaturation?.ratio,
      nodeSaturationMinRatio: stage?.evaluation?.nodeSaturation?.minRatio,
      nodeSaturationPassed: stage?.evaluation?.nodeSaturation?.passed,
      drainCompleted: stage?.drain?.completed,
      drainElapsedMs: stage?.drain?.elapsedMs,
    })),
  };
};

export const MAX_RETAINED_LOG_LINE_CHARS = 64 * 1024;

const SENSITIVE_LOG_LABEL =
  /(?:seed|mnemonic|recovery[ _-]*phrase|private[ _-]*key|secret|password|signed[ _-]*cbor|tx[ _-]*cbor|raw[ _-]*cbor|cbor[ _-]*hex)/iu;

const LONG_HEX_OR_BASE64 =
  /(?:\b[0-9a-f]{128,}\b|\b[A-Za-z0-9+/]{256,}={0,2}\b)/u;

export const SECRET_LOG_REDACTION = "[REDACTED secret-bearing driver output]";

export const containsSensitiveDriverOutput = (line) =>
  SENSITIVE_LOG_LABEL.test(line) || LONG_HEX_OR_BASE64.test(line);
