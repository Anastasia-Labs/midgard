import { createHash } from "node:crypto";
import { execFile } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { Writable } from "node:stream";
import { finished } from "node:stream/promises";
import { StringDecoder } from "node:string_decoder";
import { promisify } from "node:util";

import { canonicalJsonSha256 } from "./phase4-environment-fingerprint-lib.mjs";

const execFileAsync = promisify(execFile);

export const SHA256 = /^[0-9a-f]{64}$/u;
export const GIT_SHA = /^[0-9a-f]{40}$/u;
export const NODE_VERSION = "v22.22.2";

export const sha256Bytes = (bytes) =>
  createHash("sha256").update(bytes).digest("hex");

export const sha256File = (filePath) => {
  const hash = createHash("sha256");
  const descriptor = fs.openSync(filePath, "r");
  const buffer = Buffer.allocUnsafe(1024 * 1024);
  const before = fs.fstatSync(descriptor);
  let totalBytes = 0;
  try {
    let bytesRead = fs.readSync(descriptor, buffer, 0, buffer.length, null);
    while (bytesRead > 0) {
      hash.update(buffer.subarray(0, bytesRead));
      totalBytes += bytesRead;
      bytesRead = fs.readSync(descriptor, buffer, 0, buffer.length, null);
    }
    const after = fs.fstatSync(descriptor);
    if (
      after.dev !== before.dev ||
      after.ino !== before.ino ||
      after.size !== before.size ||
      after.mtimeMs !== before.mtimeMs ||
      totalBytes !== after.size
    ) {
      throw new Error(`artifact changed while hashing ${filePath}`);
    }
  } finally {
    fs.closeSync(descriptor);
  }
  return hash.digest("hex");
};

export const readJson = (filePath) =>
  JSON.parse(fs.readFileSync(filePath, "utf8"));

export const requiredArg = (argv, name) => {
  const index = argv.indexOf(name);
  const value = index < 0 ? undefined : argv[index + 1];
  if (value === undefined || value.startsWith("--")) {
    throw new Error(`missing required ${name}`);
  }
  return value;
};

export const absoluteArg = (argv, name) => {
  const value = requiredArg(argv, name);
  if (!path.isAbsolute(value)) throw new Error(`${name} must be absolute`);
  return path.resolve(value);
};

export const assertRegularFile = (filePath, label = filePath) => {
  const stat = fs.lstatSync(filePath);
  if (!stat.isFile() || stat.isSymbolicLink()) {
    throw new Error(`${label} must be a regular, non-symlink file`);
  }
};

const MAX_SUBMIT_RECORD_BYTES = 1024 * 1024;
const SUBMIT_RECORD_KEYS = Object.freeze([
  "error",
  "latencyMs",
  "responseTxId",
  "scheduleSlipMs",
  "scheduledAtMs",
  "statusCode",
  "submittedAtMs",
  "txHash",
]);
const TX_HASH = /^[0-9a-f]{64}$/u;

const finiteNonNegative = (value) =>
  typeof value === "number" && Number.isFinite(value) && value >= 0;

const validateSubmitRecord = (record, lineNumber) => {
  const fail = () => {
    throw new Error(
      `invalid submit-record schema at line ${lineNumber.toString()}`,
    );
  };
  if (typeof record !== "object" || record === null || Array.isArray(record)) {
    fail();
  }
  const keys = Object.keys(record).sort();
  if (
    keys.length !== SUBMIT_RECORD_KEYS.length ||
    keys.some((key, index) => key !== SUBMIT_RECORD_KEYS[index])
  ) {
    fail();
  }
  if (
    !TX_HASH.test(record.txHash ?? "") ||
    !Number.isSafeInteger(record.scheduledAtMs) ||
    record.scheduledAtMs <= 0 ||
    !Number.isSafeInteger(record.submittedAtMs) ||
    record.submittedAtMs <= 0 ||
    record.submittedAtMs < record.scheduledAtMs ||
    !finiteNonNegative(record.scheduleSlipMs) ||
    !finiteNonNegative(record.latencyMs) ||
    !(
      record.statusCode === null ||
      (Number.isSafeInteger(record.statusCode) &&
        record.statusCode >= 100 &&
        record.statusCode <= 599)
    ) ||
    !(
      record.responseTxId === null || TX_HASH.test(record.responseTxId ?? "")
    ) ||
    !(
      record.error === null ||
      (typeof record.error === "string" && record.error.length > 0)
    )
  ) {
    fail();
  }
};

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
  if (report?.benchmark !== "midgard-l2-throughput" || report?.version !== 2) {
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

const MAX_RETAINED_LOG_LINE_CHARS = 64 * 1024;
const SENSITIVE_LOG_LABEL =
  /(?:seed|mnemonic|recovery[ _-]*phrase|private[ _-]*key|secret|password|signed[ _-]*cbor|tx[ _-]*cbor|raw[ _-]*cbor|cbor[ _-]*hex)/iu;
const LONG_HEX_OR_BASE64 =
  /(?:\b[0-9a-f]{128,}\b|\b[A-Za-z0-9+/]{256,}={0,2}\b)/u;
const SECRET_LOG_REDACTION = "[REDACTED secret-bearing driver output]";

export const containsSensitiveDriverOutput = (line) =>
  SENSITIVE_LOG_LABEL.test(line) || LONG_HEX_OR_BASE64.test(line);

/**
 * Retain driver diagnostics only after line-buffered secret scanning. Sensitive
 * and oversized lines are replaced before any bytes reach the evidence file.
 */
export const createSecretScanningLog = (filePath) => {
  if (!path.isAbsolute(filePath)) {
    throw new Error("secret-scanned log path must be absolute");
  }
  if (fs.existsSync(filePath)) {
    throw new Error(`refusing to overwrite ${filePath}`);
  }
  const descriptor = fs.openSync(
    filePath,
    fs.constants.O_CREAT | fs.constants.O_EXCL | fs.constants.O_WRONLY,
    0o600,
  );
  const decoder = new StringDecoder("utf8");
  let pending = "";
  let discardingOversizedLine = false;
  let sensitiveLineCount = 0;
  let oversizedLineCount = 0;
  let retainedLineCount = 0;
  let closed = false;

  const writeRetained = (value) => {
    fs.writeSync(descriptor, value);
  };
  const redact = ({ oversized = false } = {}) => {
    sensitiveLineCount += 1;
    if (oversized) oversizedLineCount += 1;
    writeRetained(`${SECRET_LOG_REDACTION}\n`);
  };
  const retainLine = (line, hasNewline) => {
    if (containsSensitiveDriverOutput(line)) {
      redact();
      return;
    }
    retainedLineCount += 1;
    writeRetained(hasNewline ? `${line}\n` : line);
  };
  const acceptText = (text) => {
    pending += text;
    while (true) {
      const newline = pending.indexOf("\n");
      if (discardingOversizedLine) {
        if (newline < 0) {
          pending = "";
          return;
        }
        pending = pending.slice(newline + 1);
        discardingOversizedLine = false;
        continue;
      }
      if (newline >= 0) {
        const line = pending.slice(0, newline).replace(/\r$/u, "");
        pending = pending.slice(newline + 1);
        if (line.length > MAX_RETAINED_LOG_LINE_CHARS)
          redact({ oversized: true });
        else retainLine(line, true);
        continue;
      }
      if (pending.length > MAX_RETAINED_LOG_LINE_CHARS) {
        pending = "";
        discardingOversizedLine = true;
        redact({ oversized: true });
      }
      return;
    }
  };

  const stream = new Writable({
    write(chunk, _encoding, callback) {
      try {
        acceptText(decoder.write(chunk));
        callback();
      } catch (error) {
        callback(error);
      }
    },
    final(callback) {
      try {
        acceptText(decoder.end());
        if (!discardingOversizedLine && pending.length > 0) {
          retainLine(pending.replace(/\r$/u, ""), false);
        }
        pending = "";
        fs.fsyncSync(descriptor);
        fs.closeSync(descriptor);
        closed = true;
        callback();
      } catch (error) {
        callback(error);
      }
    },
    destroy(error, callback) {
      if (!closed) {
        try {
          fs.closeSync(descriptor);
        } catch {
          // Preserve the original stream error.
        }
        closed = true;
      }
      callback(error);
    },
  });
  const completion = finished(stream).then(
    () => true,
    () => false,
  );

  return {
    stream,
    async complete() {
      if (!(await completion)) {
        throw new Error("secret-scanned log failed closed");
      }
      return {
        path: filePath,
        sha256: sha256File(filePath),
        bytes: fs.statSync(filePath).size,
        secretScan: {
          schemaVersion: "midgard-secret-scanned-log-v1",
          passed: sensitiveLineCount === 0,
          sensitiveLineCount,
          oversizedLineCount,
          retainedLineCount,
        },
      };
    },
  };
};

export const writeAtomicImmutableJson = (filePath, value) => {
  if (!path.isAbsolute(filePath))
    throw new Error("output path must be absolute");
  if (fs.existsSync(filePath))
    throw new Error(`refusing to overwrite ${filePath}`);
  const directory = path.dirname(filePath);
  fs.mkdirSync(directory, { recursive: true, mode: 0o700 });
  const temporaryPath = path.join(
    directory,
    `.${path.basename(filePath)}.${process.pid.toString()}.${Date.now().toString()}.tmp`,
  );
  const bytes = `${JSON.stringify(value, null, 2)}\n`;
  const descriptor = fs.openSync(
    temporaryPath,
    fs.constants.O_CREAT | fs.constants.O_EXCL | fs.constants.O_WRONLY,
    0o600,
  );
  try {
    fs.writeFileSync(descriptor, bytes);
    fs.fsyncSync(descriptor);
  } finally {
    fs.closeSync(descriptor);
  }
  try {
    fs.linkSync(temporaryPath, filePath);
  } finally {
    fs.unlinkSync(temporaryPath);
  }
  const directoryDescriptor = fs.openSync(directory, "r");
  try {
    fs.fsyncSync(directoryDescriptor);
  } finally {
    fs.closeSync(directoryDescriptor);
  }
};

const normalizedImageId = (value) =>
  String(value ?? "").replace(/^sha256:/u, "");

export const capturePhase1CorpusIdentity = (phase1) => {
  const corpus = phase1?.corpus;
  const fields = [
    ["path", "corpusSha256", "Phase 1 corpus"],
    ["indexPath", "indexSha256", "Phase 1 corpus index"],
    ["manifestPath", "manifestSha256", "Phase 1 corpus manifest"],
  ];
  const identity = {};
  for (const [pathField, shaField, label] of fields) {
    const filePath = corpus?.[pathField];
    const expectedSha256 = corpus?.[shaField];
    if (
      typeof filePath !== "string" ||
      !path.isAbsolute(filePath) ||
      !SHA256.test(expectedSha256 ?? "")
    ) {
      throw new Error(`${label} identity is incomplete`);
    }
    const resolvedPath = path.resolve(filePath);
    assertRegularFile(resolvedPath, label);
    if (sha256File(resolvedPath) !== expectedSha256) {
      throw new Error(`${label} does not match its bound SHA-256`);
    }
    identity[pathField] = resolvedPath;
    identity[shaField] = expectedSha256;
  }
  if (
    typeof corpus?.sliceId !== "string" ||
    corpus.sliceId.trim().length === 0
  ) {
    throw new Error("Phase 1 corpus slice identity is incomplete");
  }
  const stressEnv = phase1?.stressCorpusEnv;
  if (
    stressEnv?.STRESS_CORPUS_PATH !== identity.path ||
    stressEnv?.STRESS_CORPUS_INDEX_PATH !== identity.indexPath ||
    stressEnv?.STRESS_CORPUS_MANIFEST_PATH !== identity.manifestPath ||
    stressEnv?.STRESS_CORPUS_SLICE_ID !== corpus.sliceId
  ) {
    throw new Error(
      "Phase 1 stress corpus environment diverges from its bound corpus",
    );
  }
  return { ...identity, sliceId: corpus.sliceId };
};

export const sourceIdentity = async (packageRoot) => {
  const root = path.resolve(packageRoot);
  const nodeExecutablePath = fs.realpathSync(process.execPath);
  const [
    { stdout: head },
    { stdout: status },
    { stdout: diff },
    { stdout: files },
  ] = await Promise.all([
    execFileAsync("git", ["rev-parse", "HEAD"], { cwd: packageRoot }),
    execFileAsync("git", ["status", "--porcelain=v1", "-z"], {
      cwd: packageRoot,
      encoding: "buffer",
      maxBuffer: 64 * 1024 * 1024,
    }),
    execFileAsync("git", ["diff", "--binary", "--no-ext-diff", "HEAD", "--"], {
      cwd: packageRoot,
      encoding: "buffer",
      maxBuffer: 128 * 1024 * 1024,
    }),
    execFileAsync(
      "git",
      ["ls-files", "--cached", "--others", "--exclude-standard", "-z"],
      {
        cwd: packageRoot,
        encoding: "buffer",
        maxBuffer: 64 * 1024 * 1024,
      },
    ),
  ]);
  const paths = files.toString("utf8").split("\0").filter(Boolean).sort();
  const tree = createHash("sha256");
  for (const relativePath of paths) {
    const absolutePath = path.resolve(root, relativePath);
    const name = Buffer.from(relativePath);
    let bytes;
    if (!fs.existsSync(absolutePath)) {
      bytes = Buffer.from("MIDGARD-SOURCE-MISSING-v1");
    } else {
      const stat = fs.lstatSync(absolutePath);
      if (stat.isSymbolicLink()) {
        bytes = Buffer.from(
          `MIDGARD-SOURCE-SYMLINK-v1:${fs.readlinkSync(absolutePath)}`,
        );
      } else if (stat.isFile()) {
        bytes = fs.readFileSync(absolutePath);
      } else {
        throw new Error(
          `source identity refuses non-file path ${relativePath}`,
        );
      }
    }
    const lengths = Buffer.allocUnsafe(12);
    lengths.writeUInt32LE(name.length, 0);
    lengths.writeBigUInt64LE(BigInt(bytes.length), 4);
    tree.update(lengths).update(name).update(bytes);
  }
  return {
    gitCommit: head.trim(),
    gitStatusSha256: sha256Bytes(status),
    trackedDiffSha256: sha256Bytes(diff),
    sourceTreeSha256: tree.digest("hex"),
    sourceTreeFileCount: paths.length,
    nodeVersion: process.version,
    nodeExecutablePath,
    nodeExecutableSha256: sha256File(nodeExecutablePath),
  };
};

export const captureClosureIdentity = async ({
  packageRoot,
  runtimePath,
  deploymentPath,
  phase1Path,
  ownerBinaryPath,
  ownerSha256ManifestPath,
  runnerPath,
  verifierPath,
}) => {
  for (const [label, filePath] of [
    ["runtime fingerprint", runtimePath],
    ["deployment manifest", deploymentPath],
    ["Phase 1 binding", phase1Path],
    ["owner binary", ownerBinaryPath],
    ["owner SHA-256 manifest", ownerSha256ManifestPath],
    ["runner", runnerPath],
    ["verifier", verifierPath],
  ]) {
    assertRegularFile(filePath, label);
  }
  const runtime = readJson(runtimePath);
  const deployment = readJson(deploymentPath);
  const phase1 = readJson(phase1Path);
  const ownerSha256 = sha256File(ownerBinaryPath);
  const expectedOwnerSha256 = fs
    .readFileSync(ownerSha256ManifestPath, "utf8")
    .trim()
    .split(/\s+/u)[0]
    ?.toLowerCase();
  const deploymentSha256 = sha256File(deploymentPath);
  const phase1Corpus = capturePhase1CorpusIdentity(phase1);
  if (
    runtime?.schemaVersion !== "midgard-phase4-environment-artifact-v1" ||
    runtime?.document?.schemaVersion !== "midgard-phase4-environment-v1" ||
    runtime?.documentSha256 !== canonicalJsonSha256(runtime.document) ||
    runtime?.document?.deploymentManifest?.sha256 !== deploymentSha256
  ) {
    throw new Error(
      "runtime fingerprint does not bind the deployment manifest",
    );
  }
  if (
    phase1?.schemaVersion !== "midgard-phase1-live-corpus-binding-v2" ||
    phase1?.deploymentManifestId !== deployment?.manifestId ||
    normalizedImageId(phase1?.nodeImageId) !==
      normalizedImageId(runtime?.document?.node?.imageId)
  ) {
    throw new Error(
      "Phase 1 binding does not match runtime/deployment identity",
    );
  }
  if (
    !SHA256.test(expectedOwnerSha256 ?? "") ||
    ownerSha256 !== expectedOwnerSha256
  ) {
    throw new Error("owner binary does not match its SHA-256 manifest");
  }
  return {
    source: await sourceIdentity(packageRoot),
    runtime: {
      path: runtimePath,
      sha256: sha256File(runtimePath),
      schemaVersion: runtime.schemaVersion,
      deploymentManifestSha256: deploymentSha256,
      nodeImageId: phase1.nodeImageId,
    },
    deployment: {
      path: deploymentPath,
      sha256: deploymentSha256,
      schemaVersion: deployment.schemaVersion,
      manifestId: deployment.manifestId,
    },
    phase1: {
      path: phase1Path,
      sha256: sha256File(phase1Path),
      schemaVersion: phase1.schemaVersion,
      deploymentManifestId: phase1.deploymentManifestId,
      nodeImageId: phase1.nodeImageId,
      nodeContainerId: phase1.nodeContainerId,
      corpus: phase1Corpus,
    },
    ownerBinary: {
      path: ownerBinaryPath,
      sha256: ownerSha256,
      expectedSha256: expectedOwnerSha256,
      sha256ManifestPath: ownerSha256ManifestPath,
      sha256ManifestSha256: sha256File(ownerSha256ManifestPath),
    },
    tooling: {
      runnerPath,
      runnerSha256: sha256File(runnerPath),
      verifierPath,
      verifierSha256: sha256File(verifierPath),
    },
  };
};

export const evaluateClosureIdentity = (identity) => {
  const reasons = [];
  const source = identity?.source;
  if (
    !GIT_SHA.test(source?.gitCommit ?? "") ||
    !SHA256.test(source?.gitStatusSha256 ?? "") ||
    !SHA256.test(source?.trackedDiffSha256 ?? "") ||
    !SHA256.test(source?.sourceTreeSha256 ?? "") ||
    !Number.isSafeInteger(source?.sourceTreeFileCount) ||
    source.sourceTreeFileCount <= 0 ||
    source?.nodeVersion !== NODE_VERSION
  ) {
    reasons.push(
      `source identity is incomplete or Node is not ${NODE_VERSION}`,
    );
  }
  if (
    typeof source?.nodeExecutablePath !== "string" ||
    !path.isAbsolute(source.nodeExecutablePath) ||
    !SHA256.test(source?.nodeExecutableSha256 ?? "")
  ) {
    reasons.push("Node executable identity is incomplete");
  }
  for (const label of ["runtime", "deployment", "phase1"]) {
    const value = identity?.[label];
    if (
      typeof value?.path !== "string" ||
      !path.isAbsolute(value.path) ||
      !SHA256.test(value?.sha256 ?? "") ||
      typeof value?.schemaVersion !== "string" ||
      value.schemaVersion.length === 0
    ) {
      reasons.push(`${label} identity is incomplete`);
    }
  }
  if (
    identity?.runtime?.schemaVersion !==
      "midgard-phase4-environment-artifact-v1" ||
    identity?.phase1?.schemaVersion !==
      "midgard-phase1-live-corpus-binding-v2" ||
    !SHA256.test(identity?.deployment?.manifestId ?? "") ||
    !/^(?:sha256:)?[0-9a-f]{64}$/u.test(identity?.phase1?.nodeImageId ?? "")
  ) {
    reasons.push("runtime/deployment/Phase 1 schemas or IDs are invalid");
  }
  if (
    identity?.runtime?.deploymentManifestSha256 !==
      identity?.deployment?.sha256 ||
    normalizedImageId(identity?.runtime?.nodeImageId) !==
      normalizedImageId(identity?.phase1?.nodeImageId) ||
    identity?.phase1?.deploymentManifestId !==
      identity?.deployment?.manifestId ||
    typeof identity?.phase1?.nodeContainerId !== "string" ||
    identity.phase1.nodeContainerId.length === 0 ||
    typeof identity?.phase1?.corpus?.sliceId !== "string" ||
    identity.phase1.corpus.sliceId.length === 0
  ) {
    reasons.push("runtime, deployment, and Phase 1 identities diverge");
  }
  for (const [pathField, shaField] of [
    ["path", "corpusSha256"],
    ["indexPath", "indexSha256"],
    ["manifestPath", "manifestSha256"],
  ]) {
    const corpus = identity?.phase1?.corpus;
    if (
      typeof corpus?.[pathField] !== "string" ||
      !path.isAbsolute(corpus[pathField]) ||
      !SHA256.test(corpus?.[shaField] ?? "")
    ) {
      reasons.push("Phase 1 corpus identity is incomplete");
      break;
    }
  }
  const owner = identity?.ownerBinary;
  if (
    typeof owner?.path !== "string" ||
    !path.isAbsolute(owner.path) ||
    !SHA256.test(owner?.sha256 ?? "") ||
    owner.sha256 !== owner?.expectedSha256 ||
    typeof owner?.sha256ManifestPath !== "string" ||
    !path.isAbsolute(owner.sha256ManifestPath) ||
    !SHA256.test(owner?.sha256ManifestSha256 ?? "")
  ) {
    reasons.push("pinned owner binary identity is invalid");
  }
  const tooling = identity?.tooling;
  if (
    typeof tooling?.runnerPath !== "string" ||
    !path.isAbsolute(tooling.runnerPath) ||
    !SHA256.test(tooling?.runnerSha256 ?? "") ||
    typeof tooling?.verifierPath !== "string" ||
    !path.isAbsolute(tooling.verifierPath) ||
    !SHA256.test(tooling?.verifierSha256 ?? "")
  ) {
    reasons.push("runner/verifier identity is incomplete");
  }
  return reasons;
};

export const evaluateClosureIdentityArtifacts = (
  identity,
  { skipPhase1Corpus = false } = {},
) => {
  const reasons = [];
  const artifacts = [
    [
      "Node executable",
      identity?.source?.nodeExecutablePath,
      identity?.source?.nodeExecutableSha256,
    ],
    ["runtime", identity?.runtime?.path, identity?.runtime?.sha256],
    ["deployment", identity?.deployment?.path, identity?.deployment?.sha256],
    ["Phase 1", identity?.phase1?.path, identity?.phase1?.sha256],
    [
      "Phase 1 corpus",
      identity?.phase1?.corpus?.path,
      identity?.phase1?.corpus?.corpusSha256,
    ],
    [
      "Phase 1 corpus index",
      identity?.phase1?.corpus?.indexPath,
      identity?.phase1?.corpus?.indexSha256,
    ],
    [
      "Phase 1 corpus manifest",
      identity?.phase1?.corpus?.manifestPath,
      identity?.phase1?.corpus?.manifestSha256,
    ],
    ["owner", identity?.ownerBinary?.path, identity?.ownerBinary?.sha256],
    [
      "owner SHA manifest",
      identity?.ownerBinary?.sha256ManifestPath,
      identity?.ownerBinary?.sha256ManifestSha256,
    ],
    ["runner", identity?.tooling?.runnerPath, identity?.tooling?.runnerSha256],
    [
      "verifier",
      identity?.tooling?.verifierPath,
      identity?.tooling?.verifierSha256,
    ],
  ];
  for (const [label, filePath, expectedSha256] of artifacts) {
    if (skipPhase1Corpus && label === "Phase 1 corpus") continue;
    if (
      typeof filePath !== "string" ||
      !path.isAbsolute(filePath) ||
      !SHA256.test(expectedSha256 ?? "") ||
      !fs.existsSync(filePath)
    ) {
      reasons.push(`${label} bound artifact is missing`);
      continue;
    }
    const stat = fs.lstatSync(filePath);
    if (!stat.isFile() || stat.isSymbolicLink()) {
      reasons.push(`${label} bound artifact is not a regular file`);
    } else if (sha256File(filePath) !== expectedSha256) {
      reasons.push(`${label} bound artifact SHA-256 changed`);
    }
  }
  try {
    const runtime = readJson(identity.runtime.path);
    const deployment = readJson(identity.deployment.path);
    const phase1 = readJson(identity.phase1.path);
    const deploymentSha256 = sha256File(identity.deployment.path);
    if (
      runtime?.schemaVersion !== "midgard-phase4-environment-artifact-v1" ||
      runtime?.document?.schemaVersion !== "midgard-phase4-environment-v1" ||
      runtime?.documentSha256 !== canonicalJsonSha256(runtime.document) ||
      runtime?.document?.deploymentManifest?.sha256 !== deploymentSha256 ||
      deployment?.manifestId !== identity.deployment.manifestId ||
      phase1?.schemaVersion !== "midgard-phase1-live-corpus-binding-v2" ||
      phase1?.deploymentManifestId !== deployment?.manifestId ||
      normalizedImageId(phase1?.nodeImageId) !==
        normalizedImageId(runtime?.document?.node?.imageId) ||
      phase1?.nodeContainerId !== identity.phase1.nodeContainerId ||
      phase1?.corpus?.path !== identity.phase1.corpus.path ||
      phase1?.corpus?.indexPath !== identity.phase1.corpus.indexPath ||
      phase1?.corpus?.manifestPath !== identity.phase1.corpus.manifestPath ||
      phase1?.corpus?.sliceId !== identity.phase1.corpus.sliceId ||
      phase1?.corpus?.corpusSha256 !== identity.phase1.corpus.corpusSha256 ||
      phase1?.corpus?.indexSha256 !== identity.phase1.corpus.indexSha256 ||
      phase1?.corpus?.manifestSha256 !== identity.phase1.corpus.manifestSha256
    ) {
      reasons.push(
        "bound runtime/deployment/Phase 1 artifact contents diverge",
      );
    }
    const manifestOwnerSha256 = fs
      .readFileSync(identity.ownerBinary.sha256ManifestPath, "utf8")
      .trim()
      .split(/\s+/u)[0]
      ?.toLowerCase();
    if (manifestOwnerSha256 !== identity.ownerBinary.sha256) {
      reasons.push("bound owner SHA manifest content diverges");
    }
  } catch {
    reasons.push("bound identity artifact content is unreadable");
  }
  return reasons;
};

export const sameSourceIdentity = (left, right) =>
  left?.gitCommit === right?.gitCommit &&
  left?.gitStatusSha256 === right?.gitStatusSha256 &&
  left?.trackedDiffSha256 === right?.trackedDiffSha256 &&
  left?.sourceTreeSha256 === right?.sourceTreeSha256 &&
  left?.sourceTreeFileCount === right?.sourceTreeFileCount &&
  left?.nodeVersion === right?.nodeVersion &&
  left?.nodeExecutablePath === right?.nodeExecutablePath &&
  left?.nodeExecutableSha256 === right?.nodeExecutableSha256;
