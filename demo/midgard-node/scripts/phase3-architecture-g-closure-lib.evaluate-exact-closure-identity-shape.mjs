import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";
import { promisify } from "node:util";

export const execFileAsync = promisify(execFile);

export const SHA256 = /^[0-9a-f]{64}$/u;

export const GIT_SHA = /^[0-9a-f]{40}$/u;

export const NODE_VERSION = "v22.22.2";

export const isCanonicalAbsolutePath = (value) =>
  typeof value === "string" &&
  path.isAbsolute(value) &&
  path.resolve(value) === value;

export const hasExactV1JsonKeys = (value, expectedKeys) => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return false;
  }
  const actual = Object.keys(value).sort();
  const expected = [...expectedKeys].sort();
  return (
    actual.length === expected.length &&
    actual.every((key, index) => key === expected[index])
  );
};

export const evaluateExactSourceIdentityShape = (
  source,
  label = "source identity",
) =>
  hasExactV1JsonKeys(source, [
    "gitCommit",
    "gitStatusSha256",
    "trackedDiffSha256",
    "sourceTreeSha256",
    "sourceTreeFileCount",
    "nodeVersion",
    "nodeExecutablePath",
    "nodeExecutableSha256",
  ])
    ? []
    : [`${label} must use the exact V1 keys`];

export const evaluateExactClosureIdentityShape = (
  identity,
  extraRootKeys = [],
) => {
  const reasons = [];
  const check = (value, expectedKeys, label) => {
    if (!hasExactV1JsonKeys(value, expectedKeys)) {
      reasons.push(`${label} must use the exact V1 keys`);
    }
  };
  check(
    identity,
    [
      "source",
      "runtime",
      "deployment",
      "phase1",
      "ownerBinary",
      "tooling",
      ...extraRootKeys,
    ],
    "closure identity",
  );
  reasons.push(...evaluateExactSourceIdentityShape(identity?.source));
  check(
    identity?.runtime,
    [
      "path",
      "sha256",
      "schemaVersion",
      "deploymentManifestSha256",
      "nodeImageId",
    ],
    "runtime identity",
  );
  check(
    identity?.deployment,
    ["path", "sha256", "schemaVersion", "manifestId"],
    "deployment identity",
  );
  check(
    identity?.phase1,
    [
      "path",
      "sha256",
      "schemaVersion",
      "deploymentManifestId",
      "nodeImageId",
      "nodeContainerId",
      "corpus",
    ],
    "Phase 1 identity",
  );
  check(
    identity?.phase1?.corpus,
    [
      "path",
      "indexPath",
      "manifestPath",
      "sliceId",
      "corpusSha256",
      "indexSha256",
      "manifestSha256",
    ],
    "Phase 1 corpus identity",
  );
  check(
    identity?.ownerBinary,
    [
      "path",
      "sha256",
      "expectedSha256",
      "sha256ManifestPath",
      "sha256ManifestSha256",
    ],
    "owner-binary identity",
  );
  check(
    identity?.tooling,
    ["runnerPath", "runnerSha256", "verifierPath", "verifierSha256"],
    "closure tooling identity",
  );
  return reasons;
};

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

export const MAX_SUBMIT_RECORD_BYTES = 1024 * 1024;

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

export const TX_HASH = /^[0-9a-f]{64}$/u;

const finiteNonNegative = (value) =>
  typeof value === "number" && Number.isFinite(value) && value >= 0;

export const validateSubmitRecord = (record, lineNumber) => {
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
