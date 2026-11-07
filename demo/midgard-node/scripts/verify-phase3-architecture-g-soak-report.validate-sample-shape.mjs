import { createHash } from "node:crypto";

import {
  hasExactV1JsonKeys,
  isCanonicalAbsolutePath,
  SHA256,
} from "./phase3-architecture-g-closure-lib.mjs";

export const PHASE3_ARCHITECTURE_G_SOAK_SCHEMA =
  "midgard-phase3-architecture-g-live-soak-v1";

export const PHASE3_ARCHITECTURE_G_SOAK_SCENARIO =
  "phase3-architecture-g-live-soak-24h-v1";

export const PHASE3_ARCHITECTURE_G_SOAK_DURATION_SEC = 86_400;

export const PHASE3_ARCHITECTURE_G_SAMPLE_INTERVAL_MS = 60_000;

export const PHASE3_ARCHITECTURE_G_MAX_SAMPLE_GAP_MS = 90_000;

export const PHASE3_OWNER_MAX_RESIDENT_NODES = 2_000_000;

export const PHASE3_OWNER_MAX_RESIDENT_BYTES = 2 * 1024 ** 3;

export const PHASE3_GENERATED_MAX_NODES = 1_000_000;

export const PHASE3_GENERATED_MAX_BYTES = 1024 ** 3;

export const PHASE3_PROCESS_MAX_DAILY_GROWTH_RATIO = 0.1;

export const PHASE3_MAX_AUDIT_AGE_MS = 6 * 60 * 60_000 + 90_000;

export const PHASE3_ARCHITECTURE_G_TARGET_TPS = 5_000;

export const PHASE3_OFFERED_RATE_MIN_RATIO = 0.98;

export const PHASE3_ACCEPTED_RATE_MIN_RATIO = 0.99;

export const PHASE3_NODE_SATURATION_MIN_RATIO = 1;

export const PHASE3_WORKLOAD_LIFECYCLE_GRACE_MS = 15 * 60_000;

export const PHASE3_DRAIN_TIMEOUT_SEC = 600;

export const finite = (value) =>
  typeof value === "number" && Number.isFinite(value) ? value : null;

export const integer = (value) => (Number.isSafeInteger(value) ? value : null);

export const sha256 = (bytes) =>
  createHash("sha256").update(bytes).digest("hex");

export const normalizedImageId = (value) =>
  typeof value === "string" ? value.replace(/^sha256:/u, "") : value;

export const exactShape = (value, keys, label, reasons) => {
  if (!hasExactV1JsonKeys(value, keys)) {
    reasons.push(`${label} must use the exact V1 keys`);
    return false;
  }
  return true;
};

export const validateSampleShape = (sample, label, reasons) => {
  exactShape(
    sample,
    ["observedAtMs", "elapsedMs", "readiness", "metrics", "owner", "process"],
    label,
    reasons,
  );
  exactShape(
    sample?.readiness,
    ["httpStatus", "ready", "reasons"],
    `${label} readiness`,
    reasons,
  );
  exactShape(
    sample?.metrics,
    [
      "auditDivergence",
      "auditAgeMs",
      "auditCompletedAtMs",
      "confirmedLedgerFullScanTotal",
      "validationWorkerTimeoutTotal",
      "l1ControlPlaneTimeoutTotal",
      "timeoutInsteadOfBackpressureTotal",
      "daPublicationBacklog",
      "mergeQueueDepth",
    ],
    `${label} metrics`,
    reasons,
  );
  exactShape(
    sample?.owner,
    [
      "durableRoot",
      "residentNodes",
      "residentBytes",
      "activeGenerations",
      "generatedNodes",
      "generatedBytes",
      "rssBytes",
      "peakRssBytes",
      "childRestarts",
    ],
    `${label} owner`,
    reasons,
  );
  exactShape(
    sample?.process,
    ["pid", "startTicks", "rssBytes"],
    `${label} process`,
    reasons,
  );
  if (
    !Number.isSafeInteger(sample?.observedAtMs) ||
    !(
      sample?.elapsedMs === null ||
      (Number.isSafeInteger(sample.elapsedMs) && sample.elapsedMs >= 0)
    )
  ) {
    reasons.push(`${label} timestamps are not canonical epoch milliseconds`);
  }
};

export const validateCorpusPreflightSummaryShape = (summary, reasons) => {
  exactShape(
    summary,
    [
      "path",
      "sha256",
      "bytes",
      "schemaVersion",
      "sourceTreeSha256",
      "sourceIdentitySha256",
      "phase1BindingSha256",
      "files",
      "selection",
      "validation",
    ],
    "corpus-preflight summary",
    reasons,
  );
  exactShape(
    summary?.files,
    ["corpus", "index", "manifest"],
    "corpus-preflight files",
    reasons,
  );
  for (const [label, file] of Object.entries(summary?.files ?? {})) {
    exactShape(
      file,
      ["path", "bytes", "mtimeMs", "dev", "ino", "sha256"],
      `corpus-preflight ${label} file`,
      reasons,
    );
    if (
      !isCanonicalAbsolutePath(file?.path) ||
      !SHA256.test(file?.sha256 ?? "")
    ) {
      reasons.push(`corpus-preflight ${label} identity is noncanonical`);
    }
  }
  exactShape(
    summary?.selection,
    [
      "corpusSliceId",
      "corpusShape",
      "indexEntryCount",
      "rowCount",
      "indexEntriesSha256",
    ],
    "corpus-preflight selection",
    reasons,
  );
  exactShape(
    summary?.validation,
    ["rowCount", "uniqueTxHashes", "uniqueSelectedInputs"],
    "corpus-preflight validation",
    reasons,
  );
};

export const validateIsolationSummaryShape = (summary, reasons) => {
  exactShape(
    summary,
    [
      "path",
      "sha256",
      "bytes",
      "schemaVersion",
      "placement",
      "cohosted",
      "clockOffsetMs",
      "loadGeneratorCpusAllowedList",
      "loadGeneratorEffectiveUid",
      "nodeCpusAllowedList",
      "nodeContainerId",
      "nodeImageId",
      "nodeHostPid",
      "nodeStartTicks",
      "readyUrl",
      "metricsUrl",
      "dockerClientRealPath",
      "dockerClientSha256",
      "dockerSocketRealPath",
      "dockerSocketDev",
      "dockerSocketIno",
      "dockerDaemonId",
    ],
    "load-generator isolation summary",
    reasons,
  );
};

export const validateNodeRevalidationSummaryShape = (summary, reasons) => {
  exactShape(
    summary,
    [
      "path",
      "sha256",
      "bytes",
      "schemaVersion",
      "observedAtMs",
      "isolationPath",
      "isolationSha256",
      "nodeContainerId",
      "nodeImageId",
      "nodeHostPid",
      "nodeStartTicks",
      "nodeRestartCount",
      "nodeHealthStatus",
      "readyUrl",
      "metricsUrl",
      "dockerClientSha256",
      "dockerSocketDev",
      "dockerSocketIno",
      "dockerDaemonId",
    ],
    "node revalidation summary",
    reasons,
  );
};
