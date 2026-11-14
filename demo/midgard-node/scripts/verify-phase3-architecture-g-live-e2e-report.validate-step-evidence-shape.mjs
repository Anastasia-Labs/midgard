import fs from "node:fs";
import path from "node:path";

import {
  hasExactV1JsonKeys,
  SHA256,
  sha256File,
} from "./phase3-architecture-g-closure-lib.mjs";

export const PHASE3_LIVE_E2E_SCHEMA =
  "midgard-phase3-architecture-g-clean-live-e2e-v1";

export const PHASE3_LIVE_E2E_SCENARIO =
  "phase3-architecture-g-clean-live-e2e-recovery-v1";

export const PHASE3_LIVE_E2E_AUTHORIZATION = "architecture-g-clean-live-e2e-v1";

export const PHASE3_LIVE_STEP_SCHEMA =
  "midgard-phase3-architecture-g-clean-live-step-v1";

export const PHASE3_LIVE_COMMAND_SCHEMA =
  "midgard-phase3-architecture-g-clean-live-commands-v1";

export const PHASE3_LIVE_STEP_IDS = Object.freeze([
  "fresh-deployment-preflight",
  "deposit-projection",
  "l2-submit",
  "da-attestation",
  "merge-finalization",
  "db-balance",
  "owner-child-restart",
  "post-submit-recovery",
  "final-readiness",
]);

export const TX_HASH = /^[0-9a-f]{64}$/u;

export const OWNER_EPOCH = /^[0-9a-f]{32}$/u;

export const positiveInteger = (value) =>
  Number.isSafeInteger(value) && value > 0;

export const zeroInteger = (value) =>
  Number.isSafeInteger(value) && value === 0;

export const canonicalAbsolutePath = (value) =>
  typeof value === "string" &&
  path.isAbsolute(value) &&
  path.resolve(value) === value;

export const exactShape = (value, keys, label, reasons) => {
  if (!hasExactV1JsonKeys(value, keys)) {
    reasons.push(`${label} must use the exact V1 keys`);
    return false;
  }
  return true;
};

export const ready = (value) =>
  value?.httpStatus === 200 &&
  value?.ready === true &&
  Array.isArray(value?.reasons) &&
  value.reasons.length === 0;

export const containsForbiddenEvidence = (value) => {
  if (typeof value === "string") return value.length > 4_096;
  if (Array.isArray(value)) return value.some(containsForbiddenEvidence);
  if (typeof value !== "object" || value === null) return false;
  return Object.entries(value).some(
    ([key, entry]) =>
      /(seed|mnemonic|phrase|private|secret|signed.*cbor|txcbor|rawcbor)/iu.test(
        key,
      ) || containsForbiddenEvidence(entry),
  );
};

export const validateArtifact = (artifact, label, checkArtifacts, reasons) => {
  if (
    !canonicalAbsolutePath(artifact?.path) ||
    !SHA256.test(artifact?.sha256 ?? "") ||
    !Number.isSafeInteger(artifact?.bytes) ||
    artifact.bytes < 0
  ) {
    reasons.push(`${label} artifact identity is malformed`);
    return;
  }
  if (!checkArtifacts) return;
  if (!fs.existsSync(artifact.path)) {
    reasons.push(`${label} artifact is missing`);
    return;
  }
  const stat = fs.lstatSync(artifact.path);
  if (!stat.isFile() || stat.isSymbolicLink()) {
    reasons.push(`${label} artifact is not a regular file`);
  } else if (
    stat.size !== artifact.bytes ||
    sha256File(artifact.path) !== artifact.sha256
  ) {
    reasons.push(`${label} artifact bytes changed`);
  }
};

export const validateSecretScannedLog = (artifact, label, reasons) => {
  const scan = artifact?.secretScan;
  if (
    scan?.schemaVersion !== "midgard-secret-scanned-log-v1" ||
    scan?.passed !== true ||
    !Number.isSafeInteger(scan?.sensitiveLineCount) ||
    scan.sensitiveLineCount !== 0 ||
    !Number.isSafeInteger(scan?.oversizedLineCount) ||
    scan.oversizedLineCount !== 0 ||
    !Number.isSafeInteger(scan?.retainedLineCount) ||
    scan.retainedLineCount < 0
  ) {
    reasons.push(`${label} was not retained through a clean secret scan`);
  }
};

const validateReadyShape = (value, label, reasons) => {
  exactShape(value, ["httpStatus", "ready", "reasons"], label, reasons);
};

export const validateStepEvidenceShape = (stepId, evidence, reasons) => {
  const check = (value, keys, label) => exactShape(value, keys, label, reasons);
  switch (stepId) {
    case "fresh-deployment-preflight":
      check(
        evidence,
        [
          "runMode",
          "engine",
          "localUplc",
          "provider",
          "cleanDeployment",
          "readiness",
        ],
        `${stepId} evidence`,
      );
      validateReadyShape(evidence?.readiness, `${stepId} readiness`, reasons);
      break;
    case "deposit-projection":
      check(
        evidence,
        [
          "txHash",
          "eventId",
          "confirmed",
          "projected",
          "balanceBeforeLovelace",
          "balanceAfterLovelace",
        ],
        `${stepId} evidence`,
      );
      break;
    case "l2-submit":
      check(
        evidence,
        ["transactions", "submissionErrors"],
        `${stepId} evidence`,
      );
      for (const [index, transaction] of (Array.isArray(evidence?.transactions)
        ? evidence.transactions
        : []
      ).entries()) {
        check(
          transaction,
          ["txHash", "status"],
          `${stepId} transaction ${index.toString()}`,
        );
      }
      break;
    case "da-attestation":
      check(evidence, ["headers"], `${stepId} evidence`);
      for (const [index, header] of (Array.isArray(evidence?.headers)
        ? evidence.headers
        : []
      ).entries()) {
        check(
          header,
          [
            "headerHash",
            "payloadMetadataSha256",
            "payloadCborSha256",
            "watcherStatus",
            "attestationTxHashes",
          ],
          `${stepId} header ${index.toString()}`,
        );
      }
      break;
    case "merge-finalization":
      check(
        evidence,
        [
          "automaticMerge",
          "committedTxHashes",
          "finalizedHeaderHashes",
          "stateQueueDepth",
          "unfinishedMutationJobs",
        ],
        `${stepId} evidence`,
      );
      break;
    case "db-balance":
      check(evidence, ["counts", "balanceAssertions"], `${stepId} evidence`);
      check(
        evidence?.counts,
        [
          "consumedDeposits",
          "acceptedAdmissions",
          "immutableRows",
          "confirmedLedgerRows",
          "mempoolRows",
          "processedMempoolRows",
          "blockRows",
          "unfinishedMutationJobs",
        ],
        `${stepId} counts`,
      );
      for (const [index, assertion] of (Array.isArray(
        evidence?.balanceAssertions,
      )
        ? evidence.balanceAssertions
        : []
      ).entries()) {
        check(
          assertion,
          ["addressHash", "expectedLovelace", "actualLovelace"],
          `${stepId} assertion ${index.toString()}`,
        );
      }
      break;
    case "owner-child-restart":
      check(
        evidence,
        [
          "signal",
          "ownerPidBefore",
          "ownerPidAfter",
          "nodePid",
          "childRestartsBefore",
          "childRestartsAfter",
          "nodeProcessRestarted",
          "readinessRestored",
        ],
        `${stepId} evidence`,
      );
      break;
    case "post-submit-recovery":
      check(
        evidence,
        [
          "headerHash",
          "submissionTxHash",
          "baseRoot",
          "candidateRoot",
          "eventLogDigest",
          "ownerBinarySha256",
          "replayEventCount",
          "killedAfterSubmission",
          "killedBeforePromotion",
          "ownerEpochBefore",
          "ownerEpochAfter",
          "authoritativeMarkerAfter",
          "replayedCandidateRoot",
          "journalStatus",
          "l2Status",
          "auditDivergence",
          "recoveryLogMarker",
        ],
        `${stepId} evidence`,
      );
      break;
    case "final-readiness":
      check(
        evidence,
        [
          "node",
          "da",
          "allL2Committed",
          "stateQueueDepth",
          "unfinishedMutationJobs",
          "unexpectedErrorCount",
        ],
        `${stepId} evidence`,
      );
      validateReadyShape(evidence?.node, `${stepId} node readiness`, reasons);
      validateReadyShape(evidence?.da, `${stepId} DA readiness`, reasons);
      break;
    default:
      break;
  }
};

export const validateBinding = (binding, identity, label, reasons) => {
  if (
    binding?.runtimeSha256 !== identity?.runtime?.sha256 ||
    binding?.deploymentSha256 !== identity?.deployment?.sha256 ||
    binding?.phase1Sha256 !== identity?.phase1?.sha256 ||
    binding?.ownerSha256 !== identity?.ownerBinary?.sha256
  ) {
    reasons.push(`${label} does not bind the report identity`);
  }
};
