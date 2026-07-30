#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  absoluteArg,
  evaluateClosureIdentity,
  evaluateClosureIdentityArtifacts,
  evaluateExactClosureIdentityShape,
  evaluateExactSourceIdentityShape,
  hasExactV1JsonKeys,
  sameSourceIdentity,
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

const TX_HASH = /^[0-9a-f]{64}$/u;
const OWNER_EPOCH = /^[0-9a-f]{32}$/u;
const positiveInteger = (value) => Number.isSafeInteger(value) && value > 0;
const zeroInteger = (value) => Number.isSafeInteger(value) && value === 0;
const canonicalAbsolutePath = (value) =>
  typeof value === "string" &&
  path.isAbsolute(value) &&
  path.resolve(value) === value;
const exactShape = (value, keys, label, reasons) => {
  if (!hasExactV1JsonKeys(value, keys)) {
    reasons.push(`${label} must use the exact V1 keys`);
    return false;
  }
  return true;
};
const ready = (value) =>
  value?.httpStatus === 200 &&
  value?.ready === true &&
  Array.isArray(value?.reasons) &&
  value.reasons.length === 0;
const containsForbiddenEvidence = (value) => {
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

const validateArtifact = (artifact, label, checkArtifacts, reasons) => {
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

const validateSecretScannedLog = (artifact, label, reasons) => {
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

const validateStepEvidenceShape = (stepId, evidence, reasons) => {
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

const validateBinding = (binding, identity, label, reasons) => {
  if (
    binding?.runtimeSha256 !== identity?.runtime?.sha256 ||
    binding?.deploymentSha256 !== identity?.deployment?.sha256 ||
    binding?.phase1Sha256 !== identity?.phase1?.sha256 ||
    binding?.ownerSha256 !== identity?.ownerBinary?.sha256
  ) {
    reasons.push(`${label} does not bind the report identity`);
  }
};

const validateStepEvidence = (stepId, evidence, context, reasons) => {
  switch (stepId) {
    case "fresh-deployment-preflight": {
      if (
        evidence?.runMode !== "fresh" ||
        evidence?.engine !== "architecture_g" ||
        evidence?.localUplc !== true ||
        evidence?.provider !== "Kupmios" ||
        evidence?.cleanDeployment !== true ||
        !ready(evidence?.readiness)
      ) {
        reasons.push("fresh deployment preflight is incomplete");
      }
      break;
    }
    case "deposit-projection": {
      if (
        !TX_HASH.test(evidence?.txHash ?? "") ||
        typeof evidence?.eventId !== "string" ||
        evidence.eventId.length === 0 ||
        evidence?.confirmed !== true ||
        evidence?.projected !== true ||
        !Number.isSafeInteger(evidence?.balanceBeforeLovelace) ||
        !Number.isSafeInteger(evidence?.balanceAfterLovelace) ||
        evidence.balanceAfterLovelace <= evidence.balanceBeforeLovelace
      ) {
        reasons.push("deposit confirmation/projection evidence is incomplete");
      }
      break;
    }
    case "l2-submit": {
      const transactions = Array.isArray(evidence?.transactions)
        ? evidence.transactions
        : [];
      const hashes = transactions.map(({ txHash }) => txHash);
      if (
        transactions.length < 2 ||
        new Set(hashes).size !== transactions.length ||
        transactions.some(
          ({ txHash, status }) =>
            !TX_HASH.test(txHash ?? "") ||
            !["accepted", "committed"].includes(status),
        ) ||
        evidence?.submissionErrors !== 0
      ) {
        reasons.push(
          "L2 submission evidence does not contain two clean admissions",
        );
      } else context.l2TxHashes = hashes;
      break;
    }
    case "da-attestation": {
      const headers = Array.isArray(evidence?.headers) ? evidence.headers : [];
      if (
        headers.length === 0 ||
        headers.some(
          (header) =>
            !TX_HASH.test(header?.headerHash ?? "") ||
            !SHA256.test(header?.payloadMetadataSha256 ?? "") ||
            !SHA256.test(header?.payloadCborSha256 ?? "") ||
            !["attested", "merged"].includes(header?.watcherStatus) ||
            !Array.isArray(header?.attestationTxHashes) ||
            header.attestationTxHashes.length === 0 ||
            header.attestationTxHashes.some((hash) => !TX_HASH.test(hash)),
        )
      ) {
        reasons.push("DA payload and attestation evidence is incomplete");
      } else context.headerHashes = headers.map(({ headerHash }) => headerHash);
      break;
    }
    case "merge-finalization": {
      const committed = Array.isArray(evidence?.committedTxHashes)
        ? evidence.committedTxHashes
        : [];
      const finalized = Array.isArray(evidence?.finalizedHeaderHashes)
        ? evidence.finalizedHeaderHashes
        : [];
      if (
        evidence?.automaticMerge !== true ||
        context.l2TxHashes.some((hash) => !committed.includes(hash)) ||
        context.headerHashes.some((hash) => !finalized.includes(hash)) ||
        !zeroInteger(evidence?.stateQueueDepth) ||
        !zeroInteger(evidence?.unfinishedMutationJobs)
      ) {
        reasons.push("automatic merge/finalization evidence is incomplete");
      }
      break;
    }
    case "db-balance": {
      const counts = evidence?.counts;
      const assertions = Array.isArray(evidence?.balanceAssertions)
        ? evidence.balanceAssertions
        : [];
      if (
        !positiveInteger(counts?.consumedDeposits) ||
        !Number.isSafeInteger(counts?.acceptedAdmissions) ||
        counts.acceptedAdmissions < 2 ||
        !positiveInteger(counts?.immutableRows) ||
        !positiveInteger(counts?.confirmedLedgerRows) ||
        !zeroInteger(counts?.mempoolRows) ||
        !zeroInteger(counts?.processedMempoolRows) ||
        !zeroInteger(counts?.blockRows) ||
        !zeroInteger(counts?.unfinishedMutationJobs) ||
        assertions.length < 3 ||
        assertions.some(
          (entry) =>
            typeof entry?.addressHash !== "string" ||
            entry.addressHash.length === 0 ||
            !Number.isSafeInteger(entry?.expectedLovelace) ||
            entry.actualLovelace !== entry.expectedLovelace,
        )
      ) {
        reasons.push("DB residue or exact balance assertions failed");
      }
      break;
    }
    case "owner-child-restart": {
      if (
        evidence?.signal !== "SIGKILL" ||
        !positiveInteger(evidence?.ownerPidBefore) ||
        !positiveInteger(evidence?.ownerPidAfter) ||
        evidence.ownerPidBefore === evidence.ownerPidAfter ||
        !positiveInteger(evidence?.nodePid) ||
        !Number.isSafeInteger(evidence?.childRestartsBefore) ||
        evidence?.childRestartsAfter !== evidence.childRestartsBefore + 1 ||
        evidence?.nodeProcessRestarted !== false ||
        evidence?.readinessRestored !== true
      ) {
        reasons.push("native owner child restart evidence is incomplete");
      }
      break;
    }
    case "post-submit-recovery": {
      const hashes = [
        evidence?.headerHash,
        evidence?.submissionTxHash,
        evidence?.baseRoot,
        evidence?.candidateRoot,
        evidence?.eventLogDigest,
        evidence?.ownerBinarySha256,
      ];
      if (
        hashes.some((hash) => !TX_HASH.test(hash ?? "")) ||
        evidence.ownerBinarySha256 !== context.identity?.ownerBinary?.sha256 ||
        !positiveInteger(evidence?.replayEventCount) ||
        evidence?.killedAfterSubmission !== true ||
        evidence?.killedBeforePromotion !== true ||
        !OWNER_EPOCH.test(evidence?.ownerEpochBefore ?? "") ||
        !OWNER_EPOCH.test(evidence?.ownerEpochAfter ?? "") ||
        evidence.ownerEpochAfter === evidence.ownerEpochBefore ||
        evidence?.authoritativeMarkerAfter !== evidence?.candidateRoot ||
        evidence?.replayedCandidateRoot !== evidence?.candidateRoot ||
        evidence?.journalStatus !== "finalized" ||
        evidence?.l2Status !== "committed" ||
        evidence?.auditDivergence !== 0 ||
        evidence?.recoveryLogMarker !==
          "Architecture G recovered post-submit promotion after native child restart"
      ) {
        reasons.push(
          "post-submit replay/promotion recovery evidence is incomplete",
        );
      }
      break;
    }
    case "final-readiness": {
      if (
        !ready(evidence?.node) ||
        !ready(evidence?.da) ||
        evidence?.allL2Committed !== true ||
        !zeroInteger(evidence?.stateQueueDepth) ||
        !zeroInteger(evidence?.unfinishedMutationJobs) ||
        evidence?.unexpectedErrorCount !== 0
      ) {
        reasons.push("final node/DA readiness or residue gate failed");
      }
      break;
    }
    default:
      reasons.push(`unexpected live step ${stepId}`);
  }
};

export const evaluatePhase3LiveE2EReport = (
  report,
  { checkArtifacts = true } = {},
) => {
  const reasons = [];
  exactShape(
    report,
    [
      "schemaVersion",
      "scenario",
      "authorization",
      "startedAtMs",
      "completedAtMs",
      "identity",
      "sourceAtCompletion",
      "commandManifest",
      "steps",
      "verdict",
    ],
    "live E2E report",
    reasons,
  );
  reasons.push(...evaluateExactClosureIdentityShape(report?.identity));
  reasons.push(
    ...evaluateExactSourceIdentityShape(
      report?.sourceAtCompletion,
      "completion source identity",
    ),
  );
  exactShape(
    report?.commandManifest,
    ["path", "sha256", "bytes"],
    "command-manifest artifact identity",
    reasons,
  );
  if (
    !Number.isSafeInteger(report?.startedAtMs) ||
    report.startedAtMs <= 0 ||
    !Number.isSafeInteger(report?.completedAtMs) ||
    report.completedAtMs < report.startedAtMs
  ) {
    reasons.push("live E2E report interval is not canonical");
  }
  if (report?.schemaVersion !== PHASE3_LIVE_E2E_SCHEMA) {
    reasons.push("unexpected live E2E report schema");
  }
  if (report?.scenario !== PHASE3_LIVE_E2E_SCENARIO) {
    reasons.push("unexpected live E2E scenario");
  }
  if (report?.authorization !== PHASE3_LIVE_E2E_AUTHORIZATION) {
    reasons.push("live E2E authorization is absent");
  }
  reasons.push(...evaluateClosureIdentity(report?.identity));
  if (checkArtifacts) {
    reasons.push(...evaluateClosureIdentityArtifacts(report?.identity));
  }
  if (
    !sameSourceIdentity(report?.identity?.source, report?.sourceAtCompletion)
  ) {
    reasons.push("source tree changed during live E2E");
  }
  validateArtifact(
    report?.commandManifest,
    "command manifest",
    checkArtifacts,
    reasons,
  );
  let commandManifest;
  if (
    checkArtifacts &&
    typeof report?.commandManifest?.path === "string" &&
    fs.existsSync(report.commandManifest.path)
  ) {
    try {
      commandManifest = JSON.parse(
        fs.readFileSync(report.commandManifest.path, "utf8"),
      );
      if (
        commandManifest?.schemaVersion !== PHASE3_LIVE_COMMAND_SCHEMA ||
        commandManifest?.authorization !== PHASE3_LIVE_E2E_AUTHORIZATION
      ) {
        reasons.push("command manifest schema or authorization is invalid");
      }
      exactShape(
        commandManifest,
        ["schemaVersion", "authorization", "binding", "steps"],
        "command manifest",
        reasons,
      );
      exactShape(
        commandManifest?.binding,
        ["runtimeSha256", "deploymentSha256", "phase1Sha256", "ownerSha256"],
        "command-manifest binding",
        reasons,
      );
      for (const [index, commandStep] of (Array.isArray(commandManifest?.steps)
        ? commandManifest.steps
        : []
      ).entries()) {
        exactShape(
          commandStep,
          ["id", "command", "args", "cwd", "timeoutMs"],
          `command-manifest step ${index.toString()}`,
          reasons,
        );
      }
      validateBinding(
        commandManifest?.binding,
        report?.identity,
        "command manifest",
        reasons,
      );
    } catch {
      reasons.push("command manifest is not valid JSON");
    }
  }
  const steps = Array.isArray(report?.steps) ? report.steps : [];
  if (steps.length !== PHASE3_LIVE_STEP_IDS.length) {
    reasons.push("live E2E step cardinality is not exact");
  }
  const context = {
    identity: report?.identity,
    l2TxHashes: [],
    headerHashes: [],
  };
  for (const [index, stepId] of PHASE3_LIVE_STEP_IDS.entries()) {
    const step = steps[index];
    exactShape(
      step,
      [
        "id",
        "driver",
        "exitCode",
        "signal",
        "timedOut",
        "driverStable",
        "completed",
        "stdout",
        "stderr",
        "resultArtifact",
        "result",
      ],
      `${stepId} step`,
      reasons,
    );
    exactShape(
      step?.driver,
      ["path", "sha256", "args", "cwd", "timeoutMs"],
      `${stepId} driver`,
      reasons,
    );
    exactShape(
      step?.result,
      [
        "schemaVersion",
        "stepId",
        "verdict",
        "completed",
        "binding",
        "startedAtMs",
        "completedAtMs",
        "evidence",
      ],
      `${stepId} result`,
      reasons,
    );
    exactShape(
      step?.result?.binding,
      ["runtimeSha256", "deploymentSha256", "phase1Sha256", "ownerSha256"],
      `${stepId} binding`,
      reasons,
    );
    exactShape(
      step?.resultArtifact,
      ["path", "sha256", "bytes"],
      `${stepId} result artifact`,
      reasons,
    );
    for (const [label, log] of [
      ["stdout", step?.stdout],
      ["stderr", step?.stderr],
    ]) {
      exactShape(
        log,
        ["path", "sha256", "bytes", "secretScan"],
        `${stepId} ${label} artifact`,
        reasons,
      );
      exactShape(
        log?.secretScan,
        [
          "schemaVersion",
          "passed",
          "sensitiveLineCount",
          "oversizedLineCount",
          "retainedLineCount",
        ],
        `${stepId} ${label} secret scan`,
        reasons,
      );
    }
    validateStepEvidenceShape(stepId, step?.result?.evidence, reasons);
    if (
      step?.id !== stepId ||
      step?.result?.schemaVersion !== PHASE3_LIVE_STEP_SCHEMA ||
      step?.result?.stepId !== stepId ||
      step?.result?.verdict !== "passed" ||
      step?.result?.completed !== true ||
      step?.exitCode !== 0 ||
      step?.signal !== null ||
      step?.timedOut !== false ||
      step?.driverStable !== true ||
      step?.completed !== true
    ) {
      reasons.push(`live E2E step ${stepId} did not complete exactly once`);
      continue;
    }
    validateBinding(step.result.binding, report?.identity, stepId, reasons);
    if (containsForbiddenEvidence(step.result)) {
      reasons.push(`${stepId} result contains forbidden sensitive fields`);
    }
    if (
      !Number.isSafeInteger(step.result?.startedAtMs) ||
      !Number.isSafeInteger(step.result?.completedAtMs) ||
      step.result.completedAtMs < step.result.startedAtMs
    ) {
      reasons.push(`${stepId} result interval is invalid`);
    }
    if (
      !canonicalAbsolutePath(step?.driver?.path) ||
      !SHA256.test(step?.driver?.sha256 ?? "") ||
      !Array.isArray(step?.driver?.args) ||
      !canonicalAbsolutePath(step?.driver?.cwd) ||
      !Number.isSafeInteger(step?.driver?.timeoutMs) ||
      step.driver.timeoutMs <= 0
    ) {
      reasons.push(`${stepId} driver identity is malformed`);
    }
    const commandStep = commandManifest?.steps?.[index];
    if (
      commandManifest !== undefined &&
      (commandStep?.id !== stepId ||
        commandStep?.command !== step?.driver?.path ||
        JSON.stringify(commandStep?.args) !==
          JSON.stringify(step?.driver?.args) ||
        commandStep?.cwd !== step?.driver?.cwd ||
        commandStep?.timeoutMs !== step?.driver?.timeoutMs)
    ) {
      reasons.push(`${stepId} driver differs from the bound command manifest`);
    }
    if (checkArtifacts && typeof step?.driver?.path === "string") {
      if (!fs.existsSync(step.driver.path)) {
        reasons.push(`${stepId} driver is missing`);
      } else {
        const driverStat = fs.lstatSync(step.driver.path);
        if (!driverStat.isFile() || driverStat.isSymbolicLink()) {
          reasons.push(`${stepId} driver is not a regular file`);
        } else if (sha256File(step.driver.path) !== step?.driver?.sha256) {
          reasons.push(`${stepId} driver SHA-256 changed`);
        }
      }
    }
    for (const [label, artifact] of [
      ["result", step.resultArtifact],
      ["stdout", step.stdout],
      ["stderr", step.stderr],
    ]) {
      validateArtifact(artifact, `${stepId} ${label}`, checkArtifacts, reasons);
      if (label !== "result") {
        validateSecretScannedLog(artifact, `${stepId} ${label}`, reasons);
      }
    }
    validateStepEvidence(stepId, step.result.evidence, context, reasons);
  }
  if (report?.verdict !== "passed")
    reasons.push("report verdict is not passed");
  return { passed: reasons.length === 0, reasons };
};

const isMain = process.argv[1] === fileURLToPath(import.meta.url);
if (isMain) {
  try {
    const reportPath = absoluteArg(process.argv.slice(2), "--report");
    const result = evaluatePhase3LiveE2EReport(
      JSON.parse(fs.readFileSync(reportPath, "utf8")),
    );
    process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
    if (!result.passed) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exitCode = 1;
  }
}
