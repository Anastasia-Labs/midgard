import fs from "node:fs";

import {
  evaluateClosureIdentity,
  evaluateClosureIdentityArtifacts,
  evaluateExactClosureIdentityShape,
  evaluateExactSourceIdentityShape,
  sameSourceIdentity,
  SHA256,
  sha256File,
} from "./phase3-architecture-g-closure-lib.mjs";
import { validateStepEvidence } from "./verify-phase3-architecture-g-live-e2e-report.validate-step-evidence.mjs";
import {
  canonicalAbsolutePath,
  containsForbiddenEvidence,
  exactShape,
  PHASE3_LIVE_COMMAND_SCHEMA,
  PHASE3_LIVE_E2E_AUTHORIZATION,
  PHASE3_LIVE_E2E_SCENARIO,
  PHASE3_LIVE_E2E_SCHEMA,
  PHASE3_LIVE_STEP_IDS,
  PHASE3_LIVE_STEP_SCHEMA,
  validateArtifact,
  validateBinding,
  validateSecretScannedLog,
  validateStepEvidenceShape,
} from "./verify-phase3-architecture-g-live-e2e-report.validate-step-evidence-shape.mjs";

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
