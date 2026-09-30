import { isDeepStrictEqual } from "node:util";

import {
  arrayOf,
  booleanValue,
  exactRecord,
  isoTimestamp,
  nodeSignal,
  nonEmptyString,
  nonNegativeInteger,
  nonNegativeNumber,
  nullable,
  oneOf,
  positiveInteger,
} from "midgard-node/artifact-schema";
import { type E2EEnvProvenance } from "midgard-node/e2e/env";

import {
  parseChildProcessCleanup,
  parseRedactedCommand,
  redactArg,
  type RedactedCommand,
} from "./runner.js";
import {
  E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION,
  type HostProcessServiceSpec,
  parseFileTerminationObservation,
  parseOutputTerminationObservation,
  parseServiceErrorClassification,
  type ServiceAttemptSummary,
  type ServiceErrorClassification,
  type ServiceSupervisorSummary,
} from "./service-supervisor.parse-http-probe-sample.js";

const parseServiceAttemptSummary = (
  value: unknown,
  label: string,
): ServiceAttemptSummary => {
  const input = exactRecord(value, label, [
    "attempt",
    "pid",
    "startedAt",
    "finishedAt",
    "durationMs",
    "exitCode",
    "signal",
    "timedOut",
    "classification",
    "cleanup",
    "outputTermination",
    "fileTermination",
  ]);
  const parsed: ServiceAttemptSummary = {
    attempt: positiveInteger(input.attempt, `${label}.attempt`),
    pid: nullable(input.pid, `${label}.pid`, positiveInteger),
    startedAt: isoTimestamp(input.startedAt, `${label}.startedAt`),
    finishedAt: isoTimestamp(input.finishedAt, `${label}.finishedAt`),
    durationMs: nonNegativeNumber(input.durationMs, `${label}.durationMs`),
    exitCode: nullable(input.exitCode, `${label}.exitCode`, nonNegativeInteger),
    signal: nullable(input.signal, `${label}.signal`, nodeSignal),
    timedOut: booleanValue(input.timedOut, `${label}.timedOut`),
    classification: parseServiceErrorClassification(
      input.classification,
      `${label}.classification`,
    ),
    cleanup:
      input.cleanup === null
        ? null
        : parseChildProcessCleanup(input.cleanup, `${label}.cleanup`),
    outputTermination:
      input.outputTermination === null
        ? null
        : parseOutputTerminationObservation(
            input.outputTermination,
            `${label}.outputTermination`,
          ),
    fileTermination:
      input.fileTermination === null
        ? null
        : parseFileTerminationObservation(
            input.fileTermination,
            `${label}.fileTermination`,
          ),
  };
  const elapsedMs =
    Date.parse(parsed.finishedAt) - Date.parse(parsed.startedAt);
  const externalTermination =
    parsed.outputTermination !== null || parsed.fileTermination !== null;
  if (
    elapsedMs < 0 ||
    parsed.durationMs !== elapsedMs ||
    (parsed.outputTermination !== null && parsed.fileTermination !== null) ||
    (parsed.outputTermination !== null &&
      (Date.parse(parsed.outputTermination.at) < Date.parse(parsed.startedAt) ||
        Date.parse(parsed.outputTermination.at) >
          Date.parse(parsed.finishedAt))) ||
    (parsed.fileTermination !== null &&
      (Date.parse(parsed.fileTermination.at) < Date.parse(parsed.startedAt) ||
        Date.parse(parsed.fileTermination.at) >
          Date.parse(parsed.finishedAt))) ||
    (externalTermination &&
      parsed.classification.class !== "restartable_runtime") ||
    (parsed.timedOut &&
      (externalTermination ||
        parsed.classification.class !== "restartable_runtime")) ||
    (parsed.exitCode === 0 &&
      !parsed.timedOut &&
      !externalTermination &&
      (parsed.signal !== null ||
        parsed.classification.class !== "unknown" ||
        parsed.classification.restartable))
  ) {
    throw new Error(
      `${label} timing, termination, or classification is inconsistent`,
    );
  }
  return parsed;
};

export const parseServiceSupervisorSummary = (
  value: unknown,
): ServiceSupervisorSummary => {
  const label = "service supervisor summary";
  const input = exactRecord(value, label, [
    "schemaVersion",
    "service",
    "command",
    "status",
    "rawLogPath",
    "attempts",
    "restartCount",
    "terminalClassification",
  ]);
  if (input.schemaVersion !== E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be ${E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION}`,
    );
  }
  const parsed: ServiceSupervisorSummary = {
    schemaVersion: E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION,
    service: nonEmptyString(input.service, `${label}.service`),
    command: parseRedactedCommand(input.command, `${label}.command`),
    status: oneOf(input.status, `${label}.status`, [
      "exited_success",
      "failed",
      "restart_budget_exhausted",
      "timeout",
      "supervisor_failure",
    ]),
    rawLogPath: nonEmptyString(input.rawLogPath, `${label}.rawLogPath`),
    attempts: arrayOf(
      input.attempts,
      `${label}.attempts`,
      parseServiceAttemptSummary,
    ),
    restartCount: nonNegativeInteger(
      input.restartCount,
      `${label}.restartCount`,
    ),
    terminalClassification: parseServiceErrorClassification(
      input.terminalClassification,
      `${label}.terminalClassification`,
    ),
  };
  const terminalAttempt = parsed.attempts.at(-1);
  const cleanTerminalSuccess =
    terminalAttempt !== undefined &&
    terminalAttempt.exitCode === 0 &&
    terminalAttempt.signal === null &&
    !terminalAttempt.timedOut &&
    terminalAttempt.outputTermination === null &&
    terminalAttempt.fileTermination === null &&
    !terminalAttempt.classification.restartable;
  const expectedStatus: ServiceSupervisorSummary["status"] | null =
    terminalAttempt === undefined
      ? null
      : cleanTerminalSuccess
        ? "exited_success"
        : !terminalAttempt.classification.restartable
          ? terminalAttempt.classification.class === "supervisor_failure"
            ? "supervisor_failure"
            : "failed"
          : terminalAttempt.timedOut
            ? "timeout"
            : "restart_budget_exhausted";
  if (
    terminalAttempt === undefined ||
    parsed.restartCount !== parsed.attempts.length - 1 ||
    !isDeepStrictEqual(
      parsed.terminalClassification,
      terminalAttempt.classification,
    ) ||
    parsed.status !== expectedStatus ||
    parsed.attempts.some(
      (attempt, index) =>
        attempt.attempt !== index + 1 ||
        (index < parsed.attempts.length - 1 &&
          (!attempt.classification.restartable ||
            (attempt.exitCode === 0 &&
              attempt.signal === null &&
              !attempt.timedOut &&
              attempt.outputTermination === null &&
              attempt.fileTermination === null))) ||
        (index > 0 &&
          Date.parse(attempt.startedAt) <
            Date.parse(parsed.attempts[index - 1]!.finishedAt)),
    )
  ) {
    throw new Error(
      `${label} terminal verdict or attempt history is inconsistent`,
    );
  }
  return parsed;
};

const TRANSIENT_PROVIDER_PATTERNS = [
  /fetch failed/i,
  /ECONNRESET/i,
  /ECONNREFUSED/i,
  /\b429\b/,
  /\b503\b/,
  /temporar(?:y|ily) unavailable/i,
  /timeout/i,
];

const FATAL_CONFIG_PATTERNS = [
  /invalid mnemonic/i,
  /missing required env/i,
  /L1_SUBMITTER_KEY_SOURCE/i,
  /signer[-_ ]?index/i,
  /manifest fingerprint mismatch/i,
  /EADDRINUSE/i,
  /unsupported provider/i,
  /missing watcher DB config/i,
  /removed DA_MODE/i,
];

const FATAL_PROTOCOL_PATTERNS = [
  /insufficient (lovelace|funds)/i,
  /missing collateral/i,
  /value not conserved/i,
  /bad inputs?/i,
  /script integrity/i,
  /hash mismatch/i,
  /partial deployment/i,
  /unfinished local mutation/i,
  /DA payload conflict/i,
  /root_mismatch/i,
  /malformed_da/i,
  /conflicted/i,
];

export const classifyServiceError = ({
  text,
  recentTxHashes = new Set<string>(),
}: {
  readonly text: string;
  readonly recentTxHashes?: ReadonlySet<string>;
}): ServiceErrorClassification => {
  const recent404Match = text.match(/\/txs\/([0-9a-f]{64}).*404/i);
  if (recent404Match !== null) {
    const txHash = recent404Match[1]!.toLowerCase();
    if (recentTxHashes.has(txHash)) {
      return {
        class: "transient_provider",
        reason: `recent submitted tx ${txHash} is not provider-visible yet`,
        restartable: true,
      };
    }
    return {
      class: "unknown",
      reason: `provider 404 for untracked tx ${txHash}`,
      restartable: false,
    };
  }
  if (FATAL_CONFIG_PATTERNS.some((pattern) => pattern.test(text))) {
    return {
      class: "fatal_config",
      reason: "fatal configuration error matched service logs",
      restartable: false,
    };
  }
  if (FATAL_PROTOCOL_PATTERNS.some((pattern) => pattern.test(text))) {
    return {
      class: "fatal_protocol_or_precondition",
      reason: "fatal protocol/precondition error matched service logs",
      restartable: false,
    };
  }
  if (TRANSIENT_PROVIDER_PATTERNS.some((pattern) => pattern.test(text))) {
    return {
      class: "transient_provider",
      reason: "transient provider/startup error matched service logs",
      restartable: true,
    };
  }
  return {
    class: "unknown",
    reason:
      text.trim().length === 0
        ? "service exited without output"
        : "unclassified service output",
    restartable: false,
  };
};

export const redactedCommand = (
  spec: HostProcessServiceSpec,
  provenance: E2EEnvProvenance,
): RedactedCommand => ({
  command: spec.command,
  args: (spec.args ?? []).map(redactArg),
  cwd: spec.cwd,
  envKeys: provenance.explicitEnvKeys,
  envFiles: provenance.envFiles,
  envInheritance: provenance.inheritance,
});
