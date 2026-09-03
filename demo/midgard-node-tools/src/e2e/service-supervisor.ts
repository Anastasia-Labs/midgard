import { readFile } from "node:fs/promises";
import { isDeepStrictEqual } from "node:util";

import {
  arrayOf,
  booleanValue,
  exactRecord,
  isoTimestamp,
  jsonValue,
  nodeSignal,
  nonEmptyString,
  nonNegativeInteger,
  nonNegativeNumber,
  nullable,
  nullableNonEmptyString,
  oneOf,
  positiveInteger,
} from "midgard-node/artifact-schema";
import {
  buildE2EProcessEnv,
  type BuiltE2EProcessEnv,
  type E2EEnvInheritance,
  type E2EEnvProvenance,
} from "midgard-node/e2e/env";

import type {
  FileTerminationObservation,
  FileTerminationSpec,
  OutputTerminationObservation,
  OutputTerminationSpec,
} from "./logged-child-process.js";
import { runLoggedChildProcessAttempt } from "./logged-child-process.js";
import type { ChildProcessCleanupResult } from "./process-cleanup.js";
import type { OwnedProcessGroupSpec } from "./process-ownership.js";
import {
  parseChildProcessCleanupV1,
  parseRedactedCommandV1,
  redactArg,
  type RedactedCommand,
} from "./runner.js";

export const E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION =
  "midgard-e2e-service-supervisor-v1";

export type ServiceErrorClass =
  | "transient_provider"
  | "transient_startup"
  | "restartable_runtime"
  | "fatal_config"
  | "fatal_protocol_or_precondition"
  | "supervisor_failure"
  | "unknown";

export type ServiceErrorClassification = {
  readonly class: ServiceErrorClass;
  readonly reason: string;
  readonly restartable: boolean;
};

export type HttpProbeSample = {
  readonly label: string;
  readonly url: string;
  readonly status:
    | "healthy"
    | "not_ready"
    | "http_error"
    | "timeout"
    | "malformed_json";
  readonly statusCode: number | null;
  readonly latencyMs: number;
  readonly json: unknown | null;
  readonly error: string | null;
};

export type PidFileObservation = {
  readonly path: string;
  readonly status: "absent" | "invalid" | "stale" | "runner_owned" | "foreign";
  readonly pid: number | null;
};

export type HostProcessServiceSpec = {
  readonly service: string;
  readonly command: string;
  readonly args?: readonly string[];
  readonly cwd: string;
  readonly env?: Readonly<Record<string, string | undefined>>;
  readonly envFiles?: readonly string[];
  readonly envInheritance?: E2EEnvInheritance;
  readonly rawLogPath: string;
  readonly maxRestarts?: number;
  readonly restartBackoffMs?: number;
  readonly timeoutMs?: number;
  readonly terminateOnOutput?: OutputTerminationSpec;
  readonly terminateOnFile?: FileTerminationSpec;
  readonly sleep?: (milliseconds: number) => Promise<void>;
  readonly ownership?: OwnedProcessGroupSpec;
};

export type ServiceAttemptSummary = {
  readonly attempt: number;
  readonly pid: number | null;
  readonly startedAt: string;
  readonly finishedAt: string;
  readonly durationMs: number;
  readonly exitCode: number | null;
  readonly signal: NodeJS.Signals | null;
  readonly timedOut: boolean;
  readonly classification: ServiceErrorClassification;
  readonly cleanup: ChildProcessCleanupResult | null;
  readonly outputTermination: OutputTerminationObservation | null;
  readonly fileTermination: FileTerminationObservation | null;
};

export type ServiceSupervisorSummary = {
  readonly schemaVersion: typeof E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION;
  readonly service: string;
  readonly command: RedactedCommand;
  readonly status:
    | "exited_success"
    | "failed"
    | "restart_budget_exhausted"
    | "timeout"
    | "supervisor_failure";
  readonly rawLogPath: string;
  readonly attempts: readonly ServiceAttemptSummary[];
  readonly restartCount: number;
  readonly terminalClassification: ServiceErrorClassification;
};

export const parseServiceErrorClassificationV1 = (
  value: unknown,
  label = "service error classification",
): ServiceErrorClassification => {
  const input = exactRecord(value, label, ["class", "reason", "restartable"]);
  const parsed: ServiceErrorClassification = {
    class: oneOf(input.class, `${label}.class`, [
      "transient_provider",
      "transient_startup",
      "restartable_runtime",
      "fatal_config",
      "fatal_protocol_or_precondition",
      "supervisor_failure",
      "unknown",
    ]),
    reason: nonEmptyString(input.reason, `${label}.reason`),
    restartable: booleanValue(input.restartable, `${label}.restartable`),
  };
  const restartableClasses = new Set<ServiceErrorClass>([
    "transient_provider",
    "transient_startup",
    "restartable_runtime",
  ]);
  if (parsed.restartable !== restartableClasses.has(parsed.class)) {
    throw new Error(`${label}.class/restartable binding is inconsistent`);
  }
  return parsed;
};

export const parseHttpProbeSampleV1 = (
  value: unknown,
  label = "HTTP probe sample",
): HttpProbeSample => {
  const input = exactRecord(value, label, [
    "label",
    "url",
    "status",
    "statusCode",
    "latencyMs",
    "json",
    "error",
  ]);
  const parsed: HttpProbeSample = {
    label: nonEmptyString(input.label, `${label}.label`),
    url: nonEmptyString(input.url, `${label}.url`),
    status: oneOf(input.status, `${label}.status`, [
      "healthy",
      "not_ready",
      "http_error",
      "timeout",
      "malformed_json",
    ]),
    statusCode: nullable(
      input.statusCode,
      `${label}.statusCode`,
      nonNegativeInteger,
    ),
    latencyMs: nonNegativeNumber(input.latencyMs, `${label}.latencyMs`),
    json: input.json === null ? null : jsonValue(input.json, `${label}.json`),
    error: nullableNonEmptyString(input.error, `${label}.error`),
  };
  const hasHttpStatus =
    parsed.statusCode !== null &&
    parsed.statusCode >= 100 &&
    parsed.statusCode <= 599;
  const statusIsCanonical =
    (parsed.status === "healthy" &&
      hasHttpStatus &&
      parsed.statusCode! >= 200 &&
      parsed.statusCode! < 300 &&
      parsed.json !== null &&
      parsed.error === null) ||
    (parsed.status === "not_ready" &&
      hasHttpStatus &&
      (parsed.statusCode! < 200 || parsed.statusCode! >= 300) &&
      parsed.json !== null &&
      parsed.error === null) ||
    (parsed.status === "malformed_json" &&
      hasHttpStatus &&
      parsed.json === null &&
      parsed.error !== null) ||
    ((parsed.status === "http_error" || parsed.status === "timeout") &&
      parsed.statusCode === null &&
      parsed.json === null &&
      parsed.error !== null);
  if (!statusIsCanonical) {
    throw new Error(`${label}.status/evidence binding is inconsistent`);
  }
  return parsed;
};

export const parsePidFileObservationV1 = (
  value: unknown,
  label = "PID file observation",
): PidFileObservation => {
  const input = exactRecord(value, label, ["path", "status", "pid"]);
  const parsed: PidFileObservation = {
    path: nonEmptyString(input.path, `${label}.path`),
    status: oneOf(input.status, `${label}.status`, [
      "absent",
      "invalid",
      "stale",
      "runner_owned",
      "foreign",
    ]),
    pid: nullable(input.pid, `${label}.pid`, positiveInteger),
  };
  if (
    ((parsed.status === "absent" || parsed.status === "invalid") &&
      parsed.pid !== null) ||
    ((parsed.status === "stale" ||
      parsed.status === "runner_owned" ||
      parsed.status === "foreign") &&
      parsed.pid === null)
  ) {
    throw new Error(`${label}.status/pid binding is inconsistent`);
  }
  return parsed;
};

const parseOutputTerminationObservationV1 = (
  value: unknown,
  label: string,
): OutputTerminationObservation => {
  const input = exactRecord(value, label, [
    "marker",
    "occurrence",
    "signal",
    "at",
  ]);
  return {
    marker: nonEmptyString(input.marker, `${label}.marker`),
    occurrence: positiveInteger(input.occurrence, `${label}.occurrence`),
    signal: nodeSignal(input.signal, `${label}.signal`),
    at: isoTimestamp(input.at, `${label}.at`),
  };
};

const parseFileTerminationObservationV1 = (
  value: unknown,
  label: string,
): FileTerminationObservation => {
  const input = exactRecord(value, label, ["path", "signal", "at"]);
  return {
    path: nonEmptyString(input.path, `${label}.path`),
    signal: nodeSignal(input.signal, `${label}.signal`),
    at: isoTimestamp(input.at, `${label}.at`),
  };
};

const parseServiceAttemptSummaryV1 = (
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
    classification: parseServiceErrorClassificationV1(
      input.classification,
      `${label}.classification`,
    ),
    cleanup:
      input.cleanup === null
        ? null
        : parseChildProcessCleanupV1(input.cleanup, `${label}.cleanup`),
    outputTermination:
      input.outputTermination === null
        ? null
        : parseOutputTerminationObservationV1(
            input.outputTermination,
            `${label}.outputTermination`,
          ),
    fileTermination:
      input.fileTermination === null
        ? null
        : parseFileTerminationObservationV1(
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

export const parseServiceSupervisorSummaryV1 = (
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
    command: parseRedactedCommandV1(input.command, `${label}.command`),
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
      parseServiceAttemptSummaryV1,
    ),
    restartCount: nonNegativeInteger(
      input.restartCount,
      `${label}.restartCount`,
    ),
    terminalClassification: parseServiceErrorClassificationV1(
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

const redactedCommand = (
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

export const probeHttpEndpoint = async ({
  label,
  url,
  fetchFn = fetch,
  timeoutMs = 5_000,
}: {
  readonly label: string;
  readonly url: string;
  readonly fetchFn?: typeof fetch;
  readonly timeoutMs?: number;
}): Promise<HttpProbeSample> => {
  const started = Date.now();
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), timeoutMs);
  try {
    const response = await fetchFn(url, { signal: controller.signal });
    const latencyMs = Date.now() - started;
    let json: unknown | null = null;
    try {
      json = await response.json();
    } catch {
      return parseHttpProbeSampleV1({
        label,
        url,
        status: "malformed_json",
        statusCode: response.status,
        latencyMs,
        json: null,
        error: "response body was not JSON",
      });
    }
    return parseHttpProbeSampleV1({
      label,
      url,
      status: response.ok ? "healthy" : "not_ready",
      statusCode: response.status,
      latencyMs,
      json,
      error: null,
    });
  } catch (error) {
    return parseHttpProbeSampleV1({
      label,
      url,
      status:
        error instanceof Error && error.name === "AbortError"
          ? "timeout"
          : "http_error",
      statusCode: null,
      latencyMs: Date.now() - started,
      json: null,
      error: error instanceof Error ? error.message : String(error),
    });
  } finally {
    clearTimeout(timeout);
  }
};

const pidAlive = (pid: number): boolean => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

export const inspectPidFile = async ({
  path,
  runnerOwnedPids = new Set<number>(),
}: {
  readonly path: string;
  readonly runnerOwnedPids?: ReadonlySet<number>;
}): Promise<PidFileObservation> => {
  let raw: string;
  try {
    raw = await readFile(path, "utf8");
  } catch {
    return parsePidFileObservationV1({
      path,
      status: "absent",
      pid: null,
    });
  }
  const pid = Number(raw.trim());
  if (!Number.isSafeInteger(pid) || pid <= 0) {
    return parsePidFileObservationV1({
      path,
      status: "invalid",
      pid: null,
    });
  }
  if (!pidAlive(pid)) {
    return parsePidFileObservationV1({ path, status: "stale", pid });
  }
  return parsePidFileObservationV1({
    path,
    status: runnerOwnedPids.has(pid) ? "runner_owned" : "foreign",
    pid,
  });
};

const runAttempt = async (
  spec: HostProcessServiceSpec,
  attempt: number,
  resolvedEnv: BuiltE2EProcessEnv,
): Promise<{
  readonly summary: ServiceAttemptSummary;
  readonly output: string;
}> => {
  const startedAtDate = new Date();
  const attemptResult = await runLoggedChildProcessAttempt({
    command: spec.command,
    args: spec.args,
    cwd: spec.cwd,
    env: resolvedEnv.env,
    rawLogPath: spec.rawLogPath,
    timeoutMs: spec.timeoutMs,
    terminateOnOutput: spec.terminateOnOutput,
    terminateOnFile: spec.terminateOnFile,
    startedAtDate,
    startEvent: ({ pid, startedAt }) => ({
      event: "e2e_service_start",
      service: spec.service,
      attempt,
      pid,
      at: startedAt,
      command: redactedCommand(spec, resolvedEnv.provenance),
    }),
    cleanupEvent: ({ cleanup, at }) => ({
      event: "e2e_service_cleanup",
      service: spec.service,
      attempt,
      at,
      cleanup,
    }),
    ownership: spec.ownership,
  });
  const classification: ServiceErrorClassification =
    attemptResult.outputTermination !== null ||
    attemptResult.fileTermination !== null
      ? {
          class: "restartable_runtime",
          reason:
            attemptResult.outputTermination !== null
              ? `service was externally terminated after output marker ${attemptResult.outputTermination.marker}`
              : `service was externally terminated after stop file ${attemptResult.fileTermination!.path}`,
          restartable: true,
        }
      : attemptResult.error !== null
        ? {
            class: "supervisor_failure",
            reason: attemptResult.error.message,
            restartable: false,
          }
        : attemptResult.timedOut
          ? {
              class: "restartable_runtime",
              reason: `service timed out after ${spec.timeoutMs?.toString()}ms`,
              restartable: true,
            }
          : attemptResult.exitCode === 0
            ? {
                class: "unknown",
                reason: "service exited successfully",
                restartable: false,
              }
            : classifyServiceError({ text: attemptResult.combinedOutput });
  return {
    summary: {
      attempt,
      pid: attemptResult.pid,
      startedAt: attemptResult.startedAt,
      finishedAt: attemptResult.finishedAt,
      durationMs: attemptResult.durationMs,
      exitCode: attemptResult.exitCode,
      signal: attemptResult.signal,
      timedOut: attemptResult.timedOut,
      classification,
      cleanup: attemptResult.cleanup,
      outputTermination: attemptResult.outputTermination,
      fileTermination: attemptResult.fileTermination,
    },
    output: attemptResult.combinedOutput,
  };
};

export const superviseHostProcess = async (
  spec: HostProcessServiceSpec,
): Promise<ServiceSupervisorSummary> => {
  const maxRestarts = spec.maxRestarts ?? 0;
  const restartBackoffMs = spec.restartBackoffMs ?? 1_000;
  const sleep =
    spec.sleep ??
    ((milliseconds: number) =>
      new Promise((resolve) => setTimeout(resolve, milliseconds)));
  const attempts: ServiceAttemptSummary[] = [];
  const resolvedEnv = await buildE2EProcessEnv({
    cwd: spec.cwd,
    envFiles: spec.envFiles,
    overrides: spec.env,
    inherit: spec.envInheritance,
  });
  let restartCount = 0;
  let terminalClassification: ServiceErrorClassification = {
    class: "supervisor_failure",
    reason: "service was not started",
    restartable: false,
  };

  for (let attempt = 1; attempt <= maxRestarts + 1; attempt += 1) {
    const { summary } = await runAttempt(spec, attempt, resolvedEnv);
    attempts.push(summary);
    terminalClassification = summary.classification;
    if (
      summary.exitCode === 0 &&
      summary.signal === null &&
      !summary.timedOut &&
      summary.outputTermination === null &&
      summary.fileTermination === null
    ) {
      return parseServiceSupervisorSummaryV1({
        schemaVersion: E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION,
        service: spec.service,
        command: redactedCommand(spec, resolvedEnv.provenance),
        status: "exited_success",
        rawLogPath: spec.rawLogPath,
        attempts,
        restartCount,
        terminalClassification,
      });
    }
    if (!summary.classification.restartable) {
      return parseServiceSupervisorSummaryV1({
        schemaVersion: E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION,
        service: spec.service,
        command: redactedCommand(spec, resolvedEnv.provenance),
        status:
          summary.classification.class === "supervisor_failure"
            ? "supervisor_failure"
            : summary.timedOut
              ? "timeout"
              : "failed",
        rawLogPath: spec.rawLogPath,
        attempts,
        restartCount,
        terminalClassification,
      });
    }
    if (restartCount >= maxRestarts) {
      return parseServiceSupervisorSummaryV1({
        schemaVersion: E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION,
        service: spec.service,
        command: redactedCommand(spec, resolvedEnv.provenance),
        status: summary.timedOut ? "timeout" : "restart_budget_exhausted",
        rawLogPath: spec.rawLogPath,
        attempts,
        restartCount,
        terminalClassification,
      });
    }
    restartCount += 1;
    await sleep(restartBackoffMs * 2 ** (restartCount - 1));
  }

  return parseServiceSupervisorSummaryV1({
    schemaVersion: E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION,
    service: spec.service,
    command: redactedCommand(spec, resolvedEnv.provenance),
    status: "supervisor_failure",
    rawLogPath: spec.rawLogPath,
    attempts,
    restartCount,
    terminalClassification,
  });
};
