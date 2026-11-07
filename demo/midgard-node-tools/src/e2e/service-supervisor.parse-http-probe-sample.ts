import {
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
import { type E2EEnvInheritance } from "midgard-node/e2e/env";

import type {
  FileTerminationObservation,
  FileTerminationSpec,
  OutputTerminationObservation,
  OutputTerminationSpec,
} from "./logged-child-process.js";
import type { ChildProcessCleanupResult } from "./process-cleanup.js";
import type { OwnedProcessGroupSpec } from "./process-ownership.js";
import { type RedactedCommand } from "./runner.js";

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

export const parseServiceErrorClassification = (
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

export const parseHttpProbeSample = (
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

export const parsePidFileObservation = (
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

export const parseOutputTerminationObservation = (
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

export const parseFileTerminationObservation = (
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
