import { readFile } from "node:fs/promises";

import {
  buildE2EProcessEnv,
  type BuiltE2EProcessEnv,
} from "midgard-node/e2e/env";

import { runLoggedChildProcessAttempt } from "./logged-child-process.js";
import {
  E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION,
  type HostProcessServiceSpec,
  type HttpProbeSample,
  parseHttpProbeSample,
  parsePidFileObservation,
  type PidFileObservation,
  type ServiceAttemptSummary,
  type ServiceErrorClassification,
  type ServiceSupervisorSummary,
} from "./service-supervisor.parse-http-probe-sample.js";
import {
  classifyServiceError,
  parseServiceSupervisorSummary,
  redactedCommand,
} from "./service-supervisor.parse-service-supervisor-summary.js";

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
      return parseHttpProbeSample({
        label,
        url,
        status: "malformed_json",
        statusCode: response.status,
        latencyMs,
        json: null,
        error: "response body was not JSON",
      });
    }
    return parseHttpProbeSample({
      label,
      url,
      status: response.ok ? "healthy" : "not_ready",
      statusCode: response.status,
      latencyMs,
      json,
      error: null,
    });
  } catch (error) {
    return parseHttpProbeSample({
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
    return parsePidFileObservation({
      path,
      status: "absent",
      pid: null,
    });
  }
  const pid = Number(raw.trim());
  if (!Number.isSafeInteger(pid) || pid <= 0) {
    return parsePidFileObservation({
      path,
      status: "invalid",
      pid: null,
    });
  }
  if (!pidAlive(pid)) {
    return parsePidFileObservation({ path, status: "stale", pid });
  }
  return parsePidFileObservation({
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
      return parseServiceSupervisorSummary({
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
      return parseServiceSupervisorSummary({
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
      return parseServiceSupervisorSummary({
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

  return parseServiceSupervisorSummary({
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
