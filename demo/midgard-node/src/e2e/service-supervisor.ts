import { readFile } from "node:fs/promises";

import {
  buildE2EProcessEnv,
  type BuiltE2EProcessEnv,
  type E2EEnvInheritance,
  type E2EEnvProvenance,
} from "@/e2e/env.js";
import { runLoggedChildProcessAttempt } from "@/e2e/logged-child-process.js";
import type {
  FileTerminationObservation,
  FileTerminationSpec,
  OutputTerminationObservation,
  OutputTerminationSpec,
} from "@/e2e/logged-child-process.js";
import type { ChildProcessCleanupResult } from "@/e2e/process-cleanup.js";
import type { OwnedProcessGroupSpec } from "@/e2e/process-ownership.js";
import { redactArg, type RedactedCommand } from "@/e2e/runner.js";

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
      return {
        label,
        url,
        status: "malformed_json",
        statusCode: response.status,
        latencyMs,
        json: null,
        error: "response body was not JSON",
      };
    }
    return {
      label,
      url,
      status: response.ok ? "healthy" : "not_ready",
      statusCode: response.status,
      latencyMs,
      json,
      error: null,
    };
  } catch (error) {
    return {
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
    };
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
    return { path, status: "absent", pid: null };
  }
  const pid = Number(raw.trim());
  if (!Number.isSafeInteger(pid) || pid <= 0) {
    return { path, status: "invalid", pid: null };
  }
  if (!pidAlive(pid)) {
    return { path, status: "stale", pid };
  }
  return {
    path,
    status: runnerOwnedPids.has(pid) ? "runner_owned" : "foreign",
    pid,
  };
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
    if (summary.exitCode === 0 && !summary.timedOut) {
      return {
        schemaVersion: E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION,
        service: spec.service,
        command: redactedCommand(spec, resolvedEnv.provenance),
        status: "exited_success",
        rawLogPath: spec.rawLogPath,
        attempts,
        restartCount,
        terminalClassification,
      };
    }
    if (!summary.classification.restartable) {
      return {
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
      };
    }
    if (restartCount >= maxRestarts) {
      return {
        schemaVersion: E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION,
        service: spec.service,
        command: redactedCommand(spec, resolvedEnv.provenance),
        status: summary.timedOut ? "timeout" : "restart_budget_exhausted",
        rawLogPath: spec.rawLogPath,
        attempts,
        restartCount,
        terminalClassification,
      };
    }
    restartCount += 1;
    await sleep(restartBackoffMs * 2 ** (restartCount - 1));
  }

  return {
    schemaVersion: E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION,
    service: spec.service,
    command: redactedCommand(spec, resolvedEnv.provenance),
    status: "supervisor_failure",
    rawLogPath: spec.rawLogPath,
    attempts,
    restartCount,
    terminalClassification,
  };
};
