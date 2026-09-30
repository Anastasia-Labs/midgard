import { buildE2EProcessEnv } from "midgard-node/e2e/env";

import { runLoggedChildProcessAttempt } from "./logged-child-process.js";
import {
  E2E_STEP_SCHEMA_VERSION,
  type StepSpec,
  type StepStatus,
  type StepSummary,
  type TxObservation,
} from "./runner.parse-child-process-cleanup.js";
import {
  EXPLICIT_TX_LOG_PATTERNS,
  hashObservations,
  isRecord,
  parseE2EStep,
  redactedCommand,
  STRUCTURED_SUBMITTED_TX_FIELD_NAMES,
  STRUCTURED_TX_FIELDS,
  txObservationRoleFromStructuredField,
  uniqueTxHashes,
} from "./runner.parse-e2-estep.js";

const structuredTxObservations = (
  value: unknown,
  stepId: string,
  path = "$",
): readonly TxObservation[] => {
  if (Array.isArray(value)) {
    return value.flatMap((entry, index) =>
      structuredTxObservations(entry, stepId, `${path}[${index.toString()}]`),
    );
  }
  if (!isRecord(value)) {
    return [];
  }
  const status = value.status;
  const direct = Object.entries(value).flatMap(
    ([field, fieldValue]): readonly TxObservation[] => {
      if (
        !STRUCTURED_TX_FIELDS.has(field) ||
        typeof fieldValue !== "string" ||
        !/^[0-9a-f]{64}$/i.test(fieldValue)
      ) {
        return [];
      }
      const role = txObservationRoleFromStructuredField(field, status);
      if (role === undefined) {
        return [];
      }
      return [
        {
          txHash: fieldValue.toLowerCase(),
          role,
          status: role,
          source: "parsedJson",
          field: `${path}.${field}`,
          stepId,
        },
      ];
    },
  );
  return [
    ...direct,
    ...Object.entries(value).flatMap(([field, fieldValue]) =>
      structuredTxObservations(fieldValue, stepId, `${path}.${field}`),
    ),
  ];
};

const logTxObservations = (
  text: string,
  stepId: string,
): readonly TxObservation[] =>
  EXPLICIT_TX_LOG_PATTERNS.flatMap(({ pattern, role, source }) => {
    const observations: TxObservation[] = [];
    for (const match of text.matchAll(pattern)) {
      const txHash = match[1];
      if (txHash !== undefined) {
        observations.push({
          txHash: txHash.toLowerCase(),
          role,
          status: role,
          source,
          stepId,
        });
      }
    }
    return observations;
  });

const STRUCTURED_TX_FIELD_LOG_PATTERN = new RegExp(
  `\\b(${STRUCTURED_SUBMITTED_TX_FIELD_NAMES.join("|")})=([0-9a-f]{64})\\b`,
  "gi",
);

const logStructuredTxFieldObservations = (
  text: string,
  stepId: string,
): readonly TxObservation[] => {
  const observations: TxObservation[] = [];
  for (const match of text.matchAll(STRUCTURED_TX_FIELD_LOG_PATTERN)) {
    const field = match[1];
    const txHash = match[2];
    if (field !== undefined && txHash !== undefined) {
      observations.push({
        txHash: txHash.toLowerCase(),
        role: "submitted",
        status: "submitted",
        source: "log:structured_tx_field",
        field: `$.${field}`,
        stepId,
      });
    }
  }
  return observations;
};

const uniqueTxObservations = (
  observations: readonly TxObservation[],
): readonly TxObservation[] => {
  const seen = new Set<string>();
  const unique: TxObservation[] = [];
  for (const observation of observations) {
    const key = [
      observation.stepId,
      observation.txHash,
      observation.role,
      observation.source,
      observation.field ?? "",
    ].join(":");
    if (!seen.has(key)) {
      seen.add(key);
      unique.push(observation);
    }
  }
  return unique;
};

const explicitTxObservations = ({
  combined,
  parsedJson,
  stepId,
}: {
  readonly combined: string;
  readonly parsedJson: unknown | null;
  readonly stepId: string;
}): readonly TxObservation[] =>
  uniqueTxObservations([
    ...logTxObservations(combined, stepId),
    ...logStructuredTxFieldObservations(combined, stepId),
    ...structuredTxObservations(parsedJson, stepId),
  ]);

/**
 * Returns the last JSON object in stdout: either a one-line object or a
 * pretty-printed one (the node CLI's `formatJson`) whose top-level braces
 * sit alone at column 0.
 */
const parseLastJsonDocument = (text: string): unknown | null => {
  const lines = text.split(/\r?\n/);
  for (let end = lines.length - 1; end >= 0; end -= 1) {
    const line = lines[end]!;
    const trimmed = line.trim();
    const candidates: string[] = [];
    if (trimmed.startsWith("{") && trimmed.endsWith("}")) {
      candidates.push(trimmed);
    }
    if (line === "}" && end > 0) {
      const start = lines.lastIndexOf("{", end - 1);
      if (start >= 0) {
        candidates.push(lines.slice(start, end + 1).join("\n"));
      }
    }
    for (const candidate of candidates) {
      try {
        return JSON.parse(candidate);
      } catch {
        continue;
      }
    }
  }
  return null;
};

export const runCommandStep = async (spec: StepSpec): Promise<StepSummary> => {
  const startedAtDate = new Date();
  const args = [...(spec.args ?? [])];
  const { env, provenance } = await buildE2EProcessEnv({
    cwd: spec.cwd,
    envFiles: spec.envFiles,
    overrides: spec.env,
    inherit: spec.envInheritance,
  });
  const command = redactedCommand(spec, provenance);
  const attempt = await runLoggedChildProcessAttempt({
    command: spec.command,
    args,
    cwd: spec.cwd,
    env,
    rawLogPath: spec.rawLogPath,
    timeoutMs: spec.timeoutMs,
    startedAtDate,
    startEvent: ({ pid, startedAt }) => ({
      event: "started",
      id: spec.id,
      pid,
      at: startedAt,
      command,
    }),
    cleanupEvent: ({ cleanup, at }) => ({
      event: "cleanup",
      id: spec.id,
      at,
      cleanup,
    }),
  });
  const status: StepStatus =
    attempt.error !== null
      ? "runner_error"
      : attempt.timedOut
        ? "timeout"
        : attempt.signal !== null
          ? "signaled"
          : attempt.exitCode === 0
            ? "success"
            : "failed";
  const parsedJson = parseLastJsonDocument(attempt.stdout);
  return parseE2EStep({
    schemaVersion: E2E_STEP_SCHEMA_VERSION,
    id: spec.id,
    status,
    command,
    pid: attempt.pid,
    startedAt: attempt.startedAt,
    finishedAt: attempt.finishedAt,
    durationMs: attempt.durationMs,
    exitCode: attempt.exitCode,
    signal: attempt.signal,
    timedOut: attempt.timedOut,
    rawLogPath: spec.rawLogPath,
    observedTxHashes: uniqueTxHashes(attempt.combinedOutput),
    hashObservations: hashObservations(attempt.combinedOutput, spec.id),
    txObservations: explicitTxObservations({
      combined: attempt.combinedOutput,
      parsedJson,
      stepId: spec.id,
    }),
    parsedJson,
    error:
      status === "success"
        ? null
        : attempt.error !== null
          ? attempt.error.message
          : attempt.timedOut
            ? `Step timed out after ${spec.timeoutMs?.toString()}ms.`
            : `Step exited with status ${attempt.exitCode?.toString() ?? attempt.signal ?? "unknown"}.`,
    cleanup: attempt.cleanup,
  });
};
