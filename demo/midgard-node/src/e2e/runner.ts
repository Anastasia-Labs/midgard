import {
  buildE2EProcessEnv,
  type E2EEnvFileProvenance,
  type E2EEnvInheritance,
  type E2EEnvProvenance,
} from "@/e2e/env.js";
import { runLoggedChildProcessAttempt } from "@/e2e/logged-child-process.js";
import type { ChildProcessCleanupResult } from "@/e2e/process-cleanup.js";

export { redactEnvKeys } from "@/e2e/env.js";

export const E2E_STEP_SCHEMA_VERSION = "midgard-e2e-step-v1";

export type StepStatus =
  | "success"
  | "failed"
  | "signaled"
  | "timeout"
  | "runner_error";

export type HashObservation = {
  readonly hash: string;
  readonly role: "unknown";
  readonly source: "regex";
  readonly stepId: string;
};

export type TxObservationRole =
  | "prepared"
  | "submitted"
  | "confirmed"
  | "committed"
  | "root"
  | "input"
  | "unknown";

export type TxObservation = {
  readonly txHash: string;
  readonly role: TxObservationRole;
  readonly status: string;
  readonly source: string;
  readonly field?: string;
  readonly stepId: string;
};

export type RedactedCommand = {
  readonly command: string;
  readonly args: readonly string[];
  readonly cwd: string;
  readonly envKeys: readonly string[];
  readonly envFiles: readonly E2EEnvFileProvenance[];
  readonly envInheritance: E2EEnvInheritance;
};

export type StepSpec = {
  readonly id: string;
  readonly command: string;
  readonly args?: readonly string[];
  readonly cwd: string;
  readonly env?: Readonly<Record<string, string | undefined>>;
  readonly envFiles?: readonly string[];
  readonly envInheritance?: E2EEnvInheritance;
  readonly timeoutMs?: number;
  readonly rawLogPath: string;
};

export type StepSummary = {
  readonly schemaVersion: typeof E2E_STEP_SCHEMA_VERSION;
  readonly id: string;
  readonly status: StepStatus;
  readonly command: RedactedCommand;
  readonly pid: number | null;
  readonly startedAt: string;
  readonly finishedAt: string;
  readonly durationMs: number;
  readonly exitCode: number | null;
  readonly signal: NodeJS.Signals | null;
  readonly timedOut: boolean;
  readonly rawLogPath: string;
  readonly observedTxHashes: readonly string[];
  readonly hashObservations: readonly HashObservation[];
  readonly txObservations: readonly TxObservation[];
  readonly parsedJson: unknown | null;
  readonly error: string | null;
  readonly cleanup?: ChildProcessCleanupResult | null;
};

const SECRET_ARG_PATTERN =
  /(seed|secret|private|password|passphrase|api[_-]?key|blockfrost|admin[_-]?key|token)/i;
const TX_HASH_PATTERN = /\b[0-9a-f]{64}\b/gi;

export const redactArg = (arg: string): string =>
  SECRET_ARG_PATTERN.test(arg) ? "<redacted>" : arg;

const redactedCommand = (
  spec: StepSpec,
  provenance: E2EEnvProvenance,
): RedactedCommand => ({
  command: spec.command,
  args: (spec.args ?? []).map(redactArg),
  cwd: spec.cwd,
  envKeys: provenance.explicitEnvKeys,
  envFiles: provenance.envFiles,
  envInheritance: provenance.inheritance,
});

const uniqueTxHashes = (text: string): readonly string[] =>
  Array.from(
    new Set(
      (text.match(TX_HASH_PATTERN) ?? []).map((hash) => hash.toLowerCase()),
    ),
  );

const hashObservations = (
  text: string,
  stepId: string,
): readonly HashObservation[] =>
  uniqueTxHashes(text).map((hash) => ({
    hash,
    role: "unknown",
    source: "regex",
    stepId,
  }));

const EXPLICIT_TX_LOG_PATTERNS: readonly {
  readonly pattern: RegExp;
  readonly role: TxObservationRole;
  readonly source: string;
}[] = [
  {
    pattern: /\bTransaction submitted:\s*([0-9a-f]{64})\b/gi,
    role: "submitted",
    source: "log:transaction_submitted",
  },
  {
    pattern: /\bTransaction confirmed:\s*([0-9a-f]{64})\b/gi,
    role: "confirmed",
    source: "log:transaction_confirmed",
  },
  {
    pattern: /\b[A-Za-z0-9 _.-]+ submitted:\s*txHash=([0-9a-f]{64})\b/gi,
    role: "submitted",
    source: "log:label_submitted_tx_hash",
  },
  {
    pattern: /\bSigned tx prepared:\s*txHash=([0-9a-f]{64})\b/gi,
    role: "prepared",
    source: "log:signed_tx_prepared",
  },
];

const GENERIC_STRUCTURED_TX_FIELDS = new Set(["txHash", "txId"]);

const STRUCTURED_SUBMITTED_TX_FIELD_NAMES = [
  "registerTxHash",
  "activateTxHash",
  "deregisterTxHash",
  "mergeTxHash",
  "commitTxHash",
  "initTxHash",
  "applyTxHash",
  "addSignaturesTxHash",
] as const;

const STRUCTURED_SUBMITTED_TX_FIELDS: ReadonlySet<string> = new Set(
  STRUCTURED_SUBMITTED_TX_FIELD_NAMES,
);

const STRUCTURED_TX_FIELDS = new Set([
  "txHash",
  "txId",
  ...STRUCTURED_SUBMITTED_TX_FIELDS,
]);

const txObservationRoleFromExplicitStatus = (
  status: unknown,
): TxObservationRole | undefined => {
  if (
    status === "submitted" ||
    status === "confirmed" ||
    status === "committed"
  ) {
    return status;
  }
  if (status === "prepared") {
    return "prepared";
  }
  return undefined;
};

const txObservationRoleFromStructuredField = (
  field: string,
  status: unknown,
): TxObservationRole | undefined => {
  const roleFromStatus = txObservationRoleFromExplicitStatus(status);
  if (roleFromStatus !== undefined) {
    return roleFromStatus;
  }
  if (STRUCTURED_SUBMITTED_TX_FIELDS.has(field)) {
    return "submitted";
  }
  if (GENERIC_STRUCTURED_TX_FIELDS.has(field)) {
    return undefined;
  }
  return undefined;
};

const isRecord = (value: unknown): value is Readonly<Record<string, unknown>> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

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

const parseLastJsonLine = (text: string): unknown | null => {
  const lines = text
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter((line) => line.startsWith("{") && line.endsWith("}"));
  for (let index = lines.length - 1; index >= 0; index -= 1) {
    try {
      return JSON.parse(lines[index]!);
    } catch {
      continue;
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
  const parsedJson = parseLastJsonLine(attempt.stdout);
  return {
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
  };
};
