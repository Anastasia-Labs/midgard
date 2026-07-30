import {
  buildE2EProcessEnv,
  type E2EEnvFileProvenance,
  type E2EEnvInheritance,
  type E2EEnvProvenance,
} from "@/e2e/env.js";
import {
  arrayOf,
  booleanValue,
  exactLiteral,
  exactRecord,
  integer,
  isoTimestamp,
  jsonValue,
  nodeSignal,
  nonEmptyString,
  nonNegativeNumber,
  nullable,
  nullableNonEmptyString,
  oneOf,
  positiveInteger,
  stringArray,
} from "@/e2e/exact-artifact.js";
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

const parseLowerHex64 = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (!/^[0-9a-f]{64}$/u.test(parsed)) {
    throw new Error(`${label} must be 64 lowercase hexadecimal characters`);
  }
  return parsed;
};

const parseEnvFileProvenanceV1 = (
  value: unknown,
  label: string,
): E2EEnvFileProvenance => {
  const input = exactRecord(value, label, ["path", "keys"]);
  return {
    path: nonEmptyString(input.path, `${label}.path`),
    keys: stringArray(input.keys, `${label}.keys`),
  };
};

export const parseRedactedCommandV1 = (
  value: unknown,
  label = "command",
): RedactedCommand => {
  const input = exactRecord(value, label, [
    "command",
    "args",
    "cwd",
    "envKeys",
    "envFiles",
    "envInheritance",
  ]);
  return {
    command: nonEmptyString(input.command, `${label}.command`),
    args: stringArray(input.args, `${label}.args`),
    cwd: nonEmptyString(input.cwd, `${label}.cwd`),
    envKeys: stringArray(input.envKeys, `${label}.envKeys`),
    envFiles: arrayOf(
      input.envFiles,
      `${label}.envFiles`,
      parseEnvFileProvenanceV1,
    ),
    envInheritance: oneOf(input.envInheritance, `${label}.envInheritance`, [
      "process",
      "none",
    ]),
  };
};

const parseHashObservationV1 = (
  value: unknown,
  label: string,
): HashObservation => {
  const input = exactRecord(value, label, ["hash", "role", "source", "stepId"]);
  return {
    hash: parseLowerHex64(input.hash, `${label}.hash`),
    role: exactLiteral(input.role, `${label}.role`, "unknown"),
    source: exactLiteral(input.source, `${label}.source`, "regex"),
    stepId: nonEmptyString(input.stepId, `${label}.stepId`),
  };
};

export const parseTxObservationV1 = (
  value: unknown,
  label = "txObservation",
): TxObservation => {
  const input = exactRecord(
    value,
    label,
    ["txHash", "role", "status", "source", "stepId"],
    ["field"],
  );
  return {
    txHash: parseLowerHex64(input.txHash, `${label}.txHash`),
    role: oneOf(input.role, `${label}.role`, [
      "prepared",
      "submitted",
      "confirmed",
      "committed",
      "root",
      "input",
      "unknown",
    ]),
    status: nonEmptyString(input.status, `${label}.status`),
    source: nonEmptyString(input.source, `${label}.source`),
    ...(input.field === undefined
      ? {}
      : { field: nonEmptyString(input.field, `${label}.field`) }),
    stepId: nonEmptyString(input.stepId, `${label}.stepId`),
  };
};

export const parseChildProcessCleanupV1 = (
  value: unknown,
  label: string,
): ChildProcessCleanupResult => {
  const input = exactRecord(
    value,
    label,
    ["attempted", "pid", "target", "signal", "success", "error"],
    ["ownershipValidation"],
  );
  const ownershipValidation =
    input.ownershipValidation === undefined
      ? undefined
      : exactRecord(input.ownershipValidation, `${label}.ownershipValidation`, [
          "valid",
          "reason",
        ]);
  return {
    attempted: booleanValue(input.attempted, `${label}.attempted`),
    pid: nullable(input.pid, `${label}.pid`, positiveInteger),
    target: oneOf(input.target, `${label}.target`, [
      "process_group",
      "process",
      "none",
    ]),
    signal: nodeSignal(input.signal, `${label}.signal`),
    success: booleanValue(input.success, `${label}.success`),
    error: nullableNonEmptyString(input.error, `${label}.error`),
    ...(ownershipValidation === undefined
      ? {}
      : {
          ownershipValidation: {
            valid: booleanValue(
              ownershipValidation.valid,
              `${label}.ownershipValidation.valid`,
            ),
            reason: nonEmptyString(
              ownershipValidation.reason,
              `${label}.ownershipValidation.reason`,
            ),
          },
        }),
  };
};

export const parseE2EStepV1 = (
  value: unknown,
  label = "E2E step",
): StepSummary => {
  const input = exactRecord(
    value,
    label,
    [
      "schemaVersion",
      "id",
      "status",
      "command",
      "pid",
      "startedAt",
      "finishedAt",
      "durationMs",
      "exitCode",
      "signal",
      "timedOut",
      "rawLogPath",
      "observedTxHashes",
      "hashObservations",
      "txObservations",
      "parsedJson",
      "error",
    ],
    ["cleanup"],
  );
  if (input.schemaVersion !== E2E_STEP_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be ${E2E_STEP_SCHEMA_VERSION}`,
    );
  }
  const parsed: StepSummary = {
    schemaVersion: E2E_STEP_SCHEMA_VERSION,
    id: nonEmptyString(input.id, `${label}.id`),
    status: oneOf(input.status, `${label}.status`, [
      "success",
      "failed",
      "signaled",
      "timeout",
      "runner_error",
    ]),
    command: parseRedactedCommandV1(input.command, `${label}.command`),
    pid: nullable(input.pid, `${label}.pid`, positiveInteger),
    startedAt: isoTimestamp(input.startedAt, `${label}.startedAt`),
    finishedAt: isoTimestamp(input.finishedAt, `${label}.finishedAt`),
    durationMs: nonNegativeNumber(input.durationMs, `${label}.durationMs`),
    exitCode: nullable(input.exitCode, `${label}.exitCode`, integer),
    signal: nullable(input.signal, `${label}.signal`, nodeSignal),
    timedOut: booleanValue(input.timedOut, `${label}.timedOut`),
    rawLogPath: nonEmptyString(input.rawLogPath, `${label}.rawLogPath`),
    observedTxHashes: arrayOf(
      input.observedTxHashes,
      `${label}.observedTxHashes`,
      parseLowerHex64,
    ),
    hashObservations: arrayOf(
      input.hashObservations,
      `${label}.hashObservations`,
      parseHashObservationV1,
    ),
    txObservations: arrayOf(
      input.txObservations,
      `${label}.txObservations`,
      parseTxObservationV1,
    ),
    parsedJson:
      input.parsedJson === null
        ? null
        : jsonValue(input.parsedJson, `${label}.parsedJson`),
    error: nullableNonEmptyString(input.error, `${label}.error`),
    ...(input.cleanup === undefined
      ? {}
      : {
          cleanup:
            input.cleanup === null
              ? null
              : parseChildProcessCleanupV1(input.cleanup, `${label}.cleanup`),
        }),
  };
  const elapsedMs =
    Date.parse(parsed.finishedAt) - Date.parse(parsed.startedAt);
  const observationHashes = parsed.hashObservations.map(
    (observation) => observation.hash,
  );
  if (
    elapsedMs < 0 ||
    parsed.durationMs !== elapsedMs ||
    new Set(parsed.observedTxHashes).size !== parsed.observedTxHashes.length ||
    observationHashes.length !== parsed.observedTxHashes.length ||
    observationHashes.some(
      (hash, index) => hash !== parsed.observedTxHashes[index],
    ) ||
    parsed.hashObservations.some(
      (observation) => observation.stepId !== parsed.id,
    ) ||
    parsed.txObservations.some(
      (observation) => observation.stepId !== parsed.id,
    )
  ) {
    throw new Error(`${label} timing or observation identity is inconsistent`);
  }
  const hasError = parsed.error !== null;
  const statusIsCanonical =
    (parsed.status === "success" &&
      parsed.exitCode === 0 &&
      parsed.signal === null &&
      !parsed.timedOut &&
      !hasError) ||
    (parsed.status === "failed" &&
      parsed.exitCode !== null &&
      parsed.exitCode !== 0 &&
      parsed.signal === null &&
      !parsed.timedOut &&
      hasError) ||
    (parsed.status === "signaled" &&
      parsed.signal !== null &&
      !parsed.timedOut &&
      hasError) ||
    (parsed.status === "timeout" && parsed.timedOut && hasError) ||
    (parsed.status === "runner_error" &&
      parsed.exitCode === null &&
      parsed.signal === null &&
      !parsed.timedOut &&
      hasError);
  if (!statusIsCanonical) {
    throw new Error(`${label} status and process outcome are inconsistent`);
  }
  return parsed;
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
  return parseE2EStepV1({
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
