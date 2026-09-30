import {
  arrayOf,
  booleanValue,
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
} from "midgard-node/artifact-schema";
import { type E2EEnvProvenance } from "midgard-node/e2e/env";

import {
  E2E_STEP_SCHEMA_VERSION,
  type HashObservation,
  parseChildProcessCleanup,
  parseHashObservation,
  parseLowerHex64,
  parseRedactedCommand,
  parseTxObservation,
  type RedactedCommand,
  type StepSpec,
  type StepSummary,
  type TxObservationRole,
} from "./runner.parse-child-process-cleanup.js";

export const parseE2EStep = (
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
    command: parseRedactedCommand(input.command, `${label}.command`),
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
      parseHashObservation,
    ),
    txObservations: arrayOf(
      input.txObservations,
      `${label}.txObservations`,
      parseTxObservation,
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
              : parseChildProcessCleanup(input.cleanup, `${label}.cleanup`),
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

export const redactedCommand = (
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

export const uniqueTxHashes = (text: string): readonly string[] =>
  Array.from(
    new Set(
      (text.match(TX_HASH_PATTERN) ?? []).map((hash) => hash.toLowerCase()),
    ),
  );

export const hashObservations = (
  text: string,
  stepId: string,
): readonly HashObservation[] =>
  uniqueTxHashes(text).map((hash) => ({
    hash,
    role: "unknown",
    source: "regex",
    stepId,
  }));

export const EXPLICIT_TX_LOG_PATTERNS: readonly {
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

export const STRUCTURED_SUBMITTED_TX_FIELD_NAMES = [
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

export const STRUCTURED_TX_FIELDS = new Set([
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

export const txObservationRoleFromStructuredField = (
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

export const isRecord = (
  value: unknown,
): value is Readonly<Record<string, unknown>> =>
  typeof value === "object" && value !== null && !Array.isArray(value);
