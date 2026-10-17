import {
  arrayOf,
  booleanValue,
  exactLiteral,
  exactRecord,
  nodeSignal,
  nonEmptyString,
  nullable,
  nullableNonEmptyString,
  oneOf,
  positiveInteger,
  stringArray,
} from "midgard-node/artifact-schema";
import {
  type E2EEnvFileProvenance,
  type E2EEnvInheritance,
} from "midgard-node/e2e/env";

import type { ChildProcessCleanupResult } from "./process-cleanup.js";

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

export const parseLowerHex64 = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (!/^[0-9a-f]{64}$/u.test(parsed)) {
    throw new Error(`${label} must be 64 lowercase hexadecimal characters`);
  }
  return parsed;
};

const parseEnvFileProvenance = (
  value: unknown,
  label: string,
): E2EEnvFileProvenance => {
  const input = exactRecord(value, label, ["path", "keys"]);
  return {
    path: nonEmptyString(input.path, `${label}.path`),
    keys: stringArray(input.keys, `${label}.keys`),
  };
};

export const parseRedactedCommand = (
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
      parseEnvFileProvenance,
    ),
    envInheritance: oneOf(input.envInheritance, `${label}.envInheritance`, [
      "process",
      "none",
    ]),
  };
};

export const parseHashObservation = (
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

export const parseTxObservation = (
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

export const parseChildProcessCleanup = (
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
