import { type DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import { exactRecord } from "../artifact-schema.js";

export const DEPLOYMENT_RUN_STATE_SCHEMA_VERSION =
  "midgard-deployment-run-state-v1";

export type DeploymentRunMode = "attach" | "resume" | "fresh";

export type DeploymentRunIdentity = {
  readonly network?: string;
  readonly hubOracleOneShot?: {
    readonly txHash: string;
    readonly outputIndex: number;
  };
  readonly referenceScriptAuthPolicyId?: string;
  readonly referenceScriptAuthPolicy?: {
    readonly policyId: string;
    readonly nativeScript: {
      readonly type: "Native";
      readonly cborHex: string;
      readonly expiresAtSlot: number;
      readonly expiresAtUnixTime: number;
      readonly timelockDurationMs: number;
    };
  };
  readonly manifestPath?: string;
  readonly manifestSha256?: string;
  readonly deploymentMarker?: DeploymentMarker;
};

export type DeploymentStepStatus =
  | "not_started"
  | "submitted"
  | "confirmed"
  | "complete"
  | "blocked"
  | "failed";

export type DeploymentStepState = {
  readonly status: DeploymentStepStatus;
  readonly updatedAt: string;
  readonly txHashes?: readonly string[];
  readonly outRefs?: readonly string[];
  readonly message?: string;
  readonly evidence?: readonly string[];
  readonly details?: Readonly<Record<string, string>>;
};

export type DeploymentRunEvent = {
  readonly at: string;
  readonly kind: string;
  readonly message: string;
  readonly stepId?: string;
};

export type DeploymentRunState = {
  readonly schemaVersion: typeof DEPLOYMENT_RUN_STATE_SCHEMA_VERSION;
  readonly runId: string;
  readonly createdAt: string;
  readonly updatedAt: string;
  readonly mode: DeploymentRunMode;
  readonly identity: DeploymentRunIdentity;
  readonly steps: Readonly<Record<string, DeploymentStepState>>;
  readonly events: readonly DeploymentRunEvent[];
};

export class RunStateError extends Error {
  constructor(message: string, options?: { readonly cause?: unknown }) {
    super(message, options);
    this.name = "RunStateError";
  }
}

export const exactRunStateRecord = (
  value: unknown,
  label: string,
  requiredKeys: readonly string[],
  optionalKeys: readonly string[] = [],
): Record<string, unknown> => {
  try {
    return exactRecord(value, label, requiredKeys, optionalKeys);
  } catch (cause) {
    throw new RunStateError(
      cause instanceof Error ? cause.message : String(cause),
      { cause },
    );
  }
};

export const assertRecord = (
  value: unknown,
  label: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new RunStateError(`${label} must be an object.`);
  }
  return value as Record<string, unknown>;
};

export const assertString = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.trim().length === 0 ||
    value !== value.trim()
  ) {
    throw new RunStateError(
      `${label} must be a canonical non-empty string without surrounding whitespace.`,
    );
  }
  return value;
};

export const assertLowerHex = (
  value: unknown,
  label: string,
  byteLength: number,
): string => {
  const parsed = assertString(value, label);
  if (parsed.length !== byteLength * 2 || !/^[0-9a-f]+$/u.test(parsed)) {
    throw new RunStateError(
      `${label} must be ${byteLength.toString()} bytes of lowercase hexadecimal.`,
    );
  }
  return parsed;
};

export const assertIsoString = (value: unknown, label: string): string => {
  const text = assertString(value, label);
  if (Number.isNaN(Date.parse(text)) || new Date(text).toISOString() !== text) {
    throw new RunStateError(`${label} must be a canonical ISO timestamp.`);
  }
  return text;
};

export const assertStringArray = (
  value: unknown,
  label: string,
): readonly string[] | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (
    !Array.isArray(value) ||
    value.some((entry) => typeof entry !== "string")
  ) {
    throw new RunStateError(`${label} must be an array of strings.`);
  }
  const parsed = value.map((entry, index) =>
    assertString(entry, `${label}[${index.toString()}]`),
  );
  if (new Set(parsed).size !== parsed.length) {
    throw new RunStateError(`${label} must not contain duplicates.`);
  }
  return parsed;
};

export const assertStringRecord = (
  value: unknown,
  label: string,
): Readonly<Record<string, string>> | undefined => {
  if (value === undefined) {
    return undefined;
  }
  const input = assertRecord(value, label);
  for (const [key, entry] of Object.entries(input)) {
    if (typeof entry !== "string") {
      throw new RunStateError(`${label}.${key} must be a string.`);
    }
  }
  return input as Readonly<Record<string, string>>;
};

export const parseMode = (value: unknown): DeploymentRunMode => {
  if (value === "attach" || value === "resume" || value === "fresh") {
    return value;
  }
  throw new RunStateError("mode must be attach, resume, or fresh.");
};

export const parseStepStatus = (value: unknown): DeploymentStepStatus => {
  if (
    value === "not_started" ||
    value === "submitted" ||
    value === "confirmed" ||
    value === "complete" ||
    value === "blocked" ||
    value === "failed"
  ) {
    return value;
  }
  throw new RunStateError("step.status is invalid.");
};
