import {
  MIDGARD_CONSENSUS_FEATURES,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE_ID,
  MIDGARD_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/consensus-profile";
import { type DeploymentManifestJsonValue } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  MidgardValidationPhase,
  type MidgardValidationPhaseName,
} from "@al-ft/midgard-core/validation-trace";

export const WATCHER_RULE_BUNDLE_SCHEMA_VERSION =
  "midgard-watcher-rule-bundle-v1" as const;

export const WATCHER_RULE_BUNDLE_VERSION = 1 as const;

export const WATCHER_RULE_BUNDLE_REJECTION_SELECTION =
  "first_rejection_by_phase_then_program_counter_v1" as const;

const HEX_32 = /^[0-9a-f]{64}$/u;

const COMMITMENT_NAME = /^[a-z][a-z0-9]*(?:[-_.][a-z0-9]+)*$/u;

export const WATCHER_RULE_BUNDLE_TRANSITION_PRIORITY = Object.freeze([
  "withdrawal",
  "forced_transaction",
  "l2_transaction",
  "deposit",
] as const);

export const WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY = Object.freeze(
  Object.entries(MidgardValidationPhase)
    .sort((left, right) => left[1] - right[1])
    .map(([name], index) => {
      if (
        MidgardValidationPhase[name as MidgardValidationPhaseName] !== index
      ) {
        throw new Error(
          "Canonical V1 validation phases must be contiguous from zero",
        );
      }
      return name as MidgardValidationPhaseName;
    }),
);

export type WatcherRuleBundleErrorCode =
  | "consensus_profile_mismatch"
  | "deployment_identity_mismatch"
  | "disabled_feature"
  | "feature_set_mismatch"
  | "invalid_field"
  | "missing_field"
  | "program_commitment_mismatch"
  | "rule_bundle_commitment_mismatch"
  | "target_parameters_mismatch"
  | "transition_priority_mismatch"
  | "unknown_feature"
  | "unknown_field"
  | "unsupported_version"
  | "validation_priority_mismatch";

export class WatcherRuleBundleError extends Error {
  readonly code: WatcherRuleBundleErrorCode;
  readonly path: string;

  constructor(code: WatcherRuleBundleErrorCode, path: string) {
    super(`Watcher canonical V1 rule bundle rejected: ${code} at ${path}`);
    this.name = "WatcherRuleBundleV1Error";
    this.code = code;
    this.path = path;
  }
}

export const fail = (code: WatcherRuleBundleErrorCode, path: string): never => {
  throw new WatcherRuleBundleError(code, path);
};

type JsonRecord = Record<string, unknown>;

export const plainRecord = (value: unknown, path: string): JsonRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    fail("invalid_field", path);
  }
  const record = value as JsonRecord;
  if (Reflect.ownKeys(record).length !== Object.keys(record).length) {
    fail("invalid_field", path);
  }
  return record;
};

export const exactRecord = (
  value: unknown,
  path: string,
  requiredKeys: readonly string[],
): JsonRecord => {
  const record = plainRecord(value, path);
  const allowed = new Set(requiredKeys);
  for (const key of Object.keys(record)) {
    if (!allowed.has(key)) {
      fail("unknown_field", `${path}.${key}`);
    }
  }
  for (const key of requiredKeys) {
    if (!Object.prototype.hasOwnProperty.call(record, key)) {
      fail("missing_field", `${path}.${key}`);
    }
  }
  return record;
};

export const denseArray = (
  value: unknown,
  path: string,
): readonly unknown[] => {
  if (!Array.isArray(value)) {
    return fail("invalid_field", path);
  }
  if (Object.keys(value).length !== value.length) {
    return fail("invalid_field", path);
  }
  const ownKeys = Reflect.ownKeys(value);
  if (
    ownKeys.some(
      (key) =>
        typeof key !== "string" ||
        (key !== "length" && !/^(?:0|[1-9][0-9]*)$/u.test(key)),
    )
  ) {
    return fail("invalid_field", path);
  }
  return value;
};

export const exactHex32 = (value: unknown, path: string): string => {
  if (typeof value !== "string" || !HEX_32.test(value)) {
    return fail("invalid_field", path);
  }
  return value;
};

export const exactNetwork = (
  value: unknown,
  path: string,
): "Mainnet" | "Preprod" | "Preview" | "Custom" => {
  if (
    value !== "Mainnet" &&
    value !== "Preprod" &&
    value !== "Preview" &&
    value !== "Custom"
  ) {
    return fail("invalid_field", path);
  }
  return value;
};

export const canonicalJsonValue = (
  value: unknown,
  path: string,
): DeploymentManifestJsonValue => {
  if (
    value === null ||
    typeof value === "string" ||
    typeof value === "boolean"
  ) {
    return value;
  }
  if (typeof value === "number") {
    if (!Number.isFinite(value)) {
      fail("invalid_field", path);
    }
    return Object.is(value, -0) ? 0 : value;
  }
  if (Array.isArray(value)) {
    return Object.freeze(
      denseArray(value, path).map((entry, index) =>
        canonicalJsonValue(entry, `${path}[${index.toString()}]`),
      ),
    );
  }
  const record = plainRecord(value, path);
  return Object.freeze(
    Object.fromEntries(
      Object.keys(record)
        .sort()
        .map((key) => [key, canonicalJsonValue(record[key], `${path}.${key}`)]),
    ),
  );
};

export const equalStringMaps = (
  left: Readonly<Record<string, string>>,
  right: Readonly<Record<string, string>>,
): boolean => {
  const leftKeys = Object.keys(left).sort();
  const rightKeys = Object.keys(right).sort();
  return (
    leftKeys.length === rightKeys.length &&
    leftKeys.every(
      (key, index) =>
        key === rightKeys[index] && left[key] === right[rightKeys[index]!],
    )
  );
};

export const parseProgramCommitments = (
  value: unknown,
  path: string,
): Readonly<Record<string, string>> => {
  const record = plainRecord(value, path);
  const keys = Object.keys(record).sort();
  if (keys.length === 0) {
    fail("missing_field", path);
  }
  return Object.freeze(
    Object.fromEntries(
      keys.map((key) => {
        if (!COMMITMENT_NAME.test(key)) {
          fail("invalid_field", `${path}.${key}`);
        }
        return [key, exactHex32(record[key], `${path}.${key}`)];
      }),
    ),
  );
};

export type WatcherRuleBundleFeature = Readonly<{
  featureId: (typeof MIDGARD_CONSENSUS_FEATURES)[number];
  enabled: true;
}>;

export type WatcherRuleBundleTargetParameters = Readonly<{
  snapshot: Readonly<Record<string, DeploymentManifestJsonValue>>;
  digest: string;
}>;

export type WatcherRuleBundle = Readonly<{
  schemaVersion: typeof WATCHER_RULE_BUNDLE_SCHEMA_VERSION;
  ruleBundleVersion: typeof WATCHER_RULE_BUNDLE_VERSION;
  deploymentManifestId: string;
  network: "Mainnet" | "Preprod" | "Preview" | "Custom";
  blueprintHash: string;
  consensusProfileId: typeof MIDGARD_CONSENSUS_PROFILE_ID;
  consensusProfileDigest: string;
  protocolVersion: typeof MIDGARD_PROTOCOL_VERSION;
  features: readonly WatcherRuleBundleFeature[];
  limits: typeof MIDGARD_CONSENSUS_LIMITS;
  targetParameters: WatcherRuleBundleTargetParameters;
  transitionPriority: typeof WATCHER_RULE_BUNDLE_TRANSITION_PRIORITY;
  validation: Readonly<{
    phasePriority: readonly MidgardValidationPhaseName[];
    rejectionSelection: typeof WATCHER_RULE_BUNDLE_REJECTION_SELECTION;
  }>;
  programCommitments: Readonly<Record<string, string>>;
}>;

export type LoadedWatcherRuleBundle = Readonly<{
  ruleBundleCommitment: string;
  ruleBundle: WatcherRuleBundle;
}>;

export type WatcherRuleBundleConstructionIdentity = Readonly<{
  manifestId: string;
  network: "Mainnet" | "Preprod" | "Preview" | "Custom";
  blueprintHash: string;
  programCommitments: Readonly<Record<string, string>>;
}>;

export const parseFeatures = (
  value: unknown,
  path: string,
): readonly WatcherRuleBundleFeature[] => {
  const entries = denseArray(value, path);
  const known = new Set<string>(MIDGARD_CONSENSUS_FEATURES);
  const parsed = entries.map((value, index) => {
    const entryPath = `${path}[${index.toString()}]`;
    const entry = exactRecord(value, entryPath, ["featureId", "enabled"]);
    if (typeof entry.featureId !== "string" || !known.has(entry.featureId)) {
      fail("unknown_feature", `${entryPath}.featureId`);
    }
    if (entry.enabled !== true) {
      fail("disabled_feature", `${entryPath}.enabled`);
    }
    return Object.freeze({
      featureId: entry.featureId as WatcherRuleBundleFeature["featureId"],
      enabled: true as const,
    });
  });
  if (
    parsed.length !== MIDGARD_CONSENSUS_FEATURES.length ||
    parsed.some(
      (entry, index) => entry.featureId !== MIDGARD_CONSENSUS_FEATURES[index],
    )
  ) {
    fail("feature_set_mismatch", path);
  }
  return Object.freeze(parsed);
};
