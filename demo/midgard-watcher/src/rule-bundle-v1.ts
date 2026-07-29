import {
  MIDGARD_CONSENSUS_FEATURES_V1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_CONSENSUS_PROFILE_V1_ID,
  MIDGARD_PROTOCOL_V1_VERSION,
} from "../../midgard-core/src/consensus-profile-v1.js";
import {
  computeDeploymentManifestV1JsonDigest,
  type DeploymentManifestV1JsonValue,
} from "../../midgard-core/src/deployment-manifest-identity-v1.js";
import {
  MidgardValidationPhase,
  type MidgardValidationPhaseName,
} from "../../midgard-core/src/validation-trace.js";
import {
  verifyWatcherDeploymentIdentityV1,
  type WatcherDeploymentIdentityPolicyV1,
  type WatcherDeploymentTrustRootV1,
} from "./deployment-identity.js";

export const WATCHER_RULE_BUNDLE_V1_SCHEMA_VERSION =
  "midgard-watcher-rule-bundle-v1" as const;
export const WATCHER_RULE_BUNDLE_V1_VERSION = 1 as const;
export const WATCHER_RULE_BUNDLE_V1_REJECTION_SELECTION =
  "first_rejection_by_phase_then_program_counter_v1" as const;

const HEX_32 = /^[0-9a-f]{64}$/u;
const COMMITMENT_NAME = /^[a-z][a-z0-9]*(?:[-_.][a-z0-9]+)*$/u;

export const WATCHER_RULE_BUNDLE_V1_TRANSITION_PRIORITY = Object.freeze([
  "withdrawal",
  "forced_transaction",
  "l2_transaction",
  "deposit",
] as const);

export const WATCHER_RULE_BUNDLE_V1_VALIDATION_PHASE_PRIORITY = Object.freeze(
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

export type WatcherRuleBundleV1ErrorCode =
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

export class WatcherRuleBundleV1Error extends Error {
  readonly code: WatcherRuleBundleV1ErrorCode;
  readonly path: string;

  constructor(code: WatcherRuleBundleV1ErrorCode, path: string) {
    super(`Watcher canonical V1 rule bundle rejected: ${code} at ${path}`);
    this.name = "WatcherRuleBundleV1Error";
    this.code = code;
    this.path = path;
  }
}

const fail = (code: WatcherRuleBundleV1ErrorCode, path: string): never => {
  throw new WatcherRuleBundleV1Error(code, path);
};

type JsonRecord = Record<string, unknown>;

const plainRecord = (value: unknown, path: string): JsonRecord => {
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

const exactRecord = (
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

const denseArray = (value: unknown, path: string): readonly unknown[] => {
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

const exactHex32 = (value: unknown, path: string): string => {
  if (typeof value !== "string" || !HEX_32.test(value)) {
    return fail("invalid_field", path);
  }
  return value;
};

const exactNetwork = (
  value: unknown,
  path: string,
): "Mainnet" | "Preprod" | "Preview" => {
  if (value !== "Mainnet" && value !== "Preprod" && value !== "Preview") {
    return fail("invalid_field", path);
  }
  return value;
};

const canonicalJsonValue = (
  value: unknown,
  path: string,
): DeploymentManifestV1JsonValue => {
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

const equalStringMaps = (
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

const parseProgramCommitments = (
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

export type WatcherRuleBundleV1Feature = Readonly<{
  featureId: (typeof MIDGARD_CONSENSUS_FEATURES_V1)[number];
  enabled: true;
}>;

export type WatcherRuleBundleV1TargetParameters = Readonly<{
  snapshot: Readonly<Record<string, DeploymentManifestV1JsonValue>>;
  digest: string;
}>;

export type WatcherRuleBundleV1 = Readonly<{
  schemaVersion: typeof WATCHER_RULE_BUNDLE_V1_SCHEMA_VERSION;
  ruleBundleVersion: typeof WATCHER_RULE_BUNDLE_V1_VERSION;
  deploymentManifestId: string;
  network: "Mainnet" | "Preprod" | "Preview";
  releaseEvidenceDigest: string;
  consensusProfileId: typeof MIDGARD_CONSENSUS_PROFILE_V1_ID;
  consensusProfileDigest: string;
  protocolVersion: typeof MIDGARD_PROTOCOL_V1_VERSION;
  features: readonly WatcherRuleBundleV1Feature[];
  limits: typeof MIDGARD_CONSENSUS_LIMITS_V1;
  targetParameters: WatcherRuleBundleV1TargetParameters;
  transitionPriority: typeof WATCHER_RULE_BUNDLE_V1_TRANSITION_PRIORITY;
  validation: Readonly<{
    phasePriority: readonly MidgardValidationPhaseName[];
    rejectionSelection: typeof WATCHER_RULE_BUNDLE_V1_REJECTION_SELECTION;
  }>;
  programCommitments: Readonly<Record<string, string>>;
}>;

export type LoadedWatcherRuleBundleV1 = Readonly<{
  ruleBundleCommitment: string;
  ruleBundle: WatcherRuleBundleV1;
}>;

export type WatcherRuleBundleV1ConstructionIdentity = Readonly<{
  manifestId: string;
  network: "Mainnet" | "Preprod" | "Preview";
  releaseEvidenceDigest: string;
  programCommitments: Readonly<Record<string, string>>;
}>;

const parseFeatures = (
  value: unknown,
  path: string,
): readonly WatcherRuleBundleV1Feature[] => {
  const entries = denseArray(value, path);
  const known = new Set<string>(MIDGARD_CONSENSUS_FEATURES_V1);
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
      featureId: entry.featureId as WatcherRuleBundleV1Feature["featureId"],
      enabled: true as const,
    });
  });
  if (
    parsed.length !== MIDGARD_CONSENSUS_FEATURES_V1.length ||
    parsed.some(
      (entry, index) =>
        entry.featureId !== MIDGARD_CONSENSUS_FEATURES_V1[index],
    )
  ) {
    fail("feature_set_mismatch", path);
  }
  return Object.freeze(parsed);
};

const parseLimits = (
  value: unknown,
  path: string,
): typeof MIDGARD_CONSENSUS_LIMITS_V1 => {
  const expectedKeys = Object.keys(MIDGARD_CONSENSUS_LIMITS_V1);
  const limits = exactRecord(value, path, expectedKeys);
  for (const key of expectedKeys) {
    const expected =
      MIDGARD_CONSENSUS_LIMITS_V1[
        key as keyof typeof MIDGARD_CONSENSUS_LIMITS_V1
      ];
    if (limits[key] !== expected) {
      fail("consensus_profile_mismatch", `${path}.${key}`);
    }
  }
  return MIDGARD_CONSENSUS_LIMITS_V1;
};

const parseTargetParameters = (
  value: unknown,
  path: string,
): WatcherRuleBundleV1TargetParameters => {
  const target = exactRecord(value, path, ["snapshot", "digest"]);
  const snapshotInput = plainRecord(target.snapshot, `${path}.snapshot`);
  if (Object.keys(snapshotInput).length === 0) {
    fail("missing_field", `${path}.snapshot`);
  }
  const snapshot = canonicalJsonValue(
    snapshotInput,
    `${path}.snapshot`,
  ) as Readonly<Record<string, DeploymentManifestV1JsonValue>>;
  const digest = exactHex32(target.digest, `${path}.digest`);
  if (digest !== computeDeploymentManifestV1JsonDigest(snapshot)) {
    fail("target_parameters_mismatch", `${path}.digest`);
  }
  return Object.freeze({ snapshot, digest });
};

const parseExactPriority = <T extends string>(
  value: unknown,
  path: string,
  expected: readonly T[],
  code: "transition_priority_mismatch" | "validation_priority_mismatch",
): readonly T[] => {
  const entries = denseArray(value, path);
  if (
    entries.length !== expected.length ||
    entries.some((entry, index) => entry !== expected[index])
  ) {
    fail(code, path);
  }
  return expected;
};

export const parseWatcherRuleBundleV1 = (
  value: unknown,
): WatcherRuleBundleV1 => {
  const bundle = exactRecord(value, "$", [
    "schemaVersion",
    "ruleBundleVersion",
    "deploymentManifestId",
    "network",
    "releaseEvidenceDigest",
    "consensusProfileId",
    "consensusProfileDigest",
    "protocolVersion",
    "features",
    "limits",
    "targetParameters",
    "transitionPriority",
    "validation",
    "programCommitments",
  ]);
  if (
    bundle.schemaVersion !== WATCHER_RULE_BUNDLE_V1_SCHEMA_VERSION ||
    bundle.ruleBundleVersion !== WATCHER_RULE_BUNDLE_V1_VERSION
  ) {
    fail(
      "unsupported_version",
      bundle.schemaVersion !== WATCHER_RULE_BUNDLE_V1_SCHEMA_VERSION
        ? "$.schemaVersion"
        : "$.ruleBundleVersion",
    );
  }
  if (
    bundle.consensusProfileId !== MIDGARD_CONSENSUS_PROFILE_V1_ID ||
    bundle.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_V1_DIGEST ||
    bundle.protocolVersion !== MIDGARD_PROTOCOL_V1_VERSION
  ) {
    fail(
      "consensus_profile_mismatch",
      bundle.consensusProfileId !== MIDGARD_CONSENSUS_PROFILE_V1_ID
        ? "$.consensusProfileId"
        : bundle.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_V1_DIGEST
          ? "$.consensusProfileDigest"
          : "$.protocolVersion",
    );
  }
  const validation = exactRecord(bundle.validation, "$.validation", [
    "phasePriority",
    "rejectionSelection",
  ]);
  if (
    validation.rejectionSelection !== WATCHER_RULE_BUNDLE_V1_REJECTION_SELECTION
  ) {
    fail("validation_priority_mismatch", "$.validation.rejectionSelection");
  }
  return Object.freeze({
    schemaVersion: WATCHER_RULE_BUNDLE_V1_SCHEMA_VERSION,
    ruleBundleVersion: WATCHER_RULE_BUNDLE_V1_VERSION,
    deploymentManifestId: exactHex32(
      bundle.deploymentManifestId,
      "$.deploymentManifestId",
    ),
    network: exactNetwork(bundle.network, "$.network"),
    releaseEvidenceDigest: exactHex32(
      bundle.releaseEvidenceDigest,
      "$.releaseEvidenceDigest",
    ),
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_V1_ID,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
    protocolVersion: MIDGARD_PROTOCOL_V1_VERSION,
    features: parseFeatures(bundle.features, "$.features"),
    limits: parseLimits(bundle.limits, "$.limits"),
    targetParameters: parseTargetParameters(
      bundle.targetParameters,
      "$.targetParameters",
    ),
    transitionPriority: parseExactPriority(
      bundle.transitionPriority,
      "$.transitionPriority",
      WATCHER_RULE_BUNDLE_V1_TRANSITION_PRIORITY,
      "transition_priority_mismatch",
    ) as typeof WATCHER_RULE_BUNDLE_V1_TRANSITION_PRIORITY,
    validation: Object.freeze({
      phasePriority: parseExactPriority(
        validation.phasePriority,
        "$.validation.phasePriority",
        WATCHER_RULE_BUNDLE_V1_VALIDATION_PHASE_PRIORITY,
        "validation_priority_mismatch",
      ),
      rejectionSelection: WATCHER_RULE_BUNDLE_V1_REJECTION_SELECTION,
    }),
    programCommitments: parseProgramCommitments(
      bundle.programCommitments,
      "$.programCommitments",
    ),
  });
};

const parseConstructionIdentity = (
  value: WatcherRuleBundleV1ConstructionIdentity,
): WatcherRuleBundleV1ConstructionIdentity => {
  const identity = exactRecord(value, "$.constructionIdentity", [
    "manifestId",
    "network",
    "releaseEvidenceDigest",
    "programCommitments",
  ]);
  const manifestId = exactHex32(
    identity.manifestId,
    "$.constructionIdentity.manifestId",
  );
  return Object.freeze({
    manifestId,
    network: exactNetwork(identity.network, "$.constructionIdentity.network"),
    releaseEvidenceDigest: exactHex32(
      identity.releaseEvidenceDigest,
      "$.constructionIdentity.releaseEvidenceDigest",
    ),
    programCommitments: parseProgramCommitments(
      identity.programCommitments,
      "$.constructionIdentity.programCommitments",
    ),
  });
};

export const encodeWatcherRuleBundleV1 = (value: unknown): Buffer =>
  Buffer.from(JSON.stringify(parseWatcherRuleBundleV1(value)), "utf8");

export const computeWatcherRuleBundleV1Commitment = (value: unknown): string =>
  computeDeploymentManifestV1JsonDigest(parseWatcherRuleBundleV1(value));

/**
 * Constructs release material before the W02 envelope is signed.
 *
 * This helper does not authenticate or authorize a deployment. Security
 * consumers must use {@link loadWatcherRuleBundleV1}, which verifies the raw
 * signed W02 authority on every load.
 */
export const makeWatcherCanonicalRuleBundleV1 = (input: {
  readonly constructionIdentity: WatcherRuleBundleV1ConstructionIdentity;
  readonly targetParameterSnapshot: unknown;
}): WatcherRuleBundleV1 => {
  const identity = parseConstructionIdentity(input.constructionIdentity);
  const snapshot = canonicalJsonValue(
    plainRecord(input.targetParameterSnapshot, "$.targetParameterSnapshot"),
    "$.targetParameterSnapshot",
  );
  return parseWatcherRuleBundleV1({
    schemaVersion: WATCHER_RULE_BUNDLE_V1_SCHEMA_VERSION,
    ruleBundleVersion: WATCHER_RULE_BUNDLE_V1_VERSION,
    deploymentManifestId: identity.manifestId,
    network: identity.network,
    releaseEvidenceDigest: identity.releaseEvidenceDigest,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_V1_ID,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
    protocolVersion: MIDGARD_PROTOCOL_V1_VERSION,
    features: MIDGARD_CONSENSUS_FEATURES_V1.map((featureId) => ({
      featureId,
      enabled: true,
    })),
    limits: MIDGARD_CONSENSUS_LIMITS_V1,
    targetParameters: {
      snapshot,
      digest: computeDeploymentManifestV1JsonDigest(snapshot),
    },
    transitionPriority: WATCHER_RULE_BUNDLE_V1_TRANSITION_PRIORITY,
    validation: {
      phasePriority: WATCHER_RULE_BUNDLE_V1_VALIDATION_PHASE_PRIORITY,
      rejectionSelection: WATCHER_RULE_BUNDLE_V1_REJECTION_SELECTION,
    },
    programCommitments: identity.programCommitments,
  });
};

export const loadWatcherRuleBundleV1 = (input: {
  readonly signedIdentity: unknown;
  readonly policy: WatcherDeploymentIdentityPolicyV1;
  readonly trustRoots: readonly WatcherDeploymentTrustRootV1[];
  readonly durableMarker: unknown;
  readonly ruleBundle: unknown;
}): LoadedWatcherRuleBundleV1 => {
  const identity = verifyWatcherDeploymentIdentityV1({
    signedIdentity: input.signedIdentity,
    policy: input.policy,
    trustRoots: input.trustRoots,
    durableMarker: input.durableMarker,
  });
  const ruleBundle = parseWatcherRuleBundleV1(input.ruleBundle);
  if (
    ruleBundle.deploymentManifestId !== identity.manifestId ||
    ruleBundle.network !== identity.network ||
    ruleBundle.releaseEvidenceDigest !== identity.releaseEvidenceDigest
  ) {
    fail("deployment_identity_mismatch", "$.ruleBundle");
  }
  if (
    !equalStringMaps(ruleBundle.programCommitments, identity.programCommitments)
  ) {
    fail("program_commitment_mismatch", "$.ruleBundle.programCommitments");
  }
  const ruleBundleCommitment = computeWatcherRuleBundleV1Commitment(ruleBundle);
  if (ruleBundleCommitment !== identity.ruleBundleCommitment) {
    fail("rule_bundle_commitment_mismatch", "$.ruleBundle");
  }
  return Object.freeze({ ruleBundleCommitment, ruleBundle });
};
