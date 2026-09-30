import {
  MIDGARD_CONSENSUS_FEATURES,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_CONSENSUS_PROFILE_ID,
  MIDGARD_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/consensus-profile";
import {
  computeDeploymentManifestJsonDigest,
  type DeploymentManifestJsonValue,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  verifyWatcherDeploymentIdentity,
  type WatcherDeploymentIdentityPolicy,
  type WatcherDeploymentTrustRoot,
} from "../runtime/deployment-identity.js";
import {
  canonicalJsonValue,
  denseArray,
  equalStringMaps,
  exactHex32,
  exactNetwork,
  exactRecord,
  fail,
  type LoadedWatcherRuleBundle,
  parseFeatures,
  parseProgramCommitments,
  plainRecord,
  WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
  WATCHER_RULE_BUNDLE_SCHEMA_VERSION,
  WATCHER_RULE_BUNDLE_TRANSITION_PRIORITY,
  WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
  WATCHER_RULE_BUNDLE_VERSION,
  type WatcherRuleBundle,
  type WatcherRuleBundleConstructionIdentity,
  type WatcherRuleBundleTargetParameters,
} from "./rule-bundle.canonical-json-value.js";

const parseLimits = (
  value: unknown,
  path: string,
): typeof MIDGARD_CONSENSUS_LIMITS => {
  const expectedKeys = Object.keys(MIDGARD_CONSENSUS_LIMITS);
  const limits = exactRecord(value, path, expectedKeys);
  for (const key of expectedKeys) {
    const expected =
      MIDGARD_CONSENSUS_LIMITS[key as keyof typeof MIDGARD_CONSENSUS_LIMITS];
    if (limits[key] !== expected) {
      fail("consensus_profile_mismatch", `${path}.${key}`);
    }
  }
  return MIDGARD_CONSENSUS_LIMITS;
};

const parseTargetParameters = (
  value: unknown,
  path: string,
): WatcherRuleBundleTargetParameters => {
  const target = exactRecord(value, path, ["snapshot", "digest"]);
  const snapshotInput = plainRecord(target.snapshot, `${path}.snapshot`);
  if (Object.keys(snapshotInput).length === 0) {
    fail("missing_field", `${path}.snapshot`);
  }
  const snapshot = canonicalJsonValue(
    snapshotInput,
    `${path}.snapshot`,
  ) as Readonly<Record<string, DeploymentManifestJsonValue>>;
  const digest = exactHex32(target.digest, `${path}.digest`);
  if (digest !== computeDeploymentManifestJsonDigest(snapshot)) {
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

export const parseWatcherRuleBundle = (value: unknown): WatcherRuleBundle => {
  const bundle = exactRecord(value, "$", [
    "schemaVersion",
    "ruleBundleVersion",
    "deploymentManifestId",
    "network",
    "blueprintHash",
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
    bundle.schemaVersion !== WATCHER_RULE_BUNDLE_SCHEMA_VERSION ||
    bundle.ruleBundleVersion !== WATCHER_RULE_BUNDLE_VERSION
  ) {
    fail(
      "unsupported_version",
      bundle.schemaVersion !== WATCHER_RULE_BUNDLE_SCHEMA_VERSION
        ? "$.schemaVersion"
        : "$.ruleBundleVersion",
    );
  }
  if (
    bundle.consensusProfileId !== MIDGARD_CONSENSUS_PROFILE_ID ||
    bundle.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_DIGEST ||
    bundle.protocolVersion !== MIDGARD_PROTOCOL_VERSION
  ) {
    fail(
      "consensus_profile_mismatch",
      bundle.consensusProfileId !== MIDGARD_CONSENSUS_PROFILE_ID
        ? "$.consensusProfileId"
        : bundle.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_DIGEST
          ? "$.consensusProfileDigest"
          : "$.protocolVersion",
    );
  }
  const validation = exactRecord(bundle.validation, "$.validation", [
    "phasePriority",
    "rejectionSelection",
  ]);
  if (
    validation.rejectionSelection !== WATCHER_RULE_BUNDLE_REJECTION_SELECTION
  ) {
    fail("validation_priority_mismatch", "$.validation.rejectionSelection");
  }
  return Object.freeze({
    schemaVersion: WATCHER_RULE_BUNDLE_SCHEMA_VERSION,
    ruleBundleVersion: WATCHER_RULE_BUNDLE_VERSION,
    deploymentManifestId: exactHex32(
      bundle.deploymentManifestId,
      "$.deploymentManifestId",
    ),
    network: exactNetwork(bundle.network, "$.network"),
    blueprintHash: exactHex32(bundle.blueprintHash, "$.blueprintHash"),
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    protocolVersion: MIDGARD_PROTOCOL_VERSION,
    features: parseFeatures(bundle.features, "$.features"),
    limits: parseLimits(bundle.limits, "$.limits"),
    targetParameters: parseTargetParameters(
      bundle.targetParameters,
      "$.targetParameters",
    ),
    transitionPriority: parseExactPriority(
      bundle.transitionPriority,
      "$.transitionPriority",
      WATCHER_RULE_BUNDLE_TRANSITION_PRIORITY,
      "transition_priority_mismatch",
    ) as typeof WATCHER_RULE_BUNDLE_TRANSITION_PRIORITY,
    validation: Object.freeze({
      phasePriority: parseExactPriority(
        validation.phasePriority,
        "$.validation.phasePriority",
        WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
        "validation_priority_mismatch",
      ),
      rejectionSelection: WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
    }),
    programCommitments: parseProgramCommitments(
      bundle.programCommitments,
      "$.programCommitments",
    ),
  });
};

const parseConstructionIdentity = (
  value: WatcherRuleBundleConstructionIdentity,
): WatcherRuleBundleConstructionIdentity => {
  const identity = exactRecord(value, "$.constructionIdentity", [
    "manifestId",
    "network",
    "blueprintHash",
    "programCommitments",
  ]);
  const manifestId = exactHex32(
    identity.manifestId,
    "$.constructionIdentity.manifestId",
  );
  return Object.freeze({
    manifestId,
    network: exactNetwork(identity.network, "$.constructionIdentity.network"),
    blueprintHash: exactHex32(
      identity.blueprintHash,
      "$.constructionIdentity.blueprintHash",
    ),
    programCommitments: parseProgramCommitments(
      identity.programCommitments,
      "$.constructionIdentity.programCommitments",
    ),
  });
};

export const encodeWatcherRuleBundle = (value: unknown): Buffer =>
  Buffer.from(JSON.stringify(parseWatcherRuleBundle(value)), "utf8");

export const computeWatcherRuleBundleCommitment = (value: unknown): string =>
  computeDeploymentManifestJsonDigest(parseWatcherRuleBundle(value));

/**
 * Constructs release material before the W02 envelope is signed.
 *
 * This helper does not authenticate or authorize a deployment. Security
 * consumers must use {@link loadWatcherRuleBundle}, which verifies the raw
 * signed W02 authority on every load.
 */
export const makeWatcherCanonicalRuleBundle = (input: {
  readonly constructionIdentity: WatcherRuleBundleConstructionIdentity;
  readonly targetParameterSnapshot: unknown;
}): WatcherRuleBundle => {
  const identity = parseConstructionIdentity(input.constructionIdentity);
  const snapshot = canonicalJsonValue(
    plainRecord(input.targetParameterSnapshot, "$.targetParameterSnapshot"),
    "$.targetParameterSnapshot",
  );
  return parseWatcherRuleBundle({
    schemaVersion: WATCHER_RULE_BUNDLE_SCHEMA_VERSION,
    ruleBundleVersion: WATCHER_RULE_BUNDLE_VERSION,
    deploymentManifestId: identity.manifestId,
    network: identity.network,
    blueprintHash: identity.blueprintHash,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    protocolVersion: MIDGARD_PROTOCOL_VERSION,
    features: MIDGARD_CONSENSUS_FEATURES.map((featureId) => ({
      featureId,
      enabled: true,
    })),
    limits: MIDGARD_CONSENSUS_LIMITS,
    targetParameters: {
      snapshot,
      digest: computeDeploymentManifestJsonDigest(snapshot),
    },
    transitionPriority: WATCHER_RULE_BUNDLE_TRANSITION_PRIORITY,
    validation: {
      phasePriority: WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
      rejectionSelection: WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
    },
    programCommitments: identity.programCommitments,
  });
};

export const loadWatcherRuleBundle = (input: {
  readonly signedIdentity: unknown;
  readonly policy: WatcherDeploymentIdentityPolicy;
  readonly trustRoots: readonly WatcherDeploymentTrustRoot[];
  readonly durableMarker: unknown;
  readonly ruleBundle: unknown;
}): LoadedWatcherRuleBundle => {
  const identity = verifyWatcherDeploymentIdentity({
    signedIdentity: input.signedIdentity,
    policy: input.policy,
    trustRoots: input.trustRoots,
    durableMarker: input.durableMarker,
  });
  const ruleBundle = parseWatcherRuleBundle(input.ruleBundle);
  if (
    ruleBundle.deploymentManifestId !== identity.manifestId ||
    ruleBundle.network !== identity.network ||
    ruleBundle.blueprintHash !== identity.blueprintHash
  ) {
    fail("deployment_identity_mismatch", "$.ruleBundle");
  }
  if (
    !equalStringMaps(ruleBundle.programCommitments, identity.programCommitments)
  ) {
    fail("program_commitment_mismatch", "$.ruleBundle.programCommitments");
  }
  const ruleBundleCommitment = computeWatcherRuleBundleCommitment(ruleBundle);
  if (ruleBundleCommitment !== identity.ruleBundleCommitment) {
    fail("rule_bundle_commitment_mismatch", "$.ruleBundle");
  }
  return Object.freeze({ ruleBundleCommitment, ruleBundle });
};
