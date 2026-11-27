import { createHash } from "node:crypto";
import { readFile, realpath } from "node:fs/promises";
import { isAbsolute } from "node:path";

import {
  createWorkflowFundingRequirements,
  type WorkflowFundingRequirements,
  type WorkflowFundingRequirementsInput,
} from "@al-ft/midgard-fault-proofs/workflow/funding-requirements";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk/core";

import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.parse-trust-roots.js";
import { assertVerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.verify-watcher-user-event-script-binding.js";
import { watcherCanonicalJson } from "../storage/durable-store.canonical-json.js";

export const WATCHER_WORKFLOW_FUNDING_PROFILE_BUNDLE =
  "midgard-watcher-production-workflow-funding-profile-bundle-v1" as const;
export const WATCHER_WORKFLOW_FUNDING_CONFIGURATION =
  "midgard-watcher-workflow-funding-configuration-v1" as const;
export const WATCHER_WORKFLOW_FUNDING_PROFILE_OVERLAY =
  "midgard-watcher-production-workflow-funding-profile-overlay-v1" as const;

export type WatcherWorkflowFundingProfileOverlay = Readonly<{
  schemaVersion: typeof WATCHER_WORKFLOW_FUNDING_PROFILE_OVERLAY;
  hubOraclePolicyId: string;
  blueprintHash: string;
  fundingProfileBundleDigest: string;
  bundlePath: string;
  profiles: Readonly<
    Partial<
      Record<FraudProofCatalogueCategoryName, WorkflowFundingRequirements>
    >
  >;
}>;

export type WatcherWorkflowFundingProfileBody = Omit<
  WorkflowFundingRequirementsInput,
  "hubOraclePolicyId"
>;

export type WatcherWorkflowFundingProfileBundle = Readonly<{
  fundingProfileBundleBytes: Uint8Array;
  fundingProfileBundleDigest: string;
}>;

const admittedOverlays = new WeakSet<object>();

const isPlainObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  (Object.getPrototypeOf(value) === Object.prototype ||
    Object.getPrototypeOf(value) === null) &&
  Reflect.ownKeys(value).length === Object.keys(value).length;

const exact = (
  value: unknown,
  keys: readonly string[],
  field: string,
): Record<string, unknown> => {
  if (!isPlainObject(value)) {
    throw new Error(`${field} must be a plain object`);
  }
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${field} has an unexpected field set`);
  }
  return value;
};

const canonicalJson = watcherCanonicalJson;

const sha256 = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

const profileBody = (
  value: unknown,
  hubOraclePolicyId: string,
): WorkflowFundingRequirements => {
  const record = exact(
    value,
    [
      "scope",
      "blueprintSha256",
      "protocolParametersDigest",
      "economicsPolicyDigest",
      "fundingPaymentKeyHash",
      "measurementToolVersion",
      "measurementArtifactSha256",
      "actions",
    ],
    "production funding profile body",
  );
  return createWorkflowFundingRequirements({
    ...(record as unknown as WatcherWorkflowFundingProfileBody),
    hubOraclePolicyId,
  });
};

/**
 * Deployment builder seam. Profile bodies omit the hub-oracle policy id. The
 * deployment signature binds the bundle digest alongside the contract
 * blueprint hash, then profiles hydrate with the verified policy id.
 */
export const createWatcherWorkflowFundingProfileBundle = ({
  profiles,
}: {
  readonly profiles: readonly WatcherWorkflowFundingProfileBody[];
}): WatcherWorkflowFundingProfileBundle => {
  const bodies = new Map<
    FraudProofCatalogueCategoryName,
    WatcherWorkflowFundingProfileBody
  >();
  for (const profile of profiles) {
    const hydrated = profileBody(profile, "00".repeat(28));
    if (hydrated.scope.kind !== "fraud_proof_category") {
      throw new Error(
        "production funding profile body requires a catalogue category",
      );
    }
    const { category } = hydrated.scope;
    if (bodies.has(category)) {
      throw new Error(
        `production funding profile repeats category ${category}`,
      );
    }
    const {
      schemaVersion: _schemaVersion,
      hubOraclePolicyId: _hubOraclePolicyId,
      profileDigest: _profileDigest,
      ...body
    } = hydrated;
    // Preserve measured inputs; derived transaction fields are recomputed
    // when the signed bundle is loaded.
    bodies.set(category, Object.freeze({ ...body, actions: profile.actions }));
  }
  const normalizedBodies = Object.freeze(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.flatMap((category) => {
      const body = bodies.get(category);
      return body === undefined ? [] : [body];
    }),
  );
  const fundingProfileBundle = Object.freeze({
    schemaVersion: WATCHER_WORKFLOW_FUNDING_PROFILE_BUNDLE,
    profiles: normalizedBodies,
  });
  const fundingProfileBundleDigest = sha256(
    Buffer.from(canonicalJson(fundingProfileBundle), "utf8"),
  );
  const artifacts = Object.freeze({
    schemaVersion: WATCHER_WORKFLOW_FUNDING_CONFIGURATION,
    fundingProfileBundle,
    fundingProfileBundleDigest,
  });
  const artifactsBytes = Buffer.from(canonicalJson(artifacts), "utf8");
  return Object.freeze({
    fundingProfileBundleBytes: artifactsBytes,
    fundingProfileBundleDigest,
  });
};

export const assertWatcherWorkflowFundingProfileOverlay = (
  overlay: WatcherWorkflowFundingProfileOverlay,
): void => {
  if (!admittedOverlays.has(overlay)) {
    throw new Error(
      "production workflow funding profile overlay was not admitted from signed deployment configuration",
    );
  }
};

export const workflowFundingProfileFromOverlay = ({
  overlay,
  category,
}: {
  readonly overlay: WatcherWorkflowFundingProfileOverlay;
  readonly category: FraudProofCatalogueCategoryName;
}): WorkflowFundingRequirements => {
  assertWatcherWorkflowFundingProfileOverlay(overlay);
  const profile = overlay.profiles[category];
  if (profile === undefined) {
    throw new Error(`${category} has no signed measured funding profile`);
  }
  if (
    profile.scope.kind !== "fraud_proof_category" ||
    profile.scope.category !== category
  ) {
    throw new Error("production funding profile changed overlay identity");
  }
  return profile;
};

/**
 * Admits the signed funding-profile overlay of the deployment: refused when
 * an executing watcher has none or it names another deployment. Nothing at
 * runtime reads its measurements, so a category without one installs.
 */
export const admitWatcherWorkflowFundingProfileOverlay = ({
  overlay,
  deploymentIdentity,
  allowExecution,
}: {
  readonly overlay: WatcherWorkflowFundingProfileOverlay | undefined;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly allowExecution: boolean;
}): void => {
  if (allowExecution && overlay === undefined) {
    throw new Error(
      "watcher application requires its signed funding-profile overlay",
    );
  }
  if (overlay !== undefined) {
    assertWatcherWorkflowFundingProfileOverlay(overlay);
    if (
      overlay.hubOraclePolicyId !== deploymentIdentity.hubOraclePolicyId ||
      overlay.blueprintHash !== deploymentIdentity.blueprintHash
    ) {
      throw new Error(
        "watcher funding-profile overlay changed deployment identity",
      );
    }
  }
};

/**
 * Loads the funding bundle authenticated by the deployment signature and
 * local policy. Its canonical inner digest is separate from compiled release
 * evidence. A runtime path only selects bytes to verify.
 */
export const loadWatcherWorkflowFundingProfileOverlay = async ({
  bundlePath,
  deploymentIdentity,
}: {
  readonly bundlePath: string;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
}): Promise<WatcherWorkflowFundingProfileOverlay> => {
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  if (!isAbsolute(bundlePath) || bundlePath.trim() !== bundlePath) {
    throw new Error("production funding profile bundle path must be absolute");
  }
  const canonicalPath = await realpath(bundlePath);
  if (canonicalPath !== bundlePath) {
    throw new Error("production funding profile bundle path is not canonical");
  }
  const bytes = await readFile(canonicalPath);
  let value: unknown;
  try {
    value = JSON.parse(bytes.toString("utf8")) as unknown;
  } catch {
    throw new Error("production funding profile bundle is not JSON");
  }
  const canonicalBytes = Buffer.from(canonicalJson(value), "utf8");
  if (!bytes.equals(canonicalBytes)) {
    throw new Error("production funding profile bundle is not canonical JSON");
  }
  const artifacts = exact(
    value,
    ["schemaVersion", "fundingProfileBundle", "fundingProfileBundleDigest"],
    "workflow funding configuration",
  );
  if (artifacts.schemaVersion !== WATCHER_WORKFLOW_FUNDING_CONFIGURATION) {
    throw new Error("workflow funding configuration version is unsupported");
  }
  const fundingProfileBundle = exact(
    artifacts.fundingProfileBundle,
    ["schemaVersion", "profiles"],
    "production funding profile bundle",
  );
  if (
    fundingProfileBundle.schemaVersion !==
    WATCHER_WORKFLOW_FUNDING_PROFILE_BUNDLE
  ) {
    throw new Error("production funding profile bundle version is unsupported");
  }
  const fundingProfileBundleDigest = sha256(
    Buffer.from(canonicalJson(fundingProfileBundle), "utf8"),
  );
  if (artifacts.fundingProfileBundleDigest !== fundingProfileBundleDigest) {
    throw new Error(
      "production funding profile bundle digest differs from configuration",
    );
  }
  if (
    fundingProfileBundleDigest !== deploymentIdentity.fundingProfileBundleDigest
  ) {
    throw new Error(
      "production funding profile bundle does not match signed deployment identity",
    );
  }
  if (!Array.isArray(fundingProfileBundle.profiles)) {
    throw new Error(
      "production funding profile bundle profiles must be an array",
    );
  }
  const rawProfiles: readonly unknown[] = fundingProfileBundle.profiles;
  let previousCategoryIndex = -1;
  const profiles = Object.freeze(
    Object.fromEntries(
      rawProfiles.map((rawProfile, index) => {
        const profile = profileBody(
          rawProfile,
          deploymentIdentity.hubOraclePolicyId,
        );
        if (profile.scope.kind !== "fraud_proof_category") {
          throw new Error(
            `production funding profile bundle entry ${index.toString()} requires a catalogue category`,
          );
        }
        const { category } = profile.scope;
        const categoryIndex =
          FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(category);
        if (categoryIndex <= previousCategoryIndex) {
          throw new Error(
            "production funding profile bundle categories repeat or are not canonical",
          );
        }
        previousCategoryIndex = categoryIndex;
        return [category, profile] as const;
      }),
    ),
  ) as Readonly<
    Partial<
      Record<FraudProofCatalogueCategoryName, WorkflowFundingRequirements>
    >
  >;
  const overlay = Object.freeze({
    schemaVersion: WATCHER_WORKFLOW_FUNDING_PROFILE_OVERLAY,
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
    blueprintHash: deploymentIdentity.blueprintHash,
    fundingProfileBundleDigest,
    bundlePath: canonicalPath,
    profiles,
  });
  admittedOverlays.add(overlay);
  return overlay;
};
