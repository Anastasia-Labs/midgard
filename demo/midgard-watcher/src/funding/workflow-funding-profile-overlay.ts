import { createHash } from "node:crypto";
import { readFile, realpath } from "node:fs/promises";
import { isAbsolute } from "node:path";

import {
  createWorkflowFundingRequirements,
  type WorkflowFundingRequirements,
  type WorkflowFundingRequirementsInput,
} from "@al-ft/midgard-fault-proofs";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";

export const WATCHER_WORKFLOW_FUNDING_PROFILE_BUNDLE =
  "midgard-watcher-production-workflow-funding-profile-bundle-v1" as const;
export const WATCHER_WORKFLOW_FUNDING_RELEASE_EVIDENCE =
  "midgard-watcher-production-workflow-funding-release-evidence-v1" as const;
export const WATCHER_WORKFLOW_FUNDING_PROFILE_OVERLAY =
  "midgard-watcher-production-workflow-funding-profile-overlay-v1" as const;

export type WatcherWorkflowFundingProfileOverlay = Readonly<{
  schemaVersion: typeof WATCHER_WORKFLOW_FUNDING_PROFILE_OVERLAY;
  deploymentFingerprint: string;
  releaseEvidenceDigest: string;
  bundlePath: string;
  profiles: Readonly<
    Record<FraudProofCatalogueCategoryName, WorkflowFundingRequirements>
  >;
}>;

export type WatcherWorkflowFundingProfileBody = Omit<
  WorkflowFundingRequirementsInput,
  "deploymentFingerprint"
>;

export type WatcherWorkflowFundingReleaseEvidence = Readonly<{
  releaseEvidenceBytes: Uint8Array;
  releaseEvidenceDigest: string;
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

const canonicalJson = (value: unknown): string => {
  if (value === null) return "null";
  if (typeof value === "boolean" || typeof value === "string") {
    return JSON.stringify(value);
  }
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) {
      throw new Error("funding profile bundle contains a non-safe number");
    }
    return value.toString();
  }
  if (Array.isArray(value)) {
    return `[${value.map(canonicalJson).join(",")}]`;
  }
  if (!isPlainObject(value)) {
    throw new Error("funding profile bundle contains a non-JSON value");
  }
  return `{${Object.keys(value)
    .sort()
    .map((key) => `${JSON.stringify(key)}:${canonicalJson(value[key])}`)
    .join(",")}}`;
};

const sha256 = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

const profileBody = (
  value: unknown,
  deploymentFingerprint: string,
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
    deploymentFingerprint,
  });
};

/**
 * Deployment builder seam. Profile bodies deliberately omit the future
 * manifest ID, so construction terminates in finite order: funding bundle,
 * release evidence, manifest ID, then hydrated admitted profiles.
 */
export const createWatcherWorkflowFundingReleaseEvidence = ({
  profiles,
}: {
  readonly profiles: readonly WatcherWorkflowFundingProfileBody[];
}): WatcherWorkflowFundingReleaseEvidence => {
  if (profiles.length !== FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length) {
    throw new Error(
      "production funding profile bodies do not cover the exact catalogue",
    );
  }
  const normalizedBodies = Object.freeze(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category, index) => {
      const hydrated = profileBody(profiles[index], "00".repeat(32));
      if (
        hydrated.scope.kind !== "fraud_proof_category" ||
        hydrated.scope.category !== category
      ) {
        throw new Error(
          `production funding profile body ${index.toString()} is not canonical ${category}`,
        );
      }
      const {
        schemaVersion: _schemaVersion,
        deploymentFingerprint: _deploymentFingerprint,
        profileDigest: _profileDigest,
        ...body
      } = hydrated;
      return Object.freeze(body);
    }),
  );
  const fundingProfileBundle = Object.freeze({
    schemaVersion: WATCHER_WORKFLOW_FUNDING_PROFILE_BUNDLE,
    profiles: normalizedBodies,
  });
  const fundingProfileBundleDigest = sha256(
    Buffer.from(canonicalJson(fundingProfileBundle), "utf8"),
  );
  const releaseEvidence = Object.freeze({
    schemaVersion: WATCHER_WORKFLOW_FUNDING_RELEASE_EVIDENCE,
    fundingProfileBundle,
    fundingProfileBundleDigest,
  });
  const releaseEvidenceBytes = Buffer.from(
    canonicalJson(releaseEvidence),
    "utf8",
  );
  return Object.freeze({
    releaseEvidenceBytes,
    releaseEvidenceDigest: sha256(releaseEvidenceBytes),
    fundingProfileBundleDigest,
  });
};

export const assertWatcherWorkflowFundingProfileOverlay = (
  overlay: WatcherWorkflowFundingProfileOverlay,
): void => {
  if (!admittedOverlays.has(overlay)) {
    throw new Error(
      "production workflow funding profile overlay was not admitted from signed release evidence",
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
  if (
    profile.scope.kind !== "fraud_proof_category" ||
    profile.scope.category !== category ||
    profile.deploymentFingerprint !== overlay.deploymentFingerprint
  ) {
    throw new Error("production funding profile changed overlay identity");
  }
  return profile;
};

/**
 * Loads the sole release-bound funding profile bundle. The file is authority
 * only because its exact canonical bytes hash to the release-evidence digest
 * carried by the already verified signed deployment identity. A runtime path
 * selects bytes to verify; it never supplies a profile or profile digest.
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
  const releaseEvidenceDigest = sha256(bytes);
  if (releaseEvidenceDigest !== deploymentIdentity.releaseEvidenceDigest) {
    throw new Error(
      "production funding release evidence does not match signed deployment identity",
    );
  }
  const releaseEvidence = exact(
    value,
    ["schemaVersion", "fundingProfileBundle", "fundingProfileBundleDigest"],
    "production funding release evidence",
  );
  if (
    releaseEvidence.schemaVersion !== WATCHER_WORKFLOW_FUNDING_RELEASE_EVIDENCE
  ) {
    throw new Error(
      "production funding release evidence version is unsupported",
    );
  }
  const fundingProfileBundle = exact(
    releaseEvidence.fundingProfileBundle,
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
  if (
    releaseEvidence.fundingProfileBundleDigest !== fundingProfileBundleDigest
  ) {
    throw new Error(
      "production funding profile bundle digest differs from release evidence",
    );
  }
  if (!Array.isArray(fundingProfileBundle.profiles)) {
    throw new Error(
      "production funding profile bundle profiles must be an array",
    );
  }
  const rawProfiles: readonly unknown[] = fundingProfileBundle.profiles;
  if (rawProfiles.length !== FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length) {
    throw new Error(
      "production funding profile bundle does not cover the exact catalogue",
    );
  }
  const profiles = Object.freeze(
    Object.fromEntries(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category, index) => {
        const profile = profileBody(
          rawProfiles[index],
          deploymentIdentity.manifestId,
        );
        if (
          profile.scope.kind !== "fraud_proof_category" ||
          profile.scope.category !== category ||
          profile.deploymentFingerprint !== deploymentIdentity.manifestId
        ) {
          throw new Error(
            `production funding profile bundle entry ${index.toString()} is not canonical ${category}`,
          );
        }
        return [category, profile] as const;
      }),
    ),
  ) as Readonly<
    Record<FraudProofCatalogueCategoryName, WorkflowFundingRequirements>
  >;
  const overlay = Object.freeze({
    schemaVersion: WATCHER_WORKFLOW_FUNDING_PROFILE_OVERLAY,
    deploymentFingerprint: deploymentIdentity.manifestId,
    releaseEvidenceDigest,
    bundlePath: canonicalPath,
    profiles,
  });
  admittedOverlays.add(overlay);
  return overlay;
};
