import { createHash } from "node:crypto";
import { readFile, realpath } from "node:fs/promises";
import { isAbsolute } from "node:path";

import {
  createProductionWorkflowFundingRequirementsV1,
  type ProductionWorkflowFundingRequirementsInputV1,
  type ProductionWorkflowFundingRequirementsV1,
} from "@al-ft/midgard-fault-proofs";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import {
  assertVerifiedWatcherDeploymentIdentityV1,
  type VerifiedWatcherDeploymentIdentityV1,
} from "./deployment-identity.js";

export const WATCHER_PRODUCTION_WORKFLOW_FUNDING_PROFILE_BUNDLE_V1 =
  "midgard-watcher-production-workflow-funding-profile-bundle-v1" as const;
export const WATCHER_PRODUCTION_WORKFLOW_FUNDING_RELEASE_EVIDENCE_V1 =
  "midgard-watcher-production-workflow-funding-release-evidence-v1" as const;
export const WATCHER_PRODUCTION_WORKFLOW_FUNDING_PROFILE_OVERLAY_V1 =
  "midgard-watcher-production-workflow-funding-profile-overlay-v1" as const;

export type WatcherProductionWorkflowFundingProfileOverlayV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_WORKFLOW_FUNDING_PROFILE_OVERLAY_V1;
  deploymentFingerprint: string;
  releaseEvidenceDigest: string;
  bundlePath: string;
  profiles: Readonly<
    Record<
      FraudProofCatalogueCategoryName,
      ProductionWorkflowFundingRequirementsV1
    >
  >;
}>;

export type WatcherProductionWorkflowFundingProfileBodyV1 = Omit<
  ProductionWorkflowFundingRequirementsInputV1,
  "deploymentFingerprint"
>;

export type WatcherProductionWorkflowFundingReleaseEvidenceV1 = Readonly<{
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
): ProductionWorkflowFundingRequirementsV1 => {
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
  return createProductionWorkflowFundingRequirementsV1({
    ...(record as unknown as WatcherProductionWorkflowFundingProfileBodyV1),
    deploymentFingerprint,
  });
};

/**
 * Deployment builder seam. Profile bodies deliberately omit the future
 * manifest ID, so construction terminates in finite order: funding bundle,
 * release evidence, manifest ID, then hydrated admitted profiles.
 */
export const createWatcherProductionWorkflowFundingReleaseEvidenceV1 = ({
  profiles,
}: {
  readonly profiles: readonly WatcherProductionWorkflowFundingProfileBodyV1[];
}): WatcherProductionWorkflowFundingReleaseEvidenceV1 => {
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
    schemaVersion: WATCHER_PRODUCTION_WORKFLOW_FUNDING_PROFILE_BUNDLE_V1,
    profiles: normalizedBodies,
  });
  const fundingProfileBundleDigest = sha256(
    Buffer.from(canonicalJson(fundingProfileBundle), "utf8"),
  );
  const releaseEvidence = Object.freeze({
    schemaVersion: WATCHER_PRODUCTION_WORKFLOW_FUNDING_RELEASE_EVIDENCE_V1,
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

export const assertWatcherProductionWorkflowFundingProfileOverlayV1 = (
  overlay: WatcherProductionWorkflowFundingProfileOverlayV1,
): void => {
  if (!admittedOverlays.has(overlay)) {
    throw new Error(
      "production workflow funding profile overlay was not admitted from signed release evidence",
    );
  }
};

export const productionWorkflowFundingProfileFromOverlayV1 = ({
  overlay,
  category,
}: {
  readonly overlay: WatcherProductionWorkflowFundingProfileOverlayV1;
  readonly category: FraudProofCatalogueCategoryName;
}): ProductionWorkflowFundingRequirementsV1 => {
  assertWatcherProductionWorkflowFundingProfileOverlayV1(overlay);
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
export const loadWatcherProductionWorkflowFundingProfileOverlayV1 = async ({
  bundlePath,
  deploymentIdentity,
}: {
  readonly bundlePath: string;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
}): Promise<WatcherProductionWorkflowFundingProfileOverlayV1> => {
  assertVerifiedWatcherDeploymentIdentityV1(deploymentIdentity);
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
    releaseEvidence.schemaVersion !==
    WATCHER_PRODUCTION_WORKFLOW_FUNDING_RELEASE_EVIDENCE_V1
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
    WATCHER_PRODUCTION_WORKFLOW_FUNDING_PROFILE_BUNDLE_V1
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
    Record<
      FraudProofCatalogueCategoryName,
      ProductionWorkflowFundingRequirementsV1
    >
  >;
  const overlay = Object.freeze({
    schemaVersion: WATCHER_PRODUCTION_WORKFLOW_FUNDING_PROFILE_OVERLAY_V1,
    deploymentFingerprint: deploymentIdentity.manifestId,
    releaseEvidenceDigest,
    bundlePath: canonicalPath,
    profiles,
  });
  admittedOverlays.add(overlay);
  return overlay;
};
