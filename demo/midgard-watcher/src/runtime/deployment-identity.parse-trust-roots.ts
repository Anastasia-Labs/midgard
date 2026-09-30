import { createHash, createPublicKey, timingSafeEqual } from "node:crypto";

import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  type DeploymentManifest,
  type DeploymentManifestAvailabilityChallenge,
  type DeploymentManifestCardanoProtocolParameters,
  type DeploymentManifestEventHistoryRecipe,
  type DeploymentMarker,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type FraudProofReleaseEconomicsAuthority,
  type FraudProofReleaseFinalityAuthority,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs";

import {
  CATALOGUE_CATEGORY_TO_CONTRACT,
  equalStringMaps,
  exactRecord,
  exactString,
  fail,
  HEX_32,
  type ParsedPolicy,
  REFERENCE_SCRIPT_ROLES,
  WATCHER_DEPLOYMENT_AVAILABILITY_CHALLENGE_AUTHORITY_SCHEMA_VERSION,
  WATCHER_DEPLOYMENT_PROTOCOL_PARAMETER_AUTHORITY_SCHEMA_VERSION,
  WATCHER_DEPLOYMENT_PROTOCOL_SCRIPT_AUTHORITY_SCHEMA_VERSION,
  type WatcherDeploymentTrustRoot,
  type WatcherFraudProofCatalogueIdentity,
  type WatcherReferenceScriptIdentity,
} from "./deployment-identity.catalogue-category-to-contract.js";
import {
  type ParsedTrustRoot,
  type ReleaseBindings,
} from "./deployment-identity.parse-policy.js";

export const parseTrustRoots = (
  value: readonly WatcherDeploymentTrustRoot[],
): ReadonlyMap<string, ParsedTrustRoot> => {
  if (!Array.isArray(value) || value.length === 0 || value.length > 16) {
    fail("invalid_trust_root", "$.trustRoots");
  }
  const roots = new Map<string, ParsedTrustRoot>();
  value.forEach((entry, index) => {
    const path = `$.trustRoots[${index.toString()}]`;
    const root = exactRecord(entry, path, [
      "trustRootId",
      "publicKeySpkiDerHex",
    ]);
    const trustRootId = exactString(
      root.trustRootId,
      `${path}.trustRootId`,
      HEX_32,
    );
    if (roots.has(trustRootId)) {
      fail("duplicate_trust_root", `${path}.trustRootId`);
    }
    const publicKeySpkiDerHex = exactString(
      root.publicKeySpkiDerHex,
      `${path}.publicKeySpkiDerHex`,
      /^(?:[0-9a-f]{2}){44}$/u,
    );
    const publicKeySpkiDer = Buffer.from(publicKeySpkiDerHex, "hex");
    const derivedId = createHash("sha256")
      .update(publicKeySpkiDer)
      .digest("hex");
    if (
      !timingSafeEqual(
        Buffer.from(derivedId, "hex"),
        Buffer.from(trustRootId, "hex"),
      )
    ) {
      fail("invalid_trust_root", `${path}.trustRootId`);
    }
    try {
      const publicKey = createPublicKey({
        key: publicKeySpkiDer,
        format: "der",
        type: "spki",
      });
      if (publicKey.asymmetricKeyType !== "ed25519") {
        fail("invalid_trust_root", `${path}.publicKeySpkiDerHex`);
      }
    } catch {
      fail("invalid_trust_root", `${path}.publicKeySpkiDerHex`);
    }
    roots.set(trustRootId, Object.freeze({ trustRootId, publicKeySpkiDer }));
  });
  return roots;
};

const mapManifestContracts = (
  manifest: DeploymentManifest,
): Readonly<Record<string, string>> => {
  const hashes: Record<string, string> = {};
  for (const contractName of DEPLOYMENT_MANIFEST_CONTRACT_NAMES) {
    hashes[contractName] = manifest.contracts[contractName].scriptHash;
  }
  return hashes;
};

const assertReferenceScripts = (
  manifest: DeploymentManifest,
  expected: Readonly<Record<string, WatcherReferenceScriptIdentity>>,
): void => {
  const scripts = manifest.referenceScripts;
  for (const role of REFERENCE_SCRIPT_ROLES) {
    const entry = scripts[role];
    if (
      entry.scriptHash !== expected[role]?.scriptHash ||
      entry.outRef !== expected[role]?.outRef
    ) {
      fail("mismatched_identity", `$.manifest.referenceScripts.${role}`);
    }
  }
};

const assertCatalogue = (
  manifest: DeploymentManifest,
  expected: WatcherFraudProofCatalogueIdentity,
): void => {
  const contracts = manifest.contracts;
  const catalogue = contracts.fraudProofCatalogueMint.fraudProofCatalogue;
  if (catalogue === undefined) {
    return fail(
      "invalid_field",
      "$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue",
    );
  }
  if (catalogue.root !== expected.root) {
    fail(
      "mismatched_identity",
      "$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
    );
  }
  const categories = catalogue.categories;
  for (const category of Object.keys(CATALOGUE_CATEGORY_TO_CONTRACT) as Array<
    keyof typeof CATALOGUE_CATEGORY_TO_CONTRACT
  >) {
    const entry = categories[category];
    const expectedEntry = expected.categories[category];
    if (
      entry.categoryId !== expectedEntry.categoryId ||
      entry.scriptHash !== expectedEntry.scriptHash
    ) {
      fail(
        "mismatched_identity",
        `$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${category}`,
      );
    }
  }
};

export const assertPolicyBindings = (
  manifest: DeploymentManifest,
  bindings: ReleaseBindings,
  policy: ParsedPolicy,
): void => {
  if (manifest.network !== policy.network) {
    fail("mismatched_identity", "$.manifest.network");
  }
  if (manifest.hubOracleOneShot.outRef !== policy.hubOracleOneShotOutRef) {
    fail("mismatched_identity", "$.manifest.hubOracleOneShot");
  }
  const actualScriptHashes = mapManifestContracts(manifest);
  if (!equalStringMaps(actualScriptHashes, policy.appliedScriptHashes)) {
    fail("mismatched_identity", "$.manifest.contracts");
  }
  assertReferenceScripts(manifest, policy.referenceScripts);
  assertCatalogue(manifest, policy.fraudProofCatalogue);

  if (
    bindings.ruleBundleCommitment !== policy.ruleBundleCommitment ||
    !equalStringMaps(bindings.programCommitments, policy.programCommitments)
  ) {
    fail("mismatched_identity", "$.releaseBindings");
  }
  if (
    bindings.da.mode !== policy.daMode ||
    bindings.da.identityDigest !== policy.daIdentityDigest ||
    computeDeploymentManifestJsonDigest(manifest.da) !== policy.daIdentityDigest
  ) {
    fail("mismatched_identity", "$.releaseBindings.da");
  }
  if (
    bindings.fundingProfileBundleDigest !== policy.fundingProfileBundleDigest
  ) {
    fail("mismatched_identity", "$.releaseBindings.fundingProfileBundleDigest");
  }
  if (
    bindings.artifacts.blueprintHash !== policy.blueprintHash ||
    manifest.artifacts.blueprintHash !== policy.blueprintHash
  ) {
    fail("mismatched_identity", "$.releaseBindings.artifacts");
  }
};

export const verifyCanonicalManifest = (value: unknown): DeploymentManifest => {
  // Validate the exact signed manifest, including contract bytes, parameters,
  // references, and blueprint identity. No release evidence is required.
  try {
    return verifyFinalizedDeploymentManifest(value);
  } catch {
    return fail("canonical_manifest_invalid", "$.manifest");
  }
};

export type VerifiedWatcherDeploymentIdentity = Readonly<{
  manifestId: string;
  network: "Mainnet" | "Preprod" | "Preview" | "Custom";
  trustRootId: string;
  blueprintHash: string;
  fundingProfileBundleDigest: string;
  ruleBundleCommitment: string;
  programCommitments: Readonly<Record<string, string>>;
  durableMarker: DeploymentMarker;
}>;

export type WatcherDeploymentProtocolScriptAuthority = Readonly<{
  schemaVersion: typeof WATCHER_DEPLOYMENT_PROTOCOL_SCRIPT_AUTHORITY_SCHEMA_VERSION;
  deploymentFingerprint: string;
  network: "Mainnet" | "Preprod" | "Preview" | "Custom";
  hubOracleOneShotOutRef: string;
  protocolScriptHashes: Readonly<{
    hubOracleMint: string;
    stateQueueSpend: string;
    stateQueueMint: string;
    correctionLockSpend: string;
    fraudProofSpend: string;
    fraudProofMint: string;
    referenceScriptAuthMint: string;
    availabilityChallengeSpend: string;
    availabilityChallengeMint: string;
    daBondPoolSpend: string;
    daBondPoolMint: string;
    daAttestationMint: string;
    availabilityChallengeOpenWithdraw: string;
    availabilityChallengeSettleWithdraw: string;
    availabilityChallengeCloseWithdraw: string;
    availabilityChallengeTimeoutWithdraw: string;
  }>;
  referenceScripts: Readonly<Record<string, WatcherReferenceScriptIdentity>>;
  authorityDigest: string;
}>;

export type WatcherDeploymentProtocolParameterAuthority = Readonly<{
  schemaVersion: typeof WATCHER_DEPLOYMENT_PROTOCOL_PARAMETER_AUTHORITY_SCHEMA_VERSION;
  deploymentFingerprint: string;
  snapshot: DeploymentManifestCardanoProtocolParameters;
  snapshotDigest: string;
  authorityDigest: string;
}>;

export type WatcherDeploymentAvailabilityChallengeAuthority = Readonly<{
  schemaVersion: typeof WATCHER_DEPLOYMENT_AVAILABILITY_CHALLENGE_AUTHORITY_SCHEMA_VERSION;
  deploymentFingerprint: string;
  parameters: DeploymentManifestAvailabilityChallenge;
  parametersDigest: string;
  authorityDigest: string;
}>;

export const authenticatedWatcherDeploymentIdentities = new WeakSet<object>();

export const authenticatedWatcherProtocolScriptAuthorities =
  new WeakSet<object>();

export const authenticatedWatcherProtocolParameterAuthorities =
  new WeakSet<object>();

export const authenticatedWatcherReleaseFinalityAuthorities =
  new WeakSet<object>();

export const authenticatedWatcherReleaseEconomicsAuthorities =
  new WeakSet<object>();

export const authenticatedWatcherAvailabilityChallengeAuthorities =
  new WeakSet<object>();

export const protocolScriptAuthorityByDeploymentIdentity = new WeakMap<
  object,
  WatcherDeploymentProtocolScriptAuthority
>();

export const appliedScriptHashesByDeploymentIdentity = new WeakMap<
  object,
  Readonly<Record<string, string>>
>();

export const releaseFinalityAuthorityByDeploymentIdentity = new WeakMap<
  object,
  FraudProofReleaseFinalityAuthority
>();

export const releaseFinalityByDeploymentIdentity = new WeakMap<
  object,
  VerifiedFraudProofReleaseFinalityPolicy
>();

export const releaseEconomicsAuthorityByDeploymentIdentity = new WeakMap<
  object,
  FraudProofReleaseEconomicsAuthority
>();

export const protocolParameterAuthorityByDeploymentIdentity = new WeakMap<
  object,
  WatcherDeploymentProtocolParameterAuthority
>();

export const availabilityChallengeAuthorityByDeploymentIdentity = new WeakMap<
  object,
  WatcherDeploymentAvailabilityChallengeAuthority
>();

export const USER_EVENT_SIGNED_CONTRACT_NAMES = [
  "hubOracleMint",
  "depositMint",
  "depositSpend",
  "withdrawalMint",
  "withdrawalSpend",
  "depositHistoryRetentionSpend",
  "depositHistoryRetirementWithdraw",
  "withdrawalHistoryRetentionSpend",
  "withdrawalHistoryRetirementWithdraw",
  "txOrderMint",
  "txOrderSpend",
  "fieldPreimageCertificateMint",
] as const;

export type UserEventSignedContractName =
  (typeof USER_EVENT_SIGNED_CONTRACT_NAMES)[number];

export type SignedUserEventScript = Readonly<{
  type: string;
  cborHex: string;
  scriptHash: string;
}>;

export const userEventScriptsByDeploymentIdentity = new WeakMap<
  object,
  Readonly<{
    blueprintSha256: string;
    historyRecipes: Readonly<
      Record<"deposit" | "withdrawal", DeploymentManifestEventHistoryRecipe>
    >;
    contracts: Readonly<
      Record<UserEventSignedContractName, SignedUserEventScript>
    >;
  }>
>();

export declare const watcherUserEventScriptBindingBrand: unique symbol;
