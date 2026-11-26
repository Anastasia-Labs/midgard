import { sign } from "node:crypto";

import {
  computeDeploymentManifestJsonDigest,
  type DeploymentManifest,
  deploymentManifestHubOraclePolicyId,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION as WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
  SIGNED_DEPLOYMENT_RELEASE_SCHEMA_VERSION as WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-release";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { expect } from "vitest";

import {
  WatcherDeploymentIdentityError,
  type WatcherDeploymentIdentityErrorCode,
  type WatcherDeploymentIdentityPolicy,
} from "../../src/runtime/deployment-identity.catalogue-category-to-contract.js";
import { makeWatcherDeploymentIdentitySignaturePayload } from "../../src/runtime/deployment-identity.parse-policy.js";
import { verifyWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.verify-watcher-deployment-identity.js";
import {
  WatcherRuleBundleError,
  type WatcherRuleBundleErrorCode,
} from "../../src/verification/rule-bundle.canonical-json-value.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.parse-watcher-rule-bundle.js";
import {
  appliedScriptHashes,
  BLUEPRINT_HASH,
  canonicalManifestIdentity,
  cataloguePolicy,
  h32,
  makeTrustRoot,
  type MutableRecord,
  referenceScriptPolicy,
  type SignedAuthorityFixture,
  TARGET_PARAMETERS,
} from "./rule-bundle.canonical-manifest-identity.js";

export const fixture = (network: "Preprod" | "Custom" = "Preprod") => {
  const manifest: MutableRecord = { ...canonicalManifestIdentity(), network };
  const programCommitments = Object.freeze({
    "transition-order-v1": h32("8"),
    "validation-machine-v1": h32("9"),
  });
  const bundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      hubOraclePolicyId: deploymentManifestHubOraclePolicyId(
        manifest as DeploymentManifest,
      ),
      network,
      blueprintHash: BLUEPRINT_HASH,
      programCommitments,
    },
    targetParameterSnapshot: TARGET_PARAMETERS,
  });
  const ruleBundleCommitment = computeWatcherRuleBundleCommitment(bundle);
  const releaseBindings = {
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
    fundingProfileBundleDigest: "ab".repeat(32),
    ruleBundleCommitment,
    programCommitments,
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestJsonDigest(manifest.da),
    },
    artifacts: {
      blueprintHash: BLUEPRINT_HASH,
    },
  };
  const { privateKey, trustRoot } = makeTrustRoot();
  const signedIdentity: MutableRecord = {
    schemaVersion: WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
    manifest,
    releaseBindings,
    attestation: {
      algorithm: "ed25519",
      trustRootId: trustRoot.trustRootId,
      signature: "",
    },
  };
  const policy: WatcherDeploymentIdentityPolicy = {
    network,
    hubOracleOneShotOutRef: manifest.hubOracleOneShot.outRef,
    appliedScriptHashes: appliedScriptHashes(manifest),
    referenceScripts: referenceScriptPolicy(manifest),
    fraudProofCatalogue: cataloguePolicy(manifest),
    ruleBundleCommitment,
    programCommitments,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: releaseBindings.da.identityDigest,
    fundingProfileBundleDigest: releaseBindings.fundingProfileBundleDigest,

    blueprintHash: BLUEPRINT_HASH,
  };
  signedIdentity.attestation.signature = sign(
    null,
    makeWatcherDeploymentIdentitySignaturePayload(
      manifest as DeploymentManifest,
      releaseBindings,
    ),
    privateKey,
  ).toString("hex");
  const authority: SignedAuthorityFixture = Object.freeze({
    signedIdentity,
    policy,
    trustRoots: Object.freeze([trustRoot]),
  });
  return {
    authority,
    bundle,
    verifiedIdentity: verifyWatcherDeploymentIdentity(authority),
  };
};

type Mutable<T> = T extends readonly (infer Entry)[]
  ? Mutable<Entry>[]
  : T extends object
    ? { -readonly [Key in keyof T]: Mutable<T[Key]> }
    : T;

export const clone = <T>(value: T): Mutable<T> =>
  JSON.parse(JSON.stringify(value)) as Mutable<T>;

export const rejected = (
  action: () => unknown,
  code: WatcherRuleBundleErrorCode,
  path: string,
): WatcherRuleBundleError => {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherRuleBundleError);
    const ruleError = error as WatcherRuleBundleError;
    expect(ruleError.code).toBe(code);
    expect(ruleError.path).toBe(path);
    return ruleError;
  }
  throw new Error("Expected canonical V1 rule-bundle rejection");
};

export const authorityRejected = (
  action: () => unknown,
  code: WatcherDeploymentIdentityErrorCode,
  path: string,
): WatcherDeploymentIdentityError => {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherDeploymentIdentityError);
    const authorityError = error as WatcherDeploymentIdentityError;
    expect(authorityError.code).toBe(code);
    expect(authorityError.path).toBe(path);
    return authorityError;
  }
  throw new Error("Expected signed deployment-authority rejection");
};
