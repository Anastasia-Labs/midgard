import { type KeyObject, sign } from "node:crypto";

import {
  computeDeploymentManifestJsonDigest,
  makeDeploymentMarker,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect } from "vitest";

import {
  makeWatcherDeploymentIdentitySignaturePayload,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
  WatcherDeploymentIdentityError,
  type WatcherDeploymentIdentityPolicy,
} from "../../src/runtime/deployment-identity.js";
import {
  appliedScriptHashes,
  BLUEPRINT_HASH,
  canonicalIdentity,
  cataloguePolicy,
  makeTrustRoot,
  type MutableRecord,
  referenceScriptPolicy,
  RULE_BUNDLE_COMMITMENT,
  withManifestId,
} from "./deployment-identity.canonical-identity.js";

export const makeFixture = () => {
  const manifest = withManifestId(canonicalIdentity());
  verifyFinalizedDeploymentManifest(manifest);
  const programCommitments = {
    "validation-machine-v1": "88".repeat(32),
    "transition-order-v1": "99".repeat(32),
  };
  const releaseBindings = {
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
    fundingProfileBundleDigest: "ab".repeat(32),
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
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
    network: "Preprod",
    hubOracleOneShotOutRef: manifest.hubOracleOneShot.outRef,
    appliedScriptHashes: appliedScriptHashes(manifest),
    referenceScripts: referenceScriptPolicy(manifest),
    fraudProofCatalogue: cataloguePolicy(manifest),
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
    programCommitments,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: releaseBindings.da.identityDigest,

    blueprintHash: BLUEPRINT_HASH,
    fundingProfileBundleDigest: "ab".repeat(32),
  };
  const resign = (
    identity: MutableRecord = signedIdentity,
    signingKey: KeyObject = privateKey,
  ): void => {
    identity.attestation.signature = sign(
      null,
      makeWatcherDeploymentIdentitySignaturePayload(
        identity.manifest.manifestId,
        identity.releaseBindings,
      ),
      signingKey,
    ).toString("hex");
  };
  resign();
  return {
    signedIdentity,
    policy,
    trustRoot,
    privateKey,
    resign,
    durableMarker: makeDeploymentMarker(manifest.manifestId),
  };
};

export const rejection = (
  action: () => unknown,
  code: WatcherDeploymentIdentityError["code"],
  path: string | RegExp,
): WatcherDeploymentIdentityError => {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherDeploymentIdentityError);
    const deploymentError = error as WatcherDeploymentIdentityError;
    expect(deploymentError.code).toBe(code);
    if (typeof path === "string") {
      expect(deploymentError.path).toBe(path);
    } else {
      expect(deploymentError.path).toMatch(path);
    }
    return deploymentError;
  }
  throw new Error("Expected watcher deployment identity rejection");
};
