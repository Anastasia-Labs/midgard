import {
  createHash,
  createPrivateKey,
  createPublicKey,
  generateKeyPairSync,
  type KeyObject,
  sign,
} from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { pathToFileURL } from "node:url";

import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  type DeploymentManifest,
  makeDeploymentMarker,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import type { WatcherDeploymentIdentityPolicy } from "midgard-watcher";

import { writeDurableFile, writeOnceFile } from "./durable.js";
import type { Layout } from "./layout.js";

export type WatcherModule = typeof import("midgard-watcher");

/**
 * The watcher package as its own processes run it: the built distribution in
 * the checkout, not a copy bundled into this controller. The release is then
 * produced and verified by exactly the code that later loads it.
 */
export const loadWatcherModule = (layout: Layout): Promise<WatcherModule> =>
  import(
    pathToFileURL(join(layout.watcherRoot, "dist/index.js")).href
  ) as Promise<WatcherModule>;

export const releasePaths = (layout: Layout) => ({
  authority: join(layout.watcherRelease, "deployment-authority.json"),
  rules: join(layout.watcherRelease, "rules.json"),
  manifest: join(layout.watcherRelease, "deployment-manifest.json"),
  blueprint: join(layout.watcherRelease, "plutus.json"),
  deploymentInfo: join(layout.watcherRelease, "contract-deployment-info.json"),
  fundingProfiles: join(layout.watcherRelease, "funding-profiles.json"),
});

/** The finalized manifest the deployment wrote; the contract file is the manifest. */
export const readFinalizedManifest = (layout: Layout): DeploymentManifest => {
  const manifest = JSON.parse(
    readFileSync(layout.contractManifest, "utf8"),
  ) as DeploymentManifest;
  verifyFinalizedDeploymentManifest(manifest);
  if (manifest.network !== "Custom")
    throw new Error(
      `${layout.contractManifest} is not a Custom-network deployment`,
    );
  return manifest;
};

/** Generated once per run and never rotated: the saved watcher state binds it. */
const trustRootKey = (path: string): KeyObject => {
  if (!existsSync(path)) {
    const { privateKey } = generateKeyPairSync("ed25519");
    writeDurableFile(
      path,
      privateKey.export({ format: "pem", type: "pkcs8" }) as string,
      0o600,
    );
  }
  const key = createPrivateKey(readFileSync(path, "utf8"));
  if (key.asymmetricKeyType !== "ed25519")
    throw new Error(`${path} is not an ed25519 key`);
  return key;
};

/**
 * The one proof-program commitment the watcher binds: the computation-thread
 * policy, committed exactly as the watcher journeys do.
 */
const programCommitments = (manifest: DeploymentManifest) => {
  const policyId = manifest.contracts.computationThreadMint?.scriptHash;
  if (policyId === undefined)
    throw new Error("the deployment has no computation-thread policy");
  return {
    "computation-thread-policy-v1": createHash("sha256")
      .update(JSON.stringify({ computationThreadPolicyId: policyId }))
      .digest("hex"),
  };
};

const identityPolicy = (
  manifest: DeploymentManifest,
  bindings: {
    ruleBundleCommitment: string;
    programCommitments: Record<string, string>;
    fundingProfileBundleDigest: string;
    daIdentityDigest: string;
    blueprintHash: string;
  },
): WatcherDeploymentIdentityPolicy => {
  const catalogue =
    manifest.contracts.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (catalogue === undefined)
    throw new Error("the deployment has no fraud-proof catalogue");
  return {
    network: manifest.network,
    hubOracleOneShotOutRef: manifest.hubOracleOneShot.outRef,
    appliedScriptHashes: Object.fromEntries(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((name) => [
        name,
        manifest.contracts[name]!.scriptHash,
      ]),
    ),
    referenceScripts: Object.fromEntries(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
        (role) => {
          const reference =
            manifest.referenceScripts[
              role as keyof typeof manifest.referenceScripts
            ]!;
          return [
            role,
            { scriptHash: reference.scriptHash, outRef: reference.outRef },
          ];
        },
      ),
    ),
    fraudProofCatalogue: {
      root: catalogue.root,
      categories: Object.fromEntries(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => {
          const entry = catalogue.categories[category]!;
          return [
            category,
            { categoryId: entry.categoryId, scriptHash: entry.scriptHash },
          ];
        }),
      ) as WatcherDeploymentIdentityPolicy["fraudProofCatalogue"]["categories"],
    },
    ruleBundleCommitment: bindings.ruleBundleCommitment,
    programCommitments: bindings.programCommitments,
    fundingProfileBundleDigest: bindings.fundingProfileBundleDigest,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: bindings.daIdentityDigest,
    blueprintHash: bindings.blueprintHash,
  };
};

/**
 * Signs the deployment's finalized manifest into the watcher release bundle,
 * once. Every artifact is written before the authority that names them. An
 * existing release is reopened and verified, never re-signed; one signed for
 * another deployment is refused.
 *
 * No measured workflow funding profiles exist for a devnet, so the bundle is
 * empty: the watcher installs every runner and observes, and a category only
 * needs a profile when it reserves funds for a proof.
 */
export const ensureWatcherReleaseBundle = async (
  layout: Layout,
  watcher: WatcherModule,
  manifest: DeploymentManifest,
) => {
  const paths = releasePaths(layout);
  const blueprintBytes = readFileSync(layout.blueprint);
  const blueprintHash = createHash("sha256")
    .update(blueprintBytes)
    .digest("hex");
  if (blueprintHash !== manifest.artifacts.blueprintHash)
    throw new Error(
      `${layout.blueprint} is not the blueprint the deployment was built from`,
    );
  const commitments = programCommitments(manifest);
  const fundingBundle = watcher.createWatcherWorkflowFundingProfileBundle({
    profiles: [],
  });
  const ruleBundle = watcher.makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: manifest.manifestId,
      network: manifest.network,
      blueprintHash,
      programCommitments: commitments,
    },
    targetParameterSnapshot: manifest.cardanoProtocolParameters.snapshot,
  });
  const ruleBundleCommitment =
    watcher.computeWatcherRuleBundleCommitment(ruleBundle);
  const reopen = () =>
    watcher.loadWatcherVerifiedDeploymentAuthority({
      path: paths.authority,
      ruleBundlePath: paths.rules,
    });
  const key = trustRootKey(layout.watcherTrustRootKey);
  const publicKeySpkiDerHex = createPublicKey(key)
    .export({ format: "der", type: "spki" })
    .toString("hex");
  const trustRootId = createHash("sha256")
    .update(Buffer.from(publicKeySpkiDerHex, "hex"))
    .digest("hex");

  if (existsSync(paths.authority)) {
    const verified = (await reopen()).deploymentIdentity;
    if (verified.manifestId !== manifest.manifestId)
      throw new Error(
        `the watcher release under ${layout.watcherRelease} was signed for deployment ${verified.manifestId}, not ${manifest.manifestId}; it is never re-signed`,
      );
    if (
      verified.blueprintHash !== blueprintHash ||
      verified.ruleBundleCommitment !== ruleBundleCommitment ||
      verified.fundingProfileBundleDigest !==
        fundingBundle.fundingProfileBundleDigest ||
      verified.trustRootId !== trustRootId
    )
      throw new Error(
        `the watcher release under ${layout.watcherRelease} differs from this deployment's release inputs`,
      );
    await watcher.loadWatcherWorkflowFundingProfileOverlay({
      bundlePath: paths.fundingProfiles,
      deploymentIdentity: verified,
    });
    return verified;
  }

  const releaseBindings = {
    schemaVersion: watcher.WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
    ruleBundleCommitment,
    programCommitments: commitments,
    fundingProfileBundleDigest: fundingBundle.fundingProfileBundleDigest,
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestJsonDigest(manifest.da),
    },
    artifacts: { blueprintHash },
  };
  const signedIdentity = {
    schemaVersion: watcher.WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
    manifest,
    releaseBindings,
    attestation: {
      algorithm: "ed25519",
      trustRootId,
      signature: sign(
        null,
        watcher.makeWatcherDeploymentIdentitySignaturePayload(
          manifest.manifestId,
          releaseBindings,
        ),
        key,
      ).toString("hex"),
    },
  };
  const policy = identityPolicy(manifest, {
    ruleBundleCommitment,
    programCommitments: commitments,
    fundingProfileBundleDigest: fundingBundle.fundingProfileBundleDigest,
    daIdentityDigest: releaseBindings.da.identityDigest,
    blueprintHash,
  });
  // Artifacts first (each write-once), the authority that names them last.
  writeOnceFile(paths.rules, JSON.stringify(ruleBundle), 0o644);
  // The deployment's own bytes: its DA runtime manifests bind their digest.
  const manifestBytes = readFileSync(layout.contractManifest);
  writeOnceFile(paths.manifest, manifestBytes, 0o644);
  writeOnceFile(paths.blueprint, blueprintBytes, 0o644);
  writeOnceFile(paths.deploymentInfo, manifestBytes, 0o644);
  writeOnceFile(
    paths.fundingProfiles,
    fundingBundle.fundingProfileBundleBytes,
    0o644,
  );
  writeDurableFile(
    paths.authority,
    JSON.stringify({
      signedIdentity,
      policy,
      trustRoots: [{ trustRootId, publicKeySpkiDerHex }],
      durableMarker: makeDeploymentMarker(manifest.manifestId),
    }),
    0o644,
  );
  const verified = (await reopen()).deploymentIdentity;
  await watcher.loadWatcherWorkflowFundingProfileOverlay({
    bundlePath: paths.fundingProfiles,
    deploymentIdentity: verified,
  });
  return verified;
};
