import {
  createHash,
  createPrivateKey,
  createPublicKey,
  generateKeyPairSync,
  type KeyObject,
  sign,
} from "node:crypto";
import { existsSync } from "node:fs";
import { readFile, rename, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  makeDeploymentMarker,
  parseDeploymentManifestEconomics,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { computeFraudProofReleaseEconomicsPolicyDigest } from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import type { publishWorkflowDeploymentOnChain } from "midgard-node/tests/helpers/published-workflow-deployment";

import {
  createWatcherWorkflowFundingProfileBundle,
  loadWatcherWorkflowFundingProfileOverlay,
  type WatcherWorkflowFundingProfileBody,
} from "../../src/funding/workflow-funding-profile-overlay.js";
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import {
  makeWatcherDeploymentIdentitySignaturePayload,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
  type WatcherDeploymentIdentityPolicy,
} from "../../src/runtime/deployment-identity.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";

type PublishedDeployment = Awaited<
  ReturnType<typeof publishWorkflowDeploymentOnChain>
>;

export type WatcherTestTrustRootKey = Readonly<{
  privateKey: KeyObject;
  publicKeySpkiDerHex: string;
  trustRootId: string;
}>;

const trustRootKeyOf = (privateKey: KeyObject): WatcherTestTrustRootKey => {
  const publicKeySpkiDerHex = createPublicKey(privateKey)
    .export({ format: "der", type: "spki" })
    .toString("hex");
  return {
    privateKey,
    publicKeySpkiDerHex,
    trustRootId: createHash("sha256")
      .update(Buffer.from(publicKeySpkiDerHex, "hex"))
      .digest("hex"),
  };
};

/**
 * Loads the ed25519 trust-root signing key saved at `path`, creating it on
 * first use. Journeys that share one watcher runtime must attest their
 * deployment under one trust root: the watcher's user-event policy binds the
 * attesting trust root id, so a re-keyed authority makes the runtime refuse
 * its saved history as a changed dependency.
 */
export const loadOrCreateWatcherTestTrustRootKey = async (
  path: string,
): Promise<WatcherTestTrustRootKey> => {
  if (existsSync(path)) {
    const privateKey = createPrivateKey(await readFile(path, "utf8"));
    if (privateKey.asymmetricKeyType !== "ed25519")
      throw new Error(`Saved trust-root key ${path} is not an ed25519 key`);
    return trustRootKeyOf(privateKey);
  }
  const { privateKey } = generateKeyPairSync("ed25519");
  await writeFile(path, privateKey.export({ format: "pem", type: "pkcs8" }), {
    mode: 0o600,
    flag: "wx",
  });
  return trustRootKeyOf(privateKey);
};

/**
 * Signs the exact published manifest once and reuses its saved test trust root.
 * The blueprint hash, published references and supplied proof program
 * commitments still pass the production identity and rule-bundle loaders.
 * With `trustRootKeyPath` the attesting key is the saved one, so every
 * directory signed against that path publishes the same trust root; a saved
 * authority under another root is set aside and re-signed.
 */
export const createPublishedWatcherDeploymentAuthority = async ({
  deployment,
  programCommitments,
  fundingProfiles,
  directory,
  trustRootKeyPath,
}: {
  readonly deployment: PublishedDeployment;
  readonly programCommitments: Readonly<Record<string, string>>;
  readonly fundingProfiles: readonly WatcherWorkflowFundingProfileBody[];
  readonly directory: string;
  readonly trustRootKeyPath?: string;
}) => {
  const manifest = deployment.manifest;
  verifyFinalizedDeploymentManifest(manifest);
  if (manifest.network !== "Preprod" && manifest.network !== "Custom") {
    throw new Error(
      "Published watcher scenario requires a Preprod or Custom deployment",
    );
  }
  const blueprintHash = createHash("sha256")
    .update(deployment.blueprintJson)
    .digest("hex");
  if (blueprintHash !== manifest.artifacts.blueprintHash) {
    throw new Error("Published watcher blueprint differs from its manifest");
  }
  const economics = parseDeploymentManifestEconomics(manifest.economics);
  const economicsPolicyDigest = computeFraudProofReleaseEconomicsPolicyDigest({
    profile: economics.profile,
    requiredBondLovelace: economics.requiredBondLovelace.toString(),
    slashingPenaltyLovelace: economics.slashingPenaltyLovelace.toString(),
    fraudProverRewardLovelace: economics.fraudProverRewardLovelace.toString(),
    inactivitySlashingPenaltyLovelace:
      economics.inactivitySlashingPenaltyLovelace.toString(),
    proverCollateralFloorLovelace:
      economics.proverCollateralFloorLovelace.toString(),
  });
  const fundingPaymentKeyHash = paymentCredentialOf(
    await deployment.publisherLucid.wallet().address(),
  ).hash;
  const publishedReferenceScriptHashes = new Set(
    Object.values(manifest.referenceScripts).map(
      (reference) => reference!.scriptHash,
    ),
  );
  for (const profile of fundingProfiles) {
    if (
      profile.blueprintSha256 !== blueprintHash ||
      profile.protocolParametersDigest !==
        manifest.cardanoProtocolParameters.digest ||
      profile.economicsPolicyDigest !== economicsPolicyDigest ||
      profile.fundingPaymentKeyHash !== fundingPaymentKeyHash
    ) {
      throw new Error(
        "Published watcher funding measurement differs from deployment parameters or signer",
      );
    }
    if (
      profile.actions.some((action) =>
        action.referenceInputs.some(
          (reference) =>
            reference.scriptHash !== null &&
            !publishedReferenceScriptHashes.has(reference.scriptHash),
        ),
      )
    ) {
      throw new Error(
        "Published watcher funding measurement refers to a different applied script",
      );
    }
  }
  const fundingBundle = createWatcherWorkflowFundingProfileBundle({
    profiles: fundingProfiles,
  });
  const { fundingProfileBundleDigest } = fundingBundle;
  const catalogue =
    manifest.contracts.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (catalogue === undefined) {
    throw new Error("Published watcher deployment has no catalogue");
  }
  const ruleBundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: manifest.manifestId,
      network: manifest.network,
      blueprintHash,
      programCommitments,
    },
    targetParameterSnapshot: manifest.cardanoProtocolParameters.snapshot,
  });
  const ruleBundleCommitment = computeWatcherRuleBundleCommitment(ruleBundle);
  const releaseBindings = {
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
    ruleBundleCommitment,
    programCommitments,
    fundingProfileBundleDigest,
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestJsonDigest(manifest.da),
    },
    artifacts: { blueprintHash },
  };
  const authorityPath = join(directory, "deployment-authority.json");
  const ruleBundlePath = join(directory, "rules.json");
  const manifestPath = join(directory, "deployment-manifest.json");
  const blueprintPath = join(directory, "plutus.json");
  const deploymentInfoPath = join(directory, "contract-deployment-info.json");
  const fundingProfileBundlePath = join(directory, "funding-profiles.json");
  const reopen = () =>
    loadWatcherVerifiedDeploymentAuthority({
      path: authorityPath,
      ruleBundlePath,
    });
  const trustRootKey =
    trustRootKeyPath === undefined
      ? trustRootKeyOf(generateKeyPairSync("ed25519").privateKey)
      : await loadOrCreateWatcherTestTrustRootKey(trustRootKeyPath);
  if (existsSync(authorityPath) && trustRootKeyPath !== undefined) {
    const saved: { trustRoots: readonly { trustRootId: string }[] } =
      JSON.parse(await readFile(authorityPath, "utf8"));
    if (saved.trustRoots[0]?.trustRootId !== trustRootKey.trustRootId)
      await rename(
        authorityPath,
        join(
          directory,
          `deployment-authority.superseded-${saved.trustRoots[0]?.trustRootId ?? "unknown"}.json`,
        ),
      );
  }
  if (existsSync(authorityPath)) {
    const deploymentAuthority = await reopen();
    const verified = deploymentAuthority.deploymentIdentity;
    if (
      verified.manifestId !== manifest.manifestId ||
      verified.blueprintHash !== blueprintHash ||
      verified.ruleBundleCommitment !== ruleBundleCommitment ||
      verified.fundingProfileBundleDigest !== fundingProfileBundleDigest
    )
      throw new Error(
        "Saved watcher authority differs from the requested deployment or release",
      );
    const saved: {
      signedIdentity: unknown;
      policy: WatcherDeploymentIdentityPolicy;
      trustRoots: readonly {
        trustRootId: string;
        publicKeySpkiDerHex: string;
      }[];
    } = JSON.parse(await readFile(authorityPath, "utf8"));
    const fundingProfileOverlay =
      await loadWatcherWorkflowFundingProfileOverlay({
        bundlePath: fundingProfileBundlePath,
        deploymentIdentity: verified,
      });
    return {
      nativeDeployment: {
        signedIdentity: saved.signedIdentity,
        policy: saved.policy,
        trustRoots: saved.trustRoots,
        result: verified,
        marker: makeDeploymentMarker(manifest.manifestId),
        contracts: manifest.contracts,
      },
      deploymentAuthority,
      fundingProfileOverlay,
      reopen,
      authorityPath,
      ruleBundlePath,
      manifestPath,
      blueprintPath,
      deploymentInfoPath,
      fundingProfileBundlePath,
    };
  }
  const { privateKey, publicKeySpkiDerHex, trustRootId } = trustRootKey;
  const signedIdentity = {
    schemaVersion: WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
    manifest,
    releaseBindings,
    attestation: {
      algorithm: "ed25519",
      trustRootId,
      signature: sign(
        null,
        makeWatcherDeploymentIdentitySignaturePayload(
          manifest.manifestId,
          releaseBindings,
        ),
        privateKey,
      ).toString("hex"),
    },
  };
  const policy: WatcherDeploymentIdentityPolicy = {
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
          const reference = manifest.referenceScripts[role]!;
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
    ruleBundleCommitment,
    programCommitments,
    fundingProfileBundleDigest,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: releaseBindings.da.identityDigest,

    blueprintHash,
  };
  await Promise.all([
    writeFile(ruleBundlePath, JSON.stringify(ruleBundle)),
    writeFile(manifestPath, JSON.stringify(manifest)),
    writeFile(blueprintPath, deployment.blueprintJson),
    writeFile(deploymentInfoPath, JSON.stringify(deployment.deploymentInfo)),
    writeFile(
      fundingProfileBundlePath,
      fundingBundle.fundingProfileBundleBytes,
    ),
  ]);
  // Publish the reusable authority only after all of its artifacts exist.
  await writeFile(
    authorityPath,
    JSON.stringify({
      signedIdentity,
      policy,
      trustRoots: [{ trustRootId, publicKeySpkiDerHex }],
      durableMarker: makeDeploymentMarker(manifest.manifestId),
    }),
    { flag: "wx" },
  );
  const deploymentAuthority = await reopen();
  const fundingProfileOverlay = await loadWatcherWorkflowFundingProfileOverlay({
    bundlePath: fundingProfileBundlePath,
    deploymentIdentity: deploymentAuthority.deploymentIdentity,
  });
  return {
    nativeDeployment: {
      signedIdentity,
      policy,
      trustRoots: [{ trustRootId, publicKeySpkiDerHex }],
      result: deploymentAuthority.deploymentIdentity,
      marker: makeDeploymentMarker(manifest.manifestId),
      contracts: manifest.contracts,
    },
    deploymentAuthority,
    fundingProfileOverlay,
    reopen,
    authorityPath,
    ruleBundlePath,
    manifestPath,
    blueprintPath,
    deploymentInfoPath,
    fundingProfileBundlePath,
  };
};
