import {
  createHash,
  createPrivateKey,
  createPublicKey,
  sign,
} from "node:crypto";
import { readFile } from "node:fs/promises";
import { basename, join, resolve } from "node:path";

import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  makeDeploymentMarker,
  parseDeploymentManifestEconomics,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { computeFraudProofReleaseEconomicsPolicyDigest } from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { paymentCredentialOf, walletFromSeed } from "@lucid-evolution/lucid";
import { writeTextFileAtomic } from "midgard-node/files/atomic-write";
import {
  computeWatcherRuleBundleCommitment,
  createWatcherWorkflowFundingProfileBundle,
  loadWatcherVerifiedDeploymentAuthority,
  loadWatcherWorkflowFundingProfileOverlay,
  makeWatcherCanonicalRuleBundle,
  makeWatcherDeploymentIdentitySignaturePayload,
  parseWatcherProcessConfig,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
  type WatcherDeploymentIdentityPolicy,
  type WatcherWorkflowFundingProfileBody,
} from "midgard-watcher";

import { stackPaths } from "./deployment.js";
import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import type { StackProcesses } from "./process.js";

type ReleaseInput = {
  signingKeyFile: string;
  programCommitments: Record<string, string>;
  fundingProfiles: WatcherWorkflowFundingProfileBody[];
};
export async function readReleaseInput(path: string) {
  const input = (await readJsonIfPresent(path)) as ReleaseInput;
  if (
    !input ||
    Object.keys(input).sort().join() !==
      "fundingProfiles,programCommitments,signingKeyFile" ||
    !input.signingKeyFile?.startsWith("/") ||
    !Array.isArray(input.fundingProfiles)
  )
    throw new Error("Invalid watcher release inputs");
  const key = createPrivateKey(await readFile(input.signingKeyFile));
  if (key.asymmetricKeyType !== "ed25519")
    throw new Error("Release signing key must be a persistent ed25519 key");
  createWatcherWorkflowFundingProfileBundle({
    profiles: input.fundingProfiles,
  });
  const categories = input.fundingProfiles.map((profile) =>
    profile.scope.kind === "fraud_proof_category" ? profile.scope.category : "",
  );
  if (
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.some(
      (category) => !categories.includes(category),
    )
  )
    throw new Error(
      "Release inputs need measured funding profiles for every launch category",
    );
  return { input, key };
}
export async function releasePaths(processes: StackProcesses) {
  const template = parseWatcherProcessConfig(
    await readJsonIfPresent(processes.config.watcher.processTemplate),
  );
  const directory = processes.config.watcher.releaseDirectory;
  return {
    authority: join(directory, basename(template.deploymentAuthorityPath)),
    rules: join(directory, basename(template.ruleBundlePath)),
    funding: join(directory, basename(template.fundingProfileBundlePath)),
    manifest: join(
      directory,
      basename(template.faultProofInfrastructure.manifestPath),
    ),
    blueprint: join(
      directory,
      basename(template.faultProofInfrastructure.blueprintPath),
    ),
    deploymentInfo: join(
      directory,
      basename(template.faultProofInfrastructure.deploymentInfoPath),
    ),
  };
}
export async function verifyStackRelease(processes: StackProcesses) {
  const paths = await releasePaths(processes);
  const release = await loadWatcherVerifiedDeploymentAuthority({
    path: paths.authority,
    ruleBundlePath: paths.rules,
  });
  const manifest = verifyFinalizedDeploymentManifest(
    await readJsonIfPresent(stackPaths(processes).manifest),
  );
  if (release.deploymentIdentity.manifestId !== manifest.manifestId)
    throw new Error("Watcher release belongs to a different deployment");
  if (processes.config.watcher.releaseInput) {
    const { key, input } = await readReleaseInput(
      processes.config.watcher.releaseInput,
    );
    const expectedRoot = createHash("sha256")
      .update(createPublicKey(key).export({ format: "der", type: "spki" }))
      .digest("hex");
    const expectedFunding = createWatcherWorkflowFundingProfileBundle({
      profiles: input.fundingProfiles,
    });
    if (
      expectedFunding.fundingProfileBundleDigest !==
      release.deploymentIdentity.fundingProfileBundleDigest
    )
      throw new Error(
        "Measured funding profiles differ from the saved signed release",
      );
    if (release.deploymentIdentity.trustRootId !== expectedRoot)
      throw new Error("Saved release does not use the configured signing key");
  }
  const overlay = await loadWatcherWorkflowFundingProfileOverlay({
    bundlePath: paths.funding,
    deploymentIdentity: release.deploymentIdentity,
  });
  if (
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.some(
      (category) => !overlay.profiles[category],
    )
  )
    throw new Error("Release lacks a launch-category funding profile");
  return release;
}
/** Signs supplied measurements; never substitutes fabricated or empty profiles. */
export async function prepareStackRelease(processes: StackProcesses) {
  const paths = await releasePaths(processes);
  if ((await readJsonIfPresent(paths.authority)) !== undefined)
    return verifyStackRelease(processes);
  if (!processes.config.watcher.releaseInput)
    throw new Error(
      "Fresh setup requires watcher releaseInput with measured profiles and a persistent signing key",
    );
  const { input, key } = await readReleaseInput(
    processes.config.watcher.releaseInput,
  );
  const manifest = verifyFinalizedDeploymentManifest(
    await readJsonIfPresent(stackPaths(processes).manifest),
  );
  const blueprintJson = await readFile(
    resolve(processes.config.nodeRoot, "../../onchain/aiken/plutus.json"),
  );
  const blueprintHash = createHash("sha256")
    .update(blueprintJson)
    .digest("hex");
  if (blueprintHash !== manifest.artifacts.blueprintHash)
    throw new Error("Blueprint differs from deployed release");
  const economics = parseDeploymentManifestEconomics(manifest.economics);
  const economicsPolicyDigest = computeFraudProofReleaseEconomicsPolicyDigest({
    profile: economics.profile,
    requiredBondLovelace: String(economics.requiredBondLovelace),
    slashingPenaltyLovelace: String(economics.slashingPenaltyLovelace),
    fraudProverRewardLovelace: String(economics.fraudProverRewardLovelace),
    inactivitySlashingPenaltyLovelace: String(
      economics.inactivitySlashingPenaltyLovelace,
    ),
    proverCollateralFloorLovelace: String(
      economics.proverCollateralFloorLovelace,
    ),
  });
  const fundingPaymentKeyHash = paymentCredentialOf(
    walletFromSeed(processes.env[processes.config.wallets.prover!.seedEnv]!, {
      network: "Preprod",
    }).address,
  ).hash;
  const hashes = new Set(
    Object.values(manifest.referenceScripts).map(
      (reference) => reference!.scriptHash,
    ),
  );
  for (const profile of input.fundingProfiles) {
    if (
      profile.blueprintSha256 !== blueprintHash ||
      profile.protocolParametersDigest !==
        manifest.cardanoProtocolParameters.digest ||
      profile.economicsPolicyDigest !== economicsPolicyDigest ||
      profile.fundingPaymentKeyHash !== fundingPaymentKeyHash ||
      profile.actions.some((action) =>
        action.referenceInputs.some(
          (reference) =>
            reference.scriptHash !== null && !hashes.has(reference.scriptHash),
        ),
      )
    )
      throw new Error(
        "Measured funding profiles differ from this deployment or prover wallet",
      );
  }
  const funding = createWatcherWorkflowFundingProfileBundle({
    profiles: input.fundingProfiles,
  });
  const rules = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: manifest.manifestId,
      network: manifest.network,
      blueprintHash,
      programCommitments: input.programCommitments,
    },
    targetParameterSnapshot: manifest.cardanoProtocolParameters.snapshot,
  });
  const ruleBundleCommitment = computeWatcherRuleBundleCommitment(rules);
  const bindings = {
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
    ruleBundleCommitment,
    programCommitments: input.programCommitments,
    fundingProfileBundleDigest: funding.fundingProfileBundleDigest,
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestJsonDigest(manifest.da),
    },
    artifacts: { blueprintHash },
  };
  const publicKeySpkiDerHex = createPublicKey(key)
    .export({ format: "der", type: "spki" })
    .toString("hex");
  const trustRootId = createHash("sha256")
    .update(Buffer.from(publicKeySpkiDerHex, "hex"))
    .digest("hex");
  const signedIdentity = {
    schemaVersion: WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
    manifest,
    releaseBindings: bindings,
    attestation: {
      algorithm: "ed25519",
      trustRootId,
      signature: sign(
        null,
        makeWatcherDeploymentIdentitySignaturePayload(
          manifest.manifestId,
          bindings,
        ),
        key,
      ).toString("hex"),
    },
  };
  const catalogue =
    manifest.contracts.fraudProofCatalogueMint!.fraudProofCatalogue!;
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
      Object.entries(manifest.referenceScripts).map(([role, reference]) => [
        role,
        { scriptHash: reference!.scriptHash, outRef: reference!.outRef },
      ]),
    ),
    fraudProofCatalogue: {
      root: catalogue.root,
      categories: Object.fromEntries(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => [
          category,
          {
            categoryId: catalogue.categories[category]!.categoryId,
            scriptHash: catalogue.categories[category]!.scriptHash,
          },
        ]),
      ) as WatcherDeploymentIdentityPolicy["fraudProofCatalogue"]["categories"],
    },
    ruleBundleCommitment,
    programCommitments: input.programCommitments,
    fundingProfileBundleDigest: funding.fundingProfileBundleDigest,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: bindings.da.identityDigest,
    blueprintHash,
  };
  await writeDurableJson(paths.rules, rules);
  await writeDurableJson(paths.manifest, manifest);
  await writeDurableJson(paths.deploymentInfo, manifest);
  await writeTextFileAtomic(paths.blueprint, blueprintJson, { mode: 0o600 });
  // Funding bundles require canonical bytes, including no trailing newline.
  await writeTextFileAtomic(paths.funding, funding.fundingProfileBundleBytes, {
    mode: 0o600,
  });
  // Publish the signed authority only after every bound artifact is durable.
  await writeDurableJson(paths.authority, {
    signedIdentity,
    policy,
    trustRoots: [{ trustRootId, publicKeySpkiDerHex }],
    durableMarker: makeDeploymentMarker(manifest.manifestId),
  });
  return verifyStackRelease(processes);
}
