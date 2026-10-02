import { createPrivateKey } from "node:crypto";
import { readFile } from "node:fs/promises";
import { basename, join, resolve } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { paymentCredentialOf, walletFromSeed } from "@lucid-evolution/lucid";
import {
  writeTextFileAtomic,
  writeTextFileAtomicNoReplace,
} from "midgard-node/files/atomic-write";
import {
  authorWatcherDeploymentRelease,
  createWatcherWorkflowFundingProfileBundle,
  loadWatcherVerifiedDeploymentAuthority,
  loadWatcherWorkflowFundingProfileOverlay,
  parseWatcherProcessConfig,
  type WatcherWorkflowFundingProfileBody,
} from "midgard-watcher";

import { stackPaths } from "./deployment.js";
import { readJsonIfPresent } from "./journal.js";
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
/** Every launch category needs a measured funding profile in the saved release. */
function assertLaunchCategoryProfiles(overlay: {
  profiles: Readonly<Record<string, unknown>>;
}) {
  if (
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.some(
      (category) => !overlay.profiles[category],
    )
  )
    throw new Error("Release lacks a launch-category funding profile");
}
/**
 * Opens the saved release for this deployment. Setup checks it against the
 * release inputs through the watcher's own authoring (prepareStackRelease).
 */
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
  assertLaunchCategoryProfiles(
    await loadWatcherWorkflowFundingProfileOverlay({
      bundlePath: paths.funding,
      deploymentIdentity: release.deploymentIdentity,
    }),
  );
  return release;
}
/**
 * Signs supplied measurements, or reopens the release they already signed;
 * never substitutes fabricated or empty profiles. Without release inputs only
 * existing signed artifacts are used.
 */
export async function prepareStackRelease(processes: StackProcesses) {
  const paths = await releasePaths(processes);
  if (!processes.config.watcher.releaseInput) {
    if ((await readJsonIfPresent(paths.authority)) === undefined)
      throw new Error(
        "Fresh setup requires watcher releaseInput with measured profiles and a persistent signing key",
      );
    return verifyStackRelease(processes);
  }
  const { input, key } = await readReleaseInput(
    processes.config.watcher.releaseInput,
  );
  // A saved authority is reopened only when it attests exactly this release.
  const { deploymentAuthority, fundingProfileOverlay } =
    await authorWatcherDeploymentRelease({
      manifest: await readJsonIfPresent(stackPaths(processes).manifest),
      blueprintJson: await readFile(
        resolve(processes.config.nodeRoot, "../../onchain/aiken/plutus.json"),
      ),
      programCommitments: input.programCommitments,
      fundingProfiles: input.fundingProfiles,
      fundingPaymentKeyHash: paymentCredentialOf(
        walletFromSeed(
          processes.env[processes.config.wallets.prover!.seedEnv]!,
          { network: "Preprod" },
        ).address,
      ).hash,
      signingKey: key,
      paths,
      existingAuthority: "refuse",
      writer: {
        replace: (path, contents) =>
          writeTextFileAtomic(path, contents, { mode: 0o600 }),
        create: (path, contents) =>
          writeTextFileAtomicNoReplace(path, contents, { mode: 0o600 }),
      },
    });
  assertLaunchCategoryProfiles(fundingProfileOverlay);
  return deploymentAuthority;
}
