import { createHash, createPrivateKey } from "node:crypto";
import { readFile } from "node:fs/promises";
import { basename, join, resolve } from "node:path";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import {
  computeDeploymentManifestJsonDigest,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseDeploymentReleaseProvenance } from "@al-ft/midgard-core/deployment-release";
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

import { releaseProgramCommitments } from "../release-program-commitments.js";
import type { StackConfig } from "./config.js";
import { stackPaths } from "./deployment.js";
import { readJsonIfPresent } from "./journal.js";
import type { StackProcesses } from "./process.js";

type ReleaseInput = {
  signingKeyFile: string;
  provenanceFile?: string;
  parameterRegistryFile?: string;
  programCommitments: Record<string, string>;
  fundingProfiles: WatcherWorkflowFundingProfileBody[];
};
/**
 * Reads a release input. `beforeDeployment` reads only what exists before
 * the deployment does: a public stack completes the provenance and names the
 * parameter registry once its deployment has written that registry.
 */
export async function readReleaseInput(
  path: string,
  { beforeDeployment = false }: { beforeDeployment?: boolean } = {},
) {
  const input = (await readJsonIfPresent(path)) as ReleaseInput;
  if (
    !input ||
    Object.keys(input)
      .filter(
        (key) => key !== "provenanceFile" && key !== "parameterRegistryFile",
      )
      .sort()
      .join() !== "fundingProfiles,programCommitments,signingKeyFile" ||
    (Object.prototype.hasOwnProperty.call(input, "provenanceFile") &&
      !input.provenanceFile?.startsWith("/")) ||
    (Object.prototype.hasOwnProperty.call(input, "parameterRegistryFile") &&
      !input.parameterRegistryFile?.startsWith("/")) ||
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
  if (beforeDeployment)
    return {
      input,
      key,
      releaseProvenance: undefined,
      deploymentParameterRegistry: undefined,
    };
  const releaseProvenance =
    input.provenanceFile === undefined
      ? undefined
      : parseDeploymentReleaseProvenance(
          await readJsonIfPresent(input.provenanceFile),
        );
  const deploymentParameterRegistry =
    input.parameterRegistryFile === undefined
      ? undefined
      : await readJsonIfPresent(input.parameterRegistryFile);
  if (
    input.parameterRegistryFile !== undefined &&
    deploymentParameterRegistry === undefined
  )
    throw new Error("Release parameter registry file is missing");
  return { input, key, releaseProvenance, deploymentParameterRegistry };
}
/**
 * The provenance a public operator supplies: every field but
 * evidence.parameterRegistryDigest, which the stack completes from the
 * parameter registry its deployment writes. A supplied digest is kept and
 * must match that registry.
 */
export async function readOperatorProvenance(path: string) {
  const value = (await readJsonIfPresent(path)) as
    | { evidence?: unknown }
    | null
    | undefined;
  if (value === undefined)
    throw new Error(`Release provenance ${path} is missing`);
  const evidence =
    value?.evidence !== null &&
    typeof value?.evidence === "object" &&
    !Array.isArray(value.evidence)
      ? (value.evidence as Record<string, unknown>)
      : {};
  const supplied = evidence.parameterRegistryDigest as string | undefined;
  const complete = (parameterRegistryDigest: string) => {
    try {
      return parseDeploymentReleaseProvenance({
        ...value,
        evidence: { ...evidence, parameterRegistryDigest },
      });
    } catch (error) {
      throw new Error(
        `Release provenance ${path}: ${(error as Error).message}`,
      );
    }
  };
  // Every other field is checked before the deployment exists.
  const provenance = complete(supplied ?? "00".repeat(32));
  return { provenance, suppliedDigest: supplied, complete };
}
/** The parameter registry the deployment step writes beside the finalized manifest. */
export const stackRegistryPath = (config: StackConfig) =>
  join(
    config.nodeRoot,
    "deploymentInfo/contract-deployment-info.json.blueprint-registry.json",
  );
/** The operator's provenance, completed with this deployment's registry digest. */
export const completedProvenancePath = (config: StackConfig) =>
  join(config.runDirectory, "release-provenance.json");
/** The release input a public stack signs with: the operator's, completed for this deployment. */
export const completedReleaseInputPath = (config: StackConfig) =>
  join(config.runDirectory, "release-input.json");
/**
 * Completes a public operator's release input for the deployment that now
 * exists: writes the provenance with the digest of the parameter registry
 * the deployment wrote, once, and a release input naming it and that
 * registry. A resume reuses the completed provenance and refuses one that
 * differs. Returns the completed release input's path.
 */
export async function completePublicReleaseInput(processes: StackProcesses) {
  const { config } = processes;
  const { input } = await readReleaseInput(config.watcher.releaseInput!, {
    beforeDeployment: true,
  });
  const registryPath = stackRegistryPath(config);
  const registry = await readJsonIfPresent(registryPath);
  if (registry === undefined)
    throw new Error(
      `The deployment's parameter registry ${registryPath} is missing`,
    );
  const digest = computeDeploymentManifestJsonDigest(registry);
  const operator = await readOperatorProvenance(input.provenanceFile!);
  if (
    operator.suppliedDigest !== undefined &&
    operator.suppliedDigest !== digest
  )
    throw new Error(
      `Release provenance ${input.provenanceFile} has evidence.parameterRegistryDigest ${operator.suppliedDigest}, but this deployment's parameter registry ${registryPath} has digest ${digest}; leave the field out and the stack writes it`,
    );
  const provenance = operator.complete(digest);
  const provenancePath = completedProvenancePath(config);
  const saved = await readJsonIfPresent(provenancePath);
  if (saved === undefined)
    await writeTextFileAtomicNoReplace(
      provenancePath,
      `${JSON.stringify(provenance, null, 2)}\n`,
    );
  else if (
    canonicalJson(saved, "completed provenance") !==
    canonicalJson(provenance, "completed provenance")
  )
    throw new Error(
      `${provenancePath} holds the provenance this stack completed earlier, which differs from ${input.provenanceFile} completed for this deployment; restore the provenance it was completed from`,
    );
  const completed = completedReleaseInputPath(config);
  await writeTextFileAtomic(
    completed,
    `${JSON.stringify(
      {
        ...input,
        provenanceFile: provenancePath,
        parameterRegistryFile: registryPath,
      },
      null,
      2,
    )}\n`,
    { mode: 0o600 },
  );
  return completed;
}
/**
 * Prerequisites, before anything is spent: a fresh setup's release input.
 * A public stack's deployment does not exist yet, so its provenance is read
 * without the parameter registry digest and checked against the blueprint
 * just built.
 */
export async function checkFreshReleaseInput(processes: StackProcesses) {
  const paths = await releasePaths(processes);
  if ((await readJsonIfPresent(paths.authority)) !== undefined) return;
  const path = processes.config.watcher.releaseInput;
  if (!path) throw new Error("Fresh setup needs watcher release inputs");
  if (processes.config.public === undefined) {
    await readReleaseInput(path);
    return;
  }
  const { input } = await readReleaseInput(path, { beforeDeployment: true });
  const { provenance } = await readOperatorProvenance(input.provenanceFile!);
  const blueprint = stackBlueprintPath(processes.config);
  const built = createHash("sha256")
    .update(await readFile(blueprint))
    .digest("hex");
  if (provenance.blueprintHash !== built)
    throw new Error(
      `Release provenance ${input.provenanceFile} has blueprintHash ${provenance.blueprintHash}, but the blueprint this stack built, ${blueprint}, has ${built}`,
    );
}
/** The blueprint the stack built and deployed, in the checkout above the node directory. */
export const stackBlueprintPath = (config: StackConfig) =>
  resolve(config.nodeRoot, "../../onchain/aiken/plutus.json");
/** The payment key hash of the prover wallet, which funds the watcher's proofs. */
export const proverFundingKeyHash = (processes: StackProcesses) =>
  paymentCredentialOf(
    walletFromSeed(processes.env[processes.config.wallets.prover!.seedEnv]!, {
      network: "Preprod",
    }).address,
  ).hash;
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
    provenance: join(directory, "release-provenance.json"),
    parameterRegistry: join(directory, "deployment-parameters.json"),
    deploymentInfo: join(
      directory,
      basename(template.faultProofInfrastructure.deploymentInfoPath),
    ),
  };
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
  await loadWatcherWorkflowFundingProfileOverlay({
    bundlePath: paths.funding,
    deploymentIdentity: release.deploymentIdentity,
  });
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
        "Fresh setup requires watcher releaseInput with a persistent signing key",
      );
    return verifyStackRelease(processes);
  }
  // A public stack signs with its operator's input completed for this deployment.
  const { input, key, releaseProvenance, deploymentParameterRegistry } =
    await readReleaseInput(
      processes.config.public === undefined
        ? processes.config.watcher.releaseInput
        : await completePublicReleaseInput(processes),
    );
  const manifest = await readJsonIfPresent(stackPaths(processes).manifest);
  // A saved authority is reopened only when it attests exactly this release.
  const { deploymentAuthority } = await authorWatcherDeploymentRelease({
    manifest,
    blueprintJson: await readFile(stackBlueprintPath(processes.config)),
    // The derivation the release signing command signs, so both sign the same release.
    programCommitments: releaseProgramCommitments(
      input.programCommitments,
      verifyFinalizedDeploymentManifest(manifest),
    ),
    ...(releaseProvenance === undefined ? {} : { releaseProvenance }),
    ...(deploymentParameterRegistry === undefined
      ? {}
      : { deploymentParameterRegistry }),
    fundingProfiles: input.fundingProfiles,
    fundingPaymentKeyHash: proverFundingKeyHash(processes),
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
  return deploymentAuthority;
}
