import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { resolve as resolvePath } from "node:path";

import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
} from "@al-ft/midgard-core/consensus-profile";
import {
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  type DeploymentManifest,
  type DeploymentManifestEconomicsProfile,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";
import { Effect } from "effect";

import {
  computeDeploymentManifestId,
  DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  parseDeploymentManifestValue,
} from "../deployment-manifest.js";
import { contractDeploymentInfoPathOverride } from "../environment.js";
import { NodeConfig } from "../services/index.js";
import {
  buildReferenceScriptRecords,
  type ContractDeploymentInfo,
  DEFAULT_CONTRACT_DEPLOYMENT_INFO_DIRECTORY_NAME,
  DEFAULT_CONTRACT_DEPLOYMENT_INFO_FILENAME,
  defaultSteps,
  type DeploymentManifestBuildContext,
  type DeploymentManifestVerificationReport,
  type FinalizedDeploymentIdentity,
  resolvePackageRootFromModuleUrl,
} from "./contract-deployment-info.build-reference-script-out-ref-map.js";
import { assertOutRefFields } from "./contract-deployment-info.exact-protocol-parameter-snapshot.js";

const withManifestId = (
  manifest: Omit<DeploymentManifest, "manifestId">,
): DeploymentManifest => ({
  ...manifest,
  manifestId: computeDeploymentManifestId(manifest),
});

/**
 * Builds the sole canonical V1 manifest and re-parses it before return so
 * missing contracts, tuple drift, and dispute-schedule drift fail closed.
 */

export const buildDeploymentManifest = (
  deploymentInfo: ContractDeploymentInfo,
  context: DeploymentManifestBuildContext,
): DeploymentManifest => {
  assertOutRefFields(
    context.hubOracleOneShotTxHash,
    context.hubOracleOneShotOutputIndex,
  );
  const nowIso = (context.now ?? new Date()).toISOString();
  const referenceScripts = buildReferenceScriptRecords(deploymentInfo);
  const hubOracleOneShotStatus =
    context.hubOracleOneShotStatus ??
    context.existingManifest?.hubOracleOneShot.status;
  if (hubOracleOneShotStatus !== "consumed_by_init") {
    throw new Error(
      "Cannot finalize DeploymentManifestV1 before the hub-oracle one-shot is consumed by initialization",
    );
  }
  const baseSteps = {
    ...defaultSteps(),
    prepareHubOracleNonce: { status: "complete" as const },
    deployNodeRuntimeReferenceScripts: {
      status: "complete" as const,
    },
    ...(context.existingManifest?.steps ?? {}),
    ...(context.steps ?? {}),
  };
  const manifest = withManifestId({
    schemaVersion: DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    network: context.network,
    deploymentProfile: SELECTED_DEPLOYMENT_PROFILE,
    deploymentProfileDigest: SELECTED_DEPLOYMENT_PROFILE_DIGEST,
    cardanoProtocolParameters: context.cardanoProtocolParameters,
    genesis: context.genesis,
    createdAt: context.existingManifest?.createdAt ?? nowIso,
    updatedAt: context.existingManifest?.updatedAt ?? nowIso,
    referenceScriptDeployAddress: context.referenceScriptDeployAddress,
    hubOracleOneShot: {
      txHash: context.hubOracleOneShotTxHash.toLowerCase(),
      outputIndex: context.hubOracleOneShotOutputIndex,
      outRef: `${context.hubOracleOneShotTxHash.toLowerCase()}#${context.hubOracleOneShotOutputIndex.toString()}`,
      status: hubOracleOneShotStatus,
    },
    referenceScriptAuthPolicy: deploymentInfo.referenceScriptAuthPolicy,
    contracts: deploymentInfo.contracts,
    referenceScripts,
    da: context.da,
    artifacts: context.artifacts,
    steps: baseSteps,
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
    },
    l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
    economics: context.economics,
    availabilityChallenge: context.availabilityChallenge,
  });
  return parseDeploymentManifestValue(manifest);
};

export const parseDeploymentManifest = (value: unknown): DeploymentManifest =>
  parseDeploymentManifestValue(value);

export const readDeploymentManifestFile = (
  outputPath: string,
): DeploymentManifest => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  const parsed = JSON.parse(readFileSync(resolvedOutputPath, "utf8"));
  return parseDeploymentManifest(parsed);
};

export const readFinalizedDeploymentIdentity = (
  outputPath: string,
): FinalizedDeploymentIdentity => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  const raw = readFileSync(resolvedOutputPath);
  const parsed = JSON.parse(raw.toString("utf8"));
  const manifest = parseDeploymentManifest(parsed);
  return {
    path: resolvedOutputPath,
    manifestId: manifest.manifestId,
    contractDeploymentInfoSha256: createHash("sha256")
      .update(raw)
      .digest("hex"),
    manifest,
  };
};

export const verifyDeploymentManifestAgainstConfig = (
  manifest: DeploymentManifest,
  context: {
    readonly network: string;
    readonly referenceScriptDeployAddress: string;
    readonly hubOracleOneShotTxHash: string;
    readonly hubOracleOneShotOutputIndex: number;
    readonly economicsProfile: DeploymentManifestEconomicsProfile;
    readonly path?: string;
  },
): DeploymentManifestVerificationReport => {
  const mismatches: string[] = [];
  if (manifest.network !== context.network) {
    mismatches.push(
      `network manifest=${manifest.network} config=${context.network}`,
    );
  }
  if (
    manifest.referenceScriptDeployAddress !==
    context.referenceScriptDeployAddress
  ) {
    mismatches.push(
      `referenceScriptDeployAddress manifest=${manifest.referenceScriptDeployAddress} config=${context.referenceScriptDeployAddress}`,
    );
  }
  if (
    manifest.hubOracleOneShot.txHash !==
    context.hubOracleOneShotTxHash.toLowerCase()
  ) {
    mismatches.push(
      `hubOracleOneShot.txHash manifest=${manifest.hubOracleOneShot.txHash} config=${context.hubOracleOneShotTxHash}`,
    );
  }
  if (
    manifest.hubOracleOneShot.outputIndex !==
    context.hubOracleOneShotOutputIndex
  ) {
    mismatches.push(
      `hubOracleOneShot.outputIndex manifest=${manifest.hubOracleOneShot.outputIndex.toString()} config=${context.hubOracleOneShotOutputIndex.toString()}`,
    );
  }
  if (manifest.economics.profile !== context.economicsProfile) {
    mismatches.push(
      `economics.profile manifest=${manifest.economics.profile} config=${context.economicsProfile}`,
    );
  }
  return {
    ok: mismatches.length === 0,
    manifestId: manifest.manifestId,
    path: context.path,
    mismatches,
    recommendation:
      mismatches.length === 0 ? "attach" : "correct_attach_config",
  };
};

export const configuredContractDeploymentInfoPath = (): string => {
  const configuredPath = contractDeploymentInfoPathOverride();
  return configuredPath === undefined
    ? defaultContractDeploymentInfoOutputPath()
    : normalizeOutputPath(configuredPath);
};

export const verifyConfiguredDeploymentManifestProgram: Effect.Effect<
  DeploymentManifestVerificationReport,
  Error,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const path = configuredContractDeploymentInfoPath();
  const manifest = yield* Effect.try({
    try: () => readDeploymentManifestFile(path),
    catch: (cause) =>
      new Error(
        `Failed to read V1 deployment manifest at ${path}: ${String(cause)}`,
      ),
  });
  return verifyDeploymentManifestAgainstConfig(manifest, {
    network: nodeConfig.NETWORK,
    referenceScriptDeployAddress: nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
    hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
    hubOracleOneShotOutputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
    economicsProfile: nodeConfig.DEPLOYMENT_ECONOMICS_PROFILE,
    path,
  });
});

export const verifyConfiguredDeploymentManifestIfPresentProgram: Effect.Effect<
  DeploymentManifestVerificationReport | null,
  Error,
  NodeConfig
> = Effect.gen(function* () {
  const path = configuredContractDeploymentInfoPath();
  if (!existsSync(path)) {
    return null;
  }
  return yield* verifyConfiguredDeploymentManifestProgram;
});

export const defaultContractDeploymentInfoOutputPath = (): string =>
  resolvePath(
    resolvePackageRootFromModuleUrl(import.meta.url),
    DEFAULT_CONTRACT_DEPLOYMENT_INFO_DIRECTORY_NAME,
    DEFAULT_CONTRACT_DEPLOYMENT_INFO_FILENAME,
  );

export const normalizeOutputPath = (outputPath: string): string => {
  const normalized = outputPath.trim();
  if (normalized.length === 0) {
    throw new Error("Contract deployment info output path must not be empty.");
  }
  return resolvePath(normalized);
};
