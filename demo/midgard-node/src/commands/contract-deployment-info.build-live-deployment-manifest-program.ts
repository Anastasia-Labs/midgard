import {
  type DeploymentManifest,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import {
  bindDeploymentRunStateToMarker,
  defaultDeploymentRunStatePath,
  loadDeploymentRunState,
  mutateDeploymentRunState,
  sha256File,
} from "../e2e/run-state.js";
import { Lucid, MidgardContracts, NodeConfig } from "../services/index.js";
import { fetchProtocolDeploymentStatus } from "../transactions/initialization.js";
import { queryScriptRewardRegistrationProgram } from "../transactions/script-reward-registration.js";
import {
  formatDeploymentManifestVerificationReport,
  type LiveContractDeploymentInfoWriteOptions,
  readReferenceScriptAuthPolicyForLiveWrite,
  resolveLiveContractDeploymentInfoProgram,
  writeContractDeploymentInfoFileProgram,
} from "./contract-deployment-info.build-contract-deployment-info-from-contracts.js";
import {
  buildDeploymentManifest,
  readDeploymentManifestFile,
  verifyDeploymentManifestAgainstConfig,
} from "./contract-deployment-info.build-deployment-manifest.js";
import { buildDeploymentManifestIdentityContextProgram } from "./contract-deployment-info.exact-protocol-parameter-snapshot.js";

const buildLiveDeploymentManifestProgram = (
  outputPath: string,
  options: LiveContractDeploymentInfoWriteOptions = {},
): Effect.Effect<
  DeploymentManifest,
  Error,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const identityContext =
      yield* buildDeploymentManifestIdentityContextProgram;
    const referenceScriptAuthPolicy = yield* Effect.tryPromise({
      try: () => readReferenceScriptAuthPolicyForLiveWrite(outputPath),
      catch: (cause) =>
        new Error(
          `Failed to read existing reference-script auth policy metadata: ${String(cause)}`,
        ),
    });
    const deploymentInfo = yield* resolveLiveContractDeploymentInfoProgram(
      referenceScriptAuthPolicy,
    );
    const existingManifest = yield* Effect.sync(() => {
      try {
        return readDeploymentManifestFile(outputPath);
      } catch {
        return undefined;
      }
    });
    const finalizationRequested =
      options.hubOracleOneShotStatus === "consumed_by_init" &&
      options.steps?.initProtocol?.status === "complete";
    if (existingManifest === undefined && !finalizationRequested) {
      return yield* Effect.fail(
        new Error(
          "A first DeploymentManifestV1 may be created only after initialization and reference-script publication are complete",
        ),
      );
    }
    const lucidService = yield* Lucid;
    const liveContracts = yield* MidgardContracts;
    const availabilityAccounts = yield* Effect.forEach(
      Object.values(liveContracts.availabilityChallenge.yields),
      (validator) =>
        queryScriptRewardRegistrationProgram(
          lucidService.api,
          validator.withdrawalScript,
        ),
    );
    const availabilityRegistered = availabilityAccounts.every(
      (account) => account.registered,
    );
    if (finalizationRequested && !availabilityRegistered) {
      return yield* Effect.fail(
        new Error(
          "Cannot finalize deployment manifest before all availability yield reward accounts are registered",
        ),
      );
    }
    const requestedSteps = finalizationRequested
      ? {
          prepareHubOracleNonce: {
            status: "complete" as const,
            txHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH.toLowerCase(),
          },
          deployNodeRuntimeReferenceScripts: {
            status: "complete" as const,
          },
          ...options.steps,
          availabilityRegistration: { status: "complete" as const },
        }
      : {
          ...options.steps,
          availabilityRegistration: {
            status: availabilityRegistered
              ? ("complete" as const)
              : ("pending" as const),
          },
        };
    const deploymentManifest = buildDeploymentManifest(deploymentInfo, {
      network: nodeConfig.NETWORK,
      ...identityContext,
      referenceScriptDeployAddress:
        nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
      hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
      hubOracleOneShotOutputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
      existingManifest,
      steps: requestedSteps,
      hubOracleOneShotStatus: options.hubOracleOneShotStatus,
    });
    const verification = verifyDeploymentManifestAgainstConfig(
      deploymentManifest,
      {
        network: nodeConfig.NETWORK,
        referenceScriptDeployAddress:
          nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
        hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
        hubOracleOneShotOutputIndex:
          nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
        economicsProfile: nodeConfig.DEPLOYMENT_ECONOMICS_PROFILE,
        path: outputPath,
      },
    );
    if (!verification.ok) {
      return yield* Effect.fail(
        new Error(
          `Refusing to write deployment manifest with configuration drift: ${formatDeploymentManifestVerificationReport(
            verification,
          )}`,
        ),
      );
    }
    return deploymentManifest;
  });

export const writeLiveContractDeploymentInfoProgram = (
  outputPath: string,
  options: LiveContractDeploymentInfoWriteOptions = {},
): Effect.Effect<string, Error, Lucid | MidgardContracts | NodeConfig> =>
  Effect.gen(function* () {
    const deploymentManifest = yield* buildLiveDeploymentManifestProgram(
      outputPath,
      options,
    );
    const marker = makeDeploymentMarker(deploymentManifest.manifestId);
    const runStatePath = defaultDeploymentRunStatePath();
    const runState = yield* Effect.tryPromise({
      try: () => loadDeploymentRunState(runStatePath),
      catch: (cause) =>
        new Error(`Failed to inspect deployment run-state identity`, {
          cause,
        }),
    });
    if (
      runState?.identity.deploymentMarker !== undefined &&
      runState.identity.deploymentMarker.manifestId !== marker.manifestId
    ) {
      return yield* Effect.fail(
        new Error(
          `Refusing to replace final deployment manifest ${runState.identity.deploymentMarker.manifestId} with ${marker.manifestId}; start an explicit fresh deployment run instead`,
        ),
      );
    }
    const manifestPath = yield* writeContractDeploymentInfoFileProgram(
      outputPath,
      deploymentManifest,
    );
    if (runState !== null) {
      const manifestSha256 = yield* Effect.tryPromise({
        try: () => sha256File(manifestPath),
        catch: (cause) =>
          new Error(`Failed to hash final deployment manifest`, { cause }),
      });
      yield* Effect.tryPromise({
        try: () =>
          mutateDeploymentRunState(
            runStatePath,
            () => {
              throw new Error(
                "Deployment run state disappeared before final marker binding",
              );
            },
            (current) =>
              bindDeploymentRunStateToMarker(current, {
                marker,
                manifestPath,
                manifestSha256,
              }),
          ),
        catch: (cause) =>
          new Error(`Failed to bind deployment run state to final manifest`, {
            cause,
          }),
      });
    }
    return manifestPath;
  });

export type ReconcileInitializedDeploymentManifestOptions = {
  readonly outputPath: string;
  readonly initTxHash: string;
};

export type ReconcileInitializedDeploymentManifestSummary = {
  readonly status: "complete";
  readonly path: string;
  readonly manifestId: string;
  readonly initTxHash: string;
  readonly hubOracleOutRef: string;
  readonly referenceScriptsConfirmed: number;
};

export const reconcileInitializedDeploymentManifestProgram = ({
  outputPath,
  initTxHash,
}: ReconcileInitializedDeploymentManifestOptions): Effect.Effect<
  ReconcileInitializedDeploymentManifestSummary,
  Error,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const normalizedInitTxHash = initTxHash.toLowerCase();
    const deploymentStatus = yield* fetchProtocolDeploymentStatus(
      lucidService.api,
      contracts,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new Error("Failed to inspect live protocol deployment status", {
            cause,
          }),
      ),
    );
    if (!deploymentStatus.complete) {
      return yield* Effect.fail(
        new Error(
          `Cannot reconcile deployment manifest for an incomplete protocol deployment: missing_components=[${deploymentStatus.missingComponents.join(
            ",",
          )}]`,
        ),
      );
    }
    const hubOracleWitness = deploymentStatus.hubOracleWitness;
    if (hubOracleWitness === null) {
      return yield* Effect.fail(
        new Error(
          "Cannot reconcile deployment manifest without hub-oracle witness",
        ),
      );
    }
    const liveInitTxHash = hubOracleWitness.txHash.toLowerCase();
    if (liveInitTxHash !== normalizedInitTxHash) {
      return yield* Effect.fail(
        new Error(
          `Init transaction mismatch: live hub-oracle witness was created by ${liveInitTxHash}, expected ${normalizedInitTxHash}`,
        ),
      );
    }

    const path = yield* writeLiveContractDeploymentInfoProgram(outputPath, {
      hubOracleOneShotStatus: "consumed_by_init",
      steps: {
        initProtocol: {
          status: "complete",
          txHash: normalizedInitTxHash,
        },
      },
    });
    const manifest = yield* Effect.try({
      try: () => readDeploymentManifestFile(path),
      catch: (cause) =>
        new Error(
          `Failed to read reconciled deployment manifest: ${String(cause)}`,
        ),
    });
    return {
      status: "complete" as const,
      path,
      manifestId: manifest.manifestId,
      initTxHash: normalizedInitTxHash,
      hubOracleOutRef: `${hubOracleWitness.txHash}#${hubOracleWitness.outputIndex.toString()}`,
      referenceScriptsConfirmed: Object.values(
        manifest.referenceScripts,
      ).filter((record) => record.status === "confirmed").length,
    };
  });
