import { writeJsonFileAtomic } from "@al-ft/midgard-core/atomic-write";
import {
  type DeploymentManifest,
  deploymentManifestHubOraclePolicyId,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import { manifestDeployableScripts } from "../deployable-scripts.js";
import { publicDeploymentBlueprintRegistry } from "../deployment-blueprint-registry.js";
import { mutateDeploymentRunState } from "../e2e/run-state.mutate-deployment-run-state.js";
import {
  bindDeploymentRunStateToManifest,
  defaultDeploymentRunStatePath,
  loadDeploymentRunState,
  sha256File,
} from "../e2e/run-state.parse-deployment-run-state.js";
import { NodeConfig } from "../services/index.js";
import { Lucid } from "../services/lucid.js";
import { MidgardContracts } from "../services/midgard-contracts.make-midgard-contract-runtime.js";
import { contractsDeploymentReader } from "../transactions/initialization.fetch-protocol-deployment-status.js";
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
import {
  localNodeManifestL1OriginProver,
  runStateManifestL1Origin,
} from "./contract-deployment-info.manifest-l1-origin.js";

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
    // The first manifest's L1 origin needs only the run state and the local
    // node, so a run state that cannot be finalized is refused before the L1
    // provider or the contracts are touched.
    const l1Origin =
      existingManifest === undefined
        ? yield* Effect.scoped(
            Effect.gen(function* () {
              return yield* runStateManifestL1Origin({
                runStatePath:
                  options.runStatePath ?? defaultDeploymentRunStatePath(),
                nonceTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
                prover: yield* localNodeManifestL1OriginProver(nodeConfig),
              });
            }),
          )
        : existingManifest.l1Origin;
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
      ...(l1Origin === undefined ? {} : { l1Origin }),
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
    const hubOraclePolicyId =
      deploymentManifestHubOraclePolicyId(deploymentManifest);
    const runStatePath =
      options.runStatePath ?? defaultDeploymentRunStatePath();
    const runState = yield* Effect.tryPromise({
      try: () => loadDeploymentRunState(runStatePath),
      catch: (cause) =>
        new Error(`Failed to inspect deployment run-state identity`, {
          cause,
        }),
    });
    if (
      runState?.identity.hubOraclePolicyId !== undefined &&
      runState.identity.hubOraclePolicyId !== hubOraclePolicyId
    ) {
      return yield* Effect.fail(
        new Error(
          `Refusing to replace the final deployment with hub-oracle policy id ${runState.identity.hubOraclePolicyId} with ${hubOraclePolicyId}; start an explicit fresh deployment run instead`,
        ),
      );
    }
    const contracts = yield* MidgardContracts;
    const registry = publicDeploymentBlueprintRegistry(
      manifestDeployableScripts(contracts),
    );
    if (registry !== undefined) {
      yield* Effect.tryPromise({
        try: () =>
          writeJsonFileAtomic(`${outputPath}.blueprint-registry.json`, {
            schemaVersion: "midgard-deployment-parameter-registry-v1",
            hubOraclePolicyId,
            blueprintHash: deploymentManifest.artifacts.blueprintHash,
            deployment: registry,
            referenceScripts: deploymentManifest.referenceScripts,
          }),
        catch: (cause) =>
          new Error("Failed to write complete deployment parameter registry", {
            cause,
          }),
      });
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
                "Deployment run state disappeared before final manifest binding",
              );
            },
            (current) =>
              bindDeploymentRunStateToManifest(current, {
                hubOraclePolicyId,
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
  /** The deployment run state (`--run-state`); by default `MIDGARD_RUN_STATE_PATH` or deploymentInfo/midgard-run-state.json. */
  readonly runStatePath?: string;
};

export type ReconcileInitializedDeploymentManifestSummary = {
  readonly status: "complete";
  readonly path: string;
  readonly hubOraclePolicyId: string;
  readonly initTxHash: string;
  readonly hubOracleOutRef: string;
  readonly referenceScriptsConfirmed: number;
};

export const reconcileInitializedDeploymentManifestProgram = ({
  outputPath,
  initTxHash,
  runStatePath,
}: ReconcileInitializedDeploymentManifestOptions): Effect.Effect<
  ReconcileInitializedDeploymentManifestSummary,
  Error,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;
    const normalizedInitTxHash = initTxHash.toLowerCase();
    const deploymentStatus = yield* fetchProtocolDeploymentStatus(
      contractsDeploymentReader(lucidService, contracts),
      {
        nonce: {
          txHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
          outputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
        },
        initAttempts: [{ txHash: normalizedInitTxHash }],
      },
    ).pipe(
      Effect.mapError(
        (cause) =>
          new Error("Failed to inspect live protocol deployment status", {
            cause,
          }),
      ),
    );
    const hubOracleWitness = deploymentStatus.hubOracleWitness;
    if (hubOracleWitness === null) {
      return yield* Effect.fail(
        new Error(
          `Cannot reconcile deployment manifest: initialization ${normalizedInitTxHash} is not visible (status=${deploymentStatus.status}, nonce=${deploymentStatus.nonce})`,
        ),
      );
    }

    const path = yield* writeLiveContractDeploymentInfoProgram(outputPath, {
      ...(runStatePath === undefined ? {} : { runStatePath }),
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
      hubOraclePolicyId: deploymentManifestHubOraclePolicyId(manifest),
      initTxHash: normalizedInitTxHash,
      hubOracleOutRef: `${hubOracleWitness.txHash}#${hubOracleWitness.outputIndex.toString()}`,
      referenceScriptsConfirmed: Object.values(
        manifest.referenceScripts,
      ).filter((record) => record.status === "confirmed").length,
    };
  });
