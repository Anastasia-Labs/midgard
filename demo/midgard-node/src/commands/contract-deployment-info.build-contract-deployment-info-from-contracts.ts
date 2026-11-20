import { existsSync } from "node:fs";

import { writeJsonFileAtomic } from "@al-ft/midgard-core/atomic-write";
import { type DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import type * as SDKCore from "@al-ft/midgard-sdk/core";
import * as SDK from "@al-ft/midgard-sdk/state-queue";
import { type ReferenceScriptAuthPolicyDeploymentInfo } from "@al-ft/midgard-sdk/state-queue";
import * as SDKUsers from "@al-ft/midgard-sdk/users";
import { type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  defaultDeploymentRunStatePath,
  loadDeploymentRunState,
} from "../e2e/run-state.parse-deployment-run-state.js";
import { Lucid } from "../services/lucid.js";
import { MidgardContracts } from "../services/midgard-contracts.make-midgard-contract-runtime.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "../transactions/initialization.atomic-protocol-init-reference-scripts-from-publications.js";
import {
  normalizeOutputPath,
  readDeploymentManifestFile,
} from "./contract-deployment-info.build-deployment-manifest.js";
import {
  buildReferenceScriptOutRefMap,
  collectScriptDescriptors,
  type ContractDeploymentInfo,
  type ContractDeploymentInfoEntry,
  type ContractDeploymentInfoRefScriptUTxO,
  type DeploymentManifestVerificationReport,
  fetchLiveReferenceScriptUtxos,
} from "./contract-deployment-info.build-reference-script-out-ref-map.js";

export const buildContractDeploymentInfoFromContracts = (
  contracts: SDKCore.MidgardValidators,
  referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
  referenceScriptOutRefs: ReadonlyMap<
    string,
    ContractDeploymentInfoRefScriptUTxO
  > = new Map(),
  fraudProofCatalogue?: SDKCore.FraudProofCatalogueDeploymentInfo,
): ContractDeploymentInfo =>
  Object.freeze({
    referenceScriptAuthPolicy,
    contracts: Object.fromEntries(
      collectScriptDescriptors(contracts, referenceScriptAuthPolicy).map(
        (descriptor) => {
          const history =
            descriptor.name === "fraudProofFabricatedDeposit"
              ? contracts.fraudProofContracts.fabricatedDeposit.history
              : descriptor.name === "fraudProofFabricatedWithdrawal"
                ? contracts.fraudProofContracts.fabricatedWithdrawal.history
                : descriptor.name === "fraudProofTransitionTrace"
                  ? contracts.fraudProofContracts.transitionTrace.history
                  : undefined;
          const historyRecipe =
            descriptor.name === "depositMint"
              ? SDKUsers.requireEventHistoryContracts(contracts).deposit.recipe
              : descriptor.name === "withdrawalMint"
                ? SDKUsers.requireEventHistoryContracts(contracts).withdrawal
                    .recipe
                : undefined;
          return [
            descriptor.name,
            {
              refScriptUTxO:
                referenceScriptOutRefs.get(descriptor.name) ?? null,
              contract: descriptor.contract,
              scriptHash: descriptor.scriptHash,
              ...(historyRecipe === undefined
                ? {}
                : {
                    eventHistoryRecipe: {
                      kind: historyRecipe.kind,
                      hubPolicyId: historyRecipe.hubPolicyId,
                      initializationNonce: {
                        txHash: historyRecipe.initializationNonce.transactionId,
                        outputIndex: Number(
                          historyRecipe.initializationNonce.outputIndex,
                        ),
                      },
                      protectionDurationMs:
                        historyRecipe.protectionDurationMs.toString(),
                      bounds: {
                        inlineLimitBytes:
                          historyRecipe.inlineLimitBytes.toString(),
                        maxPayloadBytes:
                          historyRecipe.maxPayloadBytes.toString(),
                        maxPayloadNodes:
                          historyRecipe.maxPayloadNodes.toString(),
                      },
                    },
                  }),
              ...(history === undefined
                ? {}
                : {
                    ...("retentionAddresses" in history
                      ? {
                          eventHistoryRetentionAddresses:
                            history.retentionAddresses,
                        }
                      : {
                          eventHistoryRetentionAddress:
                            history.retentionAddress,
                        }),
                    eventHistoryBounds: {
                      inlineLimitBytes: history.inlineLimitBytes.toString(),
                      maxPayloadBytes: history.maxPayloadBytes.toString(),
                      maxPayloadNodes: history.maxPayloadNodes.toString(),
                    },
                  }),
              ...(descriptor.name === "fraudProofCatalogueMint" &&
              fraudProofCatalogue !== undefined
                ? { fraudProofCatalogue }
                : {}),
            } satisfies ContractDeploymentInfoEntry,
          ];
        },
      ),
    ),
  });

export const resolveLiveContractDeploymentInfoProgram = (
  referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
): Effect.Effect<ContractDeploymentInfo, Error, Lucid | MidgardContracts> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const descriptors = collectScriptDescriptors(
      contracts,
      referenceScriptAuthPolicy,
    );
    const referenceScriptWalletUtxos = yield* fetchLiveReferenceScriptUtxos();
    const referenceScriptOutRefs = buildReferenceScriptOutRefMap(
      referenceScriptWalletUtxos,
      descriptors,
      referenceScriptAuthPolicy,
    );
    const fraudProofCatalogue = yield* buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(contracts.fraudProofs),
    );
    return buildContractDeploymentInfoFromContracts(
      contracts,
      referenceScriptAuthPolicy,
      referenceScriptOutRefs,
      fraudProofCatalogue,
    );
  });

export const buildContractDeploymentInfoProgram = (
  contracts: SDKCore.MidgardValidators,
  referenceScriptUtxos: readonly UTxO[],
  referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
): Effect.Effect<ContractDeploymentInfo, Error> =>
  Effect.gen(function* () {
    const descriptors = collectScriptDescriptors(
      contracts,
      referenceScriptAuthPolicy,
    );
    const referenceScriptOutRefs = buildReferenceScriptOutRefMap(
      referenceScriptUtxos,
      descriptors,
      referenceScriptAuthPolicy,
    );
    const fraudProofCatalogue = yield* buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(contracts.fraudProofs),
    );
    return buildContractDeploymentInfoFromContracts(
      contracts,
      referenceScriptAuthPolicy,
      referenceScriptOutRefs,
      fraudProofCatalogue,
    );
  });

export const readReferenceScriptAuthPolicyForLiveWrite = async (
  outputPath: string,
): Promise<ReferenceScriptAuthPolicyDeploymentInfo> => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  if (existsSync(resolvedOutputPath)) {
    return readDeploymentManifestFile(resolvedOutputPath)
      .referenceScriptAuthPolicy;
  }
  const runStatePath = defaultDeploymentRunStatePath();
  const runState = await loadDeploymentRunState(runStatePath);
  const policy = runState?.identity.referenceScriptAuthPolicy;
  if (policy === undefined) {
    throw new Error(
      `Deployment run state at "${runStatePath}" is missing identity.referenceScriptAuthPolicy`,
    );
  }
  return SDK.referenceScriptAuthPolicyDeploymentInfo(
    SDK.referenceScriptAuthPolicyFromDeploymentInfo(policy),
  );
};

export const writeContractDeploymentInfoFileProgram = (
  outputPath: string,
  deploymentInfo: ContractDeploymentInfo,
): Effect.Effect<string, Error> =>
  Effect.tryPromise({
    try: async () => {
      const resolvedOutputPath = normalizeOutputPath(outputPath);
      await writeJsonFileAtomic(resolvedOutputPath, deploymentInfo);
      return resolvedOutputPath;
    },
    catch: (cause) =>
      new Error(
        `Failed to write contract deployment info file: ${String(cause)}`,
      ),
  });

export type LiveContractDeploymentInfoWriteOptions = {
  readonly steps?: Partial<DeploymentManifest["steps"]>;
  readonly hubOracleOneShotStatus?: DeploymentManifest["hubOracleOneShot"]["status"];
  /** The deployment run state (`--run-state`); by default `MIDGARD_RUN_STATE_PATH` or deploymentInfo/midgard-run-state.json. */
  readonly runStatePath?: string;
};

export const formatDeploymentManifestVerificationReport = (
  report: DeploymentManifestVerificationReport,
): string =>
  `recommendation=${report.recommendation}; mismatches=[${report.mismatches.join(
    "; ",
  )}]`;
