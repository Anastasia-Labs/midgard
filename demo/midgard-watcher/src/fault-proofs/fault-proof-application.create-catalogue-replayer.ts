import {
  bindFraudProofWorkflowDeployment,
  createCatalogueCompleteCanonicalReplay,
  type ValidationTraceReplayHold,
} from "@al-ft/midgard-fault-proofs";
import {
  FabricatedDepositStep02Datum,
  FabricatedDepositStep03Datum,
  FabricatedDepositStep04Datum,
  FabricatedWithdrawalStep02Datum,
  FabricatedWithdrawalStep03Datum,
  FabricatedWithdrawalStep04Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network } from "@lucid-evolution/lucid";

import {
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentProtocolScriptAuthority,
} from "../runtime/deployment-identity.js";
import { watcherManifestL1SlotConfig } from "./fault-proof-application.bind-watcher-deployment-authority.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "./fault-proof-application.production-dependencies.js";

/**
 * The classifier's replay of every installed category, bound to the loaded
 * deployment: the fabricated-event histories and the slot mapping come from
 * its manifest. `onReplayStuck` is told each time a script re-execution keeps
 * failing the same way.
 */
export const createWatcherCatalogueReplayer = async (input: {
  readonly manifestJson: string;
  readonly blueprintJson: string;
  readonly deploymentInfoJson: string;
  readonly headerHash: string;
  readonly owner: string;
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly onReplayStuck?: (hold: ValidationTraceReplayHold) => void;
}) => {
  const bind = (
    category: "fabricatedDeposit" | "fabricatedWithdrawal",
    stepDatumSchemas: Parameters<
      typeof bindFraudProofWorkflowDeployment
    >[0]["stepDatumSchemas"],
  ) =>
    bindFraudProofWorkflowDeployment({
      manifest: JSON.parse(input.manifestJson),
      blueprintJson: input.blueprintJson,
      deploymentInfo: JSON.parse(input.deploymentInfoJson),
      category,
      headerHash: input.headerHash,
      proverCredential: input.owner,
      stepDatumSchemas,
    });
  const [depositBinding, withdrawalBinding] = await Promise.all([
    bind("fabricatedDeposit", [
      FraudProofComputationThreadStepDatum,
      FabricatedDepositStep02Datum,
      FabricatedDepositStep03Datum,
      FabricatedDepositStep04Datum,
    ]),
    bind("fabricatedWithdrawal", [
      FraudProofComputationThreadStepDatum,
      FabricatedWithdrawalStep02Datum,
      FabricatedWithdrawalStep03Datum,
      FabricatedWithdrawalStep04Datum,
    ]),
  ]);
  const depositHistory =
    depositBinding.resolvedContracts.contracts.fabricatedDeposit?.history;
  const withdrawalHistory =
    withdrawalBinding.resolvedContracts.contracts.fabricatedWithdrawal?.history;
  if (
    depositBinding.deploymentFingerprint !==
      input.deploymentIdentity.manifestId ||
    withdrawalBinding.deploymentFingerprint !==
      input.deploymentIdentity.manifestId ||
    depositHistory === undefined ||
    withdrawalHistory === undefined
  )
    throw new Error("Event replay history differs from the applied deployment");
  const replayer = createCatalogueCompleteCanonicalReplay({
    lucid: input.lucid,
    l1SlotConfig: watcherManifestL1SlotConfig(
      JSON.parse(input.manifestJson),
      input.lucid,
    ),
    ...(input.onReplayStuck === undefined
      ? {}
      : { onReplayStuck: input.onReplayStuck }),
    network: input.network,
    hubOraclePolicyId: watcherDeploymentProtocolScriptAuthority(
      input.deploymentIdentity,
    ).protocolScriptHashes.hubOracleMint,
    minimumConfirmationDepth: 1,
    owner: input.owner,
    history: { deposit: depositHistory, withdrawal: withdrawalHistory },
  });
  if (
    replayer.launchScope.length !==
      WATCHER_INSTALLED_WORKFLOW_CATEGORIES.length ||
    replayer.launchScope.some(
      (category, index) =>
        category !== WATCHER_INSTALLED_WORKFLOW_CATEGORIES[index],
    )
  )
    throw new Error(
      "Watcher classifier differs from its exact installed workflow catalogue",
    );
  return replayer;
};
