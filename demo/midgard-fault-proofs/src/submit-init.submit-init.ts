import {
  credentialToAddress,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { parseContractDeploymentInfo } from "./inspect-contracts.js";
import { rejectUnadmittedSubmissionRoute } from "./legacy-submission-boundary.js";
import {
  makeLucidForSubmit,
  readJsonFile,
  requireDeploymentScriptHash,
  type ResolvedProverSigner,
  resolveFaultProofDeploymentContracts,
  resolveProverSigner,
} from "./runtime.js";
import { submitFaultProofTurns } from "./submit-fault-proof-turns.js";
import type { InitBuild } from "./submit-init.resolve-non-existent-input-no-index-init.js";
import {
  fraudCategoryLabel,
  requireFraudProofCatalogue,
  resolveNonExistentInputNoIndexInit,
  type SubmitInitCliConfig,
  type SubmitInitFraudCategory,
  type SubmitInitResult,
} from "./submit-init.resolve-non-existent-input-no-index-init.js";
import { buildResolvedInit } from "./submit-init.submit-resolved-init.js";
import { type FaultProofWitnessReferenceScripts } from "./witness-reference-scripts.js";

export const buildInit = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  fraudCategory = "doubleSpend",
  fraudulentBlockOutRef,
  fraudulentHeaderHash,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly fraudCategory?: SubmitInitFraudCategory;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash?: string;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}): Promise<InitBuild> => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const catalogue = requireFraudProofCatalogue(parsedDeploymentInfo);
  if (fraudCategory === "nonExistentInputNoIndex") {
    const resolvedNoIndex = await resolveNonExistentInputNoIndexInit({
      blueprint,
      deploymentInfo,
      network,
    });
    if (resolvedNoIndex.firstStepHash !== resolvedNoIndex.category.scriptHash) {
      throw new Error(
        `${fraudCategoryLabel(fraudCategory)} first-step script hash mismatch: catalogue=${resolvedNoIndex.category.scriptHash}, derived=${resolvedNoIndex.firstStepHash}.`,
      );
    }
  }
  const resolvedDeployment = await resolveFaultProofDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    categoryName: fraudCategory,
    requireStateQueueMint: true,
  });
  const category = resolvedDeployment.category;
  const selectedContracts = resolvedDeployment.contracts[fraudCategory];
  if (selectedContracts === undefined) {
    throw new Error(
      `${fraudCategoryLabel(fraudCategory)} deployment resolution returned no category contracts.`,
    );
  }
  const firstStep = selectedContracts.firstStep;
  const fraudProofCataloguePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofCatalogueMint",
  );
  const fraudProofCatalogueSpendHash = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofCatalogueSpend",
  );
  const hubOraclePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "hubOracleMint",
  );
  if (firstStep.spendingScriptHash !== category.scriptHash) {
    throw new Error(
      `${fraudCategoryLabel(fraudCategory)} first-step script hash mismatch: catalogue=${category.scriptHash}, derived=${firstStep.spendingScriptHash}.`,
    );
  }
  const result = await buildResolvedInit({
    lucid,
    blueprint,
    network,
    label: fraudCategoryLabel(fraudCategory),
    contracts: {
      steps: [firstStep],
      computationThread: resolvedDeployment.contracts.computationThread,
      hubOraclePolicyId,
      stateQueuePolicyId: resolvedDeployment.stateQueuePolicyId!,
    },
    category,
    catalogue: {
      policyId: fraudProofCataloguePolicyId,
      spendingScriptAddress: credentialToAddress(
        network,
        scriptHashToCredential(fraudProofCatalogueSpendHash),
      ),
      root: catalogue.root,
    },
    signer,
    fraudulentBlockOutRef,
    fraudulentHeaderHash,
    witnessReferenceScripts,
  });
  return {
    transaction: result.transaction,
    walletSource: result.walletSource,
    proverAddress: result.proverAddress,
    fraudProver: result.fraudProver,
    fraudulentBlockOutRef: result.fraudulentBlockOutRef,
    fraudulentHeaderHash: result.fraudulentHeaderHash,
    computationThreadPolicyId: result.computationThreadPolicyId,
    computationThreadAssetName: result.computationThreadAssetName,
    computationThreadUnit: result.computationThreadUnit,
    firstStepAddress: result.firstStepAddress,
    firstStepOutputIndex: result.firstStepOutputIndex,
    fraudCategoryId: result.fraudCategoryId,
    fraudCategoryName: fraudCategory,
    fraudCategory: result.fraudCategory,
    fraudProofCatalogueRoot: result.fraudProofCatalogueRoot,
  };
};

export const submitInitFromFiles = async (
  config: SubmitInitCliConfig,
): Promise<SubmitInitResult> => {
  rejectUnadmittedSubmissionRoute({
    command: "submit-init",
    fraudCategory: config.fraudCategory,
  });
  const [blueprint, deploymentInfo, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitFaultProofTurns(
    buildInit,
    {
      lucid,
      blueprint,
      deploymentInfo,
      network: config.network,
      signer,
      fraudCategory: config.fraudCategory,
      fraudulentBlockOutRef: config.fraudulentBlockOutRef,
      fraudulentHeaderHash: config.fraudulentHeaderHash,
    },
    { label: "init", awaitConfirmation: config.awaitConfirmation ?? true },
  );
};
