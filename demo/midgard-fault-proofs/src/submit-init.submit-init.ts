import {
  credentialToAddress,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { parseContractDeploymentInfo } from "./inspect-contracts.js";
import { rejectRetiredUnauthenticatedSubmissionRoute } from "./legacy-submission-boundary.js";
import {
  makeLucidForSubmit,
  readJsonFile,
  requireDeploymentScriptHash,
  type ResolvedProverSigner,
  resolveFaultProofDeploymentContracts,
  resolveProverSigner,
} from "./runtime.js";
import {
  fraudCategoryLabel,
  requireFraudProofCatalogue,
  resolveNonExistentInputNoIndexInit,
  type SubmitInitCliConfig,
  type SubmitInitFraudCategory,
  type SubmitInitResult,
} from "./submit-init.resolve-non-existent-input-no-index-init.js";
import { submitResolvedInit } from "./submit-init.submit-resolved-init.js";
import { type FaultProofWitnessReferenceScripts } from "./witness-reference-scripts.js";
import { type FraudProofPreSubmitBoundary } from "./workflow/transaction-boundary.js";

export const submitInit = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  fraudCategory = "doubleSpend",
  fraudulentBlockOutRef,
  fraudulentHeaderHash,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
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
  /** Production workflow seam: invoked after local evaluation, before I/O. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitInitResult> => {
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
  const result = await submitResolvedInit({
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
    preSubmitBoundary,
    awaitConfirmation,
  });
  return {
    txHash: result.txHash,
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
    awaitedConfirmation: result.awaitedConfirmation,
  };
};

export const submitInitFromFiles = async (
  config: SubmitInitCliConfig,
): Promise<SubmitInitResult> => {
  rejectRetiredUnauthenticatedSubmissionRoute({
    command: "submit-init",
    fraudCategory: config.fraudCategory,
  });
  const [blueprint, deploymentInfo, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitInit({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    fraudCategory: config.fraudCategory,
    fraudulentBlockOutRef: config.fraudulentBlockOutRef,
    fraudulentHeaderHash: config.fraudulentHeaderHash,
    awaitConfirmation: config.awaitConfirmation,
  });
};
