import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import { type FraudProofCatalogueCategoryDeploymentInfo } from "@al-ft/midgard-sdk/core";
import {
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import type { ContractDeploymentInfo } from "./inspect-contracts.inspect-contracts-output.js";
import { parseContractDeploymentInfo } from "./inspect-contracts.parse-contract-deployment-info.js";
import {
  encodePhasMembershipProofRedeemer,
  faultProofCategoryLabel,
  type ProverSignerConfig,
  type ResolvedProverSigner,
  resolveInputNoIdxDeploymentContracts,
  type SubmitProviderConfig,
  type SupportedFaultProofCategoryName,
} from "./runtime.js";
import { type FaultProofWitnessReferenceScripts } from "./witness-reference-scripts.js";
import {
  type LocallyEvaluatedTransaction,
  type OutsideWorkflowSubmission,
} from "./workflow/transaction-boundary.js";

export const PHAS_MEMBERSHIP_WITHDRAW_TITLE = "phas.membership.withdraw";

export type SubmitInitCliConfig = SubmitProviderConfig &
  ProverSignerConfig & {
    readonly blueprintPath: string;
    readonly deploymentInfoPath: string;
    readonly fraudCategory?: SubmitInitFraudCategory;
    readonly fraudulentBlockOutRef: string;
    readonly fraudulentHeaderHash?: string;
    readonly awaitConfirmation?: boolean;
  };

export type SubmitInitFraudCategory = SupportedFaultProofCategoryName;

export type InitBuild = {
  readonly transaction: LocallyEvaluatedTransaction;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly firstStepOutputIndex: number;
  readonly fraudCategoryId: string;
  readonly fraudCategoryName: SubmitInitFraudCategory;
  readonly fraudCategory: string;
  readonly fraudProofCatalogueRoot: string;
};

export type SubmitInitResult = OutsideWorkflowSubmission<InitBuild>;

export const requireFraudProofCatalogue = (
  deploymentInfo: ContractDeploymentInfo,
) => {
  const catalogue = deploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (catalogue === undefined) {
    throw new Error(
      "Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.",
    );
  }
  return catalogue;
};

export const fraudCategoryLabel = (
  category: SubmitInitFraudCategory,
): string => {
  return faultProofCategoryLabel(category);
};

export const encodePhasMembershipRedeemer = ({
  root,
  categoryId,
  categoryScriptHash,
  membershipProofCbor,
}: {
  readonly root: string;
  readonly categoryId: string;
  readonly categoryScriptHash: string;
  readonly membershipProofCbor: string;
}): string =>
  encodePhasMembershipProofRedeemer({
    root,
    keyCbor: Data.to(
      categoryId,
      asLucidSchema(Data.Bytes({ minLength: 4, maxLength: 4 })),
    ),
    valueCbor: Data.to(
      categoryScriptHash,
      asLucidSchema(
        Data.Bytes({
          minLength: 28,
          maxLength: 28,
        }),
      ),
    ),
    membershipProofCbor,
  });

export type ResolvedNonExistentInputNoIndexInit = {
  readonly category: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadMintingScript: Script;
  readonly firstStepAddress: string;
  readonly firstStepHash: string;
};

/**
 * The no-index category is derived from the compiled blueprint
 * like every other family (`buildInputNoIdxFaultProofContracts`) instead of
 * trusting the embedded deployment script bytes. The embedded bytes are still
 * cross-checked, so a deployment whose recorded contract disagrees with the
 * applied chain fails closed rather than initialising a thread nobody can
 * spend.
 */
export const resolveNonExistentInputNoIndexInit = async ({
  blueprint,
  deploymentInfo,
  network,
}: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
}): Promise<ResolvedNonExistentInputNoIndexInit> => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const deployedFirstStep =
    parsedDeploymentInfo.fraudProofNonExistentInputNoIndex;
  if (deployedFirstStep === undefined) {
    throw new Error(
      'Deployment info is missing "fraudProofNonExistentInputNoIndex"',
    );
  }
  if (deployedFirstStep.contract === undefined) {
    throw new Error(
      'Deployment info "fraudProofNonExistentInputNoIndex" is missing embedded contract bytes.',
    );
  }
  const embeddedScript: Script = {
    type: deployedFirstStep.contract.type,
    script: deployedFirstStep.contract.cborHex,
  };
  const embeddedHash = validatorToScriptHash(embeddedScript);
  if (embeddedHash !== deployedFirstStep.scriptHash) {
    throw new Error(
      `fraudProofNonExistentInputNoIndex script hash mismatch: deployment=${deployedFirstStep.scriptHash}, derived=${embeddedHash}.`,
    );
  }
  const resolvedDeployment = await resolveInputNoIdxDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const firstStep =
    resolvedDeployment.contracts.nonExistentInputNoIndex.firstStep;
  if (embeddedHash !== firstStep.spendingScriptHash) {
    throw new Error(
      `fraudProofNonExistentInputNoIndex embedded contract ${embeddedHash} does not match the input-no-idx step-01 script ${firstStep.spendingScriptHash} derived from the blueprint.`,
    );
  }
  return {
    category: resolvedDeployment.nonExistentInputNoIndexCategory,
    stateQueuePolicyId: resolvedDeployment.stateQueuePolicyId!,
    computationThreadPolicyId:
      resolvedDeployment.contracts.computationThread.policyId,
    computationThreadMintingScript:
      resolvedDeployment.contracts.computationThread.mintingScript,
    firstStepAddress: firstStep.spendingScriptAddress,
    firstStepHash: firstStep.spendingScriptHash,
  };
};

/** The contracts an init needs, already resolved from a deployment. */
export type ResolvedInitContracts = {
  readonly steps: readonly [
    {
      readonly spendingScriptAddress: string;
      readonly spendingScriptHash: string;
    },
    ...unknown[],
  ];
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
};

export type ResolvedInitCatalogueCategory = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export type SubmitResolvedInitParams = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: ResolvedInitContracts;
  readonly category: ResolvedInitCatalogueCategory;
  /** The deployed fraud-proof catalogue: its NFT policy, spend address, and MPF root. */
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash?: string;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
};

export type ResolvedInitBuild = {
  readonly transaction: LocallyEvaluatedTransaction;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly firstStepOutputIndex: number;
  /** `txHash#index` of the freshly minted thread, ready for step-01. */
  readonly nextThreadOutRef: string;
  readonly fraudCategoryId: string;
  readonly fraudCategory: string;
  readonly fraudProofCatalogueRoot: string;
};

export type SubmitResolvedInitResult =
  OutsideWorkflowSubmission<ResolvedInitBuild>;

export const STANDARD_INIT_REFERENCE_SCRIPT_ROLES = {
  computationThreadMint: "V1 fraud-proof computation-thread minting",
  phasMembershipWithdraw: "membership proof withdrawal",
} as const;
