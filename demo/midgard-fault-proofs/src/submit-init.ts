import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import {
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  type FraudProofCatalogueCategoryDeploymentInfo,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  HUB_ORACLE_ASSET_NAME,
  Proof,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  type ContractDeploymentInfo,
  parseContractDeploymentInfo,
} from "./inspect-contracts.js";
import { rejectRetiredUnauthenticatedSubmissionRoute } from "./legacy-submission-boundary.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodePhasMembershipProofRedeemer,
  faultProofCategoryLabel,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmit,
  parseOutRef,
  phasMembershipRewardAddress,
  type ProverSignerConfig,
  readJsonFile,
  requireDeploymentScriptHash,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFaultProofDeploymentContracts,
  resolveFraudulentHeaderHash,
  resolveInputNoIdxDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
  type SupportedFaultProofCategoryName,
} from "./runtime.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessWithdrawalValidatorCarriage,
} from "./witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "./workflow/transaction-boundary.js";

const PHAS_MEMBERSHIP_WITHDRAW_TITLE = "phas.membership.withdraw";

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

export type SubmitInitResult = {
  readonly txHash: string;
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
  readonly awaitedConfirmation: boolean;
};

const requireFraudProofCatalogue = (deploymentInfo: ContractDeploymentInfo) => {
  const catalogue = deploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (catalogue === undefined) {
    throw new Error(
      "Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.",
    );
  }
  return catalogue;
};

const fraudCategoryLabel = (category: SubmitInitFraudCategory): string => {
  return faultProofCategoryLabel(category);
};

const encodePhasMembershipRedeemer = ({
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
 * Q13/F20-01: the no-index category is now derived from the compiled blueprint
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
  const stateQueuePolicyId = resolvedDeployment.stateQueuePolicyId!;
  const computationThreadPolicyId =
    resolvedDeployment.contracts.computationThread.policyId;
  const computationThreadMintingScript =
    resolvedDeployment.contracts.computationThread.mintingScript;
  const selectedContracts = resolvedDeployment.contracts[fraudCategory];
  if (selectedContracts === undefined) {
    throw new Error(
      `${fraudCategoryLabel(fraudCategory)} deployment resolution returned no category contracts.`,
    );
  }
  const firstStep = selectedContracts.firstStep;
  const firstStepAddress = firstStep.spendingScriptAddress;
  const firstStepHash = firstStep.spendingScriptHash;
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
  if (firstStepHash !== category.scriptHash) {
    throw new Error(
      `${fraudCategoryLabel(fraudCategory)} first-step script hash mismatch: catalogue=${category.scriptHash}, derived=${firstStepHash}.`,
    );
  }
  const parsedFraudulentBlockOutRef = parseOutRef(
    fraudulentBlockOutRef,
    "--fraudulent-block-out-ref",
  );
  const [catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo] = await Promise.all(
    [
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(fraudProofCatalogueSpendHash),
        ),
        unit: toUnit(
          fraudProofCataloguePolicyId,
          FRAUD_PROOF_CATALOGUE_ASSET_NAME,
        ),
        label: "fraud-proof catalogue",
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(hubOraclePolicyId),
        ),
        unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: "hub oracle",
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parsedFraudulentBlockOutRef,
        label: "fraudulent block UTxO",
      }),
    ],
  );
  const resolvedHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId,
    fraudulentBlockUtxo,
    configuredHeaderHash: fraudulentHeaderHash,
  });
  const computationThreadAssetName = `${category.categoryId}${resolvedHeaderHash}`;
  const computationThreadUnit = toUnit(
    computationThreadPolicyId,
    computationThreadAssetName,
  );
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: computationThreadMintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${fraudCategoryLabel(fraudCategory)} init computation-thread mint`,
  });
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriage({
    script: phasMembershipScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${fraudCategoryLabel(fraudCategory)} init PHAS membership`,
  });
  const referenceInputs = [
    catalogueUtxo,
    hubOracleUtxo,
    fraudulentBlockUtxo,
    ...computationThreadMintCarriage.referenceInputs,
    ...phasMembershipCarriage.referenceInputs,
  ];
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const firstStepDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: null,
    },
    FraudProofComputationThreadStepDatum,
  );
  const firstStepOutputMatches = computationThreadOutputPredicate({
    address: firstStepAddress,
    datum: firstStepDatum,
    unit: computationThreadUnit,
  });
  let firstStepOutputIndex: bigint | undefined;
  const computationThreadMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      computationThreadPolicyId,
      `${fraudCategoryLabel(fraudCategory)} init computation-thread mint`,
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      firstStepOutputMatches,
      `${fraudCategoryLabel(fraudCategory)} init first step`,
    );
    firstStepOutputIndex = outputIndex;
    return Data.to(
      {
        Init: {
          first_step_output_index: outputIndex,
          fraud_category_id: category.categoryId,
          fraud_category: firstStepHash,
          fraud_category_membership_proof: Data.from(
            category.membershipProofCbor,
            Proof,
          ),
          fraud_proof_catalogue_ref_input_index: requireReferenceInputIndex(
            ctx,
            catalogueUtxo,
            `${fraudCategoryLabel(fraudCategory)} init fraud-proof catalogue`,
          ),
          inclusion_proof_script_redeemer_index: requireWithdrawalRedeemerIndex(
            ctx,
            phasRewardAddress,
            `${fraudCategoryLabel(fraudCategory)} init PHAS membership`,
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            `${fraudCategoryLabel(fraudCategory)} init hub oracle`,
          ),
          fraudulent_block_ref_input_index: requireReferenceInputIndex(
            ctx,
            fraudulentBlockUtxo,
            `${fraudCategoryLabel(fraudCategory)} init fraudulent block`,
          ),
        },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const chainedTx = lucid
    .newTx()
    .readFrom(referenceInputs)
    .withdraw(
      phasRewardAddress,
      0n,
      encodePhasMembershipRedeemer({
        root: catalogue.root,
        categoryId: category.categoryId,
        categoryScriptHash: category.scriptHash,
        membershipProofCbor: category.membershipProofCbor,
      }),
    )
    .mintAssets({ [computationThreadUnit]: 1n }, computationThreadMintRedeemer)
    .pay.ToContract(
      firstStepAddress,
      {
        kind: "inline",
        value: firstStepDatum,
      },
      { [computationThreadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = phasMembershipCarriage.attach(
    computationThreadMintCarriage.attach(chainedTx),
  );

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (firstStepOutputIndex === undefined) {
    throw new Error("BuildTxWithRedeemer did not resolve init output index.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof computation-thread minting",
        utxo: witnessReferenceScripts?.computationThreadMint,
        expectedScript: computationThreadMintingScript,
      }),
      workflowReferenceScript({
        role: "membership proof withdrawal",
        utxo: witnessReferenceScripts?.phasMembershipWithdraw,
        expectedScript: phasMembershipScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `Provider returned transaction hash ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    fraudulentBlockOutRef,
    fraudulentHeaderHash: resolvedHeaderHash,
    computationThreadPolicyId,
    computationThreadAssetName,
    computationThreadUnit,
    firstStepAddress,
    firstStepOutputIndex: Number(firstStepOutputIndex),
    fraudCategoryId: category.categoryId,
    fraudCategoryName: fraudCategory,
    fraudCategory: firstStepHash,
    fraudProofCatalogueRoot: catalogue.root,
    awaitedConfirmation: awaitConfirmation,
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
