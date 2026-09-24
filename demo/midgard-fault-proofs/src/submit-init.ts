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
  /** Production workflow seam: invoked after local evaluation, before I/O. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
};

export type SubmitResolvedInitResult = {
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
  /** `txHash#index` of the freshly minted thread, ready for step-01. */
  readonly nextThreadOutRef: string;
  readonly fraudCategoryId: string;
  readonly fraudCategory: string;
  readonly fraudProofCatalogueRoot: string;
  readonly awaitedConfirmation: boolean;
};

const STANDARD_INIT_REFERENCE_SCRIPT_ROLES = {
  computationThreadMint: "V1 fraud-proof computation-thread minting",
  phasMembershipWithdraw: "membership proof withdrawal",
} as const;

/**
 * The one computation-thread Init transaction: catalogue, hub-oracle and
 * fraudulent-block reference inputs, the PHAS membership withdrawal carrying
 * the category proof, and the `Init` mint paying the thread token to the
 * family's first step. Every family init resolves its contracts and calls
 * this; `submitInit` is the deployment-info resolver in front of it.
 * Callers own the category/first-step agreement check and its message.
 */
export const submitResolvedInit = async ({
  lucid,
  blueprint,
  network,
  label,
  contracts,
  category,
  catalogue,
  signer,
  fraudulentBlockOutRef,
  fraudulentHeaderHash,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
  referenceScriptRoles = STANDARD_INIT_REFERENCE_SCRIPT_ROLES,
}: SubmitResolvedInitParams & {
  /** Family label used in error messages. */
  readonly label: string;
  /** Journal roles of the two witness reference scripts. */
  readonly referenceScriptRoles?: {
    readonly computationThreadMint: string;
    readonly phasMembershipWithdraw: string;
  };
}): Promise<SubmitResolvedInitResult> => {
  const firstStep = contracts.steps[0];
  const [catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo] = await Promise.all(
    [
      requireSingletonUtxo({
        lucid,
        address: catalogue.spendingScriptAddress,
        unit: toUnit(catalogue.policyId, FRAUD_PROOF_CATALOGUE_ASSET_NAME),
        label: `${label} init fraud-proof catalogue`,
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOraclePolicyId),
        ),
        unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: `${label} init hub oracle`,
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          fraudulentBlockOutRef,
          "--fraudulent-block-out-ref",
        ),
        label: `${label} fraudulent block UTxO`,
      }),
    ],
  );
  const resolvedHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo,
    configuredHeaderHash: fraudulentHeaderHash,
  });
  const computationThreadAssetName = `${category.categoryId}${resolvedHeaderHash}`;
  const computationThreadUnit = toUnit(
    contracts.computationThread.policyId,
    computationThreadAssetName,
  );
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${label} init computation-thread mint`,
  });
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriage({
    script: phasMembershipScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${label} init PHAS membership`,
  });
  const referenceInputs = [
    catalogueUtxo,
    hubOracleUtxo,
    fraudulentBlockUtxo,
    ...computationThreadMintCarriage.referenceInputs,
    ...phasMembershipCarriage.referenceInputs,
  ];
  const firstStepDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: null },
    FraudProofComputationThreadStepDatum,
  );
  const firstStepOutputMatches = computationThreadOutputPredicate({
    address: firstStep.spendingScriptAddress,
    datum: firstStepDatum,
    unit: computationThreadUnit,
  });
  let firstStepOutputIndex: bigint | undefined;
  const computationThreadMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${label} init computation-thread mint`,
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      firstStepOutputMatches,
      `${label} init first step`,
    );
    firstStepOutputIndex = outputIndex;
    return Data.to(
      {
        Init: {
          first_step_output_index: outputIndex,
          fraud_category_id: category.categoryId,
          fraud_category: category.scriptHash,
          fraud_category_membership_proof: Data.from(
            category.membershipProofCbor,
            Proof,
          ),
          fraud_proof_catalogue_ref_input_index: requireReferenceInputIndex(
            ctx,
            catalogueUtxo,
            `${label} init fraud-proof catalogue`,
          ),
          inclusion_proof_script_redeemer_index: requireWithdrawalRedeemerIndex(
            ctx,
            phasRewardAddress,
            `${label} init PHAS membership`,
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            `${label} init hub oracle`,
          ),
          fraudulent_block_ref_input_index: requireReferenceInputIndex(
            ctx,
            fraudulentBlockUtxo,
            `${label} init fraudulent block`,
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
      firstStep.spendingScriptAddress,
      { kind: "inline", value: firstStepDatum },
      { [computationThreadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await phasMembershipCarriage
    .attach(computationThreadMintCarriage.attach(chainedTx))
    .complete({ localUPLCEval: true });
  if (firstStepOutputIndex === undefined) {
    throw new Error(`${label}: init output index was not resolved.`);
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: referenceScriptRoles.computationThreadMint,
        utxo: witnessReferenceScripts?.computationThreadMint,
        expectedScript: contracts.computationThread.mintingScript,
      }),
      workflowReferenceScript({
        role: referenceScriptRoles.phasMembershipWithdraw,
        utxo: witnessReferenceScripts?.phasMembershipWithdraw,
        expectedScript: phasMembershipScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `${label}: init provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName,
    computationThreadUnit,
    firstStepAddress: firstStep.spendingScriptAddress,
    firstStepOutputIndex: Number(firstStepOutputIndex),
    nextThreadOutRef: `${txHash}#${firstStepOutputIndex.toString()}`,
    fraudCategoryId: category.categoryId,
    fraudCategory: category.scriptHash,
    fraudProofCatalogueRoot: catalogue.root,
    awaitedConfirmation: awaitConfirmation,
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
