import {
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
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
} from "@lucid-evolution/lucid";

import {
  type ContractDeploymentInfo,
  parseContractDeploymentInfo,
} from "./inspect-contracts.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodePhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmit,
  parseOutRef,
  phasMembershipRewardAddress,
  type ProverSignerConfig,
  readJsonFile,
  requireDeploymentScriptHash,
  requireSingletonUtxo,
  resolveDoubleSpendDeploymentContracts,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveInvalidRangeDeploymentContracts,
  resolveProverSigner,
  resolveTransitionTraceDeploymentContracts,
  type SubmitProviderConfig,
} from "./runtime.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

const PHAS_MEMBERSHIP_WITHDRAW_TITLE = "phas.membership.withdraw";

type LucidDataSchema = Parameters<typeof Data.to>[1];

export type SubmitInitCliConfig = SubmitProviderConfig &
  ProverSignerConfig & {
    readonly blueprintPath: string;
    readonly deploymentInfoPath: string;
    readonly fraudCategory?: SubmitInitFraudCategory;
    readonly fraudulentBlockOutRef: string;
    readonly fraudulentHeaderHash?: string;
    readonly awaitConfirmation?: boolean;
  };

export type SubmitInitFraudCategory =
  | "doubleSpend"
  | "invalidRange"
  | "transitionTrace";

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
  switch (category) {
    case "doubleSpend":
      return "double-spend";
    case "invalidRange":
      return "invalid-range";
    case "transitionTrace":
      return "transition-trace";
  }
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
      Data.Bytes({ minLength: 4, maxLength: 4 }) as unknown as LucidDataSchema,
    ),
    valueCbor: Data.to(
      categoryScriptHash,
      Data.Bytes({
        minLength: 28,
        maxLength: 28,
      }) as unknown as LucidDataSchema,
    ),
    membershipProofCbor,
  });

export const submitInit = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  fraudCategory = "doubleSpend",
  fraudulentBlockOutRef,
  fraudulentHeaderHash,
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
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitInitResult> => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const catalogue = requireFraudProofCatalogue(parsedDeploymentInfo);
  let category: (typeof catalogue.categories)[SubmitInitFraudCategory];
  let stateQueuePolicyId: string;
  let computationThreadPolicyId: string;
  let computationThreadMintingScript: Script;
  let firstStepAddress: string;
  let firstStepHash: string;
  if (fraudCategory === "doubleSpend") {
    const resolvedDeployment = await resolveDoubleSpendDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireStateQueueMint: true,
    });
    category = resolvedDeployment.doubleSpendCategory;
    stateQueuePolicyId = resolvedDeployment.stateQueuePolicyId!;
    computationThreadPolicyId =
      resolvedDeployment.contracts.computationThread.policyId;
    computationThreadMintingScript =
      resolvedDeployment.contracts.computationThread.mintingScript;
    firstStepAddress =
      resolvedDeployment.contracts.doubleSpend.firstStep.spendingScriptAddress;
    firstStepHash =
      resolvedDeployment.contracts.doubleSpend.firstStep.spendingScriptHash;
  } else if (fraudCategory === "invalidRange") {
    const resolvedDeployment = await resolveInvalidRangeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireStateQueueMint: true,
    });
    category = resolvedDeployment.invalidRangeCategory;
    stateQueuePolicyId = resolvedDeployment.stateQueuePolicyId!;
    computationThreadPolicyId =
      resolvedDeployment.contracts.computationThread.policyId;
    computationThreadMintingScript =
      resolvedDeployment.contracts.computationThread.mintingScript;
    firstStepAddress =
      resolvedDeployment.contracts.invalidRange.firstStep.spendingScriptAddress;
    firstStepHash =
      resolvedDeployment.contracts.invalidRange.firstStep.spendingScriptHash;
  } else {
    const resolvedDeployment = await resolveTransitionTraceDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireStateQueueMint: true,
    });
    category = resolvedDeployment.transitionTraceCategory;
    stateQueuePolicyId = resolvedDeployment.stateQueuePolicyId!;
    computationThreadPolicyId =
      resolvedDeployment.contracts.computationThread.policyId;
    computationThreadMintingScript =
      resolvedDeployment.contracts.computationThread.mintingScript;
    firstStepAddress =
      resolvedDeployment.contracts.transitionTrace.firstStep
        .spendingScriptAddress;
    firstStepHash =
      resolvedDeployment.contracts.transitionTrace.firstStep.spendingScriptHash;
  }
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
  const referenceInputs = [catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo];
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
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
  const tx = lucid
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
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(computationThreadMintingScript)
    .attach.WithdrawalValidator(phasMembershipScript);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (firstStepOutputIndex === undefined) {
    throw new Error("BuildTxWithRedeemer did not resolve init output index.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
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
