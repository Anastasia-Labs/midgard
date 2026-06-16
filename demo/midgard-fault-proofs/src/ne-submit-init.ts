import {
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  HUB_ORACLE_ASSET_NAME,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";

import { parseContractDeploymentInfo } from "./inspect-contracts.js";
import {
  compareOutRefs,
  DEFAULT_CONFIRMATION_POLL_MS,
  encodePhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmit,
  parseOutRef,
  phasMembershipRewardAddress,
  type ProverSignerConfig,
  readJsonFile,
  referenceInputIndex,
  requireDeploymentScriptHash,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveNonExistentInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";

const PHAS_MEMBERSHIP_WITHDRAW_TITLE = "phas.membership.withdraw";
const FIRST_STEP_OUTPUT_INDEX = 0n;
const PHAS_WITHDRAW_REDEEMER_INDEX = 0n;

type LucidDataSchema = Parameters<typeof Data.to>[1];

export type NeSubmitInitCliConfig = SubmitProviderConfig &
  ProverSignerConfig & {
    readonly blueprintPath: string;
    readonly deploymentInfoPath: string;
    readonly fraudulentBlockOutRef: string;
    readonly fraudulentHeaderHash?: string;
    readonly awaitConfirmation?: boolean;
  };

export type NeSubmitInitResult = {
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
  readonly fraudCategory: string;
  readonly fraudProofCatalogueRoot: string;
  readonly awaitedConfirmation: boolean;
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

export const neSubmitInit = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  fraudulentBlockOutRef,
  fraudulentHeaderHash,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash?: string;
  readonly awaitConfirmation?: boolean;
}): Promise<NeSubmitInitResult> => {
  const { nonExistentInputCategory, contracts } =
    await resolveNonExistentInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);

  const catalogue =
    parsedDeploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (catalogue === undefined) {
    throw new Error(
      "Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.",
    );
  }
  const stateQueuePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "stateQueueMint",
  );
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

  const firstStepHash =
    contracts.nonExistentInput.firstStep.spendingScriptHash;
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
  const computationThreadAssetName = `${nonExistentInputCategory.categoryId}${resolvedHeaderHash}`;
  const computationThreadUnit = toUnit(
    contracts.computationThread.policyId,
    computationThreadAssetName,
  );
  const sortedReferenceInputs = [
    catalogueUtxo,
    hubOracleUtxo,
    fraudulentBlockUtxo,
  ].sort(compareOutRefs);
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };

  signer.selectWallet(lucid);
  const tx = lucid
    .newTx()
    .readFrom(sortedReferenceInputs)
    .withdraw(
      phasMembershipRewardAddress(network, phasMembershipScript),
      0n,
      encodePhasMembershipRedeemer({
        root: catalogue.root,
        categoryId: nonExistentInputCategory.categoryId,
        categoryScriptHash: nonExistentInputCategory.scriptHash,
        membershipProofCbor: nonExistentInputCategory.membershipProofCbor,
      }),
    )
    .mintAssets(
      { [computationThreadUnit]: 1n },
      Data.to(
        {
          Init: {
            first_step_output_index: FIRST_STEP_OUTPUT_INDEX,
            fraud_category_id: nonExistentInputCategory.categoryId,
            fraud_category: firstStepHash,
            fraud_category_membership_proof: Data.from(
              nonExistentInputCategory.membershipProofCbor,
              Proof,
            ),
            fraud_proof_catalogue_ref_input_index: referenceInputIndex(
              sortedReferenceInputs,
              catalogueUtxo,
            ),
            inclusion_proof_script_redeemer_index: PHAS_WITHDRAW_REDEEMER_INDEX,
            hub_oracle_ref_input_index: referenceInputIndex(
              sortedReferenceInputs,
              hubOracleUtxo,
            ),
            fraudulent_block_ref_input_index: referenceInputIndex(
              sortedReferenceInputs,
              fraudulentBlockUtxo,
            ),
          },
        },
        FraudProofComputationThreadRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.nonExistentInput.firstStep.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: null,
          },
          FraudProofComputationThreadStepDatum,
        ),
      },
      { [computationThreadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.WithdrawalValidator(phasMembershipScript);

  const unsigned = await tx.complete({ localUPLCEval: true });
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
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName,
    computationThreadUnit,
    firstStepAddress:
      contracts.nonExistentInput.firstStep.spendingScriptAddress,
    firstStepOutputIndex: Number(FIRST_STEP_OUTPUT_INDEX),
    fraudCategoryId: nonExistentInputCategory.categoryId,
    fraudCategory: firstStepHash,
    fraudProofCatalogueRoot: catalogue.root,
    awaitedConfirmation: awaitConfirmation,
  };
};

export const neSubmitInitFromFiles = async (
  config: NeSubmitInitCliConfig,
): Promise<NeSubmitInitResult> => {
  const [blueprint, deploymentInfo, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await neSubmitInit({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    fraudulentBlockOutRef: config.fraudulentBlockOutRef,
    fraudulentHeaderHash: config.fraudulentHeaderHash,
    awaitConfirmation: config.awaitConfirmation,
  });
};
