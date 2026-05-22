import {
  buildDoubleSpendFaultProofContracts,
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  HUB_ORACLE_ASSET_NAME,
  parseFaultProofBlueprint,
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
import { Effect } from "effect";

import {
  type ContractDeploymentInfo,
  inspectContracts,
  parseContractDeploymentInfo,
} from "./inspect-contracts.js";
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
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";

const PHAS_MEMBERSHIP_WITHDRAW_TITLE = "phas.membership.withdraw";
const FIRST_STEP_OUTPUT_INDEX = 0n;
const PHAS_WITHDRAW_REDEEMER_INDEX = 0n;

type LucidDataSchema = Parameters<typeof Data.to>[1];

export type SubmitInitCliConfig = SubmitProviderConfig &
  ProverSignerConfig & {
    readonly blueprintPath: string;
    readonly deploymentInfoPath: string;
    readonly fraudulentBlockOutRef: string;
    readonly fraudulentHeaderHash?: string;
    readonly awaitConfirmation?: boolean;
  };

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
}): Promise<SubmitInitResult> => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const inspection = await Effect.runPromise(
    inspectContracts({ blueprint, deploymentInfo, network }),
  );
  if (!inspection.fraudProofCatalogue.initReady) {
    throw new Error(
      "Fraud-proof catalogue is not ready for double-spend Init; run inspect-contracts and redeploy/export deployment info with the real double-spend first step.",
    );
  }

  const catalogue = requireFraudProofCatalogue(parsedDeploymentInfo);
  const doubleSpendCategory = catalogue.categories.doubleSpend;
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

  const contracts = await Effect.runPromise(
    buildDoubleSpendFaultProofContracts({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId,
      fraudProofCataloguePolicyId,
    }),
  );
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
  const computationThreadAssetName = `${doubleSpendCategory.categoryId}${resolvedHeaderHash}`;
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
        categoryId: doubleSpendCategory.categoryId,
        categoryScriptHash: doubleSpendCategory.scriptHash,
        membershipProofCbor: doubleSpendCategory.membershipProofCbor,
      }),
    )
    .mintAssets(
      { [computationThreadUnit]: 1n },
      Data.to(
        {
          Init: {
            first_step_output_index: FIRST_STEP_OUTPUT_INDEX,
            fraud_category_id: doubleSpendCategory.categoryId,
            fraud_category: inspection.doubleSpend.categoryFirstStepHash,
            fraud_category_membership_proof: Data.from(
              doubleSpendCategory.membershipProofCbor,
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
      contracts.doubleSpend.firstStep.spendingScriptAddress,
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
    firstStepAddress: contracts.doubleSpend.firstStep.spendingScriptAddress,
    firstStepOutputIndex: Number(FIRST_STEP_OUTPUT_INDEX),
    fraudCategoryId: doubleSpendCategory.categoryId,
    fraudCategory: inspection.doubleSpend.categoryFirstStepHash,
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
    fraudulentBlockOutRef: config.fraudulentBlockOutRef,
    fraudulentHeaderHash: config.fraudulentHeaderHash,
    awaitConfirmation: config.awaitConfirmation,
  });
};
