import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  ReferenceInputNoIdxStep04Datum,
  ReferenceInputNoIdxStep04SpendRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  type ResolvedProverSigner,
  resolveProverSigner,
  resolveReferenceInputNoIdxDeploymentContracts,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";

export type RinxSubmitStep04CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly outputsPreimagePath: string;
  readonly awaitConfirmation?: boolean;
};

export type RinxSubmitStep04Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAssetName: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly fourthStepAddress: string;
  readonly outputsPreimageCount: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step04DatumWithState = ReferenceInputNoIdxStep04Datum & {
  readonly data: NonNullable<ReferenceInputNoIdxStep04Datum["data"]>;
};

const requireStep04Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step04DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, ReferenceInputNoIdxStep04Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Step 04 input datum must carry producing-tx outputs-hash state data.",
    );
  }
  return datum as Step04DatumWithState;
};

export const rinxSubmitStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  outputsPreimage,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly outputsPreimage: readonly string[];
  readonly awaitConfirmation?: boolean;
}): Promise<RinxSubmitStep04Result> => {
  const { referenceInputNoIdxCategory, contracts } =
    await resolveReferenceInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const steps = contracts.referenceInputNoIdx.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "reference-input-no-idx step-04 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[3].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at reference-input-no-idx step 04.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: referenceInputNoIdxCategory.categoryId,
    categoryLabel: "reference-input-no-idx",
  });
  requireStep04Datum({ threadUtxo, signer });

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const fraudProofAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [fraudProofUnit]: 1n,
  };
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });

  let spendLayout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        fraudProofMintRedeemerIndex: bigint;
      }
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "reference-input-no-idx step 04");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "reference-input-no-idx step 04",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputMatches,
        "reference-input-no-idx step 04 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        "reference-input-no-idx step 04 fraud-proof",
      ),
    };
    spendLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            outputs_preimage: [...outputsPreimage],
          },
        ],
      },
      ReferenceInputNoIdxStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const fraudProofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      "reference-input-no-idx step 04 fraud-proof mint",
    );
    const ctIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      "reference-input-no-idx step 04 computation-thread burn",
    );
    computationThreadMintRedeemerIndex = ctIndex;
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: ctIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadSuccessRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "reference-input-no-idx step 04 computation-thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadToken.unit]: -1n }, computationThreadSuccessRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      fraudProofAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(steps[3].spendingScript)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript)
    .complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve reference-input-no-idx step 04 layout.",
    );
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
    threadOutRef,
    fraudProofOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    fourthStepAddress: steps[3].spendingScriptAddress,
    outputsPreimageCount: outputsPreimage.length,
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      spendLayout.fraudProofMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const rinxSubmitStep04FromFiles = async (
  config: RinxSubmitStep04CliConfig,
): Promise<RinxSubmitStep04Result> => {
  const [blueprint, deploymentInfo, outputsJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.outputsPreimagePath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await rinxSubmitStep04({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    outputsPreimage: outputsJson as readonly string[],
    awaitConfirmation: config.awaitConfirmation,
  });
};
