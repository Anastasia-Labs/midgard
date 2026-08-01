import {
  EMPTY_SPEND_INPUTS_HASH,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  ZeroInputStep02Datum,
  ZeroInputStep02SpendRedeemer,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";

import { rejectRetiredUnauthenticatedSubmissionRouteV1 } from "./legacy-submission-boundary-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  type ResolvedProverSigner,
  resolveProverSigner,
  resolveZeroInputDeploymentContracts,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";

export type SubmitZeroInputStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitZeroInputStep02Result = {
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
  readonly secondStepAddress: string;
  readonly badTxSpendInputsHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type ZeroInputStep02DatumWithState = ZeroInputStep02Datum & {
  readonly data: NonNullable<ZeroInputStep02Datum["data"]>;
};

type ZeroInputStep02ResolvedLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

type ZeroInputStep02SpendLayout = Omit<
  ZeroInputStep02ResolvedLayout,
  "computationThreadMintRedeemerIndex"
>;

const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): ZeroInputStep02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, ZeroInputStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Zero-input step 02 input datum must carry the verified spend-inputs hash.",
    );
  }
  return datum as ZeroInputStep02DatumWithState;
};

const fraudProofOutputPredicate = ({
  fraudProofAddress,
  fraudProofUnit,
  fraudProofDatum,
}: {
  readonly fraudProofAddress: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
}): ((output: TxOutput) => boolean) =>
  outputWithDatumAndUnitPredicate({
    address: fraudProofAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });

const makeZeroInputStep02SpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly onLayout: (layout: ZeroInputStep02SpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "zero-input step 02");
    const layout: ZeroInputStep02SpendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "zero-input step 02"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputPredicate({
          fraudProofAddress,
          fraudProofUnit,
          fraudProofDatum,
        }),
        "zero-input step 02 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "zero-input step 02 fraud-proof",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
          },
        ],
      },
      ZeroInputStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const makeFraudProofMintRedeemer = ({
  fraudProofPolicyId,
  computationThreadPolicyId,
  computationThreadAssetName,
  onComputationThreadMintRedeemerIndex,
}: {
  readonly fraudProofPolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly onComputationThreadMintRedeemerIndex: (index: bigint) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      fraudProofPolicyId,
      "zero-input step 02 fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "zero-input step 02 computation-thread burn",
    );
    onComputationThreadMintRedeemerIndex(computationThreadMintRedeemerIndex);
    return Data.to(
      {
        computation_thread_token_asset_name: computationThreadAssetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const makeComputationThreadSuccessRedeemer = ({
  computationThreadPolicyId,
  computationThreadAssetName,
}: {
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      computationThreadPolicyId,
      "zero-input step 02 computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const submitZeroInputStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitZeroInputStep02Result> => {
  const { zeroInputCategory, contracts } =
    await resolveZeroInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "zero-input step-02 computation-thread UTxO",
  });
  if (
    threadUtxo.address !== contracts.zeroInput.steps[1].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at zero-input step 02.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: zeroInputCategory.categoryId,
    categoryLabel: "zero-input",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });
  const badTxSpendInputsHash = inputDatum.data.bad_tx_spend_inputs_hash;
  // Mirrors the validator's only category-specific check, so a thread that
  // cannot conclude fails here instead of burning a submission on-chain.
  if (badTxSpendInputsHash !== EMPTY_SPEND_INPUTS_HASH) {
    throw new Error(
      `Zero-input step 02 datum spend-inputs hash ${badTxSpendInputsHash} is not the empty-input-list hash ${EMPTY_SPEND_INPUTS_HASH}.`,
    );
  }

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
  let spendLayout: ZeroInputStep02SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeZeroInputStep02SpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        onLayout: (layout) => {
          spendLayout = layout;
        },
      }),
    )
    .mintAssets(
      { [threadToken.unit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: threadToken.assetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: threadToken.assetName,
        onComputationThreadMintRedeemerIndex: (index) => {
          computationThreadMintRedeemerIndex = index;
        },
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      fraudProofAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(contracts.zeroInput.steps[1].spendingScript)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve zero-input step 02 layout.",
    );
  }
  const resolvedLayout: ZeroInputStep02ResolvedLayout = {
    ...spendLayout,
    computationThreadMintRedeemerIndex,
  };
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
    fraudProofOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    secondStepAddress: contracts.zeroInput.steps[1].spendingScriptAddress,
    badTxSpendInputsHash,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      resolvedLayout.computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      resolvedLayout.fraudProofMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitZeroInputStep02FromFiles = async (
  config: SubmitZeroInputStep02CliConfig,
): Promise<SubmitZeroInputStep02Result> => {
  rejectRetiredUnauthenticatedSubmissionRouteV1({
    command: "submit-zero-input-step-02",
  });
  const [blueprint, deploymentInfo, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitZeroInputStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    awaitConfirmation: config.awaitConfirmation,
  });
};
