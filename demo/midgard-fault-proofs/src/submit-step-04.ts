import {
  DoubleSpendStep04Datum,
  DoubleSpendStep04SpendRedeemer,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  type MidgardTxInput,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type TxBuilder,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";

import { parseDoubleSpentInputIndex } from "./double-spend-inputs.js";
import { rejectRetiredUnauthenticatedSubmissionRouteV1 } from "./legacy-submission-boundary-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  resolveDoubleSpendDeploymentContracts,
  type ResolvedProverSigner,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  ensureSpendInputsReferenceWitness,
  excludeUtxo,
  spendInputsWitnessFromCbors,
} from "./spend-input-witness.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { hashSpendInputCbors, parseSpendInputCbors } from "./submit-step-03.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";

export type SubmitStep04CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly tx2InputsPath: string;
  readonly doubleSpentInputIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitStep04Result = {
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
  readonly verifiedTx2SpendInputsHash: string;
  readonly doubleSpentInputIndex: number;
  readonly doubleSpentInput: MidgardTxInput;
  readonly doubleSpentInputCbor: string;
  readonly tx2SpendInputsWitnessOutRef: string;
  readonly tx2SpendInputsWitnessCreated: boolean;
  readonly tx2SpendInputsRefInputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step04DatumWithState = DoubleSpendStep04Datum & {
  readonly data: NonNullable<DoubleSpendStep04Datum["data"]>;
};

type Step04ResolvedLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly tx2SpendInputsRefInputIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

type Step04SpendLayout = Omit<
  Step04ResolvedLayout,
  "computationThreadMintRedeemerIndex"
>;

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
  const datum = Data.from(threadUtxo.datum, DoubleSpendStep04Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error("Step 04 input datum must carry verified tx2 state data.");
  }
  return datum as Step04DatumWithState;
};

const sameMidgardTxInput = (
  left: MidgardTxInput,
  right: MidgardTxInput,
): boolean =>
  left.tx_id === right.tx_id && left.output_index === right.output_index;

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

const makeStep04SpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  tx2SpendInputsReferenceInput,
  doubleSpentInputIndex,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly tx2SpendInputsReferenceInput: UTxO;
  readonly doubleSpentInputIndex: bigint;
  readonly onLayout: (layout: Step04SpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "step 04 computation thread");
    const layout: Step04SpendLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "step 04 computation thread",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputPredicate({
          fraudProofAddress,
          fraudProofUnit,
          fraudProofDatum,
        }),
        "step 04 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "step 04 fraud-proof",
      ),
      tx2SpendInputsRefInputIndex: requireReferenceInputIndex(
        ctx,
        tx2SpendInputsReferenceInput,
        "step 04 tx2 spend-input witness",
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
            tx2_spend_inputs_ref_input_index:
              layout.tx2SpendInputsRefInputIndex,
            double_spent_input_index: doubleSpentInputIndex,
          },
        ],
      },
      DoubleSpendStep04SpendRedeemer,
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
    requireOwnMintPurpose(ctx, fraudProofPolicyId, "step 04 fraud-proof mint");
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "step 04 computation-thread burn",
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
      "step 04 computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const submitStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  tx2SpendInputCbors,
  doubleSpentInputIndex,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly tx2SpendInputCbors: readonly string[];
  readonly doubleSpentInputIndex: bigint;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitStep04Result> => {
  const { doubleSpendCategory, contracts } =
    await resolveDoubleSpendDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "step-04 computation-thread UTxO",
  });
  if (
    threadUtxo.address !== contracts.doubleSpend.steps[3].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at double-spend step 04.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: doubleSpendCategory.categoryId,
    categoryLabel: "double-spend",
  });
  const inputDatum = requireStep04Datum({ threadUtxo, signer });
  const tx2SpendInputsHash = hashSpendInputCbors(tx2SpendInputCbors);
  if (tx2SpendInputsHash !== inputDatum.data.verified_tx2_spend_inputs_hash) {
    throw new Error(
      `--tx2-inputs hash mismatch: provided preimage hashes to ${tx2SpendInputsHash}, expected ${inputDatum.data.verified_tx2_spend_inputs_hash}.`,
    );
  }
  if (doubleSpentInputIndex >= BigInt(tx2SpendInputCbors.length)) {
    throw new Error(
      `doubleSpentInputIndex ${doubleSpentInputIndex.toString()} is out of bounds for ${tx2SpendInputCbors.length.toString()} tx2 inputs.`,
    );
  }
  if (doubleSpentInputIndex > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("doubleSpentInputIndex exceeds the safe integer range.");
  }
  const tx2SpendInputsWitness = spendInputsWitnessFromCbors(
    tx2SpendInputCbors,
    "--tx2-inputs",
  );
  const doubleSpentInputCbor =
    tx2SpendInputCbors[Number(doubleSpentInputIndex)]!;
  const doubleSpentInput =
    tx2SpendInputsWitness.inputs[Number(doubleSpentInputIndex)]!;
  if (
    !sameMidgardTxInput(doubleSpentInput, inputDatum.data.double_spent_input)
  ) {
    throw new Error(
      `--tx2-inputs[${doubleSpentInputIndex.toString()}] does not match the double-spent input carried by step 04 datum.`,
    );
  }

  signer.selectWallet(lucid);
  const tx2SpendInputsReferenceWitness =
    await ensureSpendInputsReferenceWitness({
      lucid,
      address: signer.address,
      paymentKeyHash: signer.paymentKeyHash,
      witness: tx2SpendInputsWitness,
      awaitConfirmation,
    });
  const referenceInputs = [tx2SpendInputsReferenceWitness.utxo];
  const walletUtxosWithoutWitness = excludeUtxo(
    await lucid.wallet().getUtxos(),
    tx2SpendInputsReferenceWitness.utxo,
  );
  const feeCandidates =
    tx2SpendInputsReferenceWitness.spentFeeInput === undefined
      ? walletUtxosWithoutWitness
      : excludeUtxo(
          walletUtxosWithoutWitness,
          tx2SpendInputsReferenceWitness.spentFeeInput,
        );
  const feeInput = selectFeeInput(feeCandidates);
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
  let spendLayout: Step04SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const computationThreadSuccessRedeemer = makeComputationThreadSuccessRedeemer(
    {
      computationThreadPolicyId: contracts.computationThread.policyId,
      computationThreadAssetName: threadToken.assetName,
    },
  );

  const makeStep04Tx = (): TxBuilder =>
    lucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom(
        [threadUtxo],
        makeStep04SpendRedeemer({
          threadUtxo,
          fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
          fraudProofPolicyId: contracts.fraudProof.policyId,
          fraudProofUnit,
          fraudProofDatum,
          tx2SpendInputsReferenceInput: tx2SpendInputsReferenceWitness.utxo,
          doubleSpentInputIndex,
          onLayout: (layout) => {
            spendLayout = layout;
          },
        }),
      )
      .readFrom(referenceInputs)
      .mintAssets({ [threadToken.unit]: -1n }, computationThreadSuccessRedeemer)
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
      .attach.SpendingValidator(contracts.doubleSpend.steps[3].spendingScript)
      .attach.MintingPolicy(contracts.computationThread.mintingScript)
      .attach.MintingPolicy(contracts.fraudProof.mintingScript);

  const unsigned = await makeStep04Tx().complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error("BuildTxWithRedeemer did not resolve step 04 layout.");
  }
  const resolvedLayout: Step04ResolvedLayout = {
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
    fourthStepAddress: contracts.doubleSpend.steps[3].spendingScriptAddress,
    verifiedTx2SpendInputsHash: inputDatum.data.verified_tx2_spend_inputs_hash,
    doubleSpentInputIndex: Number(doubleSpentInputIndex),
    doubleSpentInput,
    doubleSpentInputCbor,
    tx2SpendInputsWitnessOutRef: tx2SpendInputsReferenceWitness.outRef,
    tx2SpendInputsWitnessCreated: tx2SpendInputsReferenceWitness.created,
    tx2SpendInputsRefInputIndex: Number(
      resolvedLayout.tx2SpendInputsRefInputIndex,
    ),
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

export const submitStep04FromFiles = async (
  config: SubmitStep04CliConfig,
): Promise<SubmitStep04Result> => {
  rejectRetiredUnauthenticatedSubmissionRouteV1({
    command: "submit-step-04",
  });
  const [blueprint, deploymentInfo, tx2InputsJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.tx2InputsPath),
    makeLucidForSubmit(config),
  ]);
  const tx2SpendInputCbors = parseSpendInputCbors(
    tx2InputsJson,
    "--tx2-inputs",
  );
  const signer = resolveProverSigner(config);
  return await submitStep04({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    tx2SpendInputCbors,
    doubleSpentInputIndex: parseDoubleSpentInputIndex({
      value: config.doubleSpentInputIndex,
      inputCount: tx2SpendInputCbors.length,
      inputLabel: "tx2",
    }),
    awaitConfirmation: config.awaitConfirmation,
  });
};
