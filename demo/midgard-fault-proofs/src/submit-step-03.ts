import { computeHash32, encodeCbor } from "@al-ft/midgard-core";
import {
  DoubleSpendStep03Datum,
  DoubleSpendStep03SpendRedeemer,
  DoubleSpendStep04Datum,
  type MidgardTxInput,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { parseDoubleSpentInputIndex } from "./double-spend-inputs.js";
import { parseHex } from "./json-file.js";
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
  inputIndex,
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";

const STEP_03_OUTPUT_INDEX = 0n;

export type SubmitStep03CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly tx1InputsPath: string;
  readonly doubleSpentInputIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  readonly fourthStepAddress: string;
  readonly verifiedTx1SpendInputsHash: string;
  readonly verifiedTx2SpendInputsHash: string;
  readonly doubleSpentInputIndex: number;
  readonly doubleSpentInput: MidgardTxInput;
  readonly doubleSpentInputCbor: string;
  readonly tx1SpendInputsWitnessOutRef: string;
  readonly tx1SpendInputsWitnessCreated: boolean;
  readonly tx1SpendInputsRefInputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const parseSpendInputCbors = (
  value: unknown,
  label: string,
): readonly string[] => {
  if (!Array.isArray(value)) {
    throw new Error(
      `${label} must be a JSON array of raw input CBOR hex strings.`,
    );
  }
  return value.map((entry, index) =>
    parseHex(entry, `${label}[${index.toString()}]`),
  );
};

export const hashSpendInputCbors = (inputCbors: readonly string[]): string =>
  computeHash32(
    encodeCbor(inputCbors.map((inputCbor) => Buffer.from(inputCbor, "hex"))),
  ).toString("hex");

type Step03DatumWithState = DoubleSpendStep03Datum & {
  readonly data: NonNullable<DoubleSpendStep03Datum["data"]>;
};

const requireStep03Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step03DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, DoubleSpendStep03Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error("Step 03 input datum must carry verified tx input hashes.");
  }
  return datum as Step03DatumWithState;
};

export const submitStep03 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  tx1SpendInputCbors,
  doubleSpentInputIndex,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly tx1SpendInputCbors: readonly string[];
  readonly doubleSpentInputIndex: bigint;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitStep03Result> => {
  const { doubleSpendCategory, contracts } =
    await resolveDoubleSpendDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "step-03 computation-thread UTxO",
  });
  if (
    threadUtxo.address !== contracts.doubleSpend.steps[2].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at double-spend step 03.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    doubleSpendCategoryId: doubleSpendCategory.categoryId,
  });
  const inputDatum = requireStep03Datum({ threadUtxo, signer });
  const tx1SpendInputsHash = hashSpendInputCbors(tx1SpendInputCbors);
  if (tx1SpendInputsHash !== inputDatum.data.verified_tx1_spend_inputs_hash) {
    throw new Error(
      `--tx1-inputs hash mismatch: provided preimage hashes to ${tx1SpendInputsHash}, expected ${inputDatum.data.verified_tx1_spend_inputs_hash}.`,
    );
  }
  if (doubleSpentInputIndex >= BigInt(tx1SpendInputCbors.length)) {
    throw new Error(
      `doubleSpentInputIndex ${doubleSpentInputIndex.toString()} is out of bounds for ${tx1SpendInputCbors.length.toString()} tx1 inputs.`,
    );
  }
  if (doubleSpentInputIndex > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("doubleSpentInputIndex exceeds the safe integer range.");
  }
  const tx1SpendInputsWitness = spendInputsWitnessFromCbors(
    tx1SpendInputCbors,
    "--tx1-inputs",
  );
  const doubleSpentInputCbor =
    tx1SpendInputCbors[Number(doubleSpentInputIndex)]!;
  const doubleSpentInput =
    tx1SpendInputsWitness.inputs[Number(doubleSpentInputIndex)]!;

  signer.selectWallet(lucid);
  const tx1SpendInputsReferenceWitness =
    await ensureSpendInputsReferenceWitness({
      lucid,
      address: signer.address,
      paymentKeyHash: signer.paymentKeyHash,
      witness: tx1SpendInputsWitness,
      awaitConfirmation,
    });
  const referenceInputs = [tx1SpendInputsReferenceWitness.utxo];
  // This transaction adds exactly one reference input: the spend-input witness.
  const tx1SpendInputsRefInputIndex = 0n;
  const walletUtxosWithoutWitness = excludeUtxo(
    await lucid.wallet().getUtxos(),
    tx1SpendInputsReferenceWitness.utxo,
  );
  const feeCandidates =
    tx1SpendInputsReferenceWitness.spentFeeInput === undefined
      ? walletUtxosWithoutWitness
      : excludeUtxo(
          walletUtxosWithoutWitness,
          tx1SpendInputsReferenceWitness.spentFeeInput,
        );
  const feeInput = selectFeeInput(feeCandidates);
  const spendInputIndex = inputIndex(threadUtxo, feeInput);
  const redeemer = Data.to(
    {
      Continue: [
        {
          input_index: spendInputIndex,
          output_index: STEP_03_OUTPUT_INDEX,
          tx1_spend_inputs_ref_input_index: tx1SpendInputsRefInputIndex,
          double_spent_input_index: doubleSpentInputIndex,
        },
      ],
    },
    DoubleSpendStep03SpendRedeemer,
  );
  const step04Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        verified_tx2_spend_inputs_hash:
          inputDatum.data.verified_tx2_spend_inputs_hash,
        double_spent_input: doubleSpentInput,
      },
    },
    DoubleSpendStep04Datum,
  );
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs)
    .pay.ToContract(
      contracts.doubleSpend.steps[3].spendingScriptAddress,
      {
        kind: "inline",
        value: step04Datum,
      },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(contracts.doubleSpend.steps[2].spendingScript);

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
    threadOutRef,
    nextThreadOutRef: `${txHash}#${STEP_03_OUTPUT_INDEX.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    thirdStepAddress: contracts.doubleSpend.steps[2].spendingScriptAddress,
    fourthStepAddress: contracts.doubleSpend.steps[3].spendingScriptAddress,
    verifiedTx1SpendInputsHash: inputDatum.data.verified_tx1_spend_inputs_hash,
    verifiedTx2SpendInputsHash: inputDatum.data.verified_tx2_spend_inputs_hash,
    doubleSpentInputIndex: Number(doubleSpentInputIndex),
    doubleSpentInput,
    doubleSpentInputCbor,
    tx1SpendInputsWitnessOutRef: tx1SpendInputsReferenceWitness.outRef,
    tx1SpendInputsWitnessCreated: tx1SpendInputsReferenceWitness.created,
    tx1SpendInputsRefInputIndex: Number(tx1SpendInputsRefInputIndex),
    inputIndex: Number(spendInputIndex),
    outputIndex: Number(STEP_03_OUTPUT_INDEX),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitStep03FromFiles = async (
  config: SubmitStep03CliConfig,
): Promise<SubmitStep03Result> => {
  const [blueprint, deploymentInfo, tx1InputsJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.tx1InputsPath),
    makeLucidForSubmit(config),
  ]);
  const tx1SpendInputCbors = parseSpendInputCbors(
    tx1InputsJson,
    "--tx1-inputs",
  );
  const signer = resolveProverSigner(config);
  return await submitStep03({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    tx1SpendInputCbors,
    doubleSpentInputIndex: parseDoubleSpentInputIndex({
      value: config.doubleSpentInputIndex,
      inputCount: tx1SpendInputCbors.length,
      inputLabel: "tx1",
    }),
    awaitConfirmation: config.awaitConfirmation,
  });
};
