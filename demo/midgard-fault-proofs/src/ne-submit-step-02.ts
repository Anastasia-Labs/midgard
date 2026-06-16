import {
  type MidgardTxInput,
  NonExistentInputStep02Datum,
  NonExistentInputStep02SpendRedeemer,
  NonExistentInputStep03Datum,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
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
  resolveNonExistentInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import { spendInputsWitnessFromCbors } from "./spend-input-witness.js";
import {
  inputIndex,
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { hashSpendInputCbors, parseSpendInputCbors } from "./submit-step-03.js";

const STEP_02_OUTPUT_INDEX = 0n;

export type NeSubmitStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly inputsPath: string;
  readonly badInputIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type NeSubmitStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly thirdStepAddress: string;
  readonly missingInput: MidgardTxInput;
  readonly badInputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step02DatumWithState = NonExistentInputStep02Datum & {
  readonly data: NonNullable<NonExistentInputStep02Datum["data"]>;
};

const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, NonExistentInputStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error("Step 02 input datum must carry the bad-tx state data.");
  }
  return datum as Step02DatumWithState;
};

// The bad transaction's spend-inputs preimage is the node's native input list,
// supplied as raw `TransactionInput` CBOR hex strings (same format double-spend
// uses for `--tx1-inputs`).
export const parseInputsPreimage = (value: unknown): readonly string[] =>
  parseSpendInputCbors(value, "--inputs");

export const neSubmitStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  spendInputCbors,
  badInputIndex,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly spendInputCbors: readonly string[];
  readonly badInputIndex: bigint;
  readonly awaitConfirmation?: boolean;
}): Promise<NeSubmitStep02Result> => {
  const { nonExistentInputCategory, contracts } =
    await resolveNonExistentInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const steps = contracts.nonExistentInput.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "step-02 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at non-existent-input step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    doubleSpendCategoryId: nonExistentInputCategory.categoryId,
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });

  const inputsHash = hashSpendInputCbors(spendInputCbors);
  if (inputsHash !== inputDatum.data.bad_tx_inputs_hash) {
    throw new Error(
      `--inputs hash mismatch: provided preimage hashes to ${inputsHash}, expected ${inputDatum.data.bad_tx_inputs_hash}.`,
    );
  }
  if (badInputIndex >= BigInt(spendInputCbors.length)) {
    throw new Error(
      `--bad-input-index ${badInputIndex.toString()} is out of bounds for ${spendInputCbors.length.toString()} inputs.`,
    );
  }
  const inputsPreimage = spendInputsWitnessFromCbors(
    spendInputCbors,
    "--inputs",
  ).inputs;
  const missingInput = inputsPreimage[Number(badInputIndex)]!;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const spendInputIndex = inputIndex(threadUtxo, feeInput);
  const redeemer = Data.to(
    {
      Continue: [
        {
          input_index: spendInputIndex,
          output_index: STEP_02_OUTPUT_INDEX,
          inputs_preimage: [...inputsPreimage],
          bad_input_index: badInputIndex,
        },
      ],
    },
    NonExistentInputStep02SpendRedeemer,
  );
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        missing_input: missingInput,
        blocks_prev_utxos_root: inputDatum.data.blocks_prev_utxos_root,
        blocks_transactions_root: inputDatum.data.blocks_transactions_root,
      },
    },
    NonExistentInputStep03Datum,
  );
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(steps[1].spendingScript);

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
    nextThreadOutRef: `${txHash}#${STEP_02_OUTPUT_INDEX.toString()}`,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: steps[1].spendingScriptAddress,
    thirdStepAddress: steps[2].spendingScriptAddress,
    missingInput,
    badInputIndex: Number(badInputIndex),
    inputIndex: Number(spendInputIndex),
    outputIndex: Number(STEP_02_OUTPUT_INDEX),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const neSubmitStep02FromFiles = async (
  config: NeSubmitStep02CliConfig,
): Promise<NeSubmitStep02Result> => {
  const [blueprint, deploymentInfo, inputsJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.inputsPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await neSubmitStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    spendInputCbors: parseInputsPreimage(inputsJson),
    badInputIndex: BigInt(config.badInputIndex),
    awaitConfirmation: config.awaitConfirmation,
  });
};
