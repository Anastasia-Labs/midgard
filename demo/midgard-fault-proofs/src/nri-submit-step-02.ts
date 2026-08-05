import {
  type MidgardTxInput,
  NoReferenceInputStep02Datum,
  NoReferenceInputStep02SpendRedeemer,
  NoReferenceInputStep03Datum,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
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
  resolveNoReferenceInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

/** One reference input of the bad transaction, as committed by its hash. */
export type NriReferenceInputPreimageEntry = {
  readonly txId: string;
  readonly index: number | bigint;
};

const toMidgardTxInput = (
  entry: NriReferenceInputPreimageEntry,
): MidgardTxInput => ({
  tx_id: entry.txId,
  output_index: BigInt(entry.index),
});

export type NriSubmitStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly referenceInputsPreimagePath: string;
  readonly badReferenceInputIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type NriSubmitStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly thirdStepAddress: string;
  readonly missingReferenceInput: MidgardTxInput;
  readonly badReferenceInputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step02DatumWithState = NoReferenceInputStep02Datum & {
  readonly data: NonNullable<NoReferenceInputStep02Datum["data"]>;
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
  const datum = Data.from(threadUtxo.datum, NoReferenceInputStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Step 02 input datum must carry bad-tx-reference-inputs state data.",
    );
  }
  return datum as Step02DatumWithState;
};

export const nriSubmitStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  referenceInputsPreimage,
  badReferenceInputIndex,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceInputsPreimage: readonly NriReferenceInputPreimageEntry[];
  readonly badReferenceInputIndex: bigint;
  readonly awaitConfirmation?: boolean;
}): Promise<NriSubmitStep02Result> => {
  const { noReferenceInputCategory, contracts } =
    await resolveNoReferenceInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const steps = contracts.noReferenceInput.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "no-reference-input step-02 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at no-reference-input step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: noReferenceInputCategory.categoryId,
    categoryLabel: "no-reference-input",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });

  if (
    badReferenceInputIndex < 0n ||
    badReferenceInputIndex >= BigInt(referenceInputsPreimage.length)
  ) {
    throw new Error(
      `badReferenceInputIndex ${badReferenceInputIndex.toString()} is out of bounds for ${referenceInputsPreimage.length.toString()} reference inputs.`,
    );
  }
  const midgardInputs = referenceInputsPreimage.map(toMidgardTxInput);
  const missingReferenceInput = midgardInputs[Number(badReferenceInputIndex)]!;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        missing_reference_input: missingReferenceInput,
        blocks_prev_utxos_root: inputDatum.data.blocks_prev_utxos_root,
        blocks_transactions_root: inputDatum.data.blocks_transactions_root,
      },
    },
    NoReferenceInputStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: { inputIndex: bigint; outputIndex: bigint } | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "no-reference-input step 02");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "no-reference-input step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        "no-reference-input step 02 output",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            reference_inputs_preimage: midgardInputs,
            bad_reference_input_index: badReferenceInputIndex,
          },
        ],
      },
      NoReferenceInputStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(steps[1].spendingScript)
    .complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve no-reference-input step 02 layout.",
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
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: steps[1].spendingScriptAddress,
    thirdStepAddress: steps[2].spendingScriptAddress,
    missingReferenceInput,
    badReferenceInputIndex: Number(badReferenceInputIndex),
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const nriSubmitStep02FromFiles = async (
  config: NriSubmitStep02CliConfig,
): Promise<NriSubmitStep02Result> => {
  const [blueprint, deploymentInfo, referenceInputsJson, lucid] =
    await Promise.all([
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.referenceInputsPreimagePath),
      makeLucidForSubmit(config),
    ]);
  const signer = resolveProverSigner(config);
  return await nriSubmitStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    referenceInputsPreimage:
      referenceInputsJson as readonly NriReferenceInputPreimageEntry[],
    badReferenceInputIndex: BigInt(config.badReferenceInputIndex),
    awaitConfirmation: config.awaitConfirmation,
  });
};
