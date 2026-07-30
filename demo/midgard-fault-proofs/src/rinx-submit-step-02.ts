import {
  type MidgardTxInput,
  ReferenceInputNoIdxStep02Datum,
  ReferenceInputNoIdxStep02SpendRedeemer,
  ReferenceInputNoIdxStep03Datum,
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

import { type NriReferenceInputPreimageEntry } from "./nri-submit-step-02.js";
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
import { computationThreadOutputPredicate } from "./tx-layout.js";

const toMidgardTxInput = (
  entry: NriReferenceInputPreimageEntry,
): MidgardTxInput => ({
  tx_id: entry.txId,
  output_index: BigInt(entry.index),
});

export type RinxSubmitStep02CliConfig = SubmitProviderConfig & {
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

export type RinxSubmitStep02Result = {
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
  readonly badReferenceInput: MidgardTxInput;
  readonly badReferenceInputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step02DatumWithState = ReferenceInputNoIdxStep02Datum & {
  readonly data: NonNullable<ReferenceInputNoIdxStep02Datum["data"]>;
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
  const datum = Data.from(threadUtxo.datum, ReferenceInputNoIdxStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Step 02 input datum must carry verified-tx-reference-inputs-hash state data.",
    );
  }
  return datum as Step02DatumWithState;
};

export const rinxSubmitStep02 = async ({
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
}): Promise<RinxSubmitStep02Result> => {
  const { referenceInputNoIdxCategory, contracts } =
    await resolveReferenceInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const steps = contracts.referenceInputNoIdx.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "reference-input-no-idx step-02 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at reference-input-no-idx step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: referenceInputNoIdxCategory.categoryId,
    categoryLabel: "reference-input-no-idx",
  });
  requireStep02Datum({ threadUtxo, signer });

  if (
    badReferenceInputIndex < 0n ||
    badReferenceInputIndex >= BigInt(referenceInputsPreimage.length)
  ) {
    throw new Error(
      `badReferenceInputIndex ${badReferenceInputIndex.toString()} is out of bounds for ${referenceInputsPreimage.length.toString()} reference inputs.`,
    );
  }
  const midgardReferenceInputs = referenceInputsPreimage.map(toMidgardTxInput);
  const badReferenceInput =
    midgardReferenceInputs[Number(badReferenceInputIndex)]!;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bad_reference_input_tx_id: badReferenceInput.tx_id,
        bad_reference_input_output_index: badReferenceInput.output_index,
      },
    },
    ReferenceInputNoIdxStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: { inputIndex: bigint; outputIndex: bigint } | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "reference-input-no-idx step 02");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "reference-input-no-idx step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        "reference-input-no-idx step 02 output",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            reference_inputs_preimage: midgardReferenceInputs,
            bad_reference_input_index: badReferenceInputIndex,
          },
        ],
      },
      ReferenceInputNoIdxStep02SpendRedeemer,
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
      "BuildTxWithRedeemer did not resolve reference-input-no-idx step 02 layout.",
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
    badReferenceInput,
    badReferenceInputIndex: Number(badReferenceInputIndex),
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const rinxSubmitStep02FromFiles = async (
  config: RinxSubmitStep02CliConfig,
): Promise<RinxSubmitStep02Result> => {
  const [blueprint, deploymentInfo, referenceInputsJson, lucid] =
    await Promise.all([
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.referenceInputsPreimagePath),
      makeLucidForSubmit(config),
    ]);
  const signer = resolveProverSigner(config);
  return await rinxSubmitStep02({
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
