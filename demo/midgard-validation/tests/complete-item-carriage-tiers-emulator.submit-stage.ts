import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core/codec/native-tx-carriage";
import { type MidgardFieldCarriage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  requireInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import { type BuildTxWithRedeemer, type UTxO } from "@lucid-evolution/lucid";

import {
  type Harness,
  SIGNER_HASH,
} from "./complete-item-carriage-tiers-emulator.outputs-for-field-two-preimage-bytes.js";
import {
  feeInputFor,
  type PublishedCarriage,
  sameDatumValue,
  signedByteCount,
  type StageContract,
  type StageResult,
  submitAndAwait,
} from "./complete-item-carriage-tiers-emulator.publish-carriage.js";

export const submitStage = async ({
  harness,
  inputUtxo,
  inputContract,
  outputContract,
  outputDatum,
  label,
  encode,
  scriptReference,
  carriageReferences,
}: {
  readonly harness: Harness;
  readonly inputUtxo: UTxO;
  readonly inputContract: StageContract;
  readonly outputContract: StageContract;
  readonly outputDatum: string;
  readonly label: string;
  readonly encode: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
  }) => string;
  readonly scriptReference?: UTxO;
  readonly carriageReferences?: readonly UTxO[];
}): Promise<StageResult> => {
  const makeRedeemer: BuildTxWithRedeemer = (ctx) =>
    encode({
      inputIndex: requireInputIndex(ctx, inputUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        (output) =>
          output.address === outputContract.spendingScriptAddress &&
          output.datum != null &&
          sameDatumValue(output.datum, outputDatum) &&
          output.assets[harness.threadUnit] === 1n,
        label,
      ),
    });
  let tx = harness.lucid
    .newTx()
    .collectFrom([await feeInputFor(harness)])
    .collectFrom([inputUtxo], makeRedeemer);
  if (scriptReference !== undefined) {
    tx = tx.readFrom([scriptReference]);
  }
  if (carriageReferences !== undefined && carriageReferences.length > 0) {
    tx = tx.readFrom([...carriageReferences]);
  }
  tx = tx.pay
    .ToContract(
      outputContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      {
        lovelace: inputUtxo.assets.lovelace ?? 0n,
        [harness.threadUnit]: 1n,
      },
    )
    .addSignerKey(SIGNER_HASH);
  if (scriptReference === undefined) {
    tx = tx.attach.SpendingValidator(inputContract.spendingScript);
  }
  let unsigned;
  try {
    unsigned = await tx.complete({ localUPLCEval: true });
  } catch (cause) {
    throw new Error(
      `${label} local evaluation failed: ${
        cause instanceof Error ? cause.message : String(cause)
      }`,
    );
  }
  const { txHash, signedCbor } = await submitAndAwait(harness, unsigned);
  const nextThreadUtxo = (
    await harness.lucid.utxosAt(outputContract.spendingScriptAddress)
  ).find(
    (utxo) => utxo.txHash === txHash && utxo.assets[harness.threadUnit] === 1n,
  );
  if (nextThreadUtxo === undefined) {
    throw new Error(`${label} did not hand the thread on`);
  }
  return {
    nextThreadUtxo,
    signedBytes: signedByteCount(signedCbor),
    outputDatum,
  };
};

// ## One journey per tier

export type TierJourney = {
  readonly tier: MidgardFieldCarriage["carriage"];
  readonly preimageBytes: number;
  readonly committedCarriage: MidgardFieldCarriage;
  readonly auxiliaryBytes: number;
  readonly doorReferenceInputs: readonly UTxO[];
  readonly published: PublishedCarriage;
  readonly plan: MidgardFieldCarriagePlan;
  readonly observedDatum: string;
  readonly observedOnLedger: string;
  readonly stageBytes: Readonly<Record<string, number>>;
  readonly observation: {
    readonly itemCount: bigint;
    readonly itemLength: bigint;
  };
  readonly reobserve: (
    doorReferenceInputs: readonly UTxO[],
  ) => MidgardFieldCarriage;
};
