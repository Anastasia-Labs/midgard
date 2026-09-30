import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  fieldOpeningForField,
  MIDGARD_FIELD_INDEX,
  MissingNativeScriptTxStep03Datum,
  MissingNativeScriptTxStep03SpendRedeemer,
  type MissingNativeScriptTxStep03State,
  MissingNativeScriptTxStep04Datum,
  MissingNativeScriptTxStep04SpendRedeemer,
  type MissingNativeScriptTxStep04State,
  MissingNativeScriptTxStep05Datum,
  MissingNativeScriptTxStep05SpendRedeemer,
  type MissingNativeScriptTxStep05State,
  MissingNativeScriptTxStep06Datum,
  missingNativeScriptTxStep06ReadyState,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireMissingNativeScriptTxStepState,
  requireMissingNativeScriptTxThreadUtxo,
} from "../../src/missing-native-script-tx/submit-common.js";
import { submitMissingNativeScriptTxBinding } from "../../src/missing-native-script-tx/submit-native-binding.js";
import type { SubmitStep01TxInclusion } from "../../src/step-support.js";
import { selectFeeInput } from "../../src/step-support.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  makeMissingNativeScriptTxEmulatorHarness,
  type RawAdvanceStep,
} from "./missing-native-script-tx-emulator.setup-missing-native-script-tx-fixture.js";
import { network } from "./submit-init-emulator-shared.js";

const submitRawAdvance = async ({
  harness,
  stepIndex,
  threadOutRef,
  nextDatum,
  redeemerSchema,
  makeArgs,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly stepIndex: RawAdvanceStep;
  readonly threadOutRef: string;
  readonly nextDatum: string;
  readonly redeemerSchema: Parameters<typeof Data.to>[1];
  readonly makeArgs: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
  }) => unknown;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxo({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      stepIndex,
      threadOutRef,
    });
  harness.proverSigner.selectWallet(harness.proverLucid);
  const feeInput = selectFeeInput(
    await harness.proverLucid.wallet().getUtxos(),
  );
  const outputMatches = computationThreadOutputPredicate({
    address: harness.family.steps[stepIndex + 1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw missing-native-script-tx");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "raw missing-native-script-tx",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "raw missing-native-script-tx output",
      ),
    };
    return Data.to({ Continue: [makeArgs(layout)] }, redeemerSchema);
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await harness.proverLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([referenceScriptUtxo])
    .pay.ToContract(
      harness.family.steps[stepIndex + 1].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(harness.proverSigner.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  return txHash;
};

export const submitRawMissingNativeScriptTxStep03 = async ({
  harness,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxo({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep03State =
    requireMissingNativeScriptTxStepState({
      threadUtxo,
      signer: harness.proverSigner,
      schema: MissingNativeScriptTxStep03Datum,
      stepIndex: 2,
    });
  const nextDatum = Data.to(
    {
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: {
        producing_tx_id: txInclusion.nativeTxId,
        bad_input_output_index: state.input_with_missing_script.output_index,
        bad_tx_id: state.bad_tx_id,
        bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
      },
    },
    MissingNativeScriptTxStep04Datum,
  );
  const result = await submitMissingNativeScriptTxBinding({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.family,
    signer: harness.proverSigner,
    stepIndex: 2,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef,
    txInclusion,
    nextDatum,
    spendRedeemerSchema: MissingNativeScriptTxStep03SpendRedeemer,
    referenceScriptUtxo,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });
  return result.txHash;
};

export const submitRawMissingNativeScriptTxStep04 = async ({
  harness,
  threadOutRef,
  nativeTxCompactCbor,
  outputItemCbors,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly outputItemCbors: readonly Uint8Array[];
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo } = await requireMissingNativeScriptTxThreadUtxo({
    lucid: harness.proverLucid,
    contracts: harness.family,
    categoryId: harness.category.categoryId,
    stepIndex: 3,
    threadOutRef,
  });
  const state: MissingNativeScriptTxStep04State =
    requireMissingNativeScriptTxStepState({
      threadUtxo,
      signer: harness.proverSigner,
      schema: MissingNativeScriptTxStep04Datum,
      stepIndex: 3,
    });
  const nextDatum = Data.to(
    {
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: {
        expected_missing_script_hash: "44".repeat(28),
        bad_tx_id: state.bad_tx_id,
        bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
      },
    },
    MissingNativeScriptTxStep05Datum,
  );
  const opening = fieldOpeningForField({
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    nativeTxCompactCbor,
    carriage: {
      Inline: {
        preimage: encodeMidgardFieldPreimage(
          outputItemCbors.map((item) => Buffer.from(item)),
        ).toString("hex"),
      },
    },
  });
  return await submitRawAdvance({
    harness,
    stepIndex: 3,
    threadOutRef,
    nextDatum,
    redeemerSchema: MissingNativeScriptTxStep04SpendRedeemer,
    makeArgs: ({ inputIndex, outputIndex }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      outputs_opening: opening,
    }),
    referenceScriptUtxo,
  });
};

export const submitRawMissingNativeScriptTxStep05 = async ({
  harness,
  threadOutRef,
  missingNativeScriptBytes,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly missingNativeScriptBytes: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo } = await requireMissingNativeScriptTxThreadUtxo({
    lucid: harness.proverLucid,
    contracts: harness.family,
    categoryId: harness.category.categoryId,
    stepIndex: 4,
    threadOutRef,
  });
  const state: MissingNativeScriptTxStep05State =
    requireMissingNativeScriptTxStepState({
      threadUtxo,
      signer: harness.proverSigner,
      schema: MissingNativeScriptTxStep05Datum,
      stepIndex: 4,
    });
  const nextDatum = Data.to(
    {
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: missingNativeScriptTxStep06ReadyState(state),
    },
    MissingNativeScriptTxStep06Datum,
  );
  return await submitRawAdvance({
    harness,
    stepIndex: 4,
    threadOutRef,
    nextDatum,
    redeemerSchema: MissingNativeScriptTxStep05SpendRedeemer,
    makeArgs: ({ inputIndex, outputIndex }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      missing_native_script_bytes: Buffer.from(
        missingNativeScriptBytes,
      ).toString("hex"),
    }),
    referenceScriptUtxo,
  });
};
