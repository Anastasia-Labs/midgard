import {
  MissingNativeScriptTxStep05Datum,
  MissingNativeScriptTxStep05SpendRedeemer,
  type MissingNativeScriptTxStep05State,
  MissingNativeScriptTxStep06Datum,
  missingNativeScriptTxVersionedScriptHashV1,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { MissingNativeScriptTxContractsV1 } from "./contracts-v1.js";
import {
  missingNativeScriptTxStepLabelV1,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxReferenceScriptV1,
  requireMissingNativeScriptTxStepStateV1,
  requireMissingNativeScriptTxThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = missingNativeScriptTxStepLabelV1(4);

export type SubmitMissingNativeScriptTxStep05Result = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly expectedMissingScriptHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMissingNativeScriptTxStep05 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  missingNativeScriptBytes,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingNativeScriptTxContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly missingNativeScriptBytes: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingNativeScriptTxStep05Result> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 4,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep05State =
    requireMissingNativeScriptTxStepStateV1({
      threadUtxo,
      signer,
      schema: MissingNativeScriptTxStep05Datum,
      stepIndex: 4,
    });
  const derived = missingNativeScriptTxVersionedScriptHashV1(
    missingNativeScriptBytes,
  );
  if (derived !== state.expected_missing_script_hash) {
    throw missingNativeScriptTxSubmitError(
      `native script hashes to ${derived}, not the accused credential ${state.expected_missing_script_hash}.`,
    );
  }
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state },
    MissingNativeScriptTxStep06Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[5].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  let layout: { inputIndex: bigint; outputIndex: bigint } | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const resolved = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    layout = resolved;
    return Data.to(
      {
        Continue: [
          {
            input_index: resolved.inputIndex,
            output_index: resolved.outputIndex,
            missing_native_script_bytes: Buffer.from(
              missingNativeScriptBytes,
            ).toString("hex"),
          },
        ],
      },
      MissingNativeScriptTxStep05SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      requireMissingNativeScriptTxReferenceScriptV1({
        utxo: referenceScriptUtxo,
        expectedScriptHash: contracts.steps[4].spendingScriptHash,
        stepIndex: 4,
      }),
    ])
    .pay.ToContract(
      contracts.steps[5].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await base.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw missingNativeScriptTxSubmitError(
      "BuildTxWithRedeemer did not resolve step-05 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    expectedMissingScriptHash: state.expected_missing_script_hash,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
