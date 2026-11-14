import {
  type NativeScriptDecodingBindState,
  NativeScriptDecodingStep01SpendRedeemer,
  NativeScriptDecodingStep02Datum,
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

import { type ResolvedProverSigner } from "../runtime.js";
import { completeStep, fundStep, submitStep } from "../step-completion.js";
import { requireInitialStepDatum } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type { NativeScriptDecodingContracts } from "./contracts.js";
import {
  nativeScriptDecodingSubmitError,
  requireNativeScriptDecodingThreadUtxo,
} from "./submit-common.js";
import {
  sourceStepScript,
  STEP_LABEL,
  type SubmitNativeScriptDecodingStep01Result,
} from "./submit-native-script-decoding-step-01.source-step-script.js";

export const submitNativeScriptDecodingStep01RecordForced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** 0 (wrongful acceptance) or 1 (wrongful rejection); fixed for the thread's life. */
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep01Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  requireInitialStepDatum({ threadUtxo, signer });
  const bindState: NativeScriptDecodingBindState = "PendingForced";

  const funding = await fundStep(lucid, { signer });
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: bindState },
    NativeScriptDecodingStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            RecordForcedSource: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
            },
          },
        ],
      },
      NativeScriptDecodingStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = sourceStepScript({
    tx: funding.tx
      .collectFrom([threadUtxo], redeemer)
      .pay.ToContract(
        contracts.steps[1].spendingScriptAddress,
        { kind: "inline", value: step02Datum },
        threadAssets,
      ),
    contracts,
    referenceScriptUtxo,
  });

  const signed = await completeStep(tx, funding);
  if (resolvedLayout === undefined) {
    throw nativeScriptDecodingSubmitError(
      "BuildTxWithRedeemer did not resolve the step-01 layout.",
    );
  }
  const txHash = await submitStep(lucid, signed, {
    label: `${STEP_LABEL} record-forced`,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof native-script-decoding step-01",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[0].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
    awaitConfirmation,
  });

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    bindState,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
