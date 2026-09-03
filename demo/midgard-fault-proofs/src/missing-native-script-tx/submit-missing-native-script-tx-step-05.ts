import {
  MissingNativeScriptTxStep05Datum,
  MissingNativeScriptTxStep05SpendRedeemer,
  type MissingNativeScriptTxStep05State,
  MissingNativeScriptTxStep06Datum,
  missingNativeScriptTxStep06ReadyState,
  missingNativeScriptTxVersionedScriptHash,
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
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary-v1.js";
import type { MissingNativeScriptTxContracts } from "./contracts-v1.js";
import {
  missingNativeScriptTxStepLabel,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxReferenceScript,
  requireMissingNativeScriptTxStepState,
  requireMissingNativeScriptTxThreadUtxo,
} from "./submit-common-v1.js";

const STEP_LABEL = missingNativeScriptTxStepLabel(4);

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
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingNativeScriptTxContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly missingNativeScriptBytes: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingNativeScriptTxStep05Result> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: 4,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep05State =
    requireMissingNativeScriptTxStepState({
      threadUtxo,
      signer,
      schema: MissingNativeScriptTxStep05Datum,
      stepIndex: 4,
    });
  const derived = missingNativeScriptTxVersionedScriptHash(
    missingNativeScriptBytes,
  );
  if (derived !== state.expected_missing_script_hash) {
    throw missingNativeScriptTxSubmitError(
      `native script hashes to ${derived}, not the accused credential ${state.expected_missing_script_hash}.`,
    );
  }
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: missingNativeScriptTxStep06ReadyState(state),
    },
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
      requireMissingNativeScriptTxReferenceScript({
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
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof missing-native-script-tx step-05",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[4].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingNativeScriptTxSubmitError(
      `step-05 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
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
