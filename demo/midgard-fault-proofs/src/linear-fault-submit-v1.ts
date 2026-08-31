import type {
  BuildTxWithRedeemer,
  LucidEvolution,
  Script,
  UTxO,
} from "@lucid-evolution/lucid";

import { DEFAULT_CONFIRMATION_POLL_MS } from "./runtime.js";
import { excludeUtxo } from "./spend-input-witness.js";
import { selectFeeInput } from "./submit-step-01.js";
import type { FraudProofPreSubmitBoundaryV1 } from "./workflow/transaction-boundary-v1.js";
import {
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptV1,
} from "./workflow/transaction-boundary-v1.js";

export const submitLinearFaultContinueV1 = async ({
  lucid,
  signerPaymentKeyHash,
  threadUtxo,
  threadUnit,
  stepReference,
  stepScript,
  stepRole,
  nextAddress,
  nextDatum,
  redeemer,
  carriageUtxos = [],
  extraReferenceInputs = [],
  preSubmitBoundary,
  awaitConfirmation,
}: {
  readonly lucid: LucidEvolution;
  readonly signerPaymentKeyHash: string;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly stepReference: UTxO;
  readonly stepScript: Script;
  readonly stepRole: string;
  readonly nextAddress: string;
  readonly nextDatum: string;
  readonly redeemer: BuildTxWithRedeemer;
  readonly carriageUtxos?: readonly UTxO[];
  readonly extraReferenceInputs?: readonly UTxO[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation: boolean;
}) => {
  const walletUtxos = await lucid.wallet().getUtxos();
  const usableWalletUtxos = carriageUtxos.reduce<readonly UTxO[]>(
    (utxos, carriage) => excludeUtxo(utxos, carriage),
    walletUtxos,
  );
  const unsigned = await lucid
    .newTx()
    .collectFrom([selectFeeInput(usableWalletUtxos)])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([...carriageUtxos, stepReference, ...extraReferenceInputs])
    .pay.ToContract(
      nextAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadUnit]: 1n,
      },
    )
    .addSignerKey(signerPaymentKeyHash)
    .complete({
      localUPLCEval: true,
      ...(carriageUtxos.length === 0
        ? {}
        : { presetWalletInputs: usableWalletUtxos as UTxO[] }),
    });
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: [
      workflowReferenceScriptV1({
        role: stepRole,
        utxo: stepReference,
        expectedScript: stepScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `${stepRole}: provider returned ${txHash}, expected ${expectedTxHash}`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return txHash;
};
