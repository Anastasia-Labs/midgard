import type {
  BuildTxWithRedeemer,
  LucidEvolution,
  Script,
  UTxO,
} from "@lucid-evolution/lucid";

import {
  completeStep,
  fundStep,
  type StepSigner,
  submitStep,
} from "./step-completion.js";
import type { FraudProofPreSubmitBoundary } from "./workflow/transaction-boundary.js";
import { workflowReferenceScript } from "./workflow/transaction-boundary.js";

export const submitLinearFaultContinue = async ({
  lucid,
  signer,
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
  readonly signer: StepSigner;
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
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation: boolean;
}) => {
  const funding = await fundStep(lucid, { signer, excluding: carriageUtxos });
  const signed = await completeStep(
    funding.tx
      .collectFrom([threadUtxo], redeemer)
      .readFrom([...carriageUtxos, stepReference, ...extraReferenceInputs])
      .pay.ToContract(
        nextAddress,
        { kind: "inline", value: nextDatum },
        {
          lovelace: threadUtxo.assets.lovelace ?? 0n,
          [threadUnit]: 1n,
        },
      ),
    funding,
  );
  return await submitStep(lucid, signed, {
    label: stepRole,
    referenceScripts: [
      workflowReferenceScript({
        role: stepRole,
        utxo: stepReference,
        expectedScript: stepScript,
      }),
    ],
    boundary: preSubmitBoundary,
    awaitConfirmation,
  });
};
