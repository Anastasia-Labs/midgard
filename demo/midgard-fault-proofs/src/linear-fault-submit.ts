import type {
  BuildTxWithRedeemer,
  LucidEvolution,
  Script,
  UTxO,
} from "@lucid-evolution/lucid";

import { completeStep, fundStep, type StepSigner } from "./step-completion.js";
import { finishStep } from "./step-completion.js";
import { workflowReferenceScript } from "./workflow/transaction-boundary.js";

export const buildLinearFaultContinue = async ({
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
  return finishStep(signed, {
    referenceScripts: [
      workflowReferenceScript({
        role: stepRole,
        utxo: stepReference,
        expectedScript: stepScript,
      }),
    ],
  });
};
