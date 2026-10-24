import {
  InvalidSignatureStep01SpendRedeemer,
  InvalidSignatureStep02Datum,
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
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { requireInitialStepDatum } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { InvalidSignatureContracts } from "./contracts.js";
import type { InvalidSignatureWrongfulRejectionEvidence } from "./wrongful-rejection.js";

export const submitInvalidSignatureStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  forcedSource,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: InvalidSignatureContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: InvalidSignatureWrongfulRejectionEvidence;
  forcedSource: Readonly<Record<string, unknown>>;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  if (evidence.subject.direction !== 1n)
    throw new Error("invalidSignature: forced direction changed");
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "invalid-signature",
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: "invalid-signature",
    stepIndex: 0,
  });
  const datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        bad_tx_witness_set_hash: evidence.witnessSetHash,
      },
    } as never,
    InvalidSignatureStep02Datum as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "invalid-signature forced step-01");
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "invalid-signature forced output",
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                ...forcedSource,
                input_index: requireInputIndex(
                  ctx,
                  threadUtxo,
                  "invalid-signature",
                ),
                output_index: outputIndex,
              },
            },
          },
        ],
      } as never,
      InvalidSignatureStep01SpendRedeemer as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "invalid-signature step-01 forced",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("invalidSignature: layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
