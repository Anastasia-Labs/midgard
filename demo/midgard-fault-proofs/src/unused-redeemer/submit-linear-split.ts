import {
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
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { UnusedRedeemerContracts } from "./contracts.js";

const FAMILY = "unused-redeemer";

export const submitUnusedRedeemerLinearSplit = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stepIndex,
  nextState,
  sourceDatumSchema,
  nextDatumSchema,
  redeemerSchema,
  redeemerFields,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: UnusedRedeemerContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  stepIndex: number;
  nextState: unknown;
  sourceDatumSchema: unknown;
  nextDatumSchema: unknown;
  redeemerSchema: unknown;
  redeemerFields: Readonly<Record<string, unknown>>;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultStepState({
    threadUtxo,
    signer,
    schema: sourceDatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    nextDatumSchema as never,
  );
  const nextAddress = contracts.steps[stepIndex + 1]?.spendingScriptAddress;
  if (nextAddress === undefined)
    throw new Error(`${FAMILY}: successor changed`);
  const outputMatches = computationThreadOutputPredicate({
    address: nextAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex]?.spendingScriptHash ?? "",
    family: FAMILY,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} split step`);
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} split step`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} split step`,
    );
    return Data.to(
      {
        Continue: [
          {
            ...redeemerFields,
            input_index: inputIndex,
            output_index: outputIndex,
          },
        ],
      } as never,
      redeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex]?.spendingScript ?? "",
    stepRole: `${FAMILY} split step ${String(stepIndex)}`,
    nextAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
