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
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerContractsV1 } from "./contracts-v1.js";

const FAMILY = "unused-redeemer";

export const submitUnusedRedeemerLinearSplitV1 = async ({
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
  contracts: UnusedRedeemerContractsV1;
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
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultStepStateV1({
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
  const stepReference = requireLinearFaultReferenceScriptV1({
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
  const txHash = await submitLinearFaultContinueV1({
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
