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
import type { DistinctAssetAccumulationContracts } from "./contracts.js";
import {
  DistinctAssetStep02DatumSchema,
  DistinctAssetStep02RedeemerSchema,
  DistinctAssetStep03DatumSchema,
} from "./schemas.js";

export type DistinctAssetAccumulatorAuthentication = Readonly<{
  trace_membership: Readonly<Record<string, unknown>>;
  pre: Readonly<Record<string, unknown>>;
  trace_proof: Readonly<Record<string, unknown>>;
  control: Readonly<Record<string, unknown>>;
}>;

/** Advances the authenticated retained ValueAndMint state into the fold chain. */
export const submitDistinctAssetAccumulationStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  authentication,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DistinctAssetAccumulationContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly authentication: DistinctAssetAccumulatorAuthentication;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
    threadOutRef,
  });
  const bound = requireLinearFaultStepState<Record<string, unknown>>({
    threadUtxo,
    signer,
    schema: DistinctAssetStep02DatumSchema as never,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bound,
        control: authentication.control,
        stage: 0n,
        decisive_fault_holds: null,
      },
    } as never,
    DistinctAssetStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "distinctAssetAccumulationLimit step-02",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "distinctAssetAccumulationLimit",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "distinctAssetAccumulationLimit step-02 output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            ...authentication,
          },
        ],
      } as never,
      DistinctAssetStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[1].spendingScript,
    stepRole: "distinctAssetAccumulationLimit step-02",
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(
      "distinctAssetAccumulationLimit: step-02 layout unresolved",
    );
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
