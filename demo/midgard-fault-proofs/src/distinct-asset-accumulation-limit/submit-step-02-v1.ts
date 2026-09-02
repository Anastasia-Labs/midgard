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
import type { DistinctAssetAccumulationContractsV1 } from "./contracts-v1.js";
import {
  DistinctAssetStep02DatumV1Schema,
  DistinctAssetStep02RedeemerV1Schema,
  DistinctAssetStep03DatumV1Schema,
} from "./schemas-v1.js";

export type DistinctAssetAccumulatorAuthenticationV1 = Readonly<{
  trace_membership: Readonly<Record<string, unknown>>;
  pre: Readonly<Record<string, unknown>>;
  trace_proof: Readonly<Record<string, unknown>>;
  control: Readonly<Record<string, unknown>>;
}>;

/** Advances the authenticated retained ValueAndMint state into the fold chain. */
export const submitDistinctAssetAccumulationStep02V1 = async ({
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
  readonly contracts: DistinctAssetAccumulationContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly authentication: DistinctAssetAccumulatorAuthenticationV1;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
    threadOutRef,
  });
  const bound = requireLinearFaultStepStateV1<Record<string, unknown>>({
    threadUtxo,
    signer,
    schema: DistinctAssetStep02DatumV1Schema as never,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
  });
  const stepReference = requireLinearFaultReferenceScriptV1({
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
    DistinctAssetStep03DatumV1Schema as never,
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
      DistinctAssetStep02RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
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
