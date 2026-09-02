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
  DistinctAssetStep03DatumV1Schema,
  DistinctAssetStep03RedeemerV1Schema,
  DistinctAssetStep04RedeemerV1Schema,
  DistinctAssetStep05RedeemerV1Schema,
} from "./schemas-v1.js";

export type DistinctAssetFoldActionV1 =
  | Readonly<{ kind: "skip" }>
  | Readonly<{
      kind: "authenticate";
      evidence: Readonly<{
        mutation: Readonly<{ delta_was_present: boolean }>;
        [field: string]: unknown;
      }>;
    }>;

type FoldState = Readonly<{
  bound: Readonly<{
    coordinate: Readonly<{ fold: bigint }>;
    [field: string]: unknown;
  }>;
  control: Readonly<{
    value_accumulator: Readonly<{ seen_asset_count: bigint }>;
    [field: string]: unknown;
  }> | null;
  stage: bigint;
  decisive_fault_holds: boolean | null;
}>;

/** Concrete continuation for the input, output, or mint physical fold. */
export const submitDistinctAssetAccumulationFoldV1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stepIndex,
  action,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DistinctAssetAccumulationContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stepIndex: 2 | 3 | 4;
  readonly action: DistinctAssetFoldActionV1;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const fold = stepIndex - 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<FoldState>({
    threadUtxo,
    signer,
    schema: DistinctAssetStep03DatumV1Schema as never,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
  });
  if (state.stage !== BigInt(fold) || state.decisive_fault_holds !== null)
    throw new Error(
      "distinctAssetAccumulationLimit: fold checkpoint stage changed",
    );
  if (action.kind === "skip" && state.bound.coordinate.fold === BigInt(fold))
    throw new Error("distinctAssetAccumulationLimit: target fold cannot skip");
  if (
    action.kind === "authenticate" &&
    state.bound.coordinate.fold !== BigInt(fold)
  )
    throw new Error(
      "distinctAssetAccumulationLimit: non-target fold cannot authenticate",
    );
  const control = state.control;
  if (control === null)
    throw new Error("distinctAssetAccumulationLimit: control disappeared");
  const decisiveFault =
    action.kind === "skip"
      ? null
      : !action.evidence.mutation.delta_was_present &&
        control.value_accumulator.seen_asset_count >= 16_384n;
  const nextState = {
    ...state,
    stage: BigInt(fold + 1),
    decisive_fault_holds: decisiveFault,
  };
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
  });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    DistinctAssetStep03DatumV1Schema as never,
  );
  const nextStep = contracts.steps[stepIndex + 1]!;
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemerSchemas = [
    DistinctAssetStep03RedeemerV1Schema,
    DistinctAssetStep04RedeemerV1Schema,
    DistinctAssetStep05RedeemerV1Schema,
  ] as const;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      `distinctAssetAccumulationLimit step-0${(stepIndex + 1).toString()}`,
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "distinctAssetAccumulationLimit",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "distinctAssetAccumulationLimit fold output",
    );
    const layout = { input_index: inputIndex, output_index: outputIndex };
    const familyAction =
      action.kind === "skip"
        ? { Skip: layout }
        : { Authenticate: { ...layout, evidence: action.evidence } };
    return Data.to(
      { Continue: [familyAction] } as never,
      redeemerSchemas[fold] as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: `distinctAssetAccumulationLimit fold-${fold.toString()}`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("distinctAssetAccumulationLimit: fold layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
