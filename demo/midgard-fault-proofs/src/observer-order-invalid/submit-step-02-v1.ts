import { decodeMidgardFieldPreimageV1 } from "@al-ft/midgard-core";
import {
  type FieldOpeningV1,
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
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import {
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ObserverOrderInvalidContractsV1 } from "./contracts-v1.js";
import type { ObserverOrderInvalidEvidenceV1 } from "./family-v1.js";
import {
  ObserverOrderInvalidStep02DatumV1Schema,
  ObserverOrderInvalidStep02RedeemerV1Schema,
  ObserverOrderInvalidStep03DatumV1Schema,
} from "./schemas-v1.js";
import {
  hashObserverOrderWalkCheckpointV1,
  type ObserverOrderInvalidStagedPlanV1,
} from "./staged-plan-v1.js";

export type ObserverOrderInvalidStep02ActionV1 = Readonly<{
  kind: "authenticate";
}>;

export const submitObserverOrderInvalidStep02V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  staged,
  action,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserverOrderInvalidContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserverOrderInvalidEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly staged: ObserverOrderInvalidStagedPlanV1;
  readonly action: ObserverOrderInvalidStep02ActionV1;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "observer-order-invalid",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<Record<string, unknown>>({
    threadUtxo,
    signer,
    schema: ObserverOrderInvalidStep02DatumV1Schema as never,
    family: "observer-order-invalid",
    stepIndex,
  });
  const selected =
    "Bound" in state
      ? (state.Bound as { bound: Record<string, unknown> }).bound
      : undefined;
  if (
    selected === undefined ||
    selected.observer_index !== BigInt(evidence.observerIndex)
  )
    throw new Error("observerOrderInvalid: step-02 datum coordinate changed");
  const items = decodeMidgardFieldPreimageV1(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: 3,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: true,
    label: "observerOrderInvalid field 3",
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid,
    publisherAddress: signer.address,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error("observerOrderInvalid: field carriage disappeared");
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? await resolveFaultProofFieldPreimageCertificateV1({
          lucid,
          network: lucid.config().network!,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        })
      : undefined;
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error("observerOrderInvalid: field certificate disappeared");
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "observer-order-invalid",
    stepIndex,
  });
  const opening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs: [
      ...carriageUtxos,
      stepReference,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "observerOrderInvalid field 3",
  });
  const nextData = {
    subject: evidence.subject,
    observer_index: BigInt(evidence.observerIndex),
    checkpoint_hash: hashObserverOrderWalkCheckpointV1(staged.initialWalk),
    seen: 0n,
    previous_observer: "",
    outcome: 0n,
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    ObserverOrderInvalidStep03DatumV1Schema as never,
  );
  const nextStep = contracts.steps[2];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "observerOrderInvalid step-02");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "observerOrderInvalid",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "observerOrderInvalid step-02 output",
    );
    const common = {
      input_index: inputIndex,
      output_index: outputIndex,
      opening,
    };
    return Data.to(
      { Continue: [{ Authenticate: common }] } as never,
      ObserverOrderInvalidStep02RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[1].spendingScript,
    stepRole: `observerOrderInvalid step-02 ${action.kind}`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("observerOrderInvalid: step-02 layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
