import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  type FieldOpening,
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
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ObserverOrderInvalidContracts } from "./contracts.js";
import type { ObserverOrderInvalidEvidence } from "./family.js";
import {
  ObserverOrderInvalidStep02DatumSchema,
  ObserverOrderInvalidStep02RedeemerSchema,
  ObserverOrderInvalidStep03DatumSchema,
} from "./schemas.js";
import {
  hashObserverOrderWalkCheckpoint,
  type ObserverOrderInvalidStagedPlan,
} from "./staged-plan.js";

export type ObserverOrderInvalidStep02Action = Readonly<{
  kind: "authenticate";
}>;

export const submitObserverOrderInvalidStep02 = async ({
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
  readonly contracts: ObserverOrderInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserverOrderInvalidEvidence;
  readonly nativeTxCompactCbor: string;
  readonly staged: ObserverOrderInvalidStagedPlan;
  readonly action: ObserverOrderInvalidStep02Action;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "observer-order-invalid",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<Record<string, unknown>>({
    threadUtxo,
    signer,
    schema: ObserverOrderInvalidStep02DatumSchema as never,
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
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpening({
    fieldIndex: 3,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: true,
    label: "observerOrderInvalid field 3",
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublications({
    lucid,
    publisherAddress: signer.address,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error("observerOrderInvalid: field carriage disappeared");
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? await resolveFaultProofFieldPreimageCertificate({
          lucid,
          network: lucid.config().network!,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        })
      : undefined;
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error("observerOrderInvalid: field certificate disappeared");
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "observer-order-invalid",
    stepIndex,
  });
  const opening: FieldOpening = faultProofFieldOpening({
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
    checkpoint_hash: hashObserverOrderWalkCheckpoint(staged.initialWalk),
    seen: 0n,
    previous_observer: "",
    outcome: 0n,
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    ObserverOrderInvalidStep03DatumSchema as never,
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
      ObserverOrderInvalidStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
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
