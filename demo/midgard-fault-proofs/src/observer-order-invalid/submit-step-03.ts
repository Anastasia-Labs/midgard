import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import { type FieldOpening } from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { resolveFaultProofFieldPreimageCertificate } from "../field-opening.certify-fault-proof-field-carriage.js";
import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import {
  faultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
} from "../field-opening.publish-fault-proof-field-carriage.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { ProofMaterialsAccess } from "../workflow/proof-materials-view.js";
import type { ObserverOrderInvalidContracts } from "./contracts.js";
import type { ObserverOrderInvalidEvidence } from "./family.js";
import {
  ObserverOrderInvalidStep03DatumSchema,
  ObserverOrderInvalidStep03RedeemerSchema,
  ObserverOrderInvalidStep04DatumSchema,
} from "./schemas.js";
import {
  encodeObserverOrderWalkCheckpoint,
  hashObserverOrderWalkCheckpoint,
  type ObserverOrderInvalidStagedPlan,
  observerOrderPrefix,
} from "./staged-plan.js";

export const buildObserverOrderInvalidStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  staged,
  walkOrdinal,
  referenceScriptUtxo,
  materials,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserverOrderInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserverOrderInvalidEvidence;
  readonly nativeTxCompactCbor: string;
  readonly staged: ObserverOrderInvalidStagedPlan;
  readonly walkOrdinal: number;
  readonly referenceScriptUtxo: UTxO;
  /** The workflow's view of its proof-materials address, on a workflow. */
  readonly materials?: ProofMaterialsAccess;
}) => {
  const nextCheckpoint = staged.walk[walkOrdinal];
  if (nextCheckpoint === undefined)
    throw new Error("observerOrderInvalid: walk ordinal is outside plan");
  const priorCheckpoint =
    walkOrdinal === 0 ? staged.initialWalk : staged.walk[walkOrdinal - 1]!;
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "observer-order-invalid",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    observer_index: bigint;
    checkpoint_hash: string;
    seen: bigint;
    previous_observer: string;
    outcome: bigint;
  }>({
    threadUtxo,
    signer,
    schema: ObserverOrderInvalidStep03DatumSchema as never,
    family: "observer-order-invalid",
    stepIndex,
  });
  if (
    state.observer_index !== BigInt(evidence.observerIndex) ||
    state.checkpoint_hash !==
      hashObserverOrderWalkCheckpoint(priorCheckpoint) ||
    state.outcome !== 0n
  )
    throw new Error("observerOrderInvalid: scan datum/checkpoint changed");
  const prefix = observerOrderPrefix({
    items: staged.items,
    nextItemIndex: priorCheckpoint.nextItemIndex,
    observerIndex: evidence.observerIndex,
  });
  if (
    state.seen !== BigInt(prefix.seen) ||
    state.previous_observer !== prefix.previousObserver
  )
    throw new Error("observerOrderInvalid: scan accumulator changed");
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: 3,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: true,
    label: "observerOrderInvalid scan field 3",
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublications({
    materials,
    lucid,
    publisherAddress: signer.proofMaterialsAddress,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error("observerOrderInvalid: field carriage disappeared");
  const certificateUtxo = await resolveFaultProofFieldPreimageCertificate({
    lucid,
    network: lucid.config().network!,
    planned,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error("observerOrderInvalid: field certificate disappeared");
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: "observer-order-invalid",
    stepIndex,
  });
  const terminal = walkOrdinal === staged.walk.length - 1;
  const nextPrefix = terminal
    ? null
    : observerOrderPrefix({
        items: staged.items,
        nextItemIndex: nextCheckpoint.nextItemIndex,
        observerIndex: evidence.observerIndex,
      });
  const nextData = terminal
    ? {
        subject: evidence.subject,
        observer_index: BigInt(evidence.observerIndex),
        violation: evidence.violation,
      }
    : {
        subject: evidence.subject,
        observer_index: BigInt(evidence.observerIndex),
        checkpoint_hash: hashObserverOrderWalkCheckpoint(nextCheckpoint),
        seen: BigInt(nextPrefix!.seen),
        previous_observer: nextPrefix!.previousObserver,
        outcome: 0n,
      };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    (terminal
      ? ObserverOrderInvalidStep04DatumSchema
      : ObserverOrderInvalidStep03DatumSchema) as never,
  );
  const nextStep = terminal ? contracts.steps[3] : contracts.steps[2];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    const opening: FieldOpening = faultProofFieldOpening({
      planned,
      referenceInputs: ctx.referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: "observerOrderInvalid scan field 3",
    });
    requireOwnSpendPurpose(ctx, threadUtxo, "observerOrderInvalid step-03");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "observerOrderInvalid",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "observerOrderInvalid step-03 output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            opening,
            checkpoint_bytes:
              encodeObserverOrderWalkCheckpoint(priorCheckpoint).toString(
                "hex",
              ),
            // An exhausted walk over an empty field advances nothing; the
            // validator still requires a positive budget to reach its end.
            item_budget: BigInt(
              Math.max(
                1,
                nextCheckpoint.nextItemIndex - priorCheckpoint.nextItemIndex,
              ),
            ),
          },
        ],
      } as never,
      ObserverOrderInvalidStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[2].spendingScript,
    stepRole: `observerOrderInvalid step-03 walk ${walkOrdinal.toString()}`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
  });
  if (outputIndex === undefined)
    throw new Error("observerOrderInvalid: step-03 layout unresolved");
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
  };
};
