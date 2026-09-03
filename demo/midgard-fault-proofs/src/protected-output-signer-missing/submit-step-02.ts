import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ProtectedOutputSignerMissingContracts } from "./contracts.js";
import { actuateProtectedOutputSignerFieldOpening } from "./field-opening-actuation.js";
import { planProtectedOutputSignerOutputOpening } from "./field-plans.js";
import type { ProtectedOutputSignerMissingEvidence } from "./protected-output-signer-missing.js";
import {
  ProtectedOutputSignerStep02DatumSchema,
  ProtectedOutputSignerStep02RedeemerSchema,
  ProtectedOutputSignerStep03DatumSchema,
} from "./schemas.js";
import { submitProtectedOutputSignerOpeningTransition } from "./submit-opening-transition.js";

export const submitProtectedOutputSignerMissingStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  referenceScriptUtxo,
  certificateReferenceScriptUtxo,
  publicationBoundary,
  certificateBoundary,
  onCarriageReady,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ProtectedOutputSignerMissingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ProtectedOutputSignerMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReferenceScriptUtxo: UTxO;
  readonly publicationBoundary?: FraudProofPreSubmitBoundary;
  readonly certificateBoundary?: FraudProofPreSubmitBoundary;
  readonly onCarriageReady?: () => Promise<void>;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const current = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "protected-output-signer-missing",
    stepIndex: 1,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    bound: { subject: unknown; output_index: bigint };
    witness_set_hash: string;
  }>({
    threadUtxo: current.threadUtxo,
    signer,
    schema: ProtectedOutputSignerStep02DatumSchema as never,
    family: "protected-output-signer-missing",
    stepIndex: 1,
  });
  if (
    state.bound.output_index !== BigInt(evidence.outputIndex) ||
    state.witness_set_hash !== evidence.witnessSetHashHex
  )
    throw new Error(
      "protected-output-signer-missing: output coordinate or witness anchor changed",
    );
  const planned = planProtectedOutputSignerOutputOpening({
    evidence,
    nativeTxCompactCbor,
    owner: signer.paymentKeyHash,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "protected-output-signer-missing",
    stepIndex: 1,
  });
  const actuated = await actuateProtectedOutputSignerFieldOpening({
    lucid,
    contracts,
    signer,
    planned,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    stepReference,
    certificateReferenceScriptUtxo,
    publicationBoundary,
    certificateBoundary,
    label: "protected-output-signer-missing outputs",
    onReady: onCarriageReady,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        transaction_id: evidence.subject.transaction_id,
        witness_set_hash: evidence.witnessSetHashHex,
        output_index: BigInt(evidence.outputIndex),
        payment_credential: evidence.paymentCredentialHex,
      },
    } as never,
    ProtectedOutputSignerStep03DatumSchema as never,
  );
  return await submitProtectedOutputSignerOpeningTransition({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    stepIndex: 1,
    nextStepIndex: 2,
    nextDatum,
    opening: actuated.opening,
    referenceScriptUtxo,
    carriageReferenceInputs: actuated.referenceInputs,
    redeemerSchema: ProtectedOutputSignerStep02RedeemerSchema as never,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
