import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ProtectedOutputSignerMissingContractsV1 } from "./contracts-v1.js";
import { actuateProtectedOutputSignerFieldOpeningV1 } from "./field-opening-actuation-v1.js";
import { planProtectedOutputSignerOutputOpeningV1 } from "./field-plans-v1.js";
import type { ProtectedOutputSignerMissingEvidenceV1 } from "./protected-output-signer-missing-v1.js";
import {
  ProtectedOutputSignerStep02DatumV1Schema,
  ProtectedOutputSignerStep02RedeemerV1Schema,
  ProtectedOutputSignerStep03DatumV1Schema,
} from "./schemas-v1.js";
import { submitProtectedOutputSignerOpeningTransitionV1 } from "./submit-opening-transition-v1.js";

export const submitProtectedOutputSignerMissingStep02V1 = async ({
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
  readonly contracts: ProtectedOutputSignerMissingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ProtectedOutputSignerMissingEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReferenceScriptUtxo: UTxO;
  readonly publicationBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly certificateBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly onCarriageReady?: () => Promise<void>;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const current = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "protected-output-signer-missing",
    stepIndex: 1,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    bound: { subject: unknown; output_index: bigint };
    witness_set_hash: string;
  }>({
    threadUtxo: current.threadUtxo,
    signer,
    schema: ProtectedOutputSignerStep02DatumV1Schema as never,
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
  const planned = planProtectedOutputSignerOutputOpeningV1({
    evidence,
    nativeTxCompactCbor,
    owner: signer.paymentKeyHash,
  });
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "protected-output-signer-missing",
    stepIndex: 1,
  });
  const actuated = await actuateProtectedOutputSignerFieldOpeningV1({
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
    ProtectedOutputSignerStep03DatumV1Schema as never,
  );
  return await submitProtectedOutputSignerOpeningTransitionV1({
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
    redeemerSchema: ProtectedOutputSignerStep02RedeemerV1Schema as never,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
