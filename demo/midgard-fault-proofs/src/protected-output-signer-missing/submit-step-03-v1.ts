import { missingSignatureFieldWalkCheckpointV1 } from "@al-ft/midgard-sdk";
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
import { planProtectedOutputSignerWitnessOpeningV1 } from "./field-plans-v1.js";
import type { ProtectedOutputSignerMissingEvidenceV1 } from "./protected-output-signer-missing-v1.js";
import {
  ProtectedOutputSignerStep03DatumV1Schema,
  ProtectedOutputSignerStep03RedeemerV1Schema,
  ProtectedOutputSignerStep04DatumV1Schema,
} from "./schemas-v1.js";
import { submitProtectedOutputSignerOpeningTransitionV1 } from "./submit-opening-transition-v1.js";

export const submitProtectedOutputSignerMissingStep03V1 = async ({
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
  publishedCarriageUtxos,
  certificateUtxo,
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
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const current = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "protected-output-signer-missing",
    stepIndex: 2,
    threadOutRef,
  });
  const protectedState = requireLinearFaultStepStateV1<{
    subject: unknown;
    transaction_id: string;
    witness_set_hash: string;
    output_index: bigint;
    payment_credential: string;
  }>({
    threadUtxo: current.threadUtxo,
    signer,
    schema: ProtectedOutputSignerStep03DatumV1Schema as never,
    family: "protected-output-signer-missing",
    stepIndex: 2,
  });
  if (
    protectedState.transaction_id !== evidence.subject.transaction_id ||
    protectedState.witness_set_hash !== evidence.witnessSetHashHex ||
    protectedState.output_index !== BigInt(evidence.outputIndex) ||
    protectedState.payment_credential !== evidence.paymentCredentialHex
  )
    throw new Error(
      "protected-output-signer-missing: protected credential checkpoint changed",
    );
  const planned = planProtectedOutputSignerWitnessOpeningV1({
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
  });
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: "protected-output-signer-missing",
    stepIndex: 2,
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
    label: "protected-output-signer-missing address witnesses",
    onReady: onCarriageReady,
    publishedCarriageUtxos,
    suppliedCertificateUtxo: certificateUtxo,
  });
  const checkpoint = missingSignatureFieldWalkCheckpointV1({
    txId: evidence.subject.transaction_id,
    itemCount: planned.itemCount,
    totalLength: planned.preimage.length,
    nextItemIndex: 0,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        protected: protectedState,
        checkpoint_hash: checkpoint.checkpointHash,
        signer_present: false,
      },
    } as never,
    ProtectedOutputSignerStep04DatumV1Schema as never,
  );
  const result = await submitProtectedOutputSignerOpeningTransitionV1({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    stepIndex: 2,
    nextStepIndex: 3,
    nextDatum,
    opening: actuated.opening,
    referenceScriptUtxo,
    carriageReferenceInputs: actuated.referenceInputs,
    redeemerSchema: ProtectedOutputSignerStep03RedeemerV1Schema as never,
    preSubmitBoundary,
    awaitConfirmation,
  });
  return {
    ...result,
    carriageUtxos: actuated.carriageUtxos,
    certificateUtxo: actuated.certificateUtxo,
  };
};
