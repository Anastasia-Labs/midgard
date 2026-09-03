import { missingSignatureFieldWalkCheckpoint } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { ProtectedOutputSignerMissingContracts } from "./contracts-v1.js";
import { actuateProtectedOutputSignerFieldOpening } from "./field-opening-actuation-v1.js";
import { planProtectedOutputSignerWitnessOpening } from "./field-plans-v1.js";
import type { ProtectedOutputSignerMissingEvidence } from "./protected-output-signer-missing-v1.js";
import {
  ProtectedOutputSignerStep03DatumSchema,
  ProtectedOutputSignerStep03RedeemerSchema,
  ProtectedOutputSignerStep04DatumSchema,
} from "./schemas-v1.js";
import { submitProtectedOutputSignerOpeningTransition } from "./submit-opening-transition-v1.js";

export const submitProtectedOutputSignerMissingStep03 = async ({
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
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const current = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "protected-output-signer-missing",
    stepIndex: 2,
    threadOutRef,
  });
  const protectedState = requireLinearFaultStepState<{
    subject: unknown;
    transaction_id: string;
    witness_set_hash: string;
    output_index: bigint;
    payment_credential: string;
  }>({
    threadUtxo: current.threadUtxo,
    signer,
    schema: ProtectedOutputSignerStep03DatumSchema as never,
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
  const planned = planProtectedOutputSignerWitnessOpening({
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: "protected-output-signer-missing",
    stepIndex: 2,
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
    label: "protected-output-signer-missing address witnesses",
    onReady: onCarriageReady,
    publishedCarriageUtxos,
    suppliedCertificateUtxo: certificateUtxo,
  });
  const checkpoint = missingSignatureFieldWalkCheckpoint({
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
    ProtectedOutputSignerStep04DatumSchema as never,
  );
  const result = await submitProtectedOutputSignerOpeningTransition({
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
    redeemerSchema: ProtectedOutputSignerStep03RedeemerSchema as never,
    preSubmitBoundary,
    awaitConfirmation,
  });
  return {
    ...result,
    carriageUtxos: actuated.carriageUtxos,
    certificateUtxo: actuated.certificateUtxo,
  };
};
