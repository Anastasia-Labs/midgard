import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { planSignerWitnessScanTurn } from "../signer-witness-scan/signer-witness-scan.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ProtectedOutputSignerMissingContracts } from "./contracts.js";
import { actuateProtectedOutputSignerFieldOpening } from "./field-opening-actuation.js";
import { planProtectedOutputSignerWitnessOpening } from "./field-plans.js";
import {
  PROTECTED_OUTPUT_SIGNER_SCAN_BATCH,
  type ProtectedOutputSignerMissingEvidence,
} from "./protected-output-signer-missing.js";
import {
  ProtectedOutputSignerStep04DatumSchema,
  ProtectedOutputSignerStep04RedeemerSchema,
  ProtectedOutputSignerStep05DatumSchema,
} from "./schemas.js";
import { submitProtectedOutputSignerOpeningTransition } from "./submit-opening-transition.js";

export const submitProtectedOutputSignerMissingStep04 = async ({
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
  publishedCarriageUtxos,
  certificateUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
  ownOutputs = createOwnOutputJournal(),
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
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}) => {
  const current = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "protected-output-signer-missing",
    stepIndex: 3,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    protected: {
      subject: unknown;
      transaction_id: string;
      witness_set_hash: string;
      output_index: bigint;
      payment_credential: string;
    };
    checkpoint_hash: string;
    signer_present: boolean;
  }>({
    threadUtxo: current.threadUtxo,
    signer,
    schema: ProtectedOutputSignerStep04DatumSchema as never,
    family: "protected-output-signer-missing",
    stepIndex: 3,
  });
  if (
    state.protected.transaction_id !== evidence.subject.transaction_id ||
    state.protected.witness_set_hash !== evidence.witnessSetHashHex ||
    state.protected.payment_credential !== evidence.paymentCredentialHex
  )
    throw new Error(
      "protected-output-signer-missing: scan identity checkpoint changed",
    );
  const planned = planProtectedOutputSignerWitnessOpening({
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
  });
  const { checkpoint, signerPresent, exhausted, nextCheckpoint } =
    planSignerWitnessScanTurn({
      checkpoints: evidence.checkpoints,
      txId: evidence.subject.transaction_id,
      itemCount: planned.itemCount,
      totalLength: planned.preimage.length,
      checkpointHash: state.checkpoint_hash,
      batch: PROTECTED_OUTPUT_SIGNER_SCAN_BATCH,
    });
  // This family's scan always runs to the end of the field.
  const terminal = exhausted;
  const continuingCheckpointHash = (): string => {
    if (nextCheckpoint === undefined)
      throw new Error(
        "protected-output-signer-missing: a continuing scan has no next checkpoint",
      );
    return nextCheckpoint.checkpointHash;
  };
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[3].spendingScriptHash,
    family: "protected-output-signer-missing",
    stepIndex: 3,
  });
  const actuated = await actuateProtectedOutputSignerFieldOpening({
    ownOutputs,
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
    publishedCarriageUtxos,
    suppliedCertificateUtxo: certificateUtxo,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: terminal
        ? {
            subject: evidence.subject,
            signer_required: true,
            signer_present: signerPresent,
          }
        : {
            protected: state.protected,
            checkpoint_hash: continuingCheckpointHash(),
            signer_present: signerPresent,
          },
    } as never,
    (terminal
      ? ProtectedOutputSignerStep05DatumSchema
      : ProtectedOutputSignerStep04DatumSchema) as never,
  );
  const result = await submitProtectedOutputSignerOpeningTransition({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    stepIndex: 3,
    nextStepIndex: terminal ? 4 : 3,
    nextDatum,
    openingFor: actuated.openingFor,
    checkpointCbor: checkpoint.checkpointCbor,
    referenceScriptUtxo,
    carriageReferenceInputs: actuated.referenceInputs,
    redeemerSchema: ProtectedOutputSignerStep04RedeemerSchema as never,
    preSubmitBoundary,
    awaitConfirmation,
  });
  return {
    ...result,
    terminal,
    carriageUtxos: actuated.carriageUtxos,
    certificateUtxo: actuated.certificateUtxo,
  };
};
