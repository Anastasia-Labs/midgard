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
import {
  PROTECTED_OUTPUT_SIGNER_SCAN_BATCH_V1,
  type ProtectedOutputSignerMissingEvidenceV1,
} from "./protected-output-signer-missing-v1.js";
import {
  ProtectedOutputSignerStep04DatumV1Schema,
  ProtectedOutputSignerStep04RedeemerV1Schema,
  ProtectedOutputSignerStep05DatumV1Schema,
} from "./schemas-v1.js";
import { submitProtectedOutputSignerOpeningTransitionV1 } from "./submit-opening-transition-v1.js";

export const submitProtectedOutputSignerMissingStep04V1 = async ({
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
    stepIndex: 3,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
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
    schema: ProtectedOutputSignerStep04DatumV1Schema as never,
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
  const planned = planProtectedOutputSignerWitnessOpeningV1({
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
  });
  const candidates: number[] = [];
  for (
    let cursor = 0;
    cursor < planned.itemCount;
    cursor += PROTECTED_OUTPUT_SIGNER_SCAN_BATCH_V1
  )
    candidates.push(cursor);
  if (planned.itemCount === 0) candidates.push(0);
  const checkpoint = candidates
    .map((nextItemIndex) =>
      missingSignatureFieldWalkCheckpointV1({
        txId: evidence.subject.transaction_id,
        itemCount: planned.itemCount,
        totalLength: planned.preimage.length,
        nextItemIndex,
      }),
    )
    .find(({ checkpointHash }) => checkpointHash === state.checkpoint_hash);
  if (checkpoint === undefined)
    throw new Error(
      "protected-output-signer-missing: scan checkpoint is not on the deterministic frontier",
    );
  const nextCursor = Math.min(
    planned.itemCount,
    checkpoint.nextItemIndex + PROTECTED_OUTPUT_SIGNER_SCAN_BATCH_V1,
  );
  const terminal = nextCursor === planned.itemCount;
  const signerPresent =
    evidence.checkpoints.find(({ cursor }) => cursor === nextCursor)
      ?.signerPresent ?? state.signer_present;
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[3].spendingScriptHash,
    family: "protected-output-signer-missing",
    stepIndex: 3,
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
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: terminal
        ? { subject: evidence.subject, signer_present: signerPresent }
        : {
            protected: state.protected,
            checkpoint_hash: missingSignatureFieldWalkCheckpointV1({
              txId: evidence.subject.transaction_id,
              itemCount: planned.itemCount,
              totalLength: planned.preimage.length,
              nextItemIndex: nextCursor,
            }).checkpointHash,
            signer_present: signerPresent,
          },
    } as never,
    (terminal
      ? ProtectedOutputSignerStep05DatumV1Schema
      : ProtectedOutputSignerStep04DatumV1Schema) as never,
  );
  const result = await submitProtectedOutputSignerOpeningTransitionV1({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    stepIndex: 3,
    nextStepIndex: terminal ? 4 : 3,
    nextDatum,
    opening: actuated.opening,
    checkpointCbor: checkpoint.checkpointCbor,
    referenceScriptUtxo,
    carriageReferenceInputs: actuated.referenceInputs,
    redeemerSchema: ProtectedOutputSignerStep04RedeemerV1Schema as never,
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
