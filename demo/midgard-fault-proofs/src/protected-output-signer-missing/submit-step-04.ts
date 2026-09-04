import { missingSignatureFieldWalkCheckpoint } from "@al-ft/midgard-sdk";
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
  const candidates: number[] = [];
  for (
    let cursor = 0;
    cursor < planned.itemCount;
    cursor += PROTECTED_OUTPUT_SIGNER_SCAN_BATCH
  )
    candidates.push(cursor);
  if (planned.itemCount === 0) candidates.push(0);
  const checkpoint = candidates
    .map((nextItemIndex) =>
      missingSignatureFieldWalkCheckpoint({
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
    checkpoint.nextItemIndex + PROTECTED_OUTPUT_SIGNER_SCAN_BATCH,
  );
  const terminal = nextCursor === planned.itemCount;
  const signerPresent =
    evidence.checkpoints.find(({ cursor }) => cursor === nextCursor)
      ?.signerPresent ?? state.signer_present;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[3].spendingScriptHash,
    family: "protected-output-signer-missing",
    stepIndex: 3,
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
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: terminal
        ? { subject: evidence.subject, signer_present: signerPresent }
        : {
            protected: state.protected,
            checkpoint_hash: missingSignatureFieldWalkCheckpoint({
              txId: evidence.subject.transaction_id,
              itemCount: planned.itemCount,
              totalLength: planned.preimage.length,
              nextItemIndex: nextCursor,
            }).checkpointHash,
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
    opening: actuated.opening,
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
