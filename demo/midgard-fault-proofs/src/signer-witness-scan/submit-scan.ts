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
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
  reuseOrCertifyFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  type LinearFaultContracts,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { SignerWitnessScanEvidence } from "./evidence.js";
import { planSignerWitnessOpening } from "./field-plans.js";
import {
  SignerWitnessScanDatumSchema,
  SignerWitnessScanRedeemerSchema,
  SignerWitnessTerminalDatumSchema,
} from "./schemas.js";
import { planSignerWitnessScanTurn } from "./signer-witness-scan.js";

/** The scan sits at step index 2 and the terminal at 3 in both families. */
export const SIGNER_WITNESS_SCAN_STEP_INDEX = 2;
export const SIGNER_WITNESS_TERMINAL_STEP_INDEX = 3;

export type SignerWitnessScanContracts = LinearFaultContracts & {
  readonly fieldPreimageCertificatePolicyId: string;
  readonly fieldPreimageCertificateMintingScript: Script;
};

type ScanState = {
  subject: { transaction_id: string };
  witness_set_hash: string;
  payment_credential: string;
  checkpoint_hash: string;
};

/**
 * One turn of the shared signer-witness scan: the opening turn when the
 * thread's checkpoint is empty, a resume otherwise. The turn continues at the
 * scan, or writes the verdict to the terminal once the signer is found or the
 * field is exhausted.
 */
export const submitSignerWitnessScan = async ({
  lucid,
  family,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  referenceScriptUtxo,
  certificateReferenceScriptUtxo,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo: suppliedCertificateUtxo,
  publicationBoundary,
  certificateBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly lucid: LucidEvolution;
  /** The family name used in labels and errors. */
  readonly family: string;
  readonly contracts: SignerWitnessScanContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: SignerWitnessScanEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly publishCarriage?: boolean;
  /** Carriage an earlier turn already published, reused instead. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** A certificate an earlier turn already minted, reused instead. */
  readonly certificateUtxo?: UTxO;
  readonly publicationBoundary?: FraudProofPreSubmitBoundary;
  readonly certificateBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}) => {
  const stepIndex = SIGNER_WITNESS_SCAN_STEP_INDEX;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<ScanState>({
    threadUtxo,
    signer,
    schema: SignerWitnessScanDatumSchema as never,
    family,
    stepIndex,
  });
  if (
    state.subject.transaction_id !== evidence.subject.transaction_id ||
    state.witness_set_hash !== evidence.witnessSetHashHex ||
    state.payment_credential !== evidence.paymentCredentialHex
  )
    throw new Error(`${family}: scan state changed`);
  const label = `${family} address witnesses`;
  const planned = planSignerWitnessOpening({
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label,
  });
  const turn = planSignerWitnessScanTurn({
    checkpoints: evidence.checkpoints,
    txId: evidence.subject.transaction_id,
    itemCount: planned.itemCount,
    totalLength: planned.preimage.length,
    checkpointHash: state.checkpoint_hash,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      ownOutputs,
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label,
      preSubmitBoundary: publicationBoundary,
    }));
  const certificateUtxo =
    suppliedCertificateUtxo ??
    (planned.plan.tier === "Certified"
      ? await reuseOrCertifyFaultProofFieldCarriage({
          ownOutputs,
          lucid,
          network: lucid.config().network!,
          signer,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          certificateMintingScript:
            contracts.fieldPreimageCertificateMintingScript,
          certificateReferenceScriptUtxo,
          missingReferenceScript: `${family}: certified scan requires certificate reference script`,
          chunkUtxos: carriageUtxos,
          compactCbor: nativeTxCompactCbor,
          witnessSetCompactCbor,
          preSubmitBoundary: certificateBoundary,
        })
      : undefined);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex]!.spendingScriptHash,
    family,
    stepIndex,
  });
  const nextStep =
    contracts.steps[
      turn.terminal ? SIGNER_WITNESS_TERMINAL_STEP_INDEX : stepIndex
    ]!;
  const nextDatum = turn.terminal
    ? Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            subject: evidence.subject,
            signer_required: true,
            signer_present: turn.signerPresent,
          },
        } as never,
        SignerWitnessTerminalDatumSchema as never,
      )
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: { ...state, checkpoint_hash: turn.nextCheckpointHash! },
        } as never,
        SignerWitnessScanDatumSchema as never,
      );
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const role = `${family} scan`;
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    const opening: FieldOpening = faultProofFieldOpening({
      planned,
      referenceInputs: ctx.referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label,
    });
    requireOwnSpendPurpose(ctx, threadUtxo, role);
    const inputIndex = requireInputIndex(ctx, threadUtxo, role);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${role} output`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            witnesses_opening: opening,
            checkpoint_cbor: turn.checkpointCbor,
          },
        ],
      } as never,
      SignerWitnessScanRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex]!.spendingScript,
    stepRole: role,
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
    throw new Error(`${family}: scan layout unresolved`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    stage: turn.terminal ? ("terminal" as const) : ("scanning" as const),
    terminal: turn.terminal,
    nextCursor: turn.nextCursor,
    carriageTier: planned.plan.tier,
    carriageUtxos,
    certificateUtxo,
  };
};
