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
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
  reuseOrCertifyFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { planSignerWitnessScanTurn } from "../signer-witness-scan/signer-witness-scan.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { SpendInputSignerMissingContracts } from "./contracts.js";
import { planSpendInputSignerWitnessOpening } from "./field-plans.js";
import {
  SpendInputSignerStep04DatumSchema,
  SpendInputSignerStep04RedeemerSchema,
  SpendInputSignerStep05DatumSchema,
} from "./schemas.js";
import {
  SPEND_INPUT_SIGNER_SCAN_BATCH,
  type SpendInputSignerMissingEvidence,
} from "./spend-input-signer-missing.js";

export const submitSpendInputSignerMissingStep04 = async ({
  lucid,
  network,
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
  publicationBoundary,
  certificateBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly contracts: SpendInputSignerMissingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: SpendInputSignerMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly publishCarriage?: boolean;
  readonly publicationBoundary?: FraudProofPreSubmitBoundary;
  readonly certificateBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "spend-input-signer-missing",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    authenticated: {
      subject: typeof evidence.subject;
      transaction_id: string;
      witness_set_hash: string;
      payment_credential: string;
    };
    checkpoint_hash: string;
  }>({
    threadUtxo,
    signer,
    schema: SpendInputSignerStep04DatumSchema as never,
    family: "spend-input-signer-missing",
    stepIndex,
  });
  if (
    state.authenticated.transaction_id !== evidence.subject.transaction_id ||
    state.authenticated.witness_set_hash !== evidence.witnessSetHashHex ||
    state.authenticated.payment_credential !== evidence.paymentCredentialHex
  )
    throw new Error("spend-input-signer-missing: scan authority changed");
  const planned = planSpendInputSignerWitnessOpening({
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
  });
  const turn = planSignerWitnessScanTurn({
    checkpoints: evidence.checkpoints,
    txId: evidence.subject.transaction_id,
    itemCount: planned.itemCount,
    totalLength: planned.preimage.length,
    checkpointHash: state.checkpoint_hash,
    batch: SPEND_INPUT_SIGNER_SCAN_BATCH,
  });
  const { checkpoint, nextCursor, signerPresent } = turn;
  // A present signer ends this family's scan early, as step 04 does on chain.
  const terminal = signerPresent || turn.exhausted;
  const nextCheckpoint = terminal ? undefined : turn.nextCheckpoint;
  signer.selectWallet(lucid);
  const carriageUtxos = await publishFaultProofFieldCarriage({
    ownOutputs,
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "spend-input-signer-missing scan witnesses",
    preSubmitBoundary: publicationBoundary,
  });
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? await reuseOrCertifyFaultProofFieldCarriage({
          ownOutputs,
          lucid,
          network,
          signer,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          certificateMintingScript:
            contracts.fieldPreimageCertificateMintingScript,
          certificateReferenceScriptUtxo,
          missingReferenceScript:
            "spend-input-signer-missing: certified scan requires certificate reference script",
          chunkUtxos: carriageUtxos,
          compactCbor: nativeTxCompactCbor,
          witnessSetCompactCbor,
          preSubmitBoundary: certificateBoundary,
        })
      : undefined;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[3].spendingScriptHash,
    family: "spend-input-signer-missing",
    stepIndex,
  });
  const nextDatum = terminal
    ? Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            subject: evidence.subject,
            signer_required: true,
            signer_missing: !signerPresent,
          },
        } as never,
        SpendInputSignerStep05DatumSchema as never,
      )
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            authenticated: state.authenticated,
            checkpoint_hash: nextCheckpoint!.checkpointHash,
          },
        } as never,
        SpendInputSignerStep04DatumSchema as never,
      );
  const nextStep = terminal ? contracts.steps[4] : contracts.steps[3];
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
      label: "spend-input-signer-missing scan witnesses",
    });
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "spend-input-signer-missing step-04",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "spend-input-signer-missing step-04",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "spend-input-signer-missing step-04 output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            witnesses_opening: opening,
            checkpoint_cbor: checkpoint.checkpointCbor,
          },
        ],
      } as never,
      SpendInputSignerStep04RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[3].spendingScript,
    stepRole: "spend-input-signer-missing step-04",
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
    throw new Error("spend-input-signer-missing: step-04 layout unresolved");
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    stage: terminal ? ("step05" as const) : ("scanning" as const),
    nextCursor,
    carriageTier: planned.plan.tier,
  };
};
