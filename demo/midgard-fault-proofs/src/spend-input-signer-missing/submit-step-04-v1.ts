import { decodeMidgardAddressWitnessFieldPreimageV1 } from "@al-ft/midgard-core";
import {
  type FieldOpeningV1,
  missingSignatureFieldWalkCheckpointV1,
  missingSignatureVkeyHashV1,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  verifyAddressWitness,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  certifyFaultProofFieldCarriageV1,
  faultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../field-opening-v1.js";
import {
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { SpendInputSignerMissingContractsV1 } from "./contracts-v1.js";
import { planSpendInputSignerWitnessOpeningV1 } from "./field-plans-v1.js";
import {
  SpendInputSignerStep04DatumV1Schema,
  SpendInputSignerStep04RedeemerV1Schema,
  SpendInputSignerStep05DatumV1Schema,
} from "./schemas-v1.js";
import {
  SPEND_INPUT_SIGNER_SCAN_BATCH_V1,
  type SpendInputSignerMissingEvidenceV1,
} from "./spend-input-signer-missing-v1.js";

const resolveCursor = ({
  txId,
  itemCount,
  totalLength,
  checkpointHash,
}: {
  readonly txId: string;
  readonly itemCount: number;
  readonly totalLength: number;
  readonly checkpointHash: string;
}) => {
  for (
    let cursor = 0;
    cursor < itemCount || cursor === 0;
    cursor += SPEND_INPUT_SIGNER_SCAN_BATCH_V1
  ) {
    const candidate = missingSignatureFieldWalkCheckpointV1({
      txId,
      itemCount,
      totalLength,
      nextItemIndex: cursor,
    });
    if (candidate.checkpointHash === checkpointHash) return candidate;
    if (itemCount === 0) break;
  }
  throw new Error(
    "spend-input-signer-missing: checkpoint is not on the deterministic 16-item schedule",
  );
};

export const submitSpendInputSignerMissingStep04V1 = async ({
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
  onCarriageReady,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly contracts: SpendInputSignerMissingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: SpendInputSignerMissingEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly publishCarriage?: boolean;
  readonly publicationBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly certificateBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly onCarriageReady?: () => Promise<void>;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "spend-input-signer-missing",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
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
    schema: SpendInputSignerStep04DatumV1Schema as never,
    family: "spend-input-signer-missing",
    stepIndex,
  });
  if (
    state.authenticated.transaction_id !== evidence.subject.transaction_id ||
    state.authenticated.witness_set_hash !== evidence.witnessSetHashHex ||
    state.authenticated.payment_credential !== evidence.paymentCredentialHex
  )
    throw new Error("spend-input-signer-missing: scan authority changed");
  const planned = planSpendInputSignerWitnessOpeningV1({
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
  });
  const checkpoint = resolveCursor({
    txId: evidence.subject.transaction_id,
    itemCount: planned.itemCount,
    totalLength: planned.preimage.length,
    checkpointHash: state.checkpoint_hash,
  });
  const nextCursor = Math.min(
    checkpoint.nextItemIndex + SPEND_INPUT_SIGNER_SCAN_BATCH_V1,
    planned.itemCount,
  );
  const witnesses = decodeMidgardAddressWitnessFieldPreimageV1(
    Buffer.from(evidence.addressWitnessFieldPreimageHex, "hex"),
  );
  const signerPresent = witnesses
    .slice(checkpoint.nextItemIndex, nextCursor)
    .some((witness) => {
      const verificationKey = Buffer.from(witness.verificationKey).toString(
        "hex",
      );
      return (
        verifyAddressWitness({
          txId: evidence.subject.transaction_id,
          witness: {
            verification_key: verificationKey,
            signature: Buffer.from(witness.signature).toString("hex"),
          },
        }) &&
        missingSignatureVkeyHashV1(verificationKey) ===
          evidence.paymentCredentialHex
      );
    });
  const terminal = signerPresent || nextCursor === planned.itemCount;
  const nextCheckpoint = terminal
    ? undefined
    : missingSignatureFieldWalkCheckpointV1({
        txId: evidence.subject.transaction_id,
        itemCount: planned.itemCount,
        totalLength: planned.preimage.length,
        nextItemIndex: nextCursor,
      });
  signer.selectWallet(lucid);
  const carriageUtxos = await publishFaultProofFieldCarriageV1({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "spend-input-signer-missing scan witnesses",
    preSubmitBoundary: publicationBoundary,
  });
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? (
          await certifyFaultProofFieldCarriageV1({
            lucid,
            network,
            signer,
            planned,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
            certificateMintingScript:
              contracts.fieldPreimageCertificateMintingScript,
            certificateReferenceScriptUtxo:
              certificateReferenceScriptUtxo ??
              (() => {
                throw new Error(
                  "spend-input-signer-missing: certified scan requires certificate reference script",
                );
              })(),
            chunkUtxos: carriageUtxos,
            compactCbor: nativeTxCompactCbor,
            witnessSetCompactCbor,
            preSubmitBoundary: certificateBoundary,
          })
        ).certificateUtxo
      : undefined;
  await onCarriageReady?.();
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[3].spendingScriptHash,
    family: "spend-input-signer-missing",
    stepIndex,
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    stepReference,
  ];
  const opening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "spend-input-signer-missing scan witnesses",
  });
  const nextDatum = terminal
    ? Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: { subject: evidence.subject, signer_missing: !signerPresent },
        } as never,
        SpendInputSignerStep05DatumV1Schema as never,
      )
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            authenticated: state.authenticated,
            checkpoint_hash: nextCheckpoint!.checkpointHash,
          },
        } as never,
        SpendInputSignerStep04DatumV1Schema as never,
      );
  const nextStep = terminal ? contracts.steps[4] : contracts.steps[3];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
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
      SpendInputSignerStep04RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
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
