import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
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
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening-v1.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { ResolvedOutputNonCanonicalContracts } from "./contracts-v1.js";
import { type ResolvedOutputEvidence } from "./resolved-output-non-canonical-v1.js";
import {
  ResolvedOutputStep02DatumSchema,
  ResolvedOutputStep02RedeemerSchema,
  ResolvedOutputStep03DatumSchema,
} from "./schemas-v1.js";

export const submitResolvedOutputNonCanonicalStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo: suppliedCertificateUtxo,
  certificateReferenceScriptUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  certificatePreSubmitBoundary,
  onCarriageReady,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ResolvedOutputNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ResolvedOutputEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly certificatePreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly onCarriageReady?: () => Promise<void>;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "resolved-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    source_kind: bigint;
    input_index: bigint;
    prior_root: string;
  }>({
    threadUtxo,
    signer,
    schema: ResolvedOutputStep02DatumSchema as never,
    family: "resolved-output-non-canonical",
    stepIndex,
  });
  if (
    state.source_kind !== BigInt(evidence.coordinate.sourceKind) ||
    state.input_index !== BigInt(evidence.coordinate.inputIndex) ||
    state.prior_root !== evidence.resolved.priorRoot
  )
    throw new Error(
      "resolved-output-non-canonical: opening coordinate differs from thread",
    );
  const txMaterial = deriveMidgardNativeTxFaultEvidenceMaterial(
    Buffer.from(evidence.canonicalTransactionCborHex, "hex"),
  );
  const fieldIndex = evidence.coordinate.sourceKind;
  const items = decodeMidgardFieldPreimage(
    txMaterial.fieldPreimages[fieldIndex]!,
  );
  const planned = planFaultProofFieldOpening({
    fieldIndex,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: "resolved-output-non-canonical field opening",
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "resolved-output-non-canonical field opening",
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const certificateUtxo =
    suppliedCertificateUtxo ??
    (planned.plan.tier === "Certified"
      ? (
          await certifyFaultProofFieldCarriage({
            lucid,
            network: lucid.config().network!,
            signer,
            planned,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
            certificateMintingScript:
              contracts.fieldPreimageCertificateMintingScript,
            certificateReferenceScriptUtxo:
              certificateReferenceScriptUtxo ??
              (() => {
                throw new Error(
                  "resolved-output-non-canonical: certified opening requires certificate reference script",
                );
              })(),
            chunkUtxos: carriageUtxos,
            compactCbor: nativeTxCompactCbor,
            witnessSetCompactCbor,
            preSubmitBoundary: certificatePreSubmitBoundary,
          })
        ).certificateUtxo
      : undefined);
  await onCarriageReady?.();
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "resolved-output-non-canonical",
    stepIndex,
  });
  const opening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs: [
      ...carriageUtxos,
      stepReference,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "resolved-output-non-canonical field opening",
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        prior_root: evidence.resolved.priorRoot,
        out_ref: {
          transactionId: evidence.resolved.transactionId,
          outputIndex: BigInt(evidence.resolved.outputIndex),
        },
      },
    } as never,
    ResolvedOutputStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "resolved-output-non-canonical step-02",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "resolved-output-non-canonical",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "resolved-output-non-canonical step-02 output",
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, opening },
        ],
      } as never,
      ResolvedOutputStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[1].spendingScript,
    stepRole: "resolved-output-non-canonical step-02",
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("resolved-output-non-canonical: step-02 layout unresolved");
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    carriageTier: planned.plan.tier,
  };
};
