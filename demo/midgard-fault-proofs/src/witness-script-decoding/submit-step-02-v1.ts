import {
  type FieldOpeningV1,
  MIDGARD_FIELD_INDEX_V1,
  type NativeTxWitnessSetCompact,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type WitnessScriptDecodingBoundV1,
  type WitnessScriptDecodingScanStateV1,
  WitnessScriptDecodingStep02DatumV1Schema,
  WitnessScriptDecodingStep02RedeemerV1Schema,
  WitnessScriptDecodingStep03DatumV1Schema,
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
  planFaultProofFieldOpeningV1,
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
import {
  WITNESS_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type WitnessScriptDecodingContractsV1,
} from "./contracts-v1.js";
import {
  witnessScriptDecodingCheckpointV1,
  type WitnessScriptDecodingEvidenceV1,
} from "./witness-script-decoding-v1.js";

export const submitWitnessScriptDecodingStep02V1 = async ({
  lucid,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSet,
  witnessSetCompactCbor,
  scriptWitnessItems,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  certificateReferenceScriptUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  certificatePreSubmitBoundary,
  onCarriageReady,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly contracts: WitnessScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: WitnessScriptDecodingEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly witnessSetCompactCbor: string;
  readonly scriptWitnessItems: readonly Uint8Array[];
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly certificatePreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly onCarriageReady?: () => Promise<void> | void;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const bound = requireLinearFaultStepStateV1<WitnessScriptDecodingBoundV1>({
    threadUtxo,
    signer,
    schema: WitnessScriptDecodingStep02DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  const item = scriptWitnessItems[Number(bound.script_index)];
  if (
    item === undefined ||
    Buffer.from(item).toString("hex") !== evidence.itemHex ||
    bound.subject.transaction_id !== evidence.finding.subject.transaction_id ||
    bound.witness_set_hash !== evidence.finding.witnessSetHash
  ) {
    throw new Error(`${FAMILY}: retained evidence differs from bound subject`);
  }
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.scriptWitnesses,
    anchorTxId: bound.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: scriptWitnessItems,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet,
    anchorWitnessSetHash: bound.witness_set_hash,
    label: `${FAMILY} step 02 field 6`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriageV1({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${FAMILY} step 02 field 6`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const resolvedCertificateUtxo =
    certificateUtxo ??
    (planned.plan.tier === "Certified"
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
                  `${FAMILY}: certified field opening requires the package-bound certificate reference script`,
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
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const opening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs: [
      ...carriageUtxos,
      stepReference,
      ...(resolvedCertificateUtxo === undefined
        ? []
        : [resolvedCertificateUtxo]),
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${FAMILY} step 02 field 6`,
  });
  const nextState: WitnessScriptDecodingScanStateV1 = {
    bound,
    total_length: BigInt(evidence.itemLength),
    item_commitment: evidence.itemCommitmentHex,
    control_cbor: evidence.initialControlCbor,
    next_expected_script_hash: contracts.steps[2].spendingScriptHash,
    checkpoint_hash: witnessScriptDecodingCheckpointV1({
      evidence,
      controlCbor: evidence.initialControlCbor,
      nextExpectedScriptHash: contracts.steps[2].spendingScriptHash,
    }),
    result_class: BigInt(evidence.initialResultClass),
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    WitnessScriptDecodingStep03DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 02`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 02`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step 02`,
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, opening },
        ],
      } as never,
      WitnessScriptDecodingStep02RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} step 02`,
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      resolvedCertificateUtxo === undefined ? [] : [resolvedCertificateUtxo],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    scanState: nextState,
    carriageTier: planned.plan.tier,
  };
};
