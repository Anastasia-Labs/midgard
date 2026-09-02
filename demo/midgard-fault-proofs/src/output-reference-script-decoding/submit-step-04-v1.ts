import { decodeMidgardFieldPreimageV1 } from "@al-ft/midgard-core";
import {
  type FieldOpeningV1,
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
  OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type OutputReferenceScriptDecodingContractsV1,
} from "./contracts-v1.js";
import {
  outputReferenceScriptCheckpointV1,
  type OutputReferenceScriptDecodingEvidenceV1,
  OutputReferenceScriptResultClassesV1,
} from "./output-reference-script-decoding-v1.js";
import {
  OutputReferenceStep04DatumV1Schema,
  OutputReferenceStep04RedeemerV1Schema,
  OutputReferenceStep05DatumV1Schema,
} from "./schemas-v1.js";

export const submitOutputReferenceScriptDecodingStep04V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor = "",
  publishCarriage = false,
  publishedCarriageUtxos = [],
  certificateUtxo,
  certificateReferenceScriptUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  certificatePreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: OutputReferenceScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: OutputReferenceScriptDecodingEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor?: string;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly certificatePreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    bound: {
      subject: typeof evidence.subject;
      output_index: bigint;
      accused_class: bigint;
    };
    item_length: bigint;
    item_hash: string;
    outcome: bigint;
  }>({
    threadUtxo,
    signer,
    schema: OutputReferenceStep04DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    state.outcome !== 1n ||
    state.item_hash !== evidence.outputHashHex ||
    state.bound.output_index !== BigInt(evidence.outputIndex)
  )
    throw new Error(`${FAMILY}: canonical output binding changed`);
  const items = decodeMidgardFieldPreimageV1(
    Buffer.from(evidence.outputFieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: 2,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: publishCarriage || publishedCarriageUtxos.length > 0,
    label: `${FAMILY} reference bind`,
  });
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[3].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos.length > 0
      ? publishedCarriageUtxos
      : await publishFaultProofFieldCarriageV1({
          lucid,
          signer,
          planned,
          publisherAddress: signer.address,
          label: `${FAMILY} reference bind`,
          preSubmitBoundary: publicationPreSubmitBoundary,
        });
  const resolvedCertificate =
    certificateUtxo ??
    (planned.plan.tier === "Certified"
      ? (
          await certifyFaultProofFieldCarriageV1({
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
                  `${FAMILY}: certified opening requires certificate reference`,
                );
              })(),
            chunkUtxos: carriageUtxos,
            compactCbor: nativeTxCompactCbor,
            witnessSetCompactCbor,
            preSubmitBoundary: certificatePreSubmitBoundary,
          })
        ).certificateUtxo
      : undefined);
  const opening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs: [
      ...carriageUtxos,
      stepReference,
      ...(resolvedCertificate === undefined ? [] : [resolvedCertificate]),
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${FAMILY} reference bind`,
  });
  const initialResult =
    evidence.initialControlCbor.length > 0
      ? OutputReferenceScriptResultClassesV1.Pending
      : evidence.resultClass;
  const nextHash = contracts.steps[4].spendingScriptHash;
  const nextState = {
    bound: state.bound,
    total_length: BigInt(
      Buffer.from(evidence.referenceScriptItemHex, "hex").length,
    ),
    item_commitment: evidence.referenceScriptItemCommitmentHex,
    control_cbor: evidence.initialControlCbor,
    next_expected_script_hash: nextHash,
    checkpoint_hash: outputReferenceScriptCheckpointV1({
      evidence,
      controlCbor: evidence.initialControlCbor,
      nextExpectedScriptHash: nextHash,
    }),
    result_class: BigInt(initialResult),
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    OutputReferenceStep05DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[4].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step04`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step04`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step04 output`,
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, opening },
        ],
      } as never,
      OutputReferenceStep04RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[3].spendingScript,
    stepRole: `${FAMILY} step04`,
    nextAddress: contracts.steps[4].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      resolvedCertificate === undefined ? [] : [resolvedCertificate],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: step04 layout unresolved`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
