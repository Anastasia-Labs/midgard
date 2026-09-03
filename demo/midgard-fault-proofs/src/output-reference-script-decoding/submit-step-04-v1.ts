import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
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
import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type OutputReferenceScriptDecodingContracts,
} from "./contracts-v1.js";
import {
  outputReferenceScriptCheckpoint,
  type OutputReferenceScriptDecodingEvidence,
  OutputReferenceScriptResultClasses,
} from "./output-reference-script-decoding-v1.js";
import {
  OutputReferenceStep04DatumSchema,
  OutputReferenceStep04RedeemerSchema,
  OutputReferenceStep05DatumSchema,
} from "./schemas-v1.js";

export const submitOutputReferenceScriptDecodingStep04 = async ({
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
  readonly contracts: OutputReferenceScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: OutputReferenceScriptDecodingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor?: string;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly certificatePreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
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
    schema: OutputReferenceStep04DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    state.outcome !== 1n ||
    state.item_hash !== evidence.outputHashHex ||
    state.bound.output_index !== BigInt(evidence.outputIndex)
  )
    throw new Error(`${FAMILY}: canonical output binding changed`);
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.outputFieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpening({
    fieldIndex: 2,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: publishCarriage || publishedCarriageUtxos.length > 0,
    label: `${FAMILY} reference bind`,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[3].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos.length > 0
      ? publishedCarriageUtxos
      : await publishFaultProofFieldCarriage({
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
  const opening: FieldOpening = faultProofFieldOpening({
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
      ? OutputReferenceScriptResultClasses.Pending
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
    checkpoint_hash: outputReferenceScriptCheckpoint({
      evidence,
      controlCbor: evidence.initialControlCbor,
      nextExpectedScriptHash: nextHash,
    }),
    result_class: BigInt(initialResult),
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    OutputReferenceStep05DatumSchema as never,
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
      OutputReferenceStep04RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
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
