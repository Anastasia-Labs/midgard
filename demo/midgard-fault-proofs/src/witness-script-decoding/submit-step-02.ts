import {
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  type NativeTxWitnessSetCompact,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  type WitnessScriptDecodingBound,
  type WitnessScriptDecodingScanState,
  WitnessScriptDecodingStep02DatumSchema,
  WitnessScriptDecodingStep02RedeemerSchema,
  WitnessScriptDecodingStep03DatumSchema,
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
  planFaultProofFieldOpening,
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
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  WITNESS_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type WitnessScriptDecodingContracts,
} from "./contracts.js";
import {
  witnessScriptDecodingCheckpoint,
  type WitnessScriptDecodingEvidence,
} from "./witness-script-decoding.js";

export const submitWitnessScriptDecodingStep02 = async ({
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
  scriptDecodeRecordUtxo,
  publicationPreSubmitBoundary,
  certificatePreSubmitBoundary,
  onCarriageReady,
  preSubmitBoundary,
  awaitConfirmation = true,
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: WitnessScriptDecodingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly witnessSetCompactCbor: string;
  readonly scriptWitnessItems: readonly Uint8Array[];
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  /**
   * The decode record of the item's script when it is a Plutus script
   * (`recordScriptDecode`); a native or malformed item takes none.
   */
  readonly scriptDecodeRecordUtxo?: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly certificatePreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly onCarriageReady?: () => Promise<void> | void;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const bound = requireLinearFaultStepState<WitnessScriptDecodingBound>({
    threadUtxo,
    signer,
    schema: WitnessScriptDecodingStep02DatumSchema as never,
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
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: bound.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
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
    (await publishFaultProofFieldCarriage({
      ownOutputs,
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${FAMILY} step 02 field 6`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  // A certificate already on chain (the workflow's certify action ran) is
  // reused; only an absent one is minted here.
  const resolvedCertificateUtxo =
    certificateUtxo ??
    (planned.plan.tier === "Certified"
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
          missingReferenceScript: `${FAMILY}: certified field opening requires the package-bound certificate reference script`,
          chunkUtxos: carriageUtxos,
          compactCbor: nativeTxCompactCbor,
          witnessSetCompactCbor,
          preSubmitBoundary: certificatePreSubmitBoundary,
        })
      : undefined);
  await onCarriageReady?.();
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const extraReferenceInputs = [
    ...(resolvedCertificateUtxo === undefined ? [] : [resolvedCertificateUtxo]),
    ...(scriptDecodeRecordUtxo === undefined ? [] : [scriptDecodeRecordUtxo]),
  ];
  const nextState: WitnessScriptDecodingScanState = {
    bound,
    total_length: BigInt(evidence.itemLength),
    item_commitment: evidence.itemCommitmentHex,
    control_cbor: evidence.initialControlCbor,
    next_expected_script_hash: contracts.steps[2].spendingScriptHash,
    checkpoint_hash: witnessScriptDecodingCheckpoint({
      evidence,
      controlCbor: evidence.initialControlCbor,
      nextExpectedScriptHash: contracts.steps[2].spendingScriptHash,
    }),
    result_class: BigInt(evidence.initialResultClass),
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    WitnessScriptDecodingStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    const opening: FieldOpening = faultProofFieldOpening({
      planned,
      referenceInputs: ctx.referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${FAMILY} step 02 field 6`,
    });
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
          {
            input_index: inputIndex,
            output_index: outputIndex,
            opening,
            record_reference_input_index:
              scriptDecodeRecordUtxo === undefined
                ? -1n
                : requireReferenceInputIndex(
                    ctx,
                    scriptDecodeRecordUtxo,
                    `${FAMILY} step 02 decode record`,
                  ),
          },
        ],
      } as never,
      WitnessScriptDecodingStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
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
    extraReferenceInputs,
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
