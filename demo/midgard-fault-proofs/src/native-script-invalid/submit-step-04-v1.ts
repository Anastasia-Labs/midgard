import {
  type FieldOpeningV1,
  MIDGARD_FIELD_INDEX_V1,
  NativeScriptInvalidStep04DatumSchema,
  NativeScriptInvalidStep04SpendRedeemerSchema,
  NativeScriptInvalidStep05DatumSchema,
  type NativeTxWitnessSetCompact,
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
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../field-opening-v1.js";
import {
  linearFaultStepLabelV1,
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import {
  NATIVE_SCRIPT_INVALID_CATEGORY_LABEL as FAMILY,
  type NativeScriptInvalidContractsV1,
} from "./contracts-v1.js";
import {
  NATIVE_SCRIPT_INVALID_SIGNER_FINALIZE_BATCH_V1,
  NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH_V1,
  nativeScriptInvalidSignerScanStateV1,
  resolveNativeScriptInvalidSignerCheckpointV1,
} from "./evidence-machine-v1.js";

type State = NonNullable<
  Data.Static<typeof NativeScriptInvalidStep04DatumSchema>["data"]
>;
type Step04Datum = Data.Static<typeof NativeScriptInvalidStep04DatumSchema>;
const Step04Datum =
  NativeScriptInvalidStep04DatumSchema as unknown as Step04Datum;
type Step05Datum = Data.Static<typeof NativeScriptInvalidStep05DatumSchema>;
const Step05Datum =
  NativeScriptInvalidStep05DatumSchema as unknown as Step05Datum;
type Redeemer = Data.Static<
  typeof NativeScriptInvalidStep04SpendRedeemerSchema
>;
const Redeemer =
  NativeScriptInvalidStep04SpendRedeemerSchema as unknown as Redeemer;

export const submitNativeScriptInvalidStep04 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  addressWitnessItems,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptInvalidContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly addressWitnessItems: readonly Uint8Array[];
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3;
  const label = linearFaultStepLabelV1(FAMILY, stepIndex);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<State>({
    threadUtxo,
    signer,
    schema: Step04Datum,
    family: FAMILY,
    stepIndex,
  });
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.addressWitnesses,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: addressWitnessItems,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet,
    anchorWitnessSetHash: state.bad_tx_witness_set_hash,
    label: `${label} field 7`,
  });
  const current = resolveNativeScriptInvalidSignerCheckpointV1({
    txId: state.bad_tx_id,
    itemCount: addressWitnessItems.length,
    totalLength: planned.preimage.length,
    committedHash: state.signer_checkpoint_hash,
  });
  if (current === null) {
    throw new Error(`${label}: signer checkpoint is missing`);
  }
  const remainingItems = addressWitnessItems.length - current.nextItemIndex;
  const batchSize =
    remainingItems <= NATIVE_SCRIPT_INVALID_SIGNER_FINALIZE_BATCH_V1
      ? remainingItems
      : NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH_V1;
  const next = nativeScriptInvalidSignerScanStateV1({
    txId: state.bad_tx_id,
    addressWitnessItems,
    totalLength: planned.preimage.length,
    committedCheckpointHash: state.signer_checkpoint_hash,
    batchSize,
  });
  const itemBudget = batchSize;
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriageV1({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${label} field 7`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
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
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${label} field 7`,
  });
  const nextDatum = next.complete
    ? Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            bad_tx_id: state.bad_tx_id,
            script_item_hash: state.script_item_hash,
            validity_interval_start: state.validity_interval_start,
            validity_interval_end: state.validity_interval_end,
            signer_count: next.signerCount,
            signer_peaks: [...next.signerPeaks],
            phase: "ScriptReady",
          },
        },
        Step05Datum,
      )
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            ...state,
            signer_checkpoint_hash: next.checkpointHash,
            previous_signer_hash: next.previousSignerHash,
            signer_count: next.signerCount,
            signer_peaks: [...next.signerPeaks],
          },
        },
        Step04Datum,
      );
  const nextStepIndex = next.complete ? 4 : 3;
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const inputIndex = requireInputIndex(ctx, threadUtxo, label);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, label);
    return Data.to(
      {
        Continue: [
          next.complete
            ? {
                FinalizeSignerScan: {
                  input_index: inputIndex,
                  output_index: outputIndex,
                  addr_tx_wits_opening: opening,
                  checkpoint_bytes: current.checkpointCbor,
                  item_budget: BigInt(itemBudget),
                },
              }
            : {
                ResumeSignerScan: {
                  input_index: inputIndex,
                  output_index: outputIndex,
                  addr_tx_wits_opening: opening,
                  checkpoint_bytes: current.checkpointCbor,
                  item_budget: BigInt(itemBudget),
                },
              },
        ],
      },
      Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: label,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  return {
    txHash,
    action: next.complete ? "FinalizeSignerScan" : "ResumeSignerScan",
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    checkpointBytes: next.checkpointBytes,
    checkpointHash: next.checkpointHash,
    signerCount: next.signerCount,
    signerPeaks: next.signerPeaks,
    carriageTier: planned.plan.tier,
  } as const;
};
