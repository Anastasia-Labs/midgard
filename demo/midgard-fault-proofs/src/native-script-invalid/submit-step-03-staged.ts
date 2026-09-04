import { decodeMidgardVersionedScript } from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  NativeScriptInvalidStep03DatumSchema,
  NativeScriptInvalidStep03SpendRedeemerSchema,
  NativeScriptInvalidStep04DatumSchema,
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
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  linearFaultStepLabel,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  NATIVE_SCRIPT_INVALID_CATEGORY_LABEL as FAMILY,
  type NativeScriptInvalidContracts,
} from "./contracts.js";
import {
  NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH,
  nativeScriptInvalidSignerScanState,
  nativeScriptInvalidUsesDirectRoute,
} from "./evidence-machine.js";

type State = NonNullable<
  Data.Static<typeof NativeScriptInvalidStep03DatumSchema>["data"]
>;
type Step03Datum = Data.Static<typeof NativeScriptInvalidStep03DatumSchema>;
const Step03Datum = asDataType<Step03Datum>(
  NativeScriptInvalidStep03DatumSchema,
);
type Step04Datum = Data.Static<typeof NativeScriptInvalidStep04DatumSchema>;
const Step04Datum = asDataType<Step04Datum>(
  NativeScriptInvalidStep04DatumSchema,
);
type Redeemer = Data.Static<
  typeof NativeScriptInvalidStep03SpendRedeemerSchema
>;
const Redeemer = asDataType<Redeemer>(
  NativeScriptInvalidStep03SpendRedeemerSchema,
);

export const submitNativeScriptInvalidStep03StartSignerScan = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptItemCbor,
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
  readonly contracts: NativeScriptInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly scriptItemCbor: Uint8Array;
  readonly addressWitnessItems: readonly Uint8Array[];
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
  const label = linearFaultStepLabel(FAMILY, stepIndex);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<State>({
    threadUtxo,
    signer,
    schema: Step03Datum,
    family: FAMILY,
    stepIndex,
  });
  const script = decodeMidgardVersionedScript(scriptItemCbor);
  if (script.language !== "NativeCardano") {
    throw new Error(`${label}: selected witness is not a native script`);
  }
  if (
    nativeScriptInvalidUsesDirectRoute({
      signerCount: addressWitnessItems.length,
      scriptBytes: script.scriptBytes.length,
    })
  ) {
    throw new Error(
      `${label}: direct path fits; staged scan is not admissible`,
    );
  }
  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: addressWitnessItems,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet,
    anchorWitnessSetHash: state.bad_tx_witness_set_hash,
    label: `${label} staged field 7`,
  });
  const next = nativeScriptInvalidSignerScanState({
    txId: state.bad_tx_id,
    addressWitnessItems,
    totalLength: planned.preimage.length,
    batchSize: NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${label} staged field 7`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
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
    label: `${label} staged field 7`,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bad_tx_id: state.bad_tx_id,
        bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
        script_item_hash: state.script_item_hash,
        validity_interval_start: state.validity_interval_start,
        validity_interval_end: state.validity_interval_end,
        signer_checkpoint_hash: next.checkpointHash,
        previous_signer_hash: next.previousSignerHash,
        signer_count: next.signerCount,
        signer_peaks: [...next.signerPeaks],
      },
    },
    Step04Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
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
          {
            StartSignerScan: {
              input_index: inputIndex,
              output_index: outputIndex,
              script_item_cbor: Buffer.from(scriptItemCbor).toString("hex"),
              addr_tx_wits_opening: opening,
              item_budget: BigInt(NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH),
            },
          },
        ],
      },
      Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: label,
    nextAddress: contracts.steps[3].spendingScriptAddress,
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
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    checkpointBytes: next.checkpointBytes,
    checkpointHash: next.checkpointHash,
    signerCount: next.signerCount,
    signerPeaks: next.signerPeaks,
    carriageTier: planned.plan.tier,
  } as const;
};
