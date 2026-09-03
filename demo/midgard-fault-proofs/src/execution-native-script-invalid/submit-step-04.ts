import { decodeMidgardVersionedScript } from "@al-ft/midgard-core";
import {
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
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
  EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL as FAMILY,
  type ExecutionNativeScriptInvalidContracts,
} from "./contracts.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH,
  executionNativeScriptInvalidSignerScanState,
  executionNativeScriptInvalidUsesDirectRoute,
} from "./evidence-machine.js";
import {
  ExecutionNativeScriptInvalidStep04DatumSchema,
  ExecutionNativeScriptInvalidStep04RedeemerSchema,
  ExecutionNativeScriptInvalidStep05DatumSchema,
} from "./schemas.js";

type State = NonNullable<
  Data.Static<typeof ExecutionNativeScriptInvalidStep04DatumSchema>["data"]
>;
type Step03Datum = Data.Static<
  typeof ExecutionNativeScriptInvalidStep04DatumSchema
>;
const Step03Datum =
  ExecutionNativeScriptInvalidStep04DatumSchema as unknown as Step03Datum;
type Step04Datum = Data.Static<
  typeof ExecutionNativeScriptInvalidStep05DatumSchema
>;
const Step04Datum =
  ExecutionNativeScriptInvalidStep05DatumSchema as unknown as Step04Datum;
type Redeemer = Data.Static<
  typeof ExecutionNativeScriptInvalidStep04RedeemerSchema
>;
const Redeemer =
  ExecutionNativeScriptInvalidStep04RedeemerSchema as unknown as Redeemer;

export const submitExecutionNativeScriptInvalidStep04StartSignerScan = async ({
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
  readonly contracts: ExecutionNativeScriptInvalidContracts;
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
  const stepIndex = 3;
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
    executionNativeScriptInvalidUsesDirectRoute({
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
  const next = executionNativeScriptInvalidSignerScanState({
    txId: state.bad_tx_id,
    addressWitnessItems,
    totalLength: planned.preimage.length,
    batchSize: EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH,
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
        direction: state.direction,
        execution_index: state.execution_index,
        source_index: state.source_index,
        origin_kind: state.origin_kind,
        item_commitment: state.item_commitment,
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
    address: contracts.steps[4].spendingScriptAddress,
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
              item_budget: BigInt(
                EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH,
              ),
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
    nextAddress: contracts.steps[4].spendingScriptAddress,
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
