import { decodeMidgardVersionedScript } from "@al-ft/midgard-core";
import {
  type FieldOpeningV1,
  MIDGARD_FIELD_INDEX_V1,
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
  EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL as FAMILY,
  type ExecutionNativeScriptInvalidContractsV1,
} from "./contracts-v1.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH_V1,
  executionNativeScriptInvalidSignerScanStateV1,
  executionNativeScriptInvalidUsesDirectRouteV1,
} from "./evidence-machine-v1.js";
import {
  ExecutionNativeScriptInvalidStep04DatumV1Schema,
  ExecutionNativeScriptInvalidStep04RedeemerV1Schema,
  ExecutionNativeScriptInvalidStep05DatumV1Schema,
} from "./schemas-v1.js";

type State = NonNullable<
  Data.Static<typeof ExecutionNativeScriptInvalidStep04DatumV1Schema>["data"]
>;
type Step03Datum = Data.Static<
  typeof ExecutionNativeScriptInvalidStep04DatumV1Schema
>;
const Step03Datum =
  ExecutionNativeScriptInvalidStep04DatumV1Schema as unknown as Step03Datum;
type Step04Datum = Data.Static<
  typeof ExecutionNativeScriptInvalidStep05DatumV1Schema
>;
const Step04Datum =
  ExecutionNativeScriptInvalidStep05DatumV1Schema as unknown as Step04Datum;
type Redeemer = Data.Static<
  typeof ExecutionNativeScriptInvalidStep04RedeemerV1Schema
>;
const Redeemer =
  ExecutionNativeScriptInvalidStep04RedeemerV1Schema as unknown as Redeemer;

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
  readonly contracts: ExecutionNativeScriptInvalidContractsV1;
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
    schema: Step03Datum,
    family: FAMILY,
    stepIndex,
  });
  const script = decodeMidgardVersionedScript(scriptItemCbor);
  if (script.language !== "NativeCardano") {
    throw new Error(`${label}: selected witness is not a native script`);
  }
  if (
    executionNativeScriptInvalidUsesDirectRouteV1({
      signerCount: addressWitnessItems.length,
      scriptBytes: script.scriptBytes.length,
    })
  ) {
    throw new Error(
      `${label}: direct path fits; staged scan is not admissible`,
    );
  }
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.addressWitnesses,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: addressWitnessItems,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet,
    anchorWitnessSetHash: state.bad_tx_witness_set_hash,
    label: `${label} staged field 7`,
  });
  const next = executionNativeScriptInvalidSignerScanStateV1({
    txId: state.bad_tx_id,
    addressWitnessItems,
    totalLength: planned.preimage.length,
    batchSize: EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH_V1,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriageV1({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${label} staged field 7`,
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
                EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH_V1,
              ),
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
