import { decodeMidgardVersionedScript } from "@al-ft/midgard-core";
import {
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  NativeScriptInvalidStep02DatumSchema,
  NativeScriptInvalidStep02SpendRedeemerSchema,
  NativeScriptInvalidStep03DatumSchema,
  nativeScriptItemCommitment,
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

type Step02State = NonNullable<
  Data.Static<typeof NativeScriptInvalidStep02DatumSchema>["data"]
>;
type Step02Datum = Data.Static<typeof NativeScriptInvalidStep02DatumSchema>;
const Step02Datum =
  NativeScriptInvalidStep02DatumSchema as unknown as Step02Datum;
type Step03Datum = Data.Static<typeof NativeScriptInvalidStep03DatumSchema>;
const Step03Datum =
  NativeScriptInvalidStep03DatumSchema as unknown as Step03Datum;
type Step02Redeemer = Data.Static<
  typeof NativeScriptInvalidStep02SpendRedeemerSchema
>;
const Step02Redeemer =
  NativeScriptInvalidStep02SpendRedeemerSchema as unknown as Step02Redeemer;

export const submitNativeScriptInvalidStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptWitnessItems,
  scriptIndex,
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
  readonly scriptWitnessItems: readonly Uint8Array[];
  readonly scriptIndex: bigint;
  readonly publishCarriage?: boolean;
  /** Pre-observed publications supplied by a journaled production action. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** Pre-observed tier-3 certificate supplied by its journaled mint action. */
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const label = linearFaultStepLabel(FAMILY, stepIndex);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<Step02State>({
    threadUtxo,
    signer,
    schema: Step02Datum,
    family: FAMILY,
    stepIndex,
  });
  const item = scriptWitnessItems[Number(scriptIndex)];
  if (scriptIndex < 0n || item === undefined) {
    throw new Error(`${label}: script index is outside field 6`);
  }
  if (decodeMidgardVersionedScript(item).language !== "NativeCardano") {
    throw new Error(`${label}: selected witness is not a native script`);
  }
  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: scriptWitnessItems,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet,
    anchorWitnessSetHash: state.bad_tx_witness_set_hash,
    label: `${label} field 6`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${label} field 6`,
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
    label: `${label} field 6`,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bad_tx_id: state.bad_tx_id,
        bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
        script_item_hash: nativeScriptItemCommitment(item),
        validity_interval_start: state.validity_interval_start,
        validity_interval_end: state.validity_interval_end,
      },
    },
    Step03Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
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
            input_index: inputIndex,
            output_index: outputIndex,
            script_index: scriptIndex,
            script_tx_wits_opening: opening,
          },
        ],
      },
      Step02Redeemer,
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
    nextAddress: contracts.steps[2].spendingScriptAddress,
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
    scriptItemCbor: Buffer.from(item).toString("hex"),
    carriageTier: planned.plan.tier,
  };
};
