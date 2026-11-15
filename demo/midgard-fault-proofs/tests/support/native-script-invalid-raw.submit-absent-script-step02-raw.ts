import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  MIDGARD_FIELD_INDEX,
  NativeScriptInvalidStep02DatumSchema,
  NativeScriptInvalidStep02SpendRedeemerSchema,
  NativeScriptInvalidStep03DatumSchema,
  type NativeTxWitnessSetCompact,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../../src/field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { NativeScriptInvalidContracts } from "../../src/native-script-invalid/contracts.js";
import { NATIVE_SCRIPT_INVALID_ABSENT_SCRIPT_ITEM_HASH } from "../../src/native-script-invalid/submit-step-02.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";

const FAMILY = "native-script-invalid";
const label = "raw native-script-invalid step-02 absent script";

type Step02Datum = Data.Static<typeof NativeScriptInvalidStep02DatumSchema>;
const Step02Datum = asDataType<Step02Datum>(
  NativeScriptInvalidStep02DatumSchema,
);
type Step03Datum = Data.Static<typeof NativeScriptInvalidStep03DatumSchema>;
const Step03Datum = asDataType<Step03Datum>(
  NativeScriptInvalidStep03DatumSchema,
);
type Step02Redeemer = Data.Static<
  typeof NativeScriptInvalidStep02SpendRedeemerSchema
>;
const Step02Redeemer = asDataType<Step02Redeemer>(
  NativeScriptInvalidStep02SpendRedeemerSchema,
);

/**
 * Native-script-invalid step 02 built by hand: the thread takes the
 * absent-script close for `scriptIndex`, with field 6 opened
 * directly from `scriptWitnessItems` and the absent state written to step 03,
 * whatever the field holds. No builder check runs on whether the index is
 * past the field's last script.
 */
export const submitRawNativeScriptInvalidAbsentScriptStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptWitnessItems,
  scriptIndex,
  publishedCarriageUtxos,
  referenceScriptUtxo,
}: {
  readonly lucid: Parameters<typeof submitLinearFaultContinue>[0]["lucid"];
  readonly contracts: NativeScriptInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly scriptWitnessItems: readonly Uint8Array[];
  /** The script index the bound subject names. */
  readonly scriptIndex: bigint;
  readonly publishedCarriageUtxos: readonly UTxO[];
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<NonNullable<Step02Datum["data"]>>({
    threadUtxo,
    signer,
    schema: Step02Datum,
    family: FAMILY,
    stepIndex,
  });
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: state.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: scriptWitnessItems,
    owner: signer.paymentKeyHash,
    publish: false,
    witnessSet,
    anchorWitnessSetHash: state.bad_tx_witness_set_hash,
    label,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: state.subject,
        bad_tx_id: state.bad_tx_id,
        bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
        script_item_hash: NATIVE_SCRIPT_INVALID_ABSENT_SCRIPT_ITEM_HASH,
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
  signer.selectWallet(lucid);
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    return Data.to(
      {
        Continue: [
          {
            SelectAbsentScript: {
              input_index: requireInputIndex(ctx, threadUtxo, label),
              output_index: requireUniqueOutputIndex(
                ctx.outputs,
                outputMatches,
                label,
              ),
              script_index: scriptIndex,
              script_tx_wits_opening: faultProofFieldOpening({
                planned,
                referenceInputs: ctx.referenceInputs,
                certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
                label,
              }),
              grammar_checkpoint_bytes: "",
            },
          },
        ],
      },
      Step02Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  return await submitLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: label,
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: publishedCarriageUtxos,
    awaitConfirmation: true,
  });
};
