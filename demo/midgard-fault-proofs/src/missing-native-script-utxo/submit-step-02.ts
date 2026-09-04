import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  encodeMidgardTxInputCanonical,
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  type MidgardTxInput,
  MissingNativeScriptUtxoStep02DatumSchema,
  MissingNativeScriptUtxoStep02SpendRedeemerSchema,
  MissingNativeScriptUtxoStep03DatumSchema,
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
  MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL as FAMILY,
  type MissingNativeScriptUtxoContracts,
} from "./contracts.js";

type Step02State = NonNullable<
  Data.Static<typeof MissingNativeScriptUtxoStep02DatumSchema>["data"]
>;
type Step02Datum = Data.Static<typeof MissingNativeScriptUtxoStep02DatumSchema>;
const Step02Datum = asDataType<Step02Datum>(
  MissingNativeScriptUtxoStep02DatumSchema,
);
type Step03Datum = Data.Static<typeof MissingNativeScriptUtxoStep03DatumSchema>;
const Step03Datum = asDataType<Step03Datum>(
  MissingNativeScriptUtxoStep03DatumSchema,
);
type Step02Redeemer = Data.Static<
  typeof MissingNativeScriptUtxoStep02SpendRedeemerSchema
>;
const Step02Redeemer = asDataType<Step02Redeemer>(
  MissingNativeScriptUtxoStep02SpendRedeemerSchema,
);

export const submitMissingNativeScriptUtxoStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  spendInputs,
  badInputIndex,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingNativeScriptUtxoContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly spendInputs: readonly MidgardTxInput[];
  readonly badInputIndex: bigint;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
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
  const selected = spendInputs[Number(badInputIndex)];
  if (badInputIndex < 0n || selected === undefined) {
    throw new Error(`${label}: bad input index is outside the field`);
  }
  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: spendInputs.map(encodeMidgardTxInputCanonical),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: `${label} field 0`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${label} field 0`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const referenceInputs = [
    ...carriageUtxos,
    stepReference,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
  ];
  const opening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${label} field 0`,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        input_with_missing_script: selected,
        bad_tx_id: state.bad_tx_id,
        bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
        prev_utxos_root: state.prev_utxos_root,
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
            bad_input_index: badInputIndex,
            spend_inputs_opening: opening,
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
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    inputWithMissingScript: selected,
    carriageTier: planned.plan.tier,
  };
};
