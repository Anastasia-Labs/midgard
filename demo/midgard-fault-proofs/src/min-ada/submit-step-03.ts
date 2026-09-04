import { decodeMidgardLedgerOutputCommitment } from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  MinAdaStep03DatumSchema,
  MinAdaStep03SpendRedeemerSchema,
  MinAdaStep04DatumSchema,
  MinAdaStep05DatumSchema,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import { outputMeetsMinAda } from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

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
  MIN_ADA_CATEGORY_LABEL as FAMILY,
  type MinAdaContracts,
} from "./contracts.js";

type State = NonNullable<Data.Static<typeof MinAdaStep03DatumSchema>["data"]>;
type Step03Datum = Data.Static<typeof MinAdaStep03DatumSchema>;
const Step03Datum = asDataType<Step03Datum>(MinAdaStep03DatumSchema);
type Step04Datum = Data.Static<typeof MinAdaStep04DatumSchema>;
const Step04Datum = asDataType<Step04Datum>(MinAdaStep04DatumSchema);
type Step05Datum = Data.Static<typeof MinAdaStep05DatumSchema>;
const Step05Datum = asDataType<Step05Datum>(MinAdaStep05DatumSchema);
type Redeemer = Data.Static<typeof MinAdaStep03SpendRedeemerSchema>;
const Redeemer = asDataType<Redeemer>(MinAdaStep03SpendRedeemerSchema);

/** Applies the exact release-bound min-Ada predicate to the authenticated descriptor. */
export const submitMinAdaUtxoStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  coinsPerUtxoByte,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MinAdaContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly coinsPerUtxoByte: bigint;
  readonly referenceScriptUtxo: UTxO;
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
  const facts =
    "MinAdaTxDescriptor" in state
      ? state.MinAdaTxDescriptor
      : (() => {
          const descriptor = decodeMidgardLedgerOutputCommitment(
            Buffer.from(state.MinAdaUtxoDescriptor.descriptor_cbor, "hex"),
          );
          return {
            total_length: BigInt(descriptor.totalLength),
            lovelace: descriptor.lovelace,
          };
        })();
  if (
    coinsPerUtxoByte <= 0n ||
    outputMeetsMinAda(coinsPerUtxoByte, facts.total_length, facts.lovelace)
  ) {
    throw new Error(
      `${label}: authenticated descriptor does not violate min-Ada`,
    );
  }
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const isTx = "MinAdaTxDescriptor" in state;
  const nextDatum = isTx
    ? Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: "PredicateAndCulpabilityAuthenticated",
        },
        Step05Datum,
      )
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            out_ref_key: state.MinAdaUtxoDescriptor.out_ref_key,
            prev_utxos_root: state.MinAdaUtxoDescriptor.prev_utxos_root,
          },
        },
        Step04Datum,
      );
  const nextStepIndex = isTx ? 4 : 3;
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
      { Continue: [{ input_index: inputIndex, output_index: outputIndex }] },
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
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
  };
};
