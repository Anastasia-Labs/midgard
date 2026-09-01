import { decodeMidgardLedgerOutputCommitmentV1 } from "@al-ft/midgard-core";
import {
  MinAdaStep03DatumSchema,
  MinAdaStep03SpendRedeemerSchema,
  MinAdaStep04DatumSchema,
  MinAdaStep05DatumSchema,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import { outputMeetsMinAdaV1 } from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

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
  MIN_ADA_CATEGORY_LABEL as FAMILY,
  type MinAdaContractsV1,
} from "./contracts-v1.js";

type State = NonNullable<Data.Static<typeof MinAdaStep03DatumSchema>["data"]>;
type Step03Datum = Data.Static<typeof MinAdaStep03DatumSchema>;
const Step03Datum = MinAdaStep03DatumSchema as unknown as Step03Datum;
type Step04Datum = Data.Static<typeof MinAdaStep04DatumSchema>;
const Step04Datum = MinAdaStep04DatumSchema as unknown as Step04Datum;
type Step05Datum = Data.Static<typeof MinAdaStep05DatumSchema>;
const Step05Datum = MinAdaStep05DatumSchema as unknown as Step05Datum;
type Redeemer = Data.Static<typeof MinAdaStep03SpendRedeemerSchema>;
const Redeemer = MinAdaStep03SpendRedeemerSchema as unknown as Redeemer;

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
  readonly contracts: MinAdaContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly coinsPerUtxoByte: bigint;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
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
  const facts =
    "MinAdaTxDescriptor" in state
      ? state.MinAdaTxDescriptor
      : (() => {
          const descriptor = decodeMidgardLedgerOutputCommitmentV1(
            Buffer.from(state.MinAdaUtxoDescriptor.descriptor_cbor, "hex"),
          );
          return {
            total_length: BigInt(descriptor.totalLength),
            lovelace: descriptor.lovelace,
          };
        })();
  if (
    coinsPerUtxoByte <= 0n ||
    outputMeetsMinAdaV1(coinsPerUtxoByte, facts.total_length, facts.lovelace)
  ) {
    throw new Error(
      `${label}: authenticated descriptor does not violate min-Ada`,
    );
  }
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScriptV1({
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
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
  };
};
