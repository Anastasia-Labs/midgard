import {
  decodeMidgardAddressBytes,
  decodeMidgardLedgerOutputCommitmentV1,
  decodeMidgardNativeScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  MissingNativeScriptUtxoStep04DatumSchema,
  MissingNativeScriptUtxoStep04SpendRedeemerSchema,
  MissingNativeScriptUtxoStep05DatumSchema,
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
  MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL as FAMILY,
  type MissingNativeScriptUtxoContractsV1,
} from "./contracts-v1.js";

type State = NonNullable<
  Data.Static<typeof MissingNativeScriptUtxoStep04DatumSchema>["data"]
>;
type Step04Datum = Data.Static<typeof MissingNativeScriptUtxoStep04DatumSchema>;
const Step04Datum =
  MissingNativeScriptUtxoStep04DatumSchema as unknown as Step04Datum;
type Step05Datum = Data.Static<typeof MissingNativeScriptUtxoStep05DatumSchema>;
const Step05Datum =
  MissingNativeScriptUtxoStep05DatumSchema as unknown as Step05Datum;
type Redeemer = Data.Static<
  typeof MissingNativeScriptUtxoStep04SpendRedeemerSchema
>;
const Redeemer =
  MissingNativeScriptUtxoStep04SpendRedeemerSchema as unknown as Redeemer;

export const submitMissingNativeScriptUtxoStep04 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  missingNativeScriptBytes,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingNativeScriptUtxoContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly missingNativeScriptBytes: string;
  readonly referenceScriptUtxo: UTxO;
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
  const scriptBytes = Buffer.from(missingNativeScriptBytes, "hex");
  const actualHash = hashMidgardVersionedScript({
    language: "NativeCardano",
    scriptBytes,
    nativeScript: decodeMidgardNativeScript(scriptBytes).script,
  });
  const descriptor = decodeMidgardLedgerOutputCommitmentV1(
    Buffer.from(state.descriptor_cbor, "hex"),
  );
  const credential = decodeMidgardAddressBytes(
    descriptor.address,
  ).paymentCredential;
  if (
    descriptor.outputIndex !== Number(state.out_ref.outputIndex) ||
    credential.kind !== "Script" ||
    actualHash !== credential.hash.toString("hex")
  ) {
    throw new Error(`${label}: native preimage does not hash to credential`);
  }
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        expected_missing_script_hash: actualHash,
        bad_tx_id: state.bad_tx_id,
        bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
        phase: "Ready",
      },
    },
    Step05Datum,
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
            input_index: inputIndex,
            output_index: outputIndex,
            missing_native_script_bytes: missingNativeScriptBytes,
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
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
  };
};
