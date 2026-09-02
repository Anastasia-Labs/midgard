import {
  decodeMidgardNativeTxCompactV1,
  decodeMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  nativeScriptItemCommitmentV1,
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
  EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL as FAMILY,
  type ExecutionNativeScriptInvalidContractsV1,
} from "./contracts-v1.js";
import {
  ExecutionNativeScriptInvalidStep03DatumV1Schema,
  ExecutionNativeScriptInvalidStep03RedeemerV1Schema,
  ExecutionNativeScriptInvalidStep04DatumV1Schema,
} from "./schemas-v1.js";

type State = NonNullable<
  Data.Static<typeof ExecutionNativeScriptInvalidStep03DatumV1Schema>["data"]
>;

/** Binds canonical source bytes against the trace-authenticated bounded-item
 * commitment. The validator derives field 6/source_index for inline sources
 * and field 2/output_index(source_key) for resolved reference sources. */
export const submitExecutionNativeScriptInvalidStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  scriptItemCbor,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ExecutionNativeScriptInvalidContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly scriptItemCbor: Uint8Array;
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
    schema: ExecutionNativeScriptInvalidStep03DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  const script = decodeMidgardVersionedScript(scriptItemCbor);
  if (script.language !== "NativeCardano")
    throw new Error(`${label}: authenticated source is not a native script`);
  const compact = decodeMidgardNativeTxCompactV1(
    Buffer.from(state.compact_cbor, "hex"),
  );
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        direction: state.bound.subject.direction,
        execution_index: state.bound.execution_index,
        source_index: state.source_index,
        origin_kind: state.origin_kind,
        item_commitment: state.item_commitment,
        bad_tx_id: state.bound.subject.transaction_id,
        bad_tx_witness_set_hash: Buffer.from(
          compact.transactionWitnessSetHash,
        ).toString("hex"),
        script_item_hash: nativeScriptItemCommitmentV1(scriptItemCbor),
        validity_interval_start: compact.transactionBody.validityIntervalStart,
        validity_interval_end: compact.transactionBody.validityIntervalEnd,
      },
    } as never,
    ExecutionNativeScriptInvalidStep04DatumV1Schema as never,
  );
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
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
            input_index: inputIndex,
            output_index: outputIndex,
            script_item_cbor: Buffer.from(scriptItemCbor).toString("hex"),
          },
        ],
      } as never,
      ExecutionNativeScriptInvalidStep03RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinueV1({
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
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
  } as const;
};
