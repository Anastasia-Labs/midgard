import {
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
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type OutputReferenceScriptDecodingContracts,
} from "./contracts.js";
import {
  outputReferenceScriptControlData,
  type OutputReferenceScriptDecodingEvidence,
} from "./output-reference-script-decoding.js";
import {
  OutputReferenceOutputControlSchema,
  OutputReferenceStep03DatumSchema,
  OutputReferenceStep03RedeemerSchema,
  OutputReferenceStep04DatumSchema,
} from "./schemas.js";

export const submitOutputReferenceScriptDecodingStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: OutputReferenceScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: OutputReferenceScriptDecodingEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    bound: unknown;
    item_length: bigint;
    item_hash: string;
    chunk_hashes: readonly string[];
    control: { cursor: bigint; stage: bigint };
    outcome: bigint;
  }>({
    threadUtxo,
    signer,
    schema: OutputReferenceStep03DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    state.item_length !== BigInt(evidence.outputLength) ||
    state.item_hash !== evidence.outputHashHex ||
    state.chunk_hashes.join(":") !== evidence.outputChunkHashes.join(":") ||
    state.outcome !== 0n
  )
    throw new Error(`${FAMILY}: output scan checkpoint identity changed`);
  const encoded = Data.to(
    state.control as never,
    OutputReferenceOutputControlSchema as never,
  );
  const controlIndex = evidence.outputScanControls.findIndex(
    (control) =>
      Data.to(
        outputReferenceScriptControlData(control) as never,
        OutputReferenceOutputControlSchema as never,
      ) === encoded,
  );
  if (controlIndex < 0)
    throw new Error(
      `${FAMILY}: output scan checkpoint is outside authenticated trace`,
    );
  const nextControl = evidence.outputScanControls[controlIndex + 1];
  if (nextControl === undefined)
    throw new Error(`${FAMILY}: canonical output trace ended early`);
  const terminal = controlIndex + 1 === evidence.outputScanControls.length - 1;
  const nextState = {
    ...state,
    control: outputReferenceScriptControlData(nextControl),
    outcome: terminal ? 1n : 0n,
  };
  const nextStepIndex = terminal ? 3 : 2;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    (terminal
      ? OutputReferenceStep04DatumSchema
      : OutputReferenceStep03DatumSchema) as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step03`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step03`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step03 output`,
    );
    const item = Buffer.from(evidence.outputCborHex, "hex");
    const chunkStart = Math.floor(Number(state.control.cursor) / 4_095) * 4_095;
    const window = item
      .subarray(
        chunkStart,
        chunkStart + (state.control.stage <= 4n ? 8_190 : 4_095),
      )
      .toString("hex");
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, window },
        ],
      } as never,
      OutputReferenceStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[2].spendingScript,
    stepRole: `${FAMILY} step03`,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: [],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: step03 layout unresolved`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    terminal,
  };
};
