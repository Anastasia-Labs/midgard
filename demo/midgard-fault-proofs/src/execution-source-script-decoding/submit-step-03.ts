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
import { buildNativeScriptDecodingChunkProof } from "../native-script-decoding/evidence.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ExecutionSourceScriptDecodingContracts } from "./contracts.js";
import {
  executionSourceScriptDecodingCheckpoint,
  type ExecutionSourceScriptDecodingEvidence,
  ExecutionSourceScriptDecodingResultClasses,
} from "./family.js";
import {
  AuthenticatedExecutionSourceSchema,
  ExecutionSourceScanStateSchema,
  ExecutionSourceStep03DatumSchema,
  ExecutionSourceStep03RedeemerSchema,
  ExecutionSourceStep04DatumSchema,
} from "./schemas.js";

const FAMILY = "execution-source-script-decoding";
export const submitExecutionSourceScriptDecodingStep03 = async ({
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
  lucid: LucidEvolution;
  contracts: ExecutionSourceScriptDecodingContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: ExecutionSourceScriptDecodingEvidence;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
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
  const source = requireLinearFaultStepState<
    Data.Static<typeof AuthenticatedExecutionSourceSchema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep03DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    source.item_commitment !== evidence.itemCommitmentHex ||
    source.total_length !== BigInt(evidence.itemLength)
  )
    throw new Error(`${FAMILY}: source item differs from retained evidence`);
  const itemIndex =
    source.origin_kind === 0n
      ? Number(source.source_index)
      : Buffer.from(source.source_key, "hex").readUInt16BE(36);
  const firstChunk = buildNativeScriptDecodingChunkProof({
    fieldIndex: source.origin_kind === 0n ? 6 : 2,
    itemIndex,
    itemBytes: Buffer.from(evidence.descriptor.scriptItemHex, "hex"),
    chunkIndex: 0,
  });
  const initialResult =
    evidence.initialControlCbor === ""
      ? evidence.resultClass
      : ExecutionSourceScriptDecodingResultClasses.Pending;
  const nextState: Data.Static<typeof ExecutionSourceScanStateSchema> = {
    source,
    control_cbor: evidence.initialControlCbor,
    next_expected_script_hash: contracts.steps[3].spendingScriptHash,
    checkpoint_hash: executionSourceScriptDecodingCheckpoint({
      evidence,
      controlCbor: evidence.initialControlCbor,
      nextExpectedScriptHash: contracts.steps[3].spendingScriptHash,
    }),
    result_class: BigInt(initialResult),
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    ExecutionSourceStep04DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 03`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 03`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step 03`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            first_chunk: firstChunk,
          },
        ],
      } as never,
      ExecutionSourceStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} step 03`,
    nextAddress: contracts.steps[3].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
