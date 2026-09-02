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
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import { buildNativeScriptDecodingChunkProofV1 } from "../native-script-decoding/evidence-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ExecutionSourceScriptDecodingContractsV1 } from "./contracts-v1.js";
import {
  executionSourceScriptDecodingCheckpointV1,
  type ExecutionSourceScriptDecodingEvidenceV1,
  ExecutionSourceScriptDecodingResultClassesV1,
} from "./family-v1.js";
import {
  AuthenticatedExecutionSourceV1Schema,
  ExecutionSourceScanStateV1Schema,
  ExecutionSourceStep03DatumV1Schema,
  ExecutionSourceStep03RedeemerV1Schema,
  ExecutionSourceStep04DatumV1Schema,
} from "./schemas-v1.js";

const FAMILY = "execution-source-script-decoding";
export const submitExecutionSourceScriptDecodingStep03V1 = async ({
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
  contracts: ExecutionSourceScriptDecodingContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: ExecutionSourceScriptDecodingEvidenceV1;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const source = requireLinearFaultStepStateV1<
    Data.Static<typeof AuthenticatedExecutionSourceV1Schema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep03DatumV1Schema as never,
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
  const firstChunk = buildNativeScriptDecodingChunkProofV1({
    fieldIndex: source.origin_kind === 0n ? 6 : 2,
    itemIndex,
    itemBytes: Buffer.from(evidence.descriptor.scriptItemHex, "hex"),
    chunkIndex: 0,
  });
  const initialResult =
    evidence.initialControlCbor === ""
      ? evidence.resultClass
      : ExecutionSourceScriptDecodingResultClassesV1.Pending;
  const nextState: Data.Static<typeof ExecutionSourceScanStateV1Schema> = {
    source,
    control_cbor: evidence.initialControlCbor,
    next_expected_script_hash: contracts.steps[3].spendingScriptHash,
    checkpoint_hash: executionSourceScriptDecodingCheckpointV1({
      evidence,
      controlCbor: evidence.initialControlCbor,
      nextExpectedScriptHash: contracts.steps[3].spendingScriptHash,
    }),
    result_class: BigInt(initialResult),
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    ExecutionSourceStep04DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScriptV1({
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
      ExecutionSourceStep03RedeemerV1Schema as never,
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
