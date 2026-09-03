import {
  MidgardNativeScriptDecodingDirections,
  type MidgardNativeScriptDecodingRefusalClass,
} from "@al-ft/midgard-core";
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
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import {
  nativeScriptDecodingScanArgsEvidence,
  nativeScriptDecodingWindowProofs,
} from "../native-script-decoding/evidence-v1.js";
import {
  buildNativeScriptDecodingScanPlan,
  NativeScriptDecodingPlanRoutes,
} from "../native-script-decoding/scan-plan-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { ExecutionSourceScriptDecodingContracts } from "./contracts-v1.js";
import {
  executionSourceScriptDecodingCheckpoint,
  type ExecutionSourceScriptDecodingEvidence,
  ExecutionSourceScriptDecodingResultClasses,
} from "./family-v1.js";
import {
  ExecutionSourceScanStateSchema,
  ExecutionSourceStep04DatumSchema,
  ExecutionSourceStep04RedeemerSchema,
  ExecutionSourceStep05DatumSchema,
} from "./schemas-v1.js";

const FAMILY = "execution-source-script-decoding";
const mappedRefusal = (
  value: MidgardNativeScriptDecodingRefusalClass,
): bigint => BigInt(value);

export const submitExecutionSourceScriptDecodingStep04 = async ({
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
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<
    Data.Static<typeof ExecutionSourceScanStateSchema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep04DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    state.source.item_commitment !== evidence.itemCommitmentHex ||
    state.checkpoint_hash !==
      executionSourceScriptDecodingCheckpoint({
        evidence,
        controlCbor: state.control_cbor,
        nextExpectedScriptHash: state.next_expected_script_hash,
      })
  )
    throw new Error(
      `${FAMILY}: scan checkpoint differs from retained evidence`,
    );
  const item = Buffer.from(evidence.descriptor.scriptItemHex, "hex");
  const fieldIndex = evidence.descriptor.originKind === 0 ? 6 : 2;
  const itemIndex =
    state.source.origin_kind === 0n
      ? Number(state.source.source_index)
      : Number(Buffer.from(state.source.source_key, "hex").readUInt16BE(36));
  let args;
  let nextControl = state.control_cbor;
  let nextClass = state.result_class;
  let closes =
    state.result_class !==
    BigInt(ExecutionSourceScriptDecodingResultClasses.Pending);
  if (!closes) {
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: Number(evidence.finding.subject.direction) as 0 | 1,
    });
    if (plan.route !== NativeScriptDecodingPlanRoutes.Machine)
      throw new Error(`${FAMILY}: pending state has no machine plan`);
    const segment = plan.segments.find(
      ({ controlBefore }) => controlBefore.cborHex === state.control_cbor,
    );
    if (segment !== undefined) {
      args = nativeScriptDecodingScanArgsEvidence({
        segment,
        fieldIndex,
        itemIndex,
        itemBytes: item,
      });
      const isLast = plan.segments.at(-1) === segment;
      closes =
        isLast &&
        plan.direction ===
          MidgardNativeScriptDecodingDirections.WrongfulRejection;
      nextControl = closes ? state.control_cbor : segment.controlAfter.cborHex;
      nextClass = closes
        ? BigInt(ExecutionSourceScriptDecodingResultClasses.NoFault)
        : BigInt(ExecutionSourceScriptDecodingResultClasses.Pending);
    } else if (plan.verdict.control?.cborHex === state.control_cbor) {
      if (plan.verdict.refusalClass === null)
        throw new Error(`${FAMILY}: terminal plan was not closed by segment`);
      args = {
        control_cbor: state.control_cbor,
        ...nativeScriptDecodingWindowProofs({
          window: plan.verdict.window,
          fieldIndex,
          itemIndex,
          itemBytes: item,
        }),
        frames: [],
        step_budget: 1n,
      };
      nextClass = mappedRefusal(plan.verdict.refusalClass);
      closes = true;
    } else
      throw new Error(`${FAMILY}: checkpoint is absent from exact scan plan`);
  } else {
    args = {
      control_cbor: "",
      chunk_proof: null,
      next_chunk_proof: null,
      frames: [],
      step_budget: 0n,
    };
  }
  const nextStepIndex = closes ? 4 : 3;
  const nextExpectedScriptHash =
    contracts.steps[nextStepIndex].spendingScriptHash;
  const nextState: Data.Static<typeof ExecutionSourceScanStateSchema> = {
    ...state,
    control_cbor: nextControl,
    next_expected_script_hash: nextExpectedScriptHash,
    checkpoint_hash: executionSourceScriptDecodingCheckpoint({
      evidence,
      controlCbor: nextControl,
      nextExpectedScriptHash,
    }),
    result_class: nextClass,
  };
  const nextSchema = closes
    ? ExecutionSourceStep05DatumSchema
    : ExecutionSourceStep04DatumSchema;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    nextSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
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
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 04`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 04`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step 04`,
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, ...args },
        ],
      } as never,
      ExecutionSourceStep04RedeemerSchema as never,
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
    stepRole: `${FAMILY} step 04`,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    closed: closes,
  };
};
