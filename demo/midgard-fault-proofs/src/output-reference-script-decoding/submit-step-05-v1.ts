import { MidgardNativeScriptDecodingDirectionsV1 } from "@al-ft/midgard-core";
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
import {
  nativeScriptDecodingScanArgsEvidenceV1,
  nativeScriptDecodingWindowProofsV1,
} from "../native-script-decoding/evidence-v1.js";
import {
  buildNativeScriptDecodingScanPlanV1,
  NativeScriptDecodingPlanRoutesV1,
} from "../native-script-decoding/scan-plan-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type OutputReferenceScriptDecodingContractsV1,
} from "./contracts-v1.js";
import {
  outputReferenceScriptCheckpointV1,
  type OutputReferenceScriptDecodingEvidenceV1,
  OutputReferenceScriptResultClassesV1,
} from "./output-reference-script-decoding-v1.js";
import {
  OutputReferenceStep05DatumV1Schema,
  OutputReferenceStep05RedeemerV1Schema,
  OutputReferenceStep06DatumV1Schema,
} from "./schemas-v1.js";

type ScanState = {
  bound: {
    subject: OutputReferenceScriptDecodingEvidenceV1["subject"];
    output_index: bigint;
    accused_class: bigint;
  };
  total_length: bigint;
  item_commitment: string;
  control_cbor: string;
  next_expected_script_hash: string;
  checkpoint_hash: string;
  result_class: bigint;
};

export const submitOutputReferenceScriptDecodingStep05V1 = async ({
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
  readonly contracts: OutputReferenceScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: OutputReferenceScriptDecodingEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<ScanState>({
    threadUtxo,
    signer,
    schema: OutputReferenceStep05DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    state.item_commitment !== evidence.referenceScriptItemCommitmentHex ||
    state.total_length !==
      BigInt(Buffer.from(evidence.referenceScriptItemHex, "hex").length) ||
    state.checkpoint_hash !==
      outputReferenceScriptCheckpointV1({
        evidence,
        controlCbor: state.control_cbor,
        nextExpectedScriptHash: state.next_expected_script_hash,
      })
  )
    throw new Error(
      `${FAMILY}: authenticated scan state differs from evidence`,
    );
  let args: Record<string, unknown>;
  let nextControl = state.control_cbor;
  let nextClass = state.result_class;
  let closes =
    state.result_class !== BigInt(OutputReferenceScriptResultClassesV1.Pending);
  if (!closes) {
    const item = Buffer.from(evidence.referenceScriptItemHex, "hex");
    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: Number(evidence.subject.direction) as 0 | 1,
    });
    if (plan.route !== NativeScriptDecodingPlanRoutesV1.Machine)
      throw new Error(`${FAMILY}: pending state has non-machine plan`);
    const segment = plan.segments.find(
      ({ controlBefore }) => controlBefore.cborHex === state.control_cbor,
    );
    if (segment !== undefined) {
      args = nativeScriptDecodingScanArgsEvidenceV1({
        segment,
        fieldIndex: 2,
        itemIndex: evidence.outputIndex,
        itemBytes: item,
      });
      const isLast = plan.segments.at(-1) === segment;
      closes =
        isLast &&
        plan.direction ===
          MidgardNativeScriptDecodingDirectionsV1.WrongfulRejection;
      nextControl = closes ? state.control_cbor : segment.controlAfter.cborHex;
      nextClass = closes
        ? BigInt(OutputReferenceScriptResultClassesV1.NoFault)
        : BigInt(OutputReferenceScriptResultClassesV1.Pending);
    } else if (
      plan.verdict.control?.cborHex === state.control_cbor &&
      plan.verdict.refusalClass !== null
    ) {
      args = {
        control_cbor: state.control_cbor,
        ...nativeScriptDecodingWindowProofsV1({
          window: plan.verdict.window,
          fieldIndex: 2,
          itemIndex: evidence.outputIndex,
          itemBytes: item,
        }),
        frames: [],
        step_budget: 1n,
      };
      nextClass = BigInt(plan.verdict.refusalClass);
      closes = true;
    } else throw new Error(`${FAMILY}: checkpoint absent from exact scan plan`);
  } else
    args = {
      control_cbor: "",
      chunk_proof: null,
      next_chunk_proof: null,
      frames: [],
      step_budget: 0n,
    };
  const nextStepIndex = closes ? 5 : 4;
  const nextHash = contracts.steps[nextStepIndex].spendingScriptHash;
  const nextState: ScanState = {
    ...state,
    control_cbor: nextControl,
    next_expected_script_hash: nextHash,
    checkpoint_hash: outputReferenceScriptCheckpointV1({
      evidence,
      controlCbor: nextControl,
      nextExpectedScriptHash: nextHash,
    }),
    result_class: nextClass,
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    (closes
      ? OutputReferenceStep06DatumV1Schema
      : OutputReferenceStep05DatumV1Schema) as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[4].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step05`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step05`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step05 output`,
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, ...args },
        ],
      } as never,
      OutputReferenceStep05RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[4].spendingScript,
    stepRole: `${FAMILY} step05`,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: step05 layout unresolved`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    closed: closes,
  };
};
