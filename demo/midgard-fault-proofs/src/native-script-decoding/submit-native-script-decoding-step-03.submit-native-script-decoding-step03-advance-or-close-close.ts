import {
  isExactMidgardNativeScriptStructureTerminal,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
} from "@al-ft/midgard-core";
import type {
  BoundedItemChunkProof,
  NativeScriptDecodingScanThreadState,
} from "@al-ft/midgard-sdk";
import {
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
  NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { type ResolvedProverSigner } from "../runtime.js";
import { type FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { NativeScriptDecodingContracts } from "./contracts.js";
import {
  nativeScriptDecodingScanArgsEvidence,
  nativeScriptDecodingWindowProofs,
} from "./evidence.js";
import {
  type NativeScriptDecodingScanSegmentPlan,
  type NativeScriptDecodingVerdictPlan,
} from "./scan-plan.js";
import {
  nativeScriptDecodingSubmitError,
  requireNativeScriptDecodingThreadUtxo,
} from "./submit-common.js";
import {
  ADVANCE_OR_CLOSE_INDEX,
  advanceStep03Thread,
  requireAnchoredItemBytes,
  requireBoundPendingState,
  requireStep03State,
  STEP_04_INDEX,
  step03Result,
  type SubmitNativeScriptDecodingStep03Result,
} from "./submit-native-script-decoding-step-03.advance-step03-thread.js";

// ## AdvanceOrClose

export const submitNativeScriptDecodingStep03AdvanceOrCloseSegment = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  segment,
  referenceScriptItemBytes,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly segment: NativeScriptDecodingScanSegmentPlan;
  readonly referenceScriptItemBytes: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: ADVANCE_OR_CLOSE_INDEX,
      threadOutRef,
    });
  const state = requireStep03State({
    threadUtxo,
    signer,
    stepIndex: ADVANCE_OR_CLOSE_INDEX,
  });
  requireBoundPendingState(state);
  if (segment.controlBefore.hashHex !== state.machine_state_hash) {
    throw nativeScriptDecodingSubmitError(
      "the segment's control is not the thread's committed machine.",
    );
  }
  requireAnchoredItemBytes({
    itemBytes: referenceScriptItemBytes,
    itemIndex: Number(state.output_index),
    totalLength: state.total_length,
    itemCommitmentHex: state.item_commitment,
  });
  const evidence = nativeScriptDecodingScanArgsEvidence({
    segment,
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
    itemIndex: Number(state.output_index),
    itemBytes: referenceScriptItemBytes,
  });

  const closesTerminal =
    state.direction === NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION &&
    isExactMidgardNativeScriptStructureTerminal(segment.controlAfter.control);
  const nextState: NativeScriptDecodingScanThreadState = closesTerminal
    ? {
        ...state,
        refusal_class: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
      }
    : { ...state, machine_state_hash: segment.controlAfter.hashHex };
  const destinationAddress = closesTerminal
    ? contracts.steps[STEP_04_INDEX].spendingScriptAddress
    : contracts.steps[ADVANCE_OR_CLOSE_INDEX].spendingScriptAddress;

  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    spendingStepIndex: ADVANCE_OR_CLOSE_INDEX,
    buildRedeemer: (resolved) =>
      Data.to(
        {
          Continue: [
            {
              input_index: resolved.inputIndex,
              output_index: resolved.outputIndex,
              control_cbor: evidence.control_cbor,
              chunk_proof: evidence.chunk_proof,
              next_chunk_proof: evidence.next_chunk_proof,
              frames: [...evidence.frames],
              step_budget: evidence.step_budget,
            },
          ],
        },
        NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
      ),
    carriageUtxos: [],
    referenceScriptUtxo,
    preSubmitBoundary,
    awaitConfirmation,
  });
  return step03Result({
    txHash,
    layout,
    signer,
    threadOutRef,
    threadToken,
    destinationAddress,
    scanState: nextState,
    awaitConfirmation,
  });
};

export const submitNativeScriptDecodingStep03AdvanceOrCloseClose = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  verdict,
  referenceScriptItemBytes,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly verdict: NativeScriptDecodingVerdictPlan;
  readonly referenceScriptItemBytes?: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: ADVANCE_OR_CLOSE_INDEX,
      threadOutRef,
    });
  const state = requireStep03State({
    threadUtxo,
    signer,
    stepIndex: ADVANCE_OR_CLOSE_INDEX,
  });
  requireBoundPendingState(state);
  if (verdict.control === null) {
    throw nativeScriptDecodingSubmitError(
      "the close plan carries no machine control.",
    );
  }
  if (verdict.control.hashHex !== state.machine_state_hash) {
    throw nativeScriptDecodingSubmitError(
      "the close control is not the thread's committed machine.",
    );
  }

  let refusalClass: bigint;
  let stepBudget: bigint;
  let chunkProof: BoundedItemChunkProof | null = null;
  let nextChunkProof: BoundedItemChunkProof | null = null;
  if (state.direction === NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION) {
    if (
      verdict.window !== null ||
      !isExactMidgardNativeScriptStructureTerminal(verdict.control.control)
    ) {
      throw nativeScriptDecodingSubmitError(
        "direction B closes only an exact, windowless terminal.",
      );
    }
    refusalClass = NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED;
    stepBudget = 0n;
  } else {
    if (
      state.direction !==
        NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE ||
      verdict.refusalClass === null
    ) {
      throw nativeScriptDecodingSubmitError(
        "direction A closes only with the planner's refusing primitive step.",
      );
    }
    refusalClass = BigInt(verdict.refusalClass);
    stepBudget = 1n;
    if (verdict.window !== null) {
      if (referenceScriptItemBytes === undefined) {
        throw nativeScriptDecodingSubmitError(
          "the refusing step reads a chunk window; supply the item bytes.",
        );
      }
      requireAnchoredItemBytes({
        itemBytes: referenceScriptItemBytes,
        itemIndex: Number(state.output_index),
        totalLength: state.total_length,
        itemCommitmentHex: state.item_commitment,
      });
      const proofs = nativeScriptDecodingWindowProofs({
        window: verdict.window,
        fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
        itemIndex: Number(state.output_index),
        itemBytes: referenceScriptItemBytes,
      });
      chunkProof = proofs.chunk_proof;
      nextChunkProof = proofs.next_chunk_proof;
    }
  }

  const nextState: NativeScriptDecodingScanThreadState = {
    ...state,
    refusal_class: refusalClass,
  };
  const destinationAddress =
    contracts.steps[STEP_04_INDEX].spendingScriptAddress;
  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    spendingStepIndex: ADVANCE_OR_CLOSE_INDEX,
    buildRedeemer: (resolved) =>
      Data.to(
        {
          Continue: [
            {
              input_index: resolved.inputIndex,
              output_index: resolved.outputIndex,
              control_cbor: verdict.control!.cborHex,
              chunk_proof: chunkProof,
              next_chunk_proof: nextChunkProof,
              frames: [],
              step_budget: stepBudget,
            },
          ],
        },
        NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
      ),
    carriageUtxos: [],
    referenceScriptUtxo,
    preSubmitBoundary,
    awaitConfirmation,
  });
  return step03Result({
    txHash,
    layout,
    signer,
    threadOutRef,
    threadToken,
    destinationAddress,
    scanState: nextState,
    awaitConfirmation,
  });
};
