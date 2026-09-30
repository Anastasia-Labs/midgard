import type { MidgardNativeScriptDecodingDirection } from "@al-ft/midgard-core";
import {
  budgetedMidgardNativeScriptDecodingScan,
  buildMidgardNativeScriptDecodingTrace,
  encodeMidgardNativeScriptStructureControl,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBoundedItemChunkCount,
  MidgardNativeScriptDecodingBindKinds,
  MidgardNativeScriptDecodingDirections,
  MidgardNativeScriptDecodingScanOutcomeKinds,
  midgardNativeScriptDecodingScanWindowForCursor,
  MidgardNativeScriptDecodingTraceOutcomeKinds,
  MidgardNativeScriptStructureStages,
} from "@al-ft/midgard-core";

import {
  assertWithinBasis,
  cutSegments,
  NATIVE_SCRIPT_DECODING_DEFAULT_MAX_STEPS_PER_TX,
  NATIVE_SCRIPT_DECODING_EXEC_PINS,
  NativeScriptDecodingPlanRoutes,
  type NativeScriptDecodingScanPlan,
  type NativeScriptDecodingVerdictPlan,
  planControl,
  planError,
  predictScanSegment,
} from "./scan-plan.native-script-decoding-exec-pins.js";

/**
 * Build the ordered transaction plans for one accused item in one claimed
 * direction. Throws when the claimed fault does not exist in that polarity,
 * when a policy-widened budget predicts over the execution basis, or when a
 * planned segment fails its own replay — every segment (and a direction-A
 * verdict) is re-executed through the engine twin's `budgeted_scan_v1` with
 * exactly the window, frames and budget the plan carries, and must land on
 * the planned `controlAfter` having consumed every frame.
 */
export const buildNativeScriptDecodingScanPlan = ({
  itemBytes,
  direction,
  maxStepsPerTx = NATIVE_SCRIPT_DECODING_DEFAULT_MAX_STEPS_PER_TX,
}: {
  readonly itemBytes: Uint8Array;
  readonly direction: MidgardNativeScriptDecodingDirection;
  /**
   * Policy override for the per-transaction primitive-step budget. Widening
   * past the default is allowed only as far as the basis prediction admits;
   * an over-basis prediction throws instead of planning.
   */
  readonly maxStepsPerTx?: number;
}): NativeScriptDecodingScanPlan => {
  if (
    direction !== MidgardNativeScriptDecodingDirections.WrongfulAcceptance &&
    direction !== MidgardNativeScriptDecodingDirections.WrongfulRejection
  ) {
    throw planError(`unknown direction ${String(direction)}`);
  }
  if (!Number.isSafeInteger(maxStepsPerTx) || maxStepsPerTx < 1) {
    throw planError(`maxStepsPerTx must be a positive integer`);
  }
  const pins = NATIVE_SCRIPT_DECODING_EXEC_PINS;
  const wrongfulAcceptance =
    direction === MidgardNativeScriptDecodingDirections.WrongfulAcceptance;
  const chunkCount = midgardBoundedItemChunkCount(itemBytes.length);
  const trace = buildMidgardNativeScriptDecodingTrace(itemBytes);

  if (trace.bind.kind === MidgardNativeScriptDecodingBindKinds.Malformed) {
    if (!wrongfulAcceptance) {
      throw planError(
        "the item is malformed at bind — that is a wrongful-acceptance " +
          "fault, not a wrongful rejection",
      );
    }
    const predicted = pins.verdictWrongfulAcceptance;
    assertWithinBasis("the bind-malformed verdict", predicted);
    return {
      route: NativeScriptDecodingPlanRoutes.BindMalformed,
      direction,
      languageTag: null,
      chunkCount,
      maxStepsPerTx,
      segments: [],
      verdict: {
        control: null,
        // Bind reads only the first authenticated chunk.
        window: { chunkIndex: 0, needNext: false },
        refusalClass: null,
        predictedMemoryUnits: predicted.mem,
        predictedCpuUnits: predicted.cpu,
      },
    };
  }
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKinds.NonNative) {
    if (wrongfulAcceptance) {
      throw planError(
        "the item carries a non-native language tag — closing against a " +
          "native-script accusation is a wrongful-rejection contradiction, " +
          "not a wrongful acceptance",
      );
    }
    const predicted = pins.descriptorContradictionClose;
    assertWithinBasis("the descriptor-contradiction close", predicted);
    return {
      route: NativeScriptDecodingPlanRoutes.DescriptorContradiction,
      direction,
      languageTag: trace.bind.languageTag,
      chunkCount,
      maxStepsPerTx,
      segments: [],
      verdict: {
        control: null,
        window: { chunkIndex: 0, needNext: false },
        refusalClass: null,
        predictedMemoryUnits: predicted.mem,
        predictedCpuUnits: predicted.cpu,
      },
    };
  }

  const outcome = trace.outcome;
  if (outcome === null) {
    throw planError("bound trace carried no outcome");
  }
  if (
    wrongfulAcceptance &&
    outcome.kind !== MidgardNativeScriptDecodingTraceOutcomeKinds.Refused
  ) {
    throw planError(
      "the item decodes to the exact terminal — there is no wrongful " +
        "acceptance to prove",
    );
  }
  if (
    !wrongfulAcceptance &&
    outcome.kind !== MidgardNativeScriptDecodingTraceOutcomeKinds.Terminal
  ) {
    throw planError(
      "the machine refuses the item — there is no wrongful rejection to prove",
    );
  }

  const segments = cutSegments(trace.steps, maxStepsPerTx).map((segment) => {
    const first = segment.steps[0];
    const last = segment.steps.at(-1);
    if (first === undefined || last === undefined) {
      throw planError("planned an empty segment");
    }
    const firstToken = segment.steps.find(
      (step) => step.control.stage === MidgardNativeScriptStructureStages.Token,
    );
    const window =
      segment.chunkIndex === null
        ? null
        : {
            chunkIndex: segment.chunkIndex,
            needNext: segment.chunkIndex + 1 < chunkCount,
          };
    const frames = segment.steps.flatMap((step) =>
      step.frame === null ? [] : [step.frame],
    );
    const stepBudget = segment.steps.length;
    const predicted = predictScanSegment(stepBudget);
    assertWithinBasis(`a ${stepBudget}-step scan segment`, predicted);
    // Replay the segment exactly as its transaction will fold it.
    const replay = budgetedMidgardNativeScriptDecodingScan({
      control: first.control,
      window:
        firstToken === undefined
          ? null
          : midgardNativeScriptDecodingScanWindowForCursor({
              itemBytes,
              cursor: firstToken.control.cursor,
            }),
      frames,
      maxSteps: stepBudget,
    });
    const controlAfter = planControl(last.next);
    if (
      replay.kind !== MidgardNativeScriptDecodingScanOutcomeKinds.Advanced ||
      replay.framesConsumed !== frames.length ||
      Buffer.from(
        encodeMidgardNativeScriptStructureControl(replay.control),
      ).toString("hex") !== controlAfter.cborHex
    ) {
      throw planError("a planned scan segment failed its replay");
    }
    return {
      controlBefore: planControl(first.control),
      controlAfter,
      window,
      frames,
      stepBudget,
      predictedMemoryUnits: predicted.mem,
      predictedCpuUnits: predicted.cpu,
    };
  });

  const verdictControl = planControl(outcome.control);
  let verdict: NativeScriptDecodingVerdictPlan;
  if (outcome.kind === MidgardNativeScriptDecodingTraceOutcomeKinds.Refused) {
    const refusingStage = outcome.control.stage;
    if (refusingStage === MidgardNativeScriptStructureStages.Frame) {
      // Frozen-twin invariant: frame steps advance or abort, never refuse.
      throw planError("impossible frame-stage refusal");
    }
    const window =
      refusingStage === MidgardNativeScriptStructureStages.Token
        ? {
            chunkIndex: Math.floor(
              outcome.control.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
            ),
            needNext:
              Math.floor(
                outcome.control.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
              ) +
                1 <
              chunkCount,
          }
        : null;
    const predicted = pins.verdictWrongfulAcceptance;
    assertWithinBasis("the wrongful-acceptance verdict", predicted);
    // The budget-1 Verdict fold must exhibit exactly the pinned refusal.
    const replay = budgetedMidgardNativeScriptDecodingScan({
      control: outcome.control,
      window:
        window === null
          ? null
          : midgardNativeScriptDecodingScanWindowForCursor({
              itemBytes,
              cursor: outcome.control.cursor,
            }),
      frames: [],
      maxSteps: 1,
    });
    if (
      replay.kind !== MidgardNativeScriptDecodingScanOutcomeKinds.Refused ||
      replay.refusalClass !== outcome.refusalClass
    ) {
      throw planError("the planned verdict failed its refusal replay");
    }
    verdict = {
      control: verdictControl,
      window,
      refusalClass: outcome.refusalClass,
      predictedMemoryUnits: predicted.mem,
      predictedCpuUnits: predicted.cpu,
    };
  } else {
    const predicted = pins.verdictWrongfulRejection;
    assertWithinBasis("the wrongful-rejection verdict", predicted);
    verdict = {
      control: verdictControl,
      window: null,
      refusalClass: null,
      predictedMemoryUnits: predicted.mem,
      predictedCpuUnits: predicted.cpu,
    };
  }

  // Continuity: each segment starts where the previous one landed, the
  // first starts at the bind control, and the last lands on the verdict's
  // control (direction A: the refusing control; direction B: the terminal).
  let expected = planControl(trace.bind.control).cborHex;
  for (const segment of segments) {
    if (segment.controlBefore.cborHex !== expected) {
      throw planError("segment chain lost control continuity");
    }
    expected = segment.controlAfter.cborHex;
  }
  if (expected !== verdictControl.cborHex) {
    throw planError("the segment chain does not land on the verdict control");
  }

  return {
    route: NativeScriptDecodingPlanRoutes.Machine,
    direction,
    languageTag: null,
    chunkCount,
    maxStepsPerTx,
    segments,
    verdict,
  };
};
