/**
 * `native-script-decoding` segment planner (offchain plan §5.2/§5.3).
 *
 * Cuts the engine twin's whole-item trace into an ordered list of Scan
 * transaction plans plus one final Verdict plan, such that every plan's
 * on-chain fold provably lands exactly where the plan says it does:
 *
 * - a segment carries at most `maxStepsPerTx` primitive steps, and its
 *   `stepBudget` equals its exact step count so `budgeted_scan_v1` stops at
 *   the planned cut by budget exhaustion, never by a window/frame stall;
 * - a segment never spans a window change: every token step in a segment
 *   reads from the same authenticated chunk, and the mandatory
 *   chunk-plus-next window shape then makes the §33-byte safe-read margin
 *   unconditional (a full following chunk is ≥ 4,095 bytes of margin, and a
 *   window ending at the item's last byte satisfies the end-of-item arm);
 * - direction A (wrongful acceptance) folds every advanced step of the trace
 *   and leaves the refusing primitive step to a budget-1 Verdict fold, which
 *   carries the window only when the refusing control is token-stage — the
 *   frozen twin's frame steps can only advance or abort (witness error), so
 *   a refusal is always exhibited by a token or finalize step and the
 *   Verdict plan never needs a frame witness;
 * - direction B (wrongful rejection) folds through finalize to the exact
 *   terminal and the Verdict plan is windowless.
 *
 * Bind-level short circuits skip the machine entirely: an undecodable
 * wrapper closes for direction A (`bindMalformed`), a non-zero language tag
 * closes for direction B (`descriptorContradiction`). Either short circuit
 * requested with the opposite direction — like a machine trace whose outcome
 * contradicts the requested direction — throws: the fault does not exist in
 * the claimed polarity, and the planner refuses rather than letting a
 * submitter discover that on-chain.
 *
 * ExUnits discipline (§5.3): every plan carries a prediction derived from
 * the pinned exec ledger
 * (`onchain/aiken/scripts/native-script-decoding-engine-exec-ledger-v1.json`),
 * and the planner throws on any plan predicted over the 13.2M-mem/8B-cpu
 * GOAL_SPEC §3.3 basis. Scan predictions price every primitive step at the
 * ledger's deep per-NODE fold slope, which over-prices roughly 2–3× (a deep
 * node spends 2–3 primitive steps to earn one node's slope): the prediction
 * is a conservative ceiling, so "refuse to submit" can only fire early,
 * never lie low. Divergence between prediction and an emulator reading
 * beyond the fixture share is a finding, never a reason to raise a budget.
 */
import type {
  MidgardNativeScriptDecodingDirectionV1,
  MidgardNativeScriptDecodingRefusalClassV1,
  MidgardNativeScriptScanFrameV1,
  MidgardNativeScriptStructureControlV1,
} from "@al-ft/midgard-core";
import {
  budgetedMidgardNativeScriptDecodingScanV1,
  buildMidgardNativeScriptDecodingTraceV1,
  encodeMidgardNativeScriptStructureControlV1,
  hashMidgardNativeScriptDecodingControlV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  midgardBoundedItemChunkCountV1,
  MidgardNativeScriptDecodingBindKindsV1,
  MidgardNativeScriptDecodingDirectionsV1,
  MidgardNativeScriptDecodingScanOutcomeKindsV1,
  midgardNativeScriptDecodingScanWindowForCursorV1,
  MidgardNativeScriptDecodingTraceOutcomeKindsV1,
  MidgardNativeScriptStructureStagesV1,
} from "@al-ft/midgard-core";

import { NATIVE_SCRIPT_DECODING_CATEGORY_LABEL } from "./contracts-v1.js";

/**
 * Execution-cost pins copied from the family exec ledger (compiler fork
 * `aiken v1.1.23+6801f62`, re-pinned 2026-08-25 with the §7.2 closing arm).
 * The unit-test suite cross-checks every value here against the ledger JSON
 * itself, so a ledger re-pin that moves a number goes red here instead of
 * silently splitting the planner from the measurement.
 */
export const NATIVE_SCRIPT_DECODING_EXEC_PINS_V1 = {
  /** GOAL_SPEC §3.3 basis the whole family is priced against. */
  basisMemoryUnits: 13_200_000,
  basisCpuUnits: 8_000_000_000,
  /**
   * Deep fold slope between the ledger's 9- and 17-node rows, kept as an
   * exact numerator/denominator pair: (12,666,128 − 6,699,789) / 8 mem and
   * (5,080,012,828 − 2,667,230,989) / 8 cpu per node.
   */
  deepMemSlopeNumerator: 5_966_339,
  deepCpuSlopeNumerator: 2_412_781_839,
  slopeDenominator: 8,
  /**
   * The ≈1.0M per-transaction step envelope the ledger note prices outside
   * the fold rows (thread token, control commitments, datum shuffle).
   */
  scanStepEnvelopeMemoryUnits: 1_000_000,
  /**
   * CPU envelope: the whole pinned scan step row — a complete fixture step
   * including its own fold, so strictly conservative as an envelope.
   */
  scanStepEnvelopeCpuUnits: 1_786_546_593,
  /** `decoding_step_03_verdict_proves_a_malformed_payload`. */
  verdictWrongfulAcceptance: { mem: 3_199_717, cpu: 1_362_715_211 },
  /** `decoding_step_03_verdict_proves_the_terminal_for_direction_b`. */
  verdictWrongfulRejection: { mem: 3_396_356, cpu: 1_417_909_444 },
  /** `decoding_step_03_closes_a_descriptor_contradiction_for_direction_b`. */
  descriptorContradictionClose: { mem: 3_646_253, cpu: 1_443_683_241 },
} as const;

/**
 * Default primitive-step budget per Scan transaction:
 * floor((basis − step envelope) / ceil(deep mem slope)) =
 * floor(12,200,000 / 745,793) = 16, the ledger note's "≈16 nodes per scan
 * transaction" priced at the worst (deep) slope.
 */
export const NATIVE_SCRIPT_DECODING_DEFAULT_MAX_STEPS_PER_TX_V1 = Math.floor(
  (NATIVE_SCRIPT_DECODING_EXEC_PINS_V1.basisMemoryUnits -
    NATIVE_SCRIPT_DECODING_EXEC_PINS_V1.scanStepEnvelopeMemoryUnits) /
    Math.ceil(
      NATIVE_SCRIPT_DECODING_EXEC_PINS_V1.deepMemSlopeNumerator /
        NATIVE_SCRIPT_DECODING_EXEC_PINS_V1.slopeDenominator,
    ),
);

export const NativeScriptDecodingPlanRoutesV1 = Object.freeze({
  /** The staged machine: bind, zero or more Scan segments, Verdict. */
  Machine: "machine",
  /** Undecodable wrapper — direction-A close at bind, no scan segments. */
  BindMalformed: "bindMalformed",
  /**
   * Non-zero language tag against a native-script accusation — direction-B
   * descriptor-contradiction close, no scan segments.
   */
  DescriptorContradiction: "descriptorContradiction",
} as const);

export type NativeScriptDecodingPlanRouteV1 =
  (typeof NativeScriptDecodingPlanRoutesV1)[keyof typeof NativeScriptDecodingPlanRoutesV1];

/**
 * The authenticated window a plan's transaction must carry: the chunk proof
 * for `chunkIndex` and — whenever the item has one — the adjacent following
 * chunk (`needNext`). Mirrors `engine.authenticated_scan_window_v1`.
 */
export type NativeScriptDecodingPlanWindowV1 = {
  readonly chunkIndex: number;
  readonly needNext: boolean;
};

/** A control checkpoint as the submitters need it: value, CBOR and hash. */
export type NativeScriptDecodingPlanControlV1 = {
  readonly control: MidgardNativeScriptStructureControlV1;
  readonly cborHex: string;
  readonly hashHex: string;
};

export type NativeScriptDecodingScanSegmentPlanV1 = {
  readonly controlBefore: NativeScriptDecodingPlanControlV1;
  readonly controlAfter: NativeScriptDecodingPlanControlV1;
  readonly window: NativeScriptDecodingPlanWindowV1 | null;
  /** Frame witnesses in exact consumption order (§7.4 hash-chained). */
  readonly frames: readonly MidgardNativeScriptScanFrameV1[];
  /** Exact primitive-step count — the fold stops here by budget, always. */
  readonly stepBudget: number;
  readonly predictedMemoryUnits: number;
  readonly predictedCpuUnits: number;
};

export type NativeScriptDecodingVerdictPlanV1 = {
  /** The control the Verdict fold consumes (direction A: the refusing
   * control; direction B: the exact terminal; short circuits: absent). */
  readonly control: NativeScriptDecodingPlanControlV1 | null;
  readonly window: NativeScriptDecodingPlanWindowV1 | null;
  /** Pinned refusal class for direction A; `null` for direction B. */
  readonly refusalClass: MidgardNativeScriptDecodingRefusalClassV1 | null;
  readonly predictedMemoryUnits: number;
  readonly predictedCpuUnits: number;
};

export type NativeScriptDecodingScanPlanV1 = {
  readonly route: NativeScriptDecodingPlanRouteV1;
  readonly direction: MidgardNativeScriptDecodingDirectionV1;
  /** Non-zero wrapper language tag on the descriptor-contradiction route. */
  readonly languageTag: number | null;
  readonly chunkCount: number;
  readonly maxStepsPerTx: number;
  readonly segments: readonly NativeScriptDecodingScanSegmentPlanV1[];
  readonly verdict: NativeScriptDecodingVerdictPlanV1;
};

const planError = (message: string): Error =>
  new Error(`${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} plan: ${message}`);

const planControl = (
  control: MidgardNativeScriptStructureControlV1,
): NativeScriptDecodingPlanControlV1 => {
  const cbor = encodeMidgardNativeScriptStructureControlV1(control);
  return {
    control,
    cborHex: Buffer.from(cbor).toString("hex"),
    hashHex: hashMidgardNativeScriptDecodingControlV1(cbor).toString("hex"),
  };
};

const assertWithinBasis = (
  what: string,
  predicted: { readonly mem: number; readonly cpu: number },
): void => {
  const pins = NATIVE_SCRIPT_DECODING_EXEC_PINS_V1;
  if (
    predicted.mem > pins.basisMemoryUnits ||
    predicted.cpu > pins.basisCpuUnits
  ) {
    throw planError(
      `refusing to plan ${what} predicted over the execution basis ` +
        `(${predicted.mem} mem / ${predicted.cpu} cpu against ` +
        `${pins.basisMemoryUnits} / ${pins.basisCpuUnits})`,
    );
  }
};

const predictScanSegment = (
  stepCount: number,
): { readonly mem: number; readonly cpu: number } => {
  const pins = NATIVE_SCRIPT_DECODING_EXEC_PINS_V1;
  return {
    mem: Math.ceil(
      pins.scanStepEnvelopeMemoryUnits +
        (stepCount * pins.deepMemSlopeNumerator) / pins.slopeDenominator,
    ),
    cpu: Math.ceil(
      pins.scanStepEnvelopeCpuUnits +
        (stepCount * pins.deepCpuSlopeNumerator) / pins.slopeDenominator,
    ),
  };
};

type TraceStep = ReturnType<
  typeof buildMidgardNativeScriptDecodingTraceV1
>["steps"][number];

const tokenChunkIndexOfStep = (step: TraceStep): number | null =>
  step.control.stage === MidgardNativeScriptStructureStagesV1.Token
    ? Math.floor(step.control.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1)
    : null;

/**
 * Cut the trace's advanced steps into window-respecting, budget-respecting
 * segments. A cut happens when the running segment is full, or when a token
 * step reads from a different chunk than the segment's established one —
 * conservative relative to the fold's own safe-read stop (the window also
 * covers the following chunk), but it keeps "one segment, one window shape"
 * a planner invariant instead of a margin computation.
 */
const cutSegments = (
  steps: readonly TraceStep[],
  maxStepsPerTx: number,
): readonly {
  readonly steps: readonly TraceStep[];
  readonly chunkIndex: number | null;
}[] => {
  const segments: { steps: TraceStep[]; chunkIndex: number | null }[] = [];
  let current: { steps: TraceStep[]; chunkIndex: number | null } | null = null;
  for (const step of steps) {
    const tokenChunk = tokenChunkIndexOfStep(step);
    if (
      current === null ||
      current.steps.length >= maxStepsPerTx ||
      (tokenChunk !== null &&
        current.chunkIndex !== null &&
        tokenChunk !== current.chunkIndex)
    ) {
      current = { steps: [], chunkIndex: null };
      segments.push(current);
    }
    current.steps.push(step);
    if (tokenChunk !== null && current.chunkIndex === null) {
      current.chunkIndex = tokenChunk;
    }
  }
  return segments;
};

/**
 * Build the ordered transaction plans for one accused item in one claimed
 * direction. Throws when the claimed fault does not exist in that polarity,
 * when a policy-widened budget predicts over the execution basis, or when a
 * planned segment fails its own replay — every segment (and a direction-A
 * verdict) is re-executed through the engine twin's `budgeted_scan_v1` with
 * exactly the window, frames and budget the plan carries, and must land on
 * the planned `controlAfter` having consumed every frame.
 */
export const buildNativeScriptDecodingScanPlanV1 = ({
  itemBytes,
  direction,
  maxStepsPerTx = NATIVE_SCRIPT_DECODING_DEFAULT_MAX_STEPS_PER_TX_V1,
}: {
  readonly itemBytes: Uint8Array;
  readonly direction: MidgardNativeScriptDecodingDirectionV1;
  /**
   * Policy override for the per-transaction primitive-step budget. Widening
   * past the default is allowed only as far as the basis prediction admits;
   * an over-basis prediction throws instead of planning.
   */
  readonly maxStepsPerTx?: number;
}): NativeScriptDecodingScanPlanV1 => {
  if (
    direction !== MidgardNativeScriptDecodingDirectionsV1.WrongfulAcceptance &&
    direction !== MidgardNativeScriptDecodingDirectionsV1.WrongfulRejection
  ) {
    throw planError(`unknown direction ${String(direction)}`);
  }
  if (!Number.isSafeInteger(maxStepsPerTx) || maxStepsPerTx < 1) {
    throw planError(`maxStepsPerTx must be a positive integer`);
  }
  const pins = NATIVE_SCRIPT_DECODING_EXEC_PINS_V1;
  const wrongfulAcceptance =
    direction === MidgardNativeScriptDecodingDirectionsV1.WrongfulAcceptance;
  const chunkCount = midgardBoundedItemChunkCountV1(itemBytes.length);
  const trace = buildMidgardNativeScriptDecodingTraceV1(itemBytes);

  if (trace.bind.kind === MidgardNativeScriptDecodingBindKindsV1.Malformed) {
    if (!wrongfulAcceptance) {
      throw planError(
        "the item is malformed at bind — that is a wrongful-acceptance " +
          "fault, not a wrongful rejection",
      );
    }
    const predicted = pins.verdictWrongfulAcceptance;
    assertWithinBasis("the bind-malformed verdict", predicted);
    return {
      route: NativeScriptDecodingPlanRoutesV1.BindMalformed,
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
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKindsV1.NonNative) {
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
      route: NativeScriptDecodingPlanRoutesV1.DescriptorContradiction,
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
    outcome.kind !== MidgardNativeScriptDecodingTraceOutcomeKindsV1.Refused
  ) {
    throw planError(
      "the item decodes to the exact terminal — there is no wrongful " +
        "acceptance to prove",
    );
  }
  if (
    !wrongfulAcceptance &&
    outcome.kind !== MidgardNativeScriptDecodingTraceOutcomeKindsV1.Terminal
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
      (step) =>
        step.control.stage === MidgardNativeScriptStructureStagesV1.Token,
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
    const replay = budgetedMidgardNativeScriptDecodingScanV1({
      control: first.control,
      window:
        firstToken === undefined
          ? null
          : midgardNativeScriptDecodingScanWindowForCursorV1({
              itemBytes,
              cursor: firstToken.control.cursor,
            }),
      frames,
      maxSteps: stepBudget,
    });
    const controlAfter = planControl(last.next);
    if (
      replay.kind !== MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced ||
      replay.framesConsumed !== frames.length ||
      Buffer.from(
        encodeMidgardNativeScriptStructureControlV1(replay.control),
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
  let verdict: NativeScriptDecodingVerdictPlanV1;
  if (outcome.kind === MidgardNativeScriptDecodingTraceOutcomeKindsV1.Refused) {
    const refusingStage = outcome.control.stage;
    if (refusingStage === MidgardNativeScriptStructureStagesV1.Frame) {
      // Frozen-twin invariant: frame steps advance or abort, never refuse.
      throw planError("impossible frame-stage refusal");
    }
    const window =
      refusingStage === MidgardNativeScriptStructureStagesV1.Token
        ? {
            chunkIndex: Math.floor(
              outcome.control.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
            ),
            needNext:
              Math.floor(
                outcome.control.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
              ) +
                1 <
              chunkCount,
          }
        : null;
    const predicted = pins.verdictWrongfulAcceptance;
    assertWithinBasis("the wrongful-acceptance verdict", predicted);
    // The budget-1 Verdict fold must exhibit exactly the pinned refusal.
    const replay = budgetedMidgardNativeScriptDecodingScanV1({
      control: outcome.control,
      window:
        window === null
          ? null
          : midgardNativeScriptDecodingScanWindowForCursorV1({
              itemBytes,
              cursor: outcome.control.cursor,
            }),
      frames: [],
      maxSteps: 1,
    });
    if (
      replay.kind !== MidgardNativeScriptDecodingScanOutcomeKindsV1.Refused ||
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
    route: NativeScriptDecodingPlanRoutesV1.Machine,
    direction,
    languageTag: null,
    chunkCount,
    maxStepsPerTx,
    segments,
    verdict,
  };
};
