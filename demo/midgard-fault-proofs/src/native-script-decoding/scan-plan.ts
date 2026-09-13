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
  MidgardNativeScriptDecodingDirection,
  MidgardNativeScriptDecodingRefusalClass,
  MidgardNativeScriptScanFrame,
  MidgardNativeScriptStructureControl,
} from "@al-ft/midgard-core";
import {
  budgetedMidgardNativeScriptDecodingScan,
  buildMidgardNativeScriptDecodingTrace,
  encodeMidgardNativeScriptStructureControl,
  hashMidgardNativeScriptDecodingControl,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBoundedItemChunkCount,
  MidgardNativeScriptDecodingBindKinds,
  MidgardNativeScriptDecodingDirections,
  MidgardNativeScriptDecodingScanOutcomeKinds,
  midgardNativeScriptDecodingScanWindowForCursor,
  MidgardNativeScriptDecodingTraceOutcomeKinds,
  MidgardNativeScriptStructureStages,
} from "@al-ft/midgard-core";

import { NATIVE_SCRIPT_DECODING_CATEGORY_LABEL } from "./contracts.js";

/**
 * Execution-cost pins copied from the family exec ledger (compiler fork
 * `aiken v1.1.23+6801f62`, re-pinned 2026-08-25 with the §7.2 closing arm).
 * The unit-test suite cross-checks every value here against the ledger JSON
 * itself, so a ledger re-pin that moves a number goes red here instead of
 * silently splitting the planner from the measurement.
 */
export const NATIVE_SCRIPT_DECODING_EXEC_PINS = {
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
   * CPU envelope: the pinned direction-B terminal close — a complete fixture
   * step including its own fold, so conservative for the shorter partial row.
   */
  scanStepEnvelopeCpuUnits: 1_438_286_068,
  /** `advance_or_close_closes_a_direction_a_refusal`. */
  verdictWrongfulAcceptance: { mem: 2_708_220, cpu: 1_118_982_047 },
  /** `advance_or_close_closes_direction_b_at_the_exact_terminal`. */
  verdictWrongfulRejection: { mem: 3_579_557, cpu: 1_438_286_068 },
  /** `bind_descriptor_closes_a_non_native_direction_b_descriptor`. */
  descriptorContradictionClose: { mem: 2_602_137, cpu: 1_021_459_663 },
} as const;

/**
 * Default primitive-step budget per Scan transaction:
 * floor((basis − step envelope) / ceil(deep mem slope)) =
 * floor(12,200,000 / 745,793) = 16, the ledger note's "≈16 nodes per scan
 * transaction" priced at the worst (deep) slope.
 */
export const NATIVE_SCRIPT_DECODING_DEFAULT_MAX_STEPS_PER_TX = Math.floor(
  (NATIVE_SCRIPT_DECODING_EXEC_PINS.basisMemoryUnits -
    NATIVE_SCRIPT_DECODING_EXEC_PINS.scanStepEnvelopeMemoryUnits) /
    Math.ceil(
      NATIVE_SCRIPT_DECODING_EXEC_PINS.deepMemSlopeNumerator /
        NATIVE_SCRIPT_DECODING_EXEC_PINS.slopeDenominator,
    ),
);

export const NativeScriptDecodingPlanRoutes = Object.freeze({
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

export type NativeScriptDecodingPlanRoute =
  (typeof NativeScriptDecodingPlanRoutes)[keyof typeof NativeScriptDecodingPlanRoutes];

/**
 * The authenticated window a plan's transaction must carry: the chunk proof
 * for `chunkIndex` and — whenever the item has one — the adjacent following
 * chunk (`needNext`). Mirrors `engine.authenticated_scan_window_v1`.
 */
export type NativeScriptDecodingPlanWindow = {
  readonly chunkIndex: number;
  readonly needNext: boolean;
};

/** A control checkpoint as the submitters need it: value, CBOR and hash. */
export type NativeScriptDecodingPlanControl = {
  readonly control: MidgardNativeScriptStructureControl;
  readonly cborHex: string;
  readonly hashHex: string;
};

export type NativeScriptDecodingScanSegmentPlan = {
  readonly controlBefore: NativeScriptDecodingPlanControl;
  readonly controlAfter: NativeScriptDecodingPlanControl;
  readonly window: NativeScriptDecodingPlanWindow | null;
  /** Frame witnesses in exact consumption order (§7.4 hash-chained). */
  readonly frames: readonly MidgardNativeScriptScanFrame[];
  /** Exact primitive-step count — the fold stops here by budget, always. */
  readonly stepBudget: number;
  readonly predictedMemoryUnits: number;
  readonly predictedCpuUnits: number;
};

export type NativeScriptDecodingVerdictPlan = {
  /** The control the Verdict fold consumes (direction A: the refusing
   * control; direction B: the exact terminal; short circuits: absent). */
  readonly control: NativeScriptDecodingPlanControl | null;
  readonly window: NativeScriptDecodingPlanWindow | null;
  /** Pinned refusal class for direction A; `null` for direction B. */
  readonly refusalClass: MidgardNativeScriptDecodingRefusalClass | null;
  readonly predictedMemoryUnits: number;
  readonly predictedCpuUnits: number;
};

export type NativeScriptDecodingScanPlan = {
  readonly route: NativeScriptDecodingPlanRoute;
  readonly direction: MidgardNativeScriptDecodingDirection;
  /** Non-zero wrapper language tag on the descriptor-contradiction route. */
  readonly languageTag: number | null;
  readonly chunkCount: number;
  readonly maxStepsPerTx: number;
  readonly segments: readonly NativeScriptDecodingScanSegmentPlan[];
  readonly verdict: NativeScriptDecodingVerdictPlan;
};

const planError = (message: string): Error =>
  new Error(`${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} plan: ${message}`);

const planControl = (
  control: MidgardNativeScriptStructureControl,
): NativeScriptDecodingPlanControl => {
  const cbor = encodeMidgardNativeScriptStructureControl(control);
  return {
    control,
    cborHex: Buffer.from(cbor).toString("hex"),
    hashHex: hashMidgardNativeScriptDecodingControl(cbor).toString("hex"),
  };
};

const assertWithinBasis = (
  what: string,
  predicted: { readonly mem: number; readonly cpu: number },
): void => {
  const pins = NATIVE_SCRIPT_DECODING_EXEC_PINS;
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
  const pins = NATIVE_SCRIPT_DECODING_EXEC_PINS;
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
  typeof buildMidgardNativeScriptDecodingTrace
>["steps"][number];

const tokenChunkIndexOfStep = (step: TraceStep): number | null =>
  step.control.stage === MidgardNativeScriptStructureStages.Token
    ? Math.floor(step.control.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES)
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
