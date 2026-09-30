import type {
  MidgardNativeScriptDecodingDirection,
  MidgardNativeScriptDecodingRefusalClass,
  MidgardNativeScriptScanFrame,
  MidgardNativeScriptStructureControl,
} from "@al-ft/midgard-core";
import {
  buildMidgardNativeScriptDecodingTrace,
  encodeMidgardNativeScriptStructureControl,
  hashMidgardNativeScriptDecodingControl,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MidgardNativeScriptStructureStages,
} from "@al-ft/midgard-core";

import { NATIVE_SCRIPT_DECODING_CATEGORY_LABEL } from "./contracts.js";

/**
 * Execution-cost pins copied from the family exec ledger (compiler fork
 * `aiken v1.1.23+5adf783`, remeasured 2026-09-29).
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
  scanStepEnvelopeCpuUnits: 1_593_600_518,
  /** `advance_or_close_closes_a_direction_a_refusal`. */
  verdictWrongfulAcceptance: { mem: 3_031_303, cpu: 1_274_296_670 },
  /** `advance_or_close_closes_direction_b_at_the_exact_terminal`. */
  verdictWrongfulRejection: { mem: 3_902_639, cpu: 1_593_600_518 },
  /** `bind_descriptor_closes_a_non_native_direction_b_descriptor`. */
  descriptorContradictionClose: { mem: 2_720_814, cpu: 1_105_636_518 },
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

export const planError = (message: string): Error =>
  new Error(`${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} plan: ${message}`);

export const planControl = (
  control: MidgardNativeScriptStructureControl,
): NativeScriptDecodingPlanControl => {
  const cbor = encodeMidgardNativeScriptStructureControl(control);
  return {
    control,
    cborHex: Buffer.from(cbor).toString("hex"),
    hashHex: hashMidgardNativeScriptDecodingControl(cbor).toString("hex"),
  };
};

export const assertWithinBasis = (
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

export const predictScanSegment = (
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
export const cutSegments = (
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
