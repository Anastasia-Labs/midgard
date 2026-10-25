/**
 * The cost model of a serialise loop hop (`cek_serialise_loop_v1`), calibrated
 * on the loop validator's measured ex-units.
 *
 * A hop pays a fixed cost, a cost per staging output it names, per fresh
 * chunk it absorbs and per byte of its window; per walk step a cost by the
 * step's kind, plus a cost per 64-byte piece and per 4,095-byte chunk a leaf
 * step reads and hashes; and a cost per stack entry and entry byte it
 * restores below its own pushes and chains above its base when it hands the
 * stack on. Each fixed cost is raised so that the model is at least every
 * calibration row's measured ex-units.
 */
import type { MidgardCekExUnits } from "./cek-loop-hop-budget.js";
import type { MidgardCekSerialiseStepKind } from "./cek-serialise-walk.types.js";

/** What one hop does, as the cost model counts it. */
export type MidgardCekSerialiseHopShape = {
  /** Staging outputs the hop names. */
  readonly refs: number;
  /** Chunks it absorbs into R's frontier. */
  readonly freshChunks: number;
  /** Bytes of its window: the carried tail and the fresh runs. */
  readonly windowBytes: number;
  /** Walk steps by kind. */
  readonly steps: Readonly<Record<MidgardCekSerialiseStepKind, number>>;
  /** 64-byte pieces and 4,095-byte chunks its leaf steps read and hash. */
  readonly leafPieces: number;
  readonly leafChunks: number;
  /** Stack entries (and their bytes) it restores below its own pushes. */
  readonly restore: number;
  readonly restoreBytes: number;
  /** Stack entries (and their bytes) it chains above its base at the end. */
  readonly pushed: number;
  readonly pushedBytes: number;
};

type Axis = "memory" | "cpu";
type Feature = Exclude<keyof MidgardCekSerialiseHopShape, "steps">;
type Costs = Readonly<
  Record<"fixed" | Feature | MidgardCekSerialiseStepKind, bigint>
>;

/**
 * Calibrated on the 117 hop rows of `generate-cek-serialise-loop-v1-goldens.mjs`
 * (each row's units less its control row's): every value of
 * `cek-serialise-loop-v1.vectors.mjs` in hops of 1, 4, 32 and 256 steps and
 * budget-sized, up to the honest maximum shapes.
 */
const COST: Readonly<Record<Axis, Costs>> = {
  memory: {
    fixed: 830_176n,
    refs: 153_394n,
    freshChunks: 0n,
    windowBytes: 6n,
    integer: 47_388n,
    bytes: 86_669n,
    open: 40_674n,
    close: 0n,
    pop: 58_052n,
    popPair: 145_440n,
    finish: 69_081n,
    leaf: 112_558n,
    leafPieces: 33_501n,
    leafChunks: 0n,
    restore: 0n,
    restoreBytes: 0n,
    pushed: 0n,
    pushedBytes: 516n,
  },
  cpu: {
    fixed: 284_485_216n,
    refs: 41_727_861n,
    freshChunks: 0n,
    windowBytes: 1_977n,
    integer: 16_743_481n,
    bytes: 31_543_846n,
    open: 11_943_458n,
    close: 0n,
    pop: 36_257_101n,
    popPair: 39_593_355n,
    finish: 25_668_945n,
    leaf: 55_488_092n,
    leafPieces: 9_205_200n,
    leafChunks: 0n,
    restore: 0n,
    restoreBytes: 0n,
    pushed: 0n,
    pushedBytes: 431_229n,
  },
};

export const MIDGARD_CEK_SERIALISE_STEP_KINDS: readonly MidgardCekSerialiseStepKind[] =
  ["integer", "bytes", "open", "close", "pop", "popPair", "finish", "leaf"];

/** The modelled ex-units of a hop of this shape. */
export const midgardCekSerialiseHopCost = (
  shape: MidgardCekSerialiseHopShape,
): MidgardCekExUnits => {
  const units = (axis: Axis): bigint => {
    const cost = COST[axis];
    let total =
      cost.fixed +
      BigInt(shape.refs) * cost.refs +
      BigInt(shape.freshChunks) * cost.freshChunks +
      BigInt(shape.windowBytes) * cost.windowBytes +
      BigInt(shape.leafPieces) * cost.leafPieces +
      BigInt(shape.leafChunks) * cost.leafChunks +
      BigInt(shape.restore) * cost.restore +
      BigInt(shape.restoreBytes) * cost.restoreBytes +
      BigInt(shape.pushed) * cost.pushed +
      BigInt(shape.pushedBytes) * cost.pushedBytes;
    for (const kind of MIDGARD_CEK_SERIALISE_STEP_KINDS)
      total += BigInt(shape.steps[kind]) * cost[kind];
    return total;
  };
  return { memory: units("memory"), cpu: units("cpu") };
};
