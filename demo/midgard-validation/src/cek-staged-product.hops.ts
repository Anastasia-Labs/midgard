/**
 * The hops of the staged product loop (`cek_staged_product_v1`): a staged
 * multiplyInteger or division step is verified over the columns of a
 * product, a contiguous range of columns per transaction, each hop handing
 * the next one its cursor and state.
 *
 * A hop's range comes from a cost model calibrated on the validator's
 * measured ex-units, one for multiplyInteger and one for the divisions: a
 * hop pays a fixed cost, a cost per byte of the CBOR of the operands it
 * decodes again (a, b and, for a division, q), on the first hop a cost per
 * byte of every source it binds, and per column a cost, a cost per limb
 * product and a cost per limb it steps over to reach a column's first
 * product, plus a cost per column before its cursor. Each fixed cost is
 * raised so that the model is at least every calibration row's measured
 * ex-units, and the planner keeps a margin of the budget free on top. A hop
 * the model admits that still goes over budget is retried with its range
 * halved, down to one column, and every plan starts from the cursor the
 * chain holds, so a restart resumes where the last confirmed hop ended. A
 * step with a column no hop fits is indeterminate, never a crash.
 */
import {
  MIDGARD_CEK_LOOP_HOP_BUDGET,
  type MidgardCekExUnits,
  midgardCekLoopHopFits,
} from "./cek-loop-hop-budget.js";

/** Bytes of one limb. */
const LIMB_BYTES = 512;

/**
 * The most columns one hop covers: the claimed CBOR it reads stays well
 * below the 65,536 bytes one byte string argument may have.
 */
export const MIDGARD_CEK_STAGED_PRODUCT_MAX_HOP_COLUMNS = 64;

export {
  MIDGARD_CEK_LOOP_HOP_BUDGET as MIDGARD_CEK_STAGED_PRODUCT_HOP_BUDGET,
  MIDGARD_CEK_LOOP_HOP_MARGIN_PERCENT as MIDGARD_CEK_STAGED_PRODUCT_HOP_MARGIN_PERCENT,
  type MidgardCekExUnits,
} from "./cek-loop-hop-budget.js";

/** The calibrated costs of one hop, for one axis. */
type MidgardCekStagedProductCost = {
  /** Every hop. */
  readonly fixed: bigint;
  /** Each byte of the CBOR of the operands every hop decodes. */
  readonly perByte: bigint;
  /** On the first hop, each byte of the CBOR of every source it binds. */
  readonly firstPerByte: bigint;
  /** Each column. */
  readonly perColumn: bigint;
  /** Each limb product in a column. */
  readonly perTerm: bigint;
  /** Each limb stepped over to reach a column's first limb product. */
  readonly perSkipped: bigint;
  /** Each column before the hop's cursor. */
  readonly perCursor: bigint;
};

/**
 * Calibrated on multiplyInteger steps of 9,216, 16,384 and 32,768-byte
 * operands, and on modInteger, divideInteger, quotientInteger and
 * remainderInteger steps of 9,216 to 32,768-byte dividends, the planned
 * hops of the largest steps among them. A negative operand whose CBOR
 * argument is all 0xff bytes (the smallest argument, -2^262143, is one)
 * costs every hop more than any other operand of its size, so each fixed
 * cost is raised to the planned hops of the largest such steps as well.
 */
const COST: Readonly<
  Record<
    "product" | "division",
    Readonly<Record<"memory" | "cpu", MidgardCekStagedProductCost>>
  >
> = {
  product: {
    memory: {
      fixed: 1_898_617n,
      perByte: 73n,
      firstPerByte: 11n,
      perColumn: 84_956n,
      perTerm: 6_558n,
      perSkipped: 3_046n,
      perCursor: 2_404n,
    },
    cpu: {
      fixed: 616_064_498n,
      perByte: 30_689n,
      firstPerByte: 4_499n,
      perColumn: 39_641_639n,
      perTerm: 4_276_879n,
      perSkipped: 932_111n,
      perCursor: 720_197n,
    },
  },
  division: {
    memory: {
      fixed: 2_681_091n,
      perByte: 55n,
      firstPerByte: 12n,
      perColumn: 57_935n,
      perTerm: 8_691n,
      perSkipped: 2_715n,
      perCursor: 4_311n,
    },
    cpu: {
      fixed: 1_474_740_003n,
      perByte: 17_392n,
      firstPerByte: 4_460n,
      perColumn: 18_208_159n,
      perTerm: 5_299_432n,
      perSkipped: 869_917n,
      perCursor: 663_521n,
    },
  },
};

/** What the hops of one staged step read. */
export type MidgardCekStagedProductLoop = {
  /** Whether the step is a division (its cost model is the division one). */
  readonly division: boolean;
  /** The number of columns: the cursor the last hop ends at. */
  readonly columns: number;
  /** Limbs of the column factors: |a| (or |q|) and |b|. */
  readonly x: number;
  readonly y: number;
  /** Bytes of the CBOR of the operands every hop decodes: a, b (and q). */
  readonly decodedBytes: number;
  /** Bytes of the CBOR of every source: those and the claimed result. */
  readonly sourceBytes: number;
};

const magnitudeLimbs = (value: bigint): number => {
  const magnitude = value < 0n ? -value : value;
  return magnitude === 0n
    ? 0
    : Math.ceil(magnitude.toString(16).length / (2 * LIMB_BYTES));
};

/** Bytes of the CBOR of `value`: a bignum's bytes in 64-byte segments. */
const cborBytes = (value: bigint): number => {
  const argument = value < 0n ? -value - 1n : value;
  if (argument < 24n) return 1;
  if (argument < 1n << 64n) {
    return argument < 1n << 8n
      ? 2
      : argument < 1n << 16n
        ? 3
        : argument < 1n << 32n
          ? 5
          : 9;
  }
  const length = Math.ceil(argument.toString(16).length / 2);
  if (length <= 64) return (length < 24 ? 2 : 3) + length;
  const full = Math.floor(length / 64);
  const last = length - 64 * full;
  const tail = last === 0 ? 0 : last < 24 ? 1 + last : 2 + last;
  return 2 + 66 * full + tail + 1;
};

const sum = (values: readonly bigint[]): number =>
  values.reduce((total, value) => total + cborBytes(value), 0);

/**
 * The loop of builtin `tag` (2 to 6) over `a` and `b`; null for a step with
 * no loop (another builtin, or a division by zero).
 */
export const midgardCekStagedProductLoop = (
  tag: bigint,
  a: bigint,
  b: bigint,
): MidgardCekStagedProductLoop | null => {
  const la = magnitudeLimbs(a);
  const lb = magnitudeLimbs(b);
  if (tag === 2n) {
    return {
      division: false,
      columns: Math.max(1, la + lb),
      x: la,
      y: lb,
      decodedBytes: sum([a, b]),
      sourceBytes: sum([a, b, a * b]),
    };
  }
  if (tag < 3n || tag > 6n || b === 0n) return null;
  // quotientInteger's quotient, or divideInteger's: one further from zero
  // when the division leaves a remainder and the signs differ.
  const truncated = a / b;
  const floored =
    a % b !== 0n && a < 0n !== b < 0n ? truncated - 1n : truncated;
  const q = tag === 4n || tag === 5n ? truncated : floored;
  const decoded = [a, b, q];
  return {
    division: true,
    columns: Math.max(1, la, magnitudeLimbs(q) + lb),
    x: magnitudeLimbs(q),
    y: lb,
    decodedBytes: sum(decoded),
    sourceBytes: sum(
      tag === 5n || tag === 6n ? [...decoded, a - b * q] : decoded,
    ),
  };
};

/** The modelled ex-units of the hop over columns [cursor, end). */
export const midgardCekStagedProductHopCost = (
  loop: MidgardCekStagedProductLoop,
  cursor: number,
  end: number,
): MidgardCekExUnits => {
  let products = 0n;
  let skipped = 0n;
  for (let k = cursor; k < end; k += 1) {
    const low = Math.max(0, k - loop.y + 1);
    const high = Math.min(k, loop.x - 1);
    if (low <= high) {
      products += BigInt(high - low + 1);
      skipped += BigInt(low + loop.y - 1 - k + low);
    }
  }
  const model = COST[loop.division ? "division" : "product"];
  const units = (axis: "memory" | "cpu"): bigint => {
    const cost = model[axis];
    return (
      cost.fixed +
      BigInt(loop.decodedBytes) * cost.perByte +
      (cursor === 0 ? BigInt(loop.sourceBytes) * cost.firstPerByte : 0n) +
      BigInt(end - cursor) * cost.perColumn +
      products * cost.perTerm +
      skipped * cost.perSkipped +
      BigInt(cursor) * cost.perCursor
    );
  };
  return { memory: units("memory"), cpu: units("cpu") };
};

/**
 * Where a hop ends, or `indeterminate` when no hop from its cursor fits the
 * budget: the step cannot be proved in hops of that budget.
 */
export type MidgardCekStagedProductHopPlan =
  | { readonly kind: "hop"; readonly end: number }
  | { readonly kind: "indeterminate" };

const INDETERMINATE: MidgardCekStagedProductHopPlan = Object.freeze({
  kind: "indeterminate",
});

/**
 * Where the hop from `cursor` (the cursor the chain holds, 0 before the
 * first hop) ends: the most columns the model fits in `budget` with its
 * margin, at most {@link MIDGARD_CEK_STAGED_PRODUCT_MAX_HOP_COLUMNS}, and
 * indeterminate when not even the column at `cursor` fits.
 */
export const planMidgardCekStagedProductHop = (
  loop: MidgardCekStagedProductLoop,
  cursor: number,
  budget: MidgardCekExUnits = MIDGARD_CEK_LOOP_HOP_BUDGET,
): MidgardCekStagedProductHopPlan => {
  if (!Number.isSafeInteger(cursor) || cursor < 0 || cursor >= loop.columns) {
    throw new RangeError("the loop has no hop from this cursor");
  }
  if (
    !midgardCekLoopHopFits(
      midgardCekStagedProductHopCost(loop, cursor, cursor + 1),
      budget,
    )
  ) {
    return INDETERMINATE;
  }
  const last = Math.min(
    loop.columns,
    cursor + MIDGARD_CEK_STAGED_PRODUCT_MAX_HOP_COLUMNS,
  );
  let end = cursor + 1;
  while (
    end < last &&
    midgardCekLoopHopFits(
      midgardCekStagedProductHopCost(loop, cursor, end + 1),
      budget,
    )
  ) {
    end += 1;
  }
  return { kind: "hop", end };
};

/**
 * The hop from `cursor` that replaces one ending at `end` that went over
 * budget: half its columns, and at least one. A one-column hop that went
 * over cannot be made smaller, so the step is indeterminate.
 */
export const halveMidgardCekStagedProductHop = (
  cursor: number,
  end: number,
): MidgardCekStagedProductHopPlan =>
  end - cursor < 2
    ? INDETERMINATE
    : { kind: "hop", end: cursor + Math.floor((end - cursor) / 2) };
