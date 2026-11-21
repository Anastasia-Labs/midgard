/**
 * The cost model of a writeBits loop hop (`cek_write_bits_loop_v1`),
 * calibrated on the loop validator's measured ex-units.
 *
 * A hop pays a fixed cost, a cost per index it opens and per byte of those
 * indices' CBOR (each is hashed into its list node), and a cost per byte of
 * the input: a success carries the bytes so far in its state and hands them
 * on, and its first hop re-hashes the input's bytes and its last hop the
 * result's. A failure hop carries only the input's length, so it pays for the
 * input's bytes on its first hop alone. Each fixed cost is raised so that the
 * model is at least every calibration row's measured ex-units.
 */
import type { MidgardCekExUnits } from "./cek-loop-hop-budget.js";

/** What one hop does, as the cost model counts it. */
export type MidgardCekWriteBitsHopShape = {
  /** A success (arm 31) or a failure's walk (arm 36). */
  readonly failure: boolean;
  /** The first hop, which reveals the input and the list's node. */
  readonly first: boolean;
  /** A success's hop that opens the end of the list. */
  readonly last: boolean;
  /** Indices it opens. */
  readonly items: number;
  /** Bytes of their CBOR. */
  readonly itemBytes: number;
  /** Bytes of the input. */
  readonly inputBytes: number;
};

type Axis = "memory" | "cpu";
type Costs = Readonly<
  Record<
    | "fixed"
    | "first"
    | "last"
    | "items"
    | "itemBytes"
    | "inputBytes"
    | "firstInputBytes"
    | "lastInputBytes",
    bigint
  >
>;

/**
 * Calibrated on the hop rows of `generate-cek-write-bits-loop-v1-goldens.mjs`
 * (each row's units less its control row's).
 */
const COST: Readonly<Record<"success" | "failure", Record<Axis, Costs>>> = {
  success: {
    memory: {
      fixed: 568_378n,
      first: 92_937n,
      last: 118_574n,
      items: 120_053n,
      itemBytes: 564n,
      inputBytes: 38n,
      firstInputBytes: 9n,
      lastInputBytes: 9n,
    },
    cpu: {
      fixed: 216_115_020n,
      first: 45_452_735n,
      last: 49_845_682n,
      items: 57_263_206n,
      itemBytes: 114_773n,
      inputBytes: 9_422n,
      firstInputBytes: 3_384n,
      lastInputBytes: 104n,
    },
  },
  failure: {
    memory: {
      fixed: 380_865n,
      first: 178_674n,
      last: 0n,
      items: 125_293n,
      itemBytes: 0n,
      inputBytes: 30n,
      firstInputBytes: 13n,
      lastInputBytes: 5n,
    },
    cpu: {
      fixed: 148_646_330n,
      first: 72_340_309n,
      last: 2_369_243n,
      items: 58_308_592n,
      itemBytes: 0n,
      inputBytes: 4_667n,
      firstInputBytes: 4_154n,
      lastInputBytes: 717n,
    },
  },
};

/** The modelled ex-units of a hop of this shape. */
export const midgardCekWriteBitsHopCost = (
  shape: MidgardCekWriteBitsHopShape,
): MidgardCekExUnits => {
  const table = COST[shape.failure ? "failure" : "success"];
  const units = (axis: Axis): bigint => {
    const cost = table[axis];
    const input = BigInt(shape.inputBytes);
    return (
      cost.fixed +
      BigInt(shape.items) * cost.items +
      BigInt(shape.itemBytes) * cost.itemBytes +
      input * cost.inputBytes +
      (shape.first ? cost.first + input * cost.firstInputBytes : 0n) +
      (shape.last ? cost.last + input * cost.lastInputBytes : 0n)
    );
  };
  return { memory: units("memory"), cpu: units("cpu") };
};
