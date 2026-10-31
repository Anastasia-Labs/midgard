/** The loop arms of group 5's semantic stages (cek-semantic-chain
 * `continue_loop`): the staged product loop (semantics group 9), the
 * serialise loop (10) and the writeBits loop (12) each prove their step over
 * several transactions. Every hop but the last re-outputs the thread to the
 * arm's own script with its progress unchanged and the loop's facts
 * `LoopFacts { inner, cursor, state }` (constructor 1): the arm's facts, the
 * end of the range the hop covered and the state the next hop starts from.
 * The last hop restores the arm's facts and goes on to the settle with the
 * progress one higher. Each hop's redeemer names the end of its range, and
 * the serialise and writeBits hops also carry what the hop reads.
 *
 * The hops are planned here from the step's witness alone, deterministically,
 * so a runner started again plans the same hops and finds the one the chain
 * stands at by the cursor its facts hold. The serialise loop reads R from the
 * staging outputs the driver staged it on (one-step-staged.ts); before they
 * are staged it is planned over placeholder outputs, which plan the same
 * ranges. */
import {
  midgardCekCanonicalIntegerValue,
  type MidgardCekStagedOutRef,
  parseMidgardCekCanonicalIntegerCbor,
} from "@al-ft/midgard-core";
import {
  type MidgardCekCoreStepWitness,
  midgardCekSerialiseHopData,
  midgardCekSerialiseLoopOf,
  midgardCekSerialiseStateData,
  type MidgardCekStagedOperandSource,
  midgardCekStagedProductLoop,
  midgardCekStagedProductStateData,
  midgardCekStagedProductStates,
  midgardCekWriteBitsHopData,
  midgardCekWriteBitsLoopOfWitness,
  midgardCekWriteBitsStateData,
  planMidgardCekSerialiseHops,
  planMidgardCekStagedProductHop,
  planMidgardCekWriteBitsHops,
} from "@al-ft/midgard-validation";
import type { Data } from "@lucid-evolution/lucid";

/** The loop arms, by the semantic stage each is. */
export const CEK_CORE_LOOP_STAGES = Object.freeze([
  "stagedProduct",
  "semanticSerialiseLoop",
  "stagedWriteBitsLoop",
] as const);

export type CekCoreLoopStage = (typeof CEK_CORE_LOOP_STAGES)[number];

export const isCekCoreLoopStage = (stage: string): stage is CekCoreLoopStage =>
  (CEK_CORE_LOOP_STAGES as readonly string[]).includes(stage);

/** One hop of a loop: the end of its range, the state it hands on (null for
 * the hop that ends the loop) and, for the loops whose redeemer carries
 * one, what it reads. */
export type CekCoreLoopHop = Readonly<{
  end: number;
  state: Data | null;
  hop: Data | undefined;
}>;

/** The integer an inline or staged source of a staged step holds. */
const integerOf = (
  source: MidgardCekStagedOperandSource | undefined,
  stagedOperands: readonly Uint8Array[],
): bigint => {
  if (source?.kind === "smallInteger") return source.value;
  const bytes =
    source?.kind === "inline"
      ? source.bytes
      : source?.kind === "staged"
        ? stagedOperands[source.operand]
        : undefined;
  if (bytes === undefined)
    throw new Error("a staged product step's argument is not revealed");
  return midgardCekCanonicalIntegerValue(
    parseMidgardCekCanonicalIntegerCbor(bytes),
  );
};

/** The staged product loop's hops: from column 0, each as many columns as
 * the planner fits in the hop budget. */
const productHops = (witness: MidgardCekCoreStepWitness): CekCoreLoopHop[] => {
  if (witness.kind !== "executeBuiltinStaged")
    throw new Error("a staged product step has another witness");
  const [left, right] = witness.material.sources;
  const a = integerOf(left, witness.stagedOperands);
  const b = integerOf(right, witness.stagedOperands);
  const loop = midgardCekStagedProductLoop(witness.tag, a, b);
  if (loop === null) throw new Error("the staged product step has no loop");
  const states = midgardCekStagedProductStates(witness.tag, a, b);
  const hops: CekCoreLoopHop[] = [];
  for (let cursor = 0; cursor < loop.columns; ) {
    const planned = planMidgardCekStagedProductHop(loop, cursor);
    if (planned.kind === "indeterminate")
      throw new Error(
        `the staged product loop has no hop from column ${cursor.toString()} within the hop budget`,
      );
    const { end } = planned;
    hops.push({
      end,
      state:
        end === loop.columns
          ? null
          : midgardCekStagedProductStateData(states[end - 1]!),
      hop: undefined,
    });
    cursor = end;
  }
  return hops;
};

/** The serialise loop's hops over R's runs, staged on `runs` (one output a
 * run, in byte order) or on placeholder outputs. */
const serialiseHops = (
  witness: MidgardCekCoreStepWitness,
  runs: readonly MidgardCekStagedOutRef[] | undefined,
): CekCoreLoopHop[] => {
  if (witness.kind !== "executeBuiltinSerialise")
    throw new Error("a serialiseData step has another witness");
  return planMidgardCekSerialiseHops(
    midgardCekSerialiseLoopOf(witness, ...(runs === undefined ? [] : [runs])),
  ).map(({ end, hop, next }) => ({
    end,
    state: next === null ? null : midgardCekSerialiseStateData(next),
    hop: midgardCekSerialiseHopData(hop),
  }));
};

/** The writeBits loop's hops over the step's indices. */
const writeBitsHops = (
  witness: MidgardCekCoreStepWitness,
): CekCoreLoopHop[] => {
  if (witness.kind !== "executeBuiltinWriteBits")
    throw new Error("a writeBits step has another witness");
  return planMidgardCekWriteBitsHops(
    midgardCekWriteBitsLoopOfWitness(witness),
  ).map(({ end, hop, next }) => {
    if (next !== null && !("out" in next))
      throw new Error("a writeBits success hop hands on a failure's walk");
    return {
      end,
      state: next === null ? null : midgardCekWriteBitsStateData(next),
      hop: midgardCekWriteBitsHopData(hop),
    };
  });
};

/** The hops already planned for a witness, by the loop arm and the staging
 * outputs they read. The runner plans its thread again on every run, and
 * planning walks the whole loop (a writeBits step over the longest index list
 * takes 839 hops), so each run re-planning it made a thread's resolution
 * quadratic in its hops; the hops are a function of the witness and the
 * outputs alone. */
const plannedHops = new WeakMap<
  MidgardCekCoreStepWitness,
  Map<string, readonly CekCoreLoopHop[]>
>();

const hopsKey = (
  stage: CekCoreLoopStage,
  runs: readonly MidgardCekStagedOutRef[] | undefined,
): string =>
  [
    stage,
    ...(runs ?? []).map(
      ({ transactionId, outputIndex }) =>
        `${Buffer.from(transactionId).toString("hex")}#${outputIndex.toString()}`,
    ),
  ].join(" ");

/**
 * The hops of the loop arm `stage` over the step with witness `witness`; the
 * serialise loop reads R from the staging outputs `runs` when given. Throws
 * for a witness the loop does not take, or a step it cannot prove within the
 * hop budget.
 */
export const cekCoreLoopHops = (
  stage: CekCoreLoopStage,
  witness: MidgardCekCoreStepWitness,
  runs?: readonly MidgardCekStagedOutRef[],
): readonly CekCoreLoopHop[] => {
  const key = hopsKey(stage, runs);
  const known = plannedHops.get(witness)?.get(key);
  if (known !== undefined) return known;
  const hops = Object.freeze(
    stage === "stagedProduct"
      ? productHops(witness)
      : stage === "semanticSerialiseLoop"
        ? serialiseHops(witness, runs)
        : writeBitsHops(witness),
  );
  const byKey = plannedHops.get(witness) ?? new Map();
  plannedHops.set(witness, byKey.set(key, hops));
  return hops;
};
