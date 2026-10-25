/**
 * The hops of the serialise loop (`cek_serialise_loop_v1`): an honest
 * prover's plan for reading a serialiseData result's staged bytes R, hop by
 * hop, within the hop budget.
 *
 * A hop takes as many steps as the hop cost model
 * (`cek-serialise-loop.cost.ts`) fits in the hop budget with the loop margin
 * kept free.
 *
 * A hop reads the carried tail first and absorbs the next run whenever the
 * walk needs bytes the window does not hold, as long as it then reads into
 * that run (the tail it leaves must lie in the last run it absorbs). A hop
 * the model admits that still goes over budget is replanned with half its
 * steps, which always makes progress (one step at the least). Planning is
 * deterministic, so a restart replays the plan to the cursor the chain holds.
 *
 * A hop retries with one more run only while the window cut it short, and
 * each retry re-walks the hop's window. The runs one hop reads into are
 * bounded by the hop budget, so planning time is linear in R's length times
 * that bound.
 */
import {
  compareMidgardCekStagedOutRefs,
  type MidgardCekStagedOutRef,
  type MidgardCekStagedReferenceInput,
} from "@al-ft/midgard-core";
import { lucidDataToCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";

import {
  MIDGARD_CEK_LOOP_HOP_BUDGET,
  type MidgardCekExUnits,
  midgardCekLoopHopFits,
} from "./cek-loop-hop-budget.js";
import {
  midgardCekSerialiseHopCost,
  type MidgardCekSerialiseHopShape,
} from "./cek-serialise-loop.cost.js";
import {
  advanceMidgardCekSerialiseHop,
  MIDGARD_CEK_SERIALISE_INITIAL_STATE,
  midgardCekSerialiseEntriesOf,
  type MidgardCekSerialiseFacts,
  type MidgardCekSerialiseHop,
  type MidgardCekSerialiseState,
} from "./cek-serialise-loop.js";
import {
  midgardCekSerialiseEntryData,
  midgardCekSerialiseStackRoot,
  takeMidgardCekSerialiseEntries,
} from "./cek-serialise-walk.data.js";
import { stepMidgardCekSerialise } from "./cek-serialise-walk.js";
import type {
  MidgardCekSerialiseEntry,
  MidgardCekSerialiseStack,
  MidgardCekSerialiseStepKind,
} from "./cek-serialise-walk.types.js";

/**
 * A serialiseData step's loop: its facts and R's runs, in byte order, with
 * the staging output each run is staged on.
 */
export type MidgardCekSerialiseLoop = {
  readonly facts: MidgardCekSerialiseFacts;
  readonly runs: readonly (readonly Buffer[])[];
  readonly refs: readonly MidgardCekStagedOutRef[];
};

/** Where a plan stands between hops: the chain's cursor and state, and the
 * prover's whole stack and runs absorbed. */
export type MidgardCekSerialisePoint = {
  readonly cursor: number;
  readonly state: MidgardCekSerialiseState;
  readonly stack: MidgardCekSerialiseStack;
  readonly absorbed: number;
  readonly carried: number | null;
};

export type MidgardCekSerialisePlannedHop = {
  /** The cursor and state the hop starts from; null for the first hop. */
  readonly from: {
    readonly cursor: number;
    readonly state: MidgardCekSerialiseState;
  } | null;
  readonly end: number;
  readonly hop: MidgardCekSerialiseHop;
  /** The reference inputs the hop reads: its staging outputs, ledger order. */
  readonly referenceInputs: readonly MidgardCekStagedReferenceInput[];
  /** The state it hands on; null for the hop that ends the loop. */
  readonly next: MidgardCekSerialiseState | null;
  readonly shape: MidgardCekSerialiseHopShape;
  readonly cost: MidgardCekExUnits;
  /** Where the plan stands after it; null after the last hop. */
  readonly point: MidgardCekSerialisePoint | null;
};

export const MIDGARD_CEK_SERIALISE_START: MidgardCekSerialisePoint =
  Object.freeze({
    cursor: 0,
    state: MIDGARD_CEK_SERIALISE_INITIAL_STATE,
    stack: null,
    absorbed: 0,
    carried: null,
  });

const entrySize = (entry: MidgardCekSerialiseEntry): number =>
  lucidDataToCborIterative(midgardCekSerialiseEntryData(entry)).length;

const emptySteps = (): Record<MidgardCekSerialiseStepKind, number> => ({
  integer: 0,
  bytes: 0,
  open: 0,
  close: 0,
  pop: 0,
  popPair: 0,
  finish: 0,
  leaf: 0,
});

/**
 * Why a trial stopped: it ended the loop, took `limit` steps, would go over
 * the budget with one more step, or needs bytes past its window.
 */
type TrialStop = "finished" | "limit" | "budget" | "window";

type Trial = {
  readonly steps: number;
  readonly at: number;
  readonly stop: TrialStop;
  readonly restore: number;
  readonly shape: MidgardCekSerialiseHopShape;
};

const runBytes = (run: readonly Buffer[]): number =>
  run.reduce((total, chunk) => total + chunk.length, 0);

/**
 * The longest run of steps from `point` over a window of the carried tail and
 * `fresh` more runs that the model fits in `budget`, at most `limit` steps.
 */
const trial = (
  loop: MidgardCekSerialiseLoop,
  point: MidgardCekSerialisePoint,
  fresh: number,
  limit: number,
  budget: MidgardCekExUnits,
): Trial => {
  const pieces: Buffer[] = [];
  if (point.carried !== null && point.state.carry !== null) {
    const carried = Buffer.concat(loop.runs[point.carried]!);
    pieces.push(carried.subarray(point.state.carry.offset));
  }
  const freshRuns = loop.runs.slice(point.absorbed, point.absorbed + fresh);
  for (const run of freshRuns) pieces.push(...run);
  const window = Buffer.concat(pieces);
  const freshChunks = freshRuns.reduce((total, run) => total + run.length, 0);
  const steps = emptySteps();
  const startEntries = midgardCekSerialiseEntriesOf(point.stack);
  const above: number[] = [];
  let aboveBytes = 0;
  let restore = 0;
  let restoreBytes = 0;
  let leafPieces = 0;
  let leafChunks = 0;
  let at = 0;
  let frame = point.state.frame;
  let stack = point.stack;
  let taken = 0;
  const refs = (point.carried === null ? 0 : 1) + fresh;
  const shapeOf = (): MidgardCekSerialiseHopShape => ({
    refs,
    freshChunks,
    windowBytes: window.length,
    steps: { ...steps },
    leafPieces,
    leafChunks,
    restore,
    restoreBytes,
    pushed: above.length,
    pushedBytes: aboveBytes,
  });
  let stop: TrialStop = "limit";
  let accepted: Omit<Trial, "stop"> = {
    steps: 0,
    at: 0,
    restore: 0,
    shape: shapeOf(),
  };
  while (taken < limit) {
    const { progress, step } = stepMidgardCekSerialise(
      window,
      at,
      frame,
      stack,
      window.length,
    );
    if (progress.kind === "refused") {
      stop = "window";
      break;
    }
    const saved = {
      above: [...above],
      aboveBytes,
      restore,
      restoreBytes,
      leafPieces,
      leafChunks,
    };
    const next = progress.kind === "advanced" ? progress.stack : null;
    // The entries the step popped (top first) and pushed (bottom first).
    let left = stack;
    let right = next;
    const pushed: MidgardCekSerialiseEntry[] = [];
    while (left !== right) {
      if ((left?.depth ?? 0) >= (right?.depth ?? 0)) {
        if (above.length > 0) aboveBytes -= above.pop()!;
        else {
          restoreBytes += entrySize(startEntries[restore]!);
          restore += 1;
        }
        left = left!.below;
      } else {
        pushed.unshift(right!.entry);
        right = right!.below;
      }
    }
    for (const entry of pushed) {
      const size = entrySize(entry);
      above.push(size);
      aboveBytes += size;
    }
    steps[step.kind] += 1;
    if (step.kind === "leaf") {
      leafPieces += Math.ceil(step.read / 66);
      leafChunks += step.chunks;
    }
    if (!midgardCekLoopHopFits(midgardCekSerialiseHopCost(shapeOf()), budget)) {
      steps[step.kind] -= 1;
      above.splice(0, above.length, ...saved.above);
      ({ aboveBytes, restore, restoreBytes, leafPieces, leafChunks } = saved);
      stop = "budget";
      break;
    }
    taken += 1;
    if (progress.kind === "finished") {
      return {
        steps: taken,
        at: window.length,
        stop: "finished",
        restore: startEntries.length,
        shape: shapeOf(),
      };
    }
    at = progress.at;
    frame = progress.frame;
    stack = progress.stack;
    accepted = { steps: taken, at, restore, shape: shapeOf() };
  }
  // A hop that absorbs runs must leave its tail in the last of them.
  if (fresh > 0 && accepted.at < window.length) {
    const start = window.length - runBytes(freshRuns.at(-1)!);
    if (accepted.at < start) return { ...accepted, stop, steps: 0 };
  }
  return { ...accepted, stop };
};

/** The hop's staging outputs in ledger order and the byte-order indices. */
const hopRuns = (
  loop: MidgardCekSerialiseLoop,
  runIndices: readonly number[],
): {
  readonly refs: MidgardCekStagedOutRef[];
  readonly order: number[];
  readonly referenceInputs: MidgardCekStagedReferenceInput[];
} => {
  const sorted = [...runIndices].sort((left, right) =>
    compareMidgardCekStagedOutRefs(loop.refs[left]!, loop.refs[right]!),
  );
  return {
    refs: sorted.map((index) => loop.refs[index]!),
    order: runIndices.map((index) => sorted.indexOf(index)),
    referenceInputs: sorted.map((index) => ({
      outRef: loop.refs[index]!,
      chunks: loop.runs[index]!,
    })),
  };
};

/**
 * The hop from `point`: at most `limit` steps, the most the model fits in
 * `budget` with the loop margin, over the carried tail and as many more runs
 * as those steps read into.
 */
export const planMidgardCekSerialiseHop = (
  loop: MidgardCekSerialiseLoop,
  point: MidgardCekSerialisePoint,
  options: {
    readonly limit?: number;
    readonly budget?: MidgardCekExUnits;
  } = {},
): MidgardCekSerialisePlannedHop => {
  const limit = options.limit ?? Number.MAX_SAFE_INTEGER;
  const budget = options.budget ?? MIDGARD_CEK_LOOP_HOP_BUDGET;
  const remaining = loop.runs.length - point.absorbed;
  let best: { readonly fresh: number; readonly trial: Trial } | null = null;
  for (
    let fresh = point.carried === null && remaining > 0 ? 1 : 0;
    fresh <= remaining;
    fresh += 1
  ) {
    const tried = trial(loop, point, fresh, limit, budget);
    if (tried.steps > 0 && (best === null || tried.steps > best.trial.steps))
      best = { fresh, trial: tried };
    // More runs help only a hop the window cut short: a wider window costs
    // more, so a hop stopped by its limit or budget takes no more steps.
    if (tried.stop !== "window") break;
  }
  if (best === null) throw new Error("serialise hop cannot take a step");
  const { fresh, trial: chosen } = best;
  const runIndices = [
    ...(point.carried === null ? [] : [point.carried]),
    ...Array.from({ length: fresh }, (_, index) => point.absorbed + index),
  ];
  const { refs, order, referenceInputs } = hopRuns(loop, runIndices);
  const { entries: restore, below } = takeMidgardCekSerialiseEntries(
    point.stack,
    chosen.restore,
  );
  const hop: MidgardCekSerialiseHop = {
    refs,
    order,
    restore,
    base: midgardCekSerialiseStackRoot(below),
  };
  const from =
    point.cursor === 0 ? null : { cursor: point.cursor, state: point.state };
  const end = point.cursor + chosen.steps;
  const advanced = advanceMidgardCekSerialiseHop(
    loop.facts,
    hop,
    referenceInputs,
    from,
    end,
  );
  if (advanced.kind === "rejected")
    throw new Error(`serialise hop is rejected: ${advanced.reason}`);
  const cost = midgardCekSerialiseHopCost(chosen.shape);
  if (advanced.kind === "done")
    return {
      from,
      end,
      hop,
      referenceInputs,
      next: null,
      shape: chosen.shape,
      cost,
      point: null,
    };
  // The prover's whole stack after the hop: replay its steps.
  let stack = point.stack;
  let frame = point.state.frame;
  let at = 0;
  const window = Buffer.concat([
    ...(point.carried === null || point.state.carry === null
      ? []
      : [
          Buffer.concat(loop.runs[point.carried]!).subarray(
            point.state.carry.offset,
          ),
        ]),
    ...loop.runs.slice(point.absorbed, point.absorbed + fresh).flat(),
  ]);
  for (let taken = 0; taken < chosen.steps; taken += 1) {
    const { progress } = stepMidgardCekSerialise(
      window,
      at,
      frame,
      stack,
      window.length,
    );
    if (progress.kind !== "advanced")
      throw new Error("serialise replay diverged");
    ({ at, frame, stack } = progress);
  }
  const carry = advanced.state.carry;
  return {
    from,
    end,
    hop,
    referenceInputs,
    next: advanced.state,
    shape: chosen.shape,
    cost,
    point: {
      cursor: end,
      state: advanced.state,
      stack,
      absorbed: point.absorbed + fresh,
      carried:
        carry === null
          ? null
          : loop.refs.findIndex(
              (ref) => compareMidgardCekStagedOutRefs(ref, carry.run) === 0,
            ),
    },
  };
};

/**
 * The step limit of the hop that replaces one of `steps` steps that went
 * over budget: half of them, and at least one.
 */
export const halveMidgardCekSerialiseHop = (steps: number): number => {
  if (steps < 2) throw new RangeError("a one-step hop cannot be halved");
  return Math.floor(steps / 2);
};

/** Every hop of `loop`, from the start. */
export const planMidgardCekSerialiseHops = (
  loop: MidgardCekSerialiseLoop,
  budget: MidgardCekExUnits = MIDGARD_CEK_LOOP_HOP_BUDGET,
): MidgardCekSerialisePlannedHop[] => {
  const hops: MidgardCekSerialisePlannedHop[] = [];
  let point: MidgardCekSerialisePoint | null = MIDGARD_CEK_SERIALISE_START;
  while (point !== null) {
    const planned = planMidgardCekSerialiseHop(loop, point, { budget });
    hops.push(planned);
    point = planned.point;
  }
  return hops;
};
