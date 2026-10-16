import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
  type MidgardValidationMachineState,
  type MidgardValidationTraceDescriptor,
  type MidgardValidationTraceProof,
  validationTraceDepthForStepCount,
} from "@al-ft/midgard-core";
import { hashHexWithBlake2b } from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  type GameTraces,
  machineState,
  type TraceSide,
} from "./validation-game-traces.js";

/**
 * A trace too long to list state by state: a few runs of one repeated state.
 * Run `k` holds `runs[k].state` from index `runs[k].from` up to the next run;
 * the last run reaches the step count, and the tree pads with that last state
 * as an honest trace does.
 */
export type TraceRun = {
  readonly from: number;
  readonly state: MidgardValidationMachineState;
};

const blake2b256 = (bytes: Buffer): Buffer =>
  Buffer.from(
    Effect.runSync(hashHexWithBlake2b(bytes.toString("hex"), 32)),
    "hex",
  );

const LEAF_DOMAIN = Buffer.from("MidgardValidationTraceLeafV1", "utf8");
const BRANCH_DOMAIN = Buffer.from("MidgardValidationTraceBranchV1", "utf8");

const leafHash = (stateHash: Buffer) =>
  blake2b256(Buffer.concat([LEAF_DOMAIN, stateHash]));

const branchHash = (left: Buffer, right: Buffer) =>
  blake2b256(Buffer.concat([BRANCH_DOMAIN, left, right]));

export type SparseTrace = TraceSide & {
  readonly stateAt: (stateIndex: number) => MidgardValidationMachineState;
};

/**
 * The trace tree over `runs` with `stepCount` steps, computed node by node:
 * a subtree inside one run is a tower of one leaf, so only the nodes over a
 * run boundary need hashing, and a 2^32-leaf tree costs a few hundred hashes.
 */
export const sparseTrace = (
  runs: readonly TraceRun[],
  stepCount: number,
): SparseTrace => {
  if (runs.length === 0 || runs[0]!.from !== 0) {
    throw new Error("a trace starts with a run at index 0");
  }
  runs.forEach((run, index) => {
    if (index > 0 && run.from <= runs[index - 1]!.from) {
      throw new Error("runs must start at increasing indices");
    }
    if (run.from > stepCount) throw new Error("a run starts past the end");
  });
  const depth = validationTraceDepthForStepCount(stepCount);
  const leafCount = 2 ** depth;
  const hashes = runs.map((run) =>
    hashMidgardValidationMachineState(run.state),
  );
  /** Index of the run holding leaf `leaf` (padding belongs to the last run). */
  const runOf = (leaf: number) => {
    let found = 0;
    runs.forEach((run, index) => {
      if (run.from <= leaf) found = index;
    });
    return found;
  };
  const towers = hashes.map((hash) => {
    const tower = [leafHash(hash)];
    for (let level = 1; level <= depth; level += 1) {
      tower.push(branchHash(tower[level - 1]!, tower[level - 1]!));
    }
    return tower;
  });
  const memo = new Map<string, Buffer>();
  /** The node over leaves [index * 2^level, (index + 1) * 2^level). */
  const node = (level: number, index: number): Buffer => {
    const first = index * 2 ** level;
    const last = first + 2 ** level - 1;
    const run = runOf(first);
    if (run === runOf(last)) return towers[run]![level]!;
    const key = `${level}:${index}`;
    const cached = memo.get(key);
    if (cached !== undefined) return cached;
    const hash = branchHash(
      node(level - 1, 2 * index),
      node(level - 1, 2 * index + 1),
    );
    memo.set(key, hash);
    return hash;
  };
  const stateAt = (stateIndex: number) => {
    if (!Number.isSafeInteger(stateIndex) || stateIndex < 0) {
      throw new Error(`no state ${stateIndex}`);
    }
    if (stateIndex > stepCount) throw new Error(`no state ${stateIndex}`);
    return runs[runOf(stateIndex)]!.state;
  };
  const initial = runs[0]!.state;
  const terminal = runs[runs.length - 1]!.state;
  const descriptor: MidgardValidationTraceDescriptor = {
    ...buildMidgardValidationTraceTree(
      [
        hashMidgardValidationMachineState(initial),
        hashMidgardValidationMachineState(terminal),
      ],
      "accepted",
    ).descriptor,
    traceRoot: node(depth, 0),
    stepCount,
    initialStateHash: hashMidgardValidationMachineState(initial),
    terminalStateHash: hashMidgardValidationMachineState(terminal),
  };
  if (leafCount < stepCount + 1) throw new Error("the tree is too shallow");
  return {
    descriptor,
    stateAt,
    proofAt: (stateIndex): MidgardValidationTraceProof => {
      const stateHash = hashMidgardValidationMachineState(stateAt(stateIndex));
      const siblings: Buffer[] = [];
      let index = stateIndex;
      for (let level = 0; level < depth; level += 1) {
        siblings.push(node(level, index % 2 === 0 ? index + 1 : index - 1));
        index = Math.floor(index / 2);
      }
      return { stateIndex, stateHash, siblings };
    },
  };
};

/** Bisection rounds the game plays when the sides first differ after `agreedUpTo`. */
export const bisectionRounds = (stepCount: number, agreedUpTo: number) => {
  let low = 0;
  let high = stepCount;
  let rounds = 0;
  while (high > low + 1) {
    const midpoint = low + Math.floor((high - low) / 2);
    if (midpoint <= agreedUpTo) low = midpoint;
    else high = midpoint;
    rounds += 1;
  }
  return rounds;
};

/**
 * The last index the sides agree on: about two thirds of the way along, so
 * the bisection turns both ways, and placed so the game plays all `depth`
 * rounds.
 */
export const fullDepthBoundary = (depth: number) => {
  const stepCount = 2 ** depth - 1;
  for (let agreed = Math.floor((2 * stepCount) / 3); agreed > 0; agreed -= 1) {
    if (bisectionRounds(stepCount, agreed) === depth) return agreed;
  }
  throw new Error(`no boundary plays ${depth} rounds`);
};

export type DeepGameTraces = GameTraces & {
  readonly stepCount: number;
  readonly boundary: number;
  readonly operatorTrace: SparseTrace;
  readonly challengerTrace: SparseTrace;
};

const deepTraces = (
  depth: number,
  operatorAfter: MidgardValidationMachineState,
  challengerAfter: MidgardValidationMachineState,
  agreedLowState: MidgardValidationMachineState,
): DeepGameTraces => {
  const stepCount = 2 ** depth - 1;
  const boundary = fullDepthBoundary(depth);
  const shared: TraceRun[] = [
    { from: 0, state: machineState(0, "canonicalDecode") },
    { from: 1, state: machineState(1, "cek") },
    { from: boundary, state: agreedLowState },
  ];
  const operatorTrace = sparseTrace(
    [...shared, { from: boundary + 1, state: operatorAfter }],
    stepCount,
  );
  const challengerTrace = sparseTrace(
    [...shared, { from: boundary + 1, state: challengerAfter }],
    stepCount,
  );
  return {
    operator: operatorTrace,
    challenger: challengerTrace,
    agreedLowState,
    stepCount,
    boundary,
    operatorTrace,
    challengerTrace,
  };
};

/** The sides agree up to the boundary inside CEK and part right after it. */
export const deepDivergentTraces = (depth: number): DeepGameTraces =>
  deepTraces(
    depth,
    machineState(3, "cek"),
    machineState(4, "cek"),
    machineState(2, "cek"),
  );

/**
 * As `deepDivergentTraces`, with the challenger's trace ending at
 * `challengerEnd` instead.
 */
export const deepTracesEndingAt = (
  depth: number,
  challengerEnd: MidgardValidationMachineState,
): DeepGameTraces =>
  deepTraces(
    depth,
    machineState(3, "cek"),
    challengerEnd,
    machineState(2, "cek"),
  );
