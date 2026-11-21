/**
 * Cutting a traced data summary walk into transactions: how many steps one
 * transaction takes under the cost model, and the witness it needs.
 */
import { Data } from "@lucid-evolution/lucid";

import {
  dataSummaryWalkEntryData,
  EMPTY_STACK_ROOT,
} from "./data-summary-walk.data.js";
import type {
  DataSummaryWalkEntry,
  DataSummaryWalkStack,
  DataSummaryWalkStep,
  DataSummaryWalkStepKind,
  DataSummaryWalkTrace,
} from "./data-summary-walk.types.js";

/**
 * A transaction's budget for its walk steps. The step model below rounds
 * every calibrated cost up, and the rest of a phase-8 transaction (its
 * dispatcher and, when the walk ends, the output summaries) measured at most
 * about 3.9M memory and 1.7B cpu in the emulator, so a transaction planned to
 * this budget stays inside 13.2M memory and 8B cpu.
 */
export type DataSummaryWalkBudget = {
  readonly memory: number;
  readonly cpu: number;
  /** Encoded bytes of the restored entries the redeemer carries. */
  readonly restoreBytes: number;
};

export const DATA_SUMMARY_WALK_TRANSACTION_BUDGET: DataSummaryWalkBudget = {
  memory: 9_200_000,
  cpu: 6_000_000_000,
  restoreBytes: 6_000,
};

/**
 * The measured on-chain cost of each step kind (memory, cpu), rounded up;
 * a leaf adds a cost per byte it hashes (a byte string's content, an
 * integer's CBOR), and a bignum adds its tagged byte string's decoding.
 */
const STEP_COST: Readonly<
  Record<DataSummaryWalkStepKind, readonly [number, number]>
> = {
  integer: [65_000, 26_000_000],
  bytes: [160_000, 60_000_000],
  open: [50_000, 16_000_000],
  close: [25_000, 12_000_000],
  pop: [65_000, 32_000_000],
  popPair: [95_000, 47_000_000],
  finish: [85_000, 36_000_000],
};
const HASHED_BYTE_COST = [150, 40_000] as const;
/** An integer whose CBOR exceeds nine bytes is a tagged bignum. */
const BIGNUM_COST = [100_000, 35_000_000] as const;
/** A large constructor's index is hashed when its fields open. */
const LARGE_INDEX_COST = [130_000, 55_000_000] as const;
/** Decoding a restored entry and hashing it into the chain. */
const RESTORED_ENTRY_COST = [48_000, 22_000_000] as const;
/** Hashing an entry this transaction pushed into its chain. */
const PUSHED_ENTRY_COST = [14_000, 12_000_000] as const;

const stepCost = (step: DataSummaryWalkStep): readonly [number, number] => {
  const [memory, cpu] = STEP_COST[step.kind];
  if (step.kind === "open" && step.hashedBytes > 0)
    return [memory + LARGE_INDEX_COST[0], cpu + LARGE_INDEX_COST[1]];
  const [bignumMemory, bignumCpu] =
    step.kind === "integer" && step.hashedBytes > 9 ? BIGNUM_COST : [0, 0];
  return [
    memory + bignumMemory + step.hashedBytes * HASHED_BYTE_COST[0],
    cpu + bignumCpu + step.hashedBytes * HASHED_BYTE_COST[1],
  ];
};

export type DataSummaryWalkTransaction = {
  readonly from: number;
  /** Steps the transaction takes; `from + steps` is its next checkpoint. */
  readonly steps: number;
  readonly restore: readonly DataSummaryWalkEntry[];
  readonly base: Buffer;
  /** Whether the transaction ends the walk. */
  readonly final: boolean;
  /** The modelled cost of its steps, restores and pushes. */
  readonly estimate: { readonly memory: number; readonly cpu: number };
};

const nodes = (stack: DataSummaryWalkStack) => {
  const byDepth: NonNullable<DataSummaryWalkStack>[] = [];
  for (let node = stack; node !== null; node = node.below)
    byDepth[node.depth - 1] = node;
  return byDepth;
};

/**
 * How many entries of `start` survive under `stack`: the depth of the
 * deepest node the two share.
 */
const shared = (
  startNodes: readonly NonNullable<DataSummaryWalkStack>[],
  stack: DataSummaryWalkStack,
) => {
  let node = stack;
  while (node !== null && startNodes[node.depth - 1] !== node)
    node = node.below;
  return node?.depth ?? 0;
};

/**
 * One transaction of the walk from checkpoint `from`: as many steps as the
 * memory model allows (at least one), the entries they pop below their own
 * pushes, and the chain root under those.
 */
export const planDataSummaryWalkTransaction = (
  trace: DataSummaryWalkTrace,
  from: number,
  budget = DATA_SUMMARY_WALK_TRANSACTION_BUDGET,
): DataSummaryWalkTransaction => {
  const startPoint = trace.points[from];
  if (startPoint === undefined)
    throw new Error("data summary walk checkpoint is past its end");
  const startNodes = nodes(startPoint.stack);
  const startDepth = startPoint.stack?.depth ?? 0;
  const entryBytes = (depth: number) =>
    Data.to(dataSummaryWalkEntryData(startNodes[depth - 1]!.entry)).length / 2;
  let kept = startDepth;
  let restoreBytes = 0;
  let memory = 0;
  let cpu = 0;
  let steps = 0;
  let estimate = { memory: 0, cpu: 0 };
  for (let index = from; index < trace.steps.length; index++) {
    const [stepMemory, stepCpu] = stepCost(trace.steps[index]!);
    // The checkpoint after this step, or none when it ends the walk, which
    // pops everything left down to the root.
    const after = trace.points[index + 1];
    const keptAfter =
      after === undefined ? 0 : Math.min(kept, shared(startNodes, after.stack));
    let restoredBytes = restoreBytes;
    for (let depth = kept; depth > keptAfter; depth--)
      restoredBytes += entryBytes(depth);
    const pushed =
      after === undefined ? 0 : (after.stack?.depth ?? 0) - keptAfter;
    const restored = startDepth - keptAfter;
    const totalMemory =
      memory +
      stepMemory +
      restored * RESTORED_ENTRY_COST[0] +
      pushed * PUSHED_ENTRY_COST[0];
    const totalCpu =
      cpu +
      stepCpu +
      restored * RESTORED_ENTRY_COST[1] +
      pushed * PUSHED_ENTRY_COST[1];
    if (
      steps > 0 &&
      (totalMemory > budget.memory ||
        totalCpu > budget.cpu ||
        restoredBytes > budget.restoreBytes)
    )
      break;
    estimate = { memory: totalMemory, cpu: totalCpu };
    memory += stepMemory;
    cpu += stepCpu;
    kept = keptAfter;
    restoreBytes = restoredBytes;
    steps += 1;
  }
  const restore: DataSummaryWalkEntry[] = [];
  for (
    let node = startPoint.stack;
    node !== null && node.depth > kept;
    node = node.below
  )
    restore.push(node.entry);
  return {
    from,
    steps,
    restore,
    base: startNodes[kept - 1]?.chain ?? EMPTY_STACK_ROOT,
    final: from + steps === trace.steps.length,
    estimate,
  };
};
