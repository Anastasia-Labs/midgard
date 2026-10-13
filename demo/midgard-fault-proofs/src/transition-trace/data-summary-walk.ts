/**
 * The off-chain twin of `midgard/data_summary_walk_v1`: the linear,
 * resumable summariser the transition-trace phase-8 yields run over an
 * output's inline datum bytes.
 *
 * `traceDataSummaryWalk` takes every step the on-chain walk takes, in the
 * same order and with the same stack, so a prover can cut the walk into
 * transactions anywhere. `planDataSummaryWalkTransaction` picks how many
 * steps one transaction takes and the witness it needs: the stack entries
 * those steps pop below their own pushes, and the chain root under them.
 */
import {
  commitMidgardCekBlob,
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  hashMidgardCekDataNode,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekLargeConstrData,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { Constr, Data } from "@lucid-evolution/lucid";

export type DataSummaryWalkFrame =
  | { readonly kind: "root" }
  | { readonly kind: "list" }
  | { readonly kind: "constr"; readonly constructor: bigint }
  | {
      readonly kind: "largeConstr";
      readonly indexRoot: Uint8Array;
      readonly indexCborLength: bigint;
      readonly indexMemory: bigint;
    }
  | { readonly kind: "map"; readonly remaining: bigint }
  | {
      readonly kind: "folding";
      readonly open: DataSummaryWalkFrame;
      readonly folded: MidgardCekDataSequenceSummary;
    };

export type DataSummaryWalkEntry =
  | { readonly kind: "item"; readonly summary: MidgardCekDataSummary }
  | { readonly kind: "parent"; readonly frame: DataSummaryWalkFrame };

/**
 * A stack as an immutable list, top first. Each node caches the hash chain of
 * itself and everything below it, so a checkpoint's root costs one hash per
 * push.
 */
export type DataSummaryWalkStack = {
  readonly entry: DataSummaryWalkEntry;
  readonly below: DataSummaryWalkStack | null;
  readonly depth: number;
  readonly chain: Buffer;
} | null;

type Progress =
  | {
      readonly kind: "advanced";
      readonly at: number;
      readonly frame: DataSummaryWalkFrame;
      readonly stack: DataSummaryWalkStack;
    }
  | { readonly kind: "finished"; readonly summary: MidgardCekDataSummary }
  | { readonly kind: "refused" };

/** What one on-chain step did, for the transaction cost model. */
export type DataSummaryWalkStepKind =
  | "integer"
  | "bytes"
  | "open"
  | "close"
  | "pop"
  | "popPair"
  | "finish";

export type DataSummaryWalkStep = {
  readonly kind: DataSummaryWalkStepKind;
  /** Content bytes a leaf step hashed (byte string, bignum or index). */
  readonly hashedBytes: number;
};

/** The checkpoint between two steps. */
export type DataSummaryWalkPoint = {
  readonly at: number;
  readonly frame: DataSummaryWalkFrame;
  readonly stack: DataSummaryWalkStack;
};

export type DataSummaryWalkTrace = {
  /** `points[i]` is the checkpoint after `i` steps. */
  readonly points: readonly DataSummaryWalkPoint[];
  /** `steps[i]` leads from `points[i]` to `points[i + 1]` (or the end). */
  readonly steps: readonly DataSummaryWalkStep[];
  readonly outcome:
    | { readonly kind: "summarised"; readonly summary: MidgardCekDataSummary }
    | { readonly kind: "unreadable" };
};

const EMPTY_STACK_ROOT = Buffer.alloc(0);

// ## Data encodings (the on-chain types' constructor order)

const sequenceData = (sequence: MidgardCekDataSequenceSummary): Data =>
  new Constr(0, [
    Buffer.from(sequence.root).toString("hex"),
    sequence.length,
    sequence.payloadCborLength,
    sequence.memory,
  ]);

const summaryData = (summary: MidgardCekDataSummary): Data =>
  new Constr(0, [
    Buffer.from(summary.root).toString("hex"),
    summary.cborLength,
    summary.memory,
  ]);

export const dataSummaryWalkFrameData = (frame: DataSummaryWalkFrame): Data => {
  switch (frame.kind) {
    case "root":
      return new Constr(0, []);
    case "list":
      return new Constr(1, []);
    case "constr":
      return new Constr(2, [frame.constructor]);
    case "largeConstr":
      return new Constr(3, [
        Buffer.from(frame.indexRoot).toString("hex"),
        frame.indexCborLength,
        frame.indexMemory,
      ]);
    case "map":
      return new Constr(4, [frame.remaining]);
    case "folding":
      return new Constr(5, [
        dataSummaryWalkFrameData(frame.open),
        sequenceData(frame.folded),
      ]);
  }
};

export const dataSummaryWalkEntryData = (entry: DataSummaryWalkEntry): Data =>
  entry.kind === "item"
    ? new Constr(0, [summaryData(entry.summary)])
    : new Constr(1, [dataSummaryWalkFrameData(entry.frame)]);

const push = (
  entry: DataSummaryWalkEntry,
  below: DataSummaryWalkStack,
): DataSummaryWalkStack => ({
  entry,
  below,
  depth: (below?.depth ?? 0) + 1,
  chain: computeHash32(
    Buffer.concat([
      Buffer.from(Data.to(dataSummaryWalkEntryData(entry)), "hex"),
      below?.chain ?? EMPTY_STACK_ROOT,
    ]),
  ),
});

/** The on-chain `Walk { offset, frame, stack_root }` of a checkpoint. */
export const dataSummaryWalkData = (point: DataSummaryWalkPoint): Data =>
  new Constr(0, [
    BigInt(point.at),
    dataSummaryWalkFrameData(point.frame),
    (point.stack?.chain ?? EMPTY_STACK_ROOT).toString("hex"),
  ]);

// ## Reading

const byteAt = (bytes: Uint8Array, at: number, end: number) =>
  at < end ? bytes[at]! : -1;

const bigEndian = (bytes: Uint8Array) =>
  bytes.reduce((value, byte) => (value << 8n) | BigInt(byte), 0n);

/** A minimal head: [major, argument (-1 when indefinite), next]. */
const head = (
  bytes: Uint8Array,
  at: number,
  end: number,
): readonly [number, bigint, number] | null => {
  const first = byteAt(bytes, at, end);
  if (first < 0) return null;
  const major = first >> 5;
  const info = first & 31;
  if (info < 24) return [major, BigInt(info), at + 1];
  if (info <= 27) {
    const size = [1, 2, 4, 8][info - 24]!;
    if (at + 1 + size > end) return null;
    const value = bigEndian(bytes.subarray(at + 1, at + 1 + size));
    const minimal = value.toString(16).length;
    if (value >= 24n && 2 * Math.ceil(minimal / 2) > size)
      return [major, value, at + 1 + size];
    return null;
  }
  return info === 31 ? [major, -1n, at + 1] : null;
};

const chunks = (
  bytes: Uint8Array,
  at: number,
  end: number,
  content: Buffer,
): readonly [Buffer, number] | null => {
  const read = head(bytes, at, end);
  if (read === null || read[0] !== 2) return null;
  const length = Number(read[1]);
  const next = read[2];
  if (!(length >= 1 && length <= 64 && next + length <= end)) return null;
  const joined = Buffer.concat([content, bytes.subarray(next, next + length)]);
  const after = next + length;
  if (byteAt(bytes, after, end) === 255) return [joined, after + 1];
  return length === 64 ? chunks(bytes, after, end, joined) : null;
};

const byteString = (
  bytes: Uint8Array,
  at: number,
  end: number,
): readonly [Buffer, number] | null => {
  const read = head(bytes, at, end);
  if (read === null || read[0] !== 2) return null;
  const [, length, next] = read;
  if (length === -1n) {
    const joined = chunks(bytes, next, end, Buffer.alloc(0));
    return joined !== null && joined[0].length > 64 ? joined : null;
  }
  if (length <= 64n && next + Number(length) <= end)
    return [
      Buffer.from(bytes.subarray(next, next + Number(length))),
      next + Number(length),
    ];
  return null;
};

const unsignedByteSize = (value: bigint): bigint =>
  value < 256n ? 1n : 1n + unsignedByteSize(value / 256n);

const integerMemory = (argument: bigint) =>
  4n + unsignedByteSize(argument * 2n);

const integerLeaf = (
  cbor: Uint8Array,
  memory: bigint,
): MidgardCekDataSummary => {
  const cborLength = BigInt(cbor.length);
  return {
    root: hashMidgardCekDataNode({
      kind: "integer",
      cborRoot: commitMidgardCekBlob(cbor).root,
      cborLength,
      memory,
    }),
    cborLength,
    memory,
  };
};

const bytesLeaf = (
  content: Uint8Array,
  cborLength: number,
): MidgardCekDataSummary => {
  const bytesLength = BigInt(content.length);
  const memory = 4n + (bytesLength === 0n ? 1n : bytesLength);
  return {
    root: hashMidgardCekDataNode({
      kind: "bytes",
      bytesRoot: commitMidgardCekBlob(content).root,
      bytesLength,
      cborLength: BigInt(cborLength),
      memory,
    }),
    cborLength: BigInt(cborLength),
    memory,
  };
};

const finish = (
  open: DataSummaryWalkFrame,
  folded: MidgardCekDataSequenceSummary,
): MidgardCekDataSummary => {
  switch (open.kind) {
    case "list":
      return summarizeMidgardCekListData(folded);
    case "constr":
      return summarizeMidgardCekSmallConstrData(open.constructor, folded);
    case "largeConstr":
      return summarizeMidgardCekLargeConstrData({
        constructorCborRoot: open.indexRoot,
        constructorCborLength: open.indexCborLength,
        constructorMemory: open.indexMemory,
        fields: folded,
      });
    case "map":
      return summarizeMidgardCekMapData(folded);
    default:
      throw new Error(`data summary walk cannot finish a ${open.kind} frame`);
  }
};

type Stepped = {
  readonly progress: Progress;
  readonly step: DataSummaryWalkStep;
};

const refused: Stepped = {
  progress: { kind: "refused" },
  step: { kind: "close", hashedBytes: 0 },
};

const complete = (
  summary: MidgardCekDataSummary,
  after: number,
  frame: DataSummaryWalkFrame,
  stack: DataSummaryWalkStack,
  end: number,
): Progress => {
  if (frame.kind === "root")
    return after === end ? { kind: "finished", summary } : { kind: "refused" };
  const pushed = push({ kind: "item", summary }, stack);
  if (frame.kind === "map")
    return {
      kind: "advanced",
      at: after,
      frame:
        frame.remaining === 1n
          ? {
              kind: "folding",
              open: { kind: "map", remaining: 0n },
              folded: emptyMidgardCekDataPairSummary(),
            }
          : { kind: "map", remaining: frame.remaining - 1n },
      stack: pushed,
    };
  return { kind: "advanced", at: after, frame, stack: pushed };
};

const fold = (
  open: DataSummaryWalkFrame,
  folded: MidgardCekDataSequenceSummary,
  at: number,
  stack: DataSummaryWalkStack,
  end: number,
): Stepped => {
  if (stack === null)
    throw new Error("data summary walk folded past the bottom of its stack");
  const { entry, below } = stack;
  if (entry.kind === "parent")
    return folded.length === 0n
      ? { ...refused, step: { kind: "finish", hashedBytes: 0 } }
      : {
          progress: complete(finish(open, folded), at, entry.frame, below, end),
          step: { kind: "finish", hashedBytes: 0 },
        };
  if (open.kind === "map") {
    if (below === null || below.entry.kind !== "item")
      throw new Error("data summary walk map entry has no key");
    return {
      progress: {
        kind: "advanced",
        at,
        frame: {
          kind: "folding",
          open,
          folded: prependMidgardCekDataPairSummary(
            below.entry.summary,
            entry.summary,
            folded,
          ),
        },
        stack: below.below,
      },
      step: { kind: "popPair", hashedBytes: 0 },
    };
  }
  return {
    progress: {
      kind: "advanced",
      at,
      frame: {
        kind: "folding",
        open,
        folded: prependMidgardCekDataListSummary(entry.summary, folded),
      },
      stack: below,
    },
    step: { kind: "pop", hashedBytes: 0 },
  };
};

const close = (
  frame: DataSummaryWalkFrame,
  stack: DataSummaryWalkStack,
  after: number,
): Stepped =>
  frame.kind === "list" ||
  frame.kind === "constr" ||
  frame.kind === "largeConstr"
    ? {
        progress: {
          kind: "advanced",
          at: after,
          frame: {
            kind: "folding",
            open: frame,
            folded: emptyMidgardCekDataListSummary(),
          },
          stack,
        },
        step: { kind: "close", hashedBytes: 0 },
      }
    : refused;

const fields = (
  bytes: Uint8Array,
  at: number,
  opened: DataSummaryWalkFrame,
  frame: DataSummaryWalkFrame,
  stack: DataSummaryWalkStack,
  end: number,
  hashedBytes: number,
): Stepped => {
  const first = byteAt(bytes, at, end);
  if (first === 128)
    return {
      progress: complete(
        finish(opened, emptyMidgardCekDataListSummary()),
        at + 1,
        frame,
        stack,
        end,
      ),
      step: { kind: "finish", hashedBytes },
    };
  if (first === 159)
    return {
      progress: {
        kind: "advanced",
        at: at + 1,
        frame: opened,
        stack: push({ kind: "parent", frame }, stack),
      },
      step: { kind: "open", hashedBytes },
    };
  return refused;
};

const tagged = (
  bytes: Uint8Array,
  at: number,
  next: number,
  tag: bigint,
  frame: DataSummaryWalkFrame,
  stack: DataSummaryWalkStack,
  end: number,
): Stepped => {
  if ((tag >= 121n && tag <= 127n) || (tag >= 1280n && tag <= 1400n))
    return fields(
      bytes,
      next,
      { kind: "constr", constructor: tag <= 127n ? tag - 121n : tag - 1273n },
      frame,
      stack,
      end,
      0,
    );
  if (tag === 102n) {
    if (byteAt(bytes, next, end) !== 130) return refused;
    const index = head(bytes, next + 1, end);
    if (index === null || index[0] !== 0 || index[1] < 128n) return refused;
    const after = index[2];
    const indexCbor = bytes.subarray(next + 1, after);
    return fields(
      bytes,
      after,
      {
        kind: "largeConstr",
        indexRoot: commitMidgardCekBlob(indexCbor).root,
        indexCborLength: BigInt(after - next - 1),
        indexMemory: integerMemory(index[1]),
      },
      frame,
      stack,
      end,
      indexCbor.length,
    );
  }
  if (tag === 2n || tag === 3n) {
    const read = byteString(bytes, next, end);
    if (read === null) return refused;
    const [magnitude, after] = read;
    if (magnitude.length < 9 || magnitude[0] === 0) return refused;
    return {
      progress: complete(
        integerLeaf(
          bytes.subarray(at, after),
          4n + BigInt(magnitude.length) + (magnitude[0]! >= 128 ? 1n : 0n),
        ),
        after,
        frame,
        stack,
        end,
      ),
      step: { kind: "integer", hashedBytes: after - at },
    };
  }
  return refused;
};

const read = (
  bytes: Uint8Array,
  at: number,
  frame: DataSummaryWalkFrame,
  stack: DataSummaryWalkStack,
  end: number,
): Stepped => {
  if (byteAt(bytes, at, end) === 255) return close(frame, stack, at + 1);
  const parsed = head(bytes, at, end);
  if (parsed === null) return refused;
  const [major, value, next] = parsed;
  if (major <= 1) {
    if (value < 0n) return refused;
    return {
      progress: complete(
        integerLeaf(bytes.subarray(at, next), integerMemory(value)),
        next,
        frame,
        stack,
        end,
      ),
      step: { kind: "integer", hashedBytes: next - at },
    };
  }
  if (major === 2) {
    const string = byteString(bytes, at, end);
    if (string === null) return refused;
    const [content, after] = string;
    return {
      progress: complete(
        bytesLeaf(content, after - at),
        after,
        frame,
        stack,
        end,
      ),
      step: { kind: "bytes", hashedBytes: content.length },
    };
  }
  if (major === 4)
    return fields(bytes, at, { kind: "list" }, frame, stack, end, 0);
  if (major === 5) {
    if (value === 0n)
      return {
        progress: complete(
          summarizeMidgardCekMapData(emptyMidgardCekDataPairSummary()),
          next,
          frame,
          stack,
          end,
        ),
        step: { kind: "finish", hashedBytes: 0 },
      };
    if (value > 0n && 2n * value <= BigInt(end - next))
      return {
        progress: {
          kind: "advanced",
          at: next,
          frame: { kind: "map", remaining: 2n * value },
          stack: push({ kind: "parent", frame }, stack),
        },
        step: { kind: "open", hashedBytes: 0 },
      };
    return refused;
  }
  if (major === 6) return tagged(bytes, at, next, value, frame, stack, end);
  return refused;
};

const step = (
  bytes: Uint8Array,
  point: DataSummaryWalkPoint,
  end: number,
): Stepped =>
  point.frame.kind === "folding"
    ? fold(point.frame.open, point.frame.folded, point.at, point.stack, end)
    : read(bytes, point.at, point.frame, point.stack, end);

/**
 * Every step of the walk over `bytes[start, end)`, from a fresh checkpoint to
 * the summary or the refusal.
 */
export const traceDataSummaryWalk = (
  bytes: Uint8Array,
  start: number,
  end: number,
): DataSummaryWalkTrace => {
  const points: DataSummaryWalkPoint[] = [
    { at: start, frame: { kind: "root" }, stack: null },
  ];
  const steps: DataSummaryWalkStep[] = [];
  for (;;) {
    const { progress, step: taken } = step(bytes, points.at(-1)!, end);
    steps.push(taken);
    if (progress.kind === "finished")
      return {
        points,
        steps,
        outcome: { kind: "summarised", summary: progress.summary },
      };
    if (progress.kind === "refused")
      return { points, steps, outcome: { kind: "unreadable" } };
    points.push(progress);
  }
};

// ## Transactions

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

/** The on-chain `WalkWitness { restore, base, steps }`. */
export const dataSummaryWalkWitnessData = (
  transaction: Pick<DataSummaryWalkTransaction, "restore" | "base" | "steps">,
): Data =>
  new Constr(0, [
    transaction.restore.map(dataSummaryWalkEntryData),
    transaction.base.toString("hex"),
    BigInt(transaction.steps),
  ]);
