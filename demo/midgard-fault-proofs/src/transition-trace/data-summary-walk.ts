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
  midgardCekBytesMemoryWords,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  midgardCekIntegerMemoryWords,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekLargeConstrData,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "@al-ft/midgard-core";

import { byteAt, byteString, head } from "./data-summary-walk.cbor.js";
import { pushDataSummaryWalkEntry as push } from "./data-summary-walk.data.js";
import type {
  DataSummaryWalkFrame,
  DataSummaryWalkPoint,
  DataSummaryWalkStack,
  DataSummaryWalkStep,
  DataSummaryWalkTrace,
} from "./data-summary-walk.types.js";

export {
  dataSummaryWalkData,
  dataSummaryWalkEntryData,
  dataSummaryWalkFrameData,
  dataSummaryWalkWitnessData,
} from "./data-summary-walk.data.js";
export {
  DATA_SUMMARY_WALK_TRANSACTION_BUDGET,
  type DataSummaryWalkBudget,
  type DataSummaryWalkTransaction,
  planDataSummaryWalkTransaction,
} from "./data-summary-walk.plan.js";
export type * from "./data-summary-walk.types.js";

type Progress =
  | {
      readonly kind: "advanced";
      readonly at: number;
      readonly frame: DataSummaryWalkFrame;
      readonly stack: DataSummaryWalkStack;
    }
  | { readonly kind: "finished"; readonly summary: MidgardCekDataSummary }
  | { readonly kind: "refused" };

// ## Reading

/** `4 +` the word size of a major 0/1 integer: `|n|` is the CBOR argument
 * for major 0 and one more for major 1. */
const integerMemory = (major: number, argument: bigint) =>
  4n + midgardCekIntegerMemoryWords(argument + BigInt(major));

/** `4 +` the word size of a tag 2/3 integer from its magnitude bytes (no
 * leading zero): tag 2 holds `|n|` and tag 3 holds `|n| - 1`. */
const magnitudeMemory = (tag: bigint, magnitude: Uint8Array) =>
  4n +
  (tag === 2n
    ? midgardCekBytesMemoryWords(magnitude.length)
    : midgardCekIntegerMemoryWords(
        BigInt(`0x${Buffer.from(magnitude).toString("hex")}`) + 1n,
      ));

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
  const memory = 4n + midgardCekBytesMemoryWords(bytesLength);
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
    case "root":
    case "folding":
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
        indexMemory: integerMemory(0, index[1]),
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
        integerLeaf(bytes.subarray(at, after), magnitudeMemory(tag, magnitude)),
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
        integerLeaf(bytes.subarray(at, next), integerMemory(major, value)),
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
