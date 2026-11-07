/**
 * The off-chain twin of `midgard/data_serialise_walk_v1`: the resumable reader
 * over the canonical CBOR of a Plutus Data value that the serialise loop runs
 * through a sequence of windows.
 *
 * Every step takes the decision the on-chain step takes, over the same
 * window, with the same frame and stack, so a prover can cut the reading into
 * hops anywhere the on-chain loop accepts a cut.
 */
export { MIDGARD_CEK_SERIALISE_CHUNK_BYTES } from "./cek-serialise-walk.bytes.js";
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  finalizeMidgardCekBlobFrontier,
  hashMidgardCekBlobChunk,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekMapData,
} from "@al-ft/midgard-core";

import {
  byteAt,
  bytesLeaf,
  contents,
  finish,
  flush,
  head,
  integerLeaf,
  integerMemory,
  isAllOnes,
  type LeafState,
  magnitudeMemory,
  openLeaf,
  reach,
  shortBytes,
} from "./cek-serialise-walk.bytes.js";
import { pushMidgardCekSerialiseEntry as push } from "./cek-serialise-walk.data.js";
import type {
  MidgardCekSerialiseFrame,
  MidgardCekSerialiseProgress,
  MidgardCekSerialiseStack,
  MidgardCekSerialiseStep,
} from "./cek-serialise-walk.types.js";

type Progress = MidgardCekSerialiseProgress;
type Frame = MidgardCekSerialiseFrame;
type Stack = MidgardCekSerialiseStack;
type Stepped = {
  readonly progress: Progress;
  readonly step: MidgardCekSerialiseStep;
};

const REFUSED: Progress = { kind: "refused" };
const refusedStep: Stepped = {
  progress: REFUSED,
  step: { kind: "close", read: 0, chunks: 0 },
};

// ## Steps

const advanced = (at: number, frame: Frame, stack: Stack): Progress => ({
  kind: "advanced",
  at,
  frame,
  stack,
});

const complete = (
  summary: MidgardCekDataSummary,
  after: number,
  frame: Frame,
  stack: Stack,
  end: number,
): Progress => {
  if (frame.kind === "root")
    return after === end ? { kind: "finished", summary } : REFUSED;
  const pushed = push({ kind: "item", summary }, stack);
  if (frame.kind === "map")
    return advanced(
      after,
      frame.remaining === 1n
        ? {
            kind: "folding",
            open: { kind: "map", remaining: 0n },
            folded: emptyMidgardCekDataPairSummary(),
          }
        : { kind: "map", remaining: frame.remaining - 1n },
      pushed,
    );
  return advanced(after, frame, pushed);
};

/** Thrown where the on-chain step fails for want of restored stack entries. */
export class MidgardCekSerialiseStackShortError extends Error {
  constructor() {
    super("restore more of the stack");
  }
}

const fold = (
  open: Frame,
  folded: MidgardCekDataSequenceSummary,
  at: number,
  stack: Stack,
  end: number,
): Stepped => {
  if (stack === null) throw new MidgardCekSerialiseStackShortError();
  const { entry, below } = stack;
  if (entry.kind === "parent")
    return {
      progress:
        folded.length === 0n
          ? REFUSED
          : complete(finish(open, folded), at, entry.frame, below, end),
      step: { kind: "finish", read: 0, chunks: 0 },
    };
  if (open.kind === "map") {
    if (below === null || below.entry.kind !== "item")
      throw new MidgardCekSerialiseStackShortError();
    return {
      progress: advanced(
        at,
        {
          kind: "folding",
          open,
          folded: prependMidgardCekDataPairSummary(
            below.entry.summary,
            entry.summary,
            folded,
          ),
        },
        below.below,
      ),
      step: { kind: "popPair", read: 0, chunks: 0 },
    };
  }
  return {
    progress: advanced(
      at,
      {
        kind: "folding",
        open,
        folded: prependMidgardCekDataListSummary(entry.summary, folded),
      },
      below,
    ),
    step: { kind: "pop", read: 0, chunks: 0 },
  };
};

const close = (frame: Frame, stack: Stack, after: number): Stepped =>
  frame.kind === "list" ||
  frame.kind === "constr" ||
  frame.kind === "largeConstr"
    ? {
        progress: advanced(
          after,
          {
            kind: "folding",
            open: frame,
            folded: emptyMidgardCekDataListSummary(),
          },
          stack,
        ),
        step: { kind: "close", read: 1, chunks: 0 },
      }
    : refusedStep;

const fields = (
  bytes: Uint8Array,
  start: number,
  at: number,
  opened: Frame,
  frame: Frame,
  stack: Stack,
  end: number,
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
      step: { kind: "finish", read: at + 1 - start, chunks: 0 },
    };
  if (first === 159)
    return {
      progress: advanced(
        at + 1,
        opened,
        push({ kind: "parent", frame }, stack),
      ),
      step: { kind: "open", read: at + 1 - start, chunks: 0 },
    };
  return refusedStep;
};

const stream = (
  bytes: Uint8Array,
  at: number,
  state: LeafState,
  stack: Stack,
  end: number,
): Stepped => {
  const { parent, tag } = state;
  const reached = reach(bytes, at, end, tag, state.pending.length);
  if (reached === null || reached.after === at) return refusedStep;
  const { after, got, done } = reached;
  const pieces =
    tag === 0 || state.allFf || state.length === 0n
      ? contents(bytes, at, after)
      : [];
  const firstOk =
    state.length > 0n ||
    tag === 0 ||
    (pieces.length > 0 && pieces[0]![0] !== 0);
  const allFf = state.allFf && pieces.every(isAllOnes);
  const added =
    tag === 0
      ? Buffer.concat([state.pending, ...pieces])
      : Buffer.concat([state.pending, bytes.subarray(at, after)]);
  const flushed = flush(state.leaf, added, done);
  const length = state.length + BigInt(got);
  const cborLength = state.cborLength + BigInt(after - at);
  const step: MidgardCekSerialiseStep = {
    kind: "leaf",
    read: after - at,
    chunks: flushed.chunks,
  };
  if (!firstOk) return { progress: REFUSED, step };
  if (!done)
    return {
      progress: advanced(
        after,
        {
          kind: "leaf",
          parent,
          tag,
          leaf: flushed.leaf,
          pending: flushed.pending,
          length,
          cborLength,
          allFf,
        },
        stack,
      ),
      step,
    };
  if (length <= 64n) return { progress: REFUSED, step };
  const root = finalizeMidgardCekBlobFrontier(flushed.leaf)!;
  const summary =
    tag === 0
      ? bytesLeaf(root, length, cborLength)
      : integerLeaf(root, cborLength, magnitudeMemory(tag, length, allFf));
  return { progress: complete(summary, after, parent, stack, end), step };
};

const tagged = (
  bytes: Uint8Array,
  at: number,
  next: number,
  tag: bigint,
  frame: Frame,
  stack: Stack,
  end: number,
): Stepped => {
  if ((tag >= 121n && tag <= 127n) || (tag >= 1280n && tag <= 1400n))
    return fields(
      bytes,
      at,
      next,
      { kind: "constr", constructor: tag <= 127n ? tag - 121n : tag - 1273n },
      frame,
      stack,
      end,
    );
  if (tag === 102n) {
    if (byteAt(bytes, next, end) !== 130) return refusedStep;
    const index = head(bytes, next + 1, end);
    if (index === null || index[0] !== 0 || index[1] < 128n) return refusedStep;
    const after = index[2];
    return fields(
      bytes,
      at,
      after,
      {
        kind: "largeConstr",
        indexRoot: hashMidgardCekBlobChunk(bytes.subarray(next + 1, after)),
        indexCborLength: BigInt(after - next - 1),
        indexMemory: integerMemory(0, index[1]),
      },
      frame,
      stack,
      end,
    );
  }
  if (tag === 2n || tag === 3n) {
    const small = Number(tag) as 2 | 3;
    if (byteAt(bytes, next, end) === 0x5f) {
      const opened = openLeaf(
        frame,
        small,
        Buffer.from(bytes.subarray(at, next + 1)),
        BigInt(next + 1 - at),
        small === 3,
      );
      const streamed = stream(bytes, next + 1, opened, stack, end);
      return {
        progress: streamed.progress,
        step: { ...streamed.step, read: streamed.step.read + next + 1 - at },
      };
    }
    const read = shortBytes(bytes, next, end);
    if (read === null) return refusedStep;
    const [magnitude, after] = read;
    if (magnitude.length < 9 || magnitude[0] === 0) return refusedStep;
    return {
      progress: complete(
        integerLeaf(
          hashMidgardCekBlobChunk(bytes.subarray(at, after)),
          BigInt(after - at),
          magnitudeMemory(
            small,
            BigInt(magnitude.length),
            isAllOnes(magnitude),
          ),
        ),
        after,
        frame,
        stack,
        end,
      ),
      step: { kind: "integer", read: after - at, chunks: 0 },
    };
  }
  return refusedStep;
};

const read = (
  bytes: Uint8Array,
  at: number,
  frame: Frame,
  stack: Stack,
  end: number,
): Stepped => {
  if (byteAt(bytes, at, end) === 255) return close(frame, stack, at + 1);
  const parsed = head(bytes, at, end);
  if (parsed === null) return refusedStep;
  const [major, value, next] = parsed;
  if (major <= 1) {
    if (value < 0n) return refusedStep;
    return {
      progress: complete(
        integerLeaf(
          hashMidgardCekBlobChunk(bytes.subarray(at, next)),
          BigInt(next - at),
          integerMemory(major, value),
        ),
        next,
        frame,
        stack,
        end,
      ),
      step: { kind: "integer", read: next - at, chunks: 0 },
    };
  }
  if (major === 2) {
    if (value < 0n) {
      const streamed = stream(
        bytes,
        next,
        openLeaf(frame, 0, Buffer.alloc(0), 1n, true),
        stack,
        end,
      );
      return {
        progress: streamed.progress,
        step: { ...streamed.step, read: streamed.step.read + 1 },
      };
    }
    const string = shortBytes(bytes, at, end);
    if (string === null) return refusedStep;
    const [content, after] = string;
    return {
      progress: complete(
        bytesLeaf(
          hashMidgardCekBlobChunk(content),
          BigInt(content.length),
          BigInt(after - at),
        ),
        after,
        frame,
        stack,
        end,
      ),
      step: { kind: "bytes", read: after - at, chunks: 0 },
    };
  }
  if (major === 4)
    return fields(bytes, at, at, { kind: "list" }, frame, stack, end);
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
        step: { kind: "finish", read: next - at, chunks: 0 },
      };
    if (value > 0n)
      return {
        progress: advanced(
          next,
          { kind: "map", remaining: 2n * value },
          push({ kind: "parent", frame }, stack),
        ),
        step: { kind: "open", read: next - at, chunks: 0 },
      };
    return refusedStep;
  }
  if (major === 6) return tagged(bytes, at, next, value, frame, stack, end);
  return refusedStep;
};

/** One step over `bytes[at, end)`, as `data_serialise_walk_v1.step` takes it. */
export const stepMidgardCekSerialise = (
  bytes: Uint8Array,
  at: number,
  frame: Frame,
  stack: Stack,
  end: number,
): Stepped => {
  if (frame.kind === "folding")
    return fold(frame.open, frame.folded, at, stack, end);
  if (frame.kind === "leaf") return stream(bytes, at, frame, stack, end);
  return read(bytes, at, frame, stack, end);
};

/** Up to `steps` steps, as `data_serialise_walk_v1.run` takes them. */
export const runMidgardCekSerialise = (
  bytes: Uint8Array,
  at: number,
  frame: Frame,
  stack: Stack,
  end: number,
  steps: number,
): Progress => {
  let progress: Progress = advanced(at, frame, stack);
  for (let taken = 0; taken < steps; taken += 1) {
    if (progress.kind !== "advanced") return progress;
    progress = stepMidgardCekSerialise(
      bytes,
      progress.at,
      progress.frame,
      progress.stack,
      end,
    ).progress;
  }
  return progress;
};
