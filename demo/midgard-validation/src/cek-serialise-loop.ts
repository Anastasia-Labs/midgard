/**
 * The off-chain twin of `midgard/cek_serialise_loop_v1`: the hops of
 * serialiseData (tag 51) over a Data argument of any size.
 *
 * The result's bytes R are staged as B1 runs. Each hop absorbs the runs it
 * names into a flat-tree frontier of R and reads them, after the unread tail
 * the previous hop left, with the resumable reader. The hop that summarises
 * the value checks the summary against the argument and R's root and length
 * against the result.
 *
 * {@link advanceMidgardCekSerialiseHop} takes exactly the decisions
 * `advance_serialise_v1` takes, from the same inputs: the state the chain
 * holds (its stack only as a root) and the hop's redeemer.
 */
import {
  appendMidgardCekBlobFrontierChunk,
  collectMidgardCekStagedRuns,
  compareMidgardCekStagedOutRefs,
  emptyMidgardCekBlobFrontier,
  finalizeMidgardCekBlobFrontier,
  hashMidgardCekDataNode,
  type MidgardCekBlobFrontier,
  midgardCekBytesMemoryWords,
  midgardCekDataBytesCborLength,
  type MidgardCekStagedOutRef,
  type MidgardCekStagedReferenceInput,
} from "@al-ft/midgard-core";
import { Constr, type Data } from "@lucid-evolution/lucid";

import type { MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import {
  midgardCekSerialiseChain,
  midgardCekSerialiseChainOver,
  midgardCekSerialiseEntryData,
  midgardCekSerialiseFrameData,
  midgardCekSerialiseFrontierData,
  pushMidgardCekSerialiseEntry,
} from "./cek-serialise-walk.data.js";
import {
  MIDGARD_CEK_SERIALISE_CHUNK_BYTES,
  MidgardCekSerialiseStackShortError,
  runMidgardCekSerialise,
} from "./cek-serialise-walk.js";
import type {
  MidgardCekSerialiseEntry,
  MidgardCekSerialiseFrame,
  MidgardCekSerialiseStack,
} from "./cek-serialise-walk.types.js";

/** The unread tail: the staging output it lies in and its offset there. */
export type MidgardCekSerialiseCarry = {
  readonly run: MidgardCekStagedOutRef;
  readonly offset: number;
};

/** The loop's state between hops, as the chain holds it. */
export type MidgardCekSerialiseState = {
  readonly frame: MidgardCekSerialiseFrame;
  readonly stackRoot: Buffer;
  readonly frontier: MidgardCekBlobFrontier;
  readonly carry: MidgardCekSerialiseCarry | null;
};

export const MIDGARD_CEK_SERIALISE_INITIAL_STATE: MidgardCekSerialiseState =
  Object.freeze({
    frame: Object.freeze({ kind: "root" }),
    stackRoot: Buffer.alloc(0),
    frontier: emptyMidgardCekBlobFrontier(),
    carry: null,
  });

/**
 * What one hop reads: the staging outputs `refs` (ledger order), the indices
 * of the runs to read in byte order (the carried run first when there is a
 * tail), and the stack entries the hop pops below its own pushes (`restore`,
 * top first, above `base`).
 */
export type MidgardCekSerialiseHop = {
  readonly refs: readonly MidgardCekStagedOutRef[];
  readonly order: readonly number[];
  readonly restore: readonly MidgardCekSerialiseEntry[];
  readonly base: Buffer;
};

/** The facts a hop reads: the step's argument and result presentations. */
export type MidgardCekSerialiseFacts = {
  readonly arguments: readonly MidgardCekDirectValueWitness[];
  readonly result: MidgardCekDirectValueWitness;
};

export type MidgardCekSerialiseAdvance =
  | { readonly kind: "next"; readonly state: MidgardCekSerialiseState }
  | { readonly kind: "done" }
  | { readonly kind: "rejected"; readonly reason: string };

/** The canonical CBOR of the Data and ByteString constant types. */
const DATA_TYPE = Buffer.from("9f08ff", "hex");
const BYTES_TYPE = Buffer.from("9f01ff", "hex");

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

export const midgardCekSerialiseOutRefData = (
  ref: MidgardCekStagedOutRef,
): Data => new Constr(0, [hex(ref.transactionId), BigInt(ref.outputIndex)]);

/** `SerialiseStateV1` as the hop's output carries it. */
export const midgardCekSerialiseStateData = (
  state: MidgardCekSerialiseState,
): Data =>
  new Constr(0, [
    midgardCekSerialiseFrameData(state.frame),
    hex(state.stackRoot),
    midgardCekSerialiseFrontierData(state.frontier),
    state.carry === null
      ? new Constr(1, [])
      : new Constr(0, [
          new Constr(0, [
            midgardCekSerialiseOutRefData(state.carry.run),
            BigInt(state.carry.offset),
          ]),
        ]),
  ]);

/** `SerialiseHopV1` as the hop's redeemer carries it. */
export const midgardCekSerialiseHopData = (hop: MidgardCekSerialiseHop): Data =>
  new Constr(0, [
    hop.refs.map(midgardCekSerialiseOutRefData),
    hop.order.map((index) => BigInt(index)),
    hop.restore.map(midgardCekSerialiseEntryData),
    hex(hop.base),
  ]);

/** The stack `entries` (top first) above nothing, as the walk reads it. */
export const midgardCekSerialiseStackOf = (
  entries: readonly MidgardCekSerialiseEntry[],
): MidgardCekSerialiseStack =>
  entries.reduceRight<MidgardCekSerialiseStack>(
    (below, entry) => pushMidgardCekSerialiseEntry(entry, below),
    null,
  );

/** The entries of `stack`, top first. */
export const midgardCekSerialiseEntriesOf = (
  stack: MidgardCekSerialiseStack,
): MidgardCekSerialiseEntry[] => {
  const entries: MidgardCekSerialiseEntry[] = [];
  for (let node = stack; node !== null; node = node.below) {
    entries.push(node.entry);
  }
  return entries;
};

/** The byte-string summary of R from its flat root and length. */
export const midgardCekSerialiseResultPayload = (
  root: Uint8Array,
  length: number,
): {
  readonly root: Buffer;
  readonly cborLength: bigint;
  readonly memory: bigint;
} => {
  const bytesLength = BigInt(length);
  const cborLength = midgardCekDataBytesCborLength(bytesLength);
  const memory = 4n + midgardCekBytesMemoryWords(bytesLength);
  return {
    root: Buffer.from(
      hashMidgardCekDataNode({
        kind: "bytes",
        bytesRoot: root,
        bytesLength,
        cborLength,
        memory,
      }),
    ),
    cborLength,
    memory,
  };
};

/**
 * The facts of a serialiseData step whose argument has summary `argument`
 * and whose result is R (`bytes`), and R's 4,095-byte chunks.
 */
export const midgardCekSerialiseFactsOf = (
  argument: {
    readonly root: Uint8Array;
    readonly cborLength: bigint;
    readonly memory: bigint;
  },
  bytes: Uint8Array,
): { readonly facts: MidgardCekSerialiseFacts; readonly chunks: Buffer[] } => {
  const source = Buffer.from(bytes);
  const chunks: Buffer[] = [];
  let frontier = emptyMidgardCekBlobFrontier();
  for (
    let offset = 0;
    offset < source.length;
    offset += MIDGARD_CEK_SERIALISE_CHUNK_BYTES
  ) {
    const chunk = source.subarray(
      offset,
      offset + MIDGARD_CEK_SERIALISE_CHUNK_BYTES,
    );
    chunks.push(chunk);
    frontier = appendMidgardCekBlobFrontierChunk(frontier, chunk);
  }
  const root = finalizeMidgardCekBlobFrontier(frontier);
  if (root === null) throw new RangeError("serialiseData gives no empty bytes");
  return {
    facts: {
      arguments: [
        {
          kind: "semanticConstant",
          witness: {
            typeCbor: DATA_TYPE,
            payload: argument,
            memory: argument.memory,
          },
        },
      ],
      result: {
        kind: "semanticConstant",
        witness: {
          typeCbor: BYTES_TYPE,
          payload: midgardCekSerialiseResultPayload(root, source.length),
          memory: midgardCekBytesMemoryWords(BigInt(source.length)),
        },
      },
    },
    chunks,
  };
};

const sameSummary = (
  left: {
    readonly root: Uint8Array;
    readonly cborLength: bigint;
    readonly memory: bigint;
  },
  right: {
    readonly root: Uint8Array;
    readonly cborLength: bigint;
    readonly memory: bigint;
  },
): boolean =>
  Buffer.from(left.root).equals(Buffer.from(right.root)) &&
  left.cborLength === right.cborLength &&
  left.memory === right.memory;

const runLength = (chunks: readonly Uint8Array[]): number =>
  chunks.reduce((total, chunk) => total + chunk.length, 0);

/** A run's bytes from `offset`, as pieces; null past its end. */
const tailOf = (chunks: readonly Buffer[], offset: number): Buffer[] | null => {
  let skip = offset;
  for (let index = 0; index < chunks.length; index += 1) {
    const chunk = chunks[index]!;
    if (skip < chunk.length)
      return [chunk.subarray(skip), ...chunks.slice(index + 1)];
    skip -= chunk.length;
  }
  return null;
};

const appendChunks = (
  frontier: MidgardCekBlobFrontier,
  chunks: readonly Buffer[],
): MidgardCekBlobFrontier | null => {
  let next = frontier;
  for (const chunk of chunks) {
    if (
      chunk.length === 0 ||
      next.byteLength !==
        BigInt(next.count) * BigInt(MIDGARD_CEK_SERIALISE_CHUNK_BYTES)
    )
      return null;
    next = appendMidgardCekBlobFrontierChunk(next, chunk);
  }
  return next;
};

const rejected = (reason: string): MidgardCekSerialiseAdvance => ({
  kind: "rejected",
  reason,
});

const sameRef = (
  left: MidgardCekStagedOutRef,
  right: MidgardCekStagedOutRef,
): boolean => compareMidgardCekStagedOutRefs(left, right) === 0;

/**
 * One hop over walk steps [cursor, end) from `from` (the cursor and state the
 * previous hop left, null on the first hop), as `advance_serialise_v1` runs
 * it: the state to carry on with, done when the value is summarised and every
 * exit check holds, or the check that fails.
 */
export const advanceMidgardCekSerialiseHop = (
  facts: MidgardCekSerialiseFacts,
  hop: MidgardCekSerialiseHop,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
  from: {
    readonly cursor: number;
    readonly state: MidgardCekSerialiseState;
  } | null,
  end: number,
): MidgardCekSerialiseAdvance => {
  const result = facts.result;
  if (result.kind !== "semanticConstant") return rejected("result");
  const cursor = from?.cursor ?? 0;
  const state = from?.state ?? MIDGARD_CEK_SERIALISE_INITIAL_STATE;
  if (!(cursor < end)) return rejected("cursor");
  if (BigInt(end) > 16n * result.witness.memory) return rejected("end");
  let runs: Buffer[][];
  try {
    runs = collectMidgardCekStagedRuns(referenceInputs, hop.refs);
  } catch {
    return rejected("staging");
  }
  let tail: Buffer[] = [];
  let fresh = hop.order;
  if (state.carry !== null) {
    const [first, ...rest] = hop.order;
    const carried = first === undefined ? undefined : runs[first];
    if (carried === undefined || !sameRef(hop.refs[first!]!, state.carry.run))
      return rejected("carry");
    const pieces = tailOf(carried, state.carry.offset);
    if (pieces === null) return rejected("carry");
    tail = pieces;
    fresh = rest;
  }
  const freshRuns: Buffer[][] = [];
  for (const index of fresh) {
    const run = runs[index];
    if (run === undefined) return rejected("order");
    freshRuns.push(run);
  }
  const freshChunks = freshRuns.flat();
  const frontier = appendChunks(state.frontier, freshChunks);
  if (frontier === null) return rejected("frontier");
  const window = Buffer.concat([...tail, ...freshChunks]);
  if (!midgardCekSerialiseChain(hop.restore, hop.base).equals(state.stackRoot))
    return rejected("stack root");
  const restored = midgardCekSerialiseStackOf(hop.restore);
  let progress;
  try {
    progress = runMidgardCekSerialise(
      window,
      0,
      state.frame,
      restored,
      window.length,
      end - cursor,
    );
  } catch (error) {
    if (error instanceof MidgardCekSerialiseStackShortError)
      return rejected("restore more of the stack");
    throw error;
  }
  if (progress.kind === "refused") return rejected("unreadable");
  if (progress.kind === "advanced") {
    const { at } = progress;
    let carry: MidgardCekSerialiseCarry | null = null;
    if (at !== window.length) {
      const last = freshRuns.at(-1);
      if (last === undefined) {
        if (state.carry === null) return rejected("carry");
        carry = { run: state.carry.run, offset: state.carry.offset + at };
      } else {
        const start = window.length - runLength(last);
        if (at < start) return rejected("tail");
        carry = { run: hop.refs[fresh.at(-1)!]!, offset: at - start };
      }
    }
    return {
      kind: "next",
      state: {
        frame: progress.frame,
        stackRoot: midgardCekSerialiseChainOver(
          progress.stack,
          restored,
          hop.base,
        ),
        frontier,
        carry,
      },
    };
  }
  const [argument, ...others] = facts.arguments;
  if (argument?.kind !== "semanticConstant" || others.length !== 0)
    return rejected("argument");
  const { summary } = progress;
  if (
    !Buffer.from(argument.witness.typeCbor).equals(DATA_TYPE) ||
    !sameSummary(argument.witness.payload, summary) ||
    argument.witness.memory !== summary.memory
  )
    return rejected("summary");
  const length = Number(frontier.byteLength);
  const root = finalizeMidgardCekBlobFrontier(frontier);
  if (
    root === null ||
    !Buffer.from(result.witness.typeCbor).equals(BYTES_TYPE) ||
    !sameSummary(
      result.witness.payload,
      midgardCekSerialiseResultPayload(root, length),
    ) ||
    result.witness.memory !== midgardCekBytesMemoryWords(length)
  )
    return rejected("result");
  return { kind: "done" };
};
