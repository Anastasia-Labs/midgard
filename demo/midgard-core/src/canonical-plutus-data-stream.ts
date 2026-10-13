/**
 * TypeScript twin of `canonical_plutus_data_stream_v1`
 * (onchain/aiken/lib/midgard/canonical-plutus-data-stream-v1.ak): the
 * canonical Data grammar read as a resumable stream, one node at a time, with
 * the open containers kept as an explicit stack whose hash chain is all a
 * paused read stores.
 *
 * Every refusal, pause offset, frame and stack root must equal the on-chain
 * reader's: an off-chain plan built from this module is replayed on chain
 * step by step. The shared canonical Data corpus checks both readers.
 */
import { computeHash32 } from "./codec/hash.js";

/** One open container (`StreamFrameV1`). */
export type MidgardDataStreamFrame =
  /** An indefinite list or constructor-field list; `started` once an item began. */
  | { readonly kind: "list"; readonly started: boolean }
  /** A definite map with `remaining` keys and values still to read. */
  | { readonly kind: "map"; readonly remaining: bigint }
  /** An indefinite byte string after `seen` full chunks; `leading` for a bignum magnitude. */
  | {
      readonly kind: "chunks";
      readonly seen: number;
      readonly leading: boolean;
    };

/** Where a bounded run stopped (`StreamOutcomeV1`). */
export type MidgardDataStreamOutcome =
  | { readonly kind: "canonical" }
  | { readonly kind: "nonCanonical" }
  | {
      readonly kind: "paused";
      /** Window-relative offset of the next node. */
      readonly offset: number;
      /** The revealed open containers left after the run, top first. */
      readonly frames: readonly MidgardDataStreamFrame[];
      readonly nodes: number;
    };

/** The most bytes one node reads past its first byte's offset. */
export const MIDGARD_DATA_STREAM_MAX_NODE_READ_BYTES = 67;

/** The empty stack's hash chain. */
export const MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT: Buffer = Buffer.alloc(0);

const DATA_BYTES_CHUNK = 64;
const MIN_BIGNUM_MAGNITUDE_BYTES = 9;
const BREAK = 0xff;
const INDEFINITE_BYTES = 0x5f;
const EMPTY_LIST = 0x80;
const INDEFINITE_LIST = 0x9f;
const GENERIC_CONSTR_PAIR = 0x82;
const SMALL_CONSTR_FIRST = 121n;
const SMALL_CONSTR_LAST = 127n;
const LARGE_CONSTR_FIRST = 1280n;
const LARGE_CONSTR_LAST = 1400n;
const GENERIC_CONSTR_TAG = 102n;
const MIN_GENERIC_CONSTR_ALTERNATIVE = 128n;
const POSITIVE_BIGNUM_TAG = 2n;
const NEGATIVE_BIGNUM_TAG = 3n;

const cborUnsignedHead = (major: number, value: bigint): Buffer => {
  const prefix = major << 5;
  if (value < 24n) return Buffer.from([prefix | Number(value)]);
  if (value < 0x100n) return Buffer.from([prefix | 24, Number(value)]);
  if (value < 0x10000n) {
    const out = Buffer.alloc(3);
    out[0] = prefix | 25;
    out.writeUInt16BE(Number(value), 1);
    return out;
  }
  if (value < 0x100000000n) {
    const out = Buffer.alloc(5);
    out[0] = prefix | 26;
    out.writeUInt32BE(Number(value), 1);
    return out;
  }
  const out = Buffer.alloc(9);
  out[0] = prefix | 27;
  out.writeBigUInt64BE(value, 1);
  return out;
};

/** `serialiseData` of a non-negative Plutus integer. */
const encodeNatural = (value: bigint): Buffer => {
  if (value < 0n) throw new Error("data stream frame field is negative");
  if (value < 1n << 64n) return cborUnsignedHead(0, value);
  const hex = value.toString(16);
  const magnitude = Buffer.from(hex.length % 2 === 0 ? hex : `0${hex}`, "hex");
  if (magnitude.length > DATA_BYTES_CHUNK)
    throw new Error("data stream frame field is out of range");
  return Buffer.concat([
    Buffer.from([0xc2]),
    cborUnsignedHead(2, BigInt(magnitude.length)),
    magnitude,
  ]);
};

const encodeBool = (value: boolean): Buffer =>
  Buffer.from(value ? "d87a80" : "d87980", "hex");

const constr = (tag: number, fields: readonly Buffer[]): Buffer =>
  Buffer.concat([
    Buffer.from([0xd8, tag]),
    Buffer.from([INDEFINITE_LIST]),
    ...fields,
    Buffer.from([BREAK]),
  ]);

/** `cbor.serialise` of one `StreamFrameV1`. */
export const encodeMidgardDataStreamFrame = (
  frame: MidgardDataStreamFrame,
): Buffer => {
  switch (frame.kind) {
    case "list":
      return constr(0x79, [encodeBool(frame.started)]);
    case "map":
      return constr(0x7a, [encodeNatural(frame.remaining)]);
    case "chunks":
      return constr(0x7b, [
        encodeNatural(BigInt(frame.seen)),
        encodeBool(frame.leading),
      ]);
  }
};

/** The hash chain of `frames` (top first) above `tailRoot` (`stack_root_v1`). */
export const midgardDataStreamStackRoot = (
  frames: readonly MidgardDataStreamFrame[],
  tailRoot: Uint8Array,
): Buffer => {
  let root = Buffer.from(tailRoot);
  for (let index = frames.length - 1; index >= 0; index -= 1) {
    root = computeHash32(
      Buffer.concat([encodeMidgardDataStreamFrame(frames[index]!), root]),
    );
  }
  return root;
};

type Head = { readonly past: number; readonly value: bigint };

const NON_CANONICAL = Object.freeze({ kind: "nonCanonical" } as const);
const CANONICAL = Object.freeze({ kind: "canonical" } as const);

/**
 * Reads up to `budget` nodes of the span from `offset` in `bytes`
 * (`run_v1`). `bytes` is a window of the span's carrier, already cut at the
 * span's end when `full`; `rootStart` is the window-relative offset of the
 * span's first byte; `frames` are the revealed open containers, top first,
 * above `tailRoot`.
 */
export const runMidgardDataStream = (
  input: MidgardDataStreamRunInput,
): MidgardDataStreamOutcome => runMidgardDataStreamTracked(input).outcome;

export type MidgardDataStreamRunInput = {
  readonly bytes: Uint8Array;
  readonly full: boolean;
  readonly rootStart: number;
  readonly offset: number;
  readonly frames: readonly MidgardDataStreamFrame[];
  readonly tailRoot: Uint8Array;
  readonly budget: number;
};

/**
 * {@link runMidgardDataStream}, also reporting how many of the revealed
 * frames the run popped (`consumed`). A planner reveals one frame more than
 * that, so the on-chain run never stops early for want of a revealed frame,
 * and hashes no frame it does not touch.
 */
export const runMidgardDataStreamTracked = ({
  bytes,
  full,
  rootStart,
  offset: startOffset,
  frames: revealed,
  tailRoot,
  budget,
}: MidgardDataStreamRunInput): {
  readonly outcome: MidgardDataStreamOutcome;
  readonly consumed: number;
} => {
  const limit = bytes.length;
  const lastStart = full
    ? limit
    : limit - MIDGARD_DATA_STREAM_MAX_NODE_READ_BYTES;
  const tailClosed = tailRoot.length === 0;
  // Bottom first, so the top is the last element.
  const stack: MidgardDataStreamFrame[] = [...revealed].reverse();
  let offset = startOffset;
  let nodes = 0;
  let lowest = stack.length;

  const head = (at: number, first: number): Head | null => {
    const additional = first & 31;
    if (additional <= 23) return { past: at + 1, value: BigInt(additional) };
    if (additional === 24) {
      if (at + 1 >= limit) return null;
      const value = bytes[at + 1]!;
      return value >= 24 ? { past: at + 2, value: BigInt(value) } : null;
    }
    const width =
      additional === 25 ? 2 : additional === 26 ? 4 : additional === 27 ? 8 : 0;
    if (width === 0) return null;
    const end = at + width;
    if (end >= limit) return null;
    let value = 0n;
    for (let index = at + 1; index <= end; index += 1)
      value = (value << 8n) | BigInt(bytes[index]!);
    const least = 1n << BigInt(4 * width);
    return value >= least ? { past: end + 1, value } : null;
  };

  // Each reader consumes one node: it leaves `offset` and `stack` at the
  // next node and returns true, or returns false for a refusal.
  const listOpen = (at: number): boolean => {
    if (at >= limit) return false;
    const first = bytes[at]!;
    if (first === EMPTY_LIST) {
      offset = at + 1;
      return true;
    }
    if (first === INDEFINITE_LIST) {
      stack.push({ kind: "list", started: false });
      offset = at + 1;
      return true;
    }
    return false;
  };

  const genericConstr = (at: number): boolean => {
    if (at + 1 >= limit) return false;
    if (bytes[at] !== GENERIC_CONSTR_PAIR) return false;
    const alternativeHead = bytes[at + 1]!;
    if (alternativeHead >> 5 !== 0) return false;
    const read = head(at + 1, alternativeHead);
    if (read === null || read.value < MIN_GENERIC_CONSTR_ALTERNATIVE)
      return false;
    return listOpen(read.past);
  };

  const magnitude = (at: number): boolean => {
    if (at >= limit) return false;
    const first = bytes[at]!;
    if (first >> 5 !== 2) return false;
    if (first === INDEFINITE_BYTES) {
      stack.push({ kind: "chunks", seen: 0, leading: true });
      offset = at + 1;
      return true;
    }
    const read = head(at, first);
    if (read === null) return false;
    const length = Number(read.value);
    if (
      read.value >= BigInt(MIN_BIGNUM_MAGNITUDE_BYTES) &&
      read.value <= BigInt(DATA_BYTES_CHUNK) &&
      read.past + length <= limit &&
      bytes[read.past] !== 0
    ) {
      offset = read.past + length;
      return true;
    }
    return false;
  };

  const item = (at: number): boolean => {
    if (at >= limit) return false;
    const first = bytes[at]!;
    const major = first >> 5;
    if (major <= 1) {
      const read = head(at, first);
      if (read === null) return false;
      offset = read.past;
      return true;
    }
    if (major === 2) {
      if (first === INDEFINITE_BYTES) {
        stack.push({ kind: "chunks", seen: 0, leading: false });
        offset = at + 1;
        return true;
      }
      const read = head(at, first);
      if (read === null) return false;
      if (
        read.value <= BigInt(DATA_BYTES_CHUNK) &&
        read.past + Number(read.value) <= limit
      ) {
        offset = read.past + Number(read.value);
        return true;
      }
      return false;
    }
    if (major === 4) return listOpen(at);
    if (major === 5) {
      const read = head(at, first);
      if (read === null) return false;
      if (read.value !== 0n)
        stack.push({ kind: "map", remaining: 2n * read.value });
      offset = read.past;
      return true;
    }
    if (major === 6) {
      const read = head(at, first);
      if (read === null) return false;
      const tag = read.value;
      if (
        (tag >= SMALL_CONSTR_FIRST && tag <= SMALL_CONSTR_LAST) ||
        (tag >= LARGE_CONSTR_FIRST && tag <= LARGE_CONSTR_LAST)
      )
        return listOpen(read.past);
      if (tag === GENERIC_CONSTR_TAG) return genericConstr(read.past);
      if (tag === POSITIVE_BIGNUM_TAG || tag === NEGATIVE_BIGNUM_TAG)
        return magnitude(read.past);
      return false;
    }
    return false;
  };

  const chunk = (at: number, seen: number, leading: boolean): boolean => {
    if (at >= limit) return false;
    const first = bytes[at]!;
    if (first === BREAK) {
      if (seen < 2) return false;
      stack.pop();
      offset = at + 1;
      return true;
    }
    if (first >> 5 !== 2) return false;
    const read = head(at, first);
    if (read === null) return false;
    const { past } = read;
    if (read.value === BigInt(DATA_BYTES_CHUNK)) {
      if (past + DATA_BYTES_CHUNK <= limit && (!leading || bytes[past] !== 0)) {
        stack[stack.length - 1] = {
          kind: "chunks",
          seen: seen + 1,
          leading: false,
        };
        offset = past + DATA_BYTES_CHUNK;
        return true;
      }
      return false;
    }
    const length = Number(read.value);
    if (
      length > 0 &&
      length < DATA_BYTES_CHUNK &&
      seen >= 1 &&
      past + length < limit &&
      bytes[past + length] === BREAK
    ) {
      stack.pop();
      offset = past + length + 1;
      return true;
    }
    return false;
  };

  const done = (outcome: MidgardDataStreamOutcome) =>
    Object.freeze({ outcome, consumed: revealed.length - lowest });
  const paused = () =>
    done(
      Object.freeze({
        kind: "paused",
        offset,
        frames: Object.freeze([...stack].reverse()),
        nodes,
      }),
    );

  for (;;) {
    const stop = nodes >= budget || offset > lastStart;
    let read: boolean;
    if (stack.length === 0) {
      if (!tailClosed) return paused();
      if (offset > rootStart)
        return done(full && offset === limit ? CANONICAL : NON_CANONICAL);
      if (stop) return paused();
      read = item(offset);
    } else {
      if (stop) return paused();
      const frame = stack[stack.length - 1]!;
      switch (frame.kind) {
        case "map":
          if (frame.remaining === 0n) {
            stack.pop();
            read = true;
          } else {
            stack[stack.length - 1] = {
              kind: "map",
              remaining: frame.remaining - 1n,
            };
            read = item(offset);
          }
          break;
        case "list":
          if (offset >= limit) read = false;
          else if (bytes[offset] === BREAK) {
            if (frame.started) {
              stack.pop();
              offset += 1;
              read = true;
            } else read = false;
          } else {
            if (!frame.started)
              stack[stack.length - 1] = { kind: "list", started: true };
            read = item(offset);
          }
          break;
        case "chunks":
          read = chunk(offset, frame.seen, frame.leading);
          break;
      }
    }
    if (!read) return done(NON_CANONICAL);
    nodes += 1;
    if (stack.length < lowest) lowest = stack.length;
  }
};

/** The whole span at once: `true` exactly when it is one canonical Data item. */
export const isCanonicalMidgardDataStream = (source: Uint8Array): boolean =>
  runMidgardDataStream({
    bytes: source,
    full: true,
    rootStart: 0,
    offset: 0,
    frames: [],
    tailRoot: MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
    budget: Number.MAX_SAFE_INTEGER,
  }).kind === "canonical";
