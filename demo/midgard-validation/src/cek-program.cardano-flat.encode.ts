/**
 * Writes a program in Cardano's Flat encoding: the inverse of
 * `cek-program.cardano-flat.reader.ts` and `.decode.ts`. Data constants are
 * spelled in Midgard's canonical Data CBOR. The term is written with an
 * explicit stack, so program nesting never deepens the JavaScript stack.
 *
 * Derived from the `UPLCEncoder` and `FlatEncoder` of @harmoniclabs/uplc 2.0.7
 * (Copyright Harmonic Laboratories, Apache License 2.0,
 * https://github.com/HarmonicLabs/uplc). Changes: iterative; the version is
 * written as three Naturals (upstream writes three bytes, which differs above
 * 127); array and Value constants, which have no Midgard form, are refused.
 */
import { encodeMidgardCekPlutusData } from "./plutus-data-iterative.encode.js";

/** Constant type tags as Flat spells them (`DefaultUni`). */
const T = {
  int: 0,
  byteStr: 1,
  str: 2,
  unit: 3,
  bool: 4,
  list: 5,
  pair: 6,
  apply: 7,
  data: 8,
} as const;

/** A term as the encoder reads it: its Flat tag and fields. */
export type FlatTerm =
  | { readonly tag: 0; readonly deBruijn: number | bigint }
  | { readonly tag: 1; readonly delayedTerm: FlatTerm }
  | { readonly tag: 2; readonly body: FlatTerm }
  | { readonly tag: 3; readonly func: FlatTerm; readonly arg: FlatTerm }
  | {
      readonly tag: 4;
      readonly type: readonly number[];
      readonly value: unknown;
    }
  | { readonly tag: 5; readonly forced: FlatTerm }
  | { readonly tag: 6 }
  | { readonly tag: 7; readonly builtinTag: number }
  | {
      readonly tag: 8;
      readonly index: bigint;
      readonly terms: readonly FlatTerm[];
    }
  | {
      readonly tag: 9;
      readonly constrTerm: FlatTerm;
      readonly continuations: readonly FlatTerm[];
    };

/** A program as the encoder reads it. */
export interface FlatProgram {
  readonly version: {
    readonly major: number;
    readonly minor: number;
    readonly patch: number;
  };
  readonly body: FlatTerm;
}

/** Bits, most significant first, into a growing byte buffer. */
class FlatWriter {
  private buffer = new Uint8Array(256);
  private length = 0;
  private current = 0;
  private filled = 0;

  private pushByte(byte: number): void {
    if (this.length === this.buffer.length) {
      const grown = new Uint8Array(this.buffer.length * 2);
      grown.set(this.buffer);
      this.buffer = grown;
    }
    this.buffer[this.length] = byte;
    this.length += 1;
  }

  bit(bit: 0 | 1): void {
    this.current = (this.current << 1) | bit;
    this.filled += 1;
    if (this.filled === 8) {
      this.pushByte(this.current);
      this.current = 0;
      this.filled = 0;
    }
  }

  bits(value: number, count: number): void {
    for (let shift = count - 1; shift >= 0; shift -= 1)
      this.bit(((value >> shift) & 1) as 0 | 1);
  }

  /** Flat filler: zeros up to the last bit of the byte, then a one. */
  pad(): void {
    while (this.filled < 7) this.bit(0);
    this.bit(1);
  }

  /** 7-bit groups, least significant first, each after a "more" bit. */
  natural(value: bigint): void {
    if (value < 0n) throw new Error("Flat natural must not be negative");
    let rest = value;
    do {
      const group = Number(rest & 0x7fn);
      rest >>= 7n;
      this.bit(rest > 0n ? 1 : 0);
      this.bits(group, 7);
    } while (rest > 0n);
  }

  /** ZigZag, then a Natural. */
  integer(value: bigint): void {
    this.natural(value >= 0n ? value << 1n : (-value << 1n) - 1n);
  }

  /** Filler, then chunks of at most 255 bytes, then an empty chunk. */
  byteString(bytes: Uint8Array): void {
    this.pad();
    for (let offset = 0; offset < bytes.length; offset += 255) {
      const chunk = bytes.subarray(offset, offset + 255);
      this.pushByte(chunk.length);
      for (const byte of chunk) this.pushByte(byte);
    }
    this.pushByte(0);
  }

  /** Each item after a 1 bit, then a 0 bit. */
  list<Item>(items: readonly Item[], write: (item: Item) => void): void {
    for (const item of items) {
      this.bit(1);
      write(item);
    }
    this.bit(0);
  }

  finish(): Uint8Array {
    this.pad();
    return this.buffer.slice(0, this.length);
  }
}

/** Types without arguments: BLS types only as an empty list's element. */
const LEAF_TAGS: ReadonlySet<number> = new Set([
  T.int,
  T.byteStr,
  T.str,
  T.unit,
  T.bool,
  T.data,
  9,
  10,
  11,
]);

/** The length of the complete type that starts at `start`. */
const typeLength = (type: readonly number[], start: number): number => {
  let open = 1;
  let index = start;
  while (open > 0) {
    const tag = type[index];
    if (tag === undefined) throw new Error("incomplete constant type");
    open += (tag === T.pair ? 2 : tag === T.list ? 1 : 0) - 1;
    index += 1;
  }
  return index - start;
};

/** Flat's type tags: `list` and `pair` applied with tag 7. */
const wireType = (type: readonly number[]): number[] => {
  const tag = type[0];
  if (tag === T.list) return [T.apply, T.list, ...wireType(type.slice(1))];
  if (tag === T.pair) {
    const firstLength = typeLength(type, 1);
    return [
      T.apply,
      T.apply,
      T.pair,
      ...wireType(type.slice(1, 1 + firstLength)),
      ...wireType(type.slice(1 + firstLength)),
    ];
  }
  if (tag === undefined || !LEAF_TAGS.has(tag))
    throw new Error(`constant type tag ${String(tag)} has no Midgard form`);
  return [tag];
};

const UTF8 = new TextEncoder();

type DataValue = Parameters<typeof encodeMidgardCekPlutusData>[0];

/** A constant's value. Recursion follows the type only. */
const writeValue = (
  writer: FlatWriter,
  type: readonly number[],
  value: unknown,
): void => {
  switch (type[0]) {
    case T.int:
      return writer.integer(value as bigint);
    case T.byteStr:
      return writer.byteString(value as Uint8Array);
    case T.str:
      return writer.byteString(UTF8.encode(value as string));
    case T.unit:
      return;
    case T.bool:
      return writer.bit(value ? 1 : 0);
    case T.list: {
      const element = type.slice(1);
      return writer.list(value as readonly unknown[], (item) =>
        writeValue(writer, element, item),
      );
    }
    case T.pair: {
      const firstLength = typeLength(type, 1);
      const pair = value as { readonly fst: unknown; readonly snd: unknown };
      writeValue(writer, type.slice(1, 1 + firstLength), pair.fst);
      writeValue(writer, type.slice(1 + firstLength), pair.snd);
      return;
    }
    case T.data:
      return writer.byteString(encodeMidgardCekPlutusData(value as DataValue));
    default:
      throw new Error(`constant type tag ${String(type[0])} has no Flat value`);
  }
};

/** Work items: a term still to write, or a list item / list end marker. */
type Task =
  | { readonly op: "term"; readonly term: FlatTerm }
  | { readonly op: "item"; readonly term: FlatTerm }
  | { readonly op: "end" };

const END: Task = { op: "end" };

/** Schedules a term list: each item after a 1 bit, then a 0 bit. */
const pushList = (tasks: Task[], terms: readonly FlatTerm[]): void => {
  tasks.push(END);
  for (let index = terms.length - 1; index >= 0; index -= 1)
    tasks.push({ op: "item", term: terms[index]! });
};

const writeTerm = (writer: FlatWriter, root: FlatTerm): void => {
  const tasks: Task[] = [{ op: "term", term: root }];
  for (let task = tasks.pop(); task !== undefined; task = tasks.pop()) {
    if (task.op === "end") {
      writer.bit(0);
      continue;
    }
    if (task.op === "item") writer.bit(1);
    const term = task.term;
    writer.bits(term.tag, 4);
    switch (term.tag) {
      case 0:
        writer.natural(BigInt(term.deBruijn) + 1n);
        break;
      case 1:
        tasks.push({ op: "term", term: term.delayedTerm });
        break;
      case 2:
        tasks.push({ op: "term", term: term.body });
        break;
      case 3:
        tasks.push(
          { op: "term", term: term.arg },
          { op: "term", term: term.func },
        );
        break;
      case 4:
        writer.list(wireType(term.type), (tag) => writer.bits(tag, 4));
        writeValue(writer, term.type, term.value);
        break;
      case 5:
        tasks.push({ op: "term", term: term.forced });
        break;
      case 6:
        break;
      case 7:
        writer.bits(term.builtinTag, 7);
        break;
      case 8:
        writer.natural(term.index);
        pushList(tasks, term.terms);
        break;
      case 9:
        pushList(tasks, term.continuations);
        tasks.push({ op: "term", term: term.constrTerm });
        break;
      default:
        throw new Error(`unknown term tag ${String((term as FlatTerm).tag)}`);
    }
  }
};

/** A program's Flat bytes, filler included. */
export const encodeCardanoFlatProgram = (program: FlatProgram): Uint8Array => {
  const writer = new FlatWriter();
  const { major, minor, patch } = program.version;
  for (const number of [major, minor, patch]) writer.natural(BigInt(number));
  writeTerm(writer, program.body);
  return writer.finish();
};
