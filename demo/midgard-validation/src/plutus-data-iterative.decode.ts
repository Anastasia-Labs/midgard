import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  type KV,
} from "@harmoniclabs/plutus-data";

/**
 * CBOR -> harmonic `Data` without recursion, decoding exactly as the Cardano
 * ledger decodes Plutus Data (`decodeData` in plutus-core
 * `PlutusCore/Data.hs`, over cborg's token reader), and requiring the whole
 * input to be that one item. Its own frame stack lets any byte-cap-legal
 * nesting depth decode.
 *
 * One reader serves two entries, which differ in one parameter only:
 *
 * - `plutusDataFromCborIterative` decodes exactly the Plutus Data CBOR the
 *   Cardano ledger accepts. Use it for anything claimed to be Plutus Data:
 *   datums, redeemers, script contexts, CEK constants and Data nodes.
 * - `midgardEnvelopeDataFromCborIterative` applies the same rules except the
 *   64-byte bound on a definite byte string and on each chunk of an
 *   indefinite one. Use it only for Midgard-internal wire (work witnesses,
 *   controls, proof carriage, discovery bitmaps), whose writers emit long
 *   byte strings definite.
 *
 * The rules:
 *
 * - the input is exactly one item: trailing bytes are refused;
 * - heads may be non-minimal; additional information 28-30 is refused, and
 *   31 (indefinite) is accepted only for byte strings, lists and maps;
 * - an integer is a major-0 or major-1 head (at most 64 bits), or a bignum:
 *   the one-byte tag head `c2` (positive) or `c3` (negative) over a byte
 *   string, big-endian, where the empty string is 0 (`c2`) or -1 (`c3`);
 *   tags 2 and 3 written with a longer head are unknown tags;
 * - a byte string is definite, or indefinite with chunks that are each a
 *   definite byte string; under the Data entry a definite string and every
 *   chunk is at most 64 bytes (bignum magnitudes included);
 * - lists and maps are definite (any head width) or indefinite; map entries
 *   keep their order, duplicate keys included;
 * - a constructor is tag 121-127 (index 0-6) or 1280-1400 (index 7-127) over
 *   a list, or tag 102 over a two-element array, definite or indefinite,
 *   holding a major-0 index (up to 2^64 - 1, small ones included) and a
 *   list of fields; any other tag, and any of these tags over something else,
 *   is refused;
 * - text strings, simple values, floats and a misplaced break are refused.
 *
 * A byte string is a copy with the input's class (a Buffer input gives
 * Buffers) when it is definite or has a single chunk, and a plain
 * `Uint8Array` otherwise.
 */

/** Cardano's bound on a definite byte string or one indefinite chunk. */
export const CARDANO_PLUTUS_DATA_BYTES_CHUNK_LIMIT = 64n;

type Constructor = {
  readonly index: bigint;
  /** Tag 102 in an indefinite array: a break must follow the fields. */
  readonly breakAfter: boolean;
};

type ListFrame = {
  readonly type: "list";
  readonly length: bigint;
  readonly values: Data[];
  readonly constructor: Constructor | undefined;
};

type MapFrame = {
  readonly type: "map";
  readonly length: bigint;
  readonly pairs: KV<Data, Data>[];
  key: Data | undefined;
};

type BytesFrame = {
  readonly type: "bytes";
  readonly chunks: Uint8Array[];
  /** Set when the byte string is a bignum's magnitude. */
  readonly bignum: "positive" | "negative" | undefined;
};

type Frame = ListFrame | MapFrame | BytesFrame;

type Head = {
  readonly major: number;
  /** The argument; `INDEFINITE` for additional information 31. */
  readonly argument: bigint;
  /** Additional information, the low five bits of the initial byte. */
  readonly additional: number;
};

const INDEFINITE = -1n;

const BREAK = 0xff;

/**
 * A copy of `bytes` of the same class (a Buffer stays a Buffer), as
 * `Uint8Array.prototype.slice.call` produces.
 */
const copyOf = (bytes: Uint8Array): Uint8Array =>
  Uint8Array.prototype.slice.call(bytes);

/** A lone chunk keeps its class; no chunk or several give a `Uint8Array`. */
const concatChunks = (chunks: readonly Uint8Array[]): Uint8Array => {
  if (chunks.length === 1) return copyOf(chunks[0]!);
  let length = 0;
  for (const chunk of chunks) length += chunk.length;
  const out = new Uint8Array(length);
  let at = 0;
  for (const chunk of chunks) {
    out.set(chunk, at);
    at += chunk.length;
  }
  return out;
};

/** A big-endian magnitude; the empty string is 0. */
const magnitudeOf = (raw: Uint8Array): bigint =>
  raw.length === 0 ? 0n : BigInt(`0x${Buffer.from(raw).toString("hex")}`);

const bignumData = (sign: "positive" | "negative", raw: Uint8Array): DataI => {
  const magnitude = magnitudeOf(raw);
  return new DataI(sign === "positive" ? magnitude : -1n - magnitude);
};

/** The constructor index of a compact tag, or undefined if it is not one. */
const compactConstructorIndex = (tag: bigint): bigint | undefined => {
  if (tag >= 121n && tag <= 127n) return tag - 121n;
  if (tag >= 1280n && tag <= 1400n) return tag - 1273n;
  return undefined;
};

/**
 * The one reader behind both entries. `bytesChunkLimit` bounds a definite
 * byte string and each indefinite chunk; `undefined` leaves them unbounded.
 */
const decodePlutusDataCbor = (
  bytes: Uint8Array,
  bytesChunkLimit: bigint | undefined,
): Data => {
  let offset = 0;
  const byteAt = (at: number): number => {
    if (at >= bytes.length) {
      throw new RangeError("CBOR Plutus Data ends before its item does");
    }
    return bytes[at]!;
  };
  const readHead = (): Head => {
    const initial = byteAt(offset);
    offset += 1;
    const major = initial >> 5;
    const additional = initial & 0x1f;
    if (additional < 24) {
      return { major, argument: BigInt(additional), additional };
    }
    if (additional === 31) return { major, argument: INDEFINITE, additional };
    const width =
      additional === 24
        ? 1
        : additional === 25
          ? 2
          : additional === 26
            ? 4
            : additional === 27
              ? 8
              : 0;
    if (width === 0) {
      throw new Error("invalid CBOR additional information in Plutus Data");
    }
    let argument = 0n;
    for (let index = 0; index < width; index += 1) {
      argument = (argument << 8n) | BigInt(byteAt(offset + index));
    }
    offset += width;
    return { major, argument, additional };
  };
  /** The payload of a definite byte string whose head was just read. */
  const definiteBytes = (length: bigint): Uint8Array => {
    if (bytesChunkLimit !== undefined && length > bytesChunkLimit) {
      throw new Error("CBOR Plutus Data byte string exceeds 64 bytes");
    }
    if (length > BigInt(bytes.length - offset)) {
      throw new RangeError("CBOR Plutus Data ends before its item does");
    }
    const end = offset + Number(length);
    const raw = bytes.subarray(offset, end);
    offset = end;
    return raw;
  };
  /** Checks that a definite container's length can fit the input left. */
  const checkLength = (length: bigint): void => {
    if (length !== INDEFINITE && length > BigInt(bytes.length - offset)) {
      throw new RangeError("CBOR Plutus Data container exceeds its input");
    }
  };

  const stack: Frame[] = [];

  /** Opens a list (a constructor's fields when `constructor` is set). */
  const openList = (
    head: Head,
    constructor: Constructor | undefined,
  ): Data | undefined => {
    if (head.major !== 4) {
      throw new Error("CBOR Plutus Data constructor fields are not a list");
    }
    checkLength(head.argument);
    const frame: ListFrame = {
      type: "list",
      length: head.argument,
      values: [],
      constructor,
    };
    if (head.argument === 0n) return closeList(frame);
    stack.push(frame);
    return undefined;
  };

  const closeList = (frame: ListFrame): Data => {
    const { constructor } = frame;
    if (constructor === undefined) return new DataList(frame.values);
    if (constructor.breakAfter) {
      if (byteAt(offset) !== BREAK) {
        throw new Error("CBOR tag 102 Plutus Data has more than two elements");
      }
      offset += 1;
    }
    return new DataConstr(constructor.index, frame.values);
  };

  /** Reads the item after a tag head; undefined when it opened a frame. */
  const readTagged = (head: Head): Data | undefined => {
    if (head.argument === INDEFINITE) {
      throw new Error("invalid indefinite CBOR tag in Plutus Data");
    }
    if (head.additional === 2 || head.additional === 3) {
      const sign = head.additional === 2 ? "positive" : "negative";
      const payload = readHead();
      if (payload.major !== 2) {
        throw new Error("CBOR Plutus Data bignum is not over a byte string");
      }
      if (payload.argument === INDEFINITE) {
        stack.push({ type: "bytes", chunks: [], bignum: sign });
        return undefined;
      }
      return bignumData(sign, definiteBytes(payload.argument));
    }
    const tag = head.argument;
    if (tag === 102n) {
      const outer = readHead();
      if (
        outer.major !== 4 ||
        (outer.argument !== INDEFINITE && outer.argument !== 2n)
      ) {
        throw new Error("CBOR tag 102 Plutus Data is not a two-element array");
      }
      const index = readHead();
      if (index.major !== 0 || index.argument === INDEFINITE) {
        throw new Error("CBOR tag 102 Plutus Data index is not a word64");
      }
      return openList(readHead(), {
        index: index.argument,
        breakAfter: outer.argument === INDEFINITE,
      });
    }
    const index = compactConstructorIndex(tag);
    if (index === undefined) {
      throw new Error(`unknown CBOR tag ${tag.toString()} in Plutus Data`);
    }
    return openList(readHead(), { index, breakAfter: false });
  };

  /** Reads one item at a value position; undefined when it opened a frame. */
  const readItem = (): Data | undefined => {
    const head = readHead();
    switch (head.major) {
      case 0:
      case 1:
        if (head.argument === INDEFINITE) {
          throw new Error("invalid indefinite CBOR integer in Plutus Data");
        }
        return new DataI(
          head.major === 0 ? head.argument : -1n - head.argument,
        );
      case 2:
        if (head.argument === INDEFINITE) {
          stack.push({ type: "bytes", chunks: [], bignum: undefined });
          return undefined;
        }
        return new DataB(copyOf(definiteBytes(head.argument)));
      case 4:
        return openList(head, undefined);
      case 5: {
        checkLength(head.argument);
        if (head.argument === 0n) return new DataMap([]);
        stack.push({
          type: "map",
          length: head.argument,
          pairs: [],
          key: undefined,
        });
        return undefined;
      }
      case 6:
        return readTagged(head);
      default:
        throw new Error("invalid CBOR major type for Plutus Data");
    }
  };

  /** Reads one chunk of an indefinite byte string into `frame`. */
  const readChunk = (frame: BytesFrame): void => {
    const head = readHead();
    if (head.major !== 2 || head.argument === INDEFINITE) {
      throw new Error("indefinite CBOR bytes contain a non-definite chunk");
    }
    frame.chunks.push(definiteBytes(head.argument));
  };

  /** Closes the top frame at a break. */
  const closeAtBreak = (frame: Frame | undefined): Data => {
    if (frame === undefined) {
      throw new Error("unexpected CBOR break in Plutus Data");
    }
    switch (frame.type) {
      case "bytes": {
        const raw = concatChunks(frame.chunks);
        return frame.bignum === undefined
          ? new DataB(raw)
          : bignumData(frame.bignum, raw);
      }
      case "list":
        if (frame.length !== INDEFINITE) break;
        return closeList(frame);
      case "map":
        if (frame.length !== INDEFINITE || frame.key !== undefined) break;
        return new DataMap(frame.pairs);
    }
    throw new Error("unexpected CBOR break in Plutus Data");
  };

  for (;;) {
    let item: Data | undefined;
    const top = stack.at(-1);
    if (byteAt(offset) === BREAK) {
      offset += 1;
      stack.pop();
      item = closeAtBreak(top);
    } else if (top?.type === "bytes") {
      readChunk(top);
    } else {
      item = readItem();
    }
    // Deliver the item to its parents until one of them needs more input.
    while (item !== undefined) {
      const parent = stack.at(-1);
      if (parent === undefined) {
        if (offset !== bytes.length) {
          throw new Error("trailing bytes after CBOR Plutus Data");
        }
        return item;
      }
      if (parent.type === "list") {
        parent.values.push(item);
        item = undefined;
        if (BigInt(parent.values.length) === parent.length) {
          stack.pop();
          item = closeList(parent);
        }
      } else if (parent.type === "map") {
        if (parent.key === undefined) {
          parent.key = item;
          item = undefined;
        } else {
          parent.pairs.push({ fst: parent.key, snd: item });
          parent.key = undefined;
          item = undefined;
          if (BigInt(parent.pairs.length) === parent.length) {
            stack.pop();
            item = new DataMap(parent.pairs);
          }
        }
      } else {
        throw new Error("a CBOR byte string chunk is not a Plutus Data item");
      }
    }
  }
};

/**
 * Decodes `bytes` exactly as the Cardano ledger decodes Plutus Data. Use it
 * for anything claimed to be Plutus Data.
 */
export const plutusDataFromCborIterative = (bytes: Uint8Array): Data =>
  decodePlutusDataCbor(bytes, CARDANO_PLUTUS_DATA_BYTES_CHUNK_LIMIT);

/**
 * Decodes Midgard-internal wire: the rules of `plutusDataFromCborIterative`
 * without the 64-byte bound on a byte string or chunk.
 */
export const midgardEnvelopeDataFromCborIterative = (bytes: Uint8Array): Data =>
  decodePlutusDataCbor(bytes, undefined);
