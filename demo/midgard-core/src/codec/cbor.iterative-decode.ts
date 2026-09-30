import {
  type CborItemSpan,
  compareCborKeyBytes,
  ensureSafeLength,
  err,
  FATAL_UTF8_DECODER,
  readArgument,
} from "./cbor.read-argument.js";

/**
 * Midgard's strict CBOR reader, written with explicit stacks so that nesting
 * depth is bounded only by the input length: every byte-cap-legal item, however
 * deep, is read in constant JS stack.
 *
 * The accepted subset is definite-length unsigned and negative integers, byte
 * and text strings, arrays, maps with unique keys in length-first canonical
 * order, and `false` / `true` / `null`. Tags, indefinite lengths, floats,
 * `undefined` and other simple values are refused.
 */

type SkipFrame = {
  readonly major: 4 | 5;
  readonly start: number;
  remaining: number;
  /** Map frames only: true once the current entry's key has been read. */
  awaitingValue: boolean;
  readonly seen: Set<string> | undefined;
  previousKey: Buffer | undefined;
};

/**
 * Returns the span of the one canonical CBOR item that starts at `offset`,
 * checking it item by item in document order and throwing the first
 * `MidgardTxCodecError(CborDecode)` it meets.
 */
export const skipCborItem = (
  bytes: Uint8Array,
  offset: number,
): CborItemSpan => {
  const stack: SkipFrame[] = [];
  let cursor = offset;
  for (;;) {
    const itemStart = cursor;
    const header = readArgument(bytes, cursor);
    let itemEnd: number;
    switch (header.major) {
      case 0:
      case 1:
        itemEnd = header.nextOffset;
        break;
      case 2:
      case 3: {
        const length = ensureSafeLength(header.value, itemStart);
        const end = header.nextOffset + length;
        if (end > bytes.length) {
          throw err("CBOR string exceeds input length", `offset=${itemStart}`);
        }
        if (header.major === 3) {
          if (
            length >= 3 &&
            bytes[header.nextOffset] === 0xef &&
            bytes[header.nextOffset + 1] === 0xbb &&
            bytes[header.nextOffset + 2] === 0xbf
          ) {
            throw err(
              "CBOR text string must not begin with a UTF-8 BOM",
              `offset=${itemStart}`,
            );
          }
          try {
            FATAL_UTF8_DECODER.decode(bytes.subarray(header.nextOffset, end));
          } catch {
            throw err(
              "CBOR text string is not valid UTF-8",
              `offset=${itemStart}`,
            );
          }
        }
        itemEnd = end;
        break;
      }
      case 4:
      case 5: {
        const length = ensureSafeLength(header.value, itemStart);
        if (length === 0) {
          itemEnd = header.nextOffset;
          break;
        }
        stack.push({
          major: header.major,
          start: itemStart,
          remaining: length,
          awaitingValue: false,
          seen: header.major === 5 ? new Set<string>() : undefined,
          previousKey: undefined,
        });
        cursor = header.nextOffset;
        continue;
      }
      case 6:
        throw err(
          "CBOR tags are not valid in this Midgard codec",
          `offset=${itemStart}`,
        );
      case 7:
        if (
          header.additional === 20 ||
          header.additional === 21 ||
          header.additional === 22
        ) {
          itemEnd = header.nextOffset;
          break;
        }
        if (header.additional === 23) {
          throw err("CBOR undefined is not valid", `offset=${itemStart}`);
        }
        throw err(
          "CBOR simple values and floats are not valid",
          `offset=${itemStart}`,
        );
      default:
        throw err("Unsupported CBOR major type", `offset=${itemStart}`);
    }

    // An item just completed at [spanStart, itemEnd). Hand it to its parent,
    // closing every container it completes on the way up.
    let spanStart = itemStart;
    let spanMajor = header.major;
    for (;;) {
      const parent = stack[stack.length - 1];
      if (parent === undefined) {
        return { start: spanStart, end: itemEnd, major: spanMajor };
      }
      if (parent.major === 5 && !parent.awaitingValue) {
        const keyBytes = Buffer.from(bytes.subarray(spanStart, itemEnd));
        const keyHex = keyBytes.toString("hex");
        if (parent.seen!.has(keyHex)) {
          throw err("Duplicate CBOR map key", `offset=${spanStart}`);
        }
        parent.seen!.add(keyHex);
        if (
          parent.previousKey !== undefined &&
          compareCborKeyBytes(parent.previousKey, keyBytes) > 0
        ) {
          throw err(
            "Non-canonical CBOR map key ordering",
            `offset=${spanStart}`,
          );
        }
        parent.previousKey = keyBytes;
        parent.awaitingValue = true;
        break;
      }
      parent.awaitingValue = false;
      parent.remaining -= 1;
      if (parent.remaining > 0) {
        break;
      }
      stack.pop();
      spanStart = parent.start;
      spanMajor = parent.major;
    }
    cursor = itemEnd;
  }
};

const TEXT_DECODER = new TextDecoder();
const MAX_SAFE = BigInt(Number.MAX_SAFE_INTEGER);
const MIN_SAFE = BigInt(Number.MIN_SAFE_INTEGER);

type BuildFrame = {
  readonly container: unknown[] | Map<unknown, unknown>;
  remaining: number;
  pendingKey: unknown;
  awaitingValue: boolean;
};

/**
 * Builds the JS value of one canonical CBOR item that {@link skipCborItem}
 * has already accepted, in the shapes Midgard's decoders consume: `Map` for
 * maps, arrays, fresh `Uint8Array` copies for bytes, strings, booleans, `null`,
 * and integers as `number` inside the safe-integer range and `bigint` outside
 * it.
 */
export const buildCanonicalCborValue = (bytes: Uint8Array): unknown => {
  const stack: BuildFrame[] = [];
  let cursor = 0;
  for (;;) {
    const header = readArgument(bytes, cursor);
    let value: unknown;
    switch (header.major) {
      case 0:
        value = header.value <= MAX_SAFE ? Number(header.value) : header.value;
        cursor = header.nextOffset;
        break;
      case 1: {
        const negative = -1n - header.value;
        value = negative >= MIN_SAFE ? Number(negative) : negative;
        cursor = header.nextOffset;
        break;
      }
      case 2:
      case 3: {
        const end = header.nextOffset + Number(header.value);
        const slice = bytes.subarray(header.nextOffset, end);
        value =
          header.major === 2
            ? new Uint8Array(slice)
            : TEXT_DECODER.decode(slice);
        cursor = end;
        break;
      }
      case 4:
      case 5: {
        const length = Number(header.value);
        const container =
          header.major === 4
            ? new Array<unknown>(0)
            : new Map<unknown, unknown>();
        cursor = header.nextOffset;
        if (length > 0) {
          stack.push({
            container,
            remaining: length,
            pendingKey: undefined,
            awaitingValue: false,
          });
          continue;
        }
        value = container;
        break;
      }
      case 7:
        value =
          header.additional === 20
            ? false
            : header.additional === 21
              ? true
              : null;
        cursor = header.nextOffset;
        break;
      default:
        throw err(
          "Unsupported CBOR item in validated input",
          `offset=${cursor}`,
        );
    }

    for (;;) {
      const parent = stack[stack.length - 1];
      if (parent === undefined) {
        return value;
      }
      if (parent.container instanceof Map) {
        if (!parent.awaitingValue) {
          parent.pendingKey = value;
          parent.awaitingValue = true;
          break;
        }
        parent.container.set(parent.pendingKey, value);
        parent.pendingKey = undefined;
        parent.awaitingValue = false;
      } else {
        parent.container.push(value);
      }
      parent.remaining -= 1;
      if (parent.remaining > 0) {
        break;
      }
      stack.pop();
      value = parent.container;
    }
  }
};
