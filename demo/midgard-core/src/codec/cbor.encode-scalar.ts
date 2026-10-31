/**
 * Scalar writing for the iterative CBOR encoder: the value classification it
 * dispatches on, a growable byte writer, and the encoding of every
 * non-container value, with the reference encoder's error messages.
 */
const ENCODE_ERROR_PREFIX = "CBOR encode error:";
const RANGE_ERROR_MESSAGE =
  "CBOR decode error: encountered BigInt larger than allowable range";
export const COMPLEX_KEY_ERROR_MESSAGE =
  "rfc8949MapSorter: complex key types are not supported yet";
export const CIRCULAR_ERROR_MESSAGE = `${ENCODE_ERROR_PREFIX} object contains circular references`;

const TWO_POW_64 = 1n << 64n;

const MAJOR_UINT = 0x00;
const MAJOR_NEGINT = 0x20;
const MAJOR_BYTES = 0x40;
const MAJOR_TEXT = 0x60;
export const MAJOR_ARRAY = 0x80;
export const MAJOR_MAP = 0xa0;

const OBJECT_TYPE_NAMES: readonly string[] = [
  "Object",
  "RegExp",
  "Date",
  "Error",
  "Map",
  "Set",
  "WeakMap",
  "WeakSet",
  "ArrayBuffer",
  "SharedArrayBuffer",
  "DataView",
  "Promise",
  "URL",
  "HTMLElement",
  "Int8Array",
  "Uint8ClampedArray",
  "Int16Array",
  "Uint16Array",
  "Int32Array",
  "Uint32Array",
  "Float32Array",
  "Float64Array",
  "BigInt64Array",
  "BigUint64Array",
];

const BYTE_VIEW_TYPES: ReadonlySet<string> = new Set([
  "DataView",
  "Uint8ClampedArray",
  "Uint16Array",
  "Uint32Array",
  "Int8Array",
  "Int16Array",
  "Int32Array",
  "BigUint64Array",
  "BigInt64Array",
  "Float32Array",
  "Float64Array",
]);

/** The value classification the encoder dispatches on. */
export const classify = (value: unknown): string => {
  if (value === null) {
    return "null";
  }
  if (value === undefined) {
    return "undefined";
  }
  if (value === true || value === false) {
    return "boolean";
  }
  const typeOf = typeof value;
  if (
    typeOf === "string" ||
    typeOf === "number" ||
    typeOf === "bigint" ||
    typeOf === "symbol"
  ) {
    return typeOf;
  }
  if (typeOf === "function") {
    return "Function";
  }
  if (Array.isArray(value)) {
    return "Array";
  }
  if (value instanceof Uint8Array) {
    return "Uint8Array";
  }
  if ((value as { constructor?: unknown }).constructor === Object) {
    return "Object";
  }
  const tag = Object.prototype.toString.call(value).slice(8, -1);
  return OBJECT_TYPE_NAMES.includes(tag) ? tag : "Object";
};

export class ByteWriter {
  private buffer: Buffer = Buffer.allocUnsafe(256);
  private length = 0;

  private reserve(extra: number): void {
    const needed = this.length + extra;
    if (needed <= this.buffer.length) {
      return;
    }
    let capacity = this.buffer.length * 2;
    while (capacity < needed) {
      capacity *= 2;
    }
    const next = Buffer.allocUnsafe(capacity);
    this.buffer.copy(next, 0, 0, this.length);
    this.buffer = next;
  }

  byte(value: number): void {
    this.reserve(1);
    this.buffer[this.length] = value;
    this.length += 1;
  }

  bytes(value: Uint8Array): void {
    this.reserve(value.length);
    this.buffer.set(value, this.length);
    this.length += value.length;
  }

  head(major: number, value: number | bigint): void {
    if (value < 24) {
      this.byte(major | Number(value));
    } else if (value < 256) {
      this.reserve(2);
      this.buffer[this.length] = major | 24;
      this.buffer[this.length + 1] = Number(value);
      this.length += 2;
    } else if (value < 65536) {
      this.reserve(3);
      this.buffer[this.length] = major | 25;
      this.buffer.writeUInt16BE(Number(value), this.length + 1);
      this.length += 3;
    } else if (value < 4294967296) {
      this.reserve(5);
      this.buffer[this.length] = major | 26;
      this.buffer.writeUInt32BE(Number(value), this.length + 1);
      this.length += 5;
    } else {
      const big = BigInt(value);
      if (big >= TWO_POW_64) {
        throw new Error(RANGE_ERROR_MESSAGE);
      }
      this.reserve(9);
      this.buffer[this.length] = major | 27;
      this.buffer.writeBigUInt64BE(big, this.length + 1);
      this.length += 9;
    }
  }

  text(value: string): void {
    const size = Buffer.byteLength(value, "utf8");
    this.head(MAJOR_TEXT, size);
    this.reserve(size);
    this.buffer.write(value, this.length, size, "utf8");
    this.length += size;
  }

  float64(value: number): void {
    this.reserve(9);
    this.buffer[this.length] = 0xfb;
    this.buffer.writeDoubleBE(value, this.length + 1);
    this.length += 9;
  }

  toBuffer(): Buffer {
    return Buffer.from(this.buffer.subarray(0, this.length));
  }
}

/**
 * Writes a non-container value. Returns false when `type` is a container
 * (Array, Map or Object); throws for unsupported types.
 */
export const writeScalar = (
  writer: ByteWriter,
  value: unknown,
  type: string,
): boolean => {
  switch (type) {
    case "null":
      writer.byte(0xf6);
      return true;
    case "undefined":
      writer.byte(0xf7);
      return true;
    case "boolean":
      writer.byte(value === true ? 0xf5 : 0xf4);
      return true;
    case "number": {
      const n = value as number;
      if (!Number.isInteger(n) || !Number.isSafeInteger(n)) {
        writer.float64(n);
      } else if (n >= 0) {
        writer.head(MAJOR_UINT, n);
      } else {
        writer.head(MAJOR_NEGINT, n * -1 - 1);
      }
      return true;
    }
    case "bigint": {
      const n = value as bigint;
      if (n >= 0n) {
        writer.head(MAJOR_UINT, n);
      } else {
        writer.head(MAJOR_NEGINT, n * -1n - 1n);
      }
      return true;
    }
    case "string":
      writer.text(value as string);
      return true;
    case "Uint8Array": {
      const bytes = value as Uint8Array;
      writer.head(MAJOR_BYTES, bytes.length);
      writer.bytes(bytes);
      return true;
    }
    case "ArrayBuffer": {
      const bytes = new Uint8Array(value as ArrayBuffer);
      writer.head(MAJOR_BYTES, bytes.length);
      writer.bytes(bytes);
      return true;
    }
    case "Array":
    case "Map":
    case "Object":
      return false;
    default: {
      if (BYTE_VIEW_TYPES.has(type)) {
        const view = value as ArrayBufferView;
        const bytes = new Uint8Array(
          view.buffer,
          view.byteOffset,
          view.byteLength,
        );
        writer.head(MAJOR_BYTES, bytes.length);
        writer.bytes(bytes);
        return true;
      }
      throw new Error(`${ENCODE_ERROR_PREFIX} unsupported type: ${type}`);
    }
  }
};

export const isContainerType = (type: string): boolean =>
  type === "Array" || type === "Map" || type === "Object";

/** Validates a scalar type without writing it (token-mode leaves). */
export const assertScalarType = (type: string): void => {
  switch (type) {
    case "null":
    case "undefined":
    case "boolean":
    case "number":
    case "bigint":
    case "string":
    case "Uint8Array":
    case "ArrayBuffer":
      return;
    default:
      if (!BYTE_VIEW_TYPES.has(type)) {
        throw new Error(`${ENCODE_ERROR_PREFIX} unsupported type: ${type}`);
      }
  }
};
