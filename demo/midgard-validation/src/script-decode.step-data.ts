/**
 * Plutus Data and hashing for the TypeScript port of the script-decode step
 * machine (`onchain/aiken/lib/midgard/script-decode-v1.ak`).
 *
 * The step machine commits its frames and run datum with Aiken's
 * `cbor.serialise`, which is the ledger's `serialiseData`: constructors under
 * tags 121-127, 1280-1400 or 102, non-empty lists as indefinite arrays, byte
 * strings over 64 bytes as indefinite strings of 64-byte chunks, and integers
 * outside 64 bits as tag 2 or 3 bignums. `serialiseScriptDecodeData` writes
 * exactly those bytes.
 */
import { midgardBlake2b } from "@al-ft/midgard-core";

/** A Plutus Data value as the port builds it. */
export type ScriptDecodeData =
  | bigint
  | Uint8Array
  | readonly ScriptDecodeData[]
  | {
      readonly constr: number | bigint;
      readonly fields: readonly ScriptDecodeData[];
    };

export const constrData = (
  constr: number | bigint,
  fields: readonly ScriptDecodeData[] = [],
): ScriptDecodeData => ({ constr, fields });

export const intData = (value: number | bigint): ScriptDecodeData =>
  BigInt(value);

/** Aiken's `Bool`: `False` is constructor 0, `True` constructor 1. */
export const boolData = (value: boolean): ScriptDecodeData =>
  constrData(value ? 1 : 0);

/** Aiken's `Option`: `Some(x)` is constructor 0, `None` constructor 1. */
export const optionData = <T>(
  value: T | undefined,
  encode: (inner: T) => ScriptDecodeData,
): ScriptDecodeData =>
  value === undefined ? constrData(1) : constrData(0, [encode(value)]);

const head = (major: number, argument: bigint): Buffer => {
  const initial = major << 5;
  if (argument < 24n) return Buffer.from([initial | Number(argument)]);
  if (argument <= 0xffn) return Buffer.from([initial | 24, Number(argument)]);
  if (argument <= 0xffffn) {
    const out = Buffer.alloc(3);
    out[0] = initial | 25;
    out.writeUInt16BE(Number(argument), 1);
    return out;
  }
  if (argument <= 0xffff_ffffn) {
    const out = Buffer.alloc(5);
    out[0] = initial | 26;
    out.writeUInt32BE(Number(argument), 1);
    return out;
  }
  const out = Buffer.alloc(9);
  out[0] = initial | 27;
  out.writeBigUInt64BE(argument, 1);
  return out;
};

const BYTES_CHUNK = 64;
const TWO_TO_64 = 1n << 64n;

const encodeBytes = (bytes: Uint8Array): Buffer => {
  const value = Buffer.from(bytes);
  if (value.length <= BYTES_CHUNK) {
    return Buffer.concat([head(2, BigInt(value.length)), value]);
  }
  const parts: Buffer[] = [Buffer.from([0x5f])];
  for (let offset = 0; offset < value.length; offset += BYTES_CHUNK) {
    const chunk = value.subarray(offset, offset + BYTES_CHUNK);
    parts.push(head(2, BigInt(chunk.length)), chunk);
  }
  parts.push(Buffer.from([0xff]));
  return Buffer.concat(parts);
};

/** The big-endian bytes of a positive magnitude, without leading zeros. */
export const magnitudeBytes = (magnitude: bigint): Buffer => {
  if (magnitude === 0n) return Buffer.alloc(0);
  const hexValue = magnitude.toString(16);
  return Buffer.from(
    hexValue.length % 2 === 0 ? hexValue : `0${hexValue}`,
    "hex",
  );
};

/** The ledger's `encodeInteger`, as `serialiseData` writes an `I` item. */
export const encodePlutusInteger = (value: bigint): Buffer => {
  if (value >= 0n && value < TWO_TO_64) return head(0, value);
  if (value < 0n && value >= -TWO_TO_64) return head(1, -1n - value);
  return value > 0n
    ? Buffer.concat([Buffer.from([0xc2]), encodeBytes(magnitudeBytes(value))])
    : Buffer.concat([
        Buffer.from([0xc3]),
        encodeBytes(magnitudeBytes(-1n - value)),
      ]);
};

const encodeList = (items: readonly ScriptDecodeData[]): Buffer =>
  items.length === 0
    ? Buffer.from([0x80])
    : Buffer.concat([
        Buffer.from([0x9f]),
        ...items.map(encodeData),
        Buffer.from([0xff]),
      ]);

const encodeData = (value: ScriptDecodeData): Buffer => {
  if (typeof value === "bigint") return encodePlutusInteger(value);
  if (value instanceof Uint8Array) return encodeBytes(value);
  if (Array.isArray(value)) return encodeList(value);
  const { constr, fields } = value as {
    readonly constr: number | bigint;
    readonly fields: readonly ScriptDecodeData[];
  };
  const index = BigInt(constr);
  if (index < 0n) throw new RangeError("constructor index is negative");
  if (index < 7n) {
    return Buffer.concat([head(6, 121n + index), encodeList(fields)]);
  }
  if (index < 128n) {
    return Buffer.concat([head(6, 1280n + index - 7n), encodeList(fields)]);
  }
  return Buffer.concat([
    head(6, 102n),
    Buffer.from([0x82]),
    encodePlutusInteger(index),
    encodeList(fields),
  ]);
};

/** `cbor.serialise` of a Data value. */
export const serialiseScriptDecodeData = (value: ScriptDecodeData): Buffer =>
  encodeData(value);

export const blake2b256 = (bytes: Uint8Array): Buffer =>
  Buffer.from(midgardBlake2b(bytes, { dkLen: 32 }));

export const blake2b224 = (bytes: Uint8Array): Buffer =>
  Buffer.from(midgardBlake2b(bytes, { dkLen: 28 }));
